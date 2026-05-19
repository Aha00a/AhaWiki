package controllers

import actors.ActorAhaWiki.Calculate
import akka.actor._
import akka.{NotUsed}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.scaladsl.SourceQueueWithComplete
import akka.stream.scaladsl.Keep
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleSpreadsheetApi
import com.aha00a.supercsv.SupercsvUtil
import com.github.difflib.DiffUtils
import com.github.difflib.UnifiedDiffUtils
import logics._
import logics.wikis.ExtractConvertInjectInterpreterCustom
import logics.wikis.PageLogic
import logics.wikis.WikiPermission
import logics.wikis.WikiPermissionDetail
import logics.wikis.WikiSnippet
import logics.wikis.SignedReadUrlLogic
import logics.wikis.interpreters.Interpreters
import models.RequestWrapper
import models._
import models.tables.CalculatedCosineSimilarity
import models.tables.CalculatedLink
import models.tables.Attachment
import models.tables.Page
import models.tables.Permission
import models.tables.Site
import models.tables.User
import play.api.Environment
import play.api.Configuration
import play.api.Logging
import play.api.Mode
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.Files.TemporaryFile
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.io.ByteArrayInputStream
import java.net.URLDecoder
import java.net.URLEncoder
import java.nio.file.Files
import java.sql.Connection
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.inject._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import java.util.Base64
import java.util.UUID
import scala.collection.concurrent.TrieMap
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class Wiki @Inject()(implicit val
controllerComponents: ControllerComponents,
                     actorSystem: ActorSystem,
                     database: Database,
                     environment: Environment,
                     @Named("db-actor") actorAhaWiki: ActorRef,
                     applicationConf: ApplicationConf,
                     ahaWikiCache: AhaWikiCache,
                     wsClient: WSClient,
                     executionContext: ExecutionContext,
                     configuration: Configuration
) extends BaseController with Logging {
  private def roomKeyForPage(siteId: Long, pageId: String): String = s"wiki:$siteId:$pageId"

  private object PageCursorHub {
    private case class PageSubscriber(
      queue: SourceQueueWithComplete[String],
      var saveSenderId: Option[String],
    )
    private val subscribers = TrieMap.empty[String, TrieMap[String, PageSubscriber]]

    def subscribe(page: String, id: String, queue: SourceQueueWithComplete[String]): Unit = {
      val pageMap = subscribers.getOrElseUpdate(page, TrieMap.empty[String, PageSubscriber])
      pageMap.put(id, PageSubscriber(queue, None))
    }

    def unsubscribe(page: String, id: String): Unit = {
      subscribers.get(page).foreach { pageMap =>
        pageMap.remove(id).foreach(_.queue.complete())
        if (pageMap.isEmpty) subscribers.remove(page)
      }
    }

    def broadcast(page: String, senderId: String, payload: String): Unit = {
      subscribers.get(page).foreach { pageMap =>
        pageMap.foreach { case (id, subscriber) =>
          if (id != senderId) subscriber.queue.offer(payload)
        }
      }
    }

    def setSaveSenderId(page: String, id: String, saveSenderId: Option[String]): Unit = {
      subscribers.get(page).flatMap(_.get(id)).foreach { subscriber =>
        subscriber.saveSenderId = saveSenderId.filter(_.nonEmpty)
      }
    }

    def broadcastPageUpdated(page: String, saveSenderId: Option[String], payload: String): Unit = {
      subscribers.get(page).foreach { pageMap =>
        pageMap.foreach { case (_, subscriber) =>
          val shouldExclude = saveSenderId.nonEmpty && subscriber.saveSenderId == saveSenderId
          if (!shouldExclude) subscriber.queue.offer(payload)
        }
      }
    }
  }

  implicit class RichResult(result: Result) {
    def withHeaderRobotNoIndexNoFollow: Result = result.withHeaders("X-Robots-Tag" -> "noindex, nofollow")
  }

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)

  private val attachmentTimestampFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH-mm-ss")

  private lazy val signedReadUrlSecret: String = configuration.getOptional[String]("play.http.secret.key").getOrElse("")

  private val pagePermissionModeKeep = "keep"
  private val pagePermissionModeGeneral = "general"
  private val pagePermissionModePrivateRead = "privateRead"
  private val pagePermissionModePrivateWrite = "privateWrite"
  private val pagePermissionModes = Set(
    pagePermissionModeKeep,
    pagePermissionModeGeneral,
    pagePermissionModePrivateRead,
    pagePermissionModePrivateWrite,
  )

  private def isAdmin(implicit request: RequestHeader): Boolean = {
    SessionLogic.getUser(request).exists(u => u.email == "aha00a@gmail.com" || u.seq == 1)
  }

  private def normalizePagePermissionMode(value: Option[String], isNewPage: Boolean): String = {
    val mode = value.map(_.trim).filter(pagePermissionModes.contains).getOrElse(pagePermissionModeKeep)
    if (isNewPage && mode == pagePermissionModeKeep) pagePermissionModeGeneral else mode
  }

  private def pagePermissionSummary(name: String)(implicit request: RequestHeader, connection: Connection, site: Site): Seq[(String, String)] = {
    val permissions = Permission.select()
    val permissionLogic = new PermissionLogic(permissions)
    val currentActor = SessionLogic.getUser(request).map(_.email).getOrElse("")
    val matchingTargetPermissions = permissionLogic.seq.filter(_.targetMatches(name))

    def actionName(action: Int): String =
      Permission.Action.values.find(_.id == action).map(_.toString).getOrElse(action.toString)

    def permissionLabel(permission: Permission): String = {
      val target = if (permission.targetType == Permission.TargetType.All) "*" else permission.target
      val actor = if (permission.actorType == Permission.ActorType.All) "*" else permission.actor
      s"${permission.targetType}($target), ${permission.actorType}($actor) -> ${actionName(permission.action)}"
    }

    val currentMatch = permissionLogic.matched(name, currentActor)
      .map(permission => "Current User" -> permissionLabel(permission))
      .getOrElse("Current User" -> "None")
    val anonymousMatch = permissionLogic.matched(name, "")
      .map(permission => "Anonymous User" -> permissionLabel(permission))
      .getOrElse("Anonymous User" -> "None")
    val targetMatches = if (matchingTargetPermissions.isEmpty) {
      Seq("Page Matching" -> "None")
    } else {
      matchingTargetPermissions.map(permission => "Page Matching" -> permissionLabel(permission))
    }

    currentMatch +: anonymousMatch +: targetMatches
  }

  private def applyPagePermissionMode(name: String, mode: String, actor: String)
                                     (implicit connection: Connection, site: Site): Unit = {
    mode match {
      case `pagePermissionModeKeep` =>
      case `pagePermissionModeGeneral` =>
        Permission.deleteExactTarget(name)
        AhaWikiCacheMemoryPermission.invalidate(site.seq)
      case `pagePermissionModePrivateRead` =>
        Permission.deleteExactTarget(name)
        Permission.upsert(Permission(name, Permission.TargetType.Exact, "", Permission.ActorType.All, Permission.none))
        Permission.upsert(Permission(name, Permission.TargetType.Exact, actor, Permission.ActorType.Exact, Permission.admin))
        AhaWikiCacheMemoryPermission.invalidate(site.seq)
      case `pagePermissionModePrivateWrite` =>
        Permission.deleteExactTarget(name)
        Permission.upsert(Permission(name, Permission.TargetType.Exact, "", Permission.ActorType.All, Permission.read))
        Permission.upsert(Permission(name, Permission.TargetType.Exact, actor, Permission.ActorType.Exact, Permission.admin))
        AhaWikiCacheMemoryPermission.invalidate(site.seq)
      case _ =>
    }
  }

  private def sanitizeAttachmentPathSegment(v: String): String = {
    val sanitized = v.replaceAll("[^\\p{IsHangul}\\p{IsHan}\\p{IsHiragana}\\p{IsKatakana}a-zA-Z0-9._-]", "_")
    if (sanitized.nonEmpty) sanitized else "_"
  }

  private def buildAttachmentObjectKey(siteSeq: Long, pageName: String, originalFileName: String, extension: String, now: LocalDateTime = LocalDateTime.now()): String = {
    val sanitizedPageName = sanitizeAttachmentPathSegment(pageName)
    val sanitizedOriginalFileName = sanitizeAttachmentPathSegment(originalFileName)
    val sanitizedExtension = sanitizeAttachmentPathSegment(extension).toLowerCase
    val sanitizedOriginalFileNameWithoutExtension = {
      val stripped = sanitizedOriginalFileName.stripSuffix(s".$sanitizedExtension")
      if (stripped.nonEmpty) stripped else sanitizedOriginalFileName
    }
    val formattedDateTime = now.format(attachmentTimestampFormatter)
    s"Attachment/$siteSeq/$sanitizedPageName/$sanitizedOriginalFileName/${sanitizedOriginalFileNameWithoutExtension}.$formattedDateTime.$sanitizedExtension"
  }

  private def toAttachmentMacroArgument(objectKey: String, siteSeq: Long, pageName: String): String = {
    val sitePrefix = s"Attachment/$siteSeq/"
    val pagePrefix = s"$sitePrefix${sanitizeAttachmentPathSegment(pageName)}/"
    if (objectKey.startsWith(pagePrefix)) {
      objectKey.stripPrefix(pagePrefix)
    } else if (objectKey.startsWith(sitePrefix)) {
      objectKey.stripPrefix(sitePrefix)
    } else {
      objectKey
    }
  }

  private def resolveAttachmentObjectKey(siteSeq: Long, pageName: String, rawObjectKey: String): String = {
    val trimmed = Option(rawObjectKey).map(_.trim).getOrElse("")
    if (trimmed.isEmpty) {
      ""
    } else if (trimmed.startsWith("Attachment/")) {
      trimmed
    } else {
      s"Attachment/$siteSeq/${sanitizeAttachmentPathSegment(pageName)}/$trimmed"
    }
  }

  private def listPageAttachmentObjectKeysFromS3(siteSeq: Long, pageName: String): Seq[String] = {
    val bucket = applicationConf.AhaWiki.aws.s3.bucket()
    val prefix = s"Attachment/$siteSeq/${sanitizeAttachmentPathSegment(pageName)}/"
    val amazonS3 = buildAmazonS3Client()
    val request = new com.amazonaws.services.s3.model.ListObjectsV2Request()
      .withBucketName(bucket)
      .withPrefix(prefix)
      .withMaxKeys(200)
    val result = amazonS3.listObjectsV2(request)
    result.getObjectSummaries.asScala.toSeq
      .map(_.getKey)
      .filter(key => key != null && key.nonEmpty && !key.endsWith("/"))
  }

  private def deletePageAttachments(siteSeq: Long, pageName: String)
                                   (implicit connection: Connection): Either[String, Unit] = {
    val bucket = applicationConf.AhaWiki.aws.s3.bucket()
    val amazonS3 = buildAmazonS3Client()
    val objectKeysFromDb = Attachment.selectObjectKeysByPage(siteSeq, pageName)
    val objectKeysFromS3 = listPageAttachmentObjectKeysFromS3(siteSeq, pageName)
    val objectKeys = (objectKeysFromDb ++ objectKeysFromS3).map(_.trim).filter(_.nonEmpty).distinct

    val failedObjectKeys = objectKeys.flatMap { objectKey =>
      Try(amazonS3.deleteObject(bucket, objectKey)) match {
        case Success(_) => None
        case Failure(error) =>
          logger.error(s"deletePageAttachments failed. pageName=$pageName objectKey=$objectKey", error)
          Some(objectKey)
      }
    }
    if (failedObjectKeys.nonEmpty) {
      Left(s"Attachment delete failed. pageName=$pageName failedObjectKeys=${failedObjectKeys.mkString(",")}")
    } else {
      objectKeys.foreach(Attachment.markDeleted)
      Right(())
    }
  }

  private def getWritablePageContext(pageName: String)
                                    (implicit request: Request[Any], connection: Connection): Either[Result, (Site, ContextWikiPage, RequestWrapper)] = {
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(pageName)
    implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
    val latestContent = Page.selectLastRevision(pageName).map(page => PageContent(page.content))
    if (!WikiPermission().isWritable(pageName, latestContent)) Left(Forbidden("Permission denied."))
    else Right((site, contextWikiPage, provider))
  }

  private def buildAmazonS3Client(): com.amazonaws.services.s3.AmazonS3 = {
    import com.amazonaws.auth.AWSStaticCredentialsProvider
    import com.amazonaws.auth.BasicAWSCredentials
    import com.amazonaws.services.s3.AmazonS3
    import com.amazonaws.services.s3.AmazonS3ClientBuilder
    val credentials = new BasicAWSCredentials(
      applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
      applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
    )
    AmazonS3ClientBuilder.standard
      .withCredentials(new AWSStaticCredentialsProvider(credentials))
      .withRegion(applicationConf.AhaWiki.aws.AWS_REGION())
      .build()
  }

  private def insertInitiatedAttachment(
                                         siteSeq: Long,
                                         pageName: String,
                                         originalFilename: String,
                                         objectKey: String,
                                         contentType: String,
                                         fileSize: Long,
                                       )(implicit request: RequestHeader, connection: Connection): Unit = {
    val currentUser = SessionLogic.getUser(request)
    val bucket = applicationConf.AhaWiki.aws.s3.bucket()
    val storedFilename = objectKey.split("/").lastOption.getOrElse(objectKey)
    Attachment.insertInitiated(
      site = siteSeq,
      pageName = pageName,
      user = currentUser.map(_.seq),
      uploaderEmail = currentUser.map(_.email),
      originalFilename = originalFilename,
      storedFilename = storedFilename,
      bucket = bucket,
      objectKey = objectKey,
      contentType = contentType,
      fileSize = fileSize,
    )
  }

  def view(nameEncoded: String, revision: Int, action: String): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)

      val name = decodeWikiName(nameEncoded)

      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper

      val pageFirstRevision = Page.selectFirstRevision(name)
      val pageLastRevision = Page.selectLastRevision(name)
      val pageSpecificRevision = Page.select(name, revision)

      val pageLastRevisionContent = pageLastRevision.map(s => PageContent(s.content))
      val wikiPermission = WikiPermission()
      val hasReadPermissionRestriction = !wikiPermission.isReadableByAnonymous(name, pageLastRevisionContent)
      val permissionDetail = Some(wikiPermission.detail(name, pageLastRevisionContent))
      val isReadableByPermission = wikiPermission.isReadable(name, pageLastRevisionContent)
      val isReadableBySignedUrl = SignedReadUrlLogic.verifyReadRequest(
        host = request.host,
        name = name,
        revision = revision,
        action = action,
        expiresAtEpochSeconds = SignedReadUrlLogic.parseEpochSecond(request.getQueryString(SignedReadUrlLogic.QueryParamExpires)),
        signature = request.getQueryString(SignedReadUrlLogic.QueryParamSignature),
        secret = signedReadUrlSecret,
      )
      val isReadable = isReadableByPermission || isReadableBySignedUrl
      val isWritable = wikiPermission.isWritable(name, pageLastRevisionContent)

      //noinspection ScalaUnusedSymbol
      (pageSpecificRevision, action, isReadable, isWritable) match {
        case (None, "edit", _, true) =>
          val content = DefaultPageLogic.getOption(name).getOrElse(s"""= $name\n""")
          val page = Page(name, 0, LocalDateTime.now(), Some("AhaWiki"), None, "127.0.0.1", "", isMinorEdit = false, content)
          val (initialEditorText, partialRange) = buildEditFormState(page.content, request)
          Ok(views.html.Wiki.edit(
            page,
            applicationConf,
            initialEditorText,
            partialRange,
            canManagePagePermission = isAdmin,
            pagePermissionDefault = pagePermissionModeGeneral,
            pagePermissionSummary = pagePermissionSummary(name),
            isNewPage = true,
          )).withHeaders("X-Robots-Tag" -> "noindex, nofollow")

        case (None, "edit", _, false) =>
          Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaderRobotNoIndexNoFollow


        case (None, _, _, _) =>
          renderNotFoundPage(name, isWritable, pageFirstRevision, pageLastRevision)

        case (Some(page), "" | "view", true, _) =>
          renderReadablePage(page, name, isWritable, hasReadPermissionRestriction, permissionDetail, pageFirstRevision, pageLastRevision)
        case (Some(page), "diff", true, _) =>
          renderDiffPage(name)

        case (Some(page), "raw", true, _) => Ok(page.content).withHeaderRobotNoIndexNoFollow
        case (Some(page), "history", true, _) => Ok(views.html.Wiki.history(name, Page.selectHistory(name))).withHeaderRobotNoIndexNoFollow
        case (Some(page), "blame", true, _) =>
          import com.aha00a.colors.GradientPreset
          val blame = Page.selectHistoryStream(name, new Blame[PageMetaData, String](), (blame: Blame[PageMetaData, String], p) => blame.next(new PageMetaData(p), p.content.splitLinesSeq()))
          val seqRevision: Seq[Long] = blame.seqBlameLine.map(_.metaData.revision).distinct.sorted
          val mapRevisionColor = seqRevision.map(v => (v, GradientPreset.ahaWikiBlame.getColor(seqRevision.indexOf(v).toDouble / seqRevision.size).toHashString)).toMap
          Ok(views.html.Wiki.blame(blame, mapRevisionColor, isWritable, pageFirstRevision, pageLastRevision)).withHeaderRobotNoIndexNoFollow

        case (Some(page), "edit", _, true) =>
          val (initialEditorText, partialRange) = buildEditFormState(page.content, request)
          Ok(views.html.Wiki.edit(
            page,
            applicationConf,
            initialEditorText,
            partialRange,
            canManagePagePermission = isAdmin,
            pagePermissionDefault = pagePermissionModeKeep,
            pagePermissionSummary = pagePermissionSummary(name),
            isNewPage = false,
          )).withHeaderRobotNoIndexNoFollow
        case (Some(page), "rename", _, true) => Ok(views.html.Wiki.rename(page)).withHeaderRobotNoIndexNoFollow
        case (Some(page), "delete", _, true) => Ok(views.html.Wiki.delete(page)).withHeaderRobotNoIndexNoFollow
        case _ => Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaderRobotNoIndexNoFollow
      }
    }
  }

  def watch(nameEncoded: String): WebSocket = WebSocket.acceptOrResult[String, String] { request =>
    Future {
      val name = decodeWikiName(nameEncoded)
      implicit val provider: RequestWrapper = RequestWrapper()(request)

      def latestRevisionAndContent(): (Long, Option[PageContent]) = {
        database.withConnection { implicit connection =>
          implicit val site: Site = SiteLogic.get(request.host)
          val latest = Page.selectLastRevision(name)
          (latest.map(_.revision).getOrElse(0L), latest.map(v => PageContent(v.content)))
        }
      }

      val siteForWs = database.withConnection { implicit connection =>
        SiteLogic.get(request.host)
      }
      val (_, pageLastRevisionContent) = latestRevisionAndContent()
      val isReadable = database.withConnection { implicit connection =>
        implicit val site: Site = siteForWs
        val ctxSite = ContextSite.empty()(database, actorAhaWiki, applicationConf, ahaWikiCache, site)
        val permission = WikiPermission()(provider, connection, ctxSite)
        permission.isReadable(name, pageLastRevisionContent)
      }
      if (!isReadable) {
        logger.warn(s"WebSocket watch denied: host=${request.host}, name=$name, uri=${request.uri}, remote=${request.remoteAddress}")
        Left(Forbidden("Permission denied."))
      } else {
        val connectionId = UUID.randomUUID().toString
        val currentUser = SessionLogic.getUser(request)
        val nickname = currentUser.map(_.nickname).filter(_.nonEmpty).getOrElse("")
        val profileImageUrl = currentUser.flatMap { user =>
          database.withConnection { implicit connection =>
            User.selectBySeq(user.seq).flatMap(_.profileImageUrl).filter(_.nonEmpty)
          }
        }.getOrElse("")
        val source = Source.queue[String](32, OverflowStrategy.dropHead)
        val sink = Sink.foreach[String] { incoming =>
          val payload = Try(Json.parse(incoming).asOpt[play.api.libs.json.JsObject].getOrElse(Json.obj())).getOrElse(Json.obj())
          (payload \ "type").asOpt[String] match {
            case Some("cursor.move") =>
              val x = (payload \ "x").asOpt[play.api.libs.json.JsNumber].map(_.value.toDouble).getOrElse(0d).max(0d).min(1d)
              val y = (payload \ "y").asOpt[play.api.libs.json.JsNumber].map(_.value.toDouble).getOrElse(0d).max(0d).min(1d)
              val outgoing = Json.obj(
                "type" -> "cursor.move",
                "siteId" -> siteForWs.seq,
                "pageId" -> name,
                "senderId" -> connectionId,
                "x" -> x,
                "y" -> y,
                "ts" -> System.currentTimeMillis()
              ).toString()
              PageCursorHub.broadcast(roomKeyForPage(siteForWs.seq, name), connectionId, outgoing)

            case Some("cursor.hello") =>
              val saveSenderId = (payload \ "saveSenderId").asOpt[String].map(_.trim).filter(_.nonEmpty)
              PageCursorHub.setSaveSenderId(roomKeyForPage(siteForWs.seq, name), connectionId, saveSenderId)
              val hello = Json.obj("type" -> "cursor.hello", "senderId" -> connectionId, "nickname" -> nickname, "profileImageUrl" -> profileImageUrl).toString()
              PageCursorHub.broadcast(roomKeyForPage(siteForWs.seq, name), connectionId, hello)

            case _ =>
          }
        }
        val flow = Flow.fromSinkAndSourceCoupledMat(sink, source)(Keep.right).mapMaterializedValue { queue =>
          PageCursorHub.subscribe(roomKeyForPage(siteForWs.seq, name), connectionId, queue)
          val hello = Json.obj("type" -> "cursor.hello", "senderId" -> connectionId, "nickname" -> nickname, "profileImageUrl" -> profileImageUrl).toString()
          queue.offer(hello)
          PageCursorHub.broadcast(roomKeyForPage(siteForWs.seq, name), connectionId, hello)
          NotUsed
        }.watchTermination() { (_, done) =>
          done.onComplete(_ => PageCursorHub.unsubscribe(roomKeyForPage(siteForWs.seq, name), connectionId))(executionContext)
          NotUsed
        }
        Right(flow)
      }
    }
  }

  private def decodeWikiName(nameEncoded: String): String =
    URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")

  private def buildEditFormState(pageContent: String, request: RequestHeader): (String, Option[(Int, Int)]) = {
    val lineStart = request.getQueryString("lineStart").flatMap(v => scala.util.Try(v.toInt).toOption).filter(_ > 0)
    val lineEnd = request.getQueryString("lineEnd").flatMap(v => scala.util.Try(v.toInt).toOption).filter(_ > 0)

    val partialRange = for {
      start <- lineStart
      end <- lineEnd
      if end >= start
    } yield (start, end)

    val pageLines = pageContent.split("""\r\n|\n""", -1).toSeq
    val partialRangeInBounds = partialRange.filter { case (_, end) => end <= pageLines.length + 1 }
    val initialEditorText = partialRangeInBounds
      .map { case (start, end) => pageLines.slice(start - 1, end - 1).mkString("\n") }
      .getOrElse(pageContent)

    (initialEditorText, partialRangeInBounds)
  }

  private def renderNotFoundPage(name: String, isWritable: Boolean, pageFirstRevision: Option[Page], pageLastRevision: Option[Page])
                                (implicit wikiContext: ContextWikiPage, connection: Connection, site: Site): Result = {
    val additionalInfo = getAhaMarkAdditionalInfo(name)

    def render(content: String): Result = {
      val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
      NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))
    }

    DefaultPageLogic.getOption(name).map(content => render(content)).getOrElse(render(WikiSnippet.notFound(name)))
  }

  private def renderReadablePage(page: Page,
                                 name: String,
                                 isWritable: Boolean,
                                 hasReadPermissionRestriction: Boolean,
                                 permissionDetail: Option[WikiPermissionDetail],
                                 pageFirstRevision: Option[Page],
                                 pageLastRevision: Option[Page])
                                (implicit request: Request[AnyContent], wikiContext: ContextWikiPage, connection: Connection, site: Site): Result = {
    try {
      val pageContent: PageContent = PageContent(page.content)
      val additionalInfo = getAhaMarkAdditionalInfo(name)
      pageContent.redirect match {
        case Some(directive) =>
          val redirectFromEditLink = s"/w/${URLEncoder.encode(page.name, "utf-8").replace("+", "%20")}?action=edit"
          val message = s"""Redirected from <a href="$redirectFromEditLink">${page.name}</a>"""
          val newMessage = request.flash.get("success").map(v => v + "<br/>" + message).getOrElse(message)
          Redirect(URLEncoder.encode(directive, "utf-8").replace("+", "%20")).flashing("success" -> newMessage)
        case None =>
          SessionLogic.getUser(request).foreach(user => models.tables.UserViewHistory.insert(user.seq, site.seq, page.name))
          val description = pageContent.content.replaceAll("""[^가-힣\w:/+,.()-]+""", " ").split("\\s+").filter(_.isNotNullOrEmpty).take(50).mkString("", " ", "...")
          Ok(pageContent.interpreter match {
              case Some("Paper") =>
                val contentInterpreted = Interpreters.toHtmlString(page.content)
                views.html.Wiki.view(name, description, "Paper", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision, hasReadPermissionRestriction, permissionDetail)
              case None | Some("Wiki") =>
                val contentInterpreted = Interpreters.toHtmlString(page.content + additionalInfo)
                views.html.Wiki.view(name, description, "Wiki", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision, hasReadPermissionRestriction, permissionDetail)
              case _ =>
                val contentInterpreted = s"""<h1>$name</h1>""" + Interpreters.toHtmlString(page.content) + Interpreters.toHtmlString(additionalInfo)
                views.html.Wiki.view(name, description, pageContent.interpreter.getOrElse(""), contentInterpreted, isWritable, pageFirstRevision, pageLastRevision, hasReadPermissionRestriction, permissionDetail)
            })
      }
    }
    finally {
      //noinspection SimplifyBoolean
      if (true || environment.mode == Mode.Dev && request.isLocalhost)
        actorAhaWiki ! Calculate(site, name)
    }
  }
  private def renderDiffPage(name: String)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage, connection: Connection, site: Site): Result = {
    val after = request.getQueryString("after").getOrElse("0").toInt
    val before = request.getQueryString("before").getOrElse((after - 1).toString).toInt

    val beforePage = Page.selectSpecificRevision(name, before)
    val afterPage = Page.selectSpecificRevision(name, after)

    val beforeContent = beforePage.map(_.content).getOrElse("").split("""(\r\n|\n)""").toSeq
    val afterContent = afterPage.map(_.content).getOrElse("").split("""(\r\n|\n)""").toSeq

    val beforeComment = beforePage.map(_.comment).getOrElse("")
    val afterComment = afterPage.map(_.comment).getOrElse("")

    val diff = DiffUtils.diff(beforeContent.asJava, afterContent.asJava)
    val unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(name, name, beforeContent.asJava, diff, 10).asScala.mkString("\n")
    Ok(views.html.Wiki.diff(name, before, beforeComment, after, afterComment, unifiedDiff)).withHeaderRobotNoIndexNoFollow
  }

  private case class MarkupContext(schema: String, backlinks: Boolean, similarPages: Boolean, adjacentPages: Int)

  def getAhaMarkAdditionalInfo(name: String)(implicit wikiContext: ContextWikiPage, connection: Connection, site: Site): String = {
    import models.tables.CalculatedLink

    val schemaMarkup = getMarkupSchema(name)
    val hasBacklinks = CalculatedLink.selectDstLimit1(name).isDefined
    val similarPages = CalculatedCosineSimilarity.select(name).view.filter(_.and(wikiContext.pageCanSee)).take(1).toSeq
    val adjacentPagesCount = Adjacent.getSeqLinkFiltered(name).length

    val context = MarkupContext(
      schema = schemaMarkup.toOption.map(generateSchemaMarkup).getOrElse(""),
      backlinks = hasBacklinks,
      similarPages = similarPages.nonEmpty,
      adjacentPages = adjacentPagesCount,
    )

    if (isEmptyMarkup(context)) "" else generateFullMarkup(context)
  }

  private def generateSchemaMarkup(schema: String): String =
    s"=== [schema:Schema Schema] === #Schema-Generated.generated\n$schema"

  private def generateBacklinksMarkup: String =
    """=== Backlinks === #Backlinks-Generated.generated
      |<Columns count="3" gap="16" minWidth="220">
      |[[Backlinks]]
      |</Columns>""".stripMargin

  private def generateSimilarPagesMarkup: String =
    "=== Similar Pages === #Similar-Pages-Generated.generated\nSimilar pages by cosine similarity. Words after page name are term frequency.\n[[SimilarPages]]"

  private def isEmptyMarkup(context: MarkupContext): Boolean =
    context.schema.isEmpty && !context.backlinks && !context.similarPages && context.adjacentPages == 0

  private def generateFullMarkup(context: MarkupContext): String = {
    val backlinksMarkup = if (context.backlinks) generateBacklinksMarkup else ""
    val similarPagesMarkup = if (context.similarPages) generateSimilarPagesMarkup else ""

    s"""
       |== See Also == #See-Also-Generated.generated
       |${context.schema}
       |
       |$backlinksMarkup
       |
       |$similarPagesMarkup
       |
       |[[Html(<div style="clear: both;"></div>)]]
       |=== Adjacent Pages === #Adjacent-Pages-Generated.generated
       |[[AdjacentPages]]
       |""".stripMargin
  }

  private def getMarkupSchema(name: String)(implicit wikiContext: ContextWikiPage, connection: Connection, site: Site) = {
    import models.tables.CalculatedSchemaOrg
    val listSchemaOrg = CalculatedSchemaOrg.selectWhereValue(name).filter(s => s.and(wikiContext.pageCanSee))
    val mapClsList = listSchemaOrg.groupBy(_.cls)
    mapClsList.keys.toSeq.sorted.map(k => {
      s"""==== [schema:$k $k] ==== #$k-Generated.generated
         |<Columns count="3" gap="16" minWidth="220">
         |${mapClsList(k).map(t => s""" 1. [schema:${t.prop} ${t.prop}] of ["${t.page}"]""").mkString("\n")}
         |</Columns>
         |""".stripMargin
    }).mkString("\n")
  }


  def save(nameEncoded: String): Action[AnyContent] = Action.async { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")

    val (revision, body, comment, minorEdit, recaptcha, partialLineStart, partialLineEnd, saveSenderId, pagePermissionModeRaw) = Form(tuple(
      "revision" -> number,
      "text" -> text,
      "comment" -> text,
      "minorEdit" -> optional(boolean),
      "recaptcha" -> text,
      "lineStart" -> optional(number),
      "lineEnd" -> optional(number),
      "saveSenderId" -> optional(text),
      "pagePermissionMode" -> optional(text),
    )).bindFromRequest.get
    val isMinorEdit = minorEdit.getOrElse(false)
    val secretKey = applicationConf.AhaWiki.google.reCAPTCHA.secretKey()
    val remoteAddress = request.remoteAddressWithXRealIp

    def doSave() = {
      database.withConnection { implicit connection =>
        implicit val site: Site = SiteLogic.get(request.host)
        implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
        implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
        val latestPage = Page.selectLastRevision(name)
        val (latestText, latestRevision, latestTime) = latestPage.map(w => (w.content, w.revision, w.dateTime)).getOrElse(("", 0, LocalDateTime.now()))
        val canManagePagePermission = isAdmin
        val pagePermissionMode = if (canManagePagePermission) {
          normalizePagePermissionMode(pagePermissionModeRaw, latestPage.isEmpty)
        } else {
          pagePermissionModeKeep
        }
        val willChangePagePermission = pagePermissionMode != pagePermissionModeKeep

        if (!WikiPermission().isWritable(name, latestPage.map(page => PageContent(page.content)))) {
          Forbidden("Permission denied.")
        } else if (revision != latestRevision) {
          Conflict("revision != latestRevision")
        } else {
          val mergedBodyEither = (partialLineStart, partialLineEnd) match {
            case (Some(lineStart), Some(lineEnd)) =>
              mergePartialBody(latestText, body, lineStart, lineEnd)
            case (None, None) =>
              Right(body)
            case _ =>
              Left("partial line range mismatch")
          }
          mergedBodyEither match {
            case Left(error) =>
              BadRequest(error)
            case Right(mergedBody) if mergedBody == latestText && !willChangePagePermission =>
              BadRequest("body == latestText")
            case Right(mergedBody) =>
              val now = LocalDateTime.now()
              val bodyChanged = mergedBody != latestText
              val nextRevision = if (bodyChanged) revision + 1 else revision
              if (bodyChanged) {
                PageLogic.insert(name, nextRevision, now, comment, isMinorEdit, mergedBody)
              }
              if (willChangePagePermission) {
                provider.getUser.map(_.email).filter(_.nonEmpty).foreach { actor =>
                  applyPagePermissionMode(name, pagePermissionMode, actor)
                }
              }

              val editorNickname = provider.getUser.map(_.nickname).getOrElse("Guest")
              val pageUpdatedPayload = Json.obj(
                "type" -> "page.updated",
                "pageName" -> name,
                "revision" -> nextRevision,
                "editorNickname" -> editorNickname,
                "dateInserted" -> now.toString
              ).toString()
              PageCursorHub.broadcastPageUpdated(roomKeyForPage(site.seq, name), saveSenderId.map(_.trim).filter(_.nonEmpty), pageUpdatedPayload)

              name match {
                case ".header" => ahaWikiCache.Header.invalidate()
                case ".footer" => ahaWikiCache.Footer.invalidate()
                case ".config" => ahaWikiCache.Config.invalidate()
                case _ => // do nothing
              }

              implicit val tupleDatabaseSite: (Database, Site) = (database, site)
              ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.invalidate()

              Ok("")
          }
        }
      }
    }

    if (secretKey.isNotNullOrEmpty && recaptcha.isNotNullOrEmpty) {
      wsClient.url("https://www.google.com/recaptcha/api/siteverify").post(Map(
        "secret" -> Seq(secretKey),
        "response" -> Seq(recaptcha),
        "remoteip" -> Seq(remoteAddress)
      )).map(response => {
        logger.info(response.body.replaceAll("""\s+""", " "))
        val json: JsValue = response.json
        if (!(json \ "success").as[Boolean]) {
          val errorCodes: Seq[String] = (json \ "error-codes").as[Seq[String]]
          logger.error(s"robot - ${errorCodes.mkString("\t")}")
          Forbidden("reCAPTCHA failed")
        } else {
          doSave()
        }
      })
    } else {
      Future {
        doSave()
      }
    }
  }

  private def mergePartialBody(latestText: String, partialBody: String, lineStart: Int, lineEnd: Int): Either[String, String] = {
    if (lineStart <= 0 || lineEnd < lineStart) {
      Left("invalid partial line range")
    } else {
      val latestLines = latestText.split("""\r\n|\n""", -1).toVector
      if (lineEnd > latestLines.length + 1) {
        Left("partial line range out of bounds")
      } else {
        val partialLines = stripGeneratedSeeAlso(partialBody.split("""\r\n|\n""", -1).toVector)
        val mergedLines = latestLines.take(lineStart - 1) ++ partialLines ++ latestLines.drop(lineEnd - 1)
        Right(mergedLines.mkString("\n"))
      }
    }
  }

  private def stripGeneratedSeeAlso(lines: Vector[String]): Vector[String] = {
    val seeAlsoGeneratedLineIndex = lines.indexWhere(_.contains("#See-Also-Generated"))
    if (seeAlsoGeneratedLineIndex == -1) lines
    else lines.take(seeAlsoGeneratedLineIndex)
  }


  def delete(): Action[AnyContent] = Action { implicit request =>
    val name = Form("name" -> text).bindFromRequest.get
    database.withTransaction { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      Page.selectLastRevision(name) match {
        case Some(page) =>
          if (WikiPermission().isWritable(name, PageContent(page.content))) {
            implicit val tupleDatabaseSite: (Database, Site) = (database, site)
            ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.invalidate()
            deletePageAttachments(site.seq, name) match {
              case Left(error) =>
                logger.error(error)
                throw new RuntimeException(error)
              case Right(_) =>
                Page.deleteWithRelatedData(name)
                Ok("")
            }
          } else {
            Forbidden("")
          }
        case None =>
          Forbidden("")
      }
    }
  }

  def deleteLastRevision(): Action[AnyContent] = Action { implicit request =>
    database.withTransaction { implicit connection =>
      val name = Form("name" -> text).bindFromRequest.get
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      Page.selectLastRevision(name) match {
        case Some(page) =>
          if (WikiPermission().isWritable(name, PageContent(page.content))) {
            implicit val tupleDatabaseSite: (Database, Site) = (database, site)
            ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.invalidate()

            Page.deleteSpecificRevisionWithRelatedData(name, page.revision)
            actorAhaWiki ! Calculate(site, name)
            Ok("")
          } else {
            Forbidden("")
          }
        case None =>
          NotFound("")
      }
    }
  }

  val regexGoogleSpreadsheetUrl: Regex = """https://docs.google.com/spreadsheets/d/([^/]+)(/(edit(#gid=0)?)?)?""".r

  def padColumns[T](matrix: Seq[Seq[T]], default: T): Seq[Seq[T]] = {
    val maxLength = matrix.map(_.length).max
    matrix.map(_.padTo(maxLength, default))
  }

  def syncGoogleSpreadsheet: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      val (pageName, url, sheetName) = Form(tuple("pageName" -> text, "url" -> text, "sheetName" -> text)).bindFromRequest.get
      Page.selectLastRevision(pageName) match {
        case Some(page) =>
          implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(pageName)
          implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
          val pageContent = PageContent(page.content)
          if (WikiPermission().isWritable(pageName, pageContent)) {
            val extractConvertApplyInterpreterRefresh = new ExtractConvertInjectInterpreterCustom(s => {
              val pageContentChunk = PageContent(s)
              if (url == pageContentChunk.argument.getOrElse(0, "") && sheetName == pageContentChunk.argument.getOrElse(1, "")) {
                url match {
                  case regexGoogleSpreadsheetUrl(id, _, _, _) =>
                    val googleSheetsApiKey = applicationConf.AhaWiki.google.credentials.api.GoogleSheetsAPI.key()
                    val futureSpreadsheet: Future[Seq[Seq[String]]] = GoogleSpreadsheetApi.readSpreadSheet(googleSheetsApiKey, id, sheetName)
                    val spreadsheet: Seq[Seq[String]] = Await.result(futureSpreadsheet, 5 seconds)
                    s"[[[#!Map $url $sheetName\n${SupercsvUtil.toTsvString(padColumns(spreadsheet, ""))}]]]"
                  case _ =>
                    s
                }
              } else {
                s
              }
            })
            val body = extractConvertApplyInterpreterRefresh.inject(extractConvertApplyInterpreterRefresh.extract(pageContent.content))
            if (pageContent.content != body) {
              PageLogic.insert(pageName, page.revision + 1, LocalDateTime.now(), "Sync Google Spreadsheet", isMinorEdit = false, body)
              Ok("")
            } else {
              Ok("NotChanged")
            }
          } else {
            Forbidden("")
          }
        case None =>
          NotFound("")
      }
    }
  }


  def rename(): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      val (name, newName) = Form(tuple("name" -> text, "newName" -> text)).bindFromRequest.get
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      (Page.selectLastRevision(name), Page.selectLastRevision(newName)) match {
        case (Some(page), None) =>
          if (WikiPermission().isWritable(name, PageContent(page.content))) {
            implicit val tupleDatabaseSite: (Database, Site) = (database, site)
            ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.invalidate()

            Page.rename(name, newName)
            PageLogic.insert(name, 1, LocalDateTime.now(), "redirect", isMinorEdit = false, s"#!redirect $newName")
            actorAhaWiki ! Calculate(site, newName)
            Ok("")
          } else {
            Forbidden("")
          }
        case (Some(_), Some(_)) => Conflict("")
        case _ => Forbidden("")
      }
    }
  }


  def preview(): Action[AnyContent] = Action { implicit request =>
    val (name, body, partialLineStart, partialLineEnd) = Form(tuple(
      "name" -> text,
      "text" -> text,
      "lineStart" -> optional(number),
      "lineEnd" -> optional(number)
    )).bindFromRequest.get
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage.preview(name)
      val isPartialEditPreview = partialLineStart.isDefined && partialLineEnd.isDefined
      val additionalInfo = if (isPartialEditPreview) "" else getAhaMarkAdditionalInfo(name)
      Ok(s"""<div class="wikiContent preview"><div class="limitWidth">${Interpreters.toHtmlString(body + additionalInfo)}</div></div>""")
    }
  }

  def uploadAttachment(): Action[MultipartFormData[TemporaryFile]] = Action(parse.multipartFormData) { implicit request =>
    import com.amazonaws.services.s3.model.ObjectMetadata
    import logics.wikis.macros.S3AttachmentUrlLogic
    import play.api.libs.json.Json

    val pageNameOption = request.body.dataParts.get("pageName").flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)
    val fileOption = request.body.file("file")

    (pageNameOption, fileOption) match {
      case (Some(pageName), Some(filePart)) =>
        database.withConnection { implicit connection =>
          getWritablePageContext(pageName) match {
            case Left(result) => result
            case Right((site0, contextWikiPage0, provider0)) =>
              implicit val site: Site = site0
              implicit val contextWikiPage: ContextWikiPage = contextWikiPage0
              implicit val provider: RequestWrapper = provider0
              val originalFileName = filePart.filename.trim
              if (originalFileName.isEmpty) {
                BadRequest("invalid file name")
              } else {
                val extension = originalFileName.split('.').lastOption.getOrElse("bin").replaceAll("[^a-zA-Z0-9]", "").toLowerCase
                val objectKey = buildAttachmentObjectKey(
                  site.seq,
                  pageName,
                  originalFileName = originalFileName,
                  extension = if (extension.nonEmpty) extension else "bin",
                )

                val contentType = filePart.contentType.getOrElse("application/octet-stream")
                val contentLength = filePart.fileSize
                val metadata = new ObjectMetadata()
                metadata.setContentType(contentType)
                metadata.setContentLength(contentLength)

                val bucket = applicationConf.AhaWiki.aws.s3.bucket()
                val amazonS3 = buildAmazonS3Client()
                insertInitiatedAttachment(
                  siteSeq = site.seq,
                  pageName = pageName,
                  originalFilename = originalFileName,
                  objectKey = objectKey,
                  contentType = contentType,
                  fileSize = contentLength,
                )

                try {
                  val inputStream = Files.newInputStream(filePart.ref.path)
                  try {
                    val putResult = amazonS3.putObject(bucket, objectKey, inputStream, metadata)
                    Attachment.markUploaded(objectKey, Option(putResult.getETag))
                  } finally {
                    inputStream.close()
                  }

                  val fileUrl = S3AttachmentUrlLogic.generatePresignedUrl(objectKey).toOption.getOrElse("")
                  Ok(Json.obj(
                    "objectKey" -> objectKey,
                    "attachmentMacro" -> s"[[Attachment(${toAttachmentMacroArgument(objectKey, site.seq, pageName)})]]",
                    "fileUrl" -> fileUrl,
                    "contentType" -> contentType,
                  ))
                } catch {
                  case error: Throwable =>
                    logger.error(s"uploadAttachment failed. objectKey=$objectKey", error)
                    Attachment.markFailed(objectKey)
                    InternalServerError("File upload failed.")
                }
              }
          }
        }
      case _ =>
        BadRequest("pageName and file are required")
    }
  }

  def pageAttachments(): Action[AnyContent] = Action { implicit request =>
    import play.api.libs.json.Json

    val pageName = request.getQueryString("pageName").map(_.trim).getOrElse("")
    if (pageName.isEmpty) {
        BadRequest("pageName is required")
    } else {
      database.withConnection { implicit connection =>
        getWritablePageContext(pageName) match {
          case Left(result) => result
          case Right((site0, contextWikiPage0, provider0)) =>
            implicit val site: Site = site0
            implicit val contextWikiPage: ContextWikiPage = contextWikiPage0
            implicit val provider: RequestWrapper = provider0
            val dbAttachments = Attachment.selectUploadedByPage(site.seq, pageName).map { attachment =>
              val presignedUrlOption = logics.wikis.macros.S3AttachmentUrlLogic.generatePresignedUrl(attachment.objectKey).toOption
              (attachment, presignedUrlOption)
            }
            val dbObjectKeys = dbAttachments.map(_._1.objectKey).toSet
            val s3OnlyObjects = listPageAttachmentObjectKeysFromS3(site.seq, pageName).filterNot(dbObjectKeys.contains)
            val s3OnlyJson = s3OnlyObjects.map { objectKey =>
              val presignedUrlOption = logics.wikis.macros.S3AttachmentUrlLogic.generatePresignedUrl(objectKey).toOption
              val inferredFilename = objectKey.split("/").toSeq.lastOption.getOrElse(objectKey)
              Json.obj(
                "objectKey" -> objectKey,
                "originalFilename" -> inferredFilename,
                "contentType" -> "unknown",
                "fileSize" -> 0,
                "fileUrl" -> presignedUrlOption,
                "integrityStatus" -> "S3_ONLY",
                "attachmentMacro" -> s"[[Attachment(${toAttachmentMacroArgument(objectKey, site.seq, pageName)})]]",
              )
            }
            Ok(Json.obj(
              "attachments" -> (dbAttachments.map { case (attachment, presignedUrlOption) => Json.obj(
                "objectKey" -> attachment.objectKey,
                "originalFilename" -> attachment.originalFilename,
                "contentType" -> attachment.contentType,
                "fileSize" -> attachment.fileSize,
                "fileUrl" -> presignedUrlOption,
                "integrityStatus" -> (if (presignedUrlOption.isDefined) "OK" else "DB_ONLY"),
                "attachmentMacro" -> s"[[Attachment(${toAttachmentMacroArgument(attachment.objectKey, site.seq, pageName)})]]",
              )} ++ s3OnlyJson)
            ))
        }
      }
    }
  }

  def deleteAttachment(): Action[AnyContent] = Action { implicit request =>
    import play.api.libs.json.Json

    val form = Form(tuple("pageName" -> text, "objectKey" -> text)).bindFromRequest
    form.fold(_ => BadRequest("invalid form"), {
      case (pageNameRaw, objectKeyRaw) =>
        val pageName = pageNameRaw.trim
        val objectKey = objectKeyRaw.trim
        if (pageName.isEmpty || objectKey.isEmpty) {
          BadRequest("pageName and objectKey are required")
        } else {
          database.withConnection { implicit connection =>
            getWritablePageContext(pageName) match {
              case Left(result) => result
              case Right((site0, contextWikiPage0, provider0)) =>
                implicit val site: Site = site0
                implicit val contextWikiPage: ContextWikiPage = contextWikiPage0
                implicit val provider: RequestWrapper = provider0
                val resolvedObjectKey = resolveAttachmentObjectKey(site.seq, pageName, objectKey)
                Attachment.selectByObjectKey(site.seq, pageName, resolvedObjectKey) match {
                  case None =>
                    NotFound("Attachment not found")
                  case Some(_) =>
                    val amazonS3 = buildAmazonS3Client()
                    val bucket = applicationConf.AhaWiki.aws.s3.bucket()

                    try {
                      amazonS3.deleteObject(bucket, resolvedObjectKey)
                      Attachment.markDeleted(resolvedObjectKey)
                      Ok(Json.obj("ok" -> true, "objectKey" -> resolvedObjectKey))
                    } catch {
                      case error: Throwable =>
                        logger.error(s"deleteAttachment failed. objectKey=$resolvedObjectKey", error)
                        InternalServerError("Attachment delete failed.")
                    }
                }
            }
          }
        }
    })
  }

  def uploadClipboardImage(): Action[AnyContent] = Action { implicit request =>
    import com.amazonaws.services.s3.model.ObjectMetadata
    import logics.wikis.macros.S3AttachmentUrlLogic
    import play.api.libs.json.Json

    val form = Form(tuple("pageName" -> text, "dataUrl" -> text)).bindFromRequest
    form.fold(_ => BadRequest("invalid form"), {
      case (pageName, dataUrl) =>
        database.withConnection { implicit connection =>
          getWritablePageContext(pageName) match {
            case Left(result) => result
            case Right((site0, contextWikiPage0, provider0)) =>
              implicit val site: Site = site0
              implicit val contextWikiPage: ContextWikiPage = contextWikiPage0
              implicit val provider: RequestWrapper = provider0
              if (!dataUrl.startsWith("data:image/")) {
                BadRequest("only image dataUrl is supported")
              } else {
                val pattern = """^data:(image/[-+.a-zA-Z0-9]+);base64,(.+)$""".r
                dataUrl match {
                  case pattern(contentType, base64Data) =>
                    val bytes = Base64.getDecoder.decode(base64Data)
                    val extension = contentType.split("/").lastOption.getOrElse("png").replace("+xml", "").replaceAll("[^a-zA-Z0-9]", "").toLowerCase
                    val objectKey = buildAttachmentObjectKey(
                      site.seq,
                      pageName,
                      originalFileName = "clipboard",
                      extension = extension,
                    )

                    val amazonS3 = buildAmazonS3Client()
                    val bucket = applicationConf.AhaWiki.aws.s3.bucket()
                    val metadata = new ObjectMetadata()
                    metadata.setContentType(contentType)
                    metadata.setContentLength(bytes.length)
                    insertInitiatedAttachment(
                      siteSeq = site.seq,
                      pageName = pageName,
                      originalFilename = "clipboard",
                      objectKey = objectKey,
                      contentType = contentType,
                      fileSize = bytes.length,
                    )

                    try {
                      val inputStream = new ByteArrayInputStream(bytes)
                      try {
                        val putResult = amazonS3.putObject(bucket, objectKey, inputStream, metadata)
                        Attachment.markUploaded(objectKey, Option(putResult.getETag))
                      } finally {
                        inputStream.close()
                      }

                      val imageUrl = S3AttachmentUrlLogic.generatePresignedUrl(objectKey).toOption.getOrElse("")
                      Ok(Json.obj(
                        "objectKey" -> objectKey,
                        "attachmentMacro" -> s"[[Attachment(${toAttachmentMacroArgument(objectKey, site.seq, pageName)})]]",
                        "imageUrl" -> imageUrl,
                      ))
                    } catch {
                      case error: Throwable =>
                        logger.error(s"uploadClipboardImage failed. objectKey=$objectKey", error)
                        Attachment.markFailed(objectKey)
                        InternalServerError("Image upload failed.")
                    }
                  case _ =>
                    BadRequest("invalid image dataUrl")
                }
            }
          }
        }
    })
  }
}
