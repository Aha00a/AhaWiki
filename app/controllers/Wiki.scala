package controllers

import logics.wikis.PageNameUrl
import actors.ActorPageCalculator.Calculate
import org.apache.pekko.actor._
import org.apache.pekko.stream.scaladsl.{Flow, Sink, Source}
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.BooleanUtil
import com.aha00a.commons.utils.UriUtil
import com.aha00a.play.Implicits._
import com.github.difflib.DiffUtils
import com.github.difflib.UnifiedDiffUtils
import logics._
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
import models.tables.Config
import models.tables.Page
import models.tables.Permission
import models.tables.Site
import models.tables.User
import models.tables.UserEmail
import play.api.Environment
import play.api.Configuration
import play.api.Logging
import play.api.Mode
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.net.URLEncoder
import java.sql.Connection
import java.time.LocalDateTime
import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

object Wiki {
  private[controllers] def shouldNotifyTelegramForPageSave(isMinorEdit: Boolean, viaApi: Boolean): Boolean =
    !isMinorEdit && !viaApi
}

/**
 * Reading and writing a wiki page: view, save, delete, rename, and the preview the editor
 * asks for while typing.
 *
 * What used to sit alongside moved out by concern — [[WikiAttachment]] puts bytes in S3,
 * [[WikiRealtime]] holds the page WebSocket, and [[WikiSpreadsheet]] pulls in someone else's
 * service. The rendering helpers stayed because view and preview render the same way, and
 * the page-permission helpers stayed because view shows the summary and save applies it.
 *
 * `save` still reaches [[logics.PageCursorHub]] to announce the change. That is deliberate:
 * the hub is shared with the WebSocket rather than owned by it, so the two can live apart
 * without each keeping half the watchers.
 */
class Wiki @Inject()(implicit val
controllerComponents: ControllerComponents,
                     actorSystem: ActorSystem,
                     database: Database,
                     environment: Environment,
                     wikiActors: WikiActors,
                     applicationConf: ApplicationConf,
                     ahaWikiCache: AhaWikiCache,
                     wsClient: WSClient,
                     executionContext: ExecutionContext,
                     configuration: Configuration,
                     telegramLogic: TelegramLogic,
) extends BaseController with JsonResults with AdminAuth with Logging {
  implicit class RichResult(result: Result) {
    def withHeaderRobotNoIndexNoFollow: Result = result.withHeaders("X-Robots-Tag" -> "noindex, nofollow")
  }

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

  private def normalizePagePermissionMode(value: Option[String], isNewPage: Boolean): String = {
    val mode = value.map(_.trim).filter(pagePermissionModes.contains).getOrElse(pagePermissionModeKeep)
    if (isNewPage && mode == pagePermissionModeKeep) pagePermissionModeGeneral else mode
  }

  private def pagePermissionSummary(name: String)(implicit request: RequestHeader, connection: Connection, site: Site): Seq[(String, String)] = {
    val permissions = Permission.select()
    val permissionLogic = new PermissionLogic(permissions)
    val currentActors = SessionLogic.getUser(request)
      .map { user =>
        val emails = UserEmail.selectEmailsByUser(user.seq)
        if (emails.nonEmpty) emails else user.loginEmail.toSeq
      }
      .getOrElse(Seq.empty)
    val matchingTargetPermissions = permissionLogic.seq.filter(_.targetMatches(name))

    def actionName(action: Int): String =
      Permission.Action.values.find(_.id == action).map(_.toString).getOrElse(action.toString)

    def permissionLabel(permission: Permission): String = {
      val target = if (permission.targetType == Permission.TargetType.All) "*" else permission.target
      val actor = if (permission.actorType == Permission.ActorType.All) "*" else permission.actor
      s"${permission.targetType}($target), ${permission.actorType}($actor) -> ${actionName(permission.action)}"
    }

    val currentMatch = permissionLogic.matched(name, currentActors)
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
        Permission.upsert(Permission(name, Permission.TargetType.Exact, "", Permission.ActorType.All, Permission.Action.None.id))
        Permission.upsert(Permission(name, Permission.TargetType.Exact, actor, Permission.ActorType.Exact, Permission.Action.Admin.id))
        AhaWikiCacheMemoryPermission.invalidate(site.seq)
      case `pagePermissionModePrivateWrite` =>
        Permission.deleteExactTarget(name)
        Permission.upsert(Permission(name, Permission.TargetType.Exact, "", Permission.ActorType.All, Permission.Action.Read.id))
        Permission.upsert(Permission(name, Permission.TargetType.Exact, actor, Permission.ActorType.Exact, Permission.Action.Admin.id))
        AhaWikiCacheMemoryPermission.invalidate(site.seq)
      case _ =>
    }
  }

  def view(nameEncoded: String, revision: Int, action: String): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)

      val name = PageNameUrl.decode(nameEncoded)

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
      val isRenamable = wikiPermission.isRenamable(name)
      val isDeletable = wikiPermission.isDeletable(name)

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
            isRenamable = isRenamable,
            isDeletable = isDeletable,
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
          val mapRevisionColorLight = seqRevision.map(v => (v, GradientPreset.ahaWikiBlame.getColor(seqRevision.indexOf(v).toDouble / seqRevision.size).toHashString)).toMap
          val mapRevisionColorDark = seqRevision.map(v => (v, GradientPreset.ahaWikiBlameDark.getColor(seqRevision.indexOf(v).toDouble / seqRevision.size).toHashString)).toMap
          Ok(views.html.Wiki.blame(blame, mapRevisionColorLight, mapRevisionColorDark, isWritable, pageFirstRevision, pageLastRevision)).withHeaderRobotNoIndexNoFollow

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
            isRenamable = isRenamable,
            isDeletable = isDeletable,
          )).withHeaderRobotNoIndexNoFollow
        case (Some(page), "rename", _, _) if isRenamable => Ok(views.html.Wiki.rename(page)).withHeaderRobotNoIndexNoFollow
        case (Some(page), "delete", _, _) if isDeletable => Ok(views.html.Wiki.delete(page)).withHeaderRobotNoIndexNoFollow
        case _ => Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaderRobotNoIndexNoFollow
      }
    }
  }

  private def buildEditFormState(pageContent: String, request: RequestHeader): (String, Option[(Int, Int)]) = {
    val lineStart = request.getQueryString("lineStart").flatMap(v => scala.util.Try(v.toInt).toOption).filter(_ > 0)
    val lineEnd = request.getQueryString("lineEnd").flatMap(v => scala.util.Try(v.toInt).toOption).filter(_ > 0)

    // URL의 줄 번호는 directives가 제거된 content 기준이므로 raw content에서 슬라이싱할 때 directive 줄 수만큼 보정
    val numDirectiveLines = models.PageContent(pageContent).directives.length
    val adjustedLineStart = lineStart.map(_ + numDirectiveLines)
    val adjustedLineEnd = lineEnd.map(_ + numDirectiveLines)

    val partialRange = for {
      start <- adjustedLineStart
      end <- adjustedLineEnd
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
          val redirectFromEditLink = s"/w/${PageNameUrl.encode(page.name)}?action=edit"
          val message = s"""Redirected from <a href="$redirectFromEditLink">${page.name}</a>"""
          val newMessage = request.flash.get("success").map(v => v + "<br/>" + message).getOrElse(message)
          Redirect(routes.Wiki.view(UriUtil.encodeURIComponent(directive), 0, "")).flashing("success" -> newMessage)
        case None =>
          SessionLogic.getUser(request).foreach(user => models.tables.UserViewHistory.insert(user.seq, site.seq, page.name))
          val description = wikiContext.seqPageByPermission
            .find(_.name == name)
            .flatMap(_.description)
            .orElse(PageLogic.extractDescription(page.content))
            .getOrElse(name)
          Ok(pageContent.interpreter match {
              case None | Some("Wiki") =>
                val contentInterpreted = Interpreters.toHtmlString(page.content + additionalInfo)
                views.html.Wiki.view(name, description, "Wiki", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision, hasReadPermissionRestriction, permissionDetail)
              case Some("Paper") =>
                // additionalInfo(See Also 등)를 Paper 바깥에 독립 렌더링, 흰색 배경 보장
                val contentInterpreted = Interpreters.toHtmlString(page.content) +
                  s"""<div class="paperAdditionalSection">${Interpreters.toHtmlString(additionalInfo)}</div>"""
                views.html.Wiki.view(name, description, "Paper", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision, hasReadPermissionRestriction, permissionDetail)
              case _ =>
                val contentInterpreted = s"""<h1>$name</h1>""" + Interpreters.toHtmlString(page.content) + Interpreters.toHtmlString(additionalInfo)
                views.html.Wiki.view(name, description, pageContent.interpreter.getOrElse(""), contentInterpreted, isWritable, pageFirstRevision, pageLastRevision, hasReadPermissionRestriction, permissionDetail)
            })
      }
    }
    finally {
      //noinspection SimplifyBoolean
      if (BooleanUtil.random(0.1) || environment.mode == Mode.Dev && request.isLocalhost)
        wikiActors.pageCalculation ! Calculate(site, name)
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

  private case class MarkupContext(schema: String, backlinks: Boolean, twinPages: Boolean, similarPages: Boolean, adjacentPages: Int)

  def getAhaMarkAdditionalInfo(name: String)(implicit wikiContext: ContextWikiPage, connection: Connection, site: Site): String = {
    import models.tables.CalculatedLink

    val schemaMarkup = getMarkupSchema(name)
    val hasBacklinks = CalculatedLink.selectDstLimit1(name).isDefined
    val sameSiteSimilarPages = CalculatedCosineSimilarity.selectSameSite(name).view.filter(_.and(wikiContext.pageCanSee)).take(1).toSeq
    val crossSiteSimilarPages = CalculatedCosineSimilarity.selectCrossSite(name).view.filter(c => anonymousCanRead(c.site2, c.name2)).take(1).toSeq
    val adjacentPagesCount = Adjacent.getSeqLinkFiltered(name).length

    val context = MarkupContext(
      schema = schemaMarkup.toOption.map(generateSchemaMarkup).getOrElse(""),
      backlinks = hasBacklinks,
      twinPages = hasTwinPages(name),
      similarPages = sameSiteSimilarPages.nonEmpty || crossSiteSimilarPages.nonEmpty,
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

  private def generateTwinPagesMarkup: String =
    "=== Twin Pages === #Twin-Pages-Generated.generated\n[[TwinPages]]"

  private def isEmptyMarkup(context: MarkupContext): Boolean =
    context.schema.isEmpty && !context.backlinks && !context.twinPages && !context.similarPages && context.adjacentPages == 0

  private def generateFullMarkup(context: MarkupContext): String = {
    val backlinksMarkup = if (context.backlinks) generateBacklinksMarkup else ""
    val twinPagesMarkup = if (context.twinPages) generateTwinPagesMarkup else ""
    val similarPagesMarkup = if (context.similarPages) generateSimilarPagesMarkup else ""

    s"""
       |== See Also == #See-Also-Generated.generated
       |${context.schema}
       |
       |$backlinksMarkup
       |
       |$twinPagesMarkup
       |
       |$similarPagesMarkup
       |
       |[[Html(<div style="clear: both;"></div>)]]
       |=== Adjacent Pages === #Adjacent-Pages-Generated.generated
       |[[AdjacentPages]]
       |""".stripMargin
  }

  private def hasTwinPages(name: String)(implicit wikiContext: ContextWikiPage, connection: Connection): Boolean = {
    AhaWikiCacheMemoryDomainSite
      .getSites()(wikiContext.database)
      .filter(_.seq != wikiContext.site.seq)
      .exists { targetSite =>
        implicit val databaseSite: (Database, Site) = (wikiContext.database, targetSite)
        wikiContext.ahaWikiCache.PageMeta.SeqPageLatestSummary.get().exists(_.name == name) &&
          anonymousCanRead(targetSite.seq, name)
      }
  }

  private def anonymousCanRead(siteSeq: Long, pageName: String)(implicit wikiContext: ContextWikiPage, connection: Connection): Boolean =
    PermissionLogic.anonymousCanRead(siteSeq, pageName)(connection, wikiContext.database)

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
    val name = PageNameUrl.decode(nameEncoded)

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
    )).bindFromRequest().get
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
                provider.getUser.flatMap { user =>
                  UserEmail.selectPrimaryEmailByUser(user.seq).orElse(user.loginEmail)
                }.filter(_.nonEmpty).foreach { actor =>
                  applyPagePermissionMode(name, pagePermissionMode, actor)
                }
              }

              val editorNickname = provider.getUser.map(_.nickname).getOrElse("Guest")
              val shouldNotifyTelegram = bodyChanged && Wiki.shouldNotifyTelegramForPageSave(isMinorEdit = isMinorEdit, viaApi = false)
              if (shouldNotifyTelegram) {
                if (latestPage.isEmpty)
                  telegramLogic.notifyPageCreated(request.host, name, editorNickname, comment, Config.Query.Telegram.chatId())
                else
                  telegramLogic.notifyPageEdited(request.host, name, nextRevision, editorNickname, comment, Config.Query.Telegram.chatId())
              }
              val pageUpdatedPayload = Json.obj(
                "type" -> "page.updated",
                "pageName" -> name,
                "revision" -> nextRevision,
                "editorNickname" -> editorNickname,
                "dateInserted" -> now.toString
              ).toString()
              PageCursorHub.broadcastPageUpdated(PageCursorHub.roomKeyForPage(site.seq, name), saveSenderId.map(_.trim).filter(_.nonEmpty), pageUpdatedPayload)

              name match {
                case ".footer" => ahaWikiCache.Footer.invalidate()
                case ".config" => ahaWikiCache.Config.invalidate()
                case _ => // do nothing
              }

              implicit val tupleDatabaseSite: (Database, Site) = (database, site)
              ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()

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
    val name = Form("name" -> text).bindFromRequest().get
    database.withTransaction { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      Page.selectLastRevision(name) match {
        case Some(_) =>
          if (WikiPermission().isDeletable(name)) {
            implicit val tupleDatabaseSite: (Database, Site) = (database, site)
            ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()
            AttachmentLogic.deletePageAttachments(site.seq, name) match {
              case Left(error) =>
                logger.error(error)
                throw new RuntimeException(error)
              case Right(_) =>
                Page.deleteWithRelatedData(name)
                telegramLogic.notifyPageDeleted(request.host, name, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
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
      val name = Form("name" -> text).bindFromRequest().get
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      Page.selectLastRevision(name) match {
        case Some(page) =>
          if (WikiPermission().isDeletable(name)) {
            implicit val tupleDatabaseSite: (Database, Site) = (database, site)
            ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()

            Page.deleteSpecificRevisionWithRelatedData(name, page.revision)
            wikiActors.pageCalculation ! Calculate(site, name)
            telegramLogic.notifyLastRevisionDeleted(request.host, name, page.revision, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
            Ok("")
          } else {
            logger.warn(s"deleteLastRevision forbidden: host=${request.host}, name=$name, user=${provider.getUser.map(u => s"${u.nickname}(${u.loginEmail.getOrElse("no-email")})").getOrElse("anonymous")}, remote=${request.remoteAddress}")
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
      val (name, newName) = Form(tuple("name" -> text, "newName" -> text)).bindFromRequest().get
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      (Page.selectLastRevision(name), Page.selectLastRevision(newName)) match {
        case (Some(_), None) =>
          if (WikiPermission().isRenamable(name)) {
            implicit val tupleDatabaseSite: (Database, Site) = (database, site)
            ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()

            Page.rename(name, newName)
            PageLogic.insert(name, 1, LocalDateTime.now(), "redirect", isMinorEdit = false, s"#!redirect $newName")
            wikiActors.pageCalculation ! Calculate(site, newName)
            telegramLogic.notifyPageRenamed(request.host, name, newName, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
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
    )).bindFromRequest().get
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage.preview(name)
      val isPartialEditPreview = partialLineStart.isDefined && partialLineEnd.isDefined
      val hasPaperContent = body.contains("#!Paper")
      val hasGanttContent = body.trim.toLowerCase.startsWith("#!gantt")
      val hasKanbanContent = body.trim.toLowerCase.startsWith("#!kanban")
      val additionalInfo = if (isPartialEditPreview || hasPaperContent) "" else getAhaMarkAdditionalInfo(name)
      val contentHtml = if (hasGanttContent || hasKanbanContent)
        Interpreters.toHtmlString(body) + Interpreters.toHtmlString(additionalInfo)
      else
        Interpreters.toHtmlString(body + additionalInfo)
      Ok(s"""<div class="wikiContent preview"><div class="limitWidth">$contentHtml</div></div>""")
    }
  }

}
