package controllers

import actors.ActorPageCalculator.Calculate
import anorm.SqlParser._
import anorm._
import com.aha00a.play.Implicits.RichRequest
import com.aha00a.play.AnormSqlParser.{localDateTime, optionLong, optionStr}
import io.circe.Json
import io.circe.syntax._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import logics.wikis.PageLogic
import logics.wikis.RenderingMode
import logics.wikis.WikiPermission
import models.ContextWikiPage
import models.PageContent
import models.RequestWrapper
import models.WikiActors
import models.tables.Attachment
import models.tables.Page
import models.tables.Site
import models.tables.User
import models.tables.UserApiKey
import play.api.Logging
import play.api.db.Database
import play.api.mvc._

import java.net.URLDecoder
import java.security.MessageDigest
import java.time.LocalDateTime
import javax.inject._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

class ApiV1 @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
) extends BaseController with Logging {

  private def Ok(json: io.circe.Json): Result = Results.Ok(json.toString()).as(JSON)

  private def decodePageName(nameEncoded: String): String =
    URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")

  private def sha256Hex(text: String): String =
    MessageDigest.getInstance("SHA-256")
      .digest(text.getBytes("UTF-8"))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def isS3Configured: Boolean =
    Seq(
      applicationConf.AhaWiki.aws.AWS_REGION(),
      applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
      applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
      applicationConf.AhaWiki.aws.s3.bucket(),
    ).forall(_.trim.nonEmpty)

  private def buildAmazonS3Client(): com.amazonaws.services.s3.AmazonS3 = {
    import com.amazonaws.auth.AWSStaticCredentialsProvider
    import com.amazonaws.auth.BasicAWSCredentials
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

  private def sanitizeAttachmentPathSegment(v: String): String = {
    val sanitized = v.replaceAll("[^\\p{IsHangul}\\p{IsHan}\\p{IsHiragana}\\p{IsKatakana}a-zA-Z0-9._-]", "_")
    if (sanitized.nonEmpty) sanitized else "_"
  }

  private def listPageAttachmentObjectKeysFromS3(siteSeq: Long, pageName: String): Seq[String] = {
    if (!isS3Configured) {
      Seq.empty
    } else {
      val bucket = applicationConf.AhaWiki.aws.s3.bucket()
      val prefix = s"Attachment/$siteSeq/${sanitizeAttachmentPathSegment(pageName)}/"
      val request = new com.amazonaws.services.s3.model.ListObjectsV2Request()
        .withBucketName(bucket)
        .withPrefix(prefix)
        .withMaxKeys(200)
      val result = buildAmazonS3Client().listObjectsV2(request)
      result.getObjectSummaries.asScala.toSeq
        .map(_.getKey)
        .filter(key => key != null && key.nonEmpty && !key.endsWith("/"))
    }
  }

  private def deletePageAttachments(siteSeq: Long, pageName: String)(implicit connection: java.sql.Connection): Either[String, Unit] = {
    val objectKeysFromDb = Attachment.selectObjectKeysByPage(siteSeq, pageName)
    val objectKeysFromS3 = listPageAttachmentObjectKeysFromS3(siteSeq, pageName)
    val objectKeys = (objectKeysFromDb ++ objectKeysFromS3).map(_.trim).filter(_.nonEmpty).distinct

    val failedObjectKeys =
      if (!isS3Configured) {
        Seq.empty
      } else {
        val bucket = applicationConf.AhaWiki.aws.s3.bucket()
        val amazonS3 = buildAmazonS3Client()
        objectKeys.flatMap { objectKey =>
          Try(amazonS3.deleteObject(bucket, objectKey)) match {
            case Success(_) => None
            case Failure(error) =>
              logger.error(s"deletePageAttachments failed. pageName=$pageName objectKey=$objectKey", error)
              Some(objectKey)
          }
        }
      }
    if (failedObjectKeys.nonEmpty) {
      Left(s"Attachment delete failed. pageName=$pageName failedObjectKeys=${failedObjectKeys.mkString(",")}")
    } else {
      objectKeys.foreach(Attachment.markDeleted)
      Right(())
    }
  }

  private def unauthorized: Result =
    Unauthorized(Json.obj("error" -> Json.fromString("API key is required or invalid.")).toString()).as(JSON)

  private def withApiUser(request: RequestHeader)(f: User.SessionUser => Result): Result =
    SessionLogic.getApiKeyUser(request)(database).map(f).getOrElse(unauthorized)

  private def withApiUserAndKey(request: RequestHeader)(f: (User.SessionUser, UserApiKey) => Result): Result =
    SessionLogic.getApiKeyWithUser(request)(database).map { case (apiKey, user) => f(user, apiKey) }.getOrElse(unauthorized)

  private def pageMetadataJson(page: Page, userApiKeyName: Option[String]): Json =
    Json.obj(
      "name" -> Json.fromString(page.name),
      "revision" -> Json.fromLong(page.revision),
      "dateTime" -> Json.fromString(page.dateTime.toString),
      "isMinorEdit" -> Json.fromBoolean(page.isMinorEdit),
      "viaApi" -> Json.fromBoolean(page.viaApi),
      "userApiKeyName" -> userApiKeyName.map(Json.fromString).getOrElse(Json.Null),
      "size" -> Json.fromInt(page.content.length),
      "contentHash" -> Json.fromString("sha256:" + sha256Hex(page.content)),
    )

  private def selectLatestPages(prefix: String, limit: Int)(implicit connection: java.sql.Connection, site: Site): Seq[Page] = {
    val prefixTrimmed = prefix.trim
    val prefixLike = s"$prefixTrimmed%"
    val boundedLimit = limit.max(1).min(5000)
    SQL"""
      SELECT P.name, P.revision, P.dateTime, U.nickname AS nickname, P.`user` AS `user`, P.remoteAddress, P.comment, P.isMinorEdit, P.content, P.viaApi
      FROM Page P
      LEFT JOIN User U ON U.seq = P.user
      INNER JOIN (
        SELECT site, name, MAX(revision) AS revision
        FROM Page
        WHERE site = ${site.seq}
        GROUP BY site, name
      ) Latest
        ON Latest.site = P.site
        AND Latest.name = P.name
        AND Latest.revision = P.revision
      WHERE P.site = ${site.seq}
        AND ($prefixTrimmed = '' OR P.name LIKE $prefixLike)
      ORDER BY P.name ASC
      LIMIT $boundedLimit
    """.as((str("name") ~ long("revision") ~ localDateTime("dateTime") ~ get[Option[String]]("nickname") ~ optionLong("user") ~ str("remoteAddress") ~ str("comment") ~ bool("isMinorEdit") ~ str("content") ~ bool("viaApi")).map {
      case name ~ revision ~ dateTime ~ nickname ~ user ~ remoteAddress ~ comment ~ isMinorEdit ~ content ~ viaApi =>
        Page(name, revision, dateTime, nickname, user, remoteAddress, comment, isMinorEdit, content, viaApi)
    }.*)
  }

  def listPages(prefix: String, limit: Int): Action[AnyContent] = Action { implicit request =>
    withApiUser(request) { user =>
      database.withConnection { implicit connection =>
        implicit val site: Site = SiteLogic.get(request.host)
        implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
        implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(""), RenderingMode.Normal)
        val wikiPermission = WikiPermission()

        val readablePages = selectLatestPages(prefix, limit)
          .filter(page => wikiPermission.isReadable(page.name, Some(PageContent(page.content))))
        val keyNames = UserApiKey.selectNamesBySeqs(readablePages.flatMap(_.userApiKey))
        val pages = readablePages.map(page => pageMetadataJson(page, page.userApiKey.flatMap(keyNames.get)))

        Ok(Json.obj("pages" -> Json.fromValues(pages)))
      }
    }
  }

  def pageMetadata(): Action[AnyContent] = Action { implicit request =>
    withApiUser(request) { user =>
      request.body.asJson match {
        case None =>
          BadRequest(Json.obj("error" -> Json.fromString("JSON body is required.")).toString()).as(JSON)
        case Some(body) =>
          val names = (body \ "names").asOpt[Seq[String]].getOrElse(Seq.empty).map(_.trim).filter(_.nonEmpty).distinct.take(500)
          database.withConnection { implicit connection =>
            implicit val site: Site = SiteLogic.get(request.host)
            implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
            implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(""), RenderingMode.Normal)
            val wikiPermission = WikiPermission()
            val pages = if (names.isEmpty) Seq.empty else Page.selectLastRevision(names)
            val pageByName = pages.map(page => page.name -> page).toMap
            val readablePages = names.flatMap(name => pageByName.get(name))
              .filter(page => wikiPermission.isReadable(page.name, Some(PageContent(page.content))))
            val readableNames = readablePages.map(_.name).toSet
            val existingNames = pages.map(_.name).toSet
            val forbiddenNames = names.filter(name => existingNames.contains(name) && !readableNames.contains(name))
            val missingNames = names.filterNot(existingNames.contains)
            val keyNames = UserApiKey.selectNamesBySeqs(readablePages.flatMap(_.userApiKey))

            Ok(Json.obj(
              "pages" -> Json.fromValues(readablePages.map(page => pageMetadataJson(page, page.userApiKey.flatMap(keyNames.get)))),
              "missing" -> missingNames.asJson,
              "forbidden" -> forbiddenNames.asJson,
            ))
          }
      }
    }
  }

  def changes(name: String, prefix: String, since: String, afterRevision: Long, includeMinorEdit: Int, includeViaApi: Int, limit: Int): Action[AnyContent] = Action { implicit request =>
    withApiUser(request) { user =>
      database.withConnection { implicit connection =>
        implicit val site: Site = SiteLogic.get(request.host)
        implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
        implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(""), RenderingMode.Normal)
        val wikiPermission = WikiPermission()
        val nameTrimmed = name.trim
        val prefixTrimmed = prefix.trim
        val prefixLike = s"$prefixTrimmed%"
        val sinceOptEither: Either[Throwable, Option[LocalDateTime]] =
          if (since.trim.isEmpty) Right(None) else Try(LocalDateTime.parse(since.trim)).toEither.map(Some(_))
        if (sinceOptEither.isLeft) {
          BadRequest(Json.obj("error" -> Json.fromString("since must be an ISO-8601 local date-time.")).toString()).as(JSON)
        } else if (afterRevision > 0 && nameTrimmed.isEmpty) {
          BadRequest(Json.obj("error" -> Json.fromString("afterRevision requires name because revisions are page-local.")).toString()).as(JSON)
        } else {
          val sinceOpt = sinceOptEither.toOption.flatten
          val sinceValue = sinceOpt.getOrElse(LocalDateTime.of(1970, 1, 1, 0, 0))
          val boundedLimit = limit.max(1).min(1000)
          val batchSize = boundedLimit.max(100).min(1000)

          case class ChangeSourceRow(name: String, revision: Long, dateTime: LocalDateTime, comment: String, isMinorEdit: Boolean, viaApi: Boolean, userApiKeyName: Option[String])

          def selectRows(offset: Int): List[ChangeSourceRow] =
            SQL"""
              SELECT P.name, P.revision, P.dateTime, P.comment, P.isMinorEdit, P.viaApi, AK.name AS userApiKeyName
              FROM Page P
              LEFT JOIN UserApiKey AK ON AK.seq = P.userApiKey
              WHERE P.site = ${site.seq}
                AND ($nameTrimmed = '' OR P.name = $nameTrimmed)
                AND ($prefixTrimmed = '' OR P.name LIKE $prefixLike)
                AND (${sinceOpt.isEmpty} OR P.dateTime >= $sinceValue)
                AND ($afterRevision <= 0 OR P.revision > $afterRevision)
                AND (${includeMinorEdit == 1} OR P.isMinorEdit = false)
                AND (${includeViaApi == 1} OR P.viaApi = false)
              ORDER BY P.dateTime DESC, P.revision DESC, P.name ASC
              LIMIT $batchSize OFFSET $offset
            """.as((str("name") ~ long("revision") ~ localDateTime("dateTime") ~ str("comment") ~ bool("isMinorEdit") ~ bool("viaApi") ~ optionStr("userApiKeyName")).map {
              case name ~ revision ~ dateTime ~ comment ~ isMinorEdit ~ viaApi ~ userApiKeyName =>
                ChangeSourceRow(name, revision, dateTime, comment, isMinorEdit, viaApi, userApiKeyName)
            }.*)

          @scala.annotation.tailrec
          def collectReadableRows(offset: Int, acc: List[ChangeSourceRow]): List[ChangeSourceRow] = {
            if (acc.size >= boundedLimit) {
              acc.take(boundedLimit)
            } else {
              val rows = selectRows(offset)
              val readableRows = rows.filter(row => wikiPermission.isReadable(row.name))
              val next = acc ++ readableRows
              if (rows.size < batchSize) next.take(boundedLimit) else collectReadableRows(offset + rows.size, next)
            }
          }

          val rows = collectReadableRows(0, List.empty).map { row =>
            Json.obj(
              "name" -> Json.fromString(row.name),
              "revision" -> Json.fromLong(row.revision),
              "dateTime" -> Json.fromString(row.dateTime.toString),
              "comment" -> Json.fromString(row.comment),
              "isMinorEdit" -> Json.fromBoolean(row.isMinorEdit),
              "viaApi" -> Json.fromBoolean(row.viaApi),
              "userApiKeyName" -> row.userApiKeyName.map(Json.fromString).getOrElse(Json.Null),
            )
          }

          Ok(Json.obj("changes" -> Json.fromValues(rows)))
        }
      }
    }
  }

  def getPage(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    withApiUser(request) { user =>
      val name = decodePageName(nameEncoded)
      database.withConnection { implicit connection =>
        implicit val site: Site = SiteLogic.get(request.host)
        implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
        implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(name), RenderingMode.Normal)

        Page.selectLastRevision(name) match {
          case None =>
            NotFound(Json.obj("error" -> Json.fromString("Page not found.")).toString()).as(JSON)
          case Some(page) if !WikiPermission().isReadable(name, Some(PageContent(page.content))) =>
            Forbidden(Json.obj("error" -> Json.fromString("Permission denied.")).toString()).as(JSON)
          case Some(page) =>
            val userApiKeyName = page.userApiKey.flatMap(seq => UserApiKey.selectNamesBySeqs(Seq(seq)).get(seq))
            Ok(Json.obj(
              "name" -> Json.fromString(page.name),
              "revision" -> Json.fromLong(page.revision),
              "content" -> Json.fromString(page.content),
              "dateTime" -> Json.fromString(page.dateTime.toString),
              "isMinorEdit" -> Json.fromBoolean(page.isMinorEdit),
              "viaApi" -> Json.fromBoolean(page.viaApi),
              "userApiKeyName" -> userApiKeyName.map(Json.fromString).getOrElse(Json.Null),
              "contentHash" -> Json.fromString("sha256:" + sha256Hex(page.content)),
            ))
        }
      }
    }
  }

  def savePage(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    withApiUserAndKey(request) { (user, apiKey) =>
      request.body.asJson match {
        case None =>
          BadRequest(Json.obj("error" -> Json.fromString("JSON body is required.")).toString()).as(JSON)
        case Some(body) =>
          val name = decodePageName(nameEncoded)
          val revisionOpt = (body \ "revision").asOpt[Long]
          val textOpt = (body \ "text").asOpt[String]
          val comment = (body \ "comment").asOpt[String].getOrElse("")
          val isMinorEdit = (body \ "minorEdit").asOpt[Boolean].getOrElse(false)

          (revisionOpt, textOpt) match {
            case (None, _) =>
              BadRequest(Json.obj("error" -> Json.fromString("revision is required.")).toString()).as(JSON)
            case (_, None) =>
              BadRequest(Json.obj("error" -> Json.fromString("text is required.")).toString()).as(JSON)
            case (Some(revision), Some(text)) =>
              database.withConnection { implicit connection =>
                implicit val site: Site = SiteLogic.get(request.host)
                implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
                implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(name), RenderingMode.Normal)

                val latestPage = Page.selectLastRevision(name)
                val (latestText, latestRevision) = latestPage.map(page => (page.content, page.revision)).getOrElse(("", 0L))
                if (!WikiPermission().isWritable(name, latestPage.map(page => PageContent(page.content)))) {
                  Forbidden(Json.obj("error" -> Json.fromString("Permission denied.")).toString()).as(JSON)
                } else if (revision != latestRevision) {
                  Conflict(Json.obj(
                    "error" -> Json.fromString("revision != latestRevision"),
                    "latestRevision" -> Json.fromLong(latestRevision),
                  ).toString()).as(JSON)
                } else if (text == latestText) {
                  BadRequest(Json.obj("error" -> Json.fromString("text == latestText")).toString()).as(JSON)
                } else {
                  val now = LocalDateTime.now()
                  val nextRevision = latestRevision + 1
                  PageLogic.insert(name, nextRevision, now, comment, isMinorEdit, text, viaApi = true, userApiKey = Some(apiKey.seq))
                  name match {
                    case ".footer" => ahaWikiCache.Footer.invalidate()
                    case ".config" => ahaWikiCache.Config.invalidate()
                    case _ => // do nothing
                  }
                  implicit val tupleDatabaseSite: (Database, Site) = (database, site)
                  ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()
                  Ok(Json.obj(
                    "name" -> Json.fromString(name),
                    "revision" -> Json.fromLong(nextRevision),
                    "dateTime" -> Json.fromString(now.toString),
                  ))
                }
              }
          }
      }
    }
  }

  def renamePage(): Action[AnyContent] = Action { implicit request =>
    withApiUserAndKey(request) { (user, apiKey) =>
      request.body.asJson match {
        case None =>
          BadRequest(Json.obj("error" -> Json.fromString("JSON body is required.")).toString()).as(JSON)
        case Some(body) =>
          val name = (body \ "name").asOpt[String].getOrElse("").trim
          val newName = (body \ "newName").asOpt[String].getOrElse("").trim
          val revisionOpt = (body \ "revision").asOpt[Long]
          val comment = (body \ "comment").asOpt[String].filter(_.trim.nonEmpty).getOrElse(s"rename to $newName")

          if (name.isEmpty || newName.isEmpty) {
            BadRequest(Json.obj("error" -> Json.fromString("name and newName are required.")).toString()).as(JSON)
          } else {
            database.withTransaction { implicit connection =>
              implicit val site: Site = SiteLogic.get(request.host)
              implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
              implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(name), RenderingMode.Normal)

              (Page.selectLastRevision(name), Page.selectLastRevision(newName), revisionOpt) match {
                case (_, _, None) =>
                  BadRequest(Json.obj("error" -> Json.fromString("revision is required.")).toString()).as(JSON)
                case (None, _, _) =>
                  NotFound(Json.obj("error" -> Json.fromString("Page not found.")).toString()).as(JSON)
                case (Some(page), _, Some(revision)) if revision != page.revision =>
                  Conflict(Json.obj(
                    "error" -> Json.fromString("revision != latestRevision"),
                    "latestRevision" -> Json.fromLong(page.revision),
                  ).toString()).as(JSON)
                case (Some(_), Some(_), _) =>
                  Conflict(Json.obj("error" -> Json.fromString("Target page already exists.")).toString()).as(JSON)
                case (Some(page), None, Some(_)) if !WikiPermission().isRenamable(name) =>
                  Forbidden(Json.obj("error" -> Json.fromString("Permission denied.")).toString()).as(JSON)
                case (Some(page), None, Some(_)) =>
                  implicit val tupleDatabaseSite: (Database, Site) = (database, site)
                  ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()

                  Page.rename(name, newName)
                  PageLogic.insert(name, 1, LocalDateTime.now(), comment, isMinorEdit = false, body = s"#!redirect $newName", viaApi = true, userApiKey = Some(apiKey.seq))
                  wikiActors.pageCalculation ! Calculate(site, newName)

                  Ok(Json.obj(
                    "name" -> Json.fromString(name),
                    "newName" -> Json.fromString(newName),
                    "revision" -> Json.fromLong(page.revision),
                    "redirectRevision" -> Json.fromLong(1),
                  ))
              }
            }
          }
      }
    }
  }

  def deletePage(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    withApiUser(request) { user =>
      request.body.asJson match {
        case None =>
          BadRequest(Json.obj("error" -> Json.fromString("JSON body is required.")).toString()).as(JSON)
        case Some(body) =>
          val name = decodePageName(nameEncoded)
          val revisionOpt = (body \ "revision").asOpt[Long]
          val confirm = (body \ "confirm").asOpt[Boolean].getOrElse(false)

          if (!confirm) {
            BadRequest(Json.obj("error" -> Json.fromString("confirm must be true.")).toString()).as(JSON)
          } else {
            database.withTransaction { implicit connection =>
              implicit val site: Site = SiteLogic.get(request.host)
              implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
              implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(name), RenderingMode.Normal)

              (Page.selectLastRevision(name), revisionOpt) match {
                case (_, None) =>
                  BadRequest(Json.obj("error" -> Json.fromString("revision is required.")).toString()).as(JSON)
                case (None, _) =>
                  NotFound(Json.obj("error" -> Json.fromString("Page not found.")).toString()).as(JSON)
                case (Some(page), Some(revision)) if revision != page.revision =>
                  Conflict(Json.obj(
                    "error" -> Json.fromString("revision != latestRevision"),
                    "latestRevision" -> Json.fromLong(page.revision),
                  ).toString()).as(JSON)
                case (Some(_), Some(_)) if !WikiPermission().isDeletable(name) =>
                  Forbidden(Json.obj("error" -> Json.fromString("Permission denied.")).toString()).as(JSON)
                case (Some(page), Some(_)) =>
                  implicit val tupleDatabaseSite: (Database, Site) = (database, site)
                  ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()
                  deletePageAttachments(site.seq, name) match {
                    case Left(error) =>
                      logger.error(error)
                      InternalServerError(Json.obj("error" -> Json.fromString("Attachment delete failed.")).toString()).as(JSON)
                    case Right(_) =>
                      Page.deleteWithRelatedData(name)
                      wikiActors.pageCalculation ! Calculate(site, name)

                      Ok(Json.obj(
                        "ok" -> Json.fromBoolean(true),
                        "name" -> Json.fromString(name),
                        "deletedRevision" -> Json.fromLong(page.revision),
                      ))
                  }
              }
            }
          }
      }
    }
  }
}
