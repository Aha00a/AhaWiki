package controllers

import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics._
import logics.wikis.WikiPermission
import models.RequestWrapper
import models._
import models.tables.Attachment
import models.tables.Config
import models.tables.Site
import models.tables.User
import play.api.Logging
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.Json
import play.api.mvc._

import java.sql.Connection
import javax.inject._
import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/**
 * Uploading, listing and deleting a page's attachments, plus clipboard image paste.
 *
 * These are the endpoints that put bytes in S3 and rows in `Attachment`. Where those bytes
 * live is `AttachmentLogic`'s business; this decides who may put them there and what the
 * editor gets told afterwards.
 *
 * Every endpoint goes through `getPageContextFor`, which resolves the site and page context
 * and refuses before any S3 call. Uploading to a page you cannot write to should fail before
 * anything reaches the bucket, not after.
 */
class WikiAttachment @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wikiActors: WikiActors,
  telegramLogic: TelegramLogic,
) extends BaseController with JsonResults with Logging {

  private def toAttachmentMacroArgument(objectKey: String, siteSeq: Long, pageName: String): String = {
    val sitePrefix = AttachmentLogic.sitePrefix(siteSeq)
    val pagePrefix = AttachmentLogic.pagePrefix(siteSeq, pageName)
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
    } else if (trimmed.startsWith(s"${AttachmentLogic.Root}/")) {
      trimmed
    } else {
      s"${AttachmentLogic.pagePrefix(siteSeq, pageName)}$trimmed"
    }
  }

  private def getPageContextFor(pageName: String)(permitted: WikiPermission => Boolean)
                               (implicit request: Request[Any], connection: Connection): Either[Result, (Site, ContextWikiPage, RequestWrapper)] = {
    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(pageName).holding(connection)
    implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
    if (!permitted(WikiPermission())) Left(Forbidden("Permission denied."))
    else Right((site, contextWikiPage, provider))
  }

  private def insertInitiatedAttachment(
                                         siteSeq: Long,
                                         pageName: String,
                                         originalFilename: String,
                                         objectKey: String,
                                         contentType: String,
                                         fileSize: Long,
                                         uploader: Option[User.SessionUser],
                                       )(implicit connection: Connection): Unit = {
    val bucket = applicationConf.AhaWiki.aws.s3.bucket()
    val storedFilename = objectKey.split("/").lastOption.getOrElse(objectKey)
    Attachment.insertInitiated(
      site = siteSeq,
      pageName = pageName,
      user = uploader.map(_.seq),
      uploaderEmail = uploader.flatMap(_.loginEmail),
      originalFilename = originalFilename,
      storedFilename = storedFilename,
      bucket = bucket,
      objectKey = objectKey,
      contentType = contentType,
      fileSize = fileSize,
    )
  }

  /** Every upload route parses with this; the limit and why they share it are on the constant. */
  private def uploadBodyParser: BodyParser[MultipartFormData[TemporaryFile]] =
    parse.multipartFormData(AttachmentLogic.MaxUploadRequestBytes)

  /**
   * Puts one uploaded file in S3 under a new key and keeps its row in step; the three upload
   * routes differ only in how they get there and what they answer. The row is written first, as
   * initiated, so a put that dies half way leaves a row that says so (Dev Attachment).
   *
   * Only the put is inside the failure handling. Until 2026-11-26 each route also wrapped the
   * presigned URL and the Telegram notice in it, so a notice that failed after a good put marked
   * a stored file Failed.
   */
  private def storeFile(
                         pageName: String,
                         originalFilename: String,
                         extension: String,
                         contentType: String,
                         filePart: MultipartFormData.FilePart[TemporaryFile],
                         uploader: Option[User.SessionUser],
                       )(implicit connection: Connection, site: Site): Try[String] = {
    val objectKey = AttachmentLogic.objectKey(site.seq, pageName, originalFileName = originalFilename, extension = extension)
    insertInitiatedAttachment(site.seq, pageName, originalFilename, objectKey, contentType, filePart.fileSize, uploader)
    Try(Attachment.markUploaded(objectKey, S3Logic.putFile(applicationConf, objectKey, contentType, filePart.ref.path))) match {
      case Success(_) => Success(objectKey)
      case Failure(error) =>
        logger.error(s"upload failed. objectKey=$objectKey", error)
        Attachment.markFailed(objectKey)
        Failure(error)
    }
  }

  // A file keeps its own extension, letters and digits only, or "bin" when it has none.
  private def fileExtension(originalFileName: String): String =
    Some(originalFileName.split('.').lastOption.getOrElse("").replaceAll("[^a-zA-Z0-9]", "").toLowerCase).filter(_.nonEmpty).getOrElse("bin")

  /** What a file upload answers, from the editor or from the API: the editor and an agent both paste `attachmentMacro`. */
  private def fileUploadedJson(objectKey: String, contentType: String, pageName: String)(implicit site: Site): play.api.libs.json.JsObject =
    Json.obj(
      "objectKey" -> objectKey,
      "attachmentMacro" -> s"[[Attachment(${toAttachmentMacroArgument(objectKey, site.seq, pageName)})]]",
      "fileUrl" -> logics.wikis.macros.S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, objectKey).fold(_ => "", url => url),
      "contentType" -> contentType,
    )

  def uploadAttachment(): Action[MultipartFormData[TemporaryFile]] = Action(uploadBodyParser) { implicit request =>
    val pageNameOption = request.body.dataParts.get("pageName").flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)
    val fileOption = request.body.file("file")

    (pageNameOption, fileOption) match {
      case (Some(pageName), Some(filePart)) =>
        database.withConnection { implicit connection =>
          getPageContextFor(pageName)(_.isUploadable(pageName)) match {
            case Left(result) => result
            case Right((site0, contextWikiPage0, provider0)) =>
              implicit val site: Site = site0
              implicit val contextWikiPage: ContextWikiPage = contextWikiPage0
              implicit val provider: RequestWrapper = provider0
              val originalFileName = filePart.filename.trim
              if (originalFileName.isEmpty) {
                BadRequest("invalid file name")
              } else {
                val contentType = filePart.contentType.getOrElse("application/octet-stream")
                storeFile(pageName, originalFileName, fileExtension(originalFileName), contentType, filePart, provider.getUser) match {
                  case Failure(_) => InternalServerError("File upload failed.")
                  case Success(objectKey) =>
                    telegramLogic.notifyAttachmentUploaded(request.host, pageName, originalFileName, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
                    Ok(fileUploadedJson(objectKey, contentType, pageName))
                }
              }
          }
        }
      case _ =>
        BadRequest("pageName and file are required")
    }
  }

  /**
   * The same upload for an API key: `POST /api/v1/attachment/<page>` with the file as the
   * multipart part `file`. It answers what the editor's upload answers, so an agent puts the
   * returned `attachmentMacro` in the page and saves it with `POST /api/v1/page/<page>`.
   *
   * It lives here, not in ApiV1, so that the three uploads share `storeFile`; the path is under
   * /api/v1/ so that Filters lets it through without a CSRF token, as it does the rest of the API.
   * The key's user is checked for Upload on the page, as the editor's session is. Like a save
   * through the API, it sends no Telegram notice (wiki page Api, «제한사항»).
   *
   * The body is parsed before the key is looked at, as the editor's upload parses before the
   * session is: a request without a valid key costs the server the read of up to
   * MaxUploadRequestBytes, and nothing reaches S3 or the table.
   */
  def uploadAttachmentViaApi(nameEncoded: String): Action[MultipartFormData[TemporaryFile]] = Action(uploadBodyParser) { implicit request =>
    SessionLogic.getApiKeyUser(request)(database) match {
      case None => JsonError(Unauthorized, "API key is required or invalid.")
      case Some(user) =>
        val pageName = logics.wikis.PageNameUrl.decode(nameEncoded)
        request.body.file("file") match {
          case None => JsonError(BadRequest, "file is required.")
          case Some(filePart) =>
            val originalFileName = filePart.filename.trim
            database.withConnection { implicit connection =>
              implicit val site: Site = SiteLogic.get(request.host)
              implicit val provider: RequestWrapper = RequestWrapper.forUser(user)
              implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(pageName), logics.wikis.RenderingMode.Normal).holding(connection)
              if (!WikiPermission().isUploadable(pageName)) {
                JsonError(Forbidden, "Permission denied.")
              } else if (originalFileName.isEmpty) {
                JsonError(BadRequest, "invalid file name")
              } else {
                val contentType = filePart.contentType.getOrElse("application/octet-stream")
                storeFile(pageName, originalFileName, fileExtension(originalFileName), contentType, filePart, Some(user)) match {
                  case Failure(_) => JsonError(InternalServerError, "File upload failed.")
                  case Success(objectKey) => Ok(fileUploadedJson(objectKey, contentType, pageName))
                }
              }
            }
        }
    }
  }

  def pageAttachments(): Action[AnyContent] = Action { implicit request =>
    import play.api.libs.json.Json

    val pageName = request.getQueryString("pageName").map(_.trim).getOrElse("")
    if (pageName.isEmpty) {
        BadRequest("pageName is required")
    } else {
      database.withConnection { implicit connection =>
        getPageContextFor(pageName)(_.isReadable(pageName)) match {
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
            // After the rows, not before: a row reads Uploaded only once its putObject has returned,
            // so a listing taken later already holds it. Listed first, an upload finishing in between
            // would show its own new row as DB_ONLY.
            val listing = AttachmentLogic.listPageObjects(site.seq, pageName)
            // An object another page's row holds, or a kept deleted page's, is not this page's
            // S3_ONLY: the prefix is shared, not the object (AttachmentLogic.heldElsewhere).
            val notInRows = listing.map(_.keys).getOrElse(Seq.empty).filterNot(dbObjectKeys.contains)
            val s3OnlyObjects = notInRows.filterNot(AttachmentLogic.heldElsewhere(site.seq, pageName, notInRows).contains)
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
                "integrityStatus" -> AttachmentLogic.integrityStatus(attachment.objectKey, presignedUrlOption.isDefined, listing),
                "attachmentMacro" -> s"[[Attachment(${toAttachmentMacroArgument(attachment.objectKey, site.seq, pageName)})]]",
              )} ++ s3OnlyJson)
            ))
        }
      }
    }
  }

  def deleteAttachment(): Action[AnyContent] = Action { implicit request =>
    import play.api.libs.json.Json

    val form = Form(tuple("pageName" -> text, "objectKey" -> text)).bindFromRequest()
    form.fold(_ => BadRequest("invalid form"), {
      case (pageNameRaw, objectKeyRaw) =>
        val pageName = pageNameRaw.trim
        val objectKey = objectKeyRaw.trim
        if (pageName.isEmpty || objectKey.isEmpty) {
          BadRequest("pageName and objectKey are required")
        } else {
          database.withConnection { implicit connection =>
            getPageContextFor(pageName)(_.isDeletable(pageName)) match {
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
                    try {
                      S3Logic.deleteObject(applicationConf, resolvedObjectKey)
                      Attachment.markDeleted(resolvedObjectKey)
                      val attachFilename = resolvedObjectKey.split("/").lastOption.getOrElse(resolvedObjectKey)
                      telegramLogic.notifyAttachmentDeleted(request.host, pageName, attachFilename, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
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

  def uploadClipboardImage(): Action[MultipartFormData[TemporaryFile]] = Action(uploadBodyParser) { implicit request =>
    import logics.wikis.macros.S3AttachmentUrlLogic
    import play.api.libs.json.Json

    val pageNameOption = request.body.dataParts.get("pageName").flatMap(_.headOption).map(_.trim).filter(_.nonEmpty)
    val fileOption = request.body.file("file")

    (pageNameOption, fileOption) match {
      case (Some(pageName), Some(filePart)) =>
        database.withConnection { implicit connection =>
          getPageContextFor(pageName)(_.isUploadable(pageName)) match {
            case Left(result) => result
            case Right((site0, contextWikiPage0, provider0)) =>
              implicit val site: Site = site0
              implicit val contextWikiPage: ContextWikiPage = contextWikiPage0
              implicit val provider: RequestWrapper = provider0
              val contentType = filePart.contentType.getOrElse("")
              if (!contentType.startsWith("image/")) {
                BadRequest("only image is supported")
              } else {
                val extension = contentType.split("/").lastOption.getOrElse("png").replace("+xml", "").replaceAll("[^a-zA-Z0-9]", "").toLowerCase
                storeFile(pageName, "clipboard", extension, contentType, filePart, provider.getUser) match {
                  case Failure(_) => InternalServerError("Image upload failed.")
                  case Success(objectKey) =>
                    val imageUrl = S3AttachmentUrlLogic.generatePresignedUrl(objectKey).toOption.getOrElse("")
                    telegramLogic.notifyClipboardImageUploaded(request.host, pageName, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
                    Ok(Json.obj(
                      "objectKey" -> objectKey,
                      "attachmentMacro" -> s"[[Attachment(${toAttachmentMacroArgument(objectKey, site.seq, pageName)})]]",
                      "imageUrl" -> imageUrl,
                    ))
                }
              }
          }
        }
      case _ =>
        BadRequest("pageName and file are required")
    }
  }
}
