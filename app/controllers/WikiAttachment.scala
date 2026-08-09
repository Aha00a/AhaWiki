package controllers

import com.amazonaws.services.s3.model.ObjectMetadata
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics._
import logics.wikis.WikiPermission
import models.RequestWrapper
import models._
import models.tables.Attachment
import models.tables.Config
import models.tables.Site
import play.api.Logging
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.Json
import play.api.mvc._

import java.nio.file.Files
import java.sql.Connection
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
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

  private val attachmentTimestampFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH-mm-ss")

  private def buildAttachmentObjectKey(siteSeq: Long, pageName: String, originalFileName: String, extension: String, now: LocalDateTime = LocalDateTime.now()): String = {
    val sanitizedPageName = AttachmentLogic.sanitizePathSegment(pageName)
    val sanitizedOriginalFileName = AttachmentLogic.sanitizePathSegment(originalFileName)
    val sanitizedExtension = AttachmentLogic.sanitizePathSegment(extension).toLowerCase
    val sanitizedOriginalFileNameWithoutExtension = {
      val stripped = sanitizedOriginalFileName.stripSuffix(s".$sanitizedExtension")
      if (stripped.nonEmpty) stripped else sanitizedOriginalFileName
    }
    val formattedDateTime = now.format(attachmentTimestampFormatter)
    s"${AttachmentLogic.sitePrefix(siteSeq)}$sanitizedPageName/$sanitizedOriginalFileName/${sanitizedOriginalFileNameWithoutExtension}.$formattedDateTime.$sanitizedExtension"
  }

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
    implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(pageName)
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
                                       )(implicit request: RequestHeader, connection: Connection): Unit = {
    val currentUser = SessionLogic.getUser(request)
    val bucket = applicationConf.AhaWiki.aws.s3.bucket()
    val storedFilename = objectKey.split("/").lastOption.getOrElse(objectKey)
    Attachment.insertInitiated(
      site = siteSeq,
      pageName = pageName,
      user = currentUser.map(_.seq),
      uploaderEmail = currentUser.flatMap(_.loginEmail),
      originalFilename = originalFilename,
      storedFilename = storedFilename,
      bucket = bucket,
      objectKey = objectKey,
      contentType = contentType,
      fileSize = fileSize,
    )
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
                val amazonS3 = S3Logic.client(applicationConf)
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
                  telegramLogic.notifyAttachmentUploaded(request.host, pageName, originalFileName, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
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
            val s3OnlyObjects = AttachmentLogic.listPageObjectKeys(site.seq, pageName).filterNot(dbObjectKeys.contains)
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
                    val amazonS3 = S3Logic.client(applicationConf)
                    val bucket = applicationConf.AhaWiki.aws.s3.bucket()

                    try {
                      amazonS3.deleteObject(bucket, resolvedObjectKey)
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

  def uploadClipboardImage(): Action[MultipartFormData[TemporaryFile]] = Action(parse.multipartFormData) { implicit request =>
    import com.amazonaws.services.s3.model.ObjectMetadata
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
                val objectKey = buildAttachmentObjectKey(
                  site.seq,
                  pageName,
                  originalFileName = "clipboard",
                  extension = extension,
                )

                val amazonS3 = S3Logic.client(applicationConf)
                val bucket = applicationConf.AhaWiki.aws.s3.bucket()
                val metadata = new ObjectMetadata()
                metadata.setContentType(contentType)
                metadata.setContentLength(filePart.fileSize)
                insertInitiatedAttachment(
                  siteSeq = site.seq,
                  pageName = pageName,
                  originalFilename = "clipboard",
                  objectKey = objectKey,
                  contentType = contentType,
                  fileSize = filePart.fileSize,
                )

                try {
                  val inputStream = Files.newInputStream(filePart.ref.path)
                  try {
                    val putResult = amazonS3.putObject(bucket, objectKey, inputStream, metadata)
                    Attachment.markUploaded(objectKey, Option(putResult.getETag))
                  } finally {
                    inputStream.close()
                  }

                  val imageUrl = S3AttachmentUrlLogic.generatePresignedUrl(objectKey).toOption.getOrElse("")
                  telegramLogic.notifyClipboardImageUploaded(request.host, pageName, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
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
              }
          }
        }
      case _ =>
        BadRequest("pageName and file are required")
    }
  }
}
