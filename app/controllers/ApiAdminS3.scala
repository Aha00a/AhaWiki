package controllers

import io.circe.Json
import logics.ApplicationConf
import logics.S3Logic
import logics.wikis.macros.S3AttachmentUrlLogic
import play.api.Logging
import play.api.mvc._
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request
import software.amazon.awssdk.services.s3.model.ListObjectsV2Response

import javax.inject._
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * The admin bucket browser: list, delete, and hand out a download URL.
 *
 * It reaches S3 directly rather than through the attachment layout, because it exists to
 * show what is actually in the bucket — including whatever does not follow that layout.
 */
class ApiAdminS3 @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  applicationConf: ApplicationConf,
) extends BaseController with JsonResults with AdminAuth with Logging {

  def adminS3Objects(prefix: String = "", maxKeys: Int = 500, recursive: Boolean = false): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      val safeMaxKeys = Math.min(ApiAdminS3.maxKeysPerRequest, Math.max(1, maxKeys))
      val safePrefix = Option(prefix).map(_.trim).getOrElse("")
      try {
        val s3Client = S3Logic.client(applicationConf)
        val bucket = S3Logic.bucket(applicationConf)
        // Not shared with AttachmentLogic.listPageObjectKeys: that reads one page of one wiki page's
        // prefix, this browses with a delimiter and continuation, and neither changes with the other.
        val listRequest = ListObjectsV2Request.builder()
          .bucket(bucket)
          .delimiter(if (recursive) null else "/")
          .prefix(if (safePrefix.nonEmpty) safePrefix else null)
          .build()
        val (results, token) = ApiAdminS3.listPages((r: ListObjectsV2Request) => s3Client.listObjectsV2(r), listRequest, safeMaxKeys, recursive)

        val directories = results.flatMap(_.commonPrefixes().asScala.map(_.prefix())).distinct
        val files = results.flatMap(_.contents().asScala).map { item =>
          Json.obj(
            "key" -> Json.fromString(item.key()),
            "size" -> Json.fromLong(Option(item.size()).map(_.longValue).getOrElse(0L)),
            "lastModified" -> Json.fromString(Option(item.lastModified()).map(_.toString).getOrElse("")),
            "isDirectory" -> Json.fromBoolean(false),
          )
        }
        val directoryRows = directories.map { directory =>
          Json.obj(
            "key" -> Json.fromString(directory),
            "size" -> Json.fromLong(0),
            "lastModified" -> Json.fromString(""),
            "isDirectory" -> Json.fromBoolean(true),
          )
        }
        Ok(Json.obj(
          "bucket" -> Json.fromString(bucket),
          "prefix" -> Json.fromString(safePrefix),
          "maxKeys" -> Json.fromInt(safeMaxKeys),
          "isTruncated" -> Json.fromBoolean(token.isDefined),
          "nextContinuationToken" -> Json.fromString(token.getOrElse("")),
          "items" -> Json.fromValues(directoryRows ++ files),
        ))
      } catch {
        case error: Throwable =>
          logger.error(s"adminS3Objects failed. prefix=$safePrefix", error)
          JsonError(InternalServerError, "S3 조회에 실패했습니다.")
      }
    }
  }

  def adminDeleteS3Objects: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      val keys = request.body.asJson
        .flatMap(json => (json \ "keys").asOpt[Seq[String]])
        .getOrElse(Seq.empty)
        .map(_.trim)
        .filter(_.nonEmpty)
        .distinct
      if (keys.isEmpty) {
        JsonError(BadRequest, "keys is required")
      } else {
        try {
          val failures = S3Logic.deleteObjects(applicationConf, keys)
          if (failures.isEmpty) {
            Ok(Json.obj("ok" -> Json.fromBoolean(true), "deletedCount" -> Json.fromInt(keys.size)))
          } else {
            logger.error(s"adminDeleteS3Objects: S3 did not delete ${failures.size} of ${keys.size} keys. ${failures.take(10).mkString(", ")}")
            JsonError(InternalServerError, "S3 삭제에 실패했습니다.")
          }
        } catch {
          case error: Throwable =>
            logger.error(s"adminDeleteS3Objects failed. keys=${keys.take(10).mkString(",")}", error)
            JsonError(InternalServerError, "S3 삭제에 실패했습니다.")
        }
      }
    }
  }

  def adminS3DownloadUrl(key: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      val objectKey = Option(key).map(_.trim).getOrElse("")
      if (objectKey.isEmpty) {
        JsonError(BadRequest, "key is required")
      } else {
        S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, objectKey) match {
          case Right(url) => Ok(Json.obj("url" -> Json.fromString(url), "key" -> Json.fromString(objectKey)))
          case Left(errorMessage) =>
            logger.error(s"adminS3DownloadUrl failed. key=$objectKey error=$errorMessage")
            JsonError(InternalServerError, "다운로드 URL 생성 실패")
        }
      }
    }
  }
}

object ApiAdminS3 {
  /** The most keys one ListObjectsV2 request returns; S3 will not send more. */
  val maxKeysPerRequest = 1000

  /**
   * Lists the pages of a listing: until `maxKeys` objects have come back or the listing ends, and
   * only past the first page when the listing is recursive. Returns the pages, and the token for
   * the rest when there is a rest.
   *
   * `list` is the S3 call, passed in so the paging can be tried without S3. Each request is
   * `request` with its own token and key limit.
   */
  def listPages(list: ListObjectsV2Request => ListObjectsV2Response, request: ListObjectsV2Request, maxKeys: Int, recursive: Boolean): (Seq[ListObjectsV2Response], Option[String]) = {
    val results = mutable.ArrayBuffer.empty[ListObjectsV2Response]
    var remaining = maxKeys
    var token: String = null
    do {
      val result = list(request.toBuilder.continuationToken(token).maxKeys(Math.min(maxKeysPerRequest, Math.max(1, remaining))).build())
      results += result
      remaining -= result.contents().size()
      token = result.nextContinuationToken()
    } while (recursive && token != null && remaining > 0)
    (results.toSeq, Option(token))
  }
}
