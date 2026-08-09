package controllers

import com.amazonaws.services.s3.model.DeleteObjectsRequest
import com.amazonaws.services.s3.model.ListObjectsV2Request
import com.amazonaws.services.s3.model.ListObjectsV2Result
import com.amazonaws.services.s3.model.S3ObjectSummary
import io.circe.Json
import logics.ApplicationConf
import logics.S3Logic
import logics.wikis.macros.S3AttachmentUrlLogic
import play.api.Logging
import play.api.mvc._

import javax.inject._
import scala.collection.mutable

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

  private val maxKeysPerRequest = 1000

  def adminS3Objects(prefix: String = "", maxKeys: Int = 500, recursive: Boolean = false): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      val safeMaxKeys = Math.min(maxKeysPerRequest, Math.max(1, maxKeys))
      val safePrefix = Option(prefix).map(_.trim).getOrElse("")
      try {
        val amazonS3 = S3Logic.client(applicationConf)
        val bucket = S3Logic.bucket(applicationConf)
        val requestBuilder = new ListObjectsV2Request()
          .withBucketName(bucket)
          .withMaxKeys(safeMaxKeys)
        if (!recursive) {
          requestBuilder.withDelimiter("/")
        }
        if (safePrefix.nonEmpty) {
          requestBuilder.withPrefix(safePrefix)
        }
        val results = mutable.ArrayBuffer.empty[ListObjectsV2Result]
        var remaining = safeMaxKeys
        var token: String = null
        do {
          requestBuilder.withContinuationToken(token)
          requestBuilder.withMaxKeys(Math.min(maxKeysPerRequest, Math.max(1, remaining)))
          val result = amazonS3.listObjectsV2(requestBuilder)
          results += result
          remaining -= Option(result.getObjectSummaries).map(_.size()).getOrElse(0)
          token = result.getNextContinuationToken
        } while (recursive && token != null && remaining > 0)

        val directories = results.flatMap(r => Option(r.getCommonPrefixes).map(_.toArray.toSeq).getOrElse(Seq.empty).map(_.toString)).distinct
        val files = results.flatMap(r => Option(r.getObjectSummaries).map(_.toArray.toSeq).getOrElse(Seq.empty)).map { raw =>
          val item = raw.asInstanceOf[S3ObjectSummary]
          Json.obj(
            "key" -> Json.fromString(item.getKey),
            "size" -> Json.fromLong(item.getSize),
            "lastModified" -> Json.fromString(Option(item.getLastModified).map(_.toInstant.toString).getOrElse("")),
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
          "isTruncated" -> Json.fromBoolean(token != null),
          "nextContinuationToken" -> Json.fromString(Option(token).getOrElse("")),
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
          val amazonS3 = S3Logic.client(applicationConf)
          val bucket = S3Logic.bucket(applicationConf)
          val deleteRequest = new DeleteObjectsRequest(bucket).withKeys(keys: _*)
          amazonS3.deleteObjects(deleteRequest)
          Ok(Json.obj("ok" -> Json.fromBoolean(true), "deletedCount" -> Json.fromInt(keys.size)))
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
