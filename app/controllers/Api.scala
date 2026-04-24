package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import anorm.SqlParser.{bool, long, str}
import anorm._
import com.amazonaws.auth.AWSStaticCredentialsProvider
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.amazonaws.services.s3.model.ObjectMetadata
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits.RichRequest
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import logics.wikis.PageLogic
import logics.wikis.WikiPermission
import logics.wikis.SignedReadUrlLogic
import logics.wikis.ExtractConvertInjectMacro
import logics.wikis.interpreters.Interpreters
import logics.wikis.macros.S3AttachmentUrlLogic
import models.Adjacent
import models.ContextSite
import models.ContextWikiPage
import models.PageContent
import models.RequestWrapper
import models.tables.CalculatedLink
import models.tables.Page
import models.tables.PageWithoutContentWithSize
import models.tables.Site
import models.tables.UserSite
import models.tables.Config
import play.api.Configuration
import play.api.Logging
import play.api.db.Database
import play.api.libs.Files.TemporaryFile
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.filters.csrf.CSRF
import services.ApplicationLifecycleHook

import java.nio.file.Files
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.net.URLDecoder
import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Random


class Api @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  @Named("db-actor") actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  applicationLifecycleHook: ApplicationLifecycleHook,
  configuration: Configuration
) extends BaseController with Logging {
  private def isAdmin(implicit request: RequestHeader): Boolean = {
    SessionLogic.getUser(request).exists(u => u.email == "aha00a@gmail.com" || u.seq == 1)
  }

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)


  private lazy val signedReadUrlSecret: String = configuration.getOptional[String]("play.http.secret.key").getOrElse("")
  private val adminFaviconConfigKey: String = "site.favicon.objectKey"
  private val adminFaviconTimestampFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH-mm-ss")

  private def sanitizeAttachmentPathSegment(v: String): String = {
    val sanitized = v.replaceAll("[^\\p{IsHangul}\\p{IsHan}\\p{IsHiragana}\\p{IsKatakana}a-zA-Z0-9._-]", "_")
    if (sanitized.nonEmpty) sanitized else "_"
  }

  private def buildAdminFaviconObjectKey(siteSeq: Long, originalFileName: String, now: LocalDateTime = LocalDateTime.now()): String = {
    val extension = originalFileName
      .split('.')
      .lastOption
      .map(_.replaceAll("[^a-zA-Z0-9]", "").toLowerCase)
      .filter(_.nonEmpty)
      .getOrElse("png")
    val sanitizedFilename = sanitizeAttachmentPathSegment(originalFileName)
    val formattedDateTime = now.format(adminFaviconTimestampFormatter)
    s"Favicon/$siteSeq/${sanitizedFilename}.$formattedDateTime.$extension"
  }

  private def buildAmazonS3Client(): AmazonS3 = {
    val credentials = new BasicAWSCredentials(
      applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
      applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
    )
    AmazonS3ClientBuilder.standard
      .withCredentials(new AWSStaticCredentialsProvider(credentials))
      .withRegion(applicationConf.AhaWiki.aws.AWS_REGION())
      .build()
  }

  private def parseSiteSeq(v: String): Option[Long] = scala.util.Try(v.trim.toLong).toOption.filter(_ > 0)

  private def resolveAdminTargetSite(siteSeqValue: Option[String])(implicit request: RequestHeader): Either[Result, Site] = {
    siteSeqValue
      .flatMap(parseSiteSeq)
      .flatMap(seq => SiteLogic.get(seq)(database, ahaWikiCache))
      .toRight(BadRequest(Json.obj("error" -> Json.fromString("Valid siteSeq is required.")).toString()).as(JSON))
  }


  def adminGenerateSignedReadUrl: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      val name = request.getQueryString("name").map(_.trim).getOrElse("")
      val revision = request.getQueryString("revision").flatMap(v => scala.util.Try(v.toInt).toOption).getOrElse(0)
      val actionInput = request.getQueryString("action").getOrElse("")
      val action = actionInput match {
        case "" | "view" => "view"
        case "raw" => "raw"
        case "history" => "history"
        case "diff" => "diff"
        case _ => ""
      }

      if (signedReadUrlSecret.isEmpty) {
        InternalServerError(Json.obj("error" -> Json.fromString("Signed URL secret is not configured.")).toString()).as(JSON)
      } else if (name.isEmpty) {
        BadRequest(Json.obj("error" -> Json.fromString("Query parameter 'name' is required.")).toString()).as(JSON)
      } else if (action.isEmpty) {
        BadRequest(Json.obj("error" -> Json.fromString("action must be one of: view, raw, history, diff")).toString()).as(JSON)
      } else {
        val expiresAt = java.time.Instant.now().getEpochSecond + SignedReadUrlLogic.ValidDurationSeconds
        val signature = SignedReadUrlLogic.signReadRequest(
          host = request.host,
          name = name,
          revision = revision,
          action = action,
          expiresAtEpochSeconds = expiresAt,
          secret = signedReadUrlSecret,
        )

        val basePath = routes.Wiki.view(java.net.URLEncoder.encode(name, "UTF-8").replace("+", "%20"), revision, action).url
        val separator = if (basePath.contains("?")) "&" else "?"
        val signedPath = s"$basePath$separator${SignedReadUrlLogic.QueryParamExpires}=$expiresAt&${SignedReadUrlLogic.QueryParamSignature}=$signature"

        Ok(Json.obj(
          "name" -> Json.fromString(name),
          "revision" -> Json.fromInt(revision),
          "action" -> Json.fromString(action),
          "expiresAtEpochSeconds" -> Json.fromLong(expiresAt),
          "signedPath" -> Json.fromString(signedPath),
          "signedUrl" -> Json.fromString(s"${request.scheme}://${request.host}$signedPath"),
        ).toString()).as(JSON)
      }
    }
  }

  def adminSites: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminSite(seq: Long, name: String, domains: Seq[String], userCount: Long, pageCount: Long)
        case class AdminSiteRow(seq: Long, name: String, domain: Option[String], userCount: Long, pageCount: Long)

        val rows = SQL"""
          SELECT
            S.seq,
            S.name,
            SD.domain,
            COALESCE(US.user_count, 0) AS user_count,
            COALESCE(P.page_count, 0) AS page_count
          FROM Site S
          LEFT JOIN SiteDomain SD ON SD.site = S.seq
          LEFT JOIN (
            SELECT site, COUNT(*) AS user_count
            FROM UserSite
            GROUP BY site
          ) US ON US.site = S.seq
          LEFT JOIN (
            SELECT site, COUNT(*) AS page_count
            FROM Page
            GROUP BY site
          ) P ON P.site = S.seq
          ORDER BY S.seq, SD.domain
        """.as((long("seq") ~ str("name") ~ str("domain").? ~ long("user_count") ~ long("page_count")).map {
          case seq ~ name ~ domain ~ userCount ~ pageCount => AdminSiteRow(seq, name, domain, userCount, pageCount)
        }.*)

        val sites = rows
          .groupBy(r => (r.seq, r.name, r.userCount, r.pageCount))
          .toSeq
          .sortBy(_._1._1)
          .map { case ((seq, name, userCount, pageCount), groupedRows) =>
            AdminSite(
              seq = seq,
              name = name,
              domains = groupedRows.flatMap(_.domain).distinct.sorted,
              userCount = userCount,
              pageCount = pageCount,
            )
          }

        Ok(sites.asJson)
      }
    }
  }

  def adminSiteUsers: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        implicit val site: Site = SiteLogic.get(request.host)
        case class AdminSiteUser(user: Long, site: Long, created: String, email: String, nickname: String)
        val users = UserSite.select().map(u => AdminSiteUser(u.user, u.site, u.created.toString, u.email, u.nickname))
        Ok(users.asJson)
      }
    }
  }

  def adminSiteFavicon: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        resolveAdminTargetSite(request.getQueryString("siteSeq")) match {
          case Left(errorResult) => errorResult
          case Right(siteValue) =>
            implicit val site: Site = siteValue
            val objectKeyOption = Config.select(adminFaviconConfigKey).map(_.v.trim).filter(_.nonEmpty)
            val faviconUrlOption = objectKeyOption.flatMap(objectKey => S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, objectKey).toOption)
            Ok(Json.obj(
              "siteSeq" -> Json.fromLong(site.seq),
              "objectKey" -> Json.fromString(objectKeyOption.getOrElse("")),
              "faviconUrl" -> Json.fromString(faviconUrlOption.getOrElse("/public/favicon.png")),
            ))
        }
      }
    }
  }

  def adminUploadSiteFavicon: Action[MultipartFormData[TemporaryFile]] = Action(parse.multipartFormData) { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      request.body.file("file") match {
        case None =>
          BadRequest(Json.obj("error" -> Json.fromString("file is required")).toString()).as(JSON)
        case Some(filePart) =>
          val contentType = filePart.contentType.getOrElse("application/octet-stream")
          if (!contentType.startsWith("image/")) {
            BadRequest(Json.obj("error" -> Json.fromString("Only image files are allowed.")).toString()).as(JSON)
          } else {
            database.withConnection { implicit connection =>
              val siteSeqValue = request.body.dataParts.get("siteSeq").flatMap(_.headOption)
              resolveAdminTargetSite(siteSeqValue) match {
                case Left(errorResult) => errorResult
                case Right(siteValue) =>
                  implicit val site: Site = siteValue
                  val objectKey = buildAdminFaviconObjectKey(site.seq, filePart.filename.trim)
                  val amazonS3 = buildAmazonS3Client()
                  val bucket = applicationConf.AhaWiki.aws.s3.bucket()
                  val metadata = new ObjectMetadata()
                  metadata.setContentType(contentType)
                  metadata.setContentLength(filePart.fileSize)

                  try {
                    val inputStream = Files.newInputStream(filePart.ref.path)
                    try {
                      amazonS3.putObject(bucket, objectKey, inputStream, metadata)
                    } finally {
                      inputStream.close()
                    }
                    Config.upsert(adminFaviconConfigKey, objectKey)
                    val faviconUrl = S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, objectKey).toOption.getOrElse("/public/favicon.png")
                    Ok(Json.obj(
                      "ok" -> Json.fromBoolean(true),
                      "siteSeq" -> Json.fromLong(site.seq),
                      "objectKey" -> Json.fromString(objectKey),
                      "faviconUrl" -> Json.fromString(faviconUrl),
                    ))
                  } catch {
                    case error: Throwable =>
                      logger.error(s"adminUploadSiteFavicon failed. objectKey=$objectKey", error)
                      InternalServerError(Json.obj("error" -> Json.fromString("Favicon upload failed.")).toString()).as(JSON)
                  }
              }
            }
          }
      }
    }
  }

  def adminDeleteSiteFavicon: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        resolveAdminTargetSite(request.getQueryString("siteSeq")) match {
          case Left(errorResult) => errorResult
          case Right(siteValue) =>
            implicit val site: Site = siteValue
            val objectKeyOption = Config.select(adminFaviconConfigKey).map(_.v.trim).filter(_.nonEmpty)
            objectKeyOption.foreach { objectKey =>
              try {
                val amazonS3 = buildAmazonS3Client()
                val bucket = applicationConf.AhaWiki.aws.s3.bucket()
                amazonS3.deleteObject(bucket, objectKey)
              } catch {
                case error: Throwable =>
                  logger.warn(s"adminDeleteSiteFavicon: failed to delete old object from S3. objectKey=$objectKey", error)
              }
            }
            Config.delete(adminFaviconConfigKey)
            Ok(Json.obj(
              "ok" -> Json.fromBoolean(true),
              "siteSeq" -> Json.fromLong(site.seq),
              "faviconUrl" -> Json.fromString("/public/favicon.png"),
            ))
        }
      }
    }
  }

  def adminUsers: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminUser(
          seq: Long,
          created: String,
          updated: String,
          email: String,
          nickname: String,
          siteCount: Long,
          visitCount: Long,
          lastViewed: Option[String],
        )

        val users = SQL"""
          SELECT
            U.seq,
            DATE_FORMAT(U.created, '%Y-%m-%d %H:%i:%s') AS created,
            DATE_FORMAT(U.updated, '%Y-%m-%d %H:%i:%s') AS updated,
            U.email,
            U.nickname,
            COALESCE(US.site_count, 0) AS site_count,
            COALESCE(UV.visit_count, 0) AS visit_count,
            UV.last_viewed
          FROM User U
          LEFT JOIN (
            SELECT user, COUNT(*) AS site_count
            FROM UserSite
            GROUP BY user
          ) US ON US.user = U.seq
          LEFT JOIN (
            SELECT
              user,
              COUNT(*) AS visit_count,
              DATE_FORMAT(MAX(dateInserted), '%Y-%m-%d %H:%i:%s') AS last_viewed
            FROM UserViewHistory
            GROUP BY user
          ) UV ON UV.user = U.seq
          ORDER BY
            CASE WHEN UV.last_viewed IS NULL THEN 1 ELSE 0 END ASC,
            UV.last_viewed DESC,
            U.seq DESC
        """.as((long("seq") ~ str("created") ~ str("updated") ~ str("email") ~ str("nickname") ~ long("site_count") ~ long("visit_count") ~ str("last_viewed").?).map {
          case seq ~ created ~ updated ~ email ~ nickname ~ siteCount ~ visitCount ~ lastViewed =>
            AdminUser(
              seq = seq,
              created = created,
              updated = updated,
              email = email,
              nickname = nickname,
              siteCount = siteCount,
              visitCount = visitCount,
              lastViewed = lastViewed,
            )
        }.*)

        Ok(users.asJson)
      }
    }
  }

  def adminUserViews(userSeq: Long, n: Int = 200): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminUserViewHistory(
          seq: Long,
          user: Long,
          site: Long,
          siteName: String,
          siteDomain: Option[String],
          pageName: String,
          viewedAt: String,
        )

        val limit = math.max(1, math.min(1000, n))

        val histories = SQL"""
          SELECT
            UV.seq,
            UV.user,
            UV.site,
            S.name AS site_name,
            SD.site_domain,
            UV.pageName,
            DATE_FORMAT(UV.dateInserted, '%Y-%m-%d %H:%i:%s') AS viewed_at
          FROM UserViewHistory UV
          INNER JOIN Site S ON S.seq = UV.site
          LEFT JOIN (
            SELECT site, MIN(domain) AS site_domain
            FROM SiteDomain
            GROUP BY site
          ) SD ON SD.site = UV.site
          WHERE UV.user = $userSeq
          ORDER BY UV.seq DESC
          LIMIT $limit
        """.as((long("seq") ~ long("user") ~ long("site") ~ str("site_name") ~ str("site_domain").? ~ str("pageName") ~ str("viewed_at")).map {
          case seq ~ user ~ site ~ siteName ~ siteDomain ~ pageName ~ viewedAt =>
            AdminUserViewHistory(
              seq = seq,
              user = user,
              site = site,
              siteName = siteName,
              siteDomain = siteDomain,
              pageName = pageName,
              viewedAt = viewedAt,
            )
        }.*)

        Ok(histories.asJson)
      }
    }
  }


  def adminSchedulers: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      case class AdminScheduler(
        name: String,
        minSeconds: Int,
        maxSeconds: Int,
        running: Boolean,
        nextDelaySeconds: Option[Int],
        lastStartedAt: Option[String],
        lastFinishedAt: Option[String],
        lastResult: Option[String],
        runCount: Long,
      )

      val schedulers = applicationLifecycleHook.getSchedulerStatuses.map { scheduler =>
        AdminScheduler(
          scheduler.name,
          scheduler.minSeconds,
          scheduler.maxSeconds,
          scheduler.running,
          scheduler.nextDelaySeconds,
          scheduler.lastStartedAt,
          scheduler.lastFinishedAt,
          scheduler.lastResult,
          scheduler.runCount,
        )
      }

      Ok(schedulers.asJson)
    }
  }

  def adminDailyStats: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class DailyCount(ymd: String, count: Long)
        case class AdminDailyStats(
          userCreated: Seq[DailyCount],
          siteUserCreated: Seq[DailyCount],
          pageCreated: Seq[DailyCount],
          pageEdited: Seq[DailyCount],
        )

        val userCreated = SQL"""
          SELECT DATE_FORMAT(U.created, '%Y-%m-%d') ymd, COUNT(*) cnt
          FROM User U
          GROUP BY DATE_FORMAT(U.created, '%Y-%m-%d')
          ORDER BY DATE_FORMAT(U.created, '%Y-%m-%d') DESC
          LIMIT 30
        """.as((str("ymd") ~ long("cnt")).map {
          case ymd ~ cnt => DailyCount(ymd, cnt)
        }.*)

        val siteUserCreated = SQL"""
          SELECT DATE_FORMAT(US.created, '%Y-%m-%d') ymd, COUNT(*) cnt
          FROM UserSite US
          GROUP BY DATE_FORMAT(US.created, '%Y-%m-%d')
          ORDER BY DATE_FORMAT(US.created, '%Y-%m-%d') DESC
          LIMIT 30
        """.as((str("ymd") ~ long("cnt")).map {
          case ymd ~ cnt => DailyCount(ymd, cnt)
        }.*)

        val pageCreated = SQL"""
          SELECT DATE_FORMAT(P.dateTime, '%Y-%m-%d') ymd, COUNT(*) cnt
          FROM Page P
          WHERE P.revision = 1
          GROUP BY DATE_FORMAT(P.dateTime, '%Y-%m-%d')
          ORDER BY DATE_FORMAT(P.dateTime, '%Y-%m-%d') DESC
          LIMIT 30
        """.as((str("ymd") ~ long("cnt")).map {
          case ymd ~ cnt => DailyCount(ymd, cnt)
        }.*)

        val pageEdited = SQL"""
          SELECT DATE_FORMAT(P.dateTime, '%Y-%m-%d') ymd, COUNT(*) cnt
          FROM Page P
          GROUP BY DATE_FORMAT(P.dateTime, '%Y-%m-%d')
          ORDER BY DATE_FORMAT(P.dateTime, '%Y-%m-%d') DESC
          LIMIT 30
        """.as((str("ymd") ~ long("cnt")).map {
          case ymd ~ cnt => DailyCount(ymd, cnt)
        }.*)

        Ok(
          AdminDailyStats(
            userCreated = userCreated,
            siteUserCreated = siteUserCreated,
            pageCreated = pageCreated,
            pageEdited = pageEdited,
          ).asJson
        )
      }
    }
  }

  def adminTopViewedPages: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminTopViewedPage(
          siteSeq: Long,
          siteName: String,
          siteDomain: Option[String],
          pageName: String,
          viewCount: Long,
          lastViewedAt: String,
        )

        val limit = request.getQueryString("n")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(200))
          .getOrElse(30)

        val rows = SQL"""
          SELECT
            UV.site AS site_seq,
            S.name AS site_name,
            SD.site_domain,
            UV.pageName AS page_name,
            COUNT(*) AS view_count,
            DATE_FORMAT(MAX(UV.dateInserted), '%Y-%m-%d %H:%i:%s') AS last_viewed_at
          FROM UserViewHistory UV
          INNER JOIN Site S ON S.seq = UV.site
          LEFT JOIN (
            SELECT site, MIN(domain) AS site_domain
            FROM SiteDomain
            GROUP BY site
          ) SD ON SD.site = UV.site
          GROUP BY UV.site, S.name, SD.site_domain, UV.pageName
          ORDER BY view_count DESC, MAX(UV.dateInserted) DESC
          LIMIT $limit
        """.as((long("site_seq") ~ str("site_name") ~ str("site_domain").? ~ str("page_name") ~ long("view_count") ~ str("last_viewed_at")).map {
          case siteSeq ~ siteName ~ siteDomain ~ pageName ~ viewCount ~ lastViewedAt =>
            AdminTopViewedPage(
              siteSeq = siteSeq,
              siteName = siteName,
              siteDomain = siteDomain,
              pageName = pageName,
              viewCount = viewCount,
              lastViewedAt = lastViewedAt,
            )
        }.*)

        Ok(rows.asJson)
      }
    }
  }

  def adminRecentChanges: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminRecentChange(
          siteSeq: Long,
          siteName: String,
          name: String,
          revision: Long,
          dateTime: String,
          nickname: Option[String],
          remoteAddress: String,
          comment: String,
          isMinorEdit: Boolean,
        )

        val limit = request.getQueryString("n")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(500))
          .getOrElse(50)

        val rows = SQL"""
          SELECT
            P.site AS site_seq,
            S.name AS site_name,
            P.name,
            P.revision,
            DATE_FORMAT(P.dateTime, '%Y-%m-%d %H:%i:%s') AS date_time,
            U.nickname,
            P.remoteAddress,
            P.comment,
            P.isMinorEdit
          FROM Page P
          INNER JOIN Site S ON S.seq = P.site
          LEFT JOIN User U ON U.seq = P.user
          ORDER BY P.dateTime DESC
          LIMIT $limit
        """.as((long("site_seq") ~ str("site_name") ~ str("name") ~ long("revision") ~ str("date_time") ~ str("nickname").? ~ str("remoteAddress") ~ str("comment") ~ bool("isMinorEdit")).map {
          case siteSeq ~ siteName ~ name ~ revision ~ dateTime ~ nickname ~ remoteAddress ~ comment ~ isMinorEdit =>
            AdminRecentChange(
              siteSeq = siteSeq,
              siteName = siteName,
              name = name,
              revision = revision,
              dateTime = dateTime,
              nickname = nickname,
              remoteAddress = remoteAddress,
              comment = comment,
              isMinorEdit = isMinorEdit,
            )
        }.*)

        Ok(rows.asJson)
      }
    }
  }

  def adminRunScheduler(name: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      if (applicationLifecycleHook.runSchedulerNow(name)) {
        Ok(Map("status" -> "queued", "name" -> name).asJson)
      } else {
        NotFound(name)
      }
    }
  }

  def csrf: Action[AnyContent] = Action { implicit request =>
    val token: Option[CSRF.Token] = CSRF.getToken
    Ok(token.asJson)
  }

  def pageMap: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      val listLink = Random.shuffle(CalculatedLink.selectAllButNotEmpty()).take(10)
      Ok(listLink.asJson)
    }
  }

  def pageNames: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()
      implicit val requestWrapper: RequestWrapper = RequestWrapper()
      Ok(PageLogic.getListPageByPermission().map(_.name).asJson)
    }
  }

  def macroNames: Action[AnyContent] = Action { implicit request =>
    Ok(ExtractConvertInjectMacro.macroNames.asJson)
  }

  def interpreterNames: Action[AnyContent] = Action { implicit request =>
    Ok(Interpreters.map.values.map(_.name).toSeq.distinct.sorted.asJson)
  }

  def schemaClassNames: Action[AnyContent] = Action { implicit request =>
    Ok(logics.CalculatedSchemaOrg.mapClass.keys.toSeq.sorted.asJson)
  }

  def schemaPropertyNames: Action[AnyContent] = Action { implicit request =>
    val schemaClass: String = request.getQueryString("schemaClass").map(_.trim).getOrElse("")
    val source: String = request.getQueryString("source").map(_.trim).filter(_.nonEmpty).getOrElse("class-or-recommended")
    implicit val site: Site = SiteLogic.get(request.host)

    val allProperties: Seq[String] = logics.CalculatedSchemaOrg.mapProperty.keys.toSeq.sorted

    val properties = if (schemaClass.isEmpty) {
      allProperties
    } else {
      val classProperties = logics.CalculatedSchemaOrg.seqProperty
        .filter(_.domainIncludes.contains(schemaClass))
        .map(_.id)
        .distinct
        .sorted
      val recommendedProperties = database.withConnection { implicit connection =>
        models.tables.CalculatedSchemaOrg.selectPropCountWhereCls(schemaClass).map(_.prop)
      }

      source match {
        case "recommended" => recommendedProperties
        case "class" => classProperties
        case _ => (recommendedProperties ++ classProperties).distinct
      }
    }

    Ok(properties.asJson)
  }


  def links(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()
      Ok(Adjacent.getSeqLinkFiltered(name).asJson)
    }
  }

  def statistics(): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("")

      val seqPage: Seq[PageWithoutContentWithSize] = contextWikiPage.seqPageByPermission
      val selectYmdCountOfFirstRevision: Seq[(String, Long)] = Page.selectYmdCountOfFirstRevision()

      val totalSize: Long = seqPage.map(_.size).sum

      val value1: Map[String, Seq[(String, Long)]] = Map(
        "arrayArrayYmdCountOfFirstRevision" -> selectYmdCountOfFirstRevision,
      )
      val value2: Map[String, Long] = Map(
        "totalSize" -> totalSize,
        "pageCount" -> seqPage.length,
        "count" -> seqPage.length,
      )

      val json1: Json = value1.asJson
      val json2: Json = value2.asJson

      Ok(json1.deepMerge(json2))
    }
  }

  def cacheDelete(siteSeq: Long): Action[AnyContent] = Action { implicit request =>
    SiteLogic.get(siteSeq) foreach { implicit site =>
      implicit val tupleDatabaseSite: (Database, Site) = (database, site)
      implicit val contextSite: ContextSite = ContextSite()
      Seq(
        () => { ahaWikiCache.SiteDomain.invalidate() },
        () => { ahaWikiCache.SiteDomain.Map.invalidate() },
        () => { ahaWikiCache.Site.invalidate() },
        () => { ahaWikiCache.Site.Map.invalidate() },
        () => { ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.invalidate() },
        () => { ahaWikiCache.Header.invalidate() },
      ).zipWithIndex foreach { case (f, i) =>
        actorSystem.scheduler.scheduleOnce((2 * i) second) {f()}
      }
    }
    Ok("ok")
  }
}
