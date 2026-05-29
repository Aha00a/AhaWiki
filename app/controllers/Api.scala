package controllers

import org.apache.pekko.actor.ActorSystem
import anorm.SqlParser.{bool, get, int, long, str}
import anorm._
import com.amazonaws.auth.AWSStaticCredentialsProvider
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.amazonaws.services.s3.model.DeleteObjectsRequest
import com.amazonaws.services.s3.model.ListObjectsV2Request
import com.amazonaws.services.s3.model.ObjectMetadata
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.IpAddressUtil
import com.aha00a.play.Implicits.RichRequest
import io.circe.Json
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryApiLinks
import logics.AhaWikiCacheMemoryDomainSite
import logics.AhaWikiCacheMemoryPermission
import logics.AhaWikiCacheMemoryApiLinks.Snapshot
import logics.ApplicationConf
import logics.PermissionLogic
import logics.SessionLogic
import logics.SiteLogic
import logics.SiteThemeLogic
import logics.wikis.PageLogic
import logics.wikis.WikiPermission
import logics.wikis.SignedReadUrlLogic
import logics.wikis.ExtractConvertInjectMacro
import logics.wikis.interpreters.Interpreters
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.macros.S3AttachmentUrlLogic
import models.Adjacent
import models.ContextSite
import models.ContextWikiPage
import models.PageContent
import models.PageLatestSummary
import models.RequestWrapper
import models.WikiActors
import models.tables.CalculatedLink
import models.tables.Page
import models.tables.Site
import models.tables.Config
import models.tables.Permission
import models.tables.SiteAdmin
import play.api.Configuration
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{Json => PlayJson}
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
import scala.util.Try


class Api @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  applicationLifecycleHook: ApplicationLifecycleHook,
  configuration: Configuration,
  syncCacheApi: SyncCacheApi,
  ahaWikiCacheMemoryApiLinks: AhaWikiCacheMemoryApiLinks,
) extends BaseController with Logging {
  private case class MemoryCacheStatsPayload(instancePort: String, stats: Snapshot)

  private def isAdmin(implicit request: RequestHeader): Boolean =
    logics.AdminLogic.isAdmin(request)

  private def isSiteAdmin(siteSeq: Long)(implicit request: RequestHeader): Boolean =
    logics.AdminLogic.isSiteAdmin(siteSeq, request)(database)

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)


  private lazy val signedReadUrlSecret: String = configuration.getOptional[String]("play.http.secret.key").getOrElse("")
  private val memoryCacheSnapshotKey = "admin:memoryCacheStats:instances"
  private val instancePort: String = configuration.getOptional[String]("play.server.http.port").getOrElse("unknown")
  private def readMemoryCacheSnapshots(): Map[String, Snapshot] = {
    syncCacheApi
      .get[String](memoryCacheSnapshotKey)
      .flatMap(json => decode[Map[String, Snapshot]](json).toOption)
      .getOrElse(Map.empty)
  }

  private def writeMemoryCacheSnapshots(stats: Map[String, Snapshot]): Unit = {
    syncCacheApi.set(memoryCacheSnapshotKey, stats.asJson.noSpaces, 10.minutes)
  }

  applicationLifecycleHook.scheduleWithDynamicDelay(
    name = "ahaWikiCacheMemoryApiLinks",
    initialDelay = scala.concurrent.duration.Duration.Zero,
    nextDelay = () => 5 minutes,
    job = () => {
      val currentSnapshot = ahaWikiCacheMemoryApiLinks.snapshot(instancePort)
      val merged = readMemoryCacheSnapshots() + (instancePort -> currentSnapshot)
      writeMemoryCacheSnapshots(merged)
    },
  )

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
  private def parseHexColorValue(v: Option[String]): String = v.flatMap(SiteThemeLogic.normalizeHexColor).getOrElse("")

  private def parsePublicListedOrder(v: Option[String]): Either[String, Option[BigDecimal]] = {
    val raw = v.map(_.trim).getOrElse("")
    if (raw.isEmpty) {
      Right(None)
    } else {
      scala.util.Try(BigDecimal(raw)).toOption
        .filter(_ >= 0)
        .map(value => Right(Some(value)))
        .getOrElse(Left("publicListedOrder must be empty or a non-negative number."))
    }
  }

  private def permissionJson(permission: Permission): Json = Json.obj(
    "target" -> Json.fromString(permission.target),
    "targetType" -> Json.fromString(permission.targetType.toString),
    "actor" -> Json.fromString(permission.actor),
    "actorType" -> Json.fromString(permission.actorType.toString),
    "action" -> Json.fromInt(permission.action),
    "actionName" -> Json.fromString(Permission.Action.values.find(_.id == permission.action).map(_.toString).getOrElse(permission.action.toString)),
    "specificity" -> Json.fromInt(permission.specificity),
    "targetLevel" -> Json.fromInt(permission.targetLevel),
    "actorLevel" -> Json.fromInt(permission.actorLevel),
  )

  private def parsePermissionPayload(form: Map[String, Seq[String]], query: String => Option[String] = _ => None): Either[String, Permission] = {
    def field(name: String): String = form.get(name).flatMap(_.headOption).orElse(query(name)).map(_.trim).getOrElse("")
    for {
      targetType <- Permission.parseTargetType(field("targetType"))
      actorType <- Permission.parseActorType(field("actorType"))
      action <- Permission.parseAction(field("action"))
      permission <- Permission.validate(Permission(
        target = if (targetType == Permission.TargetType.All) "" else field("target"),
        targetType = targetType,
        actor = if (actorType == Permission.ActorType.All || actorType == Permission.ActorType.Login) "" else field("actor"),
        actorType = actorType,
        action = action,
      ))
    } yield permission
  }

  private def parsePermissionKey(query: String => Option[String]): Either[String, Permission] = {
    def field(name: String): String = query(name).map(_.trim).getOrElse("")
    for {
      targetType <- Permission.parseTargetType(field("targetType"))
      actorType <- Permission.parseActorType(field("actorType"))
    } yield Permission(
      target = if (targetType == Permission.TargetType.All) "" else field("target"),
      targetType = targetType,
      actor = if (actorType == Permission.ActorType.All || actorType == Permission.ActorType.Login) "" else field("actor"),
      actorType = actorType,
      action = Permission.Action.None.id,
    )
  }

  private def resolveAdminTargetSite(siteSeqValue: Option[String])(implicit request: RequestHeader): Either[Result, Site] = {
    siteSeqValue
      .flatMap(parseSiteSeq)
      .flatMap(seq => SiteLogic.get(seq)(database))
      .toRight(BadRequest(Json.obj("error" -> Json.fromString("Valid siteSeq is required.")).toString()).as(JSON))
  }

  private def resolveAdminTargetSiteWithAuth(siteSeqValue: Option[String])(implicit request: RequestHeader): Either[Result, Site] = {
    resolveAdminTargetSite(siteSeqValue).flatMap { site =>
      if (isSiteAdmin(site.seq)) Right(site)
      else Left(Forbidden("Access denied."))
    }
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
    val userOpt = logics.SessionLogic.getUser(request)
    val siteAdminSeqsOpt: Option[Set[Long]] = if (isAdmin) None else {
      userOpt.map { user =>
        database.withConnection { implicit connection =>
          SiteAdmin.selectByUser(user.seq).map(_.site).toSet
        }
      }
    }
    if (!isAdmin && siteAdminSeqsOpt.forall(_.isEmpty)) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminSite(seq: Long, name: String, abbr: String, mainDomain: String, publicListedOrder: Option[BigDecimal], domains: Seq[String], pageCount: Long)
        case class AdminSiteRow(seq: Long, name: String, abbr: String, mainDomain: String, publicListedOrder: Option[BigDecimal], domain: Option[String], pageCount: Long)

        val rows = SQL"""
          SELECT
            S.seq,
            S.name,
            S.abbr,
            S.mainDomain,
            S.publicListedOrder,
            SD.domain,
            COALESCE(P.page_count, 0) AS page_count
          FROM Site S
          LEFT JOIN SiteDomain SD ON SD.site = S.seq
          LEFT JOIN (
            SELECT site, COUNT(*) AS page_count
            FROM Page
            GROUP BY site
          ) P ON P.site = S.seq
          ORDER BY S.seq, SD.domain
        """.as((long("seq") ~ str("name") ~ str("abbr") ~ str("mainDomain") ~ get[Option[BigDecimal]]("publicListedOrder") ~ str("domain").? ~ long("page_count")).map {
          case seq ~ name ~ abbr ~ mainDomain ~ publicListedOrder ~ domain ~ pageCount =>
            AdminSiteRow(seq, name, abbr, mainDomain, publicListedOrder, domain, pageCount)
        }.*)

        val sites = rows
          .groupBy(r => (r.seq, r.name, r.abbr, r.mainDomain, r.publicListedOrder, r.pageCount))
          .toSeq
          .sortBy(_._1._1)
          .map { case ((seq, name, abbr, mainDomain, publicListedOrder, pageCount), groupedRows) =>
            AdminSite(
              seq = seq,
              name = name,
              abbr = abbr,
              mainDomain = mainDomain,
              publicListedOrder = publicListedOrder,
              domains = groupedRows.flatMap(_.domain).distinct.sorted,
              pageCount = pageCount,
            )
          }

        val filtered = siteAdminSeqsOpt match {
          case None       => sites
          case Some(seqs) => sites.filter(s => seqs.contains(s.seq))
        }

        Ok(filtered.asJson)
      }
    }
  }

  def adminUpdateSite(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      val form = request.body.asFormUrlEncoded.getOrElse(Map.empty)
      val abbr = form.get("abbr").flatMap(_.headOption).map(_.trim).getOrElse("")
      val mainDomain = form.get("mainDomain").flatMap(_.headOption).map(_.trim).getOrElse("")
      if (abbr.isEmpty) {
        BadRequest(Map("error" -> "abbr is required").asJson.toString()).as(JSON)
      } else {
        parsePublicListedOrder(form.get("publicListedOrder").flatMap(_.headOption)) match {
          case Left(errorMessage) =>
            BadRequest(Map("error" -> errorMessage).asJson.toString()).as(JSON)
          case Right(publicListedOrder) =>
            database.withConnection { implicit connection =>
              try {
                val updated = Site.updateAbbrAndMainDomain(seq, abbr, mainDomain, publicListedOrder)
                if (updated == 0) {
                  NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
                } else {
                  AhaWikiCacheMemoryDomainSite.invalidate()
                  SiteLogic.get(seq)(database) match {
                    case Some(site) => Ok(site.asJson)
                    case None => NotFound(Map("error" -> s"site not found after update: $seq").asJson.toString()).as(JSON)
                  }
                }
              } catch {
                case e: java.sql.SQLIntegrityConstraintViolationException =>
                  BadRequest(Map("error" -> Option(e.getMessage).getOrElse("site update violates constraints")).asJson.toString()).as(JSON)
              }
            }
        }
      }
    }
  }

  def adminPermissions(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isSiteAdmin(seq)) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(site) =>
          database.withConnection { implicit connection =>
            implicit val implicitSite: Site = site
            val permissions = Permission.select()
            Ok(Json.obj(
              "siteSeq" -> Json.fromLong(site.seq),
              "permissions" -> Json.fromValues(permissions.map(permissionJson)),
            ))
          }
      }
    }
  }

  def adminUpsertPermission(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isSiteAdmin(seq)) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(site) =>
          val form = request.body.asFormUrlEncoded.getOrElse(Map.empty)
          parsePermissionPayload(form) match {
            case Left(error) => BadRequest(Json.obj("error" -> Json.fromString(error)).toString()).as(JSON)
            case Right(permission) =>
              database.withConnection { implicit connection =>
                implicit val implicitSite: Site = site
                Permission.upsert(permission)
                AhaWikiCacheMemoryPermission.invalidate(site.seq)
                Ok(Json.obj(
                  "ok" -> Json.fromBoolean(true),
                  "siteSeq" -> Json.fromLong(site.seq),
                  "permission" -> permissionJson(permission),
                ))
              }
          }
      }
    }
  }

  def adminDeletePermission(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isSiteAdmin(seq)) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(site) =>
          parsePermissionKey(request.getQueryString) match {
            case Left(error) => BadRequest(Json.obj("error" -> Json.fromString(error)).toString()).as(JSON)
            case Right(permission) =>
              database.withConnection { implicit connection =>
                implicit val implicitSite: Site = site
                val deletedCount = Permission.delete(permission)
                AhaWikiCacheMemoryPermission.invalidate(site.seq)
                Ok(Json.obj(
                  "ok" -> Json.fromBoolean(true),
                  "siteSeq" -> Json.fromLong(site.seq),
                  "deletedCount" -> Json.fromInt(deletedCount),
                ))
              }
          }
      }
    }
  }

  def adminSiteAdmins(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(_) =>
          database.withConnection { implicit connection =>
            val admins = SiteAdmin.selectBySite(seq).map(sa =>
              Json.obj(
                "site" -> Json.fromLong(sa.site),
                "user" -> Json.fromLong(sa.user),
                "dateInserted" -> Json.fromString(sa.dateInserted.toString),
              )
            )
            Ok(admins.asJson)
          }
      }
    }
  }

  def adminInsertSiteAdmin(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(_) =>
          val form = request.body.asFormUrlEncoded.getOrElse(Map.empty)
          form.get("user").flatMap(_.headOption).flatMap(s => s.toLongOption) match {
            case None => BadRequest(Json.obj("error" -> Json.fromString("user is required")).toString()).as(JSON)
            case Some(userSeq) =>
              database.withConnection { implicit connection =>
                SiteAdmin.insert(seq, userSeq)
                logics.AhaWikiCacheMemorySiteAdmin.invalidate(seq)
                Ok(Json.obj("ok" -> Json.fromBoolean(true), "site" -> Json.fromLong(seq), "user" -> Json.fromLong(userSeq)))
              }
          }
      }
    }
  }

  def adminDeleteSiteAdmin(seq: Long, userSeq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(_) =>
          database.withConnection { implicit connection =>
            val deletedCount = SiteAdmin.delete(seq, userSeq)
            logics.AhaWikiCacheMemorySiteAdmin.invalidate(seq)
            Ok(Json.obj("ok" -> Json.fromBoolean(true), "deletedCount" -> Json.fromInt(deletedCount)))
          }
      }
    }
  }

  def adminPermissionDiagnose(seq: Long, pageName: String, actor: String, action: String): Action[AnyContent] = Action { implicit request =>
    if (!isSiteAdmin(seq)) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(site) =>
          Permission.parseAction(action) match {
            case Left(error) => BadRequest(Json.obj("error" -> Json.fromString(error)).toString()).as(JSON)
            case Right(requiredAction) =>
              database.withConnection { implicit connection =>
                implicit val implicitSite: Site = site
                val logic = new PermissionLogic(Permission.select())
                val matched = logic.matched(pageName, actor)
                Ok(Json.obj(
                  "siteSeq" -> Json.fromLong(site.seq),
                  "pageName" -> Json.fromString(pageName),
                  "actor" -> Json.fromString(actor),
                  "requiredAction" -> Json.fromInt(requiredAction),
                  "permitted" -> Json.fromBoolean(matched.exists(_.permitted(requiredAction))),
                  "matchedPermission" -> matched.map(permissionJson).getOrElse(Json.Null),
                ))
              }
          }
      }
    }
  }

  def adminPermissionAudit: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        val rows = Permission.auditSites()
        Ok(Json.obj(
          "sites" -> Json.fromValues(rows.map { row =>
            Json.obj(
              "siteSeq" -> Json.fromLong(row.siteSeq),
              "siteName" -> Json.fromString(row.siteName),
              "permissionCount" -> Json.fromLong(row.permissionCount),
              "hasAnyPermission" -> Json.fromBoolean(row.permissionCount > 0),
              "hasPublicRead" -> Json.fromBoolean(row.publicReadRows > 0),
              "hasLoginCreate" -> Json.fromBoolean(row.loginCreateRows > 0),
              "publicReadRows" -> Json.fromLong(row.publicReadRows),
              "loginCreateRows" -> Json.fromLong(row.loginCreateRows),
            )
          }),
        ))
      }
    }
  }

  def adminSiteFavicon: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      resolveAdminTargetSiteWithAuth(request.getQueryString("siteSeq")) match {
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

  def adminUploadSiteFavicon: Action[MultipartFormData[TemporaryFile]] = Action(parse.multipartFormData) { implicit request =>
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
            resolveAdminTargetSiteWithAuth(siteSeqValue) match {
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

  def adminDeleteSiteFavicon: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      resolveAdminTargetSiteWithAuth(request.getQueryString("siteSeq")) match {
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

  def adminSiteTheme: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      resolveAdminTargetSiteWithAuth(request.getQueryString("siteSeq")) match {
          case Left(errorResult) => errorResult
          case Right(siteValue) =>
            implicit val site: Site = siteValue
            val headerBackgroundColor = parseHexColorValue(Config.select(SiteThemeLogic.HeaderBackgroundColorKey).map(_.v))
            val headerForegroundColor = parseHexColorValue(Config.select(SiteThemeLogic.HeaderForegroundColorKey).map(_.v))
            val bodyBackgroundColor = parseHexColorValue(Config.select(SiteThemeLogic.BodyBackgroundColorKey).map(_.v))
            val bodyForegroundColor = parseHexColorValue(Config.select(SiteThemeLogic.BodyForegroundColorKey).map(_.v))
            val footerBackgroundColor = parseHexColorValue(Config.select(SiteThemeLogic.FooterBackgroundColorKey).map(_.v))
            val footerForegroundColor = parseHexColorValue(Config.select(SiteThemeLogic.FooterForegroundColorKey).map(_.v))
            val defaultHue = Config.select(SiteThemeLogic.DefaultHueKey).flatMap(c => SiteThemeLogic.parseHue(c.v)).map(_.toString).getOrElse("")
            Ok(Json.obj(
              "siteSeq" -> Json.fromLong(site.seq),
              "headerBackgroundColor" -> Json.fromString(headerBackgroundColor),
              "headerForegroundColor" -> Json.fromString(headerForegroundColor),
              "bodyBackgroundColor" -> Json.fromString(bodyBackgroundColor),
              "bodyForegroundColor" -> Json.fromString(bodyForegroundColor),
              "footerBackgroundColor" -> Json.fromString(footerBackgroundColor),
              "footerForegroundColor" -> Json.fromString(footerForegroundColor),
              "defaultHue" -> Json.fromString(defaultHue),
            ))
      }
    }
  }

  def adminUpdateSiteTheme: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      val body = request.body.asFormUrlEncoded.getOrElse(Map.empty)
      val siteSeqValue = body.get("siteSeq").flatMap(_.headOption).orElse(request.getQueryString("siteSeq"))
      resolveAdminTargetSiteWithAuth(siteSeqValue) match {
          case Left(errorResult) => errorResult
          case Right(siteValue) =>
            implicit val site: Site = siteValue
            val headerBackgroundColor = parseHexColorValue(body.get("headerBackgroundColor").flatMap(_.headOption))
            val headerForegroundColor = parseHexColorValue(body.get("headerForegroundColor").flatMap(_.headOption))
            val bodyBackgroundColor = parseHexColorValue(body.get("bodyBackgroundColor").flatMap(_.headOption))
            val bodyForegroundColor = parseHexColorValue(body.get("bodyForegroundColor").flatMap(_.headOption))
            val footerBackgroundColor = parseHexColorValue(body.get("footerBackgroundColor").flatMap(_.headOption))
            val footerForegroundColor = parseHexColorValue(body.get("footerForegroundColor").flatMap(_.headOption))
            val defaultHue = SiteThemeLogic.parseHue(body.get("defaultHue").flatMap(_.headOption).getOrElse("")).map(_.toString).getOrElse("")

            Seq(
              SiteThemeLogic.HeaderBackgroundColorKey -> headerBackgroundColor,
              SiteThemeLogic.HeaderForegroundColorKey -> headerForegroundColor,
              SiteThemeLogic.BodyBackgroundColorKey -> bodyBackgroundColor,
              SiteThemeLogic.BodyForegroundColorKey -> bodyForegroundColor,
              SiteThemeLogic.FooterBackgroundColorKey -> footerBackgroundColor,
              SiteThemeLogic.FooterForegroundColorKey -> footerForegroundColor,
              SiteThemeLogic.DefaultHueKey -> defaultHue,
            ).foreach { case (key, value) =>
              if (value.nonEmpty) {
                Config.upsert(key, value)
              } else {
                Config.delete(key)
              }
            }

            Ok(Json.obj(
              "ok" -> Json.fromBoolean(true),
              "siteSeq" -> Json.fromLong(site.seq),
              "headerBackgroundColor" -> Json.fromString(headerBackgroundColor),
              "headerForegroundColor" -> Json.fromString(headerForegroundColor),
              "bodyBackgroundColor" -> Json.fromString(bodyBackgroundColor),
              "bodyForegroundColor" -> Json.fromString(bodyForegroundColor),
              "footerBackgroundColor" -> Json.fromString(footerBackgroundColor),
              "footerForegroundColor" -> Json.fromString(footerForegroundColor),
              "defaultHue" -> Json.fromString(defaultHue),
            ))
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
          profileImageUrl: Option[String],
          visitCount: Long,
          lastViewed: Option[String],
        )

        val page = request.getQueryString("page")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1))
          .getOrElse(1)
        val pageSize = request.getQueryString("pageSize")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(1000))
          .getOrElse(20)
        val search = request.getQueryString("search").map(_.trim).getOrElse("")
        val sortByRaw = request.getQueryString("sortBy").map(_.trim).getOrElse("seq")
        val sortOrderRaw = request.getQueryString("sortOrder").map(_.trim.toLowerCase).getOrElse("desc")

        val sortBy = sortByRaw match {
          case "created" => "U.created"
          case "updated" => "U.updated"
          case "email" => "email"
          case "nickname" => "U.nickname"
          case "visitCount" => "visit_count"
          case "lastViewed" => "last_viewed_raw"
          case _ => "U.seq"
        }
        val sortOrder = if (sortOrderRaw == "asc") "ASC" else "DESC"
        val offset = (page - 1) * pageSize
        val searchLike = s"%$search%"

        val count = SQL"""
          SELECT COUNT(*) AS count_value
          FROM User U
          LEFT JOIN (
            SELECT
              user,
              GROUP_CONCAT(email ORDER BY isPrimary DESC, email SEPARATOR ', ') AS emails
            FROM UserEmail
            GROUP BY user
          ) UE ON UE.user = U.seq
          WHERE (
            ${search.isEmpty} = TRUE OR
            UE.emails LIKE $searchLike OR
            U.nickname LIKE $searchLike OR
            CAST(U.seq AS CHAR) LIKE $searchLike
          )
        """.as(long("count_value").single)

        val orderBySql = s"$sortBy $sortOrder"
        val users = SQL(s"""
          SELECT
            U.seq,
            DATE_FORMAT(U.created, '%Y-%m-%d %H:%i:%s') AS created,
            DATE_FORMAT(U.updated, '%Y-%m-%d %H:%i:%s') AS updated,
            COALESCE(UE.primary_email, UE.fallback_email, '') AS email,
            U.nickname,
            U.profileImageUrl,
            COALESCE(UV.visit_count, 0) AS visit_count,
            UV.last_viewed,
            UV.last_viewed_raw
          FROM User U
          LEFT JOIN (
            SELECT
              user,
              COUNT(*) AS visit_count,
              MAX(dateInserted) AS last_viewed_raw,
              DATE_FORMAT(MAX(dateInserted), '%Y-%m-%d %H:%i:%s') AS last_viewed
            FROM UserViewHistory
            GROUP BY user
          ) UV ON UV.user = U.seq
          LEFT JOIN (
            SELECT
              user,
              MIN(CASE WHEN isPrimary THEN email ELSE NULL END) AS primary_email,
              MIN(email) AS fallback_email,
              GROUP_CONCAT(email ORDER BY isPrimary DESC, email SEPARATOR ', ') AS emails
            FROM UserEmail
            GROUP BY user
          ) UE ON UE.user = U.seq
          WHERE (
            {searchIsEmpty} = TRUE OR
            UE.emails LIKE {searchLike} OR
            U.nickname LIKE {searchLike} OR
            CAST(U.seq AS CHAR) LIKE {searchLike}
          )
          ORDER BY $orderBySql
          LIMIT {pageSize} OFFSET {offset}
        """).on(
          "searchIsEmpty" -> search.isEmpty,
          "searchLike" -> searchLike,
          "pageSize" -> pageSize,
          "offset" -> offset,
        ).as((long("seq") ~ str("created") ~ str("updated") ~ str("email") ~ str("nickname") ~ str("profileImageUrl").? ~ long("visit_count") ~ str("last_viewed").?).map {
          case seq ~ created ~ updated ~ email ~ nickname ~ profileImageUrl ~ visitCount ~ lastViewed =>
            AdminUser(
              seq = seq,
              created = created,
              updated = updated,
              email = email,
              nickname = nickname,
              profileImageUrl = profileImageUrl,
              visitCount = visitCount,
              lastViewed = lastViewed,
            )
        }.*)

        Ok(Map(
          "array" -> users.asJson,
          "page" -> page.asJson,
          "pageSize" -> pageSize.asJson,
          "count" -> count.asJson,
        ).asJson)
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


  def adminDailyStats: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class DailyCount(ymd: String, count: Long)
        case class AdminDailyStats(
          userCreated: Seq[DailyCount],
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
          siteDomain: Option[String],
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
            SD.site_domain,
            P.name,
            P.revision,
            DATE_FORMAT(P.dateTime, '%Y-%m-%d %H:%i:%s') AS date_time,
            U.nickname,
            P.remoteAddress,
            P.comment,
            P.isMinorEdit
          FROM Page P
          INNER JOIN Site S ON S.seq = P.site
          LEFT JOIN (
            SELECT site, MIN(domain) AS site_domain
            FROM SiteDomain
            GROUP BY site
          ) SD ON SD.site = P.site
          LEFT JOIN User U ON U.seq = P.user
          ORDER BY P.dateTime DESC
          LIMIT $limit
        """.as((long("site_seq") ~ str("site_name") ~ str("site_domain").? ~ str("name") ~ long("revision") ~ str("date_time") ~ str("nickname").? ~ str("remoteAddress") ~ str("comment") ~ bool("isMinorEdit")).map {
          case siteSeq ~ siteName ~ siteDomain ~ name ~ revision ~ dateTime ~ nickname ~ remoteAddress ~ comment ~ isMinorEdit =>
            AdminRecentChange(
              siteSeq = siteSeq,
              siteName = siteName,
              siteDomain = siteDomain,
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

  def change(name: String, includeMinorEdit: Int, limit: Int): Action[AnyContent] = Action { implicit request =>
    // TODO: improve check permission
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)

      case class ChangeRow(name: String, revision: Long, dateTime: String, nickname: Option[String], profileImageUrl: Option[String], remoteAddressMasked: String, comment: String, commentInlineHtml: String, isMinorEdit: Boolean)
      case class ChangeSourceRow(name: String, revision: Long, dateTime: String, nickname: Option[String], profileImageUrl: Option[String], remoteAddress: String, comment: String, isMinorEdit: Boolean, content: String)

      implicit val provider: RequestWrapper = RequestWrapper()
      val wikiPermission = WikiPermission()
      val boundedLimit = limit.max(1).min(1000)

      val rows = SQL"""
        SELECT
          P.name,
          P.revision,
          DATE_FORMAT(P.dateTime, '%Y-%m-%d %H:%i:%s') AS date_time,
          U.nickname,
          U.profileImageUrl,
          P.remoteAddress,
          P.comment,
          P.isMinorEdit,
          P.content
        FROM Page P
        LEFT JOIN User U ON U.seq = P.user
        WHERE P.site = ${site.seq}
          AND (${includeMinorEdit == 1} OR P.isMinorEdit = false)
        ORDER BY P.dateTime DESC
        LIMIT $boundedLimit
      """.as((str("name") ~ long("revision") ~ str("date_time") ~ str("nickname").? ~ str("profileImageUrl").? ~ str("remoteAddress") ~ str("comment") ~ bool("isMinorEdit") ~ str("content")).map {
        case name ~ revision ~ dateTime ~ nickname ~ profileImageUrl ~ remoteAddress ~ comment ~ isMinorEdit ~ content =>
          ChangeSourceRow(name, revision, dateTime, nickname, profileImageUrl, remoteAddress, comment, isMinorEdit, content)
      }.*)

      val readableRows = rows.filter(row => wikiPermission.isReadable(row.name, PageContent(row.content))).map { row =>
        ChangeRow(
          name = row.name,
          revision = row.revision,
          dateTime = row.dateTime,
          nickname = row.nickname,
          profileImageUrl = row.profileImageUrl,
          remoteAddressMasked = IpAddressUtil.mask(row.remoteAddress),
          comment = row.comment,
          commentInlineHtml = InterpreterWiki.inlineToHtmlString(row.comment),
          isMinorEdit = row.isMinorEdit,
        )
      }

      Ok(readableRows.asJson)
    }
  }

  def adminAccessLogs: Action[AnyContent] = Action { implicit request =>
    val requestedSiteSeq = request.getQueryString("siteSeq")
      .flatMap(raw => scala.util.Try(raw.toLong).toOption)
      .filter(_ > 0)
    val permitted = isAdmin || requestedSiteSeq.exists(seq => isSiteAdmin(seq))
    if (!permitted) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminAccessLog(
          seq: Long,
          siteSeq: Long,
          siteName: String,
          ipDenySeq: Option[Long],
          userSeq: Option[Long],
          method: String,
          scheme: String,
          host: String,
          uri: String,
          status: Int,
          remoteAddress: String,
          durationMilli: Int,
          userAgent: String,
          dateInserted: String,
        )

        val page = request.getQueryString("page")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1))
          .getOrElse(1)
        val pageSize = request.getQueryString("pageSize")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(1000))
          .getOrElse(20)
        val search = request.getQueryString("search").map(_.trim).getOrElse("")
        // SiteAdmin은 자신의 사이트로 siteSeq 강제 적용
        val siteSeq = if (isAdmin) {
          request.getQueryString("siteSeq")
            .flatMap(raw => scala.util.Try(raw.toLong).toOption)
            .filter(_ > 0)
        } else {
          requestedSiteSeq
        }
        val sortByRaw = request.getQueryString("sortBy").map(_.trim).getOrElse("seq")
        val sortOrderRaw = request.getQueryString("sortOrder").map(_.trim.toLowerCase).getOrElse("desc")

        val sortBy = sortByRaw match {
          case "dateInserted" => "AL.dateInserted"
          case "status" => "AL.status"
          case "durationMilli" => "AL.durationMilli"
          case "remoteAddress" => "AL.remoteAddress"
          case "method" => "AL.method"
          case "uri" => "AL.uri"
          case _ => "AL.seq"
        }
        val sortOrder = if (sortOrderRaw == "asc") "ASC" else "DESC"
        val offset = (page - 1) * pageSize
        val searchLike = s"%$search%"
        val siteSeqIsEmpty = siteSeq.isEmpty
        val siteSeqValue: Long = siteSeq.getOrElse(0L)
        val searchIsEmpty = search.isEmpty

        val count = SQL"""
          SELECT COUNT(*) AS count_value
          FROM AccessLog AL
          INNER JOIN Site S ON S.seq = AL.site
          WHERE (
            $siteSeqIsEmpty = TRUE OR AL.site = $siteSeqValue
          ) AND (
            $searchIsEmpty = TRUE OR
            S.name LIKE $searchLike OR
            AL.method LIKE $searchLike OR
            AL.uri LIKE $searchLike OR
            AL.remoteAddress LIKE $searchLike OR
            AL.userAgent LIKE $searchLike
          )
        """.as(long("count_value").single)

        val orderBySql = s"$sortBy $sortOrder"
        val rows = SQL(s"""
          SELECT
            AL.seq,
            AL.site AS site_seq,
            S.name AS site_name,
            AL.ipDeny AS ip_deny_seq,
            AL.user AS user_seq,
            AL.method,
            AL.scheme,
            AL.host,
            AL.uri,
            AL.status,
            AL.remoteAddress,
            AL.durationMilli,
            AL.userAgent,
            DATE_FORMAT(AL.dateInserted, '%Y-%m-%d %H:%i:%s') AS date_inserted
          FROM AccessLog AL
          INNER JOIN Site S ON S.seq = AL.site
          WHERE (
            {siteSeqIsEmpty} = TRUE OR AL.site = {siteSeq}
          ) AND (
            {searchIsEmpty} = TRUE OR
            S.name LIKE {searchLike} OR
            AL.method LIKE {searchLike} OR
            AL.uri LIKE {searchLike} OR
            AL.remoteAddress LIKE {searchLike} OR
            AL.userAgent LIKE {searchLike}
          )
          ORDER BY $orderBySql
          LIMIT {pageSize} OFFSET {offset}
        """).on(
          "searchIsEmpty" -> searchIsEmpty,
          "siteSeqIsEmpty" -> siteSeqIsEmpty,
          "siteSeq" -> siteSeqValue,
          "searchLike" -> searchLike,
          "pageSize" -> pageSize,
          "offset" -> offset,
        ).as((long("seq") ~ long("site_seq") ~ str("site_name") ~ long("ip_deny_seq").? ~ long("user_seq").? ~ str("method") ~ str("scheme") ~ str("host") ~ str("uri") ~ int("status") ~ str("remoteAddress") ~ int("durationMilli") ~ str("userAgent") ~ str("date_inserted")).map {
          case seq ~ siteSeq ~ siteName ~ ipDenySeq ~ userSeq ~ method ~ scheme ~ host ~ uri ~ status ~ remoteAddress ~ durationMilli ~ userAgent ~ dateInserted =>
            AdminAccessLog(
              seq = seq,
              siteSeq = siteSeq,
              siteName = siteName,
              ipDenySeq = ipDenySeq,
              userSeq = userSeq,
              method = method,
              scheme = scheme,
              host = host,
              uri = uri,
              status = status,
              remoteAddress = remoteAddress,
              durationMilli = durationMilli,
              userAgent = userAgent,
              dateInserted = dateInserted,
            )
        }.*)

        Ok(Map(
          "array" -> rows.asJson,
          "page" -> page.asJson,
          "pageSize" -> pageSize.asJson,
          "count" -> count.asJson,
        ).asJson)
      }
    }
  }

  def adminPageMetaList(seq: Long, page: Int, pageSize: Int, search: String, sortBy: String, sortOrder: String): Action[AnyContent] = Action { implicit request =>
    if (!isSiteAdmin(seq)) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(site) =>
          database.withConnection { implicit connection =>
            implicit val implicitSite: Site = site
            val normalizedPage = page.max(1)
            val normalizedPageSize = pageSize.max(1).min(200)
            val rows = models.tables.PageMeta.selectPagedForAdmin(
              page = normalizedPage,
              pageSize = normalizedPageSize,
              search = search,
              sortBy = sortBy,
              sortOrder = sortOrder,
            )
            val count = models.tables.PageMeta.countPagedForAdmin(search)
            Ok(Map(
              "array" -> rows.asJson,
              "page" -> normalizedPage.asJson,
              "pageSize" -> normalizedPageSize.asJson,
              "count" -> count.asJson,
            ).asJson)
          }
      }
    }
  }

  def adminSitePageNames(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isSiteAdmin(seq)) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(site) =>
          implicit val tupleDatabaseSite: (Database, Site) = (database, site)
          val pageNames = ahaWikiCache.PageMeta.SeqPageLatestSummary
            .get()
            .map(_.name)
            .distinct
            .sorted
          Ok(pageNames.asJson)
      }
    }
  }

  def adminSiteCalculate(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (!isSiteAdmin(seq)) {
      Forbidden("Access denied.")
    } else {
      SiteLogic.get(seq)(database) match {
        case None => NotFound(Map("error" -> s"site not found: $seq").asJson.toString()).as(JSON)
        case Some(site) =>
          implicit val tupleDatabaseSite: (Database, Site) = (database, site)
          val pageNames = ahaWikiCache.PageMeta.SeqPageLatestSummary
            .get()
            .map(_.name)
            .distinct
          val requestedPageName = request.getQueryString("pageName").map(_.trim).getOrElse("")
          val mode = request.getQueryString("mode").map(_.trim).filter(_.nonEmpty).getOrElse("default")
          val force = request.getQueryString("force").exists(_.trim.equalsIgnoreCase("true")) || mode == "force"
          val maybePageName = if (requestedPageName.nonEmpty) {
            pageNames.find(_ == requestedPageName)
          } else if (mode == "missingPageMeta") {
            database.withConnection { implicit connection =>
              implicit val implicitSite: Site = site
              models.tables.PageMeta.selectMissingPageNames(limit = 100)
                .headOption
            }
          } else {
            pageNames match {
              case Seq() => None
              case seqPageNames => Some(Random.shuffle(seqPageNames).head)
            }
          }

          maybePageName match {
            case Some(pageName) =>
              wikiActors.pageCalculation ! actors.ActorPageCalculator.Calculate(site, pageName)
              Ok(Map(
                "status" -> "queued",
                "siteSeq" -> site.seq.toString,
                "pageName" -> pageName,
                "source" -> (if (force) "forced" else if (requestedPageName.nonEmpty) "selected" else if (mode == "missingPageMeta") "missingPageMeta" else "random"),
                "mode" -> mode,
                "force" -> force.toString,
              ).asJson)
            case None =>
              if (requestedPageName.nonEmpty) {
                BadRequest(Map("error" -> s"page not found in site cache: $requestedPageName").asJson.toString()).as(JSON)
              } else {
                NotFound(Map("error" -> "No page exists in site cache.").asJson.toString()).as(JSON)
              }
          }
      }
    }
  }

  def me: Action[AnyContent] = Action { implicit request =>
    SessionLogic.getUser(request) match {
      case None =>
        Ok(Json.obj("loggedIn" -> Json.fromBoolean(false)))
      case Some(user) =>
        val profileImageUrl = SessionLogic.getUserProfileImageUrl(request).getOrElse("")
        val currentSite = SiteLogic.get(request.host)
        val siteAdminSeqs = database.withConnection { implicit connection =>
          SiteAdmin.selectByUser(user.seq).map(_.site)
        }
        Ok(Json.obj(
          "loggedIn"        -> Json.fromBoolean(true),
          "seq"             -> Json.fromLong(user.seq),
          "nickname"        -> Json.fromString(user.nickname),
          "loginEmail"      -> user.loginEmail.fold(Json.Null)(Json.fromString),
          "profileImageUrl" -> Json.fromString(profileImageUrl),
          "isAdmin"         -> Json.fromBoolean(logics.AdminLogic.isAdmin(request)),
          "siteAdminSeqs"   -> Json.fromValues(siteAdminSeqs.map(Json.fromLong)),
          "currentSiteSeq"  -> Json.fromLong(currentSite.seq),
        ))
    }
  }

  def csrf: Action[AnyContent] = Action { implicit request =>
    val token: Option[CSRF.Token] = CSRF.getToken
    Ok(token.asJson)
  }

  def pageRevision(pageName: String): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      val revision = Page.selectLastRevision(pageName).map(_.revision).getOrElse(0L)
      Results.Ok(PlayJson.obj("status" -> "ok", "pageName" -> pageName, "revision" -> revision)).as(JSON)
    }
  }

  def renderAhaMark(pageName: String): Action[AnyContent] = Action { implicit request =>
    request.body.asJson match {
      case Some(body) =>
        val comment = (body \ "comment").asOpt[String].getOrElse("")
        implicit val site: Site = SiteLogic.get(request.host)
        implicit val wikiContext: ContextWikiPage = ContextWikiPage(pageName)
        val html = InterpreterWiki.toHtmlString(comment)
        Results.Ok(PlayJson.obj("status" -> "ok", "html" -> html)).as(JSON)
      case None =>
        Results.BadRequest(PlayJson.obj("status" -> "error", "message" -> "JSON body is required")).as(JSON)
    }
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
    implicit val site: Site = SiteLogic.get(request.host)
    val weightedClassNames = database.withConnection { implicit connection =>
      models.tables.CalculatedSchemaOrg.selectClsCount().map(_.cls)
    }
    val allClassNames = logics.CalculatedSchemaOrg.mapClass.keys.toSeq.sorted
    val orderedClassNames = (weightedClassNames ++ allClassNames).distinct
    Ok(orderedClassNames.asJson)
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


  private def compactPreviewText(raw: String, maxLength: Int = 400): String = {
    val normalized = Option(raw).getOrElse("").replaceAll("""[\s\xA0]+""", " ").trim
    if (normalized.length <= maxLength) normalized else normalized.take(maxLength).trim + "..."
  }

  private def previewImageUrl(raw: String)(implicit request: RequestHeader, site: Site): String = {
    val trimmed = Option(raw).map(_.trim).getOrElse("")
    if (trimmed.isEmpty) {
      ""
    } else if (trimmed.startsWith("http://") || trimmed.startsWith("https://")) {
      trimmed
    } else if (trimmed.startsWith("//")) {
      s"${request.scheme}:$trimmed"
    } else if (trimmed.startsWith("attachment:")) {
      val objectKey = s"Attachment/${site.seq}/${trimmed.stripPrefix("attachment:")}"
      S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, objectKey).toOption.getOrElse("")
    } else if (trimmed.startsWith("/")) {
      s"${request.scheme}://${request.host}$trimmed"
    } else {
      s"${request.scheme}://${request.host}/$trimmed"
    }
  }

  def pagePreview(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper

      Page.selectLastRevision(name) match {
        case None =>
          NotFound(Json.obj(
            "success" -> Json.fromBoolean(false),
            "message" -> Json.fromString("Page not found"),
          ).toString()).as(JSON)

        case Some(page) =>
          val pageContent = PageContent(page.content)
          if (!WikiPermission().isReadable(name, Some(pageContent))) {
            Forbidden(Json.obj(
              "success" -> Json.fromBoolean(false),
              "message" -> Json.fromString("Permission denied"),
            ).toString()).as(JSON)
          } else {
            val renderedText = Try(Interpreters.toText(page.content)).getOrElse(pageContent.content)
            val description = pageContent.redirect
              .map(target => s"Redirect: $target")
              .getOrElse(compactPreviewText(renderedText))
            val image = models.tables.PageMeta.select(name).flatMap(_.image).map(image => previewImageUrl(image)).getOrElse("")

            Ok(Json.obj(
              "success" -> Json.fromBoolean(true),
              "title" -> Json.fromString(name),
              "image" -> Json.fromString(image),
              "description" -> Json.fromString(description),
              "revision" -> Json.fromLong(page.revision),
            ))
          }
      }
    }
  }


  private case class AdjacentLinkPayload(src: String, dst: String, alias: String, imageUrl: String, srcImageUrl: String, dstImageUrl: String)

  def links(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()

      def toAbsoluteImageUrl(raw: String): String = {
        if (raw.startsWith("http://") || raw.startsWith("https://")) raw
        else if (raw.startsWith("attachment:")) {
          val objectKey = s"Attachment/${site.seq}/${raw.stripPrefix("attachment:")}"
          S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, objectKey).toOption.getOrElse("")
        }
        else s"https://${request.host}/$raw"
      }

      val links = ahaWikiCacheMemoryApiLinks.getOrElseUpdate(site.seq, name) {
        Adjacent.getSeqLinkFiltered(name)
      }.filter(_.and(contextSite.pageCanSee))

      val pageNamesForImages = links.flatMap(link => Seq(link.src, link.dst)).distinct
      val imageUrlByPageName = models.tables.PageMeta.selectImageMap(pageNamesForImages)
        .view
        .mapValues(toAbsoluteImageUrl)
        .toMap

      val linksWithImage = links.map { link =>
        val adjacentName = if (link.src == name) link.dst else link.src
        AdjacentLinkPayload(
          src = link.src,
          dst = link.dst,
          alias = link.alias,
          imageUrl = imageUrlByPageName.getOrElse(adjacentName, ""),
          srcImageUrl = imageUrlByPageName.getOrElse(link.src, ""),
          dstImageUrl = imageUrlByPageName.getOrElse(link.dst, ""),
        )
      }
      Ok(linksWithImage.asJson)
    }
  }

  def adminMemoryCacheStats(): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Unauthorized(Map("error" -> "forbidden").asJson.toString()).as(JSON)
    } else {
      val stats = readMemoryCacheSnapshots()
      val normalized = stats.toSeq.sortBy(_._1).map { case (_, payload) =>
        MemoryCacheStatsPayload(instancePort = payload.instancePort, stats = payload)
      }
      Ok(normalized.asJson)
    }
  }

  def adminS3Objects(prefix: String = "", maxKeys: Int = 500, recursive: Boolean = false): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      val safeMaxKeys = Math.min(1000, Math.max(1, maxKeys))
      val safePrefix = Option(prefix).map(_.trim).getOrElse("")
      try {
        val amazonS3 = buildAmazonS3Client()
        val bucket = applicationConf.AhaWiki.aws.s3.bucket()
        val requestBuilder = new ListObjectsV2Request()
          .withBucketName(bucket)
          .withMaxKeys(safeMaxKeys)
        if (!recursive) {
          requestBuilder.withDelimiter("/")
        }
        if (safePrefix.nonEmpty) {
          requestBuilder.withPrefix(safePrefix)
        }
        val results = collection.mutable.ArrayBuffer.empty[com.amazonaws.services.s3.model.ListObjectsV2Result]
        var remaining = safeMaxKeys
        var token: String = null
        do {
          requestBuilder.withContinuationToken(token)
          requestBuilder.withMaxKeys(Math.min(1000, Math.max(1, remaining)))
          val result = amazonS3.listObjectsV2(requestBuilder)
          results += result
          remaining -= Option(result.getObjectSummaries).map(_.size()).getOrElse(0)
          token = result.getNextContinuationToken
        } while (recursive && token != null && remaining > 0)

        val directories = results.flatMap(r => Option(r.getCommonPrefixes).map(_.toArray.toSeq).getOrElse(Seq.empty).map(_.toString)).distinct
        val files = results.flatMap(r => Option(r.getObjectSummaries).map(_.toArray.toSeq).getOrElse(Seq.empty)).map { raw =>
          val item = raw.asInstanceOf[com.amazonaws.services.s3.model.S3ObjectSummary]
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
          InternalServerError(Json.obj("error" -> Json.fromString("S3 조회에 실패했습니다.")).toString()).as(JSON)
      }
    }
  }

  def adminDeleteS3Objects: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      val keys = request.body.asJson
        .flatMap(json => (json \ "keys").asOpt[Seq[String]])
        .getOrElse(Seq.empty)
        .map(_.trim)
        .filter(_.nonEmpty)
        .distinct
      if (keys.isEmpty) {
        BadRequest(Json.obj("error" -> Json.fromString("keys is required")).toString()).as(JSON)
      } else {
        try {
          val amazonS3 = buildAmazonS3Client()
          val bucket = applicationConf.AhaWiki.aws.s3.bucket()
          val deleteRequest = new DeleteObjectsRequest(bucket).withKeys(keys: _*)
          amazonS3.deleteObjects(deleteRequest)
          Ok(Json.obj("ok" -> Json.fromBoolean(true), "deletedCount" -> Json.fromInt(keys.size)))
        } catch {
          case error: Throwable =>
            logger.error(s"adminDeleteS3Objects failed. keys=${keys.take(10).mkString(",")}", error)
            InternalServerError(Json.obj("error" -> Json.fromString("S3 삭제에 실패했습니다.")).toString()).as(JSON)
        }
      }
    }
  }

  def adminS3DownloadUrl(key: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      val objectKey = Option(key).map(_.trim).getOrElse("")
      if (objectKey.isEmpty) {
        BadRequest(Json.obj("error" -> Json.fromString("key is required")).toString()).as(JSON)
      } else {
        S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, objectKey) match {
          case Right(url) => Ok(Json.obj("url" -> Json.fromString(url), "key" -> Json.fromString(objectKey)))
          case Left(errorMessage) =>
            logger.error(s"adminS3DownloadUrl failed. key=$objectKey error=$errorMessage")
            InternalServerError(Json.obj("error" -> Json.fromString("다운로드 URL 생성 실패")).toString()).as(JSON)
        }
      }
    }
  }

  def statistics(): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("")

      val seqPage: Seq[PageLatestSummary] = contextWikiPage.seqPageByPermission
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
    ahaWikiCacheMemoryApiLinks.clear()
    AhaWikiCacheMemoryPermission.clear()
    SiteLogic.get(siteSeq) foreach { implicit site =>
      implicit val tupleDatabaseSite: (Database, Site) = (database, site)
      implicit val contextSite: ContextSite = ContextSite()
      ahaWikiCache.invalidateSiteCaches()
    }
    Ok("ok")
  }
}
