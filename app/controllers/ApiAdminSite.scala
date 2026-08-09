package controllers

import org.apache.pekko.actor.ActorSystem
import anorm.SqlParser.{get, long, str}
import anorm._
import com.amazonaws.services.s3.model.ObjectMetadata
import com.aha00a.commons.Implicits._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryDomainSite
import logics.AhaWikiCacheMemoryPermission
import logics.ApplicationConf
import logics.AttachmentLogic
import logics.PermissionLogic
import logics.S3Logic
import logics.SiteLogic
import logics.SiteThemeLogic
import logics.wikis.macros.S3AttachmentUrlLogic
import models.WikiActors
import models.tables.Config
import models.tables.Permission
import models.tables.Site
import models.tables.SiteAdmin
import play.api.Logging
import play.api.db.Database
import play.api.libs.Files.TemporaryFile
import play.api.mvc._

import java.nio.file.Files
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.inject._
import scala.util.Random

/**
 * Everything that changes one site: its record, its permissions and admins, its appearance,
 * and the caches derived from its pages.
 *
 * Every endpoint here is scoped to a site seq and goes through withSiteAdmin or
 * withAdminSite, which decide permission before looking the site up. That ordering is the
 * reason they live in one place — see the note on withSiteAdmin.
 */
class ApiAdminSite @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
) extends BaseController with JsonResults with AdminAuth with Logging {

  private def siteNotFound(seq: Long): Result = JsonError(NotFound, s"site not found: $seq")

  /**
   * Checks site admin permission, loads the site, and hands it to the body.
   *
   * Repeating the rejection and the site 404 at every endpoint means a later change can fix
   * one of them and still compile. The order and both responses live here instead. The
   * permission check runs first, so an unknown site reads as 403 to a stranger rather than
   * revealing whether it exists.
   */
  private def withSiteAdmin(seq: Long)(block: Site => Result)(implicit request: RequestHeader): Result =
    if (!isSiteAdmin(seq)) AccessDenied
    else SiteLogic.get(seq)(database).fold(siteNotFound(seq))(block)

  /** Same as [[withSiteAdmin]], but only a global admin passes, not a site admin. */
  private def withAdminSite(seq: Long)(block: Site => Result)(implicit request: RequestHeader): Result =
    if (!isAdmin) AccessDenied
    else SiteLogic.get(seq)(database).fold(siteNotFound(seq))(block)

  private val adminFaviconConfigKey: String = "site.favicon.objectKey"

  private val adminFaviconTimestampFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH-mm-ss")

  private def buildAdminFaviconObjectKey(siteSeq: Long, originalFileName: String, now: LocalDateTime = LocalDateTime.now()): String = {
    val extension = originalFileName
      .split('.')
      .lastOption
      .map(_.replaceAll("[^a-zA-Z0-9]", "").toLowerCase)
      .filter(_.nonEmpty)
      .getOrElse("png")
    val sanitizedFilename = AttachmentLogic.sanitizePathSegment(originalFileName)
    val formattedDateTime = now.format(adminFaviconTimestampFormatter)
    s"Favicon/$siteSeq/${sanitizedFilename}.$formattedDateTime.$extension"
  }

  private def parseSiteSeq(v: String): Option[Long] = scala.util.Try(v.trim.toLong).toOption.filter(_ > 0)

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
      .toRight(JsonError(BadRequest, "Valid siteSeq is required."))
  }

  private def resolveAdminTargetSiteWithAuth(siteSeqValue: Option[String])(implicit request: RequestHeader): Either[Result, Site] = {
    resolveAdminTargetSite(siteSeqValue).flatMap { site =>
      if (isSiteAdmin(site.seq)) Right(site)
      else Left(AccessDenied)
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
      AccessDenied
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
      AccessDenied
    } else {
      val form = request.body.asFormUrlEncoded.getOrElse(Map.empty)
      val abbr = form.get("abbr").flatMap(_.headOption).map(_.trim).getOrElse("")
      val mainDomain = form.get("mainDomain").flatMap(_.headOption).map(_.trim).getOrElse("")
      if (abbr.isEmpty) {
        JsonError(BadRequest, "abbr is required")
      } else {
        parsePublicListedOrder(form.get("publicListedOrder").flatMap(_.headOption)) match {
          case Left(errorMessage) =>
            JsonError(BadRequest, errorMessage)
          case Right(publicListedOrder) =>
            database.withConnection { implicit connection =>
              try {
                val updated = Site.updateAbbrAndMainDomain(seq, abbr, mainDomain, publicListedOrder)
                if (updated == 0) {
                  siteNotFound(seq)
                } else {
                  AhaWikiCacheMemoryDomainSite.invalidate()
                  SiteLogic.get(seq)(database) match {
                    case Some(site) => Ok(site.asJson)
                    case None => JsonError(NotFound, s"site not found after update: $seq")
                  }
                }
              } catch {
                case e: java.sql.SQLIntegrityConstraintViolationException =>
                  JsonError(BadRequest, Option(e.getMessage).getOrElse("site update violates constraints"))
              }
            }
        }
      }
    }
  }

  def adminPermissions(seq: Long): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
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

  def adminUpsertPermission(seq: Long): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
      val form = request.body.asFormUrlEncoded.getOrElse(Map.empty)
      parsePermissionPayload(form) match {
        case Left(error) => JsonError(BadRequest, error)
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

  def adminDeletePermission(seq: Long): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
      parsePermissionKey(request.getQueryString) match {
        case Left(error) => JsonError(BadRequest, error)
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

  def adminSiteAdmins(seq: Long): Action[AnyContent] = Action { implicit request =>
    withAdminSite(seq) { _ =>
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

  def adminInsertSiteAdmin(seq: Long): Action[AnyContent] = Action { implicit request =>
    withAdminSite(seq) { _ =>
      val form = request.body.asFormUrlEncoded.getOrElse(Map.empty)
      form.get("user").flatMap(_.headOption).flatMap(s => s.toLongOption) match {
        case None => JsonError(BadRequest, "user is required")
        case Some(userSeq) =>
          database.withConnection { implicit connection =>
            SiteAdmin.insert(seq, userSeq)
            logics.AhaWikiCacheMemorySiteAdmin.invalidate(seq)
            Ok(Json.obj("ok" -> Json.fromBoolean(true), "site" -> Json.fromLong(seq), "user" -> Json.fromLong(userSeq)))
          }
      }
    }
  }

  def adminDeleteSiteAdmin(seq: Long, userSeq: Long): Action[AnyContent] = Action { implicit request =>
    withAdminSite(seq) { _ =>
      database.withConnection { implicit connection =>
        val deletedCount = SiteAdmin.delete(seq, userSeq)
        logics.AhaWikiCacheMemorySiteAdmin.invalidate(seq)
        Ok(Json.obj("ok" -> Json.fromBoolean(true), "deletedCount" -> Json.fromInt(deletedCount)))
      }
    }
  }

  def adminPermissionDiagnose(seq: Long, pageName: String, actor: String, action: String): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
      Permission.parseAction(action) match {
        case Left(error) => JsonError(BadRequest, error)
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

  def adminPermissionAudit: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
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
        JsonError(BadRequest, "file is required")
      case Some(filePart) =>
        val contentType = filePart.contentType.getOrElse("application/octet-stream")
        if (!contentType.startsWith("image/")) {
          JsonError(BadRequest, "Only image files are allowed.")
        } else {
          database.withConnection { implicit connection =>
            val siteSeqValue = request.body.dataParts.get("siteSeq").flatMap(_.headOption)
            resolveAdminTargetSiteWithAuth(siteSeqValue) match {
                case Left(errorResult) => errorResult
                case Right(siteValue) =>
                  implicit val site: Site = siteValue
                  val objectKey = buildAdminFaviconObjectKey(site.seq, filePart.filename.trim)
                  val amazonS3 = S3Logic.client(applicationConf)
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
                      JsonError(InternalServerError, "Favicon upload failed.")
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
                val amazonS3 = S3Logic.client(applicationConf)
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
            val defaultHue = Config.select(SiteThemeLogic.DefaultHueKey).flatMap(c => SiteThemeLogic.parseHue(c.v)).map(_.toString).getOrElse("")
            Ok(Json.obj(
              "siteSeq" -> Json.fromLong(site.seq),
              "defaultHue" -> Json.fromString(defaultHue),
            ))
      }
    }
  }

  def adminSiteTelegram(seq: Long): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
      database.withConnection { implicit connection =>
        implicit val implicitSite: Site = site
        val chatId = Config.Query.Telegram.chatId().getOrElse("")
        Ok(Json.obj(
          "siteSeq" -> Json.fromLong(site.seq),
          "chatId"  -> Json.fromString(chatId),
        ))
      }
    }
  }

  def adminUpdateSiteTelegram(seq: Long): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
      val body   = request.body.asFormUrlEncoded.getOrElse(Map.empty)
      val chatId = body.get("chatId").flatMap(_.headOption).map(_.trim).getOrElse("")
      database.withConnection { implicit connection =>
        implicit val implicitSite: Site = site
        Config.Query.Telegram.saveChatId(chatId)
        Ok(Json.obj(
          "ok"      -> Json.fromBoolean(true),
          "siteSeq" -> Json.fromLong(site.seq),
          "chatId"  -> Json.fromString(chatId),
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
            val defaultHue = SiteThemeLogic.parseHue(body.get("defaultHue").flatMap(_.headOption).getOrElse("")).map(_.toString).getOrElse("")

            Seq(
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
              "defaultHue" -> Json.fromString(defaultHue),
            ))
      }
    }
  }

  def adminPageMetaList(seq: Long, page: Int, pageSize: Int, search: String, sortBy: String, sortOrder: String): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
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

  def adminSitePageNames(seq: Long): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
      implicit val tupleDatabaseSite: (Database, Site) = (database, site)
      val pageNames = ahaWikiCache.PageMeta.SeqPageLatestSummary
        .get()
        .map(_.name)
        .distinct
        .sorted
      Ok(pageNames.asJson)
    }
  }

  def adminSiteCalculate(seq: Long): Action[AnyContent] = Action { implicit request =>
    withSiteAdmin(seq) { site =>
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
            JsonError(BadRequest, s"page not found in site cache: $requestedPageName")
          } else {
            JsonError(NotFound, "No page exists in site cache.")
          }
      }
    }
  }
}
