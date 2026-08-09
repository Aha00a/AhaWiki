package controllers

import org.apache.pekko.actor.ActorSystem
import anorm.SqlParser.{bool, get, int, long, str}
import anorm._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.IpAddressUtil
import com.aha00a.play.Implicits.RichRequest
import io.circe.Json
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryApiLinks
import logics.AhaWikiCacheMemoryPermission
import logics.AhaWikiCacheMemoryApiLinks.Snapshot
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
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
import models.tables.Permission
import models.tables.SiteAdmin
import models.tables.User
import models.tables.UserApiKey
import play.api.Configuration
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.libs.json.{Json => PlayJson}
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.filters.csrf.CSRF
import services.ApplicationLifecycleHook

import java.net.URLDecoder
import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Random
import scala.util.Try


/**
 * What the wiki itself calls: the current user, a CSRF token, page names and previews,
 * link graphs, the names the editor completes from, and instance-wide statistics.
 *
 * Administration used to live here too. It moved out by audience — [[ApiAdminSite]] changes
 * one site, [[ApiAdminReport]] reads across all of them, [[ApiAdminS3]] browses the bucket,
 * and [[ApiApiKey]] handles keys from both the account page and the admin screen. What is
 * left is what a wiki page or the editor calls while someone is reading or writing.
 *
 * Two admin endpoints stayed: the signed read URL is about a page rather than a site, and
 * the memory-cache snapshot is written by a scheduled job registered in this constructor,
 * so the reader and the writer of that snapshot key stay together.
 */
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
) extends BaseController with JsonResults with AdminAuth with Logging {
  private case class MemoryCacheStatsPayload(instancePort: String, stats: Snapshot)

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

  def adminGenerateSignedReadUrl: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
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
        JsonError(InternalServerError, "Signed URL secret is not configured.")
      } else if (name.isEmpty) {
        JsonError(BadRequest, "Query parameter 'name' is required.")
      } else if (action.isEmpty) {
        JsonError(BadRequest, "action must be one of: view, raw, history, diff")
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
        ))
      }
    }
  }

  def change(name: String, includeMinorEdit: Int, includeViaApi: Int, limit: Int): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)

      case class ChangeRow(name: String, revision: Long, dateTime: String, nickname: Option[String], profileImageUrl: Option[String], remoteAddressMasked: String, comment: String, commentInlineHtml: String, isMinorEdit: Boolean, viaApi: Boolean, userApiKeyName: Option[String])
      case class ChangeSourceRow(name: String, revision: Long, dateTime: String, nickname: Option[String], profileImageUrl: Option[String], remoteAddress: String, comment: String, isMinorEdit: Boolean, viaApi: Boolean, userApiKeyName: Option[String])

      implicit val provider: RequestWrapper = RequestWrapper()
      val wikiPermission = WikiPermission()
      val boundedLimit = limit.max(1).min(1000)
      val batchSize = boundedLimit.max(100).min(1000)

      def selectRows(offset: Int): List[ChangeSourceRow] = {
        SQL"""
          SELECT
            P.name,
            P.revision,
            DATE_FORMAT(P.dateTime, '%Y-%m-%d %H:%i:%s') AS date_time,
            U.nickname,
            U.profileImageUrl,
            P.remoteAddress,
            P.comment,
            P.isMinorEdit,
            P.viaApi,
            AK.name AS userApiKeyName
          FROM Page P
          LEFT JOIN User U ON U.seq = P.user
          LEFT JOIN UserApiKey AK ON AK.seq = P.userApiKey
          WHERE P.site = ${site.seq}
            AND (${includeMinorEdit == 1} OR P.isMinorEdit = false)
            AND (${includeViaApi == 1} OR P.viaApi = false)
          ORDER BY P.dateTime DESC, P.revision DESC, P.name ASC
          LIMIT $batchSize OFFSET $offset
        """.as((str("name") ~ long("revision") ~ str("date_time") ~ str("nickname").? ~ str("profileImageUrl").? ~ str("remoteAddress") ~ str("comment") ~ bool("isMinorEdit") ~ bool("viaApi") ~ str("userApiKeyName").?).map {
          case name ~ revision ~ dateTime ~ nickname ~ profileImageUrl ~ remoteAddress ~ comment ~ isMinorEdit ~ viaApi ~ userApiKeyName =>
            ChangeSourceRow(name, revision, dateTime, nickname, profileImageUrl, remoteAddress, comment, isMinorEdit, viaApi, userApiKeyName)
        }.*)
      }

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

      val readableRows = collectReadableRows(0, List.empty).map { row =>
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
          viaApi = row.viaApi,
          userApiKeyName = row.userApiKeyName,
        )
      }

      Ok(readableRows.asJson)
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
          "isAdmin"         -> Json.fromBoolean(isAdmin),
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
          // Neither the {"error": ...} nor the {"message": ...} envelope — this endpoint
          // has always answered {"success": false, ...} and its callers are unknown.
          JsonResult(NotFound, Json.obj(
            "success" -> Json.fromBoolean(false),
            "message" -> Json.fromString("Page not found"),
          ))

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
      JsonError(Unauthorized, "forbidden")
    } else {
      val stats = readMemoryCacheSnapshots()
      val normalized = stats.toSeq.sortBy(_._1).map { case (_, payload) =>
        MemoryCacheStatsPayload(instancePort = payload.instancePort, stats = payload)
      }
      Ok(normalized.asJson)
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
