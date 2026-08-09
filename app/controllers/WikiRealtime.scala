package controllers

import org.apache.pekko.actor._
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.scaladsl.{Flow, Sink, Source}
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Keep
import com.aha00a.play.Implicits._
import logics._
import logics.wikis.PageNameUrl
import logics.wikis.WikiPermission
import models.RequestWrapper
import models._
import models.tables.Page
import models.tables.Site
import models.tables.User
import play.api.Logging
import play.api.db.Database
import play.api.libs.json.Json
import play.api.mvc._

import java.util.UUID
import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

/**
 * The page WebSocket: who else is on this page, where their cursor is, and when the page
 * was saved under them.
 *
 * The subscriber registry is [[logics.PageCursorHub]] rather than something private here,
 * because the save endpoint announces page updates into the same rooms. Two registries would
 * each hold half the watchers and half of them would miss every update.
 */
class WikiRealtime @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  materializer: Materializer,
  database: Database,
  wikiActors: WikiActors,
  ahaWikiCache: AhaWikiCache,
  applicationConf: ApplicationConf,
  executionContext: ExecutionContext,
) extends BaseController with Logging {

  def watch(nameEncoded: String): WebSocket = WebSocket.acceptOrResult[String, String] { request =>
    Future {
      val name = PageNameUrl.decode(nameEncoded)
      implicit val provider: RequestWrapper = RequestWrapper()(request)

      def latestRevisionAndContent(): (Long, Option[PageContent]) = {
        database.withConnection { implicit connection =>
          implicit val site: Site = SiteLogic.get(request.host)
          val latest = Page.selectLastRevision(name)
          (latest.map(_.revision).getOrElse(0L), latest.map(v => PageContent(v.content)))
        }
      }

      val siteForWs = database.withConnection { implicit connection =>
        SiteLogic.get(request.host)
      }
      val (_, pageLastRevisionContent) = latestRevisionAndContent()
      val isReadable = database.withConnection { implicit connection =>
        implicit val site: Site = siteForWs
        val ctxSite = ContextSite.empty()(database, wikiActors, applicationConf, ahaWikiCache, site)
        val permission = WikiPermission()(provider, connection, ctxSite)
        permission.isReadable(name, pageLastRevisionContent)
      }
      if (!isReadable) {
        logger.warn(s"WebSocket watch denied: host=${request.host}, name=$name, uri=${request.uri}, remote=${request.remoteAddress}")
        Left(Forbidden("Permission denied."))
      } else {
        val connectionId = UUID.randomUUID().toString
        val currentUser = SessionLogic.getUser(request)
        val nickname = currentUser.map(_.nickname).filter(_.nonEmpty).getOrElse("")
        val profileImageUrl = currentUser.flatMap { user =>
          database.withConnection { implicit connection =>
            User.selectBySeq(user.seq).flatMap(_.profileImageUrl).filter(_.nonEmpty)
          }
        }.getOrElse("")
        val source = Source.queue[String](32, OverflowStrategy.dropHead)
        val sink = Sink.foreach[String] { incoming =>
          val payload = Try(Json.parse(incoming).asOpt[play.api.libs.json.JsObject].getOrElse(Json.obj())).getOrElse(Json.obj())
          (payload \ "type").asOpt[String] match {
            case Some("cursor.move") =>
              val x = (payload \ "x").asOpt[play.api.libs.json.JsNumber].map(_.value.toDouble).getOrElse(0d).max(0d).min(1d)
              val y = (payload \ "y").asOpt[play.api.libs.json.JsNumber].map(_.value.toDouble).getOrElse(0d).max(0d).min(1d)
              val outgoing = Json.obj(
                "type" -> "cursor.move",
                "siteId" -> siteForWs.seq,
                "pageId" -> name,
                "senderId" -> connectionId,
                "x" -> x,
                "y" -> y,
                "ts" -> System.currentTimeMillis()
              ).toString()
              PageCursorHub.broadcast(PageCursorHub.roomKeyForPage(siteForWs.seq, name), connectionId, outgoing)

            case Some("cursor.hello") =>
              val saveSenderId = (payload \ "saveSenderId").asOpt[String].map(_.trim).filter(_.nonEmpty)
              PageCursorHub.setSaveSenderId(PageCursorHub.roomKeyForPage(siteForWs.seq, name), connectionId, saveSenderId)
              val hello = Json.obj("type" -> "cursor.hello", "senderId" -> connectionId, "nickname" -> nickname, "profileImageUrl" -> profileImageUrl).toString()
              PageCursorHub.broadcast(PageCursorHub.roomKeyForPage(siteForWs.seq, name), connectionId, hello)

            case Some("cursor.ping") => ()

            case _ =>
          }
        }
        val flow = Flow.fromSinkAndSourceCoupledMat(sink, source)(Keep.right).mapMaterializedValue { queue =>
          PageCursorHub.subscribe(PageCursorHub.roomKeyForPage(siteForWs.seq, name), connectionId, queue)
          val hello = Json.obj("type" -> "cursor.hello", "senderId" -> connectionId, "nickname" -> nickname, "profileImageUrl" -> profileImageUrl).toString()
          queue.offer(hello)
          PageCursorHub.broadcast(PageCursorHub.roomKeyForPage(siteForWs.seq, name), connectionId, hello)
          NotUsed
        }.watchTermination() { (_, done) =>
          done.onComplete(_ => PageCursorHub.unsubscribe(PageCursorHub.roomKeyForPage(siteForWs.seq, name), connectionId))(executionContext)
          NotUsed
        }
        Right(flow)
      }
    }
  }
}
