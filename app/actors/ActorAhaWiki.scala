package actors

import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import javax.inject.Inject
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.wikis.interpreters.Interpreters
import models.LatLng
import models.WikiContext
import models.tables.Page
import play.api.Configuration
import play.api.Environment
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.ws.WSClient

import scala.concurrent.ExecutionContext

object ActorAhaWiki {
  def props: Props = Props[ActorAhaWiki]

  case class Calculate(name: String, i: Int = 1, length: Int = 1)

  case class CalculateCosineSimilarity(name: String, i: Int = 1, length: Int = 1)

  case class CalculateLink(name: String, i: Int = 1, length: Int = 1)

  case class Geocode(address: String)

  case class Distance(src: String, dst: String)

}

class ActorAhaWiki @Inject()(implicit
                             syncCacheApi: SyncCacheApi,
                             database: Database,
                             wsClient: WSClient,
                             executionContext: ExecutionContext,
                             configuration: Configuration,
                             environment: Environment
                            ) extends Actor with Logging {

  import ActorAhaWiki._
  import models.WikiContext.Provider
  val provider: Provider = Provider.empty
  val seqStopWord: Seq[String] =
    """at in on of by to is the
      |gmail com http https
      |""".stripMargin.split("""\s""").toSeq

  def receive: PartialFunction[Any, Unit] = {
    case Calculate(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculate($i/$length)") {
      context.self ! CalculateCosineSimilarity(name, i, length)
      context.self ! CalculateLink(name, i, length)
    }

    case CalculateCosineSimilarity(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculateCosineSimilarity($i/$length)") {
      database.withConnection { implicit connection =>
        Page.selectLastRevision(name) foreach { page =>
          import logics.AhaWikiInjects
          import logics.wikis.RenderingMode

          implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()
          implicit val wikiContext: WikiContext = new WikiContext(Seq(page.name), RenderingMode.Normal)(ahaWikiInjects, provider)

          val text = Interpreters.toText(page.content)
          val seqWord = text
            .replaceAll("""([a-z])([A-Z])""", "$1 $2")
            .toLowerCase()
            .split("""\s""").toSeq
            .flatMap(_.split("""[/@.]""").toSeq)
            .map(s => s.replaceAll("""[{\}\[\]/?.,;:|)*~`!^\-_+<>@#$%&\\=('"]""", ""))
            .flatMap(s => s.replaceAll("""^(\d{8})t(\d{6})$""", "$1").split(" ").toSeq)
            .filterNot(s => s.length < 2)
            .filterNot(s => s.length > 15)
          val seqWordFiltered = seqWord.filter(w => !seqStopWord.contains(w))
          val wordCount = seqWordFiltered.groupByCount()
          logger.info(text)
          logger.info(seqWordFiltered.mkString(" "))
          logger.info(wordCount.toList.sortBy(-_._2).mkString(" "))

          Page.updateSimilarPage(name, wordCount)
        }
      }
    }
    case CalculateLink(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculateLink($i/$length)") {
      database.withConnection { implicit connection =>
        Page.selectLastRevision(name) foreach { page =>
          import logics.AhaWikiInjects
          import logics.wikis.RenderingMode
          import models.tables.Link
          import models.tables.SchemaOrg
          implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()
          implicit val wikiContext: WikiContext = new WikiContext(Seq(page.name), RenderingMode.Normal)(ahaWikiInjects, provider)
          val seqLink = Interpreters.toSeqLink(page.content).filterNot(_.isDstExternal) ++ Seq(Link(page.name, "", ""))
          Page.updateLink(page.name, seqLink)

          val seqSchemaOrg: Seq[SchemaOrg] = Interpreters.toSeqSchemaOrg(page.content)
          Page.updateSchemaOrg(name, seqSchemaOrg)
        }
      }
    }
    case Geocode(address) => StopWatch(s"Query Google Geocode - $address") {
      implicit val latLngReads: Reads[LatLng] = Json.reads[LatLng]
      wsClient
        .url("https://maps.googleapis.com/maps/api/geocode/json")
        .withQueryStringParameters(
          "address" -> address,
          "key" -> ApplicationConf().AhaWiki.google.credentials.api.Geocoding.key()
        )
        .get()
        .map(r => {
          logger.info(s"$address - ${r.json}")
          (r.json \ "results" \ 0 \ "geometry" \ "location").as[LatLng]
        })
        .map(latLng => {
          database.withTransaction { implicit connection =>
            import models.tables.GeocodeCache
            GeocodeCache.replace(address, latLng)
          }
        })
    }
    case Distance(src, dst) => StopWatch(s"Query Google Distance Matrix Api - $src - $dst") {
      // TODO: remove
      wsClient
        .url("https://maps.googleapis.com/maps/api/distancematrix/json")
        .withQueryStringParameters(
          "mode" -> "transit",
          "origins" -> src,
          "destinations" -> dst,
          "key" -> ApplicationConf().AhaWiki.google.credentials.api.Geocoding.key()
        )
        .get()
        .map(r => {
          logger.info(s"$src - $dst - ${r.json}")
          (
            (r.json \ "rows" \ 0 \ "elements" \ 0 \ "distance" \ "value").as[Int],
            (r.json \ "rows" \ 0 \ "elements" \ 0 \ "duration" \ "value").as[Int]
          )
        })
        .map(metersSeconds => {
          logger.info(metersSeconds.toString())
        })
    }
    case _ =>
      logger.error("Unknown")
  }

}

