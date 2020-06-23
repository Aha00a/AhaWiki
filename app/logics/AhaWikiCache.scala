package logics

import actors.ActorAhaWiki.Geocode
import akka.actor.ActorRef
import logics.wikis.interpreters.Interpreters
import models.{AhaWikiQuery, LatLng, PageWithoutContentWithSize, WikiContext}
import play.api.Logger
import play.api.cache.CacheApi
import play.api.db.Database

import scala.concurrent.duration._

object AhaWikiCache {
  trait CacheEntity {
    val key: String = getClass.getName
    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      Logger.info(s"Invalidate Cache: $key")
      wikiContext.cacheApi.remove(key)
    }
  }

  object PageList extends CacheEntity {
    def get()(implicit cacheApi: CacheApi, database:Database): List[PageWithoutContentWithSize] = cacheApi.getOrElse(key, 60.minutes) {
      database.withConnection { implicit connection =>
        AhaWikiQuery().pageSelectPageList()
      }
    }
  }

  object Header extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.cacheApi.getOrElse(key, 60.minutes) { wikiContext.database.withConnection { implicit connection =>
      Interpreters.toHtmlString(AhaWikiQuery().Page.selectLastRevision(".header").map(_.content).getOrElse(""))
    }}
  }

  object Footer extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.cacheApi.getOrElse(key, 60.minutes) { wikiContext.database.withConnection { implicit connection =>
      Interpreters.toHtmlString(AhaWikiQuery().Page.selectLastRevision(".footer").map(_.content).getOrElse(""))
    }}
  }

  object Config extends CacheEntity {
    def get()(implicit cacheApi: CacheApi, database:Database): String = cacheApi.getOrElse(key, 60.minutes) { database.withConnection { implicit connection =>
      AhaWikiQuery().Page.selectLastRevision(".config").map(_.content).getOrElse("")
    }}
  }


  object AddressToLatLng {
    def key(address: String): String = "Addr" + address
    def set(address: String, latLng: LatLng)(implicit cacheApi: CacheApi): Unit = cacheApi.set(key(address), latLng, 365 days)
    def get(address:String)(implicit cacheApi: CacheApi, actorAhaWiki: ActorRef, database: Database): LatLng = {
      cacheApi.get[LatLng](key(address)) match {
        case Some(latLng) => latLng
        case _ =>
          if (!(address == null) && !address.isEmpty) {
            database.withConnection { implicit connection =>
              AhaWikiQuery().GeocodeCache.select(address) match {
                case Some(geocodeCache) =>
                  val latLng = geocodeCache.latLng
                  AhaWikiCache.AddressToLatLng.set(address, latLng)
                  latLng
                case None =>
                  actorAhaWiki ! Geocode(address)
                  LatLng(Double.NaN, Double.NaN)
              }
            }
          } else {
            LatLng(Double.NaN, Double.NaN)
          }
      }
    }
  }

  object Distance {
    def key(src: String, dst: String): String = "Addr" + src + dst
    def set(src: String, dst: String, meters: Int)(implicit cacheApi: CacheApi): Unit = cacheApi.set(key(src, dst), meters, 365 days)
    def get(src: String, dst: String)(implicit cacheApi: CacheApi, actorAhaWiki: ActorRef, database: Database): Int = {
      cacheApi.get[Int](key(src, dst)) match {
        case Some(meters) => meters
        case _ =>
          database.withConnection { implicit connection =>
            AhaWikiQuery().DistanceCache.select(src, dst) match {
              case Some(distance) =>
                val meters = distance.meters
                AhaWikiCache.Distance.set(src, dst, meters)
                meters
              case None =>
                import actors.ActorAhaWiki
                actorAhaWiki ! ActorAhaWiki.Distance(src, dst)
                0
            }
          }
      }
    }
  }
}
