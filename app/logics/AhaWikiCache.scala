package logics

import actors.ActorAhaWiki.Geocode
import akka.actor.ActorRef
import logics.wikis.interpreters.Interpreters
import models.{AhaWikiQuery, LatLng, PageWithoutContentWithSize, WikiContext}
import play.api.Logger
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database

import scala.concurrent.duration._

object AhaWikiCache extends Logging {
  trait CacheEntity {
    val key: String = getClass.getName
    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      logger.info(s"Invalidate Cache: $key")
      wikiContext.syncCacheApi.remove(key)
    }
  }

  object PageList extends CacheEntity {
    def get()(implicit syncCacheApi: SyncCacheApi, database:Database): List[PageWithoutContentWithSize] = syncCacheApi.getOrElseUpdate(key, 60.minutes) {
      database.withConnection { implicit connection =>
        AhaWikiQuery().pageSelectPageList()
      }
    }
  }

  object Header extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.syncCacheApi.getOrElseUpdate(key, 60.minutes) { wikiContext.database.withConnection { implicit connection =>
      Interpreters.toHtmlString(AhaWikiQuery().Page.selectLastRevision(".header").map(_.content).getOrElse(""))
    }}
  }

  object Footer extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.syncCacheApi.getOrElseUpdate(key, 60.minutes) { wikiContext.database.withConnection { implicit connection =>
      Interpreters.toHtmlString(AhaWikiQuery().Page.selectLastRevision(".footer").map(_.content).getOrElse(""))
    }}
  }

  object Config extends CacheEntity {
    def get()(implicit syncCacheApi: SyncCacheApi, database:Database): String = syncCacheApi.getOrElseUpdate(key, 60.minutes) { database.withConnection { implicit connection =>
      AhaWikiQuery().Page.selectLastRevision(".config").map(_.content).getOrElse("")
    }}
  }


  object AddressToLatLng {
    def key(address: String): String = "Addr" + address
    def set(address: String, latLng: LatLng)(implicit syncCacheApi: SyncCacheApi): Unit = syncCacheApi.set(key(address), latLng, 365 days)
    def get(address:String)(implicit syncCacheApi: SyncCacheApi, actorAhaWiki: ActorRef, database: Database): LatLng = {
      syncCacheApi.get[LatLng](key(address)) match {
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
    def set(src: String, dst: String, meters: Int)(implicit syncCacheApi: SyncCacheApi): Unit = syncCacheApi.set(key(src, dst), meters, 365 days)
    def get(src: String, dst: String)(implicit syncCacheApi: SyncCacheApi, actorAhaWiki: ActorRef, database: Database): Int = {
      syncCacheApi.get[Int](key(src, dst)) match {
        case Some(meters) => meters
        case _ =>
          database.withConnection { implicit connection =>
            import models.tables.DistanceCache
            DistanceCache.select(src, dst) match {
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
