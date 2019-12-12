package logics

import actors.ActorAhaWiki.Geocode
import akka.actor.ActorRef
import logics.wikis.Interpreters
import models.{AhaWikiDatabase, LatLng, WikiContext}
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
    def get()(implicit cacheApi: CacheApi, db:Database): List[AhaWikiDatabase.PageNameRevisionTimeAuthorRemoteAddressSizeComment] = cacheApi.getOrElse(key, 60.minutes) {
      AhaWikiDatabase().pageSelectPageList()
    }

    override def invalidate()(implicit wikiContext: WikiContext): Unit = {
      super.invalidate()
      PageNameSet.invalidate()
    }
  }

  object PageNameSet extends CacheEntity {
    def get()(implicit cacheApi: CacheApi, db:Database): Set[String] = cacheApi.getOrElse(key, 60.minutes) {
      PageList.get().map(_.name).toSet
    }
  }

  object Header extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Interpreters.interpret(AhaWikiDatabase()(wikiContext.database).pageSelectLastRevision(".header").map(_.content).getOrElse(""))
    }
  }

  object Footer extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Interpreters.interpret(AhaWikiDatabase()(wikiContext.database).pageSelectLastRevision(".footer").map(_.content).getOrElse(""))
    }
  }

  object Config extends CacheEntity {
    def get()(implicit cacheApi: CacheApi, db:Database): String = cacheApi.getOrElse(key, 60.minutes) {
      AhaWikiDatabase().pageSelectLastRevision(".config").map(_.content).getOrElse("")
    }
  }


  object AddressToLatLng {
    def key(address: String): String = "Addr" + address
    def set(address: String, latLng: LatLng)(implicit cacheApi: CacheApi): Unit = cacheApi.set(key(address), latLng, 365 days)
    def get(address:String)(implicit cacheApi: CacheApi, actorAhaWiki: ActorRef, database: Database): LatLng = {
      cacheApi.get[LatLng](key(address)) match {
        case Some(latLng) => latLng
        case _ =>
          if (!(address == null) && !address.isEmpty) {
            AhaWikiDatabase().GeocodeCacheTable.select(address) match {
              case Some(geocodeCache) =>
                val latLng = geocodeCache.latLng
                AhaWikiCache.AddressToLatLng.set(address, latLng)
                latLng
              case None =>
                actorAhaWiki ! Geocode(address)
                LatLng(Double.NaN, Double.NaN)
            }
          } else {
            LatLng(Double.NaN, Double.NaN)
          }
      }
    }
  }
}
