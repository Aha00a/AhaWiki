package logics.wikis.interpreters

import java.io.StringReader
import java.net.URLEncoder

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import logics.wikis.PageLogic
import logics.wikis.interpreters.InterpreterTable.convert
import logics.wikis.macros.MacroError
import logics.{AhaWikiCache, ApplicationConf}
import models.{AhaWikiQuery, LatLng, PageContent, WikiContext}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

import scala.collection.mutable

object InterpreterMap {
  case class Location(
                       name:String,
                       exists: Boolean,
                       address:String,
                       score:Double,
                       rest:Seq[(String, String)]
                     )(implicit wikiContext: WikiContext) {
    val latLng: LatLng = AhaWikiCache.AddressToLatLng.get(address)(wikiContext.cacheApi, wikiContext.actorAhaWiki, wikiContext.database)
    val fillOpacity: Double = score / 10
    val strokeColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, 1)"
    val fillColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, ${score / 10})"
    val labelColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, 1)"
    val scale:Double = score
    val urlMap:String = address.toOption.map(u => s"https://www.google.com/maps/search/${URLEncoder.encode(u, "utf-8")}").getOrElse("")
  }

  case class LocationListVisited(location: Location, listVisited: List[String])

  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    implicit val configuration: Configuration = wikiContext.configuration
    val geocodingKey = ApplicationConf().AhaWiki.google.credentials.api.Geocoding.key()
    val mapJavaScriptApiKey = ApplicationConf().AhaWiki.google.credentials.api.MapsJavaScriptAPI.key()
    if(geocodingKey.isNullOrEmpty || mapJavaScriptApiKey.isNullOrEmpty){
      return MacroError(s"[[[#!Map Error - Please setup Geocoding and MapsJavaScriptAPI key in your application.conf]]]")
    }

    implicit val request: Request[Any] = wikiContext.request
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val database: Database = wikiContext.database
    val setPageName: Set[String] = PageLogic.getSetPageName()
    Using(new CsvListReader(new StringReader(pageContent.content), CsvPreference.TAB_PREFERENCE)) { listReader =>
      val rowColumnData: Seq[Seq[String]] = convert(listReader)
      val head: Seq[String] = rowColumnData.head
      val tail: Seq[Seq[String]] = rowColumnData.tail

      val seqHeaderName: Seq[String] = Seq("Name", "Address", "Score")
      val seqHeaderIndex: Seq[Int] = seqHeaderName.map(head.indexOf)
      val seqIndexRest: Seq[Int] = head.zipWithIndex.filterNot(v => seqHeaderName.contains(v._1)).map(_._2)
      val seqHeaderRest: Seq[String] = seqIndexRest.map(i => head.getOrElse(i, ""))

      //noinspection ZeroIndexToHead
      val locations: Seq[Location] = tail.map(row => {
        val name = row.getOrElse(seqHeaderIndex(0), "")
        Location(
          name,
          setPageName.contains(name),
          row.getOrElse(seqHeaderIndex(1), ""),
          row.getOrElse(seqHeaderIndex(2), "").toOption.map(_.toDouble).getOrElse(0),
          seqIndexRest
            .map(v => (head.getOrElse(v, "").getOrElse(""), row.getOrElse(v, "").getOrElse("")))
        )
      })
      wikiContext.database.withConnection { implicit connection =>
        val seqLocationLastVisited = locations.map(l => {
          val listDates = AhaWikiQuery().Link.selectBacklinkOfDatePage(l.name).map(_.src).sorted(Ordering[String].reverse)
          LocationListVisited(l, listDates)
        })

        val seqAddressLocation: Seq[(Seq[String], LocationListVisited)] = seqLocationLastVisited
          .filterNot(_.location.address.isNullOrEmpty)
          .map(l => (l.location.address.split(" ").toSeq.padTo(3, ""), l))

        //noinspection ZeroIndexToHead
        val mapSeqTuple = seqAddressLocation.groupBy(_._1(0))
        val mapMapSeqTuple = mapSeqTuple.mapValues(_.groupBy(_._1(1)))
        val mapMapMapSeqTuple = mapMapSeqTuple.mapValues(_.mapValues(_.groupBy(_._1(2))))
        val mapMapMapSeq = mapMapMapSeqTuple.mapValues(_.mapValues(_.mapValues(_.map(_._2))))

        val buffer = mutable.Buffer[String]()
        for ((k1, v1) <- mapMapMapSeq.toList.sortBy(_._1)) {
          if (!k1.isNullOrEmpty) buffer += s"== $k1"
          for ((k2, v2) <- v1.toList.sortBy(_._1)) {
            if (!k2.isNullOrEmpty) buffer += s"=== $k2"
            for ((k3, v3) <- v2.toList.sortBy(_._1)) {
              if (!k3.isNullOrEmpty) buffer += s"==== $k3"
              for (v <- v3) {
                if (v.location.exists) {
                  buffer += s" * [${v.location.name}] - ${v.location.score}"
                } else {
                  buffer += s" * ${v.location.name} - ${v.location.score}"
                }
                buffer += s"  * ${v.location.address}"
                buffer ++= v.location.rest.filterNot(_._2.isNullOrEmpty).map(v => s"  * ${v._1}: ${v._2}")
                buffer += ""
              }
            }
          }
        }

        val htmlStringMap: String = views.html.Wiki.map(mapJavaScriptApiKey, pageContent.argument.getOrElse(0, ""), pageContent.argument.getOrElse(1, ""), seqLocationLastVisited, seqHeaderName, seqHeaderRest).toString()
        val htmlStringWiki: String = InterpreterWiki(buffer.mkString("\n", "\n", "\n"))
        htmlStringMap + htmlStringWiki
      }
    }
  }
}
