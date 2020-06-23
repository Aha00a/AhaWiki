package logics.wikis.interpreters

import java.io.StringReader
import java.net.URLEncoder

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import logics.wikis.interpreters.InterpreterTable.convert
import logics.wikis.macros.MacroError
import logics.{AhaWikiCache, ApplicationConf}
import models._
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

object InterpreterMap extends TraitInterpreter {
  val originString: String = "Origin"
  case class Location(
                       name:String,
                       exists: Boolean,
                       address:String,
                       scoreRaw:String,
                       raw:Seq[String]
                     )(implicit wikiContext: WikiContext) {
    val latLng: LatLng = AhaWikiCache.AddressToLatLng.get(address)(wikiContext.cacheApi, wikiContext.actorAhaWiki, wikiContext.database)
    val isOrigin: Boolean = scoreRaw == originString

    val score: Double = if(isOrigin) 10 else scoreRaw.toDoubleOrZero
    val fillOpacity: Double = score / 10
    val strokeColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, 1)"
    val fillColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, ${score / 10})"
    val labelColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, 1)"
    val scale:Double = score
    val urlMap:String = address.toOption.map(u => s"https://www.google.com/maps/search/${URLEncoder.encode(u, "utf-8")}").getOrElse("")
  }

  case class LocationListVisited(location: Location, listVisited: List[String])

  def parse(pageContent: PageContent)(implicit wikiContext: WikiContext): (Seq[String], Seq[Location], Map[String, Int]) = {
    val setPageName: Set[String] = wikiContext.setPageNameByPermission
    Using(new CsvListReader(new StringReader(pageContent.content), CsvPreference.TAB_PREFERENCE)) { listReader =>
      val rowColumnData: Seq[Seq[String]] = convert(listReader)
      val head: Seq[String] = rowColumnData.head
      val tail: Seq[Seq[String]] = rowColumnData.tail

      val indexName = head.indexOf("Name")
      val indexScore = head.indexOf("Score")
      val indexAddress = head.indexOf("Address")

      //noinspection ZeroIndexToHead
      val locations: Seq[Location] = tail.map(row => {
        val name = row.getOrElse(indexName, "")
        Location(
          name,
          setPageName.contains(name),
          row.getOrElse(indexAddress, ""),
          row.getOrElse(indexScore, ""),
          row
        )
      })

      val mapAddressMeters: Map[String, Int] = locations.find(_.isOrigin) match {
        case Some(locationOrigin) =>
          locations.filter(_.address != locationOrigin.address).map(l => {
            val meters = AhaWikiCache.Distance.get(locationOrigin.address, l.address)(wikiContext.cacheApi, wikiContext.actorAhaWiki, wikiContext.database)
            (l.address, meters)
          }).toMap
        case None =>
          Map()
      }

      (head, locations, mapAddressMeters)
    }
  }

  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    implicit val configuration: Configuration = wikiContext.configuration

    val pageContent: PageContent = PageContent(content)
    val geocodingKey = ApplicationConf().AhaWiki.google.credentials.api.Geocoding.key()
    val mapJavaScriptApiKey = ApplicationConf().AhaWiki.google.credentials.api.MapsJavaScriptAPI.key()
    if(geocodingKey.isNullOrEmpty || mapJavaScriptApiKey.isNullOrEmpty){
      return MacroError.toHtmlString(s"[[[#!Map Error - Please setup Geocoding and MapsJavaScriptAPI key in your application.conf]]]")
    }

    implicit val request: Request[Any] = wikiContext.request
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val database: Database = wikiContext.database
    val (seqHeader, locations, mapAddressMeters) = parse(pageContent)
    wikiContext.database.withConnection { implicit connection =>
      val seqLocationLastVisited = locations.map(l => {
        val listDates = AhaWikiQuery().Link.selectBacklinkOfDatePage(l.name).map(_.src).sorted(Ordering[String].reverse)
        LocationListVisited(l, listDates)
      })

      views.html.Wiki.map(
        mapJavaScriptApiKey,
        pageContent.argument.getOrElse(0, ""),
        pageContent.argument.getOrElse(1, ""),
        seqHeader,
        seqLocationLastVisited,
        mapAddressMeters
      ).toString()
    }
  }

  override def toSeqWord(content: String)(implicit wikiContext: WikiContext): Seq[String] = {
    val pageContent: PageContent = PageContent(content)
    val (seqHeader, locations, mapAddressMeters) = parse(pageContent)
    seqHeader ++ locations.flatMap(_.raw)
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    // TODO: implement
    Seq()
  }
}
