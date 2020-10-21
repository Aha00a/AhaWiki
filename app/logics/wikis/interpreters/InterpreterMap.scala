package logics.wikis.interpreters

import java.io.StringReader
import java.net.URLEncoder

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import logics.wikis.interpreters.InterpreterTable.convert
import logics.wikis.macros.MacroError
import logics.AhaWikiCache
import logics.ApplicationConf
import models._
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import play.api.Configuration
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc.Request

object InterpreterMap extends TraitInterpreter {

  import models.tables.Link

  val originString: String = "Origin"
  case class Location(
                       name:String,
                       exists: Boolean,
                       address:String,
                       scoreRaw:String,
                       raw:Seq[String]
                     )(implicit wikiContext: WikiContext) {

    import com.aha00a.colors.Color
    import com.aha00a.colors.GradientPreset
    
    val isOrigin: Boolean = scoreRaw == originString

    val score: Double = if(isOrigin) 10 else scoreRaw.toDoubleOrZero

    val fillOpacity: Double = score / 10
    val color: Color = GradientPreset.ahaWikiMap.getColor(score / 10)

    val strokeColor: String = (color / 1.2).toHashString
    val fillColor: String = color.toHashString
    val labelColor: String = color.toHashString
    val scale:Double = score
    val urlMap:String = address.toOption.map(u => s"https://www.google.com/maps/search/${URLEncoder.encode(u, "utf-8")}").getOrElse("")
  }

  case class LocationSeqVisited(location: Location, latLng: LatLng, seqVisited: Seq[String])

  def parse(pageContent: PageContent)(implicit wikiContext: WikiContext): (Seq[String], Seq[Location]) = {
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

      (head, locations)
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

    implicit val request: Request[Any] = wikiContext.provider.request
    implicit val syncCacheApi: SyncCacheApi = wikiContext.syncCacheApi
    implicit val database: Database = wikiContext.database
    val (seqHeader, locations) = parse(pageContent)
    wikiContext.database.withConnection { implicit connection =>
      import models.tables.GeocodeCache
      val seqLink = Link.selectBacklinkOfDatePage(locations.map(_.name))
      val mapDstLink = seqLink.groupBy(_.dst)

      val seqGeocodeCache = GeocodeCache.select(locations.map(_.address))
      val mapAddressLatLng: Map[String, LatLng] = seqGeocodeCache.map(g => (g.address, g.latLng)).toMap
      val seqLocationLastVisited = locations.map(l => {
        val seqVisited: Seq[String] = mapDstLink.getOrElse(l.name, Seq()).map(_.src)
        val latLng = mapAddressLatLng.get(l.address) match {
          case Some(ll) => ll
          case _ =>
            import actors.ActorAhaWiki.Geocode
            wikiContext.actorAhaWiki ! Geocode(l.address)
            LatLng.Empty
        }
        LocationSeqVisited(l, latLng, seqVisited)
      })
      val query: Map[String, String] = "Name,Score,Tag,Category,Comment,Address".split(",").map(q => (q, wikiContext.provider.getQueryString(q).getOrElse(""))).filter(_._2.isNotNullOrEmpty).toMap
      views.html.Wiki.map(
        mapJavaScriptApiKey,
        pageContent.argument.getOrElse(0, ""),
        pageContent.argument.getOrElse(1, ""),
        seqHeader,
        seqLocationLastVisited,
        query
      ).toString()
    }
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    // TODO: implement
    Seq()
  }
}
