package logics.wikis.interpreters

import actors.ActorAhaWiki.Geocode
import com.aha00a.colors.Color
import com.aha00a.colors.GradientPreset
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import logics.wikis.interpreters.InterpreterTable.convert
import logics.wikis.macros.MacroError
import models._
import models.tables.GeocodeCache
import models.tables.Link
import models.tables.Link.DstMaxMinCount
import models.tables.Site
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference

import java.io.StringReader
import java.net.URLEncoder

object InterpreterMap extends TraitInterpreter {


  case class Location(
                       name:String,
                       exists: Boolean,
                       address:String,
                       scoreRaw:String,
                       raw:Seq[String]
                     ) {


    val score: Double = scoreRaw.toDoubleOrZero

    val fillOpacity: Double = score / 10
    val color: Color = GradientPreset.ahaWikiMap.getColor(score / 10)

    val strokeColor: String = (color / 1.2).toHashString
    val fillColor: String = color.toHashString
    val scale:Double = score
    val urlMap:String = address.toOption.map(u => s"https://www.google.com/maps/search/${URLEncoder.encode(u, "utf-8")}").getOrElse("")
  }

  case class LocationLatLngDstMaxMinCount(location: Location, latLng: LatLng, dstMaxMinCount: DstMaxMinCount)

  def parse(pageContent: PageContent)(implicit wikiContext: ContextWikiPage): (Seq[String], Seq[Location]) = {
    val setPageName: Set[String] = wikiContext.setPageNameByPermission
    Using(new CsvListReader(new StringReader(pageContent.content), CsvPreference.TAB_PREFERENCE)) { listReader =>
      val rowColumnData: Seq[Seq[String]] = convert(listReader)
      val head: Seq[String] = rowColumnData.head
      val tail: Seq[Seq[String]] = rowColumnData.tail

      val indexName = head.indexOf("Name")
      val indexScore = head.indexOf("Score")
      val indexAddress = head.indexOf("Address")

      //noinspection ZeroIndexToHead
      val seqLocation: Seq[Location] = tail.map(row => {
        val name = row.getOrElse(indexName, "")
        Location(name, setPageName.contains(name), row.getOrElse(indexAddress, ""), row.getOrElse(indexScore, ""), row)
      })

      (head, seqLocation)
    }
  }

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    val geocodingKey = wikiContext.applicationConf.AhaWiki.google.credentials.api.Geocoding.key()
    val mapJavaScriptApiKey = wikiContext.applicationConf.AhaWiki.google.credentials.api.MapsJavaScriptAPI.key()
    if(geocodingKey.isNullOrEmpty || mapJavaScriptApiKey.isNullOrEmpty){
      return MacroError.toHtmlString(s"[[[#!Map Error - Please setup Geocoding and MapsJavaScriptAPI key in your application.conf]]]")
    }

    val (seqHeader, seqLocation) = parse(pageContent)

    wikiContext.database.withConnection { implicit connection =>
      implicit val site: Site = wikiContext.site

      val seqDstMaxMinCount: Seq[DstMaxMinCount] = Link.selectDstMaxMinCountWhereSrcIsDatePage(seqLocation.map(_.name))
      val mapDstMaxMinCount: Map[String, DstMaxMinCount] = seqDstMaxMinCount.map(_.dst).zip(seqDstMaxMinCount).toMap

      val seqGeocodeCache: Seq[GeocodeCache] = GeocodeCache.select(seqLocation.map(_.address))
      val mapAddressLatLng: Map[String, LatLng] = seqGeocodeCache.map(geocodeCache => (geocodeCache.address, geocodeCache.latLng)).toMap
      val seqLocationWithCalculatedField = seqLocation.map(location => {
        val latLng = mapAddressLatLng.get(location.address) match {
          case Some(latLng) => latLng
          case _ =>
            if (location.address.isNotNullOrEmpty)
              wikiContext.actorAhaWiki ! Geocode(location.address)

            LatLng.Empty
        }

        LocationLatLngDstMaxMinCount(location, latLng, mapDstMaxMinCount.getOrElse(location.name, null))
      })
      val query: Map[String, String] = "Name,Score,Tag,Category,Comment,Address".split(",")
        .map(q => (q, wikiContext.requestWrapper.getQueryString(q).getOrElse(""))).filter(_._2.isNotNullOrEmpty).toMap
      views.html.Wiki.map(
        mapJavaScriptApiKey,
        pageContent.argument.getOrElse(0, ""),
        pageContent.argument.getOrElse(1, ""),
        seqHeader,
        seqLocationWithCalculatedField,
        query
      ).toString()
    }
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    val (_, seqLocation) = parse(pageContent)
    val links = seqLocation.filter(l => wikiContext.setPageNameByPermission.contains(l.name)).map(l => Link(wikiContext.nameTop, l.name, ""))
    links
  }
}
