package logics.wikis.interpreters

import java.io.StringReader

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import com.aha00a.play.Implicits._
import com.aha00a.supercsv.SupercsvUtil
import logics.wikis.interpreters.InterpreterTable.convert
import logics.{AhaWikiCache, ApplicationConf}
import models.{AhaWikiDatabase, LatLng, PageContent, WikiContext}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import play.api.{Configuration, Logger}
import play.api.mvc.Request

object InterpreterMap {
  case class Location(
                       name:String,
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
  }

  case class LocationListVisited(location: Location, listVisited: List[String])

  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    Using(new CsvListReader(new StringReader(pageContent.content), CsvPreference.TAB_PREFERENCE)) { listReader =>
      val rowColumnData: Seq[Seq[String]] = convert(listReader)
      val head: Seq[String] = rowColumnData.head
      val tail: Seq[Seq[String]] = rowColumnData.tail

      implicit val request: Request[Any] = wikiContext.request
      val seqHeaderName: Seq[String] = Seq("Name", "Address", "Score")
      val seqHeaderIndex: Seq[Int] = seqHeaderName.map(head.indexOf)
      val seqIndexRest: Seq[Int] = head.zipWithIndex.filterNot(v => seqHeaderName.contains(v._1)).map(_._2)
      val seqHeaderRest: Seq[String] = seqIndexRest.map(i => head.getOrElse(i, ""));

      //noinspection ZeroIndexToHead
      val locations: Seq[Location] = tail.map(row => {
        Location(
          row.getOrElse(seqHeaderIndex(0), ""),
          row.getOrElse(seqHeaderIndex(1), ""),
          row.getOrElse(seqHeaderIndex(2), "").toOption.map(_.toDouble).getOrElse(0),
          seqIndexRest
            .map(v => (head.getOrElse(v, "").getOrElse(""), row.getOrElse(v, "").getOrElse("")))
        )
      })

      val ahaWikiDatabase = AhaWikiDatabase()(wikiContext.database)
      val seqLocationLastVisited = locations.map(l => {
        val listDates = ahaWikiDatabase.LinkTable.selectBacklink(l.name).map(_.src).sorted(Ordering[String].reverse)
        LocationListVisited(l, listDates)
      })

      implicit val configuration: Configuration = wikiContext.configuration
      val mapJavaScriptApiKey = ApplicationConf().AhaWiki.google.credentials.api.MapsJavaScriptAPI.key()
      views.html.Wiki.map(mapJavaScriptApiKey, pageContent.argument.getOrElse(0, ""), pageContent.argument.getOrElse(1, ""), seqLocationLastVisited, seqHeaderName, seqHeaderRest).toString()
    }
  }
}
