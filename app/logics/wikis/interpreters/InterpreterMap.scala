package logics.wikis.interpreters

import java.io.StringReader

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import com.aha00a.play.Implicits._
import logics.wikis.interpreters.InterpreterTable.convert
import logics.{AhaWikiCache, ApplicationConf}
import models.{LatLng, PageContent, WikiContext}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import play.api.Configuration
import play.api.mvc.Request

object InterpreterMap {
  case class Location(
                       name:String,
                       address:String,
                       score:Double,
                       rest:Seq[(String, String)]
                     )(implicit wikiContext: WikiContext) {
    val latLng: LatLng = AhaWikiCache.AddressToLatLng.get(address)(wikiContext.cacheApi, wikiContext.actorAhaWiki)
    val fillOpacity: Double = score / 10
    val strokeColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, 1)"
    val fillColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, ${score / 10})"
    val labelColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, 1)"
    val scale:Double = score
  }

  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    Using(new CsvListReader(new StringReader(pageContent.content), CsvPreference.TAB_PREFERENCE)) { listReader =>
      val rowColumnData: Seq[Seq[String]] = convert(listReader)
      val head: Seq[String] = rowColumnData.head
      val tail: Seq[Seq[String]] = rowColumnData.tail

      implicit val request: Request[Any] = wikiContext.request
      val seqFields: Seq[String] = Seq("Name", if(request.isLocalhost) "AddressForDev" else "Address", "Score")
      val seqFieldIndex: Seq[Int] = seqFields.map(head.indexOf)
      val seqIndexRest: Seq[Int] = head.zipWithIndex.filterNot(v => seqFields.contains(v._1)).map(_._2)

      //noinspection ZeroIndexToHead
      val locations: Seq[Location] = tail.map(row => {
        Location(
          row.getOrElse(seqFieldIndex(0), ""),
          row.getOrElse(seqFieldIndex(1), ""),
          row.getOrElse(seqFieldIndex(2), "").toOption.map(_.toDouble).getOrElse(0),
          seqIndexRest
            .map(v => (head.getOrElse(v, ""), row.getOrElse(v, "")))
            .filter(s => !s._1.isNullOrEmpty && !s._2.isNullOrEmpty)
        )
      })

      implicit val configuration: Configuration = wikiContext.configuration
      val mapJavaScriptApiKey = ApplicationConf().AhaWiki.google.credentials.api.MapsJavaScriptAPI.key()
      val latLng: LatLng = locations.map(_.latLng).find(!_.lat.isNaN).getOrElse(models.LatLng(37.549521, 126.9157683))
      val resultMap = views.html.Wiki.map(mapJavaScriptApiKey, pageContent.argument.getOrElse(0, ""), pageContent.argument.getOrElse(1, ""), latLng, locations).toString()
      val resultTable = InterpreterTable.interpret(PageContent("#!Table tsv 1 tablesorter\n" + pageContent.content))
      resultMap + resultTable
    }
  }
}
