package logics.wikis.interpreters

import java.io.StringReader

import com.aha00a.commons.implicits.Implicits._
import com.aha00a.commons.utils.Using
import logics.wikis.interpreters.InterpreterTable.convert
import logics.{AhaWikiCache, ApplicationConf}
import models.{LatLng, PageContent, WikiContext}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import play.api.{Configuration, Play}

object InterpreterMap {
  case class Location(
                       name:String,
                       address:String,
                       score:Double,
                       rest:Seq[(String, String)]
                     )(implicit wikiContext: WikiContext) {
    val latLng: LatLng = AhaWikiCache.AddressToLatLng.get(address)(wikiContext.cacheApi, wikiContext.actorAhaWiki)
    val fillOpacity: Double = score / 10
    val strokeColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, ${score / 10})"
    val labelColor: String = s"hsla(${score * 360 / 10}, 100%, 50%, 1)"
    val scale:Double = score
  }

  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    Using(new CsvListReader(new StringReader(pageContent.content), CsvPreference.TAB_PREFERENCE)) { listReader =>
      val rowColumnData: Seq[Seq[String]] = convert(listReader)
      val head: Seq[String] = rowColumnData.head
      val tail: Seq[Seq[String]] = rowColumnData.tail

      val seqFields: Seq[String] = Seq("Name", "Address", "Score")
      val seqFieldIndex: Seq[Int] = seqFields.map(head.indexOf)
      val seqIndexRest: Seq[Int] = head.zipWithIndex.filterNot(v => seqFields.contains(v._1)).map(_._2)

      //noinspection ZeroIndexToHead
      val locations: Seq[Location] = tail.map(row => {
        Location(
          row.getOrElse(seqFieldIndex(0), ""),
          row.getOrElse(seqFieldIndex(1), ""),
          row.getOrElse(seqFieldIndex(2), "").toDouble,
          seqIndexRest
            .map(v => (head.getOrElse(v, ""), row.getOrElse(v, "")))
            .filter(s => !s._1.isNullOrEmpty && !s._2.isNullOrEmpty)
        )
      })

      implicit val configuration: Configuration = wikiContext.configuration
      val resultMap = views.html.Wiki.map(ApplicationConf().AhaWiki.google.credentials.api.MapsJavaScriptAPI.key(), locations).toString()
      val resultTable = InterpreterTable.interpret(PageContent("#!Table tsv 1 tablesorter\n" + pageContent.content))
      resultMap + resultTable
    }
  }
}
