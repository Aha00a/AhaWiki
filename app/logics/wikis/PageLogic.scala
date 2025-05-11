package logics.wikis

import actors.ActorAhaWiki.Calculate
import akka.actor.ActorRef
import anorm.SqlParser.flatten
import anorm.SqlParser.int
import anorm.SqlParser.str
import anorm.SqlStringInterpolation
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import logics.AhaWikiConfig
import logics.ApplicationConf
import logics.wikis.interpreters.Interpreters
import models.HighScoredTerm
import models._
import models.tables.CalculatedCosineSimilarity
import models.tables.CalculatedTermFrequency
import models.tables.Link
import models.tables.Page
import models.tables.SchemaOrg
import play.api.Configuration
import play.api.Logger
import play.api.db.Database

import java.sql.Connection
import java.util.Date
import scala.collection.immutable

object PageLogic {

  import models.ContextSite.RequestWrapper
  import models.tables.PageWithoutContentWithSize
  import models.tables.Site

  def insert(name: String, revision: Long, dateTime: Date, comment: String, body: String)(implicit wikiContext: ContextWikiPage, connection: Connection): Unit = {
    import models.tables.Page
    import models.tables.Site
    implicit val site: Site = wikiContext.site
    val author = wikiContext.requestWrapper.getId.getOrElse("anonymous")
    val permRead = PageContent(body).read.getOrElse("")
    val page = Page(name, revision, dateTime, author, wikiContext.requestWrapper.remoteAddress, comment, permRead, body)
    Page.insert(page)
    wikiContext.actorAhaWiki ! Calculate(site, name)
  }

  def getListPageByPermission()(implicit provider: RequestWrapper, connection: Connection, site: Site): List[PageWithoutContentWithSize] = {
    val permissionDefaultRead = AhaWikiConfig().permission.default.read()
    val permissionDefaultReadSplit = permissionDefaultRead.splitCommaIgnoreAroundWhitespace()
    val wikiPermission = WikiPermission()
    val optionId = provider.getId
    val list: List[PageWithoutContentWithSize] = Page.pageSelectPageList()
    val listFiltered = list.filter(p => {
      wikiPermission.allowed(optionId, p.permRead.toOption.map(_.splitCommaIgnoreAroundWhitespace()).getOrElse(permissionDefaultReadSplit))
    })
    listFiltered
  }

  def calculate(name: String)(
    implicit
    database: Database,
    connection: Connection,
    configuration: Configuration,
    applicationConf: ApplicationConf,
    actorAhaWiki: ActorRef,
    requestWrapper: RequestWrapper,
    logger: Logger,
    site: Site,
  ): Unit = {
    val seqStopWord: Seq[String] = """at in on of by to is the gmail com http https""".stripMargin.split("""\s""").toSeq

    Page.selectLastRevision(name) foreach { page =>
      implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(page.name), RenderingMode.Normal)

      val text = Interpreters.toText(page.content)
      if (!text.isNullOrEmpty) {
        val seqWord = text
          .replaceAll("""%[0-9A-F][0-9A-F]""", " ") // TODO: URL decode
          .replaceAll("""([a-z])([A-Z])""", "$1 $2")
          .replaceAll("""(\d{4})-(\d{2})-(\d{2})""", "$1$2$3")
          .replaceAll("""(\d{2}):(\d{2}):(\d{2})""", "$1$2$3")
          .replaceAll("""[{}\[\]/?.,;:|)*~`!^\-_+<>@#$%&\\=('"]""", " ")
          .toLowerCase()
          .split("""\s""").toSeq
          .flatMap(s => s.replaceAll("""^(\d{8})t(\d{6})$""", "$1").split(" ").toSeq)
          .filterNot(s => s.length < 2)
          .filterNot(s => s.length > 15)
          .filterNot(s => s.matches("""\d{1,2}"""))
        val seqWordFiltered = seqWord.filter(w => !seqStopWord.contains(w))
        val wordCount = seqWordFiltered.groupByCount()
        val seqWordCountSorted = wordCount.toSeq.sortBy(-_._2)

        StopWatch(s"Calculate\t${site.name}(${site.seq})\t$name\tInsert Term Frequency\t${seqWordCountSorted.size}") {
          CalculatedTermFrequency.delete(name)
          for ((term, frequency) <- seqWordCountSorted) {
            CalculatedTermFrequency.insert(name, term, frequency)
          }
        }
        logger.info(seqWordCountSorted.mkString(" "))

        StopWatch(s"Calculate\t${site.name}(${site.seq})\t$name\tCalculate Cosine Similarity") {
          CalculatedCosineSimilarity.recalc(name)
        }
      }

      val seqLink = Interpreters.toSeqLink(page.content).filterNot(_.isDstExternal) ++ Seq(Link(page.name, "", ""))
      Link.delete(name)
      Link.insert(seqLink)

      val seqSchemaOrg: Seq[SchemaOrg] = Interpreters.toSeqSchemaOrg(page.content)
      SchemaOrg.delete(name)
      SchemaOrg.insert(seqSchemaOrg)
    }
  }

  def selectHighScoredTerm(name:String, similarPageNames:Seq[String])(implicit connection: Connection, site: Site): Seq[HighScoredTerm] = {
    if(similarPageNames.isEmpty) {
      immutable.Seq()
    } else {
      SQL"""
SELECT
    CTF2.name, CT.term, CTF1.frequency frequency1, CTF2.frequency frequency2
    FROM CalculatedTermFrequency CTF1
    INNER JOIN CalculatedTermFrequency CTF2 ON CTF1.term = CTF2.term
    INNER JOIN CalculatedTerm CT ON CTF1.term = CT.seq
    WHERE
        CTF1.site = ${site.seq} AND CTF1.name = $name AND
        CTF2.site = ${site.seq} AND CTF2.name IN ($similarPageNames)
    ORDER BY frequency1 + frequency2 DESC
      """
        .as(str("name") ~ str("term") ~ int("frequency1") ~ int("frequency2") *).map(flatten)
        .map(HighScoredTerm.tupled)
    }
  }
}
