package logics.wikis

import actors.ActorAhaWiki.Calculate
import akka.actor.ActorRef
import com.aha00a.commons.Implicits._
import logics.AhaWikiConfig
import logics.wikis.interpreters.Interpreters
import models._
import models.tables.CosineSimilarity
import models.tables.Link
import models.tables.Page
import models.tables.SchemaOrg
import models.tables.TermFrequency
import models.tables.TermFrequency
import play.api.Configuration
import play.api.Logger
import play.api.db.Database

import java.sql.Connection
import java.util.Date

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
        logger.info(wordCount.toList.sortBy(-_._2).mkString(" "))

        TermFrequency.delete(name)
        TermFrequency.insert(name, wordCount)
        CosineSimilarity.recalc(name)
      }

      val seqLink = Interpreters.toSeqLink(page.content).filterNot(_.isDstExternal) ++ Seq(Link(page.name, "", ""))
      Link.delete(name)
      Link.insert(seqLink)

      val seqSchemaOrg: Seq[SchemaOrg] = Interpreters.toSeqSchemaOrg(page.content)
      SchemaOrg.delete(name)
      SchemaOrg.insert(seqSchemaOrg)
    }
  }
}
