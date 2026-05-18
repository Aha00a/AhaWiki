package logics.wikis

import actors.ActorAhaWiki.Calculate
import akka.actor.ActorRef
import anorm.SqlParser.flatten
import anorm.SqlParser.int
import anorm.SqlParser.str
import anorm.SqlStringInterpolation
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import logics.AhaWikiCache
import logics.AhaWikiConfig
import logics.ApplicationConf
import logics.wikis.interpreters.Interpreters
import models.HighScoredTerm
import models._
import models.tables.CalculatedCosineSimilarity
import models.tables.CalculatedTermFrequency
import models.tables.CalculatedLink
import models.tables.Page
import models.tables.CalculatedSchemaOrg
import models.tables.Attachment
import models.tables.PageMeta
import play.api.Logger
import play.api.db.Database

import java.sql.Connection
import java.time.LocalDateTime
import scala.collection.immutable

object PageLogic {

  private val regexMacroImage = """(?is)\[\[Image\((.*?)\)]]""".r
  private val regexMacroAttachment = """(?is)\[\[Attachment\((.*?)\)]]""".r
  private val regexSchemaBlock = """(?is)\[\[\[#!\s*Schema[^\n]*\n(.*?)]]]""".r

  private def extractSchemaImage(content: String): Option[String] = {
    regexSchemaBlock
      .findAllMatchIn(content)
      .flatMap { schemaBlock =>
        schemaBlock.group(1)
          .splitLinesSeq()
          .flatMap { line =>
            line.splitTabsSeq() match {
              case Seq(key, values @ _*) if key == "image" || key == "logo" => values.map(_.trim).find(_.nonEmpty)
              case _ => None
            }
          }
          .headOption
      }
      .toSeq
      .headOption
  }

  private def extractMacroImage(content: String): Option[String] = {
    regexMacroImage.findAllMatchIn(content).flatMap { m =>
      m.group(1).split(",").headOption.map(_.trim).filter(_.nonEmpty)
    }.toSeq.headOption
  }

  private def extractAttachmentImage(pageName: String)(implicit connection: Connection, site: models.tables.Site): Option[String] = {
    Attachment.selectUploadedByPage(site.seq, pageName).headOption.map(_.objectKey)
  }

  private def extractRepresentativeImage(content: String, pageName: String)(implicit connection: Connection, site: models.tables.Site): Option[String] = {
    extractSchemaImage(content)
      .orElse(extractAttachmentImage(pageName))
      .orElse(extractMacroImage(content))
      .orElse {
        regexMacroAttachment.findAllMatchIn(content).flatMap { m =>
          m.group(1).split(",").headOption.map(_.trim).filter(_.nonEmpty)
        }.toSeq.headOption
      }
  }

  import models.RequestWrapper
  import models.tables.PageWithoutContentWithSize
  import models.tables.Site

  def insert(name: String, revision: Long, dateTime: LocalDateTime, comment: String, isMinorEdit: Boolean, body: String)(implicit wikiContext: ContextWikiPage, connection: Connection): Unit = {
    import models.tables.Page
    import models.tables.Site
    implicit val site: Site = wikiContext.site
    val user = wikiContext.requestWrapper.getUser
    val permRead = PageContent(body).read.getOrElse("")
    val page = Page(name, revision, dateTime, None, user.map(_.seq), wikiContext.requestWrapper.remoteAddress, comment, permRead, isMinorEdit, body)
    Page.insert(page)
    wikiContext.actorAhaWiki ! Calculate(site, name)
  }

  def getListPageByPermission()(implicit provider: RequestWrapper, connection: Connection, contextSite: ContextSite): Seq[PageWithoutContentWithSize] = {
    implicit val site: Site = contextSite.site
    implicit val tupleDatabaseSite: (Database, Site) = (contextSite.database, site)

    val permissionDefaultRead = AhaWikiConfig().permission.default.read()
    val permissionDefaultReadSplit = permissionDefaultRead.splitCommaIgnoreAroundWhitespace()
    val wikiPermission = WikiPermission()

    val list: Seq[PageWithoutContentWithSize] = models.tables.PageMeta.selectSeqWithoutContentWithSizeLatest().map { p =>
      PageWithoutContentWithSize(
        name = p.name,
        revision = p.revision,
        dateTime = p.datePageLastChanged,
        nickname = None,
        user = None,
        remoteAddress = "",
        comment = "",
        permRead = p.permRead,
        isMinorEdit = false,
        size = p.size,
      )
    }
    val listFiltered = list.filter(p => {
      wikiPermission.isReadable(p.name, p.permRead.toOption.map(_.splitCommaIgnoreAroundWhitespace()).getOrElse(permissionDefaultReadSplit))
    })
    // TODO: caching?

    listFiltered
  }

  def calculate(name: String)(
    implicit
    database: Database,
    connection: Connection,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    actorAhaWiki: ActorRef,
    requestWrapper: RequestWrapper,
    logger: Logger,
    site: Site,
  ): Unit = {
    val verbose = false;
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

        CalculatedTermFrequency.delete(name)
        for ((term, frequency) <- seqWordCountSorted) {
          CalculatedTermFrequency.insert(name, term, frequency)
        }
        if(verbose)
          logger.info(seqWordCountSorted.take(10).mkString(" "))

        CalculatedCosineSimilarity.recalc(name)
      }

      val seqLink = Interpreters.toSeqLink(page.content).filterNot(_.isDstExternal) ++ Seq(CalculatedLink(page.name, "", ""))
      CalculatedLink.delete(name)
      CalculatedLink.insert(seqLink)

      val seqSchemaOrg: Seq[CalculatedSchemaOrg] = Interpreters.toSeqSchemaOrg(page.content)
      CalculatedSchemaOrg.delete(name)
      CalculatedSchemaOrg.insert(seqSchemaOrg)

      PageMeta.upsert(
        pageName = page.name,
        revision = page.revision,
        datePageLastChanged = page.dateTime,
        image = extractRepresentativeImage(page.content, page.name),
        permRead = page.permRead,
        size = page.content.length,
      )
    }
  }

  def selectHighScoredTerm(name: String, similarPageNames: Seq[String])(implicit connection: Connection, site: models.tables.Site): Seq[HighScoredTerm] = {
    if (similarPageNames.isEmpty) {
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
