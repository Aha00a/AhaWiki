package logics.wikis

import actors.ActorPageCalculator.Calculate
import anorm.SqlParser.flatten
import anorm.SqlParser.int
import anorm.SqlParser.str
import anorm.SqlStringInterpolation
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.wikis.interpreters.Interpreters
import logics.wikis.macros.MacroAttachment
import models.HighScoredTerm
import models._
import models.tables.CalculatedCosineSimilarity
import models.tables.CalculatedTerm
import models.tables.CalculatedTermFrequency
import models.tables.CalculatedLink
import models.tables.Page
import models.tables.CalculatedSchemaOrg
import models.tables.Attachment
import models.tables.PageMeta
import play.api.Logger
import play.api.db.Database

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.sql.Connection
import java.time.LocalDateTime
import scala.collection.immutable
import scala.util.matching.Regex

object PageLogic {
  private val percentEncodedRegex: Regex = """(?:%[0-9A-Fa-f]{2})+""".r
  private val DescriptionMaxLength = 180

  private[wikis] def decodePercentEncodedText(text: String): String =
    percentEncodedRegex.replaceAllIn(text, m =>
      Regex.quoteReplacement(URLDecoder.decode(m.matched, StandardCharsets.UTF_8.name()))
    )

  private def truncateDescription(text: String): String = {
    if (text.length <= DescriptionMaxLength) text else text.take(DescriptionMaxLength).trim + "..."
  }

  def extractDescription(content: String)(implicit wikiContext: ContextWikiPage): Option[String] = {
    val pageContent = PageContent(content)
    if (pageContent.redirect.nonEmpty) {
      None
    } else {
      val contentWithoutSchemaBlocks = regexSchemaBlock.replaceAllIn(content, "")
      val text = decodePercentEncodedText(Interpreters.toText(contentWithoutSchemaBlocks))
        .replaceAll("""\s+""", " ")
        .trim
      text.toOption.map(truncateDescription)
    }
  }


  private val regexMacroImage = """(?is)\[\[Image\((.*?)\)]]""".r
  private val regexMacroAttachment = """(?is)\[\[Attachment\((.*?)\)]]""".r
  private val regexSchemaBlock = """(?is)\[\[\[#!\s*Schema[^\n]*\n(.*?)]]]""".r

  private def normalizeImageValue(value: String, pageName: String)(implicit site: models.tables.Site): String = {
    regexMacroAttachment.findFirstMatchIn(value) match {
      case Some(m) =>
        val rawKey = m.group(1).split(",").headOption.map(_.trim).getOrElse("")
        MacroAttachment.toAttachmentUri(rawKey, site.seq, pageName)
      case None => value
    }
  }

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

  // A page's markup as the renderer reads it (ExtractConvertInjectBackQuote.markupOnly). The image
  // macros are looked for in this rather than in the raw text, because a [[Image(...)]] written in
  // a code span or a #!Vim block is shown, not run. Until 2026-10-03 it became the page's image
  // anyway: Dev Page, which lists the image sources in backticks, had "..." for its image, and
  // every graph that showed the page asked for /... and got a 404.
  private[wikis] def markupOnly(content: String): String = new ExtractConvertInjectBackQuote().markupOnly(content)

  private[wikis] def extractMacroImage(markup: String): Option[String] = {
    regexMacroImage.findAllMatchIn(markup).flatMap { m =>
      m.group(1).split(",").headOption.map(_.trim).filter(_.nonEmpty)
    }.toSeq.headOption
  }

  private[wikis] def extractMacroAttachmentKey(markup: String): Option[String] = {
    regexMacroAttachment.findAllMatchIn(markup).flatMap { m =>
      m.group(1).split(",").headOption.map(_.trim).filter(_.nonEmpty)
    }.toSeq.headOption
  }

  private def extractAttachmentImage(pageName: String)(implicit connection: Connection, site: models.tables.Site): Option[String] = {
    Attachment.selectUploadedByPage(site.seq, pageName).headOption.map { att =>
      s"attachment:${att.objectKey.stripPrefix(s"Attachment/${site.seq}/")}"
    }
  }

  private def extractRepresentativeImage(content: String, pageName: String)(implicit connection: Connection, site: models.tables.Site): Option[String] = {
    lazy val markup = markupOnly(content)
    extractSchemaImage(content).map(normalizeImageValue(_, pageName))
      .orElse(extractAttachmentImage(pageName))
      .orElse(extractMacroImage(markup))
      .orElse(extractMacroAttachmentKey(markup).map(MacroAttachment.toAttachmentUri(_, site.seq, pageName)))
  }

  import models.RequestWrapper
  import models.tables.Site

  def insert(name: String, revision: Long, dateTime: LocalDateTime, comment: String, isMinorEdit: Boolean, body: String, viaApi: Boolean = false, userApiKey: Option[Long] = None)(implicit wikiContext: ContextWikiPage, connection: Connection): Unit = {
    import models.tables.Page
    import models.tables.Site
    implicit val site: Site = wikiContext.site
    val user = wikiContext.requestWrapper.getUser
    val page = Page(name, revision, dateTime, None, user.map(_.seq), wikiContext.requestWrapper.remoteAddress, comment, isMinorEdit, body, viaApi, userApiKey)
    Page.insert(page)
    wikiContext.pageCalculationActor ! Calculate(site, name)
  }

  def getListPageByPermission()(implicit provider: RequestWrapper, connection: Connection, contextSite: ContextSite, ahaWikiCache: AhaWikiCache): Seq[PageLatestSummary] = {
    implicit val site: Site = contextSite.site
    implicit val tupleDatabaseSite: (Database, Site) = contextSite.tupleDatabaseSite

    val wikiPermission = contextSite.wikiPermission

    val list: Seq[PageLatestSummary] = ahaWikiCache.PageMeta.SeqPageLatestSummary.get()
    // Every rule of most sites targets every page, and then one answer serves the whole list
    // instead of one match per page -- see WikiPermission.isReadableRegardlessOfTarget.
    wikiPermission.isReadableRegardlessOfTarget match {
      case Some(true) => list
      case Some(false) => Seq.empty
      case None => list.filter(p => wikiPermission.isReadable(p.name))
    }
  }

  /**
   * How long a calculation stands before a view may redo it.
   *
   * Almost everything `calculate` derives -- description, image, term frequencies, links, schema.org
   * -- comes from the page's own text, which only an edit changes, and every edit queues its own
   * calculation. What goes stale without one is the page's similar pages: `CalculatedCosineSimilarity`
   * rows are written from the calculated page's side only, so another page's edit reaches this page's
   * list when this page is calculated again. A view is what does that, and until 2026-10-12 one view
   * in ten did it every time. In the first 80 minutes of one instance taking every request, views
   * started 16 calculations, 8.7s in all and up to 3.8s each, three of them for a page calculated
   * less than half an hour before.
   *
   * A day is how far behind a similar-pages list may now run. It is also how long a change to the
   * calculation itself takes to reach a page that is viewed but not edited.
   *
   * What it bought, measured on the production log over 12.9 days before and 6.3 days after it
   * went live on 2026-10-24: the repeats. Calculations started by views ran 18.6 times per 80
   * minutes before and 20.2 after -- crawlers spread their views over pages that have not been
   * calculated in a day -- while a page recalculated within half an hour of the last time went
   * from 293 cases to 3, about 7% of the runs. The wiki ToDo page has the figures.
   */
  val ViewRecalculationInterval: java.time.Duration = java.time.Duration.ofDays(1)

  /**
   * A term held by more than this share of all calculated pages is left out of a page's term
   * vector, and so out of its similarity.
   *
   * The similarity of a page is a join of its terms against every page that shares one, so a
   * term's cost is the number of pages holding it, and a handful of terms that nearly every page
   * holds -- particles, "page", "wiki" -- are most of the work and say nothing about which pages
   * are alike. Eleven English stop words were the whole list until 2026-11-01; the wiki is mostly
   * Korean. Measured on the 130 wiki pages in this repository (tokenised the same way): 64 of
   * 12,299 terms stood in more than a fifth of the pages and made 38% of the join work, the
   * squared frequencies summed. A fifth keeps a term that a strong theme of the wiki shares.
   *
   * The cut applies when a page is calculated, so two pages agree on a term only once both have
   * been calculated under it; until then the term still counts between them. The similarity
   * sections therefore change gradually, and a deploy's page-by-page comparison will show them
   * differing. Setting the ratio above 1 restores the old vectors at the next calculation.
   */
  val MaxDocumentFrequencyRatio: Double = 0.2

  /**
   * Below this many pages the cut does nothing: on a wiki of thirty pages, a fifth is six, and a
   * term six pages share is a subject, not noise. Also what keeps the specs' small fixtures whole.
   */
  val MinDocumentFrequencyCutoff: Long = 20

  /** The document frequency above which a term is dropped, for a corpus of this many pages. */
  def documentFrequencyCutoff(calculatedPages: Long): Long =
    math.max(MinDocumentFrequencyCutoff, math.ceil(calculatedPages * MaxDocumentFrequencyRatio).toLong)

  /** True when the page was calculated at its latest revision less than ViewRecalculationInterval
    * ago. No row, no update time or a revision behind the latest all mean it was not. */
  def isCalculatedRecently(meta: Option[PageMeta], lastRevision: Option[Long], now: LocalDateTime): Boolean =
    meta.exists(m => lastRevision.contains(m.revision) && m.dateUpdated.exists(_.isAfter(now.minus(ViewRecalculationInterval))))

  def calculate(name: String)(
    implicit
    database: Database,
    connection: Connection,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    wikiActors: WikiActors,
    requestWrapper: RequestWrapper,
    logger: Logger,
    site: Site,
  ): Unit = {
    val verbose = false;
    val seqStopWord: Seq[String] = """at in on of by to is the gmail com http https""".stripMargin.split("""\s""").toSeq

    Page.selectLastRevision(name) foreach { page =>
      implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(page.name), RenderingMode.Normal).holding(connection)

      val revisionIsStillThere = PageMeta.upsert(
        pageName = page.name,
        revision = page.revision,
        image = extractRepresentativeImage(page.content, page.name),
        description = extractDescription(page.content),
        size = page.content.length,
      )

      if (!revisionIsStillThere) {
        // Deleted or renamed since it was read a few lines up. Nothing more is derived from a
        // revision that is gone; the request that removed it queues its own calculation.
        logger.info(s"\t\tCalculate: ${site.name}(${site.seq})\t$name\trevision ${page.revision} is gone, skipped")
      } else {
        val text = Interpreters.toText(page.content)
        if (!text.isNullOrEmpty) {
          val seqWord = decodePercentEncodedText(text)
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

          val termByWord = CalculatedTerm.ensureSeqByTerm(seqWordCountSorted.map(_._1))
          val cutoff = documentFrequencyCutoff(CalculatedTermFrequency.countCalculatedPages())
          val documentFrequency = CalculatedTermFrequency.selectDocumentFrequency(termByWord.values.toSeq)
          val seqTermFrequency = seqWordCountSorted.flatMap { case (term, frequency) =>
            termByWord.get(term)
              .filterNot(seq => documentFrequency.getOrElse(seq, 0) > cutoff)
              .map(_ -> frequency)
          }
          CalculatedTermFrequency.replace(name, seqTermFrequency)

          if(verbose)
            logger.info(seqWordCountSorted.take(10).mkString(" "))

          CalculatedCosineSimilarity.recalc(name)
        } else {
          CalculatedTermFrequency.delete(name)
          CalculatedCosineSimilarity.delete(name)
        }

        val seqLink = Interpreters.toSeqLink(page.content).filterNot(_.isDstExternal) ++ Seq(CalculatedLink(page.name, "", ""))
        CalculatedLink.delete(name)
        CalculatedLink.insert(seqLink)

        val seqSchemaOrg: Seq[CalculatedSchemaOrg] = Interpreters.toSeqSchemaOrg(page.content)
        CalculatedSchemaOrg.delete(name)
        CalculatedSchemaOrg.insert(seqSchemaOrg)
      }
    }
  }

  def selectHighScoredTerm(name: String, similarPageNames: Seq[String])(implicit connection: Connection, site: models.tables.Site): Seq[HighScoredTerm] = {
    selectHighScoredTerm(site, name, site, similarPageNames)
  }

  /**
   * The terms a page shares with each of its similar pages, strongest first.
   *
   * The caller keeps the first ten per page, so the ordering has to be total. Ranking by the
   * summed frequency alone left every tie to the database, which does not answer the same way
   * twice: `SimilarPages` rendered a different set of terms, in a different order, on two
   * consecutive requests for a page nobody had edited.
   */
  def selectHighScoredTerm(
    sourceSite: models.tables.Site,
    name: String,
    targetSite: models.tables.Site,
    similarPageNames: Seq[String],
  )(implicit connection: Connection): Seq[HighScoredTerm] = {
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
        CTF1.site = ${sourceSite.seq} AND CTF1.name = $name AND
        CTF2.site = ${targetSite.seq} AND CTF2.name IN ($similarPageNames)
    ORDER BY frequency1 + frequency2 DESC, frequency1 DESC, CT.term ASC
      """
        .as(str("name") ~ str("term") ~ int("frequency1") ~ int("frequency2") *).map(flatten)
        .map(HighScoredTerm.tupled)
    }
  }
}
