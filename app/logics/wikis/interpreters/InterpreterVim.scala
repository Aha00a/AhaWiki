package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import models.ContextWikiPage
import models.PageContent
import models.tables.CalculatedLink
import play.api.Logging

import java.io.File
import java.nio.charset.CodingErrorAction
import java.security.MessageDigest
import scala.io.Codec
import scala.sys.process._
import scala.util.control.NonFatal

object InterpreterVim extends TraitInterpreter with Logging {

  private val ColorScheme = "ron"
  private val Shebang = "#!Vim"

  // What a syntax name is made of: the name of a Vim syntax file -- letters, digits, `_`, `-`, and
  // `.` (Vim reads `c.doxygen` as two syntaxes at once).
  private val regexSyntaxName = """[A-Za-z0-9_.-]{1,40}""".r

  /**
   * The syntax name in what the author wrote after `#!Vim` (or after `#!` on the next line): its
   * first word when that is a syntax name, and otherwise none -- the block then shows without
   * highlighting, as a block that names no syntax does.
   *
   * The name goes into the command line that runs vi (buildCacheEntry) and into an HTML attribute.
   * Until 2026-11-21 it went there as written, so `#!Vim $(...)` ran a command in a shell on the
   * server -- and a preview renders a block for anyone, logged in or not. Even without a shell the
   * name is an Ex command's argument, where `|` starts the next command and `!` runs one, so it is
   * restricted rather than quoted, and it is restricted here, where the block is read, so nothing
   * downstream ever holds anything else.
   */
  private def syntaxName(written: String): String =
    written.trim.split("""\s+""").headOption.filter(regexSyntaxName.matches).getOrElse("")

  case class Parser(raw: String) {
    val (syntax:String, content:String, isError:Boolean) = {
      // The interpreter lookup ignores case (Interpreters.getInterpreter), so `#!vim` reaches
      // here too, and this check has to agree with it. It was case-sensitive until 2026-09-12,
      // and a `#!vim` block showed "Error!".
      if (!raw.regionMatches(true, 0, Shebang, 0, Shebang.length)) {
        ("", "", true)
      } else {
        val lines: Array[String] = raw.split( """\r\n|\n""")
        val writtenOnFirstLine = lines.head.substring(Shebang.length).trim
        val rest = lines.drop(1)
        if (writtenOnFirstLine.nonEmpty) {
          (syntaxName(writtenOnFirstLine), rest.mkString("\n"), false)
        } else {
          rest.headOption match {
            case Some(l2) if l2.startsWith("#!") => (syntaxName(l2.substring(2)), rest.drop(1).mkString("\n"), false)
            // No syntax name: the body shows without highlighting. Until 2026-09-12 it came
            // out empty.
            case _ => ("", rest.mkString("\n"), false)
          }
        }
      }
    }
  }

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String =
    toHtmlString(content, getCacheDir)

  /**
   * The cache directory is a parameter so a caller -- a test -- can point it somewhere that cannot
   * be written, which is the case this has to survive: see the fallback below.
   */
  def toHtmlString(content: String, cacheDir: File)(implicit wikiContext: ContextWikiPage):String = {
    implicit val codec:Codec = Codec.UTF8
    val pageContent: PageContent = PageContent(content)

    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val raw = pageContent.raw
    val parser: Parser = Parser(raw.trim)


    val body = parser.content
    val syntax = parser.syntax
    if(parser.isError) {
      "Error!"
    } else {
      val colorscheme = ColorScheme

      val md5 = MessageDigest.getInstance("MD5").digest((colorscheme + raw).getBytes).map("%02x".format(_)).mkString
      val cacheFileHtmlRaw: File = getCacheFileHtmlRaw(cacheDir, md5)
      val cacheFileHtml: File = getCacheFileHtml(cacheDir, md5)
      // Highlighting is a nicety; the code itself is not. Anything that goes wrong while producing
      // the cache entry -- an unwritable cache directory, no `sh` on the PATH -- degrades to the
      // plain block rather than taking the page down with it. Only the failure to RUN vi was caught
      // before, and only as a RuntimeException, so an IOException from the file work or from
      // starting the process answered 500 for the whole page. On ahawiki.net that is what a
      // freshly written Vim block did: a block already in the cache rendered, a new one 500'd.
      val cachedHtml = try {
        buildCacheEntry(cacheDir, cacheFileHtml, cacheFileHtmlRaw, md5, body, syntax, colorscheme)
        Using(scala.io.Source.fromFile(cacheFileHtml))(_.mkString)
      } catch {
        case NonFatal(e) =>
          logger.error(s"Vim highlighting unavailable, falling back to plain text (md5=$md5, cacheDir=${cacheDir.getPath}): $e")
          s"<pre>${body.escapeHtml()}</pre>"
      }

      s"""<div data-md5="$md5" data-delete="${controllers.routes.Dev.deleteVimCache(md5)}" class="class_$md5 Interpreter Vim vim notranslate" data-lang="$syntax">""" + cachedHtml + """</div>"""
    }
  }

  /**
   * How vi is run: as a program with its arguments, not through a shell. Until 2026-11-22 the same
   * command line was written to `{md5}.sh` and run with `sh`, so a block left four files behind --
   * the code, the script, vi's own HTML and the HTML the page reads -- and only the last was ever
   * read again. Without the shell there is no quoting for anything to escape from; the syntax
   * name is still restricted (syntaxName), because it is an Ex command's argument either way.
   */
  def viCommand(syntax: String, colorscheme: String, rawHtmlOut: File, textIn: File): Seq[String] = Seq(
    "vi", "-T", "xterm",
    "+set encoding=utf-8", s"+colorscheme $colorscheme", "+syntax on", "+set nonu", s"+set syntax=$syntax",
    "+runtime! syntax/2html.vim", s"+wq! ${rawHtmlOut.getSlashBasedPath}", "+q!",
    textIn.getSlashBasedPath,
  )

  /** What a working directory under the cache is called. The cleanup removes one left behind by a
    * process that died while vi ran (CacheFileCleanupService). */
  val WorkDirPrefix = "work-"

  private def buildCacheEntry(
    cacheDir: File,
    cacheFileHtml: File,
    cacheFileHtmlRaw: File,
    md5: String,
    body: String,
    syntax: String,
    colorscheme: String,
  )(implicit codec: Codec): Unit = {
    if (isCacheFileHtmlInvalid(cacheFileHtml)) {
      cacheDir.mkdirs()
      // vi's input and output live in a directory of their own, inside the cache so the finished
      // file moves into place on the same file system, and the directory goes when this is done.
      val workDir = java.nio.file.Files.createTempDirectory(cacheDir.toPath, WorkDirPrefix).toFile
      try {
        val textIn = new File(workDir, "in.txt")
        val rawHtmlOut = new File(workDir, cacheFileHtmlRaw.getName)
        textIn.writeAll(body)
        val command = viCommand(syntax, colorscheme, rawHtmlOut, textIn)
        logger.info(command.mkString(" "))
        try {
          //noinspection LanguageFeature
          command ! ProcessLogger(_ => (), _ => ())
        } catch {
          case e: RuntimeException => logger.error(e.toString)
        }
        writeCacheFileHtml(rawHtmlOut, new File(workDir, cacheFileHtml.getName), md5)
        java.nio.file.Files.move(new File(workDir, cacheFileHtml.getName).toPath, cacheFileHtml.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      } finally {
        Option(workDir.listFiles()).toSeq.flatten.foreach(_.delete())
        workDir.delete()
      }
    } else {
      markUsed(cacheFileHtml)
    }
  }

  /**
   * An entry's modification time is when it was last drawn, at a day's grain: that is what the
   * monthly cleanup reads to drop entries nothing has drawn for a year. The access time looked like
   * it would do and did not -- anything that reads the directory sets it, and one read of every
   * file on 2026-11-21 made all 7,043 entries look used that day.
   */
  def markUsed(cacheFileHtml: File, now: Long = System.currentTimeMillis()): Unit =
    if (now - cacheFileHtml.lastModified() > 24L * 60 * 60 * 1000) cacheFileHtml.setLastModified(now)

  private def writeCacheFileHtml(cacheFileHtmlRaw: File, cacheFileHtml: File, md5: String)(implicit codec: Codec): Unit = {
    {
      if(cacheFileHtmlRaw.exists()) {
        val lines = Using(scala.io.Source.fromFile(cacheFileHtmlRaw))(_.getLines().toSeq)
        val style = lines.dropWhile(!_.startsWith("<style")).takeWhile(_ != "</style>")
        val styleReplaced = style
          .filterNot(l => l.startsWith("*"))
          .map(_.replaceAll("^(\\.)", s".AhaWiki .wikiContent .class_$md5 pre $$1"))
          .map(_.replaceAll("^pre", s".AhaWiki .wikiContent .class_$md5 pre"))
          .map(_.replaceAll("^body", s".AhaWiki .wikiContent .class_$md5 pre"))
        val styleString = styleReplaced.mkString("\n") + "</style>"
        val pre = lines.dropWhile(!_.startsWith("<pre")).takeWhile(_ != "</pre>").mkString("\n") + "</pre>"

        cacheFileHtml.writeAll(styleString + pre)
      } else {
        // vi wrote nothing -- it is missing, or it failed. The caller then shows the code escaped
        // and caches nothing, so installing vim later fixes the block. Until 2026-11-21 this
        // wrote the code into the cache as it was, unescaped: `List<String>` lost `<String>`, an
        // HTML example ran as markup, and it stayed so after vim was installed.
        throw new java.io.IOException(s"vi wrote no highlighted file for ${cacheFileHtmlRaw.getName}")
      }
    }
  }

  private def isCacheFileHtmlInvalid(cacheFileHtml: File): Boolean = {
    if(!cacheFileHtml.exists())
      return true

    if(cacheFileHtml.length() > 310)
      return false

    val s = Using(scala.io.Source.fromFile(cacheFileHtml))(_.mkString)
    s.endsWith("</style><pre>\n</pre>") || s.endsWith("</style><pre id='vimCodeElement'>\n</pre>")
  }

  def getCacheDir: File = new File(new File("cache"), "Vim")

  def getCacheFileHtmlRaw(cacheDir: File, md5: String): File = new File(cacheDir, md5 + ".raw.html")

  def getCacheFileHtml(cacheDir: File, md5: String): File = new File(cacheDir, md5 + ".html")

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq()
}
