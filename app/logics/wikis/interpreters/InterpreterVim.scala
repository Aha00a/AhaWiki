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

  case class Parser(raw: String) {
    val (syntax:String, content:String, isError:Boolean) = {
      // The interpreter lookup ignores case (Interpreters.getInterpreter), so `#!vim` reaches
      // here too, and this check has to agree with it. It was case-sensitive until 2026-09-12,
      // and a `#!vim` block showed "Error!".
      if (!raw.regionMatches(true, 0, Shebang, 0, Shebang.length)) {
        ("", "", true)
      } else {
        val lines: Array[String] = raw.split( """\r\n|\n""")
        val syntaxOnFirstLine = lines.head.substring(Shebang.length).trim
        val rest = lines.drop(1)
        if (syntaxOnFirstLine.nonEmpty) {
          (syntaxOnFirstLine, rest.mkString("\n"), false)
        } else {
          rest.headOption match {
            case Some(l2) if l2.startsWith("#!") => (l2.substring(2), rest.drop(1).mkString("\n"), false)
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
      val cacheFileText = new File(cacheDir, md5 + ".txt")
      cacheFileText.writeAll(body)


      val cacheFileSh = new File(cacheDir, md5 + ".sh")
      val shellScript = s"""vi -T xterm +"set encoding=utf-8" +"colorscheme $colorscheme" +"syntax on" +"set nonu" +"set syntax=$syntax" +"runtime! syntax/2html.vim" +"wq! ${cacheFileHtmlRaw.getSlashBasedPath}" +q! ${cacheFileText.getSlashBasedPath} 2> /dev/null"""
      logger.info(shellScript)
      cacheFileSh.writeAll(shellScript)
      //noinspection LanguageFeature
      try {
        Seq("sh", cacheFileSh.getPath) !!
      }
      catch {
        case e:RuntimeException => logger.error(e.toString)
      }

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
        val lines = Using(scala.io.Source.fromFile(cacheFileText))(_.getLines().toSeq)
        cacheFileHtml.writeAll("<pre>" + lines.mkString("\n") + "</pre>")
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
