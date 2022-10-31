package logics.wikis.interpreters

import java.io.File
import java.nio.charset.CodingErrorAction
import java.security.MessageDigest

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import models.PageContent
import models.ContextWikiPage
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database

import scala.io.Codec
import scala.sys.process._

object InterpreterVim extends TraitInterpreter with Logging {

  import models.tables.Link

  case class Parser(raw: String) {
    val (syntax:String, content:String, isError:Boolean) = {
      if (!raw.startsWith("#!Vim")) {
        ("", "", true)
      } else {
        val array: Array[String] = raw.split( """\r\n|\n""")
        if (array.length == 0) {
          ("", "", true)
        }
        else {
          val l1 = array.head
          if (array.length == 1) {
            if (l1.length <= 6) {
              ("", "", false)
            }
            else if (l1.length > 6) {
              (l1.substring(6), "", false)
            }
          }
          else {
            if (l1.length > 6) {
              (l1.substring(6), array.slice(1, array.length).mkString("\n"), false)
            } else {
              val l2: String = array(1)
              if (l2.startsWith("#!")) {
                (l2.substring(2), array.slice(2, array.length).mkString("\n"), false)
              } else {
                ("", "", false)
              }
            }
          }
        }
      }
    }
  }

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage):String = {
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
      import models.tables.Config
      import models.tables.Site
      implicit val database: Database = wikiContext.database
      implicit val site: Site = wikiContext.site
      val (colorscheme, debug) = database.withConnection { implicit connection =>
        val colorscheme = Config.Query.InterpreterVim.colorScheme()
        val debug = Config.Query.InterpreterVim.debug()
        (colorscheme, debug)
      }

      val md5 = MessageDigest.getInstance("MD5").digest((colorscheme + raw).getBytes).map("%02x".format(_)).mkString
      val cacheDir: File = getCacheDir
      val cacheFileHtmlRaw: File = getCacheFileHtmlRaw(cacheDir, md5)
      val cacheFileHtml: File = getCacheFileHtml(cacheDir, md5)
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

        if(debug) {
          cacheFileHtmlRaw.delete()
          cacheFileSh.delete()
          cacheFileText.delete()
        }
      }

      s"""<div data-md5="$md5" data-delete="${controllers.routes.Dev.deleteVimCache(md5)}" class="class_$md5 vim notranslate" data-lang="$syntax">""" + Using(scala.io.Source.fromFile(cacheFileHtml))(_.mkString) + """</div>"""
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

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = Seq()
}
