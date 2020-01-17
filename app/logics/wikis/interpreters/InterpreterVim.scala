package logics.wikis.interpreters

import java.io.File
import java.nio.charset.CodingErrorAction
import java.security.MessageDigest

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import logics.AhaWikiConfig
import models.{Link, PageContent, WikiContext}
import play.api.Logger
import play.api.cache.CacheApi
import play.api.db.Database

import scala.io.Codec
import scala.sys.process._

object InterpreterVim extends TraitInterpreter {
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

  override def interpret(content: String)(implicit wikiContext: WikiContext):String = {
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
      implicit val cacheApi: CacheApi = wikiContext.cacheApi
      implicit val database: Database = wikiContext.database
      val colorscheme: String = AhaWikiConfig().interpreter.Vim.colorscheme()
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
        Logger.info(shellScript)
        cacheFileSh.writeAll(shellScript)
        //noinspection LanguageFeature
        try {
          Seq("sh", cacheFileSh.getPath) !!
        }
        catch {
          case e:RuntimeException => Logger.error(e.toString)
        }

        if(cacheFileHtmlRaw.exists()) {
          val lines = Using(scala.io.Source.fromFile(cacheFileHtmlRaw))(_.getLines().toSeq)
          val style = lines.dropWhile(!_.startsWith("<style ")).takeWhile(_ != "</style>").filterNot(_.startsWith("*")).map(_.replaceAll("^body", s".AhaWiki .wikiContent .class_$md5 pre")).mkString("\n") + "</style>"
          val pre = lines.dropWhile(!_.startsWith("<pre")).takeWhile(_ != "</pre>").mkString("\n") + "</pre>"

          cacheFileHtml.writeAll(style + pre)
        } else {
          val lines = Using(scala.io.Source.fromFile(cacheFileText))(_.getLines().toSeq)
          cacheFileHtml.writeAll("<pre>" + lines.mkString("\n") + "</pre>")
        }

        if(!AhaWikiConfig().interpreter.Vim.debug()) {
          cacheFileHtmlRaw.delete()
          cacheFileSh.delete()
          cacheFileText.delete()
        }
      }

      s"""<div data-md5="$md5" data-delete="${controllers.routes.Dev.deleteVimCache(md5).absoluteURL()(wikiContext.request)}" class="class_$md5 vim" data-lang="$syntax">""" + Using(scala.io.Source.fromFile(cacheFileHtml))(_.mkString) + """</div>"""
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

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
