package logics.wikis.interpreters

import java.io.File
import java.nio.charset.CodingErrorAction
import java.security.MessageDigest

import com.aha00a.commons.implicits.Implicits
import Implicits._
import logics.ApplicationConf
import models.{PageContent, WikiContext}
import play.api.Logger

import scala.io.Codec
import scala.sys.process._

object InterpreterVim {
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

  def interpret(pageContent: PageContent)(implicit wikiContext: WikiContext):String = {
    implicit val codec:Codec = Codec.UTF8
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val raw = pageContent.raw
    val parser: Parser = Parser(raw.trim)


    val body = parser.content
    val syntax = parser.syntax
    if(parser.isError) {
      "Error!"
    } else {
      val colorscheme: String = ApplicationConf.AhaWiki.config.interpreter.Vim.colorscheme()
      val md5 = MessageDigest.getInstance("MD5").digest((colorscheme + raw).getBytes).map("%02x".format(_)).mkString
      val cacheDir: File = getCacheDir
      val cacheFileHtmlRaw: File = getCacheFileHtmlRaw(cacheDir, md5)
      val cacheFileHtml: File = getCacheFileHtml(cacheDir, md5)
      if (!cacheFileHtml.exists()) {
        cacheDir.mkdirs()
        val cacheFileText = new File(cacheDir, md5 + ".txt")
        cacheFileText.writeAll(body)


        val cacheFileSh = new File(cacheDir, md5 + ".sh")
        val shellScript = s"""vi -T xterm +"set encoding=utf-8" +"colorscheme $colorscheme" +"syntax on" +"set nonu" +"set syntax=$syntax" +"runtime! syntax/2html.vim" +"wq! ${cacheFileHtmlRaw.getPath}" +q! ${cacheFileText.getPath} 2> /dev/null"""
        Logger.info(shellScript)
        cacheFileSh.writeAll(shellScript)
        //noinspection LanguageFeature
        try {
          Seq("sh", cacheFileSh.getPath) !!
        }
        catch {
          case e:RuntimeException => Logger.error(e.toString)
        }
        val lines = scala.io.Source.fromFile(cacheFileHtmlRaw).getLines()
        val style = lines.dropWhile(!_.startsWith("<style ")).takeWhile(_ != "</style>").map(_.replaceAll("^body", s".AhaWiki .wikiContent .class_$md5 pre")).mkString("\n") + "</style>"
        val pre = lines.dropWhile(!_.startsWith("<pre")).takeWhile(_ != "</pre>").mkString("\n") + "</pre>"

        cacheFileHtml.writeAll(style + pre)

        if(!ApplicationConf.AhaWiki.config.interpreter.Vim.debug()) {
          cacheFileHtmlRaw.delete()
          cacheFileSh.delete()
          cacheFileText.delete()
        }
      }

      s"""<div data-md5="$md5" data-delete="${controllers.routes.Dev.deleteVimCache(md5).absoluteURL()(wikiContext.request)}" class="class_$md5 vim" data-lang="$syntax">""" + scala.io.Source.fromFile(cacheFileHtml).mkString + """</div>"""
    }
  }

  def getCacheDir: File = {
    new File(play.Play.application().getFile("cache"), "Vim")
  }

  def getCacheFileHtmlRaw(cacheDir: File, md5: String): File = {
    val cacheFileHtmlRaw = new File(cacheDir, md5 + ".raw.html")
    cacheFileHtmlRaw
  }

  def getCacheFileHtml(cacheDir: File, md5: String): File = {
    val cacheFileHtml = new File(cacheDir, md5 + ".html")
    cacheFileHtml
  }
}
