package logics.wikis.interpreters

import java.io.File
import java.security.MessageDigest

import implicits.Implicits._
import models.PageContent
import play.api.Logger

import scala.io.Codec
import scala.sys.process._

object InterpreterVim {
  case class Parser(raw: String) {
    val (syntax, content) = {
      if (!raw.startsWith("#!vim")) {
        ("Error!", "Error!")
      } else {
        val array: Array[String] = raw.split( """\r\n|\n""")
        if (array.length == 0) {
          ("Error!", "Error!")
        }
        else {
          val l1 = array.head
          if (array.length == 1) {
            if (l1.length <= 6) {
              ("", "")
            }
            else if (l1.length > 6) {
              (l1.substring(6), "")
            }
          }
          else {
            if (l1.length > 6) {
              (l1.substring(6), array.slice(1, array.length).mkString("\n"))
            } else {
              val l2: String = array(1)
              if (l2.startsWith("#!")) {
                (l2.substring(2), array.slice(2, array.length).mkString("\n"))
              } else {
                ("", "")
              }
            }
          }
        }
      }
    }
  }

  def interpret(pageContent: PageContent):String = {
    implicit val codec:Codec = Codec.UTF8

    val raw = pageContent.raw
    val body = pageContent.content
    val shebang = pageContent.argument.mkString(" ")
    if(shebang.matches("""\W""")) {
      s"Error $shebang"
    } else {
      val md5 = MessageDigest.getInstance("MD5").digest(raw.getBytes).map("%02x".format(_)).mkString
      val cacheDir = new File(new File("cache"), "vim")
      val cacheFileHtml = new File(cacheDir, md5 + ".html")
      if (!cacheFileHtml.exists()) {
        cacheDir.mkdirs()
        val cacheFileText = new File(cacheDir, md5 + ".txt")
        cacheFileText.writeAll(body)

        val cacheFileSh = new File(cacheDir, md5 + ".sh")
        val shellScript = s"""vi -T xterm +"colorscheme elflord" +"syntax on" +"set nonu" +"set syntax=$shebang" +"runtime! syntax/2html.vim" +"wq! ${cacheFileHtml.getPath}" +q! ${cacheFileText.getPath} 2> /dev/null"""
        Logger.info(shellScript)
        cacheFileSh.writeAll(shellScript)
        //noinspection LanguageFeature
        Seq("sh", cacheFileSh.getPath) !!;
        cacheFileSh.delete()
        cacheFileText.delete()

        cacheFileHtml.writeAll(scala.io.Source.fromFile(cacheFileHtml).getLines().drop(9).mkString("\n")
          .replace("body { font-family: monospace; color: #ffffff; background-color: #000000; }", "")
          .replace("pre { font-family: monospace; color: #ffffff; background-color: #000000; }", "")
          .replace("<head>", "")
          .replace("</head>", "")
          .replace("<body>", "")
          .replace("</body>", "")
          .replace("</html>", ""))
      }

      scala.io.Source.fromFile(cacheFileHtml).mkString
    }
  }

}
