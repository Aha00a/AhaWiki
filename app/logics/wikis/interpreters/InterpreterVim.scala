package logics.wikis.interpreters

import java.io.File
import java.security.MessageDigest

import implicits.Implicits._
import models.PageContent

import scala.io.Codec
import scala.sys.process._

object InterpreterVim {
  def interpret(pageContent: PageContent):String = {
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
        cacheFileSh.writeAll( s"""vi -T xterm +"colorscheme elflord" +"syntax on" +"set nonu" +"set syntax=$shebang" +"runtime! syntax/2html.vim" +"wq! ${cacheFileHtml.getPath}" +q! ${cacheFileText.getPath} 2> /dev/null""")
        //noinspection LanguageFeature
        Seq("sh", cacheFileSh.getPath) !!;
        cacheFileSh.delete()
        cacheFileText.delete()

        implicit val codec:Codec = Codec.UTF8
        cacheFileHtml.writeAll(scala.io.Source.fromFile(cacheFileHtml).getLines().drop(9).mkString("\n")
          .replace("body { font-family: monospace; color: #ffffff; background-color: #000000; }", "")
          .replace("pre { font-family: monospace; color: #ffffff; background-color: #000000; }", "")
          .replace("<head>", "")
          .replace("</head>", "")
          .replace("<body>", "")
          .replace("</body>", "")
          .replace("</html>", ""))
      }

      implicit val codec:Codec = Codec.UTF8
      scala.io.Source.fromFile(cacheFileHtml).mkString
    }
  }

}
