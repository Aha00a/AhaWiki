package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.interpreters.InterpreterVim
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

import java.io.File
import java.nio.file.Files

object InterpreterVimUnit {
  def run(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    def test(p: InterpreterVim.Parser, syntax: String, content: String, isError: Boolean): Unit = {
      assertEquals(p.syntax, syntax)
      assertEquals(p.content, content)
      assertEquals(p.isError, isError)
    }

    test(InterpreterVim.Parser(""), "", "", true)
    test(InterpreterVim.Parser("#!Vi"), "", "", true)
    test(InterpreterVim.Parser("#!Vim"), "", "", false)
    test(InterpreterVim.Parser("#!Vim c"), "c", "", false)
    test(InterpreterVim.Parser("#!Vim cpp"), "cpp", "", false)
    test(InterpreterVim.Parser("#!Vim\n"), "", "", false)
    test(InterpreterVim.Parser("#!Vim cpp\n"), "cpp", "", false)
    test(InterpreterVim.Parser("#!Vim cpp\n1234"), "cpp", "1234", false)
    test(InterpreterVim.Parser("#!Vim\n#!cpp\n1234"), "cpp", "1234", false)
    // The name is matched without regard to case, as the interpreter lookup does (2026-09-12).
    test(InterpreterVim.Parser("#!vim cpp\n1234"), "cpp", "1234", false)
    test(InterpreterVim.Parser("#!VIM\n#!cpp\n1234"), "cpp", "1234", false)
    // Without a syntax name the body still shows. It came out empty until 2026-09-12.
    test(InterpreterVim.Parser("#!Vim\n1234"), "", "1234", false)
    test(InterpreterVim.Parser("#!Vim  cpp \n1234"), "cpp", "1234", false)

    // The syntax name goes into the command line that runs vi and into an HTML attribute, so it
    // is a Vim syntax file's name or it is nothing: the block then shows without highlighting, as
    // one that names no syntax does. Until 2026-11-21 the name went there as written, and
    // `#!Vim $(...)` ran the command in a shell -- for anyone, since a preview needs no login.
    Seq("$(true)", "`true`", "x\";true;\"", "c|!true", "c;true", "a/b", "c++", "<b>", "\"", "'", "x" * 41).foreach { hostile =>
      test(InterpreterVim.Parser(s"#!Vim $hostile\n1234"), "", "1234", false)
      test(InterpreterVim.Parser(s"#!Vim\n#!$hostile\n1234"), "", "1234", false)
    }
    // Only the first word is the name; what follows it never reaches the command line.
    test(InterpreterVim.Parser("#!Vim scala $(true)\n1234"), "scala", "1234", false)
    test(InterpreterVim.Parser("#!Vim\n#!scala $(true)\n1234"), "scala", "1234", false)
    // Names Vim really has: dotted for two syntaxes at once, digits, an underscore, a hyphen.
    Seq("c.doxygen", "asm68k", "git_rebase", "objc-x", "x" * 40).foreach { name =>
      test(InterpreterVim.Parser(s"#!Vim $name\n1234"), name, "1234", false)
    }

    // And the command vi is run with holds no part of a refused name. It is built from what the
    // parser kept, so this reads the same on a machine with no vi.
    locally {
      val refused = InterpreterVim.Parser("#!Vim $(true)\nval a = 1\n")
      val command = InterpreterVim.viCommand(refused.syntax, "ron", new File("out.html"), new File("in.txt"))
      assertEquals(command.exists(_.contains("$(")), false)
      assertEquals(command.contains("+set syntax="), true)
      assertEquals(command.head, "vi")
    }

    // Drawing a block leaves at most the one file the page reads: no copy of the code, no script,
    // no vi output, no working directory -- whether or not this machine has a vi. Until 2026-11-22
    // every block left four files, and on ahawiki.net three of every four cache files were these.
    locally {
      val dir = Files.createTempDirectory("vim-cache-leftovers").toFile
      try {
        val hostileHtml = InterpreterVim.toHtmlString("#!Vim $(true)\nval a = 1\n", dir)
        assertEquals(hostileHtml.contains("""data-lang="""""), true)
        assertEquals(hostileHtml.contains("$(true)"), false)
        val left = Option(dir.listFiles()).toSeq.flatten.map(_.getName)
        assertEquals(left.forall(_.matches("[0-9a-f]{32}\\.html")), true)
      } finally {
        Option(dir.listFiles()).toSeq.flatten.foreach(_.delete())
        dir.delete()
      }
    }

    // An entry that is drawn again is marked as used, at a day's grain, for the cleanup to read.
    locally {
      val dir = Files.createTempDirectory("vim-cache-used").toFile
      try {
        val raw = "#!Vim scala\nval used = 1\n"
        val md5 = java.security.MessageDigest.getInstance("MD5").digest(("ron" + raw).getBytes).map("%02x".format(_)).mkString
        val entry = InterpreterVim.getCacheFileHtml(dir, md5)
        Files.write(entry.toPath, ("<style>" + ("x" * 400) + "</style><pre>val used = 1</pre>").getBytes("UTF-8"))
        val longAgo = System.currentTimeMillis() - 400L * 24 * 60 * 60 * 1000
        entry.setLastModified(longAgo)
        InterpreterVim.toHtmlString(raw, dir)
        assertEquals(entry.lastModified() > longAgo + 300L * 24 * 60 * 60 * 1000, true)
        // Within a day it is left alone, so a page drawn often does not write to the disk each time.
        val recently = System.currentTimeMillis() - 60L * 60 * 1000
        entry.setLastModified(recently)
        InterpreterVim.toHtmlString(raw, dir)
        assertEquals(entry.lastModified() / 1000, recently / 1000)
      } finally {
        Option(dir.listFiles()).toSeq.flatten.foreach(_.delete())
        dir.delete()
      }
    }

    // A cache directory that cannot be written -- here a plain file standing where the directory
    // would go, so mkdirs and every write under it fail. The block still renders, as the code it
    // holds rather than as highlighted code, and the page around it lives.
    //
    // Before 2026-09-25 only a RuntimeException from running vi was caught, so an IOException from
    // the file work answered 500 for the whole page. On ahawiki.net that was visible as: a Vim
    // block already in the cache rendered, a freshly written one failed.
    val blockingFile = Files.createTempFile("vim-cache-not-a-dir", ".tmp").toFile
    blockingFile.deleteOnExit()
    // Whether or not this machine has a working vi, the code is never put into the page as markup.
    // With no vi output it used to be cached unescaped (until 2026-11-21).
    locally {
      val dir = Files.createTempDirectory("vim-cache-markup").toFile
      try {
        val drawn = InterpreterVim.toHtmlString("#!Vim nosuchsyntaxzz\n<b>bold?</b> List<String>\n", dir)
        assertEquals(drawn.contains("<b>bold?</b>"), false)
        assertEquals(drawn.contains("List<String>"), false)
      } finally {
        Option(dir.listFiles()).toSeq.flatten.foreach(_.delete())
        dir.delete()
      }
    }

    val html = InterpreterVim.toHtmlString("#!Vim scala\nval a = 1 < 2\n", new File(blockingFile, "Vim"))
    assertEquals(html.contains("<pre>val a = 1 &lt; 2</pre>"), true)
    assertEquals(html.contains("Interpreter Vim vim notranslate"), true)
  }
}
