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

    // A cache directory that cannot be written -- here a plain file standing where the directory
    // would go, so mkdirs and every write under it fail. The block still renders, as the code it
    // holds rather than as highlighted code, and the page around it lives.
    //
    // Before 2026-09-25 only a RuntimeException from running vi was caught, so an IOException from
    // the file work answered 500 for the whole page. On ahawiki.net that was visible as: a Vim
    // block already in the cache rendered, a freshly written one failed.
    val blockingFile = Files.createTempFile("vim-cache-not-a-dir", ".tmp").toFile
    blockingFile.deleteOnExit()
    val html = InterpreterVim.toHtmlString("#!Vim scala\nval a = 1 < 2\n", new File(blockingFile, "Vim"))
    assertEquals(html.contains("<pre>val a = 1 &lt; 2</pre>"), true)
    assertEquals(html.contains("Interpreter Vim vim notranslate"), true)
  }
}
