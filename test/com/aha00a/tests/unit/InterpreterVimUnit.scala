package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.interpreters.InterpreterVim

object InterpreterVimUnit {
  def run(testUtil: TestUtil): Unit = {
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
  }
}
