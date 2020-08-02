package logics.wikis.interpreters

import org.scalatest.freespec.AnyFreeSpec

//noinspection NameBooleanParameters
class InterpreterVimSpec extends AnyFreeSpec {
  "Parser" in {
    def test(p: InterpreterVim.Parser, syntax: String, content: String, isError: Boolean): Unit = {
      assert(p.syntax === syntax)
      assert(p.content === content)
      assert(p.isError === isError)
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
    test(InterpreterVim.Parser("#!Vim cpp\n1234\n1234"), "cpp", "1234\n1234", false)
    test(InterpreterVim.Parser("#!Vim\n#!cpp\n1234\n1234"), "cpp", "1234\n1234", false)
    test(InterpreterVim.Parser("#!Vim\n#!sh\n#!/bin/sh\n1234"), "sh", "#!/bin/sh\n1234", false)
    test(InterpreterVim.Parser("#!Vim\n#!sh\n#!/bin/sh\n1234\na\n\nb\n\nc"), "sh", "#!/bin/sh\n1234\na\n\nb\n\nc", false)
  }
}
