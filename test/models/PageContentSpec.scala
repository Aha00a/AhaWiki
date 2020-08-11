package models

import org.scalatest.freespec.AnyFreeSpec

class PageContentSpec extends AnyFreeSpec {
  "1" in {
    val pageContent: PageContent = PageContent(
      """#!read all
        |#!write aha00a
        |#!redirect FrontPage""".stripMargin)
    assert(pageContent.read === Some("all"))
    assert(pageContent.write === Some("aha00a"))
    assert(pageContent.redirect === Some("FrontPage"))
    assert(pageContent.shebang === Seq[String]())
    assert(pageContent.content === "")
  }
  "2" in {
    val pageContent: PageContent = PageContent(
      """#!read login
        |#!write login
        |something""".stripMargin)
    assert(pageContent.read === Some("login"))
    assert(pageContent.write === Some("login"))
    assert(pageContent.redirect === None)
    assert(pageContent.shebang === List())
    assert(pageContent.interpreter === None)
    assert(pageContent.content === "something")
  }
  "3" in {
    val pageContent: PageContent = PageContent(
      """#!Wiki
        |#!Html
        |something""".stripMargin)
    assert(pageContent.read === None)
    assert(pageContent.write === None)
    assert(pageContent.redirect === None)
    assert(pageContent.shebang === List("Wiki", "Html"))
    assert(pageContent.content === "something")
  }
  "4" in {
    val pageContent: PageContent = PageContent(
      """#!Paper a b
        |#!read login
        |#!write login
        |something""".stripMargin)
    assert(pageContent.read === Some("login"))
    assert(pageContent.write === Some("login"))
    assert(pageContent.redirect === None)
    assert(pageContent.interpreter === Some("Paper"))
    assert(pageContent.content === "something")
  }
  "5" in {
    val pageContent: PageContent = PageContent(
      """#!read login
        |#!write login
        |#!Paper a b
        |something""".stripMargin)
    assert(pageContent.read === Some("login"))
    assert(pageContent.write === Some("login"))
    assert(pageContent.redirect === None)
    assert(pageContent.interpreter === Some("Paper"))
    assert(pageContent.content === "something")
  }
  "6" in {
    val pageContent: PageContent = PageContent(
      """#!WikiSyntaxPreview Text
        |some text
        |""".stripMargin)
    assert(pageContent.read === None)
    assert(pageContent.write === None)
    assert(pageContent.redirect === None)
    assert(pageContent.shebang === List("WikiSyntaxPreview", "Text"))
    assert(pageContent.content === "some text")
  }
  "7" in {
    val pageContent: PageContent = PageContent(
      """#!WikiSyntaxPreview Html
        |<h1>h1</h1>
        |<p>paragraph</p>
        |""".stripMargin)
    assert(pageContent.read === None)
    assert(pageContent.write === None)
    assert(pageContent.redirect === None)
    assert(pageContent.shebang === List("WikiSyntaxPreview", "Html"))
    assert(pageContent.content === "<h1>h1</h1>\n<p>paragraph</p>")
  }
  "8" in {
    val pageContent: PageContent = PageContent(
      """#!WikiSyntaxPreview Vim
        |#!java
        |class C {
        |    private String s = "Hello === java!";
        |}
        |""".stripMargin)
    assert(pageContent.read === None)
    assert(pageContent.write === None)
    assert(pageContent.redirect === None)
    assert(pageContent.shebang === List("WikiSyntaxPreview", "Vim", "java"))
    assert(pageContent.content === "class C {\n    private String s = \"Hello === java!\";\n}")
  }
  "9" in {
    assert("a:b:::c:".split(":", -1).toList === List("a", "b", "", "", "c", ""))
    val pageContent: PageContent = PageContent(
      """aaa
        |[[[#!Vim java
        |a
        |
        |b
        |
        |c
        |]]]
        |""".stripMargin)
    assert(pageContent.read === None)
    assert(pageContent.write === None)
    assert(pageContent.redirect === None)
    assert(pageContent.shebang === List[String]())
    assert(pageContent.content === "aaa\n[[[#!Vim java\na\n\nb\n\nc\n]]]")
  }
  "unapply" in {
    val str = "str"
    assert(PageContent.unapply(PageContent(str)) === Some(str))
  }
}