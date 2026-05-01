package logics.wikis.interpreters

import org.scalatest.freespec.AnyFreeSpec
import provider.EmptyContextWikiPage

class InterpreterWikiSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "toHtmlString" - {
    "supports heading id and class attributes" in {
      val html = InterpreterWiki.toHtmlString("= Title = #custom-id .hero .compact")

      assert(html.contains("""<div class="custom-id">"""))
      assert(html.contains("""<h1 id="custom-id" class="hero compact">"""))
      assert(html.contains("""href="#custom-id"""))
    }

    "supports compact heading attribute formats" in {
      val html1 = InterpreterWiki.toHtmlString("== AA == #id")
      val html2 = InterpreterWiki.toHtmlString("== AA == #id.class")
      val html3 = InterpreterWiki.toHtmlString("== AA == .class#id")
      val html4 = InterpreterWiki.toHtmlString("== AA == #id.class1.class2")

      assert(html1.contains("""<div class="id">"""))
      assert(html1.contains("""<h2 id="id" class="">"""))

      assert(html2.contains("""<div class="id">"""))
      assert(html2.contains("""<h2 id="id" class="class">"""))

      assert(html3.contains("""<div class="id">"""))
      assert(html3.contains("""<h2 id="id" class="class">"""))

      assert(html4.contains("""<div class="id">"""))
      assert(html4.contains("""<h2 id="id" class="class1 class2">"""))
    }

    "falls back to generated id when only class attributes are provided" in {
      val html = InterpreterWiki.toHtmlString("= Hello World = .hero")

      assert(html.contains("""<div class="Hello-World">"""))
      assert(html.contains("""<h1 id="Hello-World" class="hero">"""))
      assert(html.contains("""href="#Hello-World"""))
    }
  }
}
