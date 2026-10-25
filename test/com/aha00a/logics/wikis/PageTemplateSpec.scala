package com.aha00a.logics.wikis

import logics.wikis.PageTemplate
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDate

class PageTemplateSpec extends AnyFreeSpec {
  "isTemplate" - {
    "is a page named under the prefix" in {
      assert(PageTemplate.isTemplate("Template:Person"))
    }

    "is not the bare prefix, and not a page that merely contains it" in {
      assert(!PageTemplate.isTemplate("Template:"))
      assert(!PageTemplate.isTemplate("My Template:Person"))
      assert(!PageTemplate.isTemplate("template:Person"))
    }
  }

  "label drops the prefix" in {
    assert(PageTemplate.label("Template:Restaurant Review") === "Restaurant Review")
  }

  "fill" - {
    val today = LocalDate.of(2026, 10, 25)

    "puts the page's name and today's date where the tokens are" in {
      assert(PageTemplate.fill("= {{PageName}}\ncreated {{Date}}", "Ada", today) === "= Ada\ncreated 2026-10-25")
    }

    "fills a token wherever it recurs" in {
      assert(PageTemplate.fill("{{PageName}} / {{PageName}}", "X", today) === "X / X")
    }

    "leaves any other {{name}} alone: it may be a variable the new page uses" in {
      assert(PageTemplate.fill("{{price}} for {{PageName}}", "X", today) === "{{price}} for X")
    }

    "does not interpret the page name: a name with a dollar sign or backslash comes through as written" in {
      assert(PageTemplate.fill("= {{PageName}}", """Cost $1 \ more""", today) === """= Cost $1 \ more""")
    }
  }
}
