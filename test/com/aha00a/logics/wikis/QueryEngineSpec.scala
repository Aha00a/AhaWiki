package com.aha00a.logics.wikis

import logics.wikis.QueryEngine
import logics.wikis.QueryEngine.Spec
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDate

/**
 * What `#!Query` asks of the data, apart from the database and from HTML.
 *
 * The cases that matter are the ones a person would get wrong without noticing: a property with
 * two values, a date written with no year, a page that lacks the property being sorted on, and a
 * line typed wrongly that must say so instead of matching nothing.
 */
class QueryEngineSpec extends AnyFreeSpec {
  private val today = LocalDate.of(2026, 10, 24)

  private def spec(lines: Seq[String]*): Spec =
    QueryEngine.parse(Seq("Person"), lines).fold(e => fail(e), identity)

  private val facts: Map[String, Map[String, Seq[String]]] = Map(
    "Ada" -> Map("birthDate" -> Seq("1815-10-23", "1815-10"), "birthPlace" -> Seq("London"), "score" -> Seq("9")),
    "Me" -> Map("birthDate" -> Seq("--10-23"), "score" -> Seq("10")),
    "Grace" -> Map("birthDate" -> Seq("1906-12-09"), "birthPlace" -> Seq("New York"), "score" -> Seq("2")),
    "NoDate" -> Map("birthPlace" -> Seq("Nowhere")),
  )
  private val inClass = Some(facts.keySet)

  private def names(s: Spec): Seq[String] = QueryEngine.run(s, facts, inClass, today).rows.map(_.page)

  "where" - {
    "= holds when any value is equal, whatever the case" in {
      assert(names(spec(Seq("where", "birthPlace", "=", "london"))) === Seq("Ada"))
    }

    "!= holds when no value is equal, which includes a page without the property" in {
      assert(names(spec(Seq("where", "birthPlace", "!=", "London"))) === Seq("Grace", "Me", "NoDate"))
    }

    "~ is a case-insensitive contains" in {
      assert(names(spec(Seq("where", "birthPlace", "~", "york"))) === Seq("Grace"))
    }

    "has and none test only whether the property is there" in {
      assert(names(spec(Seq("where", "birthDate", "has"))) === Seq("Ada", "Grace", "Me"))
      assert(names(spec(Seq("where", "birthDate", "none"))) === Seq("NoDate"))
    }

    "md matches the same month and day in any year, or in none" in {
      assert(names(spec(Seq("where", "birthDate", "md", "--10-23"))) === Seq("Ada", "Me"))
    }

    "md today means today's month and day" in {
      assert(names(spec(Seq("where", "birthDate", "md", "today"))) === Seq.empty)
      val onTheDay = LocalDate.of(2026, 10, 23)
      assert(QueryEngine.run(spec(Seq("where", "birthDate", "md", "today")), facts, inClass, onTheDay).rows.map(_.page) === Seq("Ada", "Me"))
    }

    "compares numbers as numbers, not as text" in {
      // As text "10" < "9"; as numbers it is the other way round.
      assert(names(spec(Seq("where", "score", ">", "5"))) === Seq("Ada", "Me"))
    }

    "compares ISO dates as dates" in {
      assert(names(spec(Seq("where", "birthDate", ">=", "1900-01-01"))) === Seq("Grace"))
    }

    "all conditions must hold" in {
      assert(names(spec(Seq("where", "birthDate", "md", "--10-23"), Seq("where", "birthPlace", "has"))) === Seq("Ada"))
    }
  }

  "ordering" - {
    "is by name when no sort is given" in {
      assert(names(spec(Seq("where", "score", "has"))) === Seq("Ada", "Grace", "Me"))
    }

    "sorts numbers as numbers" in {
      assert(names(spec(Seq("where", "score", "has"), Seq("sort", "score"))) === Seq("Grace", "Ada", "Me"))
    }

    "descending reverses it" in {
      assert(names(spec(Seq("where", "score", "has"), Seq("sort", "score", "desc"))) === Seq("Me", "Ada", "Grace"))
    }

    "puts a page without the sorted property last, whichever way the order runs" in {
      val asc = names(spec(Seq("sort", "score")))
      val desc = names(spec(Seq("sort", "score", "desc")))
      assert(asc.last === "NoDate")
      assert(desc.last === "NoDate")
    }

    "breaks ties by name" in {
      val tied = Map("B" -> Map("k" -> Seq("1")), "A" -> Map("k" -> Seq("1")), "C" -> Map("k" -> Seq("0")))
      val s = spec(Seq("sort", "k", "desc"))
      assert(QueryEngine.run(s, tied, Some(tied.keySet), today).rows.map(_.page) === Seq("A", "B", "C"))
    }
  }

  "group" - {
    "keeps the pages of a group together, groups in order, pages without the property last" in {
      val s = spec(Seq("group", "birthPlace"))
      assert(names(s) === Seq("Ada", "Grace", "Me", "NoDate").sortBy(n => (n match { case "Ada" => "London"; case "Grace" => "New York"; case "NoDate" => "Nowhere"; case _ => "~~" }, n)))
    }
  }

  "limit" - {
    "cuts the rows but still reports how many matched" in {
      val r = QueryEngine.run(spec(Seq("where", "score", "has"), Seq("limit", "2")), facts, inClass, today)
      assert(r.rows.size === 2)
      assert(r.total === 3)
    }

    "is capped" in {
      assert(spec(Seq("where", "score", "has"), Seq("limit", "99999")).limit === QueryEngine.MaxLimit)
    }
  }

  "a class restricts the candidates; without one, where decides" in {
    val outsider = facts + ("Stranger" -> Map("score" -> Seq("1")))
    val s = spec(Seq("where", "score", "has"))
    assert(QueryEngine.run(s, outsider, Some(facts.keySet), today).rows.map(_.page).contains("Stranger") === false)
    assert(QueryEngine.run(s, outsider, None, today).rows.map(_.page).contains("Stranger") === true)
  }

  "props lists what the database has to be asked for, once each" in {
    val s = spec(Seq("where", "score", ">", "1"), Seq("show", "score", "birthPlace"), Seq("sort", "birthPlace"), Seq("group", "birthDate"))
    assert(s.props === Seq("score", "birthPlace", "birthDate"))
  }

  "parse" - {
    "reports a line it does not know instead of ignoring it" in {
      val r = QueryEngine.parse(Seq("Person"), Seq(Seq("shwo", "name")))
      assert(r.left.exists(_.contains("shwo")))
    }

    "reports a malformed where" in {
      assert(QueryEngine.parse(Seq("Person"), Seq(Seq("where", "score", "???", "1"))).isLeft)
      assert(QueryEngine.parse(Seq("Person"), Seq(Seq("where", "score"))).isLeft)
    }

    "reports a bad limit and a bad view" in {
      assert(QueryEngine.parse(Seq("Person"), Seq(Seq("limit", "many"))).isLeft)
      assert(QueryEngine.parse(Seq("Person"), Seq(Seq("view", "hologram"))).isLeft)
    }

    "refuses a query that would be every page" in {
      assert(QueryEngine.parse(Seq.empty, Seq(Seq("show", "score"))).isLeft)
    }

    "accepts a query with only a where, no class" in {
      assert(QueryEngine.parse(Seq.empty, Seq(Seq("where", "score", "has"))).isRight)
    }
  }
}
