package com.aha00a.tests.unit

import logics.DefaultPageLogic
import org.scalatestplus.play.PlaySpec

class CalendarSpanUnit extends PlaySpec {
  private val saved = Seq("FrontPage", "1998-03-04", "2001", "2010-07")

  "calendarYear" should {
    "read the year of a day, a month and a year, and nothing else" in {
      DefaultPageLogic.calendarYear("2026-11-08") mustBe Some(2026)
      DefaultPageLogic.calendarYear("2026-11") mustBe Some(2026)
      DefaultPageLogic.calendarYear("2026") mustBe Some(2026)
      DefaultPageLogic.calendarYear("--11-08") mustBe None
      DefaultPageLogic.calendarYear("FrontPage") mustBe None
    }
  }

  "isWithinCalendarSpan" should {
    "run from the earliest saved year to a year past this one" in {
      DefaultPageLogic.isWithinCalendarSpan("1998-01-01", saved, 2026) mustBe true
      DefaultPageLogic.isWithinCalendarSpan("2027-12-31", saved, 2026) mustBe true
      DefaultPageLogic.isWithinCalendarSpan("1997-12-31", saved, 2026) mustBe false
      DefaultPageLogic.isWithinCalendarSpan("2028", saved, 2026) mustBe false
      DefaultPageLogic.isWithinCalendarSpan("2042-07", saved, 2026) mustBe false
    }

    "reach a saved page named after a year to come, and one past it" in {
      DefaultPageLogic.isWithinCalendarSpan("2031-01-01", saved :+ "2030-05-05", 2026) mustBe true
      DefaultPageLogic.isWithinCalendarSpan("2032-01-01", saved :+ "2030-05-05", 2026) mustBe false
    }

    "hold this year and the next on a site with no dates" in {
      DefaultPageLogic.isWithinCalendarSpan("2026-01-01", Seq("FrontPage"), 2026) mustBe true
      DefaultPageLogic.isWithinCalendarSpan("2027-01-01", Seq("FrontPage"), 2026) mustBe true
      DefaultPageLogic.isWithinCalendarSpan("2025-12-31", Seq("FrontPage"), 2026) mustBe false
    }

    "leave the names without a year alone" in {
      DefaultPageLogic.isWithinCalendarSpan("--07-10", Seq(), 2026) mustBe true
      DefaultPageLogic.isWithinCalendarSpan("schema:Movie", Seq(), 2026) mustBe true
    }
  }
}
