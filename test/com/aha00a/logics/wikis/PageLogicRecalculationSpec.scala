package logics.wikis

import models.tables.PageMeta
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDateTime

/** A view recalculates a page only when its calculation has aged or fallen behind the page.
  * PageLogic.ViewRecalculationInterval says what that leaves stale and why a day.
  */
class PageLogicRecalculationSpec extends AnyFreeSpec {
  private val now = LocalDateTime.of(2026, 10, 12, 7, 0)
  private def meta(revision: Long, updated: Option[LocalDateTime]): Option[PageMeta] =
    Some(PageMeta("Page", revision, now.minusDays(30), updated, None, None, 10))
  private def recent(m: Option[PageMeta], last: Option[Long]): Boolean = PageLogic.isCalculatedRecently(m, last, now)

  "a calculation of the latest revision" - {
    "within the interval is recent" in {
      assert(recent(meta(3, Some(now.minusHours(23))), Some(3)))
    }
    "older than the interval is not" in {
      assert(!recent(meta(3, Some(now.minusHours(25))), Some(3)))
    }
    "with no update time is not" in {
      assert(!recent(meta(3, None), Some(3)))
    }
  }

  "a calculation behind the page is not recent, however new" in {
    assert(!recent(meta(2, Some(now.minusMinutes(1))), Some(3)))
  }

  "a page never calculated is not recent" in {
    assert(!recent(None, Some(1)))
  }

  "a page gone since is not recent, so the calculation runs and finds nothing to do" in {
    assert(!recent(meta(3, Some(now.minusMinutes(1))), None))
  }
}
