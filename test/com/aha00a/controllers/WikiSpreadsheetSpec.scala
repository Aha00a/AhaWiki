package controllers

import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

/**
 * What the map's sync button writes into the page: the synced sheet's block refreshed, and every
 * other block exactly as it was. Google is stood in for by a function that returns a fixed table.
 */
class WikiSpreadsheetSpec extends AnyFreeSpec {
  private implicit val noContext: ContextWikiPage = null
  private val url = "https://docs.google.com/spreadsheets/d/SHEET1/edit"
  private val fetched = "Name\tAddress\nNew\tSeoul"
  private val fetch: String => String = id => { assert(id === "SHEET1"); fetched }

  private val vim = "[[[#!Vim scala\nval a = 1\n]]]"
  private val table = "[[[#!Table tsv 1\nA\tB\n1\t2\n]]]"
  private val otherMap = "[[[#!Map https://docs.google.com/spreadsheets/d/OTHER/edit Sheet1\nName\tAddress\nOld\tBusan]]]"
  private val thisMap = s"[[[#!Map $url Sheet1\nName\tAddress\nOld\tBusan]]]"

  "syncEmbeddedBlocks" - {
    "refreshes the synced sheet's block and leaves every other block as written" in {
      val page = Seq("= Places", vim, thisMap, table, otherMap, "end").mkString("\n")
      val synced = WikiSpreadsheet.syncEmbeddedBlocks(page, url, "Sheet1", fetch)
      assert(synced === Seq("= Places", vim, s"[[[#!Map $url Sheet1\n$fetched]]]", table, otherMap, "end").mkString("\n"))
    }

    // No block of this sheet: nothing changes, so the controller answers NotChanged and saves no
    // revision. Until 2026-11-21 the other blocks lost their brackets here too, which made the
    // content differ and was saved.
    "changes nothing when no block is this sheet's, however many other blocks there are" in {
      val page = Seq(vim, table, otherMap).mkString("\n")
      assert(WikiSpreadsheet.syncEmbeddedBlocks(page, url, "Sheet1", fetch) === page)
    }

    "leaves a block of this address but another sheet alone" in {
      val page = s"[[[#!Map $url Sheet2\nName\tAddress\nOld\tBusan]]]"
      assert(WikiSpreadsheet.syncEmbeddedBlocks(page, url, "Sheet1", fetch) === page)
    }

    "leaves a page with no blocks alone" in {
      assert(WikiSpreadsheet.syncEmbeddedBlocks("just text", url, "Sheet1", fetch) === "just text")
    }
  }
}
