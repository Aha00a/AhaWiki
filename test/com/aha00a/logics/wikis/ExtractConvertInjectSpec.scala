package com.aha00a.logics.wikis

import logics.wikis.ExtractConvertInject
import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable

/** `inject` puts each converted block back where its key was. It used to call `String.replace` once
  * per key; it now finds every key's places and builds the result once. These are the cases where
  * the two could have parted. */
class ExtractConvertInjectSpec extends AnyFreeSpec {
  private implicit val wikiContext: ContextWikiPage = null

  private class Upper(keys: String*) extends ExtractConvertInject {
    private val nextKey = mutable.Queue(keys: _*)
    val converted: mutable.Buffer[String] = mutable.Buffer.empty
    override def getUniqueKey: String = nextKey.dequeue()
    override def extract(s: String): String = extractByMarkers(s)
    override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = { converted += s; s.toUpperCase }
  }

  "puts every block back in place, every occurrence of its key" in {
    val e = new Upper("K1", "K2")
    val extracted = e.extract("a [[[x]]] b [[[y]]] c")
    assert(extracted === "a K1 b K2 c")
    assert(e.inject(extracted + " K1") === "a X b Y c X")
  }

  "converts every block in order, even one whose key is no longer in the text" in {
    val e = new Upper("K1", "K2")
    e.extract("[[[x]]] [[[y]]]")
    assert(e.inject("only K2 is left") === "only Y is left")
    assert(e.converted === Seq("x", "y"))
  }

  // A later block can hold an earlier key: a code span written inside a ``double`` one is pulled
  // first, and the single one around it then takes its key along. Replacing key by key had already
  // passed the earlier key when the later text went in, so the earlier key stayed as written.
  "leaves an earlier key that arrives inside a later block's text" in {
    val e = new Upper()
    e.arrayBuffer += "K1" -> "x"
    e.arrayBuffer += "K2" -> "k1 in here"   // converts to "K1 IN HERE"
    assert(e.inject("K2") === "K1 IN HERE")
  }

  "a key listed twice keeps its first text" in {
    val e = new Upper()
    e.arrayBuffer += "K" -> "first"
    e.arrayBuffer += "K" -> "second"
    assert(e.inject("K K") === "FIRST FIRST")
  }

  "text with no blocks comes back as it was" in {
    assert(new Upper().inject("plain") === "plain")
  }
}
