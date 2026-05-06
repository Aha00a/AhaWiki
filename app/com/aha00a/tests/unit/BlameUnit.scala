package com.aha00a.tests.unit

import com.aha00a.commons.Implicits._
import com.aha00a.tests.TestUtil
import models.Blame

object BlameUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    class MetaData(val revision: Int)

    assertEquals(new Blame().size, 0)
    val blame1 = new Blame().next(new MetaData(1), "A".splitLinesSeq())
    assertEquals(blame1.size, 1)
    assertEquals(blame1.seqBlameLine(0).metaData.revision, 1)

    val blame2 = blame1.next(new MetaData(2), "B".splitLinesSeq())
    assertEquals(blame2.seqBlameLine(0).metaData.revision, 2)

    val blame3 = blame2.next(new MetaData(3), "a\nb\nc\nd\ne".splitLinesSeq())
    assertEquals(blame3.size, 5)

    val blame4 = blame3.next(new MetaData(4), "a\nb\nd\ne".splitLinesSeq())
    assertEquals(blame4.size, 4)
    assertEquals(blame4.seqBlameLine.map(_.item), Seq("a", "b", "d", "e"))

    val b1 = new Blame().next(new MetaData(1), "1\n1\n1\n2\n2\n2\n2\n1\n1\n1".splitLinesSeq())
    val b2 = b1.next(new MetaData(2), "1\n1\n2\n2\n1\n1".splitLinesSeq())
    assertEquals(b2.size, 6)
    assertEquals(b2.seqBlameLine.map(_.item), Seq("1", "1", "2", "2", "1", "1"))
  }
}
