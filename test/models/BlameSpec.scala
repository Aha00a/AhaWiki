package models

import org.scalatest.freespec.AnyFreeSpec

//noinspection ZeroIndexToHead,DuplicatedCode
class BlameSpec extends AnyFreeSpec {
  import com.aha00a.commons.Implicits._

  "blame1" in {
    class MetaData(val revision: Int)
    assert(new Blame().size === 0)

    val blame1 = new Blame().next(new MetaData(1), "A".splitLinesSeq())
    assert(blame1.size === 1)
    assert(blame1.seqBlameLine(0).metaData.revision === 1)
    assert(blame1.seqBlameLine(0).item === "A")

    val blame2 = blame1.next(new MetaData(2), "B".splitLinesSeq())
    assert(blame2.size === 1)
    assert(blame2.seqBlameLine(0).metaData.revision === 2)
    assert(blame2.seqBlameLine(0).item === "B")

    val blame3 = blame2.next(new MetaData(3), "a\nb\nc\nd\ne".splitLinesSeq())
    assert(blame3.size === 5)
    assert(blame3.seqBlameLine(0).metaData.revision === 3)
    assert(blame3.seqBlameLine(0).item === "a")
    assert(blame3.seqBlameLine(1).metaData.revision === 3)
    assert(blame3.seqBlameLine(1).item === "b")
    assert(blame3.seqBlameLine(2).metaData.revision === 3)
    assert(blame3.seqBlameLine(2).item === "c")
    assert(blame3.seqBlameLine(3).metaData.revision === 3)
    assert(blame3.seqBlameLine(3).item === "d")
    assert(blame3.seqBlameLine(4).metaData.revision === 3)
    assert(blame3.seqBlameLine(4).item === "e")

    val blame4 = blame3.next(new MetaData(4), "a\nb\nd\ne".splitLinesSeq())
    assert(blame4.size === 4)
    assert(blame4.seqBlameLine(0).metaData.revision === 3)
    assert(blame4.seqBlameLine(0).item === "a")
    assert(blame4.seqBlameLine(1).metaData.revision === 3)
    assert(blame4.seqBlameLine(1).item === "b")
    assert(blame4.seqBlameLine(2).metaData.revision === 3)
    assert(blame4.seqBlameLine(2).item === "d")
    assert(blame4.seqBlameLine(3).metaData.revision === 3)
    assert(blame4.seqBlameLine(3).item === "e")
  }

  "blame2" in {
    class MetaData(val revision: Int)
    assert(new Blame().size === 0)

    val blame1 = new Blame().next(new MetaData(1), "1\n1\n1\n2\n2\n2\n2\n1\n1\n1".splitLinesSeq())
    val blame2 = blame1.next(new MetaData(2), "1\n1\n2\n2\n1\n1".splitLinesSeq())
    assert(blame2.size === 6)
    assert(blame2.seqBlameLine(0).metaData.revision === 1)
    assert(blame2.seqBlameLine(0).item === "1")
    assert(blame2.seqBlameLine(1).metaData.revision === 1)
    assert(blame2.seqBlameLine(1).item === "1")
    assert(blame2.seqBlameLine(2).metaData.revision === 1)
    assert(blame2.seqBlameLine(2).item === "2")
    assert(blame2.seqBlameLine(3).metaData.revision === 1)
    assert(blame2.seqBlameLine(3).item === "2")
    assert(blame2.seqBlameLine(4).metaData.revision === 1)
    assert(blame2.seqBlameLine(4).item === "1")
    assert(blame2.seqBlameLine(5).metaData.revision === 1)
    assert(blame2.seqBlameLine(5).item === "1")
  }

}
