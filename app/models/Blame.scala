package models

import java.util
import java.util.Date

import com.aha00a.commons.Implicits._
import com.github.difflib.DiffUtils
import com.github.difflib.patch.{AbstractDelta, Chunk, DeltaType}

import scala.collection.JavaConversions._

class Blame[MetaData](val seqBlameLine: Seq[BlameLine[MetaData]] = Seq()) {
  def size: Int = seqBlameLine.size
  def next(metaData: MetaData, seq:Seq[String]): Blame[MetaData] = {
    val deltas: util.List[AbstractDelta[String]] = DiffUtils.diff(seqBlameLine.map(_.line), seq).getDeltas
    deltas.sortBy(-_.getSource.getPosition).foldLeft(this)((blame, delta) => {
      val source: Chunk[String] = delta.getSource
      val target: Chunk[String] = delta.getTarget
      delta.getType match {
        case DeltaType.EQUAL =>
          blame
        case _ =>
          val sourcePosition = source.getPosition
          val targetLines = target.getLines
          val sourceSize = source.size()
          val lines = blame.seqBlameLine.patch(sourcePosition, targetLines.map(l => new BlameLine[MetaData](metaData, l)), sourceSize)
          new Blame(lines)
      }
    })
  }
}

