package models

import java.util
import java.util.Date

import com.aha00a.commons.Implicits._
import com.github.difflib.DiffUtils
import com.github.difflib.patch.{AbstractDelta, Chunk, DeltaType}

import scala.collection.JavaConversions._

class BlameItem[MetaData, Item](val metaData:MetaData, val item:Item)

class Blame[MetaData, Item](val seqBlameLine: Seq[BlameItem[MetaData, Item]] = Seq()) {
  def size: Int = seqBlameLine.size
  def next(metaData: MetaData, seq:Seq[Item]): Blame[MetaData, Item] = {
    val deltas = DiffUtils.diff(seqBlameLine.map(_.item), seq).getDeltas
    deltas.sortBy(-_.getSource.getPosition).foldLeft(this)((blame, delta) => {
      val source = delta.getSource
      val target = delta.getTarget
      delta.getType match {
        case DeltaType.EQUAL =>
          blame
        case _ =>
          val sourcePosition = source.getPosition
          val targetLines = target.getLines
          val sourceSize = source.size()
          val lines = blame.seqBlameLine.patch(sourcePosition, targetLines.map(l => new BlameItem[MetaData, Item](metaData, l)), sourceSize)
          new Blame(lines)
      }
    })
  }
}

