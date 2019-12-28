package models

import java.util

import com.aha00a.commons.Implicits._
import com.github.difflib.DiffUtils
import com.github.difflib.patch.{AbstractDelta, Chunk, DeltaType}

import scala.collection.JavaConversions._

case class Blame(seqBlameLine: Seq[BlameLine] = Seq()) {
  lazy val maxRevision: Long = seqBlameLine.map(_.revision).max
  def next(page:models.Page): Blame = {
    val deltas: util.List[AbstractDelta[String]] = DiffUtils.diff(seqBlameLine.map(_.line), page.content.splitLines().toSeq).getDeltas
    deltas.sortBy(-_.getSource.getPosition).foldLeft(this)((blame, delta) => {
      val source: Chunk[String] = delta.getSource
      val target: Chunk[String] = delta.getTarget
      delta.getType match {
        case DeltaType.EQUAL => blame
        case _ =>
          val sourcePosition = source.getPosition
          val targetLines = target.getLines
          val sourceSize = source.size()
          val lines = seqBlameLine.patch(sourcePosition, targetLines.map(l => BlameLine(page.revision, page.dateTime, page.author, page.comment, l)), sourceSize)
          Blame(lines)
      }
    })
  }
}