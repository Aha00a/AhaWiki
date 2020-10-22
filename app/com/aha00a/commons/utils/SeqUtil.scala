package com.aha00a.commons.utils

object SeqUtil {

  def mergeOneByOne[T](seqSeq: Seq[T]*): Seq[T] = {
    (0 until seqSeq.map(_.length).max)
      .flatMap(i => seqSeq.map(k => k.lift(i)))
      .filter(_.isDefined)
      .map(_.get)
  }

}
