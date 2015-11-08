package logics.wikis.macros

import logics.Cache
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext
import implicits.Implicits._

object MacroPageList {
  def apply()(implicit wikiContext: WikiContext) = {
    new InterpreterWiki().interpret(
      "[[[#!Table tsv 1\nName\tDate\tSize\tRevision\tAuthor\tRemote Address\tComment\n" +
      Cache.PageList.get().map { t =>
        Seq(
          s"'''[${t.name}]'''",
          s"${t.localDateTime.toIsoDateTimeString}",
          s"${t.size}",
          s"""[[Html(<a href="${t.name}?action=diff&after=${t.revision}">${t.revision}</a>)]]""",
          s"[${t.author}]",
          s"${t.remoteAddress}",
          s"${t.comment.getOrElse(" ")}"
        ).mkString("\t")
      }.mkString("\n") +
      "\n]]]"
    )
  }
}
