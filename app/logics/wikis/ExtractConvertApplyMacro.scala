package logics.wikis

import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.macros._
import models.WikiContext

import scala.collection.mutable
import scala.util.matching.Regex

object ExtractConvertApplyMacro {
  val mapMacros: Map[String, TraitMacro] = Seq(

    MacroPageOutline,

    MacroBr,
    MacroHtml,
    MacroImage,
    MacroRuby,

    MacroNavigation,
    MacroPageList,
    MacroTitleIndex,
    MacroRecentChanges,
    MacroRecentChangesList,
    MacroPageMap,
    MacroBacklinks,

    MacroPageCount,
    MacroPercentLinkTitle,

    MacroEmbed,
    MacroInclude,


    MacroMonths,
    MacroCalendar,
    MacroIncludeDays,

    MacroDayHeader,
    MacroLinkDate,
    MacroWeekdayName,
    MacroMonthName,


    MacroPeriod,

    MacroError,
    MacroInfo,
    MacroSuccess
  ).map(m => m.name -> m).toMap
}
class ExtractConvertApplyMacro() extends ExtractConvertApply {
  val mapVariable = new mutable.HashMap[String, String]()

  val regex: Regex =
    """(?x)
      \[\[
        (\w*)
        (?:
          \(
            (.*?)
          \)
        )?
      \]\]""".r

  override def extract(s: String): String = {
    //noinspection ScalaUnusedSymbol
    regex.replaceAllIn(s, _ match {
      case a@regex(name, argument) =>
        val uniqueKey = getUniqueKey
        arrayBuffer += uniqueKey -> a.group(0)
        uniqueKey
      case _ => "error"
    })
  }

  override def convert(s: String)(implicit wikiContext: WikiContext): String = s match {
    case regex(name, argument) =>
      ExtractConvertApplyMacro.mapMacros.get(name).map(_(argument)).getOrElse {
        name match {
          case "Set" => MacroSet(argument)
          case "Get" => MacroGet(argument)
          case "AhaWikiVersion" => Some(play.core.PlayVersion).map(v => s"""AhaWiki: 0.0.1, Play Framework: ${v.current}, sbt: ${v.sbtVersion}, scala: ${v.scalaVersion}""").getOrElse("")
          case _ =>
            val macroErrorResult = MacroError(s"$s - Macro not found.")
            if(wikiContext.isPreview) {
              val linkAhaWikiSyntaxMacro = InterpreterWiki.formatInline("[https://wiki.aha00a.com/w/AhaWikiSyntaxMacro AhaWikiSyntaxMacro]")
              val macroList = InterpreterWiki.formatInline(ExtractConvertApplyMacro.mapMacros.keys.toSeq.sorted.mkString(", "))
              val macroInfoResult = MacroInfo(Seq(
                "Available Macros",
                macroList,
                linkAhaWikiSyntaxMacro
              ).mkString("<br/>"))
              macroErrorResult + macroInfoResult
            } else {
              macroErrorResult
            }
        }
      }
    case _ => "error"
  }

  def extractLink()(implicit wikiContext: WikiContext): Seq[String] = {
    arrayBuffer.map(_._2).flatMap {
      case regex(name, argument) => ExtractConvertApplyMacro.mapMacros.get(name)
        .map(_.extractLink(argument))
        .getOrElse(Seq())
      case _ => Seq()
    }
  }

  def MacroSet(argument: String): String = {
    val a = argument.split(",", 2)
    mapVariable.put(a(0), a(1))
    ""
  }

  def MacroGet(argument: String): String = {
    mapVariable.getOrElse(argument, s"Error! $argument")
  }

}



