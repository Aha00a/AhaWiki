package logics.wikis

import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.macros._
import models.ContextWikiPage

import scala.collection.mutable
import scala.util.matching.Regex

object ExtractConvertInjectMacro {
  private val mapMacros: Map[String, TraitMacro] = Seq(

    MacroSiteName,
    MacroUptime,

    MacroBr,
    MacroColorCode,
    MacroHtml,
    MacroImage,
    MacroAttachment,
    MacroRuby,
    MacroTrivial,
    MacroCopyable,

    MacroNavigation,
    MacroPageList,
    MacroTitleIndex,
    MacroRecentChanges,
    MacroRecentChangesList,
    MacroWikiStatistics,

    MacroAdjacentPages,
    MacroPageMap,
    MacroTwinPages,
    MacroSimilarPages,
    MacroBacklinks,
    MacroYears,

    MacroPageCount,
    MacroPercentLinkTitle,

    MacroEmbed,
    MacroInclude,

    MacroCalendar,
    MacroIncludeDays,

    MacroDayHeader,
    MacroLinkDate,
    MacroWeekdayName,
    MacroMonthName,
    MacroNavigationYearMonth,
    MacroNavigationYear,


    MacroPeriod,

    MacroError,
    MacroInfo,
    MacroSuccess
  ).map(m => m.name -> m).toMap

  val macroNames: Seq[String] = mapMacros.keys.toSeq.sorted
}

class ExtractConvertInjectMacro extends ExtractConvertInject {
  private val mapVariable = new mutable.HashMap[String, String]()

  val regex: Regex =
    """(?x)
      \[\[
        (\w*)
        (?:
          \(
            (.*?)
          \)
        )?
      ]]""".r

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

  override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = s match {
    case regex(name, argument) =>
      val result = ExtractConvertInjectMacro.mapMacros.get(name).map(_.toHtmlString(argument)).getOrElse {
        name match {
          case "Set" => MacroSet(argument)
          case "Get" => MacroGet(argument)
          case _ =>
            val macroErrorResult = MacroError.toHtmlString(s"$s - Macro not found.")
            wikiContext.renderingMode match {
              case RenderingMode.Normal =>
                macroErrorResult
              case RenderingMode.Preview =>
                val linkAhaWikiSyntaxMacro = InterpreterWiki.inlineToHtmlString("[https://wiki.aha00a.com/w/AhaWikiSyntaxMacro AhaWikiSyntaxMacro]")
                val macroList = InterpreterWiki.inlineToHtmlString(ExtractConvertInjectMacro.mapMacros.keys.toSeq.sorted.mkString(", "))
                val macroInfoResult = MacroInfo.toHtmlString(Seq(
                  "Available Macros",
                  macroList,
                  linkAhaWikiSyntaxMacro
                ).mkString("<br/>"))
                macroErrorResult + macroInfoResult
            }
        }
      }
      if(name == "Set") result else wrapMacro(name, result)
    case _ => "error"
  }

  private def wrapMacro(name: String, body: String): String =
    ExtractConvertInjectMacro.mapMacros.get(name).filter(_.isBlock) match {
      case Some(_) => s"""<div class="Macro BlockMacro $name">$body</div>"""
      case None => s"""<span class="Macro InlineMacro $name">$body</span>"""
    }

  def isBlockToken(token: String): Boolean = {
    arrayBuffer.find(_._1 == token).exists { case (_, raw) =>
      raw match {
        case regex(name, _) => ExtractConvertInjectMacro.mapMacros.get(name).exists(_.isBlock)
        case _ => false
      }
    }
  }

  def extractLink()(implicit wikiContext: ContextWikiPage): Seq[String] = {
    arrayBuffer.map(_._2).flatMap {
      case regex(name, argument) => ExtractConvertInjectMacro.mapMacros.get(name)
        .map(_.extractLink(argument))
        .getOrElse(Seq())
      case _ => Seq()
    }.toSeq
  }

  private def MacroSet(argument: String): String = {
    val a = argument.split(",", 2)
    mapVariable.put(a(0), a(1))
    ""
  }

  private def MacroGet(argument: String): String = {
    mapVariable.getOrElse(argument, s"Error! $argument")
  }

}
