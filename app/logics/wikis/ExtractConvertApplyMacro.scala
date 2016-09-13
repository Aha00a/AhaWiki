package logics.wikis

import logics.wikis.macros._
import models.WikiContext

import scala.collection.mutable

class ExtractConvertApplyMacro() extends ExtractConvertApply {
  val mapMacros = Seq(
    MacroPageOutline,

    MacroBr,
    MacroHtml,
    MacroImage,

    MacroNavigation,
    MacroPageList,
    MacroTitleIndex,
    MacroRecentChanges,
    MacroRecentChangesList,
    MacroPageMap,
    MacroPageCount,
    MacroBacklinks,

    MacroEmbed,
    MacroLinkWithPercent,
    MacroInclude,
    MacroMonths,
    MacroDays,
    MacroCalendar,
    MacroIncludeDays,
    // TODO: MacroIncludeStartsWith

    MacroError
  ).map(m => m.name -> m).toMap

  val mapVariable = new mutable.HashMap[String, String]()

  val regex =
    """(?x)
      \[\[
        (\w+)
        (?:
          \(
            (.+?)
          \)
        )?
      \]\]""".r

  override def extract(s: String): String = {
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
      mapMacros.get(name).map(_.apply(argument)).getOrElse {
        name match {
          case "Set" => MacroSet(argument)
          case "Get" => MacroGet(argument)
          case "AhaWikiVersion" => Some(play.core.PlayVersion).map(v => s"""AhaWiki: 0.0.1, playframework: ${v.current}, sbt: ${v.sbtVersion}, scala: ${v.scalaVersion}""").getOrElse("")
          case _ => MacroError.apply(s"Macro not found. - [[$name($argument)]]")
        }
      }
    case _ => "error"
  }

  def extractLink()(implicit wikiContext: WikiContext): Seq[String] = {
    arrayBuffer.map(_._2).flatMap {
      case regex(name, argument) => mapMacros.get(name).map(_.extractLinkExistsOnly(argument)).getOrElse(Seq())
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



