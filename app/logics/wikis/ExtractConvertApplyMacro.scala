package logics.wikis

import logics.wikis.macros._
import models.WikiContext

import scala.collection.mutable
import scala.util.matching.Regex.Match

class ExtractConvertApplyMacro() extends ExtractConvertApply {
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

  override def convert(s: String)(implicit wikiContext: WikiContext): String = {
    val in: Option[Match] = regex.findFirstMatchIn(s)
    in match {
      case Some(m) => execute(m.group(1), m.group(2))
      case _ => "error"
    }
  }


  def execute(name: String, argument: String)(implicit wikiContext: WikiContext): String = {
    name match {
      case "PageOutline" => MacroPageOutline(argument)
      case "Br" => MacroBr(argument)
      case "Html" => MacroHtml(argument)
      case "Image" => MacroImage(argument)

      case "Navigation" => MacroNavigation()
      case "PageList" => MacroPageList()
      case "TitleIndex" => MacroTitleIndex()
      case "RecentChanges" => MacroRecentChanges()
      case "PageMap" => MacroPageMap()
      case "Backlinks" => MacroBacklinks()

      case "Set" => MacroSet(argument)
      case "Get" => MacroGet(argument)

      case "Embed" => MacroEmbed(argument)
      case "AhaWikiVersion" => Some(play.core.PlayVersion).map(v => s"""AhaWiki: 0.0.1, playframework: ${v.current}, sbt: ${v.sbtVersion}, scala: ${v.scalaVersion}""").getOrElse("")
      case "LinkWithPercent" => MacroLinkWithPercent(argument)
      case "Include" => MacroInclude(argument)
      case "Months" => MacroMonths(argument)
      case "Days" => MacroDays(argument)
      case "Calendar" => MacroCalendar(argument)
      case _ => "Error" + name + argument
    }
  }

  def extractLink()(implicit wikiContext: WikiContext): Seq[String] = {
    val map = Seq(MacroCalendar, MacroDays, MacroMonths).map(m => m.name -> m).toMap
    arrayBuffer.map(_._2).flatMap{
      case regex(name, argument) => map.get(name).map(_.extractLink(argument)).getOrElse(Seq())
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



