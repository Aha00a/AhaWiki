package logics.wikis

import logics.wikis.macros._
import models.WikiContext
import utils.UuidUtil

import scala.collection.mutable
import scala.util.matching.Regex.Match

class WikiMacroProcessor() extends ExtractConvertApply {
  val mapVariable = new mutable.HashMap[String, String]()

  val regex = """\[\[(\w+)(?:\(([^)]+)\))?\]\]""".r

  def extract(s: String):String = {
    regex.replaceAllIn(s, _ match {
      case a@regex(name, argument) =>
        val uuid = UuidUtil.newString
        map.put(uuid, a.group(0))
        uuid
      case _ => "error"
    })
  }

  def convert(s:String)(implicit wikiContext:WikiContext):String = {
    val in: Option[Match] = regex.findFirstMatchIn(s)
    in match {
      case Some(m) => execute(m.group(1), m.group(2))
      case _ => "error"
    }
  }


  def execute(name: String, argument: String)(implicit wikiContext:WikiContext): String = {
    name match {
      case "PageOutline" => MacroPageOutline()
      case "Br" => MacroBr()
      case "Html" => MacroHtml(argument)
      case "Image" => MacroImage(argument)

      case "Navigation" => MacroNavigation()
      case "PageList" => MacroPageList()
      case "TitleIndex" => MacroTitleIndex()
      case "RecentChanges" => MacroRecentChanges()
      case "PageMap" => MacroPageMap()

      case "Set" => MacroSet(argument)
      case "Get" => MacroGet(argument)

      case "Embed" => MacroEmbed(argument)
      case "RecentlyVisited" => MacroRecentlyVisited()
      case "AhaWikiVersion" => Some(play.core.PlayVersion).map(v => s"""AhaWiki: 0.0.1, playframework: ${v.current}, sbt: ${v.sbtVersion}, scala: ${v.scalaVersion}""").getOrElse("")
      case "LinkWithPercent" => MacroLinkWithPercent(argument)
      case "Include" => MacroInclude(argument)
      case _ => name + argument
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



