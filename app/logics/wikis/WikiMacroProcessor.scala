package logics.wikis

import logics.wikis.macros._
import models.WikiContext
import utils.UuidUtil

import scala.collection.mutable

class WikiMacroProcessor() extends ExtractApply {
  val mapVariable = new mutable.HashMap[String, String]()

  def extract(s: String)(implicit wikiContext:WikiContext):String = {
    val regex = """\[\[(\w+)(?:\(([^)]+)\))?\]\]""".r
    regex.replaceAllIn(s, _ match {
      case regex(name, argument) =>
        val uuid = UuidUtil.newString
        map.put(uuid, execute(name, argument))
        uuid
      case _ => "error"
    })
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



