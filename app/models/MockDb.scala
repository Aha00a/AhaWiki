package models

import java.io.File

import models.DirectQuery.Page
import play.Play
import play.api.Logger
import utils.DateTimeUtil

import scala.io.Codec

object MockDb {
  def pageFromFile() = {
    Play.application().getFile("app/assets/Page").listFiles().map(file => {
      val name = file.getName
      implicit val codec:Codec = Codec.UTF8
      val body = scala.io.Source.fromFile(file).mkString
      Page(name, 1, DateTimeUtil.nowEpochNano, "AhaWiki", "0:0:0:0:0:0:0:1", body, Some("initial"))
    })
  }

  def readAllTextFromFile(name: String):Option[Page] = {
    return None
    val file = new File(Play.application().getFile("app/assets/Page"), name)
    if(file.exists()) {
      implicit val codec:Codec = Codec.UTF8
      Some(new Page(name, scala.io.Source.fromFile(file).mkString))
    } else {
      None
    }
  }

  def selectPage(name: String, revision: Int, pageLastRevision:Page): Option[DirectQuery.Page] = {
    if (revision == 0) {
      if(pageLastRevision == null)
        MockDb.selectPageLastRevision(name)
      else
        Some(pageLastRevision)
    } else {
      MockDb.selectPageSpecificRevision(name, revision)
    }
  }


  def selectPageFirstRevision(name: String):Option[Page] = {
    readAllTextFromFile(name).orElse(DirectQuery.pageSelectFirstRevision(name))
  }

  def selectPageLastRevision(name: String): Option[Page] = {
    Logger.info("SELECT " + name)
    readAllTextFromFile(name).orElse(DirectQuery.pageSelectLastRevision(name))
  }

  def selectPageSpecificRevision(name: String, revision:Int):Option[Page] = {
    readAllTextFromFile(name).orElse(DirectQuery.pageSelectSpecificRevision(name, revision))
  }
}
