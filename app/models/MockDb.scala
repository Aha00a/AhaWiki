package models

import java.io.File

import models.AhaWikiDatabase.Page
import play.Play
import play.api.Logger
import com.aha00a.commons.utils.DateTimeUtil

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

  //noinspection ScalaUnreachableCode
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

  def selectPage(name: String, revision: Int): Option[AhaWikiDatabase.Page] = {
    if (revision == 0) {
      MockDb.selectPageLastRevision(name)
    } else {
      MockDb.selectPageSpecificRevision(name, revision)
    }
  }


  def selectPageFirstRevision(name: String):Option[Page] = {
    readAllTextFromFile(name).orElse(AhaWikiDatabase().pageSelectFirstRevision(name))
  }

  def selectPageLastRevision(name: String): Option[Page] = {
    readAllTextFromFile(name).orElse(AhaWikiDatabase().pageSelectLastRevision(name))
  }

  def selectPageSpecificRevision(name: String, revision:Int):Option[Page] = {
    readAllTextFromFile(name).orElse(AhaWikiDatabase().pageSelectSpecificRevision(name, revision))
  }
}
