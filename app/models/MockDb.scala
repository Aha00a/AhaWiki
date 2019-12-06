package models

import java.io.File

import com.aha00a.commons.utils.{DateTimeUtil, Using}
import com.google.inject.Inject
import models.AhaWikiDatabase.Page
import play.api.db.Database

import scala.io.Codec

case class MockDb @Inject()(implicit db:Database) {
  def pageFromFile(): Array[Page] = {
    new File("app/assets/Page").listFiles().map(file => {
      val name = file.getName
      implicit val codec:Codec = Codec.UTF8
      val body = Using(scala.io.Source.fromFile(file))(_.mkString)
      Page(name, 1, DateTimeUtil.nowEpochNano, "AhaWiki", "127.0.0.1", body, Some("initial"))
    })
  }
  
  def selectPage(name: String, revision: Int): Option[AhaWikiDatabase.Page] = {
    if (revision == 0) {
      selectPageLastRevision(name)
    } else {
      selectPageSpecificRevision(name, revision)
    }
  }


  def selectPageFirstRevision(name: String):Option[Page] = {
    AhaWikiDatabase().pageSelectFirstRevision(name)
  }

  def selectPageLastRevision(name: String): Option[Page] = {
    AhaWikiDatabase().pageSelectLastRevision(name)
  }

  def selectPageSpecificRevision(name: String, revision:Int):Option[Page] = {
    AhaWikiDatabase().pageSelectSpecificRevision(name, revision)
  }
}
