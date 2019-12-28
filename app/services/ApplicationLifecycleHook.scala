package services

import java.io.File
import java.util.Date

import actors.ActorAhaWiki.Calculate
import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{DateTimeUtil, Using}
import javax.inject._
import models.AhaWikiQuery
import models.Page
import play.api.Logger
import play.api.db.Database
import play.api.inject.ApplicationLifecycle

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Codec

@Singleton
class ApplicationLifecycleHook @Inject()(implicit
                                         applicationLifecycle: ApplicationLifecycle,
                                         actorSystem: ActorSystem,
                                         executionContext: ExecutionContext,
                                         database: Database,
                                         @Named("db-actor") actorAhaWiki: ActorRef
                                        ) {
  applicationLifecycle.addStopHook { () =>
    Logger.info("OnApplicationStop")
    Future.successful(())
  }

  Logger.info("OnApplicationStart")

  actorSystem.scheduler.scheduleOnce(2 second, () => { database.withConnection { implicit connection =>
    Logger.info("OnApplicationStarted")
    if (0 == AhaWikiQuery().Page.selectCount()) {
      def getArrayPageFromFile: Array[Page] = {
        new File("app/assets/Page").listFiles().map(file => {
          val name = file.getName
          implicit val codec: Codec = Codec.UTF8
          val body = Using(scala.io.Source.fromFile(file))(_.mkString)
          Page(name, 1, new Date(), "AhaWiki", "127.0.0.1", body, "initial")
        })
      }

      getArrayPageFromFile.foreach(p => {
        AhaWikiQuery().pageInsert(p.name, p.revision, p.dateTime, p.author, p.remoteAddress, p.content, p.comment)
        actorAhaWiki ! Calculate(p.name)
      })
    }
  }})

  //noinspection LanguageFeature
  actorSystem.scheduler.schedule(3 seconds, 3 minutes, () => { database.withConnection { implicit connection =>
    AhaWikiQuery().pageSelectNameWhereNoCosineSimilarity() match {
      case Some(s) => actorAhaWiki ! Calculate(s)
      case None => Logger.info("None")
    }
  }})
}