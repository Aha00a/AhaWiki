package services

import java.io.File
import java.util.Date

import actors.ActorAhaWiki.Calculate
import actors.ActorAhaWiki.CalculateCosineSimilarity
import actors.ActorAhaWiki.CalculateLink
import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import javax.inject._
import models.tables.Page
import play.api.Logging
import play.api.db.Database
import play.api.inject.ApplicationLifecycle

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.Codec

class ApplicationLifecycleHook @Inject()(implicit
                                         applicationLifecycle: ApplicationLifecycle,
                                         actorSystem: ActorSystem,
                                         executionContext: ExecutionContext,
                                         database: Database,
                                         @Named("db-actor") actorAhaWiki: ActorRef
                                        ) extends Logging {

  import java.sql.Connection

  applicationLifecycle.addStopHook { () =>
    logger.info("OnApplicationStop")
    Future.successful(())
  }

  logger.info("OnApplicationStart")
  actorSystem.scheduler.scheduleOnce(2 second, () => {
    database.withConnection { implicit connection =>
      logger.info("OnApplicationStarting")

      insertSeedPages()

      logger.info("OnApplicationStarted")
    }
  })

  private def insertSeedPages()(implicit connection: Connection): Unit = {
    if (0 == Page.selectCount()) {
      import models.tables.Page
      def getArrayPageFromFile: Array[Page] = {
        new File("app/assets/Page").listFiles().map(file => {
          val name = file.getName
          implicit val codec: Codec = Codec.UTF8
          val body = Using(scala.io.Source.fromFile(file))(_.mkString)
          Page(name, 1, new Date(), "AhaWiki", "127.0.0.1", "initial", "", body)
        })
      }

      getArrayPageFromFile.foreach(p => {
        Page.insert(p)
        actorAhaWiki ! Calculate(p.name)
      })
    }
  }

  actorSystem.scheduler.scheduleWithFixedDelay(30 seconds, 5 minutes)(() => {
    database.withConnection { implicit connection =>
      Page.pageSelectNameWhereNoCosineSimilarity() match {
        case Some(s) => actorAhaWiki ! CalculateCosineSimilarity(s)
        case None => logger.info("None")
      }
    }
  })

  actorSystem.scheduler.scheduleWithFixedDelay(15 seconds, 30 seconds)(() => {
    database.withConnection { implicit connection =>
      val seq: Seq[String] = Page.pageSelectNameWhereNoLinkSrc()
      for ((v, i) <- seq.zipWithIndex) {
        actorAhaWiki ! CalculateLink(v, i, seq.length)
      }
    }
  })
}
