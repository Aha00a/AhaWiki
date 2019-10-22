package services

import javax.inject._
import actors.ActorAhaWiki
import actors.ActorAhaWiki.Calculate
import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.implicits.Implicits._
import models.{AhaWikiDatabase, MockDb}
import play.api.Logger
import play.api.db.Database
import play.api.inject.ApplicationLifecycle

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

@Singleton
class ApplicationLifecycleHook @Inject()(
  implicit applicationLifecycle: ApplicationLifecycle,
  actorSystem: ActorSystem,
  executionContext: ExecutionContext,
  db: Database,
  @Named("db-actor") actorAhaWiki: ActorRef
) {
//  val actorAhaWiki: ActorRef = actorSystem.actorOf(ActorAhaWiki.props)
  applicationLifecycle.addStopHook { () =>
    Logger.info("OnApplicationStop")
    Future.successful(())
  }

  Logger.info("OnApplicationStart")

  //noinspection LanguageFeature
  actorSystem.scheduler.scheduleOnce(2 second, () => {
    Logger.info("OnApplicationStarted")
    if (0 == AhaWikiDatabase().pageSelectCount()) {
      MockDb().pageFromFile().foreach(p => {
        AhaWikiDatabase().pageInsert(p.name, p.revision, p.time, p.author, p.remoteAddress, p.content, p.comment.getOrElse(""))
        actorAhaWiki ! Calculate(p.name)
      })
    }
  })

  //noinspection LanguageFeature
  actorSystem.scheduler.schedule(3 seconds, 60 minutes, () => {
    AhaWikiDatabase().pageSelectNameWhereNoCosineSimilarity() match {
      case Some(s) => actorAhaWiki ! Calculate(s)
      case None => Logger.info("None")
    }
  })
}