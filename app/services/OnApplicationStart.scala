package services

import javax.inject._

import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.commons.implicits.Implicits._
import models.{Database, MockDb}
import play.api.Logger
import play.api.inject.ApplicationLifecycle

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

//noinspection LanguageFeature
@Singleton
class OnApplicationStart @Inject()(applicationLifecycle: ApplicationLifecycle, actorSystem: ActorSystem) {
  val actorPageProcessor: ActorRef = actorSystem.actorOf(ActorPageProcessor.props)

  applicationLifecycle.addStopHook { () =>
    Logger.info("OnApplicationStop")
    Future.successful(())
  }

  Logger.info("OnApplicationStart")

  actorSystem.scheduler.scheduleOnce(1 second, () => {
    Logger.info("OnApplicationStarted")
    if (0 == Database.pageSelectCount()) {
      MockDb.pageFromFile().foreach(p => {
        Database.pageInsert(p.name, p.revision, p.time, p.author, p.remoteAddress, p.content, p.comment.getOrElse(""))
        actorPageProcessor ! Calculate(p.name)
      })
    }
  })

  actorSystem.scheduler.schedule(2 seconds, 60 minutes, () => {
    Database.pageSelectNameWhereNoCosineSimilarity() match {
      case Some(s) => actorPageProcessor ! Calculate(s)
      case None => Logger.info("None")
    }
  })
}