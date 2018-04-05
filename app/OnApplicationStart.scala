import scala.concurrent.Future
import javax.inject._

import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor.{ActorRef, ActorSystem}
import models.{Database, MockDb}
import play.api.Logger
import play.api.inject.ApplicationLifecycle
import play.libs.Akka

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

//noinspection LanguageFeature
@Singleton
class OnApplicationStart @Inject()(lifecycle: ApplicationLifecycle, val system: ActorSystem) {
  val actorSimilarPage: ActorRef = system.actorOf(ActorPageProcessor.props)

  lifecycle.addStopHook { () =>
    Logger.info("OnApplicationStop")
    Future.successful(())
  }

  implicit def lambdaToRunnable(f: () => Unit): Runnable = new Runnable() { def run(): Unit = f() }

  system.scheduler.scheduleOnce(1 second, () => {
    Logger.info("OnApplicationStart")
    if (0 == Database.pageSelectCount()) {
      MockDb.pageFromFile().foreach(p => {
        Database.pageInsert(p.name, p.revision, p.time, p.author, p.remoteAddress, p.content, p.comment.getOrElse(""))
        actorSimilarPage ! Calculate(p.name)
      })
    }
  })

  system.scheduler.schedule(2 seconds, 60 minutes, () => {
    Database.pageSelectNameWhereNoCosineSimilarity() match {
      case Some(s) => actorSimilarPage ! Calculate(s)
      case None => Logger.info("None")
    }
  })
}