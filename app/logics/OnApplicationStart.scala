package logics

import javax.inject.{Inject, Singleton}

import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor.ActorSystem
import models.{MockDb, DirectQuery}
import play.api.{Logger, Application}
import play.libs.Akka

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


@Singleton
class OnApplicationStart @Inject()(val app: Application, system: ActorSystem) {
  val actorSimilarPage = system.actorOf(ActorPageProcessor.props)

  if(0 == DirectQuery.pageSelectCount()) {
    MockDb.pageFromFile().foreach(p => {
      DirectQuery.pageInsert(p.name, p.revision, p.time, p.author, p.remoteAddress, p.content, p.comment.getOrElse(""))
      actorSimilarPage ! Calculate(p.name)
    })
  }

  //noinspection LanguageFeature
  Akka.system().scheduler.schedule(0 seconds, 60 minutes, new Runnable {
    override def run(): Unit = {
      updateCosineSimilarity()
    }
  })

  def updateCosineSimilarity(): Unit = {
    DirectQuery.pageSelectNameWhereNoCosineSimilarity() match {
      case Some(s) => actorSimilarPage ! Calculate(s)
      case None => Logger.info("None")
    }
  }
}
