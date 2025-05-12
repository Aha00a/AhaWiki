package models

import akka.actor.ActorRef
import logics.ApplicationConf
import models.RequestWrapper
import play.api.db.Database

class Context()(
  implicit
  val database: Database,
  val actorAhaWiki: ActorRef,
  val applicationConf: ApplicationConf,
  val requestWrapper: RequestWrapper,
) {
}
