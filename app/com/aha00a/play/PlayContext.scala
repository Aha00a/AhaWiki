package com.aha00a.play

import akka.actor.ActorRef
import play.api.Configuration
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc.Request

case class PlayContext()(
  implicit
  val request: Request[Any],
  val syncCacheApi: SyncCacheApi,
  val database: Database,
  val actorAhaWiki: ActorRef,
  val configuration: Configuration
)
