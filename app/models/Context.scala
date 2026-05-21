package models

import logics.AhaWikiCache
import logics.ApplicationConf
import play.api.db.Database

class Context()(
  implicit
  val database: Database,
  val wikiActors: WikiActors,
  val applicationConf: ApplicationConf,
  val ahaWikiCache: AhaWikiCache,
) {
  def pageCalculationActor = wikiActors.pageCalculation
  def geocodeActor = wikiActors.geocode
}
