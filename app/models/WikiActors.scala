package models

import org.apache.pekko.actor.ActorRef

import javax.inject.Inject
import javax.inject.Named

class WikiActors @Inject()(
  @Named("actor-page-calculator") val pageCalculation: ActorRef,
  @Named("actor-geocode") val geocode: ActorRef,
)

object WikiActors {
  def apply(pageCalculation: ActorRef, geocode: ActorRef): WikiActors = new WikiActors(pageCalculation, geocode)
}
