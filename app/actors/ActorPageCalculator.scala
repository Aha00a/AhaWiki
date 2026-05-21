package actors

import actors.ActorPageCalculator._
import models.WikiActors
import models.tables.Site
import org.apache.pekko.actor.Actor
import org.apache.pekko.actor.ActorRef
import play.api.Logging
import services.ServicePageCalculator

import javax.inject.Inject
import javax.inject.Named

object ActorPageCalculator {
  final case class Calculate(site: Site, name: String, i: Int = 0, length: Int = 1)
}

class ActorPageCalculator @Inject()(
  servicePageCalculator: ServicePageCalculator,
  @Named("actor-geocode") actorGeocode: ActorRef,
) extends Actor with Logging {
  override def receive: PartialFunction[Any, Unit] = {
    case Calculate(site, name, i, length) =>
      implicit val wikiActors: WikiActors = WikiActors(self, actorGeocode)
      servicePageCalculator.calculate(site, name, i, length)

    case unknown =>
      logger.error(s"Unknown PageCalculation actor message: ${unknown.getClass.getName}")
  }
}
