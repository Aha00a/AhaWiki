package actors

import actors.ActorGeocode._
import org.apache.pekko.actor.Actor
import play.api.Logging
import services.ServiceGeocode

import javax.inject.Inject

object ActorGeocode {
  final case class Geocode(address: String)
}

class ActorGeocode @Inject()(serviceGeocode: ServiceGeocode) extends Actor with Logging {
  override def receive: PartialFunction[Any, Unit] = {
    case Geocode(address) =>
      serviceGeocode.resolveAndCache(address)

    case unknown =>
      logger.error(s"Unknown Geocode actor message: ${unknown.getClass.getName}")
  }
}
