package services

import com.aha00a.commons.Implicits._
import logics.ApplicationConf
import logics.GeocodeGate
import models.LatLng
import models.tables.GeocodeCache
import play.api.Logging
import play.api.db.Database
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.ws.WSClient

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

/**
 * Asks the Geocoding API where an address is, and stores the answer.
 *
 * Every question a `#!Map` block has goes through `resolveAndCache`, so this is where they are
 * bounded: `GeocodeGate` says why and how. One instance, because the gate's memory is the point.
 */
@Singleton
class ServiceGeocode @Inject()(
  database: Database,
  wsClient: WSClient,
  applicationConf: ApplicationConf,
)(implicit executionContext: ExecutionContext) extends Logging {
  import ServiceGeocode._

  private val gate = new GeocodeGate(MaxLookupsPerDay, RetryUnresolvedAfter, RetryErrorAfter, () => nowMillis())

  /** The gate's clock. A member so that a spec can let a day pass. */
  protected def nowMillis(): Long = System.currentTimeMillis()

  def resolveAndCache(address: String): Unit =
    if (address.isNotNullOrEmpty) gate.decide(address) match {
      case GeocodeGate.Ask => ask(address)
      case GeocodeGate.OverBudget(true) =>
        logger.warn(s"Geocode\tthe $MaxLookupsPerDay lookups of a day are used up; an address is not looked up until one of them is a day old")
      case _ => ()
    }

  // One line a lookup, with how it ended. It was three until 2026-11-21 -- a stopwatch around a
  // call that had only been started, the whole answer as JSON, and the count of rows stored --
  // and a failure wrote its stack trace: an address the API has no place for wrote one at every
  // view of its page.
  private def ask(address: String): Unit =
    // A request that cannot even be started has to be reported too, or the address stays
    // «on its way» for as long as the process lives.
    Try(fetch(address)).fold(Future.failed, identity)
      .map(outcomeOf)
      .map {
        case Found(latLng) =>
          store(address, latLng)
          gate.resolved(address)
          logger.info(s"Geocode\t$address\tOK")
        case NoSuchPlace(status) =>
          gate.unresolved(address)
          logger.warn(s"Geocode\t$address\t$status: not asked again for $RetryUnresolvedAfter")
        case Refused(status) =>
          gate.erred(address)
          logger.warn(s"Geocode\t$address\t$status")
      }
      .recover {
        case NonFatal(t) =>
          gate.erred(address)
          logger.warn(s"Geocode\t$address\tfailed: $t")
      }

  // The two things that reach outside the process, as members so that a spec can stand in for
  // them and watch what is asked and what is stored (ServiceGeocodeSpec).
  protected def fetch(address: String): Future[JsValue] =
    wsClient
      .url("https://maps.googleapis.com/maps/api/geocode/json")
      .withQueryStringParameters(
        "address" -> address,
        "key" -> applicationConf.AhaWiki.google.credentials.api.Geocoding.key(),
      )
      .get()
      .map(_.json)

  protected def store(address: String, latLng: LatLng): Unit =
    database.withConnection { implicit connection =>
      GeocodeCache.replace(address, latLng)
    }
}

object ServiceGeocode {
  /**
   * How many addresses are looked up in any twenty-four hours, by this process.
   *
   * Sized by what a day of deliberate flooding would cost rather than by use: in the three months
   * of logs read on 2026-11-21 the whole instance had asked for two addresses. A new map of a few
   * hundred places still fills in a day; a longer sheet fills over several, a day's worth at each
   * view. The bill itself is bounded by the key's quota in the Google console, which only the key's
   * owner can see or set -- this number only bounds what the application will ask for.
   */
  val MaxLookupsPerDay: Int = 300

  /** An address the API has no place for is not worth asking about again today. */
  val RetryUnresolvedAfter: FiniteDuration = 1.day

  /** A lookup that failed for a reason of its own -- the network, a quota -- is worth another try soon. */
  val RetryErrorAfter: FiniteDuration = 10.minutes

  sealed trait Outcome
  final case class Found(latLng: LatLng) extends Outcome
  /** The request was fine and the address is the trouble: nothing is there, or it is not an address. */
  final case class NoSuchPlace(status: String) extends Outcome
  /** The request was not answered with a place for a reason that is not the address's. */
  final case class Refused(status: String) extends Outcome

  private implicit val latLngReads: Reads[LatLng] = Json.reads[LatLng]

  // The statuses that say «this address»: no result for it, or it is not a query at all.
  private val statusesOfTheAddress = Set("ZERO_RESULTS", "INVALID_REQUEST")

  /** What the API's answer means. The first result's location, when there is one, is the place. */
  def outcomeOf(json: JsValue): Outcome =
    (json \ "results" \ 0 \ "geometry" \ "location").asOpt[LatLng] match {
      case Some(latLng) => Found(latLng)
      case None =>
        val status = (json \ "status").asOpt[String].getOrElse("no status")
        // The message names the reason («You have exceeded your daily request quota…»), not the key.
        val said = (json \ "error_message").asOpt[String].map(message => s"$status ($message)").getOrElse(status)
        if (statusesOfTheAddress.contains(status)) NoSuchPlace(said) else Refused(said)
    }
}
