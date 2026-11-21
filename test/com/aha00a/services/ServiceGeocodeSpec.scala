package services

import models.LatLng
import org.scalatest.freespec.AnyFreeSpec
import play.api.libs.json.Json
import services.ServiceGeocode.{Found, NoSuchPlace, Refused}

/**
 * What an answer of the Geocoding API is taken to mean. Which of the three it is decides how long
 * the address is left alone (GeocodeGate): a place is stored, an address the API has no place for
 * waits a day, and an answer that is about the request waits minutes.
 */
class ServiceGeocodeSpec extends AnyFreeSpec {
  "outcomeOf" - {
    "reads the first result's location as the place" in {
      val json = Json.parse(
        """{"results": [
          |  {"geometry": {"location": {"lat": 37.5665, "lng": 126.978}}},
          |  {"geometry": {"location": {"lat": 1.0, "lng": 2.0}}}
          |], "status": "OK"}""".stripMargin)
      assert(ServiceGeocode.outcomeOf(json) === Found(LatLng(37.5665, 126.978)))
    }

    "takes no result, and a query that is not an address, as the address's own trouble" in {
      assert(ServiceGeocode.outcomeOf(Json.parse("""{"results": [], "status": "ZERO_RESULTS"}""")) === NoSuchPlace("ZERO_RESULTS"))
      assert(ServiceGeocode.outcomeOf(Json.parse("""{"results": [], "status": "INVALID_REQUEST", "error_message": "Invalid request."}"""))
        === NoSuchPlace("INVALID_REQUEST (Invalid request.)"))
    }

    "takes a quota, a refused key or an unknown error as the lookup's trouble, with what the API said" in {
      assert(ServiceGeocode.outcomeOf(Json.parse("""{"results": [], "status": "OVER_QUERY_LIMIT", "error_message": "You have exceeded your daily request quota for this API."}"""))
        === Refused("OVER_QUERY_LIMIT (You have exceeded your daily request quota for this API.)"))
      assert(ServiceGeocode.outcomeOf(Json.parse("""{"results": [], "status": "REQUEST_DENIED"}""")) === Refused("REQUEST_DENIED"))
      assert(ServiceGeocode.outcomeOf(Json.parse("""{"results": [], "status": "UNKNOWN_ERROR"}""")) === Refused("UNKNOWN_ERROR"))
    }

    "takes an answer that is not the API's shape as the lookup's trouble" in {
      assert(ServiceGeocode.outcomeOf(Json.parse("""{"unexpected": true}""")) === Refused("no status"))
      assert(ServiceGeocode.outcomeOf(Json.parse("""{"results": [{"geometry": {}}], "status": "OK"}""")) === Refused("OK"))
    }
  }

  // The service with the API and the database stood in for: `answers` says what the API says
  // about an address, and the two buffers record what was asked and what was stored. Answers
  // come back on the calling thread, so each call has ended by the time the next line runs --
  // except where a test holds the answer back itself.
  private class Watched(answers: String => scala.concurrent.Future[play.api.libs.json.JsValue])
    extends ServiceGeocode(null, null, null)(scala.concurrent.ExecutionContext.parasitic) {
    val asked = scala.collection.mutable.ArrayBuffer[String]()
    val stored = scala.collection.mutable.ArrayBuffer[(String, LatLng)]()
    var now: Long = 1000L
    def pass(d: scala.concurrent.duration.FiniteDuration): Unit = now += d.toMillis
    override protected def nowMillis(): Long = now
    override protected def fetch(address: String): scala.concurrent.Future[play.api.libs.json.JsValue] = { asked += address; answers(address) }
    override protected def store(address: String, latLng: LatLng): Unit = stored += address -> latLng
  }
  private def answered(json: String) = scala.concurrent.Future.successful(Json.parse(json))
  private val seoul = """{"results": [{"geometry": {"location": {"lat": 37.5, "lng": 127.0}}}], "status": "OK"}"""

  "resolveAndCache" - {
    "stores the place an address has" in {
      val service = new Watched(_ => answered(seoul))
      service.resolveAndCache("Seoul")
      assert(service.asked.toSeq === Seq("Seoul"))
      assert(service.stored.toSeq === Seq("Seoul" -> LatLng(37.5, 127.0)))
    }

    "asks once for an address whose answer has not come, however often the page is drawn" in {
      val held = scala.concurrent.Promise[play.api.libs.json.JsValue]()
      val service = new Watched(_ => held.future)
      (1 to 5).foreach(_ => service.resolveAndCache("Seoul"))
      assert(service.asked.toSeq === Seq("Seoul"))
      held.success(Json.parse(seoul))
      assert(service.stored.size === 1)
    }

    "does not ask again that day about an address the API has no place for" in {
      import scala.concurrent.duration._
      val service = new Watched(_ => answered("""{"results": [], "status": "ZERO_RESULTS"}"""))
      (1 to 5).foreach(_ => service.resolveAndCache("nowhere at all"))
      assert(service.asked.toSeq === Seq("nowhere at all"))
      assert(service.stored.isEmpty)
      service.pass(23.hours)
      service.resolveAndCache("nowhere at all")
      assert(service.asked.size === 1)
      service.pass(2.hours)
      service.resolveAndCache("nowhere at all")
      assert(service.asked.size === 2)
    }

    // Each of these has to be reported to the gate as a failed lookup. One that was not would
    // leave the address «on its way» -- never asked again while the process lives -- and the
    // first half of each case could not tell that apart: it is the second ask, minutes later,
    // that shows the report was made.
    "tries again in minutes, not never, when the lookup failed -- answered, failed or could not start" in {
      import scala.concurrent.duration._
      Seq[(String, String => scala.concurrent.Future[play.api.libs.json.JsValue])](
        "refused" -> (_ => answered("""{"results": [], "status": "OVER_QUERY_LIMIT"}""")),
        "failed" -> (_ => scala.concurrent.Future.failed(new java.io.IOException("no route"))),
        "could not start" -> (_ => throw new IllegalStateException("could not start")),
      ).foreach { case (how, answers) =>
        val service = new Watched(answers)
        (1 to 3).foreach(_ => service.resolveAndCache("Seoul"))
        assert(service.asked.size === 1, how)
        service.pass(11.minutes)
        service.resolveAndCache("Seoul")
        assert(service.asked.size === 2, how)
      }
    }

    "tries again in minutes when the place was found but could not be stored" in {
      import scala.concurrent.duration._
      val service = new Watched(_ => answered(seoul)) {
        override protected def store(address: String, latLng: LatLng): Unit = throw new java.sql.SQLException("no connection")
      }
      service.resolveAndCache("Seoul")
      service.resolveAndCache("Seoul")
      assert(service.asked.size === 1)
      service.pass(11.minutes)
      service.resolveAndCache("Seoul")
      assert(service.asked.size === 2)
    }

    "asks for no more than the day's budget, whatever it is shown" in {
      val service = new Watched(_ => answered(seoul))
      (1 to 5000).foreach(i => service.resolveAndCache(s"made up address $i"))
      assert(service.asked.size === ServiceGeocode.MaxLookupsPerDay)
    }

    "ignores an empty address" in {
      val service = new Watched(_ => answered(seoul))
      service.resolveAndCache("")
      service.resolveAndCache(null)
      assert(service.asked.isEmpty)
    }
  }

  "the day's budget and the two waits are the ones the wiki page states" in {
    import scala.concurrent.duration._
    assert(ServiceGeocode.MaxLookupsPerDay === 300)
    assert(ServiceGeocode.RetryUnresolvedAfter === 1.day)
    assert(ServiceGeocode.RetryErrorAfter === 10.minutes)
  }
}
