package filters

import org.scalatest.freespec.AnyFreeSpec
import play.api.Configuration
import play.api.Environment

import scala.concurrent.duration._

class FilterAccessLogSpec extends AnyFreeSpec {
  // A tarpit that outlasts the server's idle timeout ends in a reset, which the proxy counts
  // against the instance -- the reason FilterAccessLog.TarpitMax exists. Read from the
  // configuration the app starts with, so raising or lowering the timeout is checked too.
  "the longest tarpit ends before the server's idle timeout closes the connection" in {
    val idleTimeout = Configuration.load(Environment.simple()).get[Duration]("play.server.http.idleTimeout")
    assert(FilterAccessLog.TarpitMax < idleTimeout)
  }

  "tarpitDelay stays between the bounds" in {
    val delays = (1 to 1000).map(_ => FilterAccessLog.tarpitDelay())
    assert(delays.forall(d => d >= FilterAccessLog.TarpitMin && d <= FilterAccessLog.TarpitMax))
  }
}
