package logics

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.Configuration

// The whitelist is the only way past IpDeny, UriAttackDetector and IpRateLimiter, and the
// server has to be able to reach itself: the deploy restarts an instance and then polls it
// until it answers. When a site conf set AhaWiki.ipWhitelist to the operator's addresses it
// replaced the list instead of extending it, loopback went with it, and the health check was
// banned by the rate limiter it was no longer exempt from. These pin the merge so that cannot
// happen again from configuration alone.
class ApplicationConfSpec extends AnyFreeSpec with Matchers {
  private def whitelistOf(values: (String, Any)*): Seq[String] =
    new ApplicationConf(Configuration.from(values.toMap)).AhaWiki.ipWhitelist()

  "ipWhitelist" - {
    "keeps loopback when a site conf names other addresses" in {
      val whitelist = whitelistOf("AhaWiki.ipWhitelist" -> Seq("203.0.113.9"))
      whitelist must contain("127.0.0.1")
      whitelist must contain("::1")
      whitelist must contain("0:0:0:0:0:0:0:1")
      whitelist must contain("203.0.113.9")
    }

    "keeps loopback when the key is absent" in {
      val whitelist = whitelistOf()
      whitelist must contain("127.0.0.1")
      whitelist must contain("::1")
      whitelist must contain("0:0:0:0:0:0:0:1")
    }

    "keeps loopback when the key is set to an empty list" in {
      whitelistOf("AhaWiki.ipWhitelist" -> Seq.empty[String]) must contain("127.0.0.1")
    }

    "does not whitelist an address nobody configured" in {
      whitelistOf().contains("203.0.113.9") mustBe false
      whitelistOf("AhaWiki.ipWhitelist" -> Seq("203.0.113.9")).contains("198.51.100.4") mustBe false
    }
  }
}
