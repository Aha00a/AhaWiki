package logics

import javax.inject._
import play.api.Configuration

object ApplicationConf {
  // Whitelisted whatever the configuration says. Both families, and the long spelling of the
  // IPv6 one, because that is how the address arrives on a current OS.
  val AlwaysWhitelisted: Seq[String] = Seq("127.0.0.1", "::1", "0:0:0:0:0:0:0:1")
}

@Singleton
class ApplicationConf @Inject()(configuration: Configuration) {
  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    val classPart = ste.getClassName.replace(getClass.getName, "").replaceFirst("^\\$", "")
    (classPart + "." + ste.getMethodName).replaceAll("\\$", ".").replaceAll("\\.\\.", ".")
  }

  object AhaWiki {
    // The configured list is added to the loopback addresses, not substituted for them. A site
    // conf that sets this key replaces whatever base.conf held -- HOCON concatenates objects but
    // not lists -- so naming an operator's address there used to drop the server's own, silently.
    // The deploy health check then had no way past IpRateLimiter and banned itself; see
    // AhariseDevOps ahawiki/README.md, 2026-09-04.
    def ipWhitelist(): Seq[String] =
      ApplicationConf.AlwaysWhitelisted ++ configuration.getOptional[Seq[String]](fqn).getOrElse(Seq.empty)

    object accessLog {
      def sampleRate(): Double = configuration.getOptional[Double](fqn).getOrElse(0.5)
    }

    object google {
      object credentials {
        object oAuth {
          def clientId(): String = configuration.getOptional[String](fqn).getOrElse("")
          def clientSecret(): String = configuration.getOptional[String](fqn).getOrElse("")
        }
        object api {
          object Geocoding {
            def key(): String = configuration.getOptional[String](fqn).getOrElse("")
          }
          object MapsJavaScriptAPI {
            def key(): String = configuration.getOptional[String](fqn).getOrElse("")
          }
          object GoogleSheetsAPI {
            def key(): String = configuration.getOptional[String](fqn).getOrElse("")
          }
        }
      }
      object AdSense {
        def adClient(): String = configuration.getOptional[String](fqn).getOrElse("")
        def adsTxtContent(): String = configuration.getOptional[String](fqn).getOrElse("")
      }
      object reCAPTCHA {
        def siteKey(): String = configuration.getOptional[String](fqn).getOrElse("")
        def secretKey(): String = configuration.getOptional[String](fqn).getOrElse("")
      }
    }

    object aws {
      def AWS_REGION(): String = configuration.getOptional[String](fqn).getOrElse("")
      def AWS_ACCESS_KEY_ID(): String = configuration.getOptional[String](fqn).getOrElse("")
      def AWS_SECRET_ACCESS_KEY(): String = configuration.getOptional[String](fqn).getOrElse("")
      object s3 {
        def bucket(): String = configuration.getOptional[String](fqn).getOrElse("")
      }
    }

    object telegram {
      def botToken(): String = configuration.getOptional[String](fqn).getOrElse("")
      def chatId(): String = configuration.getOptional[String](fqn).getOrElse("")
    }
  }
}
