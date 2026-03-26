package logics

import javax.inject._
import play.api.Configuration

@Singleton
class ApplicationConf @Inject()(configuration: Configuration) {
  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    val classPart = ste.getClassName.replace(getClass.getName, "").replaceFirst("^\\$", "")
    (classPart + "." + ste.getMethodName).replaceAll("\\$", ".").replaceAll("\\.\\.", ".")
  }

  object AhaWiki {
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
  }
}
