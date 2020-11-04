package logics

import play.api.Configuration

// TODO: fix to use models.tables.Config

object ApplicationConf {
  def apply()(implicit configuration: Configuration) = new ApplicationConf(configuration)
}

class ApplicationConf(configuration: Configuration) {
  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    (ste.getClassName.replace(ApplicationConf.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
  }

  object AhaWiki {
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
  }
}
