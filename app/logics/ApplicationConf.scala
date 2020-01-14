package logics

import play.api.Configuration

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
          def clientId(): String = configuration.getString(fqn).getOrElse("")
          def clientSecret(): String = configuration.getString(fqn).getOrElse("")
        }
        object api {
          object Geocoding {
            def key(): String = configuration.getString(fqn).getOrElse("")
          }
          object MapsJavaScriptAPI {
            def key(): String = configuration.getString(fqn).getOrElse("")
          }
          object GoogleSheetsAPI {
            def key(): String = configuration.getString(fqn).getOrElse("")
          }
        }
      }
      object AdSense {
        def adClient(): String = configuration.getString(fqn).getOrElse("")
        def adsTxtContent(): String = configuration.getString(fqn).getOrElse("")
      }
      object reCAPTCHA {
        def siteKey(): String = configuration.getString(fqn).getOrElse("")
        def secretKey(): String = configuration.getString(fqn).getOrElse("")
      }
    }
  }
}
