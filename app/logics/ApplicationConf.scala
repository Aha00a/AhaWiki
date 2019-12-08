package logics

import play.api.Configuration
import play.api.cache.CacheApi
import play.api.db.Database

object ApplicationConf {
  def apply()(implicit configuration: Configuration) = new ApplicationConf()
  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    (ste.getClassName.replace(ApplicationConf.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
  }
}

class ApplicationConf(implicit configuration: Configuration) {
  object AhaWiki {
    import ApplicationConf.fqn
    object google {
      @Deprecated
      object api { // TODO: remove. use AhaWiki.google.credentials.oAuth
        def clientId(): String = configuration.getString(fqn).getOrElse("")
        def clientSecret(): String = configuration.getString(fqn).getOrElse("")
      }
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
        }
      }
    }
  }
}
