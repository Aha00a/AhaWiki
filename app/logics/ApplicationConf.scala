package logics

import play.api.Configuration
import play.api.cache.CacheApi
import play.api.db.Database

object ApplicationConf {
  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    (ste.getClassName.replace(ApplicationConf.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
  }

  object AhaWiki {
    // TODO: convert to class
    object google {
      @Deprecated
      object api { // TODO: remove. use AhaWiki.google.credentials.oAuth
        def clientId()(implicit configuration: Configuration): String = configuration.getString(fqn).getOrElse("")
        def clientSecret()(implicit configuration: Configuration): String = configuration.getString(fqn).getOrElse("")
      }
      object credentials {
        object oAuth {
          def clientId()(implicit configuration: Configuration): String = configuration.getString(fqn).getOrElse("")
          def clientSecret()(implicit configuration: Configuration): String = configuration.getString(fqn).getOrElse("")
        }
        object api {
          object Geocoding {
            def key()(implicit configuration: Configuration): String = configuration.getString(fqn).getOrElse("")
          }
          object MapsJavaScriptAPI {
            def key()(implicit configuration: Configuration): String = configuration.getString(fqn).getOrElse("")
          }
        }
      }
    }
  }
}
