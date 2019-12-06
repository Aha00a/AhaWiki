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
    object google {
      object api {
        def clientId()(implicit configuration: Configuration): String = configuration.getString(fqn).getOrElse("")
        def clientSecret()(implicit configuration: Configuration): String = configuration.getString(fqn).getOrElse("")
      }
    }

    object config {
      object permission {
        object default {
          def read()(implicit cacheApi: CacheApi, db:Database): String = hocon.getOrElse(fqn, "all")
          def write()(implicit cacheApi: CacheApi, db:Database): String = hocon.getOrElse(fqn, "login")
        }
      }

      object google {
        object analytics {
          def trackingId()(implicit cacheApi: CacheApi, db:Database): String = hocon.getOrElse(fqn, "")
        }
      }

      object interpreter {
        object Vim {
          def debug()(implicit cacheApi: CacheApi, db:Database): Boolean = hocon.getOrElse(fqn, default = false)
          def colorscheme()(implicit cacheApi: CacheApi, db:Database): String = hocon.getOrElse(fqn, "elflord")
        }
      }


      def hocon()(implicit cacheApi: CacheApi, db:Database): Hocon = {
        new Hocon(Cache.Config.get())
      }

      private def fqn: String = {
        val ste = Thread.currentThread.getStackTrace()(2)
        (ste.getClassName.replace(config.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
      }
    }
  }

}
