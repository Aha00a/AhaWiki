package logics

import models.WikiContext
import play.api.Play.current

object ApplicationConf {
  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    (ste.getClassName.replace(ApplicationConf.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
  }

  object AhaWiki {
    object google {
      object api {
        def clientId(): String = current.configuration.getString(fqn).getOrElse("")
        def clientSecret(): String = current.configuration.getString(fqn).getOrElse("")
      }
    }

    object config {
      object permission {
        object default {
          def read()(implicit wikiContext: WikiContext): String = hocon.getOrElse(fqn, "all")
          def write()(implicit wikiContext: WikiContext): String = hocon.getOrElse(fqn, "login")
        }
      }

      object google {
        object analytics {
          def trackingId()(implicit wikiContext: WikiContext): String = hocon.getOrElse(fqn, "")
        }
      }

      object interpreter {
        object Vim {
          def debug()(implicit wikiContext: WikiContext): Boolean = hocon.getOrElse(fqn, default = false)
          def colorscheme()(implicit wikiContext: WikiContext): String = hocon.getOrElse(fqn, "elflord")
        }
      }


      def hocon()(implicit wikiContext: WikiContext): Hocon = {
        new Hocon(Cache.Config.get())
      }

      private def fqn: String = {
        val ste = Thread.currentThread.getStackTrace()(2)
        (ste.getClassName.replace(config.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
      }
    }
  }

}
