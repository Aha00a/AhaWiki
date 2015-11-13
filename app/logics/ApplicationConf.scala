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
        def clientId()(implicit wikiContext: WikiContext) = current.configuration.getString(fqn).getOrElse("")
        def clientSecret()(implicit wikiContext: WikiContext) = current.configuration.getString(fqn).getOrElse("")
      }
    }

    object config {
      object permission {
        object default {
          def read()(implicit wikiContext: WikiContext) = hocon.getString(fqn).getOrElse("all")
          def write()(implicit wikiContext: WikiContext) = hocon.getString(fqn).getOrElse("login")
        }
      }

      object google {
        object analytics {
          def trackingId()(implicit wikiContext: WikiContext) = hocon.getString(fqn).getOrElse("")
        }
      }

      object interpreter {
        object Vim {
          def colorscheme()(implicit wikiContext: WikiContext) = hocon.getString(fqn).getOrElse("elflord")
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
