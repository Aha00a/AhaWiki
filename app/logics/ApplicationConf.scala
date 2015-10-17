package logics

import models.MockDb
import play.api.Play.current

object ApplicationConf {
  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    (ste.getClassName.replace(ApplicationConf.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
  }

  object AhaWiki {
    object google {
      object api {
        def clientId = current.configuration.getString(fqn).getOrElse("")
        def clientSecret = current.configuration.getString(fqn).getOrElse("")
      }
    }

    object config {
      object permission {
        object default {
          def read = hocon.getString(fqn).getOrElse("all")
          def write = hocon.getString(fqn).getOrElse("login")
        }
      }

      object google{
        object analytics {
          def trackingId = hocon.getString(fqn).getOrElse("")
        }
      }


      def hocon: Hocon = {
        new Hocon(MockDb.selectPageLastRevision(".config").map(_.content).getOrElse(""))
      }

      private def fqn: String = {
        val ste = Thread.currentThread.getStackTrace()(2)
        (ste.getClassName.replace(config.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
      }
    }
  }

}
