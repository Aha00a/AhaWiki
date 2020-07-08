package logics

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

class Hocon(body: String) {
  val config: Config = ConfigFactory.parseString(body)

  def getOrElse(path: String, default: String): String = {
    if (config.hasPath(path)) {
      config.getString(path)
    } else {
      default
    }
  }

  def getOrElse(path: String, default: Boolean): Boolean = {
    if (config.hasPath(path)) {
      config.getBoolean(path)
    } else {
      default
    }
  }
}
