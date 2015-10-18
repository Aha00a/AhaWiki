package logics

import com.typesafe.config.{Config, ConfigFactory}

class Hocon(body: String) {
  val config: Config = ConfigFactory.parseString(body)

  def getString(path: String): Option[String] = {
    if (config.hasPath(path)) {
      Option(config.getString(path))
    } else {
      None
    }
  }
}
