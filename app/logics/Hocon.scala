package logics

import com.typesafe.config.{Config, ConfigFactory}

class Hocon(body: String) {
  val config: Config = ConfigFactory.parseString(body)

  def getOrElse(path:String, default:String) = {
    if (config.hasPath(path)) {
      config.getString(path)
    } else {
      default
    }
  }
  def getOrElse(path:String, default:Boolean) = {
    if (config.hasPath(path)) {
      config.getBoolean(path)
    } else {
      default
    }
  }
}
