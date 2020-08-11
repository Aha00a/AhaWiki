package logics

import play.api.mvc.Request

object IdProvider {
  def createBy(request: Request[Any]): IdProvider = new IdProvider {
    override def getId: Option[String] = SessionLogic.getId(request)
  }
}
trait IdProvider {
  def getId: Option[String]
}
