package actionCompositions

import play.api.mvc._
import play.filters.csrf._

import scala.concurrent.Future

object GetAction extends ActionBuilder[Request] {
  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) = {
    // authentication code here
    block(request)
  }
  override def composeAction[A](action: Action[A]) = CSRFAddToken(action)
}
