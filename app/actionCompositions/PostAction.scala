package actionCompositions

import play.api.mvc._

import scala.concurrent.Future

object PostAction extends ActionBuilder[Request] {
  def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] = {
    // authentication code here
    block(request)
  }
}
