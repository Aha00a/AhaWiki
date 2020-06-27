package controllers

import akka.actor.{ActorRef, ActorSystem}
import com.aha00a.play.Implicits._
import javax.inject.Inject
import logics.wikis.interpreters.InterpreterVim
import play.api.cache.SyncCacheApi
import play.api.mvc._

class Dev @Inject()(
                     implicit val
                     controllerComponents: ControllerComponents,
                     syncCacheApi: SyncCacheApi,
                     system: ActorSystem,
                     database:play.api.db.Database,
                     @javax.inject.Named("db-actor") actorAhaWiki: ActorRef
                   ) extends BaseController {
  def deleteVimCache(md5:String): Action[AnyContent] = Action { implicit request =>
    val result = Redirect(request.refererOrRoot)

    if(InterpreterVim.getCacheFileHtml(InterpreterVim.getCacheDir, md5).delete())
    {
      result.flashing("success" -> "Reindex Succeed.")
    }
    else
    {
      result.flashing("error" -> "Reindex Failed")
    }
  }

  def peg = Action { implicit request =>
    import org.parboiled2._

    class Calculator(val input: ParserInput) extends Parser {
      def InputLine = rule { Expression ~ EOI }

      def Expression: Rule1[Int] = rule {
        Term ~ zeroOrMore(
          '+' ~ Term ~> ((_: Int) + _)
            | '-' ~ Term ~> ((_: Int) - _))
      }

      def Term = rule {
        Factor ~ zeroOrMore(
          '*' ~ Factor ~> ((_: Int) * _)
            | '/' ~ Factor ~> ((_: Int) / _))
      }

      def Factor = rule { Number | Parens }

      def Parens = rule { '(' ~ Expression ~ ')' }

      def Number = rule { capture(Digits) ~> (_.toInt) }

      def Digits = rule { oneOrMore(CharPredicate.Digit) }
    }

    Ok(new Calculator("1+1").InputLine.run().toString)
  }
}




