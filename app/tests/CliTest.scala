package tests

object CliTest extends App {

  import com.aha00a.tests.TestUtil

  run(new TestUtil(x => println(x)))

  def run(testUtil: TestUtil): Unit = {
    import models.WikiContext
    import testUtil.assertEquals


    def testParboiled(): Unit = {
      import org.parboiled2._

      import scala.util.Try

      //noinspection TypeAnnotation
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

      val triedInt: Try[Int] = new Calculator("1+1").InputLine.run()
      assertEquals(triedInt.isSuccess, true)
      assertEquals(triedInt.get, 2)
    }; testParboiled()
  }

}
