package logics

import java.io.File

import com.aha00a.commons.Implicits._
import scalaz.LazyOption._

object DefaultPageLogic {
  import scalaz._

  def getOption(title: String): LazyOption[String] = {
    import com.aha00a.commons.utils.DateTimeUtil

    title match {
      case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
        lazySome(s"[[DayHeader]]\n")
      case _ =>
        val file = new File("app/assets/Page", title)
        if(file.exists()) {
          lazySome(file.readAllString())
        } else {
          lazyNone
        }
    }
  }
}
