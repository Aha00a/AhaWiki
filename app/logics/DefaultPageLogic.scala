package logics

import java.io.File

import com.aha00a.commons.Implicits._

object DefaultPageLogic {
  def getOption(title: String): Option[String] = {
    import com.aha00a.commons.utils.DateTimeUtil

    title match {
      case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
        Option(s"[[DayHeader]]\n")
      case _ =>
        val file = new File("app/assets/Page", title)
        if(file.exists()) {
          Some(file.readAllString())
        } else {
          None
        }
    }
  }
}
