import play.api.http.HttpFilters
import play.filters.csrf.CSRFFilter
import javax.inject.Inject

import filters.AccessLog

class Filters @Inject() (csrfFilter: CSRFFilter, accessLog: AccessLog) extends HttpFilters {
  def filters = Seq(csrfFilter, accessLog)
}
