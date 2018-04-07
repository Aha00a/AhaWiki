import play.api.http.HttpFilters
import play.filters.csrf.CSRFFilter
import javax.inject.Inject

import filters.FilterAccessLog

class Filters @Inject() (csrfFilter: CSRFFilter, accessLog: FilterAccessLog) extends HttpFilters {
  def filters = Seq(csrfFilter, accessLog)
}
