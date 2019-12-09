import filters.FilterAccessLog
import javax.inject.Inject
import play.api.http.HttpFilters
import play.api.mvc.EssentialFilter
import play.filters.csrf.CSRFFilter

class Filters @Inject() (csrfFilter: CSRFFilter, accessLog: FilterAccessLog) extends HttpFilters {
  def filters: Seq[EssentialFilter] = Seq(csrfFilter, accessLog)
}
