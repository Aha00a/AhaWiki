import filters.FilterAccessLog
import javax.inject.Inject
import play.api.http.HttpFilters
import play.api.mvc.EssentialAction
import play.api.mvc.EssentialFilter
import play.api.mvc.RequestHeader
import play.filters.csrf.CSRFFilter

class Filters @Inject() (csrfFilter: CSRFFilter, accessLog: FilterAccessLog) extends HttpFilters {
  private def isApiV1Request(request: RequestHeader): Boolean =
    request.path.startsWith("/api/v1/")

  private val csrfExceptApiV1: EssentialFilter = new EssentialFilter {
    override def apply(next: EssentialAction): EssentialAction = {
      EssentialAction { request =>
        if (isApiV1Request(request)) next(request)
        else csrfFilter(next)(request)
      }
    }
  }

  def filters: Seq[EssentialFilter] = Seq(csrfExceptApiV1, accessLog)
}
