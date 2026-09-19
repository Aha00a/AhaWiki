import filters.FilterAccessLog
import javax.inject.Inject
import play.api.http.HttpFilters
import play.api.mvc.EssentialAction
import play.api.mvc.EssentialFilter
import play.api.mvc.RequestHeader
import play.filters.csrf.CSRFFilter

// CSRF runs everywhere except the Bearer-authenticated API. Play's protectHeaders default checks
// any request carrying Cookie or Authorization, so an API key attached to a path that is not
// exempt here turns a working anonymous call into 403 — without the key ever being read, and
// under Play's generic "Unauthorized" page, which reads as a rejected key. The wiki page Dev Api
// holds the measurement and how to call the older endpoints instead.
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
