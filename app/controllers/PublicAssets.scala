package controllers

import logics.PublicAsset
import play.api.mvc._

import javax.inject.Inject
import scala.concurrent.ExecutionContext

/** /public/, served by Play's Assets, with one exception: a `v` that is not the digest of the file
  * this instance has.
  *
  * While a deploy has instances on different releases a page drawn by the new one names
  * `?v=<new digest>`, and the proxy may send that request to the old one. Until 2026-10-11 that
  * lasted the whole canary comparison; with one instance taking readers at a time it is down to
  * the requests in flight at the hand-over (Dev Deploying). The old instance ignores the query and sends the old file, and the browser would keep
  * the old file under the new address for the full max-age, past the end of the deploy. Marked
  * `no-cache` instead, the browser asks again on the next page and gets the right file once both
  * instances have it. The same holds the other way round, an old page's `v` answered by the new
  * instance.
  *
  * Only an instance running this code can do it, so it protects the deploys after the one that
  * brought it. The wiki page Dev Deploying has the rest of the digest scheme. */
class PublicAssets @Inject()(assets: Assets, cc: ControllerComponents)(implicit ec: ExecutionContext)
  extends AbstractController(cc) {

  def at(file: String): Action[AnyContent] = Action.async { request =>
    assets.at("/public", file)(request).map { result =>
      request.getQueryString("v") match {
        case Some(v) if !PublicAsset.versionOf(file).contains(v) => result.withHeaders(CACHE_CONTROL -> "no-cache")
        case _ => result
      }
    }
  }
}
