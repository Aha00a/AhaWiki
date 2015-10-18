package logics

import models.DirectQuery.PageNameRevisionTimeAuthorRemoteAddressSizeComment
import models.{DirectQuery, WikiContext}

import scala.concurrent.duration._

object Cache {
  def pageSelectPageList()(implicit wikiContext: WikiContext): List[PageNameRevisionTimeAuthorRemoteAddressSizeComment] = {
    //noinspection LanguageFeature
    wikiContext.cacheApi.getOrElse[List[PageNameRevisionTimeAuthorRemoteAddressSizeComment]]("pageSelectPageList", 5 minutes) {
      DirectQuery.pageSelectPageList()
    }
  }
}
