package controllers

import java.time.LocalDateTime

import javax.inject.Inject
import logics.AhaWikiCache
import logics.wikis.PageLogic
import models.tables.PageWithoutContentWithSize
import play.api.cache.SyncCacheApi
import play.api.mvc._
import models.tables.Site

import scala.xml.{Elem, NodeBuffer}

class Feed @Inject()(
                      implicit val
                      controllerComponents: ControllerComponents,
                      database:play.api.db.Database
                    ) extends BaseController {
  def index: Action[AnyContent] = Action {
    Redirect(routes.Feed.atom())
  }

  def atom: Action[AnyContent] = Action { implicit request =>
    import models.WikiContext.Provider
    case class Feed(title:String, subtitle:String, linkSelf:String, link:String, id:String, updated:LocalDateTime) {
      def toXml: NodeBuffer =
        <title>{title}</title>
        <subtitle>{subtitle}</subtitle>
        <link href={linkSelf} rel="self" />
        <link href={link} />
        <id>{id}</id>
        <updated>{updated}</updated>
    }

    case class Entry(title:String, link:String, linkAlternate:String, id:String, updated:LocalDateTime, summary:String, content:String, author:String) {
      def linkEdit = s"$link?action=edit"
      def toXml: Elem =
        <entry>
          <title>{title}</title>
          <link href={link} />
          <link rel="alternate" type="text/html" href={linkAlternate} />
          <link rel="edit" href={linkEdit}/>
          <id>{id}</id>
          <updated>{updated}</updated>
          <summary>{summary}</summary>
          <content type="xhtml">{content}</content>
          <author>
            <name>{author}</name>
            <email>{author}</email>
          </author>
        </entry>

    }
    implicit val provider: Provider = Provider()
    implicit val site: Site = database.withConnection { implicit connection =>
      Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
    }

    val seqPageSorted: Seq[PageWithoutContentWithSize] = PageLogic.getListPageByPermission().sortBy(_.dateTime)
    val seqListLatest: Seq[PageWithoutContentWithSize] = seqPageSorted.reverse.take(30)
    val feed = Feed("title", "subtitle", "linkSelf1", "link", "urn:uuid:60a76c80-d399-11d9-b91C-0003939e0af6", seqListLatest.headOption.map(_.localDateTime).getOrElse(LocalDateTime.now())) // TODO
    val entries = seqListLatest.map(p => Entry(p.name, "/w/" + p.name, "/w/" + p.name, p.name, p.localDateTime, p.name, p.name, p.author)) // TODO
    Ok(
      <feed xmlns="http://www.w3.org/2005/Atom">
        {feed.toXml}
        {entries.map(_.toXml)}
      </feed>
    )
  }
}
