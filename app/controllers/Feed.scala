package controllers

import akka.actor.ActorRef
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import models.ContextSite
import models.tables.PageWithoutContentWithSize
import models.tables.Site
import play.api.mvc._

import java.time.LocalDateTime
import javax.inject.Inject
import javax.inject.Named
import scala.xml.Elem
import scala.xml.NodeBuffer

class Feed @Inject()(
                      implicit val
                      controllerComponents: ControllerComponents,
                      database:play.api.db.Database,
                      @Named("db-actor") actorAhaWiki: ActorRef,
                      applicationConf: ApplicationConf,
                      ahaWikiCache: AhaWikiCache,
                    ) extends BaseController {
  def index: Action[AnyContent] = Action {
    Redirect(routes.Feed.atom)
  }

  // TODO: implement
  def atom: Action[AnyContent] = Action { implicit request =>
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

    implicit val site: Site = SiteLogic.get(request.host)
    implicit val contextSite: ContextSite = ContextSite()

    val seqListLatest: Seq[PageWithoutContentWithSize] = contextSite.seqPageByPermission.sortBy(_.dateTime).reverse.take(30)
    val feed = Feed(site.name, "", s"https://${request.host}", "", "urn:uuid:60a76c80-d399-11d9-b91C-0003939e0af6", seqListLatest.headOption.map(_.localDateTime).getOrElse(LocalDateTime.now())) // TODO: id
    val entries = seqListLatest.map(p => Entry(s"${p.name} - ${site.name}", s"/w/${p.name}", s"/w/${p.name}", p.name, p.localDateTime, p.name, p.name, p.nickname.getOrElse(""))) // TODO: summary, content
    Ok(
      <feed xmlns="http://www.w3.org/2005/Atom">
        {feed.toXml}
        {entries.map(_.toXml)}
      </feed>
    )
  }
}
