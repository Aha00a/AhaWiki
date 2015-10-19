package controllers

import java.time.LocalDateTime
import javax.inject.Inject

import logics.Cache
import models.WikiContext
import play.api.cache.CacheApi
import play.api.mvc._

class Feed @Inject()(implicit cacheApi: CacheApi) extends Controller {
  def index = atom

  def atom = rss // TODO

  def rss = Action { implicit request =>
    case class Channel(title:String, description:String, link:String, lastBuildDate:LocalDateTime, pubDate:LocalDateTime, ttl:Int) {
      def toXml = <title>{title}</title>
        <description>{description}</description>
        <link>{link}</link>
        <lastBuildDate>{lastBuildDate}</lastBuildDate>
        <pubDate>{pubDate}</pubDate>
        <ttl>{ttl}</ttl>
    }

    case class Item(title:String, description:String, link:String, pubDate:LocalDateTime) {
      def toXml = <item>
        <title>{title}</title>
        <description>{description}</description>
        <link>{link}</link>
        <pubDate>Sun, 06 Sep 2009 16:20:00 +0000</pubDate>
      </item>
    }

    implicit val wikiContext: WikiContext = WikiContext("")
    val channel = Channel("title", "description", "link", LocalDateTime.now(), LocalDateTime.now(), 180) // TODO
    val items = Cache.PageList.get.take(15).map(p => Item(p.name, p.name, p.name, p.localDateTime)) // TODO

    Ok(<rss version="2.0">
      <channel>
        {channel.toXml}
        {items.map(_.toXml)}
      </channel>
    </rss>
    )
  }
}
