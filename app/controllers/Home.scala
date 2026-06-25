package controllers

import org.apache.pekko.actor.ActorSystem
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.UriUtil
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import models.ContextSite
import models.WikiActors
import models.tables.Site
import play.api.Environment

import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject._
import scala.concurrent.ExecutionContext

class Home @Inject() (
                       implicit val
                       controllerComponents: ControllerComponents,
                       actorSystem: ActorSystem,
                       database: Database,
                       environment: Environment,
                        wikiActors: WikiActors,
                       applicationConf: ApplicationConf,
                       ahaWikiCache: AhaWikiCache,
                       wsClient: WSClient,
                       executionContext: ExecutionContext
                      ) extends BaseController {
  private def xmlEscape(value: String): String =
    value
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")

  def index: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random: Action[AnyContent] = Action { implicit request =>
    implicit val site: Site = SiteLogic.get(request.host)
    val contextSite: ContextSite = ContextSite()
    val name = contextSite.seqPageByPermission.random().name
    Redirect(routes.Wiki.view(UriUtil.encodeURIComponent(name), 0, "")).flashing(request.flash)
  }

  def robotsTxt: Action[AnyContent] = Action { implicit request =>
    val robots =
      s"""User-agent: Linguee
         |Disallow: /
         |
         |User-agent: SemrushBot
         |Disallow: /
         |
         |User-agent: SemrushBot-SA
         |Disallow: /
         |
         |User-agent: *
         |Allow: /
         |Disallow: /Admin
         |Disallow: /api/
         |Disallow: /actions/
         |Disallow: /account
         |Disallow: /login
         |Disallow: /logout
         |Disallow: /ws/
         |Disallow: /dev/
         |Disallow: /preview
         |
         |Sitemap: https://${request.host}/sitemap.xml
         |""".stripMargin

    Ok(robots).as("text/plain; charset=utf-8")
  }

  def sitemapXml: Action[AnyContent] = Action { implicit request =>
    implicit val site: Site = SiteLogic.get(request.host)
    val contextSite: ContextSite = ContextSite()
    val host = xmlEscape(request.host)
    val urls = contextSite.seqPageByPermission.map { page =>
      val loc = s"https://$host/w/${UriUtil.encodeURIComponent(page.name)}"
      val lastmod = page.dateTime.toLocalDate.toString
      s"""  <url>
         |    <loc>$loc</loc>
         |    <lastmod>$lastmod</lastmod>
         |  </url>""".stripMargin
    }.mkString("\n")
    val xml =
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
         |$urls
         |</urlset>
         |""".stripMargin

    Ok(xml).as("application/xml; charset=utf-8")
  }

  def adsTxt: Action[AnyContent] = Action { implicit request =>
    applicationConf.AhaWiki.google.AdSense.adsTxtContent().toOption
      .map(s => Ok(s))
      .getOrElse(NotFound(""))
  }
}
