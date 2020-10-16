package controllers

import java.util.Date

import akka.actor.ActorRef
import com.aha00a.commons.Implicits._
import javax.inject._
import logics.PermissionLogic
import logics.SessionLogic
import logics.wikis.WikiPermission
import models.PageContent
import models.WikiContext
import models.tables.Permission
import play.api.Configuration
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.mvc._

class Search @Inject()(implicit val
                       controllerComponents: ControllerComponents,
                       syncCacheApi: SyncCacheApi,
                       database: play.api.db.Database,
                       @Named("db-actor") actorAhaWiki: ActorRef,
                       configuration: Configuration
                      ) extends BaseController with Logging {

  import logics.AhaWikiInjects

  implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()

  def index(q: String): Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>

    import models.WikiContext.Provider
    implicit val wikiContext: WikiContext = WikiContext("")
    implicit val provider: Provider = wikiContext.provider

    val wikiPermission = WikiPermission()
    val permissionLogic = new PermissionLogic(Permission.select())

    val id = SessionLogic.getId(request).getOrElse("")

    Ok(views.html.Search.search(
      q,
      q.toOption.map(
        models.tables.Page.pageSearch(_)
          .filter(sr => {
            val pageContent = PageContent(sr.content)
            val isReadableFromLegacy = wikiPermission.isReadable(pageContent)
            val readable = permissionLogic.permitted(sr.name, id, Permission.read)
            val editable = permissionLogic.permitted(sr.name, id, Permission.edit)
            if(isReadableFromLegacy != readable)
              logger.error(s"${sr.name}\t${isReadableFromLegacy}\t${readable}\t${wikiPermission.isWritable(pageContent)}\t${editable}")

            isReadableFromLegacy
          })
          .sortBy(_.dateTime)(Ordering[Date].reverse)
          .partition(_.name == q)
          .concat()
          .map(_.summarise(q))
      ).getOrElse(Seq.empty)
    ))
  }}
}
