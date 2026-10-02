package models

import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteThemeLogic
import logics.wikis.PageLogic
import logics.wikis.RenderingMode.RenderingMode
import models.tables.Config
import models.tables.Site
import play.api.db.Database
import play.api.db.TransactionIsolationLevel
import play.api.mvc.Request

import java.sql.Connection
import java.time.LocalDate
import javax.sql.DataSource

object ContextSite {
  def apply()(
    implicit
    database: Database,
    wikiActors: WikiActors,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    request: Request[Any],
    site: Site,
  ): ContextSite = {
    implicit val provider: RequestWrapper = RequestWrapper()
    new ContextSite()
  }

  def empty()(
    implicit
    database: Database,
    wikiActors: WikiActors,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    site: Site,
  ): ContextSite = {
    implicit val provider: RequestWrapper = RequestWrapper.empty
    new ContextSite()
  }
}

/**
 * What holds for the whole site and the reader who asked, for the length of one request.
 *
 * Rendering an included page needs a context of its own — a different page name, one more entry
 * on the include stack — but none of the answers below change with it. `parent` is how the new
 * context says so: each value asks the context it came from before working the value out itself.
 *
 * `seqPageByPermission` is the one that matters. It fetches every page in the site from the
 * cache and runs a permission test on each, and a page holding several includes would otherwise
 * do that once per include. Consistency is the better reason: the including page and everything
 * it includes then decide what the reader may see from a single answer, not from several taken
 * moments apart.
 *
 * Delegation is written on each value rather than as a list somewhere else, so that a value
 * added here cannot be forgotten there.
 */
class ContextSite(parent: Option[ContextSite] = None)(
  implicit
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  val requestWrapper: RequestWrapper,
  val site: Site,
) extends Context {
  /**
   * `database` for code that takes a Database rather than this context -- the cache loaders do,
   * through [[tupleDatabaseSite]]. Its plain `withConnection` is [[withConnection]], so a cache
   * that has to be filled in the middle of a render is filled over the connection already held.
   * Everything else, transactions included, goes to the real database.
   */
  val databaseHeldFirst: Database = new Database {
    override def name: String = database.name
    override def dataSource: DataSource = database.dataSource
    override def url: String = database.url
    override def getConnection(): Connection = database.getConnection()
    override def getConnection(autocommit: Boolean): Connection = database.getConnection(autocommit)
    override def withConnection[A](block: Connection => A): A = ContextSite.this.withConnection(block)
    override def withConnection[A](autocommit: Boolean)(block: Connection => A): A = database.withConnection(autocommit)(block)
    override def withTransaction[A](block: Connection => A): A = database.withTransaction(block)
    override def withTransaction[A](isolationLevel: TransactionIsolationLevel)(block: Connection => A): A = database.withTransaction(isolationLevel)(block)
    override def shutdown(): Unit = database.shutdown()
  }

  implicit val tupleDatabaseSite: (Database, Site) = (databaseHeldFirst, site)

  protected var heldConnection: Option[Connection] = None

  /**
   * Says the request that built this context already holds `connection`, so that everything
   * rendered under it uses that one ([[withConnection]]) instead of asking the pool for another.
   *
   * A page view opens a connection and renders inside it, and macros used to open a second for
   * themselves. A pool of N then stops dead the moment N views are being drawn at once: each
   * holds one connection and waits for another that only the others could give back. Three times
   * in the week before 2026-10-03 a crawler opening many pages together did exactly that -- every
   * macro waited out the pool's timeout, pages took 20 to 47 seconds, and other requests got 500.
   * `SingleConnectionRenderSpec` renders a page with a pool of one to keep it from coming back.
   */
  def holding(connection: Connection): this.type = {
    heldConnection = Some(connection)
    this
  }

  /**
   * A connection for the length of `f`: the one the request holds if it said so, a new one if not.
   *
   * Use this, not `database.withConnection`, in anything that runs during a render. A held
   * connection that has since been closed -- a context that outlived the block it was built in --
   * is not used.
   */
  def withConnection[A](f: Connection => A): A =
    parent.map(_.withConnection(f)).getOrElse(
      heldConnection.filterNot(_.isClosed).map(f).getOrElse(database.withConnection(f))
    )

  lazy val setPageName: Set[String] =
    parent.map(_.setPageName).getOrElse(ahaWikiCache.PageMeta.SeqPageName.get().toSet)

  lazy val seqPageByPermission: Seq[PageLatestSummary] =
    parent.map(_.seqPageByPermission).getOrElse(withConnection { implicit connection =>
      PageLogic.getListPageByPermission()(requestWrapper, connection, this, ahaWikiCache)
    })

  lazy val seqPageNameByPermission: Seq[String] =
    parent.map(_.seqPageNameByPermission).getOrElse(seqPageByPermission.map(_.name))

  lazy val setPageNameByPermission: Set[String] =
    parent.map(_.setPageNameByPermission).getOrElse(seqPageNameByPermission.toSet)

  lazy val defaultHue: Option[Int] =
    parent.map(_.defaultHue).getOrElse(withConnection { implicit connection =>
      Config.select(SiteThemeLogic.DefaultHueKey).flatMap(c => SiteThemeLogic.parseHue(c.v))
    })

  def pageCanSee(name: String): Boolean = !setPageName.contains(name) || setPageNameByPermission.contains(name)

  def toContextWikiPage(seqName: Seq[String], renderingMode: RenderingMode): ContextWikiPage =
    contextForPage(seqName, renderingMode, LocalDate.now())

  /** A context for another page of the same site, in the same request, inheriting the above. */
  def contextForPage(seqName: Seq[String], renderingMode: RenderingMode, localDateNow: LocalDate): ContextWikiPage =
    new ContextWikiPage(seqName, renderingMode, Some(this))(
      database, wikiActors, applicationConf, ahaWikiCache, requestWrapper, site, localDateNow,
    )
}
