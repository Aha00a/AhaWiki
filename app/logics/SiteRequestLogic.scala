package logics

import models.tables.Permission
import models.tables.Site
import models.tables.SiteAdmin
import models.tables.SiteDomain
import models.tables.SiteRequest
import models.tables.User
import models.tables.UserEmail

import java.sql.Connection

/**
 * Asking for a wiki, and an admin answering.
 *
 * As in UserNicknameChangeLogic, every check runs before the first write: withTransaction commits
 * unless an exception escapes, so returning a Left half way through would leave half a site.
 *
 * An approval makes every row a site needs, and the site is reachable as soon as the domain cache
 * is dropped, because the name resolves through `*.parentDomain`. What that wildcard needs, and
 * why a site on a domain of its own is not this, is on the wiki page Setup.
 */
object SiteRequestLogic {

  val RecentRequestsPerUser: Int = 5
  val NameMaxLength: Int = 200
  val PurposeMaxLength: Int = 1000

  private val SubdomainMinLength = 3
  private val SubdomainMaxLength = 63
  private val SubdomainShape = """^[a-z0-9]([a-z0-9-]*[a-z0-9])?$""".r

  // Names a visitor would take for the service itself, or that mail and DNS conventions claim.
  private val ReservedSubdomains: Set[String] = Set(
    "admin", "api", "app", "assets", "auth", "blog", "cdn", "dev", "docs", "ftp", "help", "imap", "login", "mail",
    "new", "ns1", "ns2", "oauth", "pop", "root", "smtp", "staging", "static", "status", "support", "test", "wiki", "www",
  )

  sealed abstract class SiteRequestError(val message: String)

  object SiteRequestError {
    case object NameRequired extends SiteRequestError("A site name is required.")
    case object NameTooLong extends SiteRequestError(s"A site name can be at most $NameMaxLength characters.")
    case object SubdomainInvalid extends SiteRequestError(
      s"The address must be $SubdomainMinLength to $SubdomainMaxLength lowercase letters, digits or hyphens, and cannot start or end with a hyphen.")
    case object SubdomainReserved extends SiteRequestError("That address is reserved.")
    case object SubdomainTaken extends SiteRequestError("That address is already in use.")
    case object SubdomainRequested extends SiteRequestError("Someone has already requested that address.")
    case object PurposeRequired extends SiteRequestError("Please say what the wiki is for.")
    case object PurposeTooLong extends SiteRequestError(s"The purpose can be at most $PurposeMaxLength characters.")
    case object AlreadyPending extends SiteRequestError("You already have a request waiting for review.")
    case object NotConfigured extends SiteRequestError("This server does not take site requests.")
    case object NoEmail extends SiteRequestError("That user has no email address to give the site to.")
    case object NotFound extends SiteRequestError("Request not found.")
    case object NotPending extends SiteRequestError("Request is no longer pending.")
  }

  import SiteRequestError._

  def domainOf(subdomain: String, parentDomain: String): String = s"$subdomain.$parentDomain"

  def validateSubdomain(raw: String): Either[SiteRequestError, String] = {
    val subdomain = raw.trim.toLowerCase(java.util.Locale.ROOT)
    if (subdomain.length < SubdomainMinLength || subdomain.length > SubdomainMaxLength) Left(SubdomainInvalid)
    else if (SubdomainShape.findFirstIn(subdomain).isEmpty) Left(SubdomainInvalid)
    else if (ReservedSubdomains.contains(subdomain)) Left(SubdomainReserved)
    else Right(subdomain)
  }

  private def isTaken(subdomain: String, parentDomain: String)(implicit connection: Connection): Boolean =
    SiteDomain.selectByDomain(domainOf(subdomain, parentDomain)).nonEmpty || Site.selectByAbbr(subdomain).nonEmpty

  def request(userSeq: Long, rawName: String, rawSubdomain: String, isPublic: Boolean, rawPurpose: String, parentDomain: String)(implicit connection: Connection): Either[SiteRequestError, SiteRequest] = {
    val name = rawName.trim
    val purpose = rawPurpose.trim
    if (parentDomain.isEmpty) Left(NotConfigured)
    else if (name.isEmpty) Left(NameRequired)
    else if (name.length > NameMaxLength) Left(NameTooLong)
    else if (purpose.isEmpty) Left(PurposeRequired)
    else if (purpose.length > PurposeMaxLength) Left(PurposeTooLong)
    else validateSubdomain(rawSubdomain).flatMap { subdomain =>
      if (isTaken(subdomain, parentDomain)) Left(SubdomainTaken)
      else if (SiteRequest.selectPendingByUser(userSeq).nonEmpty) Left(AlreadyPending)
      else if (SiteRequest.selectPendingBySubdomain(subdomain).nonEmpty) Left(SubdomainRequested)
      else SiteRequest.insert(userSeq, name, subdomain, isPublic, purpose)
        .flatMap(SiteRequest.selectBySeq)
        .toRight(NotFound)
    }
  }

  def cancel(requestSeq: Long, userSeq: Long)(implicit connection: Connection): Either[SiteRequestError, SiteRequest] = {
    if (SiteRequest.cancelPendingByUser(requestSeq, userSeq) == 0) {
      SiteRequest.selectBySeq(requestSeq) match {
        case Some(row) if row.user == userSeq => Left(NotPending)
        case _ => Left(NotFound)
      }
    } else {
      SiteRequest.selectBySeq(requestSeq).toRight(NotFound)
    }
  }

  /**
   * Makes the site. The address is validated and checked again, because the rules can change and
   * somebody can take the name between asking and answering.
   *
   * Being a SiteAdmin does not let anyone edit pages, so the requester also gets a Permission row
   * of their own. Reading is opened to everyone only on a public site; with no such row the
   * default is denied, which is what a private site wants.
   *
   * The caller drops AhaWikiCacheMemoryDomainSite after the commit: dropped inside the transaction,
   * a request in between would load the domain list without this site and keep it.
   */
  def approve(requestSeq: Long, adminUserSeq: Long, parentDomain: String)(implicit connection: Connection): Either[SiteRequestError, SiteRequest] = {
    SiteRequest.selectBySeqForUpdate(requestSeq) match {
      case None => Left(NotFound)
      case Some(row) if !row.isPending => Left(NotPending)
      case Some(_) if parentDomain.isEmpty => Left(NotConfigured)
      case Some(row) =>
        val email = UserEmail.selectPrimaryEmailByUser(row.user).orElse(UserEmail.selectEmailsByUser(row.user).headOption)
        validateSubdomain(row.subdomain).flatMap { subdomain =>
          if (isTaken(subdomain, parentDomain)) Left(SubdomainTaken)
          else if (User.selectBySeq(row.user).isEmpty || email.isEmpty) Left(NoEmail)
          else {
            // Checks are done; from here everything writes.
            val domain = domainOf(subdomain, parentDomain)
            val siteSeq = Site.insert(row.name, subdomain, domain)
            implicit val site: Site = Site(siteSeq, row.name, subdomain, domain)
            SiteDomain.insert(siteSeq, domain)
            SiteAdmin.insert(siteSeq, row.user)
            Permission.upsert(Permission("", Permission.TargetType.All, email.get, Permission.ActorType.Exact, Permission.Action.Admin.id))
            if (row.isPublic) {
              Permission.upsert(Permission("", Permission.TargetType.All, "", Permission.ActorType.All, Permission.Action.Read.id))
            }
            SiteRequest.review(requestSeq, SiteRequest.Status.Approved, adminUserSeq, None, Some(siteSeq))
            SiteRequest.selectBySeq(requestSeq).toRight(NotFound)
          }
        }
    }
  }

  def reject(requestSeq: Long, adminUserSeq: Long, reason: Option[String])(implicit connection: Connection): Either[SiteRequestError, SiteRequest] = {
    SiteRequest.selectBySeqForUpdate(requestSeq) match {
      case None => Left(NotFound)
      case Some(row) if !row.isPending => Left(NotPending)
      case Some(_) =>
        SiteRequest.review(requestSeq, SiteRequest.Status.Rejected, adminUserSeq, reason.map(_.trim).filter(_.nonEmpty), None)
        SiteRequest.selectBySeq(requestSeq).toRight(NotFound)
    }
  }
}
