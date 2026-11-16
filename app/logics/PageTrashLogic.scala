package logics

import models.tables.Attachment
import models.tables.DeletedPage
import models.tables.LocalTransaction
import models.tables.Page
import models.tables.Site

import java.sql.Connection
import java.time.LocalDateTime

/**
 * Deleting a page keeps it; a site admin restores it or deletes it for good (2026-11-16).
 *
 * Before, a delete removed every revision and the attachments' S3 objects, and the way back was
 * restoring the whole shared database from a backup. Now the revisions move to DeletedPage /
 * DeletedPageRevision and the attachments stay in S3 with their rows Trashed. Nothing removes a
 * kept page on a schedule -- the owner chose not to have the 30-day expiry for now -- so it stays
 * until an admin deletes it for good.
 *
 * Every way a whole page is deleted comes here: the web delete (`Wiki.delete`), API v1
 * (`ApiV1.deletePage`), and deleting the last revision of a page that has only one
 * (`Wiki.deleteLastRevision`), which is deleting the page. Deleting the last of several revisions
 * is not kept: it is an undo of one edit, and the page is still there.
 *
 * Callers hold the cache and the page calculation; this is the database and S3 part.
 */
object PageTrashLogic {
  sealed trait Failure
  case object NotFound extends Failure
  /** A page of that name exists again; restoring would mix two histories under one name. */
  case object NameTaken extends Failure
  case class AttachmentFailure(message: String) extends Failure

  private class Abort(val failure: Failure) extends RuntimeException

  /** Moves the page to the kept tables and returns the kept row's seq. */
  def trash(name: String, deletedBy: Option[Long], viaApi: Boolean, now: LocalDateTime = LocalDateTime.now())
           (implicit connection: Connection, site: Site, applicationConf: ApplicationConf): Either[Failure, Long] =
    aborting {
      LocalTransaction {
        val deletedPage = DeletedPage.insertFromPage(name, deletedBy, viaApi, now).getOrElse(throw new Abort(NotFound))
        AttachmentLogic.trashPageAttachments(site.seq, name, deletedPage).left.foreach(m => throw new Abort(AttachmentFailure(m)))
        Page.deleteWithRelatedData(name)
        deletedPage
      }
    }

  /** Writes the kept revisions back under the kept name and returns the name. */
  def restore(deletedPage: Long)(implicit connection: Connection, site: Site): Either[Failure, String] =
    DeletedPage.select(deletedPage) match {
      case None => Left(NotFound)
      case Some(kept) if Page.selectLastRevisionNumber(kept.name).isDefined => Left(NameTaken)
      case Some(kept) =>
        LocalTransaction {
          DeletedPage.copyRevisionsToPage(deletedPage)
          Attachment.restoreTrashed(deletedPage)
          DeletedPage.delete(deletedPage)
        }
        Right(kept.name)
    }

  /**
   * Deletes a kept page for good: its attachment objects from S3, then the rows. If S3 fails the
   * kept page is left as it was, to be tried again.
   */
  def purge(deletedPage: Long)(implicit connection: Connection, site: Site, applicationConf: ApplicationConf): Either[Failure, String] =
    DeletedPage.select(deletedPage) match {
      case None => Left(NotFound)
      case Some(kept) =>
        AttachmentLogic.purgeTrashedAttachments(deletedPage, kept.name) match {
          case Left(message) => Left(AttachmentFailure(message))
          case Right(_) =>
            DeletedPage.delete(deletedPage)
            Right(kept.name)
        }
    }

  private def aborting[T](f: => T): Either[Failure, T] =
    try Right(f)
    catch { case abort: Abort => Left(abort.failure) }
}
