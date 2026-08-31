package logics

import models.tables.User
import models.tables.UserNicknameChangeRequest
import models.tables.UserNicknameHistory

import java.sql.Connection

/**
 * Asking for a nickname, and an admin answering.
 *
 * A user never renames themselves — they file a request and an admin approves it. The rules
 * live here rather than in the controller so that the account page, the admin screen and the
 * specs all decide the same way.
 *
 * **Every check runs before the first write.** `Database.withTransaction` commits unless an
 * exception escapes, so returning a `Left` half way through a rename would commit the half.
 * Holding the request row with `FOR UPDATE` is what makes checking first safe: nothing else
 * can move it between the check and the write.
 */
object UserNicknameChangeLogic {

  /** Recent decisions worth showing a user on their own account page. */
  val RecentRequestsPerUser: Int = 5

  sealed trait RequestError {
    def message: String
  }

  object RequestError {
    case class Invalid(error: UserNicknamePolicy.ValidationError) extends RequestError {
      override def message: String = error.message
    }

    case object SameAsCurrent extends RequestError {
      override val message: String = "That is already your nickname."
    }

    case object NicknameTaken extends RequestError {
      override val message: String = "That nickname is already in use."
    }

    case object NicknameRequested extends RequestError {
      override val message: String = "Someone has already requested that nickname."
    }

    case object AlreadyPending extends RequestError {
      override val message: String = "You already have a request waiting for review."
    }

    case object UserNotFound extends RequestError {
      override val message: String = "User not found."
    }
  }

  sealed trait ReviewError {
    def message: String
  }

  object ReviewError {
    case object NotFound extends ReviewError {
      override val message: String = "Request not found."
    }

    case object NotPending extends ReviewError {
      override val message: String = "Request is no longer pending."
    }

    case class Invalid(error: UserNicknamePolicy.ValidationError) extends ReviewError {
      override def message: String = error.message
    }

    case object NicknameTaken extends ReviewError {
      override val message: String = "That nickname is already in use."
    }

    case object UserNotFound extends ReviewError {
      override val message: String = "User not found."
    }
  }

  /** Whether a nickname belongs to somebody other than this user. */
  private def takenByAnotherUser(nickname: String, userSeq: Long)(implicit connection: Connection): Boolean =
    User.selectByNickname(nickname).exists(_.seq != userSeq)

  def request(userSeq: Long, rawNickname: String)(implicit connection: Connection): Either[RequestError, UserNicknameChangeRequest] = {
    User.selectBySeq(userSeq) match {
      case None => Left(RequestError.UserNotFound)
      case Some(user) =>
        UserNicknamePolicy.validate(rawNickname) match {
          case Left(error) => Left(RequestError.Invalid(error))
          case Right(valid) =>
            val nickname = valid.value
            if (nickname.equalsIgnoreCase(user.nickname)) Left(RequestError.SameAsCurrent)
            else if (takenByAnotherUser(nickname, userSeq)) Left(RequestError.NicknameTaken)
            else if (UserNicknameChangeRequest.selectPendingByUser(userSeq).nonEmpty) Left(RequestError.AlreadyPending)
            else if (UserNicknameChangeRequest.selectPendingByNickname(nickname).nonEmpty) Left(RequestError.NicknameRequested)
            else UserNicknameChangeRequest.insert(userSeq, nickname, userSeq)
              .flatMap(UserNicknameChangeRequest.selectBySeq)
              .toRight(RequestError.UserNotFound)
        }
    }
  }

  /** Withdrawal, scoped to the requester so that a seq alone cannot cancel someone else's. */
  def cancel(requestSeq: Long, userSeq: Long)(implicit connection: Connection): Either[ReviewError, UserNicknameChangeRequest] = {
    if (UserNicknameChangeRequest.cancelPendingByUser(requestSeq, userSeq) == 0) {
      UserNicknameChangeRequest.selectBySeq(requestSeq) match {
        case Some(row) if row.user == userSeq => Left(ReviewError.NotPending)
        case _ => Left(ReviewError.NotFound)
      }
    } else {
      UserNicknameChangeRequest.selectBySeq(requestSeq).toRight(ReviewError.NotFound)
    }
  }

  /**
   * Approval is the only thing that renames a user, and the only thing that writes history.
   *
   * The nickname is validated again here rather than trusted from the request row: the rules
   * can change between asking and answering, and a name that is legal today may not be when
   * an admin gets to it a week later. Uniqueness is rechecked for the plainer reason that
   * somebody else may have taken it in the meantime.
   */
  def approve(requestSeq: Long, adminUserSeq: Long)(implicit connection: Connection): Either[ReviewError, UserNicknameChangeRequest] = {
    UserNicknameChangeRequest.selectBySeqForUpdate(requestSeq) match {
      case None => Left(ReviewError.NotFound)
      case Some(row) if !row.isPending => Left(ReviewError.NotPending)
      case Some(row) =>
        User.selectBySeq(row.user) match {
          case None => Left(ReviewError.UserNotFound)
          case Some(user) =>
            UserNicknamePolicy.validate(row.requestedNickname) match {
              case Left(error) => Left(ReviewError.Invalid(error))
              case Right(valid) if takenByAnotherUser(valid.value, row.user) => Left(ReviewError.NicknameTaken)
              case Right(valid) =>
                // Checks are done; from here everything writes.
                User.updateNickname(row.user, valid.value)
                UserNicknameHistory.insert(row.user, user.nickname, valid.value, adminUserSeq)
                UserNicknameChangeRequest.review(requestSeq, UserNicknameChangeRequest.Status.Approved, adminUserSeq, None)
                UserNicknameChangeRequest.selectBySeq(requestSeq).toRight(ReviewError.NotFound)
            }
        }
    }
  }

  def reject(requestSeq: Long, adminUserSeq: Long, reason: Option[String])(implicit connection: Connection): Either[ReviewError, UserNicknameChangeRequest] = {
    UserNicknameChangeRequest.selectBySeqForUpdate(requestSeq) match {
      case None => Left(ReviewError.NotFound)
      case Some(row) if !row.isPending => Left(ReviewError.NotPending)
      case Some(_) =>
        val trimmed = reason.map(_.trim).filter(_.nonEmpty)
        UserNicknameChangeRequest.review(requestSeq, UserNicknameChangeRequest.Status.Rejected, adminUserSeq, trimmed)
        UserNicknameChangeRequest.selectBySeq(requestSeq).toRight(ReviewError.NotFound)
    }
  }
}
