package logics

import java.util.Locale

object UserNicknamePolicy {
  sealed trait ValidationError {
    def message: String
  }

  object ValidationError {
    case object Empty extends ValidationError {
      override val message: String = "Nickname is required."
    }

    case object TooShort extends ValidationError {
      override val message: String = "Nickname must be at least 5 characters."
    }

    case object TooLong extends ValidationError {
      override val message: String = "Nickname must be at most 16 characters."
    }

    case object InvalidCharacters extends ValidationError {
      override val message: String = "Nickname can contain only Korean letters, English letters, and numbers."
    }

    case object MissingLetter extends ValidationError {
      override val message: String = "Nickname must contain at least one Korean or English letter."
    }

    case object Reserved extends ValidationError {
      override val message: String = "This nickname is reserved."
    }
  }

  case class ValidNickname(value: String)

  private val MinLength = 5
  private val MaxLength = 16
  private val AllowedCharacters = """^[a-zA-Z0-9가-힣]+$""".r
  private val ContainsLetter = """.*[a-zA-Z가-힣].*""".r
  private val ReservedNicknames = Set("anonymous", "guest", "admin", "ahawiki", "user")

  def normalize(rawNickname: String): String =
    Option(rawNickname).getOrElse("").trim

  def validate(rawNickname: String): Either[ValidationError, ValidNickname] = {
    val nickname = normalize(rawNickname)

    if (nickname.isEmpty) Left(ValidationError.Empty)
    else if (ReservedNicknames.contains(nickname.toLowerCase(Locale.ROOT))) Left(ValidationError.Reserved)
    else if (nickname.length < MinLength) Left(ValidationError.TooShort)
    else if (nickname.length > MaxLength) Left(ValidationError.TooLong)
    else if (AllowedCharacters.findFirstIn(nickname).isEmpty) Left(ValidationError.InvalidCharacters)
    else if (ContainsLetter.findFirstIn(nickname).isEmpty) Left(ValidationError.MissingLetter)
    else Right(ValidNickname(nickname))
  }
}
