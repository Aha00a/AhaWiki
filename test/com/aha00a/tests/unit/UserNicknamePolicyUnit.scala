package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.UserNicknamePolicy
import logics.UserNicknamePolicy.ValidationError

object UserNicknamePolicyUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    assertEquals(UserNicknamePolicy.validate("가나다라마").map(_.value), Right("가나다라마"))
    assertEquals(UserNicknamePolicy.validate("AhaWiki2026").map(_.value), Right("AhaWiki2026"))
    assertEquals(UserNicknamePolicy.validate("  AhaWiki2026  ").map(_.value), Right("AhaWiki2026"))

    assertEquals(UserNicknamePolicy.validate(""), Left(ValidationError.Empty))
    assertEquals(UserNicknamePolicy.validate("     "), Left(ValidationError.Empty))
    assertEquals(UserNicknamePolicy.validate("abcd"), Left(ValidationError.TooShort))
    assertEquals(UserNicknamePolicy.validate("abcdefghijklmnopq"), Left(ValidationError.TooLong))
    assertEquals(UserNicknamePolicy.validate("Aha Wiki"), Left(ValidationError.InvalidCharacters))
    assertEquals(UserNicknamePolicy.validate("Aha_Wiki"), Left(ValidationError.InvalidCharacters))
    assertEquals(UserNicknamePolicy.validate("AhaWiki🙂"), Left(ValidationError.InvalidCharacters))
    assertEquals(UserNicknamePolicy.validate("12345"), Left(ValidationError.MissingLetter))
    assertEquals(UserNicknamePolicy.validate("Admin"), Left(ValidationError.Reserved))
    assertEquals(UserNicknamePolicy.validate("ahawiki"), Left(ValidationError.Reserved))
    assertEquals(UserNicknamePolicy.validate("User"), Left(ValidationError.Reserved))
  }
}
