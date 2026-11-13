package logics

import play.twirl.api.HtmlFormat

/**
 * What a person is told on the login that made their account.
 *
 * That login names them after the part of their email address before the `@` (`User.insert`),
 * and the name is public from then on: beside their cursor for anyone reading the same page, and
 * as the author of whatever they save. Nothing said so, and the way to another name is a request
 * an admin has to approve (`UserNicknameChangeLogic`), which nobody goes looking for unprompted.
 *
 * It travels as a flash value, and `_base.scala.html` writes flash values into the page as HTML.
 * That is what lets this one hold a link, and it is why the name is escaped here -- everything
 * else in the string comes from this file.
 */
object FirstLoginNotice {
  /** The id of the nickname section of Account Settings, which is where the link lands. */
  val nicknameSectionId = "nickname"

  def html(nickname: String, settingsUrl: String): String =
    s"Your name on this wiki is <strong>${HtmlFormat.escape(nickname).body}</strong>, made from your email address. " +
      "Other people see it: beside your cursor on a page you are both reading, and as the author of what you write. " +
      s"""To go by another name, <a href="$settingsUrl#$nicknameSectionId">ask for one in Account Settings</a> &mdash; an admin approves it."""
}
