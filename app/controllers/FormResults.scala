package controllers

import play.api.data.Form
import play.api.mvc._

/**
 * What a form-posting endpoint answers when the body is not the form it expects.
 *
 * `bindFromRequest().get` throws `NoSuchElementException: None.get` on a body that does not bind,
 * and nothing catches it, so the caller gets a 500 and the log gets a stack trace. That was the
 * most common 500 this application produced: 726 of the 727 in seven weeks were bots posting junk
 * to `POST /w/FrontPage`. A request that does not carry the form is the caller's mistake, which is
 * what 400 means.
 *
 * The message names the fields that did not bind, so a real client debugging its own request can
 * see which one. Form errors are about field names and shapes, never about values.
 */
trait FormResults extends BaseController {
  protected def badForm(form: Form[_]): Result =
    BadRequest("Bad request: " + form.errors.map(error => s"${error.key} ${error.message}").mkString(", "))

  /** Bind, or answer 400 — for the endpoints that are not async. */
  protected def withForm[T](form: Form[T])(block: T => Result)(implicit request: Request[_]): Result =
    form.bindFromRequest().fold(badForm, block)
}
