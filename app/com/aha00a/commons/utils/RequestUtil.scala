package com.aha00a.commons.utils

import play.api.mvc.{AnyContent, Request}

object RequestUtil {
  def header(request: Request[AnyContent], key: String): Option[String] = request.headers.get(key)

  def referer(request: Request[AnyContent]): Option[String] = header(request, "referer")

  def refererOrRoot(request: Request[AnyContent]): String = referer(request).getOrElse("/")
}
