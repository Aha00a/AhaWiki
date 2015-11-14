package com.aha00a.commons.utils

import play.api.mvc.Request

object CookieUtil {
  def value(request:Request[Any], key:String, default:String):String = {
    request.cookies.get(key).map(_.value).getOrElse(default)
  }
}
