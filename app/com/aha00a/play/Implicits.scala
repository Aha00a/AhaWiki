package com.aha00a.play

import java.util.Locale

import play.api.mvc.RequestHeader

object Implicits {
  implicit class RichRequest(request:RequestHeader) {
    def isLocalhost:Boolean = request.headers.get("Host").getOrElse("").startsWith("localhost")
    def remoteAddressWithXRealIp: String = request.headers.get("X-Real-IP").getOrElse(request.remoteAddress)
    def header(key: String): Option[String] = request.headers.get(key)
    def userAgent: Option[String] = header("User-Agent")
    def referer: Option[String] = header("referer")
    def refererOrRoot: String = referer.getOrElse("/")
    def locale: Locale = request.acceptLanguages.headOption.map(_.locale).getOrElse(Locale.getDefault())
  }
}
