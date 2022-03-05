package com.aha00a.play

import java.net.URL
import java.util.Locale

import play.api.mvc.RequestHeader

object Implicits {
  implicit class RichRequest(request:RequestHeader) {
    def scheme:String = if(request.secure) "https" else "http"
    def isLocalhost:Boolean = request.headers.get("Host").getOrElse("").startsWith("localhost")
    def remoteAddressWithXRealIp: String = request.headers.get("X-Real-IP").getOrElse(request.remoteAddress)
    def header(key: String): Option[String] = request.headers.get(key)
    def userAgent: Option[String] = header("User-Agent")
    def referer: Option[String] = header("referer")
    def refererFromExternal: Option[String] = referer.filter(referer => new URL(referer).getAuthority != request.host)
    def refererOrRoot: String = referer.getOrElse("/")
    def locale: Locale = request.acceptLanguages.headOption.map(_.locale).getOrElse(Locale.getDefault())
  }
}
