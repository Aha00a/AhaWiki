package com.aha00a.play

import java.net.URL
import java.util.Locale

import com.aha00a.commons.utils.UriUtil
import play.api.mvc.RequestHeader

import scala.util.Try

object Implicits {
  implicit class RichRequest(request:RequestHeader) {
    def scheme:String = if(request.secure) "https" else "http"
    def isLocalhost:Boolean = request.headers.get("Host").getOrElse("").startsWith("localhost")
    def remoteAddressWithXRealIp: String = request.headers.get("X-Real-IP").getOrElse(request.remoteAddress)
    def header(key: String): Option[String] = request.headers.get(key)
    def userAgent: Option[String] = header("User-Agent")
    def referer: Option[String] = header("referer")
    def refererFromExternal: Option[String] = referer.filter(referer => new URL(referer).getAuthority != request.host)

    /**
     * Where to send the browser back to: the page it came from, as a path, when that page is on
     * this host -- and the front page when there is no Referer, when it is not an address, when
     * it is another host's, or when what is left of it is not plainly a path.
     *
     * A Referer is whatever page the link was on, another site's included. Until 2026-11-21 this
     * returned it whole, and each caller redirects to what this returns: a link to /logout on
     * another site had this site send its visitor back there, and a login returned there after a
     * real sign-in.
     */
    def refererOrRoot: String = referer
      .flatMap(written => Try(new URL(written)).toOption)
      .filter(_.getAuthority == request.host)
      .map(_.getFile)
      .filter(UriUtil.isLocalPath)
      .getOrElse("/")
    def locale: Locale = request.acceptLanguages.headOption.map(_.locale).getOrElse(Locale.getDefault())
  }
}
