package com.aha00a.commons.utils

import play.api.Logger
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

case class GoogleApi()(implicit wsClient: WSClient, executionContext: ExecutionContext) {


  def retrieveEmailWithCode(code: String, googleClientId: String, googleClientSecret: String, redirectUri: String): Future[Option[String]] = {
    retrieveMeWithCode(code, googleClientId, googleClientSecret, redirectUri).map {
      case Some(s) => Some(getEmail(s))
      case None => None
    }
  }

  def retrieveMeWithCode(code: String, googleClientId: String, googleClientSecret: String, redirectUri: String): Future[Option[JsValue]] = {
    requestOAuthToken(code, googleClientId, googleClientSecret, redirectUri) flatMap {
      case Some(oauthTokenJson) => requestMe(getAccessToken(oauthTokenJson))
      case None => Future.successful(None)
    }
  }

  def requestOAuthToken(code: String, googleClientId: String, googleClientSecret: String, redirectUri: String): Future[Option[JsValue]] = {
    wsClient.url("https://accounts.google.com/o/oauth2/token").post(Map(
      "client_id" -> Seq(googleClientId),
      "client_secret" -> Seq(googleClientSecret),
      "redirect_uri" -> Seq(redirectUri),
      "code" -> Seq(code),
      "grant_type" -> Seq("authorization_code")
    )).map(response => {
      Logger.info(response.status.toString)
      Logger.info(response.body)
      if (200 == response.status) {
        Some(response.json)
      } else {
        None
      }
    })
  }

  def requestMe(accessToken: String): Future[Option[JsValue]] = {
    wsClient.url("https://www.googleapis.com/plus/v1/people/me").withQueryString("access_token" -> accessToken).get().map(response => {
      Logger.info(response.status.toString)
      Logger.info(response.body)
      if (200 == response.status) {
        Some(response.json)
      } else {
        None
      }
    })
  }

  def getAccessToken(jsValue: JsValue): String = {
    (jsValue \ "access_token").as[String]
  }

  def getEmail(jsValue: JsValue): String = {
    ((jsValue \ "emails")(0) \ "value").as[String]
  }
}
