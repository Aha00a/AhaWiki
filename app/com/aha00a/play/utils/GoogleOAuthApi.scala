package com.aha00a.play.utils

import play.api.Logger
import play.api.Logging
import play.api.libs.json.JsValue
import play.api.libs.ws.{WSClient, WSResponse}

import scala.concurrent.{ExecutionContext, Future}
import scala.async.Async.{async, await}

case class GoogleOAuthApi()(implicit wsClient: WSClient, executionContext: ExecutionContext) extends Logging {


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

  def requestOAuthToken(code: String, googleClientId: String, googleClientSecret: String, redirectUri: String): Future[Option[JsValue]] = async {
    val response = await(wsClient.url("https://accounts.google.com/o/oauth2/token").post(Map(
      "client_id" -> Seq(googleClientId),
      "client_secret" -> Seq(googleClientSecret),
      "redirect_uri" -> Seq(redirectUri),
      "code" -> Seq(code),
      "grant_type" -> Seq("authorization_code")
    )))
    logger.info(response.status.toString)
    logger.info(response.body)
    if (200 == response.status) {
      Some(response.json)
    } else {
      None
    }
  }

  def requestMe(accessToken: String): Future[Option[JsValue]] = {
    val responseToOptionJsValue: WSResponse => Option[JsValue] = response => {
      if (200 == response.status) {
        Some(response.json)
      } else {
        logger.info(response.status.toString)
        logger.info(response.body)
        None
      }
    }

    val apiPeopleMeNew: Future[Option[JsValue]] = wsClient
      .url("https://people.googleapis.com/v1/people/me")
      .withQueryStringParameters(
        "access_token" -> accessToken,
        "personFields" -> "emailAddresses"
      )
      .get()
      .map(responseToOptionJsValue)

    apiPeopleMeNew flatMap {
      case Some(z) => Future(Some(z))
      case None =>
        val apiPeopleMeLegacy: Future[Option[JsValue]] = wsClient
          .url("https://www.googleapis.com/plus/v1/people/me")
          .withQueryStringParameters("access_token" -> accessToken)
          .get()
          .map(responseToOptionJsValue)
        apiPeopleMeLegacy
    }
  }

  def getAccessToken(jsValue: JsValue): String = {
    (jsValue \ "access_token").as[String]
  }

  def getEmail(jsValue: JsValue): String = {
    try {
      ((jsValue \ "emailAddresses")(0) \ "value").as[String] // New Api
    } catch  {
      case _: Exception => ((jsValue \ "emails")(0) \ "value").as[String] // Legacy Api
    }
  }
}
