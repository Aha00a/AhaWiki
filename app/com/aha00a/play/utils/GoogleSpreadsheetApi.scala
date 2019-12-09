package com.aha00a.play.utils

import logics.ApplicationConf
import play.api.Configuration
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

object GoogleSpreadsheetApi {
  def readSpreadSheet(id: String, sheetName: String)(implicit configuration:Configuration, wSClient: WSClient, executionContext: ExecutionContext): Future[Seq[Seq[String]]] = wSClient
    .url(s"https://sheets.googleapis.com/v4/spreadsheets/$id/values/$sheetName")
    .withQueryString("key" -> ApplicationConf().AhaWiki.google.credentials.api.GoogleSheetsAPI.key())
    .get()
    .map(r => (r.json \ "values").as[Seq[Seq[String]]])
}
