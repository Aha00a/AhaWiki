package com.aha00a.play.utils

import play.api.libs.ws.WSClient

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object GoogleSpreadsheetApi {
  def readSpreadSheet(
    key: String,
    id: String,
    sheetName: String
  )(
    implicit
    wSClient: WSClient,
    executionContext: ExecutionContext
  ): Future[Seq[Seq[String]]] = {
    val range = if (sheetName.isEmpty) "A1:Z1000" else s"'${sheetName.replace("'", "''")}'!A1:Z1000"
    wSClient
      .url(s"https://sheets.googleapis.com/v4/spreadsheets/$id/values/$range")
      .withQueryStringParameters("key" -> key)
      .withRequestTimeout(60 seconds)
      .get()
      .map(r => (r.json \ "values").as[Seq[Seq[String]]])
  }
}
