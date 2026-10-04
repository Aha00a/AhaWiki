package com.aha00a.controllers

import controllers.ApiAdminS3
import org.scalatest.freespec.AnyFreeSpec
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request
import software.amazon.awssdk.services.s3.model.ListObjectsV2Response
import software.amazon.awssdk.services.s3.model.S3Object

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** The admin S3 browser's paging, with the S3 call replaced by canned pages. The browser itself
  * can only be tried against the real bucket. */
class ApiAdminS3Spec extends AnyFreeSpec {
  private val request = ListObjectsV2Request.builder().bucket("example-bucket").prefix("Attachment/").delimiter("/").build()

  private def page(keyCount: Int, nextToken: Option[String]): ListObjectsV2Response =
    ListObjectsV2Response.builder()
      .contents((1 to keyCount).map(i => S3Object.builder().key(s"Attachment/$i.png").build()).asJava)
      .nextContinuationToken(nextToken.orNull)
      .build()

  /** Answers with `pages` in turn, and keeps what it was asked. */
  private def cannedS3(pages: ListObjectsV2Response*): (ListObjectsV2Request => ListObjectsV2Response, mutable.Buffer[ListObjectsV2Request]) = {
    val asked = mutable.ArrayBuffer.empty[ListObjectsV2Request]
    val answers = pages.iterator
    ((r: ListObjectsV2Request) => { asked += r; answers.next() }, asked)
  }

  "listPages" - {
    "asks once for a listing that is not recursive, and hands back the token for the rest" in {
      val (list, asked) = cannedS3(page(3, Some("t1")))
      val (pages, token) = ApiAdminS3.listPages(list, request, 500, recursive = false)
      assert(pages.size === 1)
      assert(token === Some("t1"))
      assert(asked.size === 1)
      assert(asked.head.maxKeys() === 500)
      assert(asked.head.continuationToken() === null)
      // Everything else about the request is passed on as it was given.
      assert(asked.head.bucket() === "example-bucket")
      assert(asked.head.prefix() === "Attachment/")
      assert(asked.head.delimiter() === "/")
    }

    "follows the token when recursive, asking each time only for what is still wanted" in {
      val (list, asked) = cannedS3(page(300, Some("t1")), page(200, Some("t2")))
      val (pages, token) = ApiAdminS3.listPages(list, request, 500, recursive = true)
      assert(asked.map(_.continuationToken()) === Seq(null, "t1"))
      assert(asked.map(_.maxKeys().intValue) === Seq(500, 200))
      assert(pages.flatMap(_.contents().asScala).size === 500)
      assert(token === Some("t2"), "500 came back and more are there")
    }

    "stops where the listing ends" in {
      val (list, asked) = cannedS3(page(300, Some("t1")), page(10, None))
      val (pages, token) = ApiAdminS3.listPages(list, request, 1000, recursive = true)
      assert(asked.size === 2)
      assert(pages.size === 2)
      assert(token === None)
    }

    "never asks S3 for more than one request returns" in {
      val (list, asked) = cannedS3(page(0, None))
      ApiAdminS3.listPages(list, request, 5000, recursive = true)
      assert(asked.head.maxKeys() === ApiAdminS3.maxKeysPerRequest)
    }
  }
}
