package logics

import org.apache.pekko.stream.scaladsl.SourceQueueWithComplete

import scala.collection.concurrent.TrieMap

/**
 * Who is currently watching a page, so an edit can be pushed to them.
 *
 * This is process-local mutable state, and both ends of the feature reach it: the WebSocket
 * endpoint subscribes and relays cursors, and the save endpoint announces that the page
 * changed. It used to be a private object inside the Wiki controller, which meant the two
 * could not be separated without one of them keeping a second hub — and two hubs would each
 * hold half the subscribers, so half the readers would miss every update.
 *
 * Being process-local also means it only reaches the instance the socket landed on. With two
 * instances behind nginx, an edit on one is not pushed to watchers on the other. Live
 * cursors are a convenience, so that is accepted rather than solved with a shared broker.
 */
object PageCursorHub {
  private case class PageSubscriber(
    queue: SourceQueueWithComplete[String],
    var saveSenderId: Option[String],
  )

  private val subscribers = TrieMap.empty[String, TrieMap[String, PageSubscriber]]

  /** One room per page per site. Page names are not unique across sites. */
  def roomKeyForPage(siteId: Long, pageId: String): String = s"wiki:$siteId:$pageId"

  def subscribe(page: String, id: String, queue: SourceQueueWithComplete[String]): Unit = {
    val pageMap = subscribers.getOrElseUpdate(page, TrieMap.empty[String, PageSubscriber])
    pageMap.put(id, PageSubscriber(queue, None))
  }

  def unsubscribe(page: String, id: String): Unit = {
    subscribers.get(page).foreach { pageMap =>
      pageMap.remove(id).foreach(_.queue.complete())
      if (pageMap.isEmpty) subscribers.remove(page)
    }
  }

  def broadcast(page: String, senderId: String, payload: String): Unit = {
    subscribers.get(page).foreach { pageMap =>
      pageMap.foreach { case (id, subscriber) =>
        if (id != senderId) subscriber.queue.offer(payload)
      }
    }
  }

  def setSaveSenderId(page: String, id: String, saveSenderId: Option[String]): Unit = {
    subscribers.get(page).flatMap(_.get(id)).foreach { subscriber =>
      subscriber.saveSenderId = saveSenderId.filter(_.nonEmpty)
    }
  }

  /** The saver already has the new text, so their own socket is skipped. */
  def broadcastPageUpdated(page: String, saveSenderId: Option[String], payload: String): Unit = {
    subscribers.get(page).foreach { pageMap =>
      pageMap.foreach { case (_, subscriber) =>
        val shouldExclude = saveSenderId.nonEmpty && subscriber.saveSenderId == saveSenderId
        if (!shouldExclude) subscriber.queue.offer(payload)
      }
    }
  }
}
