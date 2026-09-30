package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class UsingSpec extends AnyFreeSpec {
  // In production the app's own files are inside a jar, and a stream read from one is a
  // JarURLInputStream: a class in a package java.base does not open. Using once took anything with
  // a close() method and called it by reflection on the runtime class, which the module system
  // refuses for that class -- every page answered 500 on the 2026-10-01 canary. Locally the same
  // files are on disk and the stream is a FileInputStream, so nothing else here would notice.
  "closes a stream read from inside a jar" in {
    val stream = classOf[scala.Option[_]].getClassLoader.getResourceAsStream("scala/Option.class")
    assert(stream.getClass.getName.startsWith("sun.net.www.protocol.jar."))
    assert(Using(stream)(_.readAllBytes()).nonEmpty)
    assertThrows[java.io.IOException](stream.read())
  }

  "closes the resource when the operation throws" in {
    var closed = false
    val resource = new AutoCloseable { def close(): Unit = closed = true }
    assertThrows[IllegalStateException](Using(resource)(_ => throw new IllegalStateException()))
    assert(closed)
  }
}
