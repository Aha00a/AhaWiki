package com.aha00a.commons.utils

object Using {
  // AutoCloseable, not the structural `{ def close(): Unit }` this took from 2019: a structural call
  // is made by reflection on the resource's runtime class, and when that class is internal to the
  // JDK -- a stream read from inside a jar is a JarURLInputStream -- the module system refuses it.
  // That is how production answered 500 on every page when the canary of 2026-10-01 read its
  // assets out of the release jar. A call through the interface needs no reflection.
  def apply[TCloseable <: AutoCloseable, TResult](resource: TCloseable)(operation: TCloseable => TResult): TResult = {
    try {
      operation(resource)
    } finally {
      resource.close()
    }
  }
}
