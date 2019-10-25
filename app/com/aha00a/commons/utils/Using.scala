package com.aha00a.commons.utils

import java.io.Closeable

object Using {
  def apply[TCloseable <: { def close(): Unit }, TResult](resource: TCloseable)(operation: TCloseable => TResult): TResult = {
    try {
      operation(resource)
    } finally {
      resource.close()
    }
  }
}
