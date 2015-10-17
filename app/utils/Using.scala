package utils

import java.io.Closeable

object Using {
  def apply[TCloseable <: Closeable, TResult](resource: TCloseable)(operation: (TCloseable) => TResult): TResult = {
    try {
      operation(resource)
    } finally {
      resource.close()
    }
  }
}
