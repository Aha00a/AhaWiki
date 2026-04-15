package logics

import org.scalatest.freespec.AnyFreeSpec

class UrlDetectorSpec extends AnyFreeSpec {

  "YouTube" - {
    "returns None for invalid inputs" in {
      assert(UrlDetector.YouTube.getId(null).isEmpty)
      assert(UrlDetector.YouTube.getId("").isEmpty)
      assert(UrlDetector.YouTube.getId("alkdsjfalksdjflak").isEmpty)
      assert(UrlDetector.YouTube.getId("https://example.com/watch?v=2wKqfk8pESE").isEmpty)
      assert(UrlDetector.YouTube.getId("https://m.youtube.com/watch?v=2wKqfk8pESE").isEmpty)
    }

    "extracts id from supported YouTube URL shapes" in {
      assert(UrlDetector.YouTube.getId("http://www.youtube.com/embed/2wKqfk8pESE").contains("2wKqfk8pESE"))
      assert(UrlDetector.YouTube.getId("https://www.youtube.com/watch?v=2wKqfk8pESE&feature=youtu.be").contains("2wKqfk8pESE"))
      assert(UrlDetector.YouTube.getId("https://www.youtube.com/watch?feature=youtu.be&v=2wKqfk8pESE").contains("2wKqfk8pESE"))
      assert(UrlDetector.YouTube.getId("https://youtu.be/2wKqfk8pESE").contains("2wKqfk8pESE"))
      assert(UrlDetector.YouTube.getId("https://www.youtube.com/shorts/2wKqfk8pESE").contains("2wKqfk8pESE"))
    }

    "returns None when watch url does not contain v query" in {
      assert(UrlDetector.YouTube.getId("https://www.youtube.com/watch?feature=youtu.be").isEmpty)
      assert(UrlDetector.YouTube.getId("https://www.youtube.com/watch").isEmpty)
    }
  }
}
