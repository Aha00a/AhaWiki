package logics

import org.scalatest.freespec.AnyFreeSpec

class UrlDetectorSpec extends AnyFreeSpec {

  "YouTube" in {
    assert(UrlDetector.YouTube.getId(null).getOrElse("") == "");
    assert(UrlDetector.YouTube.getId("").getOrElse("") == "");
    assert(UrlDetector.YouTube.getId("alkdsjfalksdjflak").getOrElse("") == "");
    assert(UrlDetector.YouTube.getId("http://www.youtube.com/embed/2wKqfk8pESE").getOrElse("") == "2wKqfk8pESE");
    assert(UrlDetector.YouTube.getId("https://www.youtube.com/watch?v=2wKqfk8pESE&feature=youtu.be").getOrElse("") == "2wKqfk8pESE");
    assert(UrlDetector.YouTube.getId("https://youtu.be/2wKqfk8pESE").getOrElse("") == "2wKqfk8pESE");

  }
}
