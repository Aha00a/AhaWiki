package logics.wikis

class HeadingNumber {
  val array: Array[Int] = Array[Int](0, 0, 0, 0, 0, 0)
  def incrGet(level:Int): String = {
    array(level) += 1
    for(i <- (level + 1) until array.length) {
      array(i) = 0
    }
    array.slice(1, level + 1).foldLeft("")((a, b) => { a + b + "."})
  }
}
