import scala.annotation.tailrec

object ArraysAndStrings {
  def reverseString(array: Array[Char]): Array[Char] = {
    @tailrec
    def loop(left: Int, right: Int): Array[Char] = {
      if (left < right) {
        val temp = array(left)
        array(left) = array(right)
        array(right) = temp
        loop(left + 1, right - 1)
      } else array
    }
    loop(0, array.length - 1)
  }
}
