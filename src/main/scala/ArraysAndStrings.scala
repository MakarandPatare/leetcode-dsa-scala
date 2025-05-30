import scala.annotation.tailrec

object ArraysAndStrings {
  // https://leetcode.com/problems/reverse-string/description/
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
  // https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/description/
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    @tailrec
    def loop(low: Int, high: Int): Array[Int] = {
      val sum = numbers(low) + numbers(high)
      if (low > high) Array(-1, -1)
      else if (sum == target) Array(low + 1, high + 1)
      else if (sum < target) loop(low + 1, high)
      else loop(low, high - 1)
    }
    loop(0, numbers.length - 1)
  }
}
