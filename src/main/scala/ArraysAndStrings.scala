import scala.annotation.tailrec

object ArraysAndStrings {
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
  // https://leetcode.com/problems/is-subsequence/description/
  def isSubsequence(s: String, t: String): Boolean = {
    @tailrec
    def loop(i: Int, j: Int): Boolean = {
      if (i == s.length) true
      else if (j == t.length) false
      else if (s(i) == t(j)) loop(i + 1, j + 1)
      else loop(i, j + 1)
    }
    loop(0, 0)
  }
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

  // https://leetcode.com/problems/squares-of-a-sorted-array/description/
  def sortedSquares(nums: Array[Int]): Array[Int] = {
    val n = nums.length
    var left = 0
    var right = n - 1
    val result = new Array[Int](n)
    for (i <- n - 1 to 0 by -1) {
      var largeNum = 0
      if (math.abs(nums(right)) > math.abs(nums(left))) {
        largeNum = nums(right)
        right -= 1
      } else {
        largeNum = nums(left)
        left += 1
      }
      result(i) = largeNum * largeNum
    }
    result
  }
}
