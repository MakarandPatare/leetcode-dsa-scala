object DynamicProgramming {
  // https://leetcode.com/explore/interview/card/leetcodes-interview-crash-course-data-structures-and-algorithms/712/dynamic-programming/4580/
  def climbingStairs(input: Int): Int = {
    if (input <= 2) input
    else {
      val mem = new Array[Int](input + 1)
      mem(1) = 1
      mem(2) = 2
      for (n <- 3 to input) {
        mem(n) = mem(n - 1) + mem(n - 2)
      }
      mem(input)
    }
  }

  // https://leetcode.com/explore/interview/card/leetcodes-interview-crash-course-data-structures-and-algorithms/712/dynamic-programming/4684/
  def minCostClimbingStairs(input: Array[Int]): Int = {
    val mem = new Array[Int](input.length + 1)
    mem(0) = 0
    mem(1) = 0
    for (n <- 2 until input.length + 1) {
      mem(n) = math.min(input(n - 1) + mem(n - 1), input(n - 2) + mem(n - 2))
    }
    mem.last
  }

  // https://leetcode.com/explore/interview/card/leetcodes-interview-crash-course-data-structures-and-algorithms/712/dynamic-programming/4581/
  def coinChange(coins: Array[Int], amount: Int): Int = {
    val dp = Array.fill(amount + 1)(amount + 1)
    dp(0) = 0
    for (n <- 1 to amount) {
      for (coin <- coins) {
        if (coin <= n) {
          dp(n) = math.min(dp(n), dp(n - coin) + 1)
        }
      }
    }
    if (dp(amount) > amount) -1 else dp(amount)
  }
}
