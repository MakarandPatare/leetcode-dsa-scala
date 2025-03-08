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
  // https://leetcode.com/problems/longest-common-subsequence/
  def longestCommonSubsequence(text1: String, text2: String): Int = {
    if (text1.isEmpty || text2.isEmpty) return 0
    val dp: Array[Array[Int]] = Array.ofDim(text1.length + 1, text2.length + 1)
    dp(0)(0) = 0
    for (i <- 1 to text1.length)
      for (j <- 1 to text2.length) {
        if (text1(i - 1) == text2(j - 1)) {
          dp(i)(j) = 1 + dp(i - 1)(j - 1)
        } else {
          dp(i)(j) = math.max(dp(i)(j - 1), dp(i - 1)(j))
        }
      }
    dp(text1.length)(text2.length)
  }
  // https://leetcode.com/problems/best-time-to-buy-and-sell-stock-iv/description/
  def bestTimeToBuyAndSellStickIV(k: Int, prices: Array[Int]): Int = {
    val n = prices.length
    val dp: Array[Array[Array[Int]]] = Array.ofDim(n + 1, k + 1, 2)
    for (day <- n - 1 to 0 by -1) {
      for (remain <- k - 1 to 0 by -1) {
        for (holding <- 0 to 1) {
          val tempSol = dp(day + 1)(remain)(holding)
          dp(day)(remain)(holding) =
            if (holding == 0) {
              Math.max(tempSol, - prices(day) + dp(day + 1)(remain)(1))
            } else {
              Math.max(tempSol, prices(day) + dp(day + 1)(remain + 1)(0))
            }
        }
      }
    }
    dp(0)(0)(0)
  }
}
