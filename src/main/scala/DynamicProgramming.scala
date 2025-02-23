object DynamicProgramming {
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
}
