import org.scalatest.funspec.AnyFunSpec

class DynamicProgrammingSpec extends AnyFunSpec {
  describe("climbingStairs") {
    it("should return correct answer for just two steps") {
      val input = 2
      val expected = 2
      val output = DynamicProgramming.climbingStairs(input)
      assert(output == expected)
    }
    it("should return correct answer for three steps") {
      val input = 3
      val expected = 3
      val output = DynamicProgramming.climbingStairs(input)
      assert(output == expected)
    }
    it("should return correct answer for ten steps") {
      val input = 10
      val expected = 89
      val output = DynamicProgramming.climbingStairs(input)
      assert(output == expected)
    }
  }
  describe("minCostClimbingStairs") {
    it("should return 0 for single step") {
      val input = Array(10)
      val expected = 0
      val output = DynamicProgramming.minCostClimbingStairs(input)
      assert(output == expected)
    }
    it("should return correct answer for two steps") {
      val input = Array(10, 15)
      val expected = 10
      val output = DynamicProgramming.minCostClimbingStairs(input)
      assert(output == expected)
    }
    it("should return correct answer for three steps") {
      val input = Array(10, 15, 20)
      val expected = 15
      val output = DynamicProgramming.minCostClimbingStairs(input)
      assert(output == expected)
    }
    it("should return correct answer for too many steps") {
      val input = Array(1,100,1,1,1,100,1,1,100,1)
      val expected = 6
      val output = DynamicProgramming.minCostClimbingStairs(input)
      assert(output == expected)
    }
  }
}
