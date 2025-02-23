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
}
