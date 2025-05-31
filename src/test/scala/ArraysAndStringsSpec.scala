import ArraysAndStrings.{isSubsequence, reverseString, sortedSquares, twoSum}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldEqual

class ArraysAndStringsSpec extends AnyFunSpec {
  describe("twoSum") {
    it("should return correct output for case 1") {
      val inputArray = Array(2,7,11,15)
      val target = 9
      val expectedOutput = Array(1, 2)
      assert(twoSum(inputArray, target) sameElements expectedOutput)
    }
    it("should return correct output for case 2") {
      val inputArray = Array(2,3,4)
      val target = 6
      val expectedOutput = Array(1, 3)
      assert(twoSum(inputArray, target) sameElements expectedOutput)
    }
    it("should return correct output for case 3") {
      val inputArray = Array(-1,0)
      val target = -1
      val expectedOutput = Array(1, 2)
      assert(twoSum(inputArray, target) sameElements expectedOutput)
    }
  }
  describe("isSubsequence") {
    it("should return true for case 1") {
      val inputS = "abc"
      val inputT = "ahbgdc"
      val expected = true
      val output = isSubsequence(inputS, inputT)
      assert(output == expected)
    }
    it("should return true for case 2") {
      val inputS = "axc"
      val inputT = "ahbgdc"
      val expected = false
      val output = isSubsequence(inputS, inputT)
      assert(output == expected)
    }
  }
  describe("reverseString") {
    it("should reverse normal string") {
      val inputString = Array[Char]('h', 'e', 'l', 'l', 'o')
      val expectedString: Array[Char] = Array[Char]('o', 'l', 'l', 'e', 'h')
      val result: Array[Char] = reverseString(inputString)
      assert(result sameElements expectedString)
    }
    it("should reverse palindrome string") {
      val inputString = Array[Char]('h', 'a', 'n', 'n', 'a', 'h')
      val expectedString = Array[Char]('h', 'a', 'n', 'n', 'a', 'h')
      assert(reverseString(inputString) sameElements expectedString)
    }
    it("should reverse palindrome with mixed case") {
      val inputString = Array[Char]('H', 'A', 'n', 'N', 'a', 'h')
      val expectedString = Array[Char]('h', 'a', 'N', 'n', 'A', 'H')
      assert(reverseString(inputString) sameElements expectedString)
    }
    it("should do nothing for empty string") {
      val inputString = Array[Char]()
      val expectedString = Array[Char]()
      assert(reverseString(inputString) sameElements expectedString)
    }
  }
  describe("sortedSquares") {
    it ("should return correct response for case 1") {
      val input = Array(-4,-1,0,3,10)
      val expected = Array(0, 1, 9, 16, 100)
      val output = sortedSquares(input)
      assert(output sameElements expected)
    }
    it("should return correct response for case 2") {
      val input = Array(-7,-3,2,3,11)
      val expected = Array(4, 9, 9, 49, 121)
      val output = sortedSquares(input)
      assert(output sameElements expected)
    }
  }
}
