import ArraysAndStrings.reverseString
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldEqual

class ReverseStringSpec extends AnyFunSpec {
  describe("reverseString") {
    it("should reverse normal string") {
      val inputString = Array[Char]('h','e','l','l','o')
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
}
