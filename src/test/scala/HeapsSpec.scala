import org.scalatest.funspec.AnyFunSpec

class HeapsSpec extends AnyFunSpec {
  describe("1046. Last Stone Weight") {
    it("example 1 - stones [2,7,4,1,8,1] should return 1") {
      assert(Heaps.lastStoneWeight(Array(2, 7, 4, 1, 8, 1)) == 1)
    }
    it("example 2 - single stone [1] should return 1") {
      assert(Heaps.lastStoneWeight(Array(1)) == 1)
    }
    it("two equal stones cancel out and return 0") {
      assert(Heaps.lastStoneWeight(Array(3, 3)) == 0)
    }
    it("two different stones return their difference") {
      assert(Heaps.lastStoneWeight(Array(3, 7)) == 4)
    }
    it("all same stones with even count returns 0") {
      assert(Heaps.lastStoneWeight(Array(4, 4, 4, 4)) == 0)
    }
    it("all same stones with odd count returns stone weight") {
      assert(Heaps.lastStoneWeight(Array(3, 3, 3)) == 3)
    }
    it("empty stones array returns 0") {
      assert(Heaps.lastStoneWeight(Array.empty[Int]) == 0)
    }
    it("large values") {
      assert(Heaps.lastStoneWeight(Array(1000, 1000, 999)) == 999)
    }
  }
  describe("2208. Minimum Operations to Halve Array Sum") {
    it("example 1 - nums [5,19,8,1] should return 3") {
      assert(Heaps.halveArray(Array(5, 19, 8, 1)) == 3)
    }
    it("example 2 - nums [3,8,20] should return 3") {
      assert(Heaps.halveArray(Array(3, 8, 20)) == 3)
    }
    it("single element array") {
      assert(Heaps.halveArray(Array(10)) == 1)
    }
    it("two equal elements") {
      assert(Heaps.halveArray(Array(4, 4)) == 2)
    }
    it("all equal large elements") {
      assert(Heaps.halveArray(Array(8, 8, 8, 8)) == 4)
    }
    it("single operation needed") {
      assert(Heaps.halveArray(Array(1)) == 1)
    }
  }
}
