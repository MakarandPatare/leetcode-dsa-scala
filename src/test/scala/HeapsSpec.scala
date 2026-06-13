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
  describe("1962. Remove Stones to Minimize the Total") {
    it("example 1 - piles [5,4,9], k = 2 should return 12") {
      assert(Heaps.minStoneSum(Array(5, 4, 9), 2) == 12)
    }
    it("example 2 - piles [4,3,6,7], k = 3 should return 12") {
      assert(Heaps.minStoneSum(Array(4, 3, 6, 7), 3) == 12)
    }
    it("no operations (k = 0) returns original sum") {
      assert(Heaps.minStoneSum(Array(2, 2, 3), 0) == 7)
    }
    it("single small pile with many operations remains 1") {
      assert(Heaps.minStoneSum(Array(1), 100) == 1)
    }
    it("large equal piles with several ops") {
      assert(Heaps.minStoneSum(Array(1000, 1000, 1000), 5) == 1000)
    }
  }
  describe("347. Top K Frequent Elements") {
    it("example - nums [1,1,1,2,2,3], k = 2 should return {1,2}") {
      val res = Heaps.topKFrequent(Array(1, 1, 1, 2, 2, 3), 2)
      assert(res.toSet == Set(1, 2))
    }
    it("single element array") {
      val res = Heaps.topKFrequent(Array(42), 1)
      assert(res.toSet == Set(42))
    }
    it("all unique elements with k = number of uniques") {
      val res = Heaps.topKFrequent(Array(4, 5, 6), 3)
      assert(res.toSet == Set(4, 5, 6))
    }
    it("negative numbers") {
      val res = Heaps.topKFrequent(Array(-1, -1, -2, -2, -3), 2)
      assert(res.toSet == Set(-1, -2))
    }
    it("tie case - any k elements from top frequency candidates are valid") {
      val res = Heaps.topKFrequent(Array(1, 2, 3, 2, 3, 1), 2)
      // frequencies: 1,2,3 all appear twice -> any two of {1,2,3} are valid
      assert(res.length == 2 && res.toSet.subsetOf(Set(1, 2, 3)))
    }
  }
  describe("658. Find K Closest Elements") {
    it("example 1 - arr [1,2,3,4,5], k = 4, x = 3 should return [1,2,3,4]") {
      val res = Heaps.findClosestElements(Array(1, 2, 3, 4, 5), 4, 3)
      assert(res == List(1, 2, 3, 4))
    }
    it("example 2 - arr [1,2,3,4,5], k = 4, x = -1 should return [1,2,3,4]") {
      val res = Heaps.findClosestElements(Array(1, 2, 3, 4, 5), 4, -1)
      assert(res == List(1, 2, 3, 4))
    }
    it("single element array returns that element") {
      val res = Heaps.findClosestElements(Array(1), 1, 100)
      assert(res == List(1))
    }
    it("tie-break smaller first when distances equal") {
      val res = Heaps.findClosestElements(Array(1, 2, 3, 4, 5), 2, 3)
      assert(res == List(2, 3))
    }
    it("k equals array length returns whole array") {
      val res = Heaps.findClosestElements(Array(1, 2, 3), 3, 2)
      assert(res == List(1, 2, 3))
    }
  }
  describe("215. Kth Largest Element in an Array") {
    it("example 1 - nums [3,2,1,5,6,4], k = 2 should return 5") {
      assert(Heaps.findKthLargest(Array(3, 2, 1, 5, 6, 4), 2) == 5)
    }
    it("example 2 - nums [3,2,3,1,2,4,5,5,6], k = 4 should return 4") {
      assert(Heaps.findKthLargest(Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4) == 4)
    }
    it("single element array returns that element") {
      assert(Heaps.findKthLargest(Array(42), 1) == 42)
    }
    it("all duplicates returns the same value") {
      assert(Heaps.findKthLargest(Array(1, 1, 1, 1), 2) == 1)
    }
    it("negative numbers and k = 1 returns max (least negative)") {
      assert(Heaps.findKthLargest(Array(-4, -1, -3, -2), 1) == -1)
    }
    it("k equals array length returns minimum element") {
      assert(Heaps.findKthLargest(Array(7, 10, 4), 3) == 4)
    }
  }
}
