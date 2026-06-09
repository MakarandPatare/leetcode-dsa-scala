import scala.collection.mutable
object Heaps {
  // https://leetcode.com/problems/last-stone-weight/description/
  def lastStoneWeight(stones: Array[Int]): Int = {
    val heap = mutable.PriorityQueue[Int]()
    stones.foreach(heap.enqueue(_))
    while (heap.size > 1) {
      val (first, second) = (heap.dequeue(), heap.dequeue())
      if (first != second) heap.enqueue(first - second)
    }
    if (heap.isEmpty) 0 else heap.dequeue()
  }
  // https://leetcode.com/problems/minimum-operations-to-halve-array-sum/description/
  def halveArray(nums: Array[Int]): Int = {
    val heap = mutable.PriorityQueue[Double]()
    var target: Double = 0
    nums.foreach(num =>
      target += num
      heap.enqueue(num)
    )
    target /= 2
    var ans = 0
    while(target > 0) {
      ans += 1
      val maxHalf = heap.dequeue() / 2
      target -= maxHalf
      heap.enqueue(maxHalf)
    }
    ans
  }
}
