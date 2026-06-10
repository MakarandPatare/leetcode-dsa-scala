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
  // https://leetcode.com/problems/find-median-from-data-stream/description/
  // No tests for this problem
  object MedianFinder {
    val maxHeap: mutable.PriorityQueue[Int] = mutable.PriorityQueue()
    val minHeap: mutable.PriorityQueue[Int] = mutable.PriorityQueue.empty(Ordering.Int.reverse)

    def addNum(num: Int): Unit = {
      maxHeap.enqueue(num)
      minHeap.enqueue(maxHeap.dequeue())
      if (minHeap.size > maxHeap.size) {
        maxHeap.enqueue(minHeap.dequeue())
      }
    }
    def findMedian(): Double = {
      if (maxHeap.size > minHeap.size) maxHeap.head
      else (minHeap.head + maxHeap.head) / 2.0
    }
  }
  // https://leetcode.com/problems/remove-stones-to-minimize-the-total/editorial/
  def minStoneSum(piles: Array[Int], k: Int): Int = {
    val maxHeap = mutable.PriorityQueue[Double]()
    piles.foreach(maxHeap.enqueue(_))
    for (_ <- 0 until k) maxHeap.enqueue(math.ceil(maxHeap.dequeue() / 2))
    maxHeap.sum.toInt
  }
}
