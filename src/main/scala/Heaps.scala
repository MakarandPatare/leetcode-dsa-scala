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
  // https://leetcode.com/problems/top-k-frequent-elements/description/
  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    val counts = mutable.Map[Int, Int]()
    nums.foreach(n => counts.update(n, counts.getOrElse(n, 0) + 1))
    val ord: Ordering[Int] = Ordering.by[Int, Int](n => -counts.getOrElse(n, 0))
    val minHeap = mutable.PriorityQueue.empty[Int](ord)
    counts.keys.foreach { ck =>
      minHeap.enqueue(ck)
      if (minHeap.size > k) minHeap.dequeue()
    }
    minHeap.toArray
  }
  // https://leetcode.com/problems/find-k-closest-elements/description/
  def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] = {
    // tuple-based ordering. Tuple = (distance, value).
    // When distances are equal, the second element `n` is compared
    val ord: Ordering[Int] = Ordering.by[Int, (Int, Int)](n => (Math.abs(n - x), n))
    val heap = mutable.PriorityQueue.empty[Int](ord)
    arr.foreach{ a =>
      heap.enqueue(a)
      if (heap.size > k) heap.dequeue()
    }
    heap.toList.sorted
  }
  // https://leetcode.com/problems/kth-largest-element-in-an-array/description/
  def findKthLargest(nums: Array[Int], k: Int): Int = {
    val heap = mutable.PriorityQueue.empty[Int]((a, b) => b - a)
    nums.foreach { n =>
      heap.enqueue(n)
      if (heap.size > k) heap.dequeue()
    }
    heap.head
  }
  // https://leetcode.com/problems/k-closest-points-to-origin/description/
  def kClosest(points: Array[Array[Int]], k: Int): Array[Array[Int]] = {
    val heap = scala.collection.mutable.PriorityQueue.empty[Array[Int]]((a, b) => (a(0) * a(0) + a(1) * a(1)) - (b(0) * b(0) + b(1) * b(1)))
    points.foreach { p =>
      heap.enqueue(p)
      if (heap.size > k) heap.dequeue()
    }
    heap.toArray
  }
  // https://leetcode.com/problems/kth-largest-element-in-a-stream/description/
  // No tests for this problem
  class KthLargest(_k: Int, _nums: Array[Int]) {
    private val heap = scala.collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)
    _nums.foreach(add)
    def add(`val`: Int): Int = {
      if (heap.size < _k || heap.head < `val`) {
        heap.enqueue(`val`)
        if (heap.size > _k) heap.dequeue()
      }
      heap.head
    }
  }
}
