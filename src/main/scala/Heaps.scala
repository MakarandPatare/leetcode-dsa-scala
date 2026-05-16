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
}
