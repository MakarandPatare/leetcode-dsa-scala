import scala.collection.mutable

object TreesAndGraphs {
  case class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }
  // https://leetcode.com/problems/maximum-depth-of-binary-tree/
  def maxDepth(root: TreeNode): Int = {
    if (root == null) 0 else 1 + math.max(maxDepth(root.left), maxDepth(root.right))
  }
  def maxDepthIterative(root: TreeNode): Int = {
    if (root == null) return 0
    val stack = mutable.Stack[(TreeNode, Int)]()
    stack.push((root, 1))
    var ans = 0
    while(stack.nonEmpty) {
      val (node, depth) = stack.pop()
      ans = math.max(ans, depth)

      if (node.left != null) stack.push((node.left, depth + 1))
      if (node.right != null) stack.push((node.right, depth + 1))
    }
    ans
  }
  // https://leetcode.com/problems/path-sum/
  def hasPathSum(root: TreeNode, targetSum: Int): Boolean =
    if (root == null) false
    else if (root.left == null && root.right == null)
      root.value == targetSum
    else hasPathSum(root.left, targetSum - root.value) || hasPathSum(root.right, targetSum - root.value)
  def hasPathSumIterative(root: TreeNode, targetSum: Int): Boolean = {
    if (root == null) return false
    val stack = mutable.Stack[(TreeNode, Int)]()
    stack.push((root, targetSum))
    while(stack.nonEmpty) {
      val (node, remaining) = stack.pop()
      if (node.left == null && node.right == null && node.value == remaining) return true
      if (node.left != null) stack.push((node.left, remaining - node.value))
      if (node.right != null) stack.push((node.right, remaining - node.value))
    }
    false
  }
  // https://leetcode.com/problems/count-good-nodes-in-binary-tree/
  def goodNodes(root: TreeNode): Int = {
    def dfs(root: TreeNode, currentMax: Int): Int = {
      if (root == null) 0
      else {
        val newMax = math.max(root.value, currentMax)
        val leftRightSum = dfs(root.left, newMax) + dfs(root.right, newMax)
        if (root.value >= currentMax)
          leftRightSum + 1
        else
          leftRightSum
      }
    }
    if (root == null) 0
    else dfs(root, Int.MinValue)
  }
  // https://leetcode.com/problems/same-tree/
  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    if (p == null && q == null) true
    else if (p == null || q == null || p.value != q.value) false
    else isSameTree(p.left, q.left) && isSameTree(p.right, q.right)
  }
  // https://leetcode.com/problems/lowest-common-ancestor-of-a-binary-tree/
  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    if (root == null) null
    else if (root.value == p.value || root.value == q.value) root
    else {
      val left = lowestCommonAncestor(root.left, p, q)
      val right = lowestCommonAncestor(root.right, p, q)
      if (left != null && right != null) root
      else if (left != null) left
      else right
    }
  }
  // https://leetcode.com/explore/interview/card/leetcodes-interview-crash-course-data-structures-and-algorithms/707/traversals-trees-graphs/4691/
  def minDepth(root: TreeNode): Int = {
    if (root == null) 0
    else {
      (root.left, root.right) match {
        case (null, null) => 1
        case (left, null) => minDepth(left) + 1
        case (null, right) => minDepth(right) + 1
        case (left, right) => math.min(minDepth(left), minDepth(right)) + 1
      }
    }
  }
  // https://leetcode.com/explore/featured/card/leetcodes-interview-crash-course-data-structures-and-algorithms/707/traversals-trees-graphs/4617/
  def maxAncestorDiff(root: TreeNode): Int = {
    def loop(root: TreeNode, minVal: Int, maxVal: Int): Int = {
      if (root == null) math.abs(maxVal - minVal)
      else math.max(loop(root.left, math.min(minVal, root.value), math.max(maxVal, root.value)),
                    loop(root.right, math.min(minVal, root.value), math.max(maxVal, root.value)))
    }
    loop(root, root.value, root.value)
  }
  // https://leetcode.com/problems/diameter-of-binary-tree/description/
  def diameterOfBinaryTree(root: TreeNode): Int = {
    var diameter = 0
    def dfs(root: TreeNode): Int = {
      if (root == null) 0
      else {
        val leftDepth = dfs(root.left)
        val rightDepth = dfs(root.right)
        diameter = math.max(diameter, leftDepth + rightDepth)
        math.max(leftDepth, rightDepth) + 1
      }
    }
    dfs(root)
    diameter
  }
  // https://leetcode.com/problems/binary-tree-right-side-view/description/
  def rightSideView(root: TreeNode): List[Int] = {
    if (root == null) return Nil
    val queue = scala.collection.mutable.Queue[TreeNode]()
    val ans = scala.collection.mutable.ArrayBuffer[Int]()
    queue.enqueue(root)
    var lastNode = root
    while(queue.nonEmpty) {
      for (_ <- queue.indices) {
        lastNode = queue.dequeue()
        if (lastNode.left != null) queue.enqueue(lastNode.left)
        if (lastNode.right != null) queue.enqueue(lastNode.right)
      }
      ans += lastNode.value
    }
    ans.toList
  }
  // https://leetcode.com/problems/find-largest-value-in-each-tree-row/description/
  def largestValues(root: TreeNode): List[Int] = {
    if (root == null) return Nil
    val queue = scala.collection.mutable.Queue[TreeNode]()
    val ans = scala.collection.mutable.ArrayBuffer[Int]()
    queue.enqueue(root)
    while(queue.nonEmpty) {
      var curMax = Int.MinValue
      for (_ <- queue.indices) {
        val curNode = queue.dequeue()
        curMax = curMax.max(curNode.value)
        if (curNode.left != null) queue.enqueue(curNode.left)
        if (curNode.right != null) queue.enqueue(curNode.right)
      }
      ans += curMax
    }
    ans.toList
  }
  // https://leetcode.com/problems/deepest-leaves-sum/description/
  def deepestLeavesSum(root: TreeNode): Int = {
    if (root == null) return 0
    val queue = scala.collection.mutable.Queue[TreeNode]()
    var ans = 0
    queue.enqueue(root)
    while(queue.nonEmpty) {
      var curSum = 0
      for (_ <- queue.indices) {
        val curNode = queue.dequeue()
        /* Unnecessary in this example
         * if (curNode.left == null && curNode.right == null)
         * followed by below line in else */
        curSum += curNode.value
        if (curNode.left != null) queue.enqueue(curNode.left)
        if (curNode.right != null) queue.enqueue(curNode.right)
      }
      ans = curSum
    }
    ans
  }
  // https://leetcode.com/problems/binary-tree-zigzag-level-order-traversal/description/
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null) return Nil
    val queue = scala.collection.mutable.Queue[TreeNode]()
    val ans = scala.collection.mutable.ArrayBuffer[List[Int]]()
    queue.enqueue(root)
    var leftToRight = true
    while(queue.nonEmpty) {
      val tempAns = scala.collection.mutable.ArrayBuffer[Int]()
      for (_ <- queue.indices) {
        val curNode = queue.dequeue()
        if (leftToRight) tempAns.append(curNode.value) else tempAns.prepend(curNode.value)
        if (curNode.left != null) queue.enqueue(curNode.left)
        if (curNode.right != null) queue.enqueue(curNode.right)
      }
      leftToRight = !leftToRight
      ans += tempAns.toList
    }
    ans.toList
  }
  // https://leetcode.com/problems/range-sum-of-bst/description/
  def rangeSumBST(root: TreeNode, low: Int, high: Int): Int = {
      if (root == null) 0
      else {
        var ans = 0
        if (root.value >= low && root.value <= high) ans += root.value
        if (root.value >= low) ans += rangeSumBST(root.left, low, high)
        if (root.value <= high) ans += rangeSumBST(root.right, low, high)
        ans
      }
  }
  // https://leetcode.com/problems/minimum-absolute-difference-in-bst/description/
  def getMinimumDifference(root: TreeNode): Int = {
    val array = scala.collection.mutable.ArrayBuffer[Int]()
    def dfs(root: TreeNode): Unit = {
      if (root != null) {
        dfs(root.left)
        array += root.value
        dfs(root.right)
      }
    }
    dfs(root)
    array.sliding(2).map(l => math.abs(l(0) - l(1))).min
  }
  // https://leetcode.com/problems/validate-binary-search-tree/description/
  def isValidBST(root: TreeNode): Boolean = {
    def dfs(root: TreeNode, small: Long, large: Long): Boolean = {
      if (root == null) true
      else {
        if (root.value <= small || root.value >= large) return false
        dfs(root.left, small, root.value) && dfs(root.right, root.value, large)
      }
    }
    dfs(root, Long.MinValue, Long.MaxValue)
  }
  // https://leetcode.com/problems/insert-into-a-binary-search-tree/description/
  def insertIntoBST(root: TreeNode, `val`: Int): TreeNode = {
    if (root == null) TreeNode(`val`)
    else {
      if (`val` > root.value) root.right = insertIntoBST(root.right, `val`)
      else root.left = insertIntoBST(root.left, `val`)
      root
    }
  }
  // https://leetcode.com/problems/closest-binary-search-tree-value/description/
  def closestValue(root: TreeNode, target: Double): Int = {
    var curRoot = root
    var closest = root.value
    var curVal = root.value
    while (curRoot != null) {
      curVal = curRoot.value
      closest = if (math.abs(curVal - target) < math.abs(closest - target) ||
        (math.abs(curVal - target) == math.abs(closest - target) && curVal < closest)) curVal
      else closest
      curRoot = if (target < curRoot.value) curRoot.left else curRoot.right
    }
    closest
  }
  // https://leetcode.com/problems/number-of-provinces/description/
  // TODO: Better solutions exist for this problem. To be revisited
  def findCircleNum(isConnected: Array[Array[Int]]): Int = {
    val n = isConnected.length
    val graph = mutable.Map[Int, mutable.ArrayBuffer[Int]]()
    val seen = new Array[Boolean](n)
    // Build the graph
    for (i <- 0 until n) {
      if (!graph.contains(i)) graph(i) = mutable.ArrayBuffer[Int]()
      for (j <- i + 1 until n) {
        if (!graph.contains(j)) graph(j) = mutable.ArrayBuffer[Int]()
        if (isConnected(i)(j) == 1) {
          graph(i) += j
          graph(j) += i
        }
      }
    }
    def dfs(node: Int): Unit = {
      for (neighbor <- graph(node)) {
        if (!seen(neighbor)) {
          seen(neighbor) = true
          dfs(neighbor)
        }
      }
    }
    var ans = 0
    for (i <- 0 until n) {
      if (!seen(i)) {
        ans += 1
        seen(i) = true
        dfs(i)
      }
    }
    ans
  }
  // https://leetcode.com/problems/number-of-islands/description/
  def numIslands(grid: Array[Array[Char]]): Int = {
    val m = grid.length
    val n = grid(0).length
    val directions = Array((0, 1), (1, 0), (0, -1), (-1, 0))
    val seen = Array.ofDim[Boolean](m, n)
    
    def valid(row: Int, col: Int): Boolean = {
      0 <= row && row < m && 0 <= col && col < n && grid(row)(col) == '1'
    }
    
    def dfs(row: Int, col: Int): Unit = {
      for ((dRow, dCol) <- directions) {
        val nextRow = row + dRow
        val nextCol = col + dCol
        if (valid(nextRow, nextCol) && !seen(nextRow)(nextCol)) {
          seen(nextRow)(nextCol) = true
          dfs(nextRow, nextCol)
        }
      }
    }

    var ans = 0
    for (row <- 0 until m; col <- 0 until n) {
      if (!seen(row)(col) && grid(row)(col) == '1') {
        ans += 1
        seen(row)(col) = true
        dfs(row, col)
      }
    }

    ans
  }
  // https://leetcode.com/problems/reorder-routes-to-make-all-paths-lead-to-the-city-zero/description/
  def minReorder(n: Int, connections: Array[Array[Int]]): Int = {
    // Define undirected Graph, set of directed Paths and set of seen paths
    case class Path(from: Int, to: Int)
    val graph = mutable.Map[Int, mutable.ArrayBuffer[Int]]()
    val paths = mutable.Set[Path]()
    val seen = mutable.Set[Int]()

    // Initialize and populate graph and paths
    for (i <- 0 until n) {
      graph(i) = mutable.ArrayBuffer[Int]()
    }

    for (connection <- connections) {
      val x = connection(0)
      val y = connection(1)
      graph(x) += y
      graph(y) += x
      paths += Path(x, y)
    }

    // DFS through each unseen neighbor of node in graph
    def dfs(node: Int): Int = {
      var count = 0
      for (neighbor <- graph(node)) {
        if(!seen(neighbor)) {
          if (paths.contains(Path(node, neighbor))) count += 1
          seen += neighbor
          count += dfs(neighbor)
        }
      }
      count
    }

    // Mark 0 seen to avoid cycles
    seen += 0
    dfs(0)
  }
  // https://leetcode.com/problems/keys-and-rooms/description/
  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
    val seen = mutable.Set[Int]()
    seen += 0
    dfs(0)
    
    def dfs(node: Int): Unit = {
      for (neighbor <- rooms(node)) {
        if (!seen.contains(neighbor)) {
          seen += neighbor
          dfs(neighbor)
        }
      }
    }
    seen.size == rooms.size
  }
  // https://leetcode.com/problems/minimum-number-of-vertices-to-reach-all-nodes/description/
  def findSmallestSetOfVertices(n: Int, edges: List[List[Int]]): List[Int] = {
    val indegree = new Array[Int](n)
    for (edge <- edges) {
      indegree(edge(1)) += 1
    }
    indegree.indices.filter(indegree(_) == 0).toList
  }
}
