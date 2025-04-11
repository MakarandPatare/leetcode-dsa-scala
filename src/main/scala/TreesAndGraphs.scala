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
  // https://leetcode.com/problems/path-sum/
  def hasPathSum(root: TreeNode, targetSum: Int): Boolean =
    if (root == null) false
    else if (root.left == null && root.right == null)
      root.value == targetSum
    else hasPathSum(root.left, targetSum - root.value) || hasPathSum(root.right, targetSum - root.value)
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
        if (curNode.left == null && curNode.right == null) curSum += curNode.value
        else {
          if (curNode.left != null) queue.enqueue(curNode.left)
          if (curNode.right != null) queue.enqueue(curNode.right)
        }
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
}
