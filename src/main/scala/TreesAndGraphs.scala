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
}
