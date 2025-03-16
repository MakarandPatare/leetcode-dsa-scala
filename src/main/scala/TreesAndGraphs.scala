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
}
