import org.scalatest.funspec.AnyFunSpec
import TreesAndGraphs.TreeNode

class TreesAndGraphsSpec extends AnyFunSpec {
  describe("104. maxDepth") {
    it("should return 0 for null tree") {
      val input: Null = null
      val expected = 0
      val output = TreesAndGraphs.maxDepth(input)
      assert(output == expected)
    }
    it("should return 1 for single node") {
      val input = TreeNode()
      val expected = 1
      val output = TreesAndGraphs.maxDepth(input)
      assert(output == expected)
    }
    it("should return correct output for 2 levels deep tree") {
      val input = TreeNode(1, _right = TreeNode(2))
      val expected = 2
      val output = TreesAndGraphs.maxDepth(input)
      assert(output == expected)
    }
    it("should return correct output for multi-level tree") {
      val input = TreeNode(3, _left = TreeNode(9), _right = TreeNode(20, _left = TreeNode(12), _right = TreeNode(7)))
      val expected = 3
      val output = TreesAndGraphs.maxDepth(input)
      assert(output == expected)
    }
  }
  describe("112. hasPathSum") {
    it("should return correct output for case 1") {
      val inputRoot = TreeNode(5,
        _left = TreeNode(4,
          _left = TreeNode(11,
            _left = TreeNode(7),
            _right = TreeNode(2))),
        _right = TreeNode(8,
          _left = TreeNode(13),
          _right = TreeNode(4,
            _right = TreeNode(1))))
      val inputTargetSum = 22
      val expected = true
      val output = TreesAndGraphs.hasPathSum(inputRoot, inputTargetSum)
      assert(output == expected)
    }
    it("should return false for case 2") {
      val inputRoot = TreeNode(1, _left = TreeNode(2), _right = TreeNode(3))
      val inputTargetSum = 5
      val expected = false
      val output = TreesAndGraphs.hasPathSum(inputRoot, inputTargetSum)
      assert(output == expected)
    }
    it("should return false for case 3") {
      val inputRoot: Null = null
      val inputTargetSum = 0
      val expected = false
      val output = TreesAndGraphs.hasPathSum(inputRoot, inputTargetSum)
      assert(output == expected)
    }
  }
  describe("1448. goodNodes") {
    it("should return correct output for case 1") {
      val input = TreeNode(3,
        _left = TreeNode(1,
          _left = TreeNode(3)),
        _right = TreeNode(4,
          _left = TreeNode(1),
          _right = TreeNode(5))
      )
      val expected = 4
      val output = TreesAndGraphs.goodNodes(input)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val input = TreeNode(3,
          _left = TreeNode(3,
            _left = TreeNode(4),
            _right = TreeNode(2)))
      val expected = 3
      val output = TreesAndGraphs.goodNodes(input)
      assert(output == expected)
    }
    it("should return correct output for case 3") {
      val input = TreeNode(1)
      val expected = 1
      val output = TreesAndGraphs.goodNodes(input)
      assert(output == expected)
    }
  }
  describe("100. isSameTree") {
    it("should return correct output for case 1") {
      val input1 = TreeNode(1, _left = TreeNode(2), _right = TreeNode(3))
      val input2 = TreeNode(1, _left = TreeNode(2), _right = TreeNode(3))
      val expected = true
      val output = TreesAndGraphs.isSameTree(input1, input2)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val input1 = TreeNode(1, _left = TreeNode(2))
      val input2 = TreeNode(1, _right = TreeNode(2))
      val expected = false
      val output = TreesAndGraphs.isSameTree(input1, input2)
      assert(output == expected)
    }
    it("should return correct output for case 3") {
      val input1 = TreeNode(1, _left = TreeNode(2), _right = TreeNode(1))
      val input2 = TreeNode(1, _left = TreeNode(1), _right = TreeNode(2))
      val expected = false
      val output = TreesAndGraphs.isSameTree(input1, input2)
      assert(output == expected)
    }
  }
}
