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
  describe("236. lowestCommonAncestor") {
    it("should return correct output for case 1") {
      val inputTree = TreeNode(3,
        _left = TreeNode(5,
          _left = TreeNode(6),
          _right = TreeNode(2,
            _left = TreeNode(7),
            _right = TreeNode(4))),
        _right = TreeNode(1,
          _left = TreeNode(),
          _right = TreeNode(8)))
      val p = TreeNode(5)
      val q = TreeNode(1)
      val expected = TreeNode(3, TreeNode(5, TreeNode(6, null, null), TreeNode(2, TreeNode(7, null, null), TreeNode(4, null, null))), TreeNode(1, TreeNode(0, null, null), TreeNode(8, null, null)))
      val output = TreesAndGraphs.lowestCommonAncestor(inputTree, p, q)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val inputTree = TreeNode(3,
        _left = TreeNode(5,
          _left = TreeNode(6),
          _right = TreeNode(2,
            _left = TreeNode(7),
            _right = TreeNode(4))),
        _right = TreeNode(1,
          _left = TreeNode(),
          _right = TreeNode(8)))
      val p = TreeNode(5)
      val q = TreeNode(4)
      val expected = TreeNode(5, TreeNode(6, null, null), TreeNode(2, TreeNode(7, null, null), TreeNode(4, null, null)))
      val output = TreesAndGraphs.lowestCommonAncestor(inputTree, p, q)
      assert(output == expected)
    }
    it("should return correct output for case 3") {
      val inputTree = TreeNode(1, _left = TreeNode(2))
      val p = TreeNode(1)
      val q = TreeNode(2)
      val expected = TreeNode(1, TreeNode(2, null, null), null)
      val output = TreesAndGraphs.lowestCommonAncestor(inputTree, p, q)
      assert(output == expected)
    }
  }

  describe("4691. minDepth") {
    it("should return correct output for case 1") {
      val input = TreeNode(3,
        _left = TreeNode(9),
        _right = TreeNode(20,
          _left = TreeNode(15),
          _right = TreeNode(7)))
      val expected = 2
      val output = TreesAndGraphs.minDepth(input)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val input = TreeNode(2,
        _right = TreeNode(3,
          _right = TreeNode(4,
            _right = TreeNode(5,
              _right = TreeNode(6)))))
      val expected = 5
      val output = TreesAndGraphs.minDepth(input)
      assert(output == expected)
    }
  }
  describe("4617. maxAncestorDiff") {
    it("should return correct output for case 1") {
      //[8,3,10,1,6,null,14,null,null,4,7,13]
      val input = TreeNode(8,
        _left = TreeNode(3,
          _left = TreeNode(1),
          _right = TreeNode(6,
            _left = TreeNode(4),
            _right = TreeNode(7))),
        _right = TreeNode(10,
          _right = TreeNode(14,
            _left = TreeNode(13))))
      val expected = 7
      val output = TreesAndGraphs.maxAncestorDiff(input)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      //[1,null,2,null,0,3]
      val input = TreeNode(1,
        _right = TreeNode(2,
          _right = TreeNode(
            _left = TreeNode(3))))
      val expected = 3
      val output = TreesAndGraphs.maxAncestorDiff(input)
      assert(output == expected)
    }
  }
  describe("543. diameterOfBinaryTree") {
    it("should return correct output for case 1") {
      val input = TreeNode(1,
        _left = TreeNode(2,
          _left = TreeNode(4),
          _right = TreeNode(5)),
        _right = TreeNode(3))
      val expected = 3
      val output = TreesAndGraphs.diameterOfBinaryTree(input)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val input = TreeNode(1,
        _left = TreeNode(2))
      val expected = 1
      val output = TreesAndGraphs.diameterOfBinaryTree(input)
      assert(output == expected)
    }
  }
  describe("199. Binary Tree Right Side View") {
    it("Should return correct output for case 1") {
      // [1,2,3,null,5,null,4]
      val input = TreeNode(1,
        _left = TreeNode(2,
          _right = TreeNode(5)),
        _right = TreeNode(3,
          _right = TreeNode(4)))
      val expected = List(1,3,4)
      val output = TreesAndGraphs.rightSideView(input)
      assert(output == expected)
    }
    it("Should return correct output for case 2") {
      // [1,2,3,4,null,null,null,5]
      val input = TreeNode(1,
        _left = TreeNode(2,
          _left = TreeNode(4,
            _left = TreeNode(5))),
        _right = TreeNode(3))
      val expected = List(1,3,4,5)
      val output = TreesAndGraphs.rightSideView(input)
      assert(output == expected)
    }
    it("Should return correct output for case 3") {
      // [1,null,3]
      val input = TreeNode(1,
          _right = TreeNode(3))
      val expected = List(1, 3)
      val output = TreesAndGraphs.rightSideView(input)
      assert(output == expected)
    }
    it("Should return correct output for case 4") {
      // [1,2,3,null,5,null,4]
      val input: TreeNode = null
      val expected = List.empty
      val output = TreesAndGraphs.rightSideView(input)
      assert(output == expected)
    }
  }
  describe("515. largestValues") {
    it("should return correct output for case 1") {
      // [1,3,2,5,3,null,9]
      val input = TreeNode(1,
      _left = TreeNode(3,
        _left = TreeNode(5),
        _right = TreeNode(3)),
        _right = TreeNode(2,
          _right = TreeNode(9)))
      val expected = List(1,3,9)
      val output = TreesAndGraphs.largestValues(input)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      // [1,2,3]
      val input = TreeNode(1,
        _left = TreeNode(2),
        _right = TreeNode(3))
      val expected = List(1, 3)
      val output = TreesAndGraphs.largestValues(input)
      assert(output == expected)
    }
  }
  describe("1302. deepestLeavesSum") {
    it("should return correct output for case 1") {
      // [1,2,3,4,5,null,6,7,null,null,null,null,8]
      val input = TreeNode(1,
        _left = TreeNode(2,
          _left = TreeNode(4,
            _left = TreeNode(7)),
          _right = TreeNode(5)),
        _right = TreeNode(3,
          _right = TreeNode(6,
            _right = TreeNode(8))))
      val expected = 15
      val output = TreesAndGraphs.deepestLeavesSum(input)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      // [6,7,8,2,7,1,3,9,null,1,4,null,null,null,5]
      val input = TreeNode(6,
        _left = TreeNode(7,
          _left = TreeNode(2,
            _left = TreeNode(9)),
          _right = TreeNode(7,
            _left = TreeNode(1),
            _right = TreeNode(4))),
        _right = TreeNode(8,
          _left = TreeNode(1),
          _right = TreeNode(3,
            _right = TreeNode(5))
        ))
      val expected = 19
      val output = TreesAndGraphs.deepestLeavesSum(input)
      assert(output == expected)
    }
  }
}
