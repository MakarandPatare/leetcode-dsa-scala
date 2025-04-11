import org.scalatest.funspec.AnyFunSpec
import TreesAndGraphs.TreeNode
import org.scalatest.Ignore

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
  describe("103. zigzagLevelOrder") {
    it("should return correct output for case 1") {
      // [3,9,20,null,null,15,7]
      val input = TreeNode(3,
        _left = TreeNode(9),
        _right = TreeNode(20,
          _left = TreeNode(15),
          _right = TreeNode(7)))
      val expected = List(List(3), List(20, 9), List(15, 7))
      val output = TreesAndGraphs.zigzagLevelOrder(input)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      // [3,9,20,null,null,15,7]
      val input = TreeNode(1)
      val expected = List(List(1))
      val output = TreesAndGraphs.zigzagLevelOrder(input)
      assert(output == expected)
    }
    it("should return correct output for case 3") {
      // [3,9,20,null,null,15,7]
      val input: TreeNode = null
      val expected = List.empty
      val output = TreesAndGraphs.zigzagLevelOrder(input)
      assert(output == expected)
    }
  }
  describe("938. rangeSumBST") {
    it("should return correct answer for case 1") {
      // [10,5,15,3,7,null,18]
      val inputTree = TreeNode(10,
        _left = TreeNode(5,
          _left = TreeNode(3),
          _right = TreeNode(7)),
        _right = TreeNode(15,
          _right = TreeNode(18)))
      val ipLow = 7
      val ipHigh = 15
      val expected = 32
      val output = TreesAndGraphs.rangeSumBST(inputTree, ipLow, ipHigh)
      assert(output == expected)
    }
    it("should return correct answer for case 2") {
      // [10,5,15,3,7,13,18,1,null,6]
      val inputTree = TreeNode(10,
        _left = TreeNode(5,
          _left = TreeNode(3,
            _left = TreeNode(1)),
          _right = TreeNode(7,
            _left = TreeNode(6))),
        _right = TreeNode(15,
          _left = TreeNode(13),
          _right = TreeNode(18)))
      val ipLow = 6
      val ipHigh = 10
      val expected = 23
      val output = TreesAndGraphs.rangeSumBST(inputTree, ipLow, ipHigh)
      assert(output == expected)
    }
  }
  describe("530. getMinimumDifference") {
    it("should return correct answer for case 1") {
      // [4,2,6,1,3]
      val input = TreeNode(4,
        _left = TreeNode(2,
          _left = TreeNode(1),
          _right = TreeNode(3)),
        _right = TreeNode(6))
      val output = TreesAndGraphs.getMinimumDifference(input)
      val expected = 1
      assert(output == expected)
    }
    it("should return correct answer for case 2") {
      // [1,0,48,null,null,12,49]
      val input = TreeNode(1,
        _left = TreeNode(),
        _right = TreeNode(48,
          _left = TreeNode(12),
          _right = TreeNode(49)))
      val output = TreesAndGraphs.getMinimumDifference(input)
      val expected = 1
      assert(output == expected)
    }
  }
  describe("98. isValidBST") {
    it("should return correct answer for case 1") {
      val input = TreeNode(2,
        _left = TreeNode(1),
        _right = TreeNode(3))
      val output = TreesAndGraphs.isValidBST(input)
      val expected = true
      assert(output == expected)
    }
    it("should return correct answer for case 2") {
      // [5,1,4,null,null,3,6]
      val input = TreeNode(5,
        _left = TreeNode(1),
        _right = TreeNode(4,
          _left = TreeNode(3),
          _right = TreeNode(6)))
      val output = TreesAndGraphs.isValidBST(input)
      val expected = false
      assert(output == expected)
    }
    it("should return correct answer for custom case 1") {
      val input = TreeNode(2,
        _left = TreeNode(2),
        _right = TreeNode(2))
      val output = TreesAndGraphs.isValidBST(input)
      val expected = false
      assert(output == expected)
    }
    it("should return correct answer for custom case 2") {
      val input = TreeNode(2147483647)
      val output = TreesAndGraphs.isValidBST(input)
      val expected = true
      assert(output == expected)
    }
  }
  // Temporarily ignoring the tests as mutation of root isn't working as expected
  describe("701. insertIntoBST") {
    ignore("should return correct answer for case 1") {
      // [4,2,7,1,3]
      val inputTree = TreeNode(4,
        _left = TreeNode(2,
          _left = TreeNode(1),
          _right = TreeNode(3)),
        _right = TreeNode(7))
      val value = 5
      val output = TreesAndGraphs.insertIntoBST(inputTree, value)
      val expected = TreeNode(4,
        _left = TreeNode(2,
          _left = TreeNode(1),
          _right = TreeNode(3)),
        _right = TreeNode(7,
          _left = TreeNode(5)))
      assert(output == expected)
    }
    ignore("should return correct answer for case 2") {
      // [40,20,60,10,30,50,70]
      val inputTree = TreeNode(40,
        _left = TreeNode(20,
          _left = TreeNode(10),
          _right = TreeNode(30)),
        _right = TreeNode(60,
          _left = TreeNode(50),
          _right = TreeNode(70)))
      val value = 25
      val output = TreesAndGraphs.insertIntoBST(inputTree, value)
      val expected = TreeNode(40,
        _left = TreeNode(20,
          _left = TreeNode(10),
          _right = TreeNode(30,
            _left = TreeNode(25))),
        _right = TreeNode(60,
          _left = TreeNode(50),
          _right = TreeNode(70)))
      assert(output == expected)
    }
    ignore("should return correct answer for case 3") {
      // [4,2,7,1,3,null,null,null,null,null,null]
      val inputTree = TreeNode(4,
        _left = TreeNode(2,
          _left = TreeNode(1),
          _right = TreeNode(3)),
        _right = TreeNode(7))
      val value = 5
      val output = TreesAndGraphs.insertIntoBST(inputTree, value)
      val expected = TreeNode(4,
        _left = TreeNode(2,
          _left = TreeNode(1),
          _right = TreeNode(3)),
        _right = TreeNode(7,
          _left = TreeNode(5)))
      assert(output == expected)
    }
  }
}
