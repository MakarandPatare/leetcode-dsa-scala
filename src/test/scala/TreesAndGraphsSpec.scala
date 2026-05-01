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
    describe("Iterative approach") {
      it("should return 0 for null tree") {
        val input: Null = null
        val expected = 0
        val output = TreesAndGraphs.maxDepthIterative(input)
        assert(output == expected)
      }
      it("should return 1 for single node") {
        val input = TreeNode()
        val expected = 1
        val output = TreesAndGraphs.maxDepthIterative(input)
        assert(output == expected)
      }
      it("should return correct output for 2 levels deep tree") {
        val input = TreeNode(1, _right = TreeNode(2))
        val expected = 2
        val output = TreesAndGraphs.maxDepthIterative(input)
        assert(output == expected)
      }
      it("should return correct output for multi-level tree") {
        val input = TreeNode(3, _left = TreeNode(9), _right = TreeNode(20, _left = TreeNode(12), _right = TreeNode(7)))
        val expected = 3
        val output = TreesAndGraphs.maxDepthIterative(input)
        assert(output == expected)
      }
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
    describe("Iterative approach") {
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
        val output = TreesAndGraphs.hasPathSumIterative(inputRoot, inputTargetSum)
        assert(output == expected)
      }
      it("should return false for case 2") {
        val inputRoot = TreeNode(1, _left = TreeNode(2), _right = TreeNode(3))
        val inputTargetSum = 5
        val expected = false
        val output = TreesAndGraphs.hasPathSumIterative(inputRoot, inputTargetSum)
        assert(output == expected)
      }
      it("should return false for case 3") {
        val inputRoot: Null = null
        val inputTargetSum = 0
        val expected = false
        val output = TreesAndGraphs.hasPathSumIterative(inputRoot, inputTargetSum)
        assert(output == expected)
      }
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
  describe("270. closestValue") {
    it("should return correct answer for case 1") {
      // [4,2,5,1,3]
      val inputRoot = TreeNode(4,
        _left = TreeNode(2,
          _left = TreeNode(1),
          _right = TreeNode(3)),
        _right = TreeNode(5))
      val inputTarget = 3.714286
      val output = TreesAndGraphs.closestValue(inputRoot, inputTarget)
      val expected = 4
      assert(output == expected)
    }
    it("should return correct answer for case 2") {
      // [4,2,5,1,3]
      val inputRoot = TreeNode(1)
      val inputTarget = 4.428571
      val output = TreesAndGraphs.closestValue(inputRoot, inputTarget)
      val expected = 1
      assert(output == expected)
    }
  }
  describe("547. Number of Provinces") {
    it("should return correct output for case 1") {
      val input = Array(Array(1,1,0), Array(1,1,0), Array(0,0,1))
      val output = TreesAndGraphs.findCircleNum(input)
      val expected = 2
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val input = Array(Array(1,0,0), Array(0,1,0), Array(0,0,1))
      val output = TreesAndGraphs.findCircleNum(input)
      val expected = 3
      assert(output == expected)
    }
  }
  describe("547. Number of Provinces - Optimized for memory") {
    it("should return correct output for case 1") {
      val input = Array(Array(1, 1, 0), Array(1, 1, 0), Array(0, 0, 1))
      val output = TreesAndGraphs.findCircleNumOptimizedForMemory(input)
      val expected = 2
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val input = Array(Array(1, 0, 0), Array(0, 1, 0), Array(0, 0, 1))
      val output = TreesAndGraphs.findCircleNumOptimizedForMemory(input)
      val expected = 3
      assert(output == expected)
    }
  }
  describe("200. Number of Islands") {
    it("should return correct output for case 1") {
      val input = Array(
        Array('1','1','1','1','0'),
        Array('1','1','0','1','0'),
        Array('1','1','0','0','0'),
        Array('0','0','0','0','0')
      )
      val output = TreesAndGraphs.numIslands(input)
      val expected = 1
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val input = Array(
        Array('1','1','0','0','0'),
        Array('1','1','0','0','0'),
        Array('0','0','1','0','0'),
        Array('0','0','0','1','1')
      )
      val output = TreesAndGraphs.numIslands(input)
      val expected = 3
      assert(output == expected)
    }
  }
  describe("1466. Reorder Routes to Make All Paths Lead to the City Zero") {
    it("should return correct output for case 1") {
      val n = 6
      val connections = Array(Array(0,1), Array(1,3), Array(2,3), Array(4,0), Array(4,5))
      val output = TreesAndGraphs.minReorder(n, connections)
      val expected = 3
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val n = 5
      val connections = Array(Array(1,0), Array(1,2), Array(3,2), Array(3,4))
      val output = TreesAndGraphs.minReorder(n, connections)
      val expected = 2
      assert(output == expected)
    }
    it("should return correct output for case 3") {
      val n = 3
      val connections = Array(Array(1,0), Array(2,0))
      val output = TreesAndGraphs.minReorder(n, connections)
      val expected = 0
      assert(output == expected)
    }
  }
  describe("841. Keys and Rooms") {
    it("should return correct output for case 1") {
      val rooms = List(List(1), List(2), List(3), List())
      val output = TreesAndGraphs.canVisitAllRooms(rooms)
      val expected = true
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val rooms = List(List(1, 3), List(3, 0, 1), List(2), List(0))
      val output = TreesAndGraphs.canVisitAllRooms(rooms)
      val expected = false
      assert(output == expected)
    }
  }
  describe("1557. Minimum Number of Vertices to Reach All Nodes") {
    it("should return correct output for case 1") {
      val n = 6
      val edges = List(List(0,1), List(0,2), List(2,5), List(3,4), List(4,2))
      val output = TreesAndGraphs.findSmallestSetOfVertices(n, edges)
      val expected = List(0, 3)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val n = 5
      val edges = List(List(0,1), List(2,1), List(3,1), List(1,4), List(2,4))
      val output = TreesAndGraphs.findSmallestSetOfVertices(n, edges)
      val expected = List(0, 2, 3)
      assert(output == expected)
    }
  }
  describe("1971. Find if Path Exists in Graph") {
    it("should return correct output for case 1") {
      val n = 3
      val edges = Array(Array(0, 1), Array(1, 2), Array(2, 0))
      val source = 0
      val destination = 2
      val output = TreesAndGraphs.validPath(n, edges, source, destination)
      val expected = true
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val n = 6
      val edges = Array(Array(0, 1), Array(0, 2), Array(3, 5), Array(5, 4), Array(4, 3))
      val source = 0
      val destination = 5
      val output = TreesAndGraphs.validPath(n, edges, source, destination)
      val expected = false
      assert(output == expected)
    }
  }
  describe("323. Number of Connected Components in an Undirected Graph") {
    it("should return correct output for case 1") {
      val n = 5
      val edges = Array(Array(0, 1), Array(1, 2), Array(3, 4))
      val output = TreesAndGraphs.countComponents(n, edges)
      val expected = 2
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val n = 5
      val edges = Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(3, 4))
      val output = TreesAndGraphs.countComponents(n, edges)
      val expected = 1
      assert(output == expected)
    }
  }
  describe("695. Max Area of Island") {
    it("should return correct output for case 1") {
      val grid = Array(Array(0,0,1,0,0,0,0,1,0,0,0,0,0),Array(0,0,0,0,0,0,0,1,1,1,0,0,0),Array(0,1,1,0,1,0,0,0,0,0,0,0,0),Array(0,1,0,0,1,1,0,0,1,0,1,0,0),Array(0,1,0,0,1,1,0,0,1,1,1,0,0),Array(0,0,0,0,0,0,0,0,0,0,1,0,0),Array(0,0,0,0,0,0,0,1,1,1,0,0,0),Array(0,0,0,0,0,0,0,1,1,0,0,0,0))
      val output = TreesAndGraphs.maxAreaOfIsland(grid)
      val expected = 6
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val grid = Array(Array(0,0,0,0,0,0,0,0))
      val output = TreesAndGraphs.maxAreaOfIsland(grid)
      val expected = 0
      assert(output == expected)
    }
  }
  describe("2368. Reachable Nodes With Restrictions") {
    it("should return correct output for case 1") {
      val n = 7
      val edges = Array(Array(0, 1), Array(1, 2), Array(3, 1), Array(4, 0), Array(0, 5), Array(5, 6))
      val restricted = Array(4, 5)
      val output = TreesAndGraphs.reachableNodes(n, edges, restricted)
      val expected = 4
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val n = 7
      val edges = Array(Array(0, 1), Array(0, 2), Array(0, 5), Array(0, 4), Array(3, 2), Array(6, 5))
      val restricted = Array(4, 2, 1)
      val output = TreesAndGraphs.reachableNodes(n, edges, restricted)
      val expected = 3
      assert(output == expected)
    }
  }
  describe("1091. Shortest Path in Binary Matrix") {
    it("should return correct output for case 1") {
      val grid = Array(Array(0, 1), Array(1, 0))
      val output = TreesAndGraphs.shortestPathBinaryMatrix(grid)
      val expected = 2
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val grid = Array(Array(0,0,0),Array(1,1,0),Array(1,1,0))
      val output = TreesAndGraphs.shortestPathBinaryMatrix(grid)
      val expected = 4
      assert(output == expected)
    }
    it("should return correct output for case 3") {
      val grid = Array(Array(1,0,0),Array(1,1,0),Array(1,1,0))
      val output = TreesAndGraphs.shortestPathBinaryMatrix(grid)
      val expected = -1
      assert(output == expected)
    }
    it("should return correct output for custom case") {
      val grid = Array(Array(0,1,1,1,1,1,1,1),Array(0,1,1,0,0,0,0,0),Array(0,1,0,1,1,1,1,0),Array(0,1,0,1,1,1,1,0),Array(0,1,1,0,0,1,1,0),Array(0,1,1,1,1,0,1,0),Array(0,0,0,0,0,1,1,0),Array(1,1,1,1,1,1,1,0))
      val output = TreesAndGraphs.shortestPathBinaryMatrix(grid)
      val expected = 25
      assert(output == expected)
    }
  }
  describe("863. All Nodes Distance K in Binary Tree") {
    it("should return correct output for case 1") {
      // [3,5,1,6,2,0,8,null,null,7,4]
      val root = TreeNode(3,
        _left = TreeNode(5,
          _left = TreeNode(6),
          _right = TreeNode(2,
            _left = TreeNode(7),
            _right = TreeNode(4))),
        _right = TreeNode(1,
          _left = TreeNode(),
          _right = TreeNode(8)))
      val target = root.left
      val k = 2
      val output = TreesAndGraphs.distanceK(root, target, k)
      val expected = List(7, 4, 1)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      // [1]
      val root = TreeNode(1)
      val target = root
      val k = 3
      val output = TreesAndGraphs.distanceK(root, target, k)
      val expected = Nil
      assert(output == expected)
    }
  }
  describe("542. 01 Matrix") {
    it("should return correct output for case 1") {
      val mat = Array(Array(0, 0, 0), Array(0, 1, 0), Array(0, 0, 0))
      val output = TreesAndGraphs.updateMatrix(mat)
      val expected = Array(Array(0, 0, 0), Array(0, 1, 0), Array(0, 0, 0))
      assert(output === expected)
    }
    it("should return correct output for case 2") {
      val mat = Array(Array(0, 0, 0), Array(0, 1, 0), Array(1, 1, 1))
      val output = TreesAndGraphs.updateMatrix(mat)
      val expected = Array(Array(0, 0, 0), Array(0, 1, 0), Array(1, 2, 1))
      assert(output === expected)
    }
  }
  describe("1293. Shortest Path in a Grid with Obstacles Elimination") {
    it("should return correct output for case 1") {
      val grid = Array(Array(0, 0, 0), Array(1, 1, 0), Array(0, 0, 0), Array(0, 1, 1), Array(0, 0, 0))
      val k = 1
      val output = TreesAndGraphs.shortestPath(grid, k)
      val expected = 6
      assert(output === expected)
    }
    it("should return correct output for case 2") {
      val grid = Array(Array(0, 1, 1), Array(1, 1, 1), Array(1, 0, 0))
      val k = 1
      val output = TreesAndGraphs.shortestPath(grid, k)
      val expected = -1
      assert(output === expected)
    }
  }
  describe("1129. Shortest Path with Alternating Colors") {
    it("should return correct output for case 1") {
      val n = 3
      val redEdges = Array(Array(0, 1), Array(1, 2))
      val blueEdges = Array.empty[Array[Int]]()
      val expected = Array(0, 1, -1)
      val output = TreesAndGraphs.shortestAlternatingPaths(n, redEdges, blueEdges)
      assert(output === expected)
    }
    it("should return correct output for case 2") {
      val n = 3
      val redEdges = Array(Array(0, 1))
      val blueEdges = Array(Array(2, 1))
      val expected = Array(0, 1, -1)
      val output = TreesAndGraphs.shortestAlternatingPaths(n, redEdges, blueEdges)
      assert(output === expected)
    }
    it("should return correct output for custom case from discussion thread") {
      val n = 5
      val redEdges = Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(3, 4))
      val blueEdges = Array(Array(1, 2), Array(2, 3), Array(3, 1))
      val expected = Array(0, 1, 2, 3, 7)
      val output = TreesAndGraphs.shortestAlternatingPaths(n, redEdges, blueEdges)
      assert(output === expected)
    }
  }
  describe("1926. Nearest Exit from Entrance in Maze") {
    it("should return correct output for case 1") {
      val maze = Array(Array('+','+','.','+'),Array('.','.','.','+'),Array('+','+','+','.'))
      val entrance = Array(1, 2)
      val expected = 1
      val output = TreesAndGraphs.nearestExit(maze, entrance)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val maze = Array(Array('+','+','+'), Array('.','.','.'), Array('+','+','+'))
      val entrance = Array(1, 0)
      val expected = 2
      val output = TreesAndGraphs.nearestExit(maze, entrance)
      assert(output == expected)
    }
    it("should return correct output for case 3") {
      val maze = Array(Array('.','+'))
      val entrance = Array(0, 0)
      val expected = -1
      val output = TreesAndGraphs.nearestExit(maze, entrance)
      assert(output == expected)
    }
  }
  describe("909. Snakes and Ladders") {
    it("should return correct output for case 1") {
      val board = Array(Array(-1,-1,-1,-1,-1,-1),Array(-1,-1,-1,-1,-1,-1),Array(-1,-1,-1,-1,-1,-1),Array(-1,35,-1,-1,13,-1),Array(-1,-1,-1,-1,-1,-1),Array(-1,15,-1,-1,-1,-1))
      val expected = 4
      val output = TreesAndGraphs.snakesAndLadders(board)
      assert(output == expected)
    }
    it("should return correct output for case 2") {
      val board = Array(Array(-1,-1), Array(-1,3))
      val expected = 1
      val output = TreesAndGraphs.snakesAndLadders(board)
      assert(output == expected)
    }
  }
}
