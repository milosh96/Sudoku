package engine

import model.Table
import org.junit._

class GameTest {

  //table used in tests
  //      4-129--75
  //      2--3--8-P
  //      -7--8---6
  //      ---1-3-62
  //      1-5---4-3
  //      73-6-8---
  //      6---2--3-
  //      --7--1--4
  //      89--651-7

  @Test def transposeTest(): Unit = {
    val nums: Array[Array[Int]] = Array(Array(4, 0, 1, 2, 9, 0, 0, 7, 5), Array(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Array(0, 7, 0, 0, 8, 0, 0, 0, 6), Array(0, 0, 0, 1, 0, 3, 0, 6, 2), Array(1, 0, 5, 0, 0, 0, 4, 0, 3), Array(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Array(6, 0, 0, 0, 2, 0, 0, 3, 0), Array(0, 0, 7, 0, 0, 1, 0, 0, 4), Array(8, 9, 0, 0, 6, 5, 1, 0, 7))
    val newTable = Game.transpose(new Table(nums))
    //      42--176-8
    //      --7--3--9
    //      1---5--7-
    //      23-1-6---
    //      9-8---2-6
    //      ---3-8-15
    //      -8--4--1
    //      7--6--3--
    //      5-623--47

    Assert.assertEquals(newTable.nums(0)(0), 4)
    Assert.assertEquals(newTable.nums(3)(0), 2)
    Assert.assertEquals(newTable.nums(1)(1), 0)
    Assert.assertEquals(newTable.nums(5)(8), 5)
    Assert.assertEquals(newTable.nums(8)(2), 6)
  }

  @Test def substituteTest(): Unit = {
    val nums: Array[Array[Int]] = Array(Array(4, 0, 1, 2, 9, 0, 0, 7, 5), Array(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Array(0, 7, 0, 0, 8, 0, 0, 0, 6), Array(0, 0, 0, 1, 0, 3, 0, 6, 2), Array(1, 0, 5, 0, 0, 0, 4, 0, 3), Array(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Array(6, 0, 0, 0, 2, 0, 0, 3, 0), Array(0, 0, 7, 0, 0, 1, 0, 0, 4), Array(8, 9, 0, 0, 6, 5, 1, 0, 7))
    val newTable = Game.substitute(new Table(nums))
    //      6-981--35
    //      8--7--2--
    //      -3--2---4
    //      ---9-7-48
    //      9-5---6-7
    //      37-4-2---
    //      4---8--7-
    //      --3--9--6
    //      21--459-3

    Assert.assertEquals(newTable.nums(0)(0), 6)
    Assert.assertEquals(newTable.nums(3)(0), 0)
    Assert.assertEquals(newTable.nums(1)(0), 8)
    Assert.assertEquals(newTable.nums(5)(1), 7)
    Assert.assertEquals(newTable.nums(8)(6), 9)
  }

  @Test def filterRnCTest(): Unit = {
    val nums: Array[Array[Int]] = Array(Array(4, 0, 1, 2, 9, 0, 0, 7, 5), Array(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Array(0, 7, 0, 0, 8, 0, 0, 0, 6), Array(0, 0, 0, 1, 0, 3, 0, 6, 2), Array(1, 0, 5, 0, 0, 0, 4, 0, 3), Array(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Array(6, 0, 0, 0, 2, 0, 0, 3, 0), Array(0, 0, 7, 0, 0, 1, 0, 0, 4), Array(8, 9, 0, 0, 6, 5, 1, 0, 7))

    //filter 8s from row 5 and column 4
    val newTable = Game.filterRnC(new Table(nums), 8, 5, 4)
    //      4-129--75
    //      2--3--8--
    //      -7--X---6
    //      ---1-3-62
    //      1-5---4-3
    //      73-6-X---
    //      6---2--3-
    //      --7--1--4
    //      89--651-7

    Assert.assertEquals(newTable.nums(0)(0), 4)
    Assert.assertEquals(newTable.nums(1)(6), 8)
    Assert.assertEquals(newTable.nums(6)(0), 6)
    //filtered
    Assert.assertEquals(newTable.nums(2)(4), 0)
    Assert.assertEquals(newTable.nums(5)(5), 0)
  }

  @Test def filterInnerGridTest(): Unit = {
    val nums: Array[Array[Int]] = Array(Array(4, 0, 1, 2, 9, 0, 0, 7, 5), Array(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Array(0, 7, 0, 0, 8, 0, 0, 0, 6), Array(0, 0, 0, 1, 0, 3, 0, 6, 2), Array(1, 0, 5, 3, 0, 0, 4, 0, 3), Array(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Array(6, 0, 0, 0, 2, 0, 0, 3, 0), Array(0, 0, 7, 0, 0, 1, 0, 0, 4), Array(8, 9, 0, 0, 6, 5, 1, 0, 7))
    //table
    //      4-129--75
    //      2--3--8--
    //      -7--8---6
    //      ---1-3-62
    //      1-53--4-3
    //      73-6-8---
    //      6---2--3-
    //      --7--1--4
    //      89--651-7

    //filter 3s from center grid
    val newTable = Game.filterInnerGrid(new Table(nums), 3, 4, 3)
    //new table
    //      4-129--75
    //      2--3--8--
    //      -7--8---6
    //      ---1-X-62
    //      1-5X--4-3
    //      73-6-8---
    //      6---2--3-
    //      --7--1--4
    //      89--651-7

    Assert.assertEquals(newTable.nums(0)(0), 4)
    Assert.assertEquals(newTable.nums(1)(6), 8)
    Assert.assertEquals(newTable.nums(6)(0), 6)
    //filtered
    Assert.assertEquals(newTable.nums(3)(5), 0)
    Assert.assertEquals(newTable.nums(4)(3), 0)
  }

  @Test def solveTest(): Unit = {
    val nums: Array[Array[Int]] = Array(Array(4, 0, 1, 2, 9, 0, 0, 7, 5), Array(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Array(0, 7, 0, 0, 8, 0, 0, 0, 6), Array(0, 0, 0, 1, 0, 3, 0, 6, 2), Array(1, 0, 5, 0, 0, 0, 4, 0, 3), Array(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Array(6, 0, 0, 0, 2, 0, 0, 3, 0), Array(0, 0, 7, 0, 0, 1, 0, 0, 4), Array(8, 9, 0, 0, 6, 5, 1, 0, 7))
    val unsolved: Table = new Table(nums)
    val solved: Table = Game.solveSudoku(unsolved)

    //solved table
    //    481296375
    //    256317849
    //    379584216
    //    948153762
    //    165972483
    //    732648951
    //    614729538
    //    527831694
    //    893465127
    Assert.assertEquals(solved.nums(0)(1), 8)
    Assert.assertEquals(solved.nums(0)(5), 6)
    Assert.assertEquals(solved.nums(3)(0), 9)
    Assert.assertEquals(solved.nums(8)(0), 8)
    Assert.assertEquals(solved.nums(8)(2), 3)
  }

  @Test def solveInvalidTest(): Unit = {
    val nums: Array[Array[Int]] = Array(Array(4, 4, 1, 2, 9, 0, 0, 7, 5), Array(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Array(0, 7, 0, 0, 8, 0, 0, 0, 6), Array(0, 0, 0, 1, 0, 3, 0, 6, 2), Array(1, 0, 5, 0, 0, 0, 4, 0, 3), Array(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Array(6, 0, 0, 0, 2, 0, 0, 3, 0), Array(0, 0, 7, 0, 0, 1, 0, 0, 4), Array(8, 9, 0, 0, 6, 5, 1, 0, 7))
    val unsolved: Table = new Table(nums)
    val solved: Table = Game.solveSudoku(unsolved)
    //first row has two 4s
    if (solved == null) Assert.assertEquals(true, true)
    else Assert.assertEquals(true, false)
  }

  @Test def checkEndTest(): Unit = {
    val nums: Array[Array[Int]] = Array(Array(4, 0, 1, 2, 9, 0, 0, 7, 5), Array(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Array(0, 7, 0, 0, 8, 0, 0, 0, 6), Array(0, 0, 0, 1, 0, 3, 0, 6, 2), Array(1, 0, 5, 0, 0, 0, 4, 0, 3), Array(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Array(6, 0, 0, 0, 2, 0, 0, 3, 0), Array(0, 0, 7, 0, 0, 1, 0, 0, 4), Array(8, 9, 0, 0, 6, 5, 1, 0, 7))

    val unsolved: Table = new Table(nums)
    Assert.assertEquals(false, Game.checkEnd(unsolved))
    val solved: Table = Game.solveSudoku(unsolved)
    Assert.assertEquals(true, Game.checkEnd(solved))
  }

}
