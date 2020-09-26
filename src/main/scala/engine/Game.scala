package engine

import model.{Table}
import util.Constants

import scala.collection.mutable

object Game {

  val gridSize = Constants.GRID_SIZE
  val boardSize = Constants.BOARD_SIZE

  def checkEnd(table: Table): Boolean = {
    def checkRows(): Boolean = {
      for (x <- 0 until boardSize) {
        val nums: Array[Int] = table.nums(x)
        val seenNums: mutable.Set[Int] = mutable.Set[Int]()
        for (y <- 0 until nums.length)
          if (nums(y) == 0 || seenNums.contains(nums(y)))
            return false
          else seenNums.add(nums(y))
      }
      true
    }

    def checkColumns(): Boolean = {
      for (y <- 0 until boardSize) {
        for (x <- 0 until boardSize) {
          val seenNums: mutable.Set[Int] = mutable.Set[Int]()
          val num = table.nums(x)(y)
          if (seenNums.contains(num))
            return false
          else seenNums.add(num)
        }
      }
      true
    }

    // not necessary at all...
    def checkInner(): Boolean = {
      def check(index: Int): Boolean = {
        val xFrom: Int = (index % gridSize) * gridSize
        val xTo: Int = xFrom + gridSize
        val yFrom: Int = (index / gridSize) * gridSize
        val yTo: Int = yFrom + gridSize

        for (x <- xFrom until xTo) {
          for (y <- yFrom until yTo) {
            val seenNums: mutable.Set[Int] = mutable.Set[Int]()
            val num = table.nums(x)(y)
            if (seenNums.contains(num))
              return false
            else seenNums.add(num)
          }
        }
        true
      }

      for (innerIndex <- 0 until boardSize) {
        if (!check(innerIndex)) return false
      }
      true
    }


    checkRows && checkColumns && checkInner
  }

  def getIndex(x: Int, y: Int): Int = {
    val xDiv: Int = x / gridSize
    val yDiv: Int = y / gridSize
    (xDiv, yDiv) match {
      case (0, 0) => 0
      case (0, 1) => 1
      case (0, 2) => 2
      case (1, 0) => 3
      case (1, 1) => 4
      case (1, 2) => 5
      case (2, 0) => 6
      case (2, 1) => 7
      case (2, 2) => 8
    }
  }

  def calculatePossibilities(table: Table, x: Int, y: Int): mutable.Set[Int] = {
    def filterRow(allNums: mutable.Set[Int]) = {
      for (i <- 0 until boardSize if table.nums(x)(i) != 0)
        allNums.remove(table.nums(x)(i))
    }

    def filterColumn(allNums: mutable.Set[Int]) = {
      for (i <- 0 until boardSize if table.nums(i)(y) != 0)
        allNums.remove(table.nums(i)(y))
    }

    def filterGrid(allNums: mutable.Set[Int]): Unit = {
      val index: Int = getIndex(x, y)
      val xFrom: Int = (index / gridSize) * gridSize
      val xTo: Int = xFrom + gridSize
      val yFrom: Int = (index % gridSize) * gridSize
      val yTo: Int = yFrom + gridSize

      for (i <- xFrom until xTo) {
        for (j <- yFrom until yTo) {
          if (table.nums(i)(j) != 0)
            allNums.remove(table.nums(i)(j))
        }
      }
    }

    val allNums: mutable.Set[Int] = mutable.Set.range(1, boardSize + 1)
    filterRow(allNums)
    filterColumn(allNums)
    filterGrid(allNums)
    if (allNums.size == 0) null
    else allNums

  }

  def findPossibilities(table: Table): Array[Array[mutable.Set[Int]]] = {
    val possibilities: Array[Array[mutable.Set[Int]]] = Array.ofDim[mutable.Set[Int]](boardSize, boardSize)
    for (x <- 0 until boardSize; y <- 0 until boardSize if table.nums(x)(y) == 0) {
      possibilities(x)(y) = calculatePossibilities(table, x, y)
    }
    possibilities
  }

  def updatePossibilities(poss: Array[Array[mutable.Set[Int]]], x: Int, y: Int, num: Int): Array[Array[mutable.Set[Int]]] = {
    val newPossibilities: Array[Array[mutable.Set[Int]]] = Array.ofDim[mutable.Set[Int]](boardSize, boardSize)
    for (i <- 0 until boardSize; j <- 0 until boardSize) {
      newPossibilities(i)(j) = poss(i)(j)
    }

    newPossibilities
  }

  def getMinPossibilities(poss: Array[Array[mutable.Set[Int]]]): (Int, Int) = {
    var xBest: Int = -1
    var yBest: Int = -1
    var minNum: Int = boardSize + 1
    for (x <- 0 until boardSize; y <- 0 until boardSize if poss(x)(y) != null) {
      if (poss(x)(y).size < minNum) {
        minNum = poss(x)(y).size
        xBest = x
        yBest = y
      }
    }
    (xBest, yBest)
  }

  def hasEmptyFields(table: Table): Boolean = {
    table.nums.exists(row => row.exists(num => num == 0))
  }

  def solveSudoku(table: Table): Table = {
    val possibilities: Array[Array[mutable.Set[Int]]] = findPossibilities(table)
    solve(table, possibilities)
  }

  def solve(table: Table, possibilities: Array[Array[mutable.Set[Int]]]): Table = {
    val (x, y) = getMinPossibilities(possibilities)
    if (x == -1) {
      if (hasEmptyFields(table))
        null
      else table
    } else {
      for (tryNumber <- 0 until possibilities(x)(y).size) {
        val num: Int = possibilities(x)(y).toList(tryNumber)
        table.nums(x)(y) = num
        //        val updatedPossiblities: Array[Array[mutable.Set[Int]]] = updatePossibilities(possibilities, x, y, num)
        val updatedPossiblities: Array[Array[mutable.Set[Int]]] = findPossibilities(table)
        val newTable: Table = solve(table, updatedPossiblities)
        if (newTable != null)
          return newTable
        else table.nums(x)(y) = 0
      }
      null
    }
  }

  def transpose(table: Table): Table = {
    new Table(table.nums.transpose)
  }

  def substitute(table: Table): Table = {
    val nums: Array[Array[Int]] = Array.ofDim[Int](table.nums.length, table.nums.length)

    for (x <- 0 until table.nums.length)
      for (y <- 0 until table.nums.length) {
        if (table.nums(x)(y) != 0)
          nums(x)(y) = (Constants.BOARD_SIZE + 1) - table.nums(x)(y)
        else nums(x)(y) = 0
      }

    new Table(nums)
  }

  def filterRnC(table: Table, num: Int, x: Int, y: Int): Table = {
    if (num == 0)
      return table

    for (i <- 0 until table.nums.length) {
      if (table.nums(x)(i) == num)
        table.nums(x)(i) = 0
      if (table.nums(i)(y) == num)
        table.nums(i)(y) = 0
    }

    new Table(table.nums)
  }

  def filterInnerGrid(table: Table, num: Int, x: Int, y: Int): Table = {
    if (num == 0)
      return table

    val index: Int = getIndex(x, y)

//    val xFrom: Int = (index % gridSize) * gridSize
//    val xTo: Int = xFrom + gridSize
//    val yFrom: Int = (index / gridSize) * gridSize
//    val yTo: Int = yFrom + gridSize
    val xFrom: Int = (index / gridSize) * gridSize
    val xTo: Int = xFrom + gridSize
    val yFrom: Int = (index % gridSize) * gridSize
    val yTo: Int = yFrom + gridSize
    for (i <- xFrom until xTo) {
      for (j <- yFrom until yTo) {
        if (table.nums(i)(j) == num)
          table.nums(i)(j) = 0
      }
    }

    new Table(table.nums)
  }

}
