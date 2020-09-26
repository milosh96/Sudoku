package util

import javafx.animation.{FadeTransition}
import model.{Cell, Table}

object TableUtil {

  def createTable(cells: Array[Array[Cell]]): Table = {
    val nums: Array[Array[Int]] = Array.ofDim[Int](cells.length, cells.length)
    for (x <- 0 until cells.length) {
      for (y <- 0 until cells.length) {
        val text = cells(x)(y).field.getText()
        val num: Int = text match {
          case "" => 0
          case "-" => 0
          case e => e.toInt
        }
        nums(y)(x) = num
      }
    }

    new Table(nums)
  }

  def createTableWithPointer(cells : Array[Array[Cell]], pointer: Cell) : Table = {
    val nums: Array[Array[Int]] = Array.ofDim[Int](cells.length, cells.length)
    for (x <- 0 until cells.length) {
      for (y <- 0 until cells.length) {
        val text = cells(x)(y).field.getText()
        val num: Int = text match {
          case "" => 0
          case "-" => 0
          case e => e.toInt
        }
        nums(y)(x) = num
        if (cells(x)(y).equals(pointer))
          nums(y)(x) = -1
      }
    }

    new Table(nums)
  }

  def updateCells(cells: Array[Array[Cell]], table: Table) {
    val transitions : Array[Array[FadeTransition]] = Array.ofDim[FadeTransition](cells.length, cells.length)
    for (x <- 0 until cells.length; y <- 0 until cells.length) {
        val num = table.nums(x)(y)
        val text: String = num match {
          case 0 => ""
          case e => e.toString
        }
//        val transition : FadeTransition = new FadeTransition()
//        transition.setDuration(Duration.millis(100))
//        transition.setToValue(1)
//        transition.setFromValue(0)
//        transition.setNode(cells(x)(y).field)
//        if(x==0 && y==0) transition.play()
        cells(y)(x).field.setText(text)
//        transitions(x)(y) = transition
    }
//    for(x <-0 until cells.length; y <- 0 until cells.length){
//      transitions(x)(y).setOnFinished(e => {
//        if (y < cells.length - 1) transitions(x)(y+1).play()
//        else if (y == cells.length && x < cells.length -1) transitions(x+1)(0).play()
//      })
//    }
  }

  def copyTable(table : Table) : Table = {
    val newNums : Array[Array[Int]] = Array.ofDim[Int](table.nums.length, table.nums.length)
    for(x <- 0 until table.nums.length; y <- 0 until table.nums.length)
      newNums(x)(y)= table.nums(x)(y)

    new Table(newNums)
  }

}
