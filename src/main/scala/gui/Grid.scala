package gui


import engine.Game
import javafx.geometry.{Insets, Pos}
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.{Alert, TextField}
import javafx.scene.input.{KeyCode}
import javafx.scene.layout.{GridPane}
import model.{Cell, Coordinates, Table}
import util.{Constants, TableUtil}

class Grid extends GridPane {
  setPadding(new Insets(5, 5, 5, 5))
  val boardSize = Constants.BOARD_SIZE
  val gridSize = Constants.GRID_SIZE
  val cells: Array[Array[Cell]] = Array.ofDim[Cell](boardSize, boardSize)
  val innerGrids: Array[Array[GridPane]] = Array.ofDim[GridPane](gridSize, gridSize)

  for (x <- 0 until boardSize) {
    for (y <- 0 until boardSize) {
      val cell = new TextField()
      cell.getStyleClass.add("cell")
      cell.setAlignment(Pos.CENTER)
      setEventListeners(cell)
      cells(x)(y) = new Cell(cell, x, y, false)
    }
  }

  var gameMode = true
  var pointer: Cell = cells(0)(0)
  pointer.field.getStyleClass.add("cell-pointer")

  setOnMouseClicked(event => {
    pointer.field.requestFocus()
  })

  for (x <- 0 until gridSize) {
    for (y <- 0 until gridSize) {
      val innerGrid = new GridPane
      innerGrid.setGridLinesVisible(true)
      innerGrid.getStyleClass.add("inner-grid")

      innerGrids(x)(y) = innerGrid
      add(innerGrid, x, y)
    }
  }

  for (x <- 0 until boardSize) {
    for (y <- 0 until boardSize) {
      val xDiv: Int = x / gridSize
      val yDiv: Int = y / gridSize
      val xMod: Int = x % gridSize
      val yMod: Int = y % gridSize
      val innerGrid = innerGrids(xDiv)(yDiv)
      innerGrid.add(cells(x)(y).field, xMod, yMod)
    }
  }

  def setEventListeners(cell: TextField): Unit = {
    cell.setOnKeyPressed(event => {
      val keyCode = event.getCode
      if (keyCode == KeyCode.UP || keyCode == KeyCode.DOWN || keyCode == KeyCode.LEFT || keyCode == KeyCode.RIGHT) movePointer(keyCode)
      else if (keyCode.isDigitKey) enterValue(keyCode)
      else if (keyCode == KeyCode.DELETE) deleteValue()
      else event.consume()
    })
    cell.addEventHandler(javafx.scene.input.KeyEvent.KEY_TYPED, (e: javafx.scene.input.KeyEvent) => {
      e.consume()
    })
    cell.addEventHandler(javafx.scene.input.KeyEvent.KEY_RELEASED, (e: javafx.scene.input.KeyEvent) => {
      e.consume()
    })
    cell.addEventHandler(javafx.scene.input.MouseEvent.MOUSE_CLICKED, (e: javafx.scene.input.MouseEvent) => {
      pointer.field.requestFocus
      e.consume()
    })
    cell.addEventHandler(javafx.scene.input.MouseEvent.MOUSE_PRESSED, (e: javafx.scene.input.MouseEvent) => {
      e.consume()
    })
    cell.addEventHandler(javafx.scene.input.MouseEvent.MOUSE_RELEASED, (e: javafx.scene.input.MouseEvent) => {
      e.consume()
    })

  }

  def movePointer(keyCode: KeyCode): Unit = {
    def moveLeft(): Unit = {
      val x = pointer.x
      if (x != 0) {
        pointer.field.getStyleClass.remove("cell-pointer")
        pointer.field.getStyleClass.add("cell")
        pointer = cells(x - 1)(pointer.y)
        pointer.field.getStyleClass.add("cell-pointer")
      }
    }

    def moveRight(): Unit = {
      val x = pointer.x
      if (x != boardSize - 1) {
        pointer.field.getStyleClass.remove("cell-pointer")
        pointer.field.getStyleClass.add("cell")
        pointer = cells(x + 1)(pointer.y)
        pointer.field.getStyleClass.add("cell-pointer")
      }
    }

    def moveUp(): Unit = {
      val y = pointer.y
      if (y != 0) {
        pointer.field.getStyleClass.remove("cell-pointer")
        pointer.field.getStyleClass.add("cell")
        pointer = cells(pointer.x)(y - 1)
        pointer.field.getStyleClass.add("cell-pointer")
      }
    }

    def moveDown(): Unit = {
      val y = pointer.y
      if (y != boardSize - 1) {
        pointer.field.getStyleClass.remove("cell-pointer")
        pointer.field.getStyleClass.add("cell")
        pointer = cells(pointer.x)(pointer.y + 1)
        pointer.field.getStyleClass.add("cell-pointer")
      }
    }

    keyCode match {
      case KeyCode.UP => moveUp
      case KeyCode.DOWN => moveDown
      case KeyCode.LEFT => moveLeft
      case KeyCode.RIGHT => moveRight
    }
    pointer.field.requestFocus()
  }

  def enterValue(char: Char): Unit = {
    val num: String = char.toString
    if (gameMode && pointer.originalField) {
      val alert = new Alert(AlertType.WARNING)
      alert.setTitle("Warning")
      alert.setContentText("You cannot change original fields")
      alert.show()
    } else {
      pointer.field.setText(num)
    }
  }

  def enterValue(keyCode: KeyCode): Unit = {
    val num: String = keyCode match {
      case KeyCode.NUMPAD0 => "0"
      case KeyCode.NUMPAD1 => "1"
      case KeyCode.NUMPAD2 => "2"
      case KeyCode.NUMPAD3 => "3"
      case KeyCode.NUMPAD4 => "4"
      case KeyCode.NUMPAD5 => "5"
      case KeyCode.NUMPAD6 => "6"
      case KeyCode.NUMPAD7 => "7"
      case KeyCode.NUMPAD8 => "8"
      case KeyCode.NUMPAD9 => "9"
      case KeyCode.DIGIT0 => "0"
      case KeyCode.DIGIT1 => "1"
      case KeyCode.DIGIT2 => "2"
      case KeyCode.DIGIT3 => "3"
      case KeyCode.DIGIT4 => "4"
      case KeyCode.DIGIT5 => "5"
      case KeyCode.DIGIT6 => "6"
      case KeyCode.DIGIT7 => "7"
      case KeyCode.DIGIT8 => "8"
      case KeyCode.DIGIT9 => "9"
    }

    if (num == "0")
      return
    if (gameMode && pointer.originalField) {
      val alert = new Alert(AlertType.WARNING)
      alert.setTitle("Warning")
      alert.setContentText("You cannot change original fields")
      alert.show()
    } else {
      pointer.field.setText(num)
    }

  }

  def deleteValue(): Unit = {
    if (!gameMode || !pointer.originalField) pointer.field.setText("")
  }

  def loadTable(table: Array[Array[Char]]): Unit = {
    if (table.length != boardSize)
      throw new Exception("Error loading table")

    for (x <- 0 until boardSize) {
      for (y <- 0 until boardSize) {
        val char: Char = table(x)(y)
        val cell: Cell = cells(y)(x)
        char match {
          case c if c.isDigit && c != '0' => {
            cell.field.setText(char.toString)
            cell.originalField = true
          }
          case 'P' => {
            pointer.field.getStyleClass.remove("cell-pointer")
            pointer.field.getStyleClass.add("cell")
            pointer = cells(y)(x)
            pointer.field.getStyleClass.add("cell-pointer")
            pointer.field.setText("")
          }
          case '-' => {
            cell.field.setText("")
            cell.originalField = false
          }
          case _ => {
            throw new Exception("Invalid file")
          }
        }

      }
    }
  }

  def playSequence(sequence: Array[Char]): Unit = {
    for (x <- 0 until sequence.length) {
      val move = sequence(x)
      move match {
        case 'u' => movePointer(KeyCode.UP)
        case 'l' => movePointer(KeyCode.LEFT)
        case 'd' => movePointer(KeyCode.DOWN)
        case 'r' => movePointer(KeyCode.RIGHT)
        case _ => enterValue(move)
      }
    }
  }

  def playCommands(commands: Array[String]) = {
    def matchFilter(str: String): Unit = {
      val strings: Array[String] = str.split(" ")
      if (strings.length == 2)
        strings(0) match {
          case "filterRnC" => filterRnC(strings(1).toInt)
          case "filterInnerGrid" => filterInnerGrid(strings(1).toInt)
        }
    }

    for (x <- 0 until commands.length) {
      commands(x) match {
        case "transpose" => transpose
        case "substitute" => substitute
        case e => matchFilter(e)
      }
    }
  }

  def checkEnd(): Unit = {
    val table: Table = TableUtil.createTable(cells)
    val end = Game.checkEnd(table)
    val alert = new Alert(AlertType.INFORMATION)
    alert.setTitle("Game end")
    if (end) alert.setContentText("Sudoku game is solved")
    else alert.setContentText("Sudoku game isn't solved")

    alert.show()
  }

  def solve(): Unit = {
    operationOnTable(Game.solveSudoku)
  }

  def solveSeq(): (Table, Table) = {
    val table: Table = TableUtil.createTable(cells)
    val hardCopy: Table = TableUtil.copyTable(table)
    val resultingTable: Table = Game.solveSudoku(table)
    (hardCopy, resultingTable)
  }

  def createSeq(oldTable: Table, newTable: Table): Array[Char] = {
    val startPosition: Coordinates = new Coordinates(pointer.y, pointer.x)
    Game.createSequence(oldTable, newTable, startPosition)
  }

  def solvable(): Boolean = {
    val table: Table = TableUtil.createTable(cells)
    val resultingTable: Table = Game.solveSudoku(table)
    if (resultingTable == null) false
    else true
  }

  def modifyBoard() = {
    gameMode = false
  }

  def playBoard() = {
    if (!gameMode) {
      gameMode = true
      setAllFieldsAsOriginals
    }
  }

  //board transforamtions
  def transpose(): Unit = {
    operationOnTable(Game.transpose)
  }

  def substitute(): Unit = {
    operationOnTable(Game.substitute)
  }

  def filterRnC(num: Int): Unit = {
    filter(Game.filterRnC, num)
  }

  def filterInnerGrid(num: Int): Unit = {
    filter(Game.filterInnerGrid, num)
  }

  def filter(op: (Table, Int, Int, Int) => Table, num: Int): Unit = {
    val table: Table = TableUtil.createTable(cells)
    val resultingTable: Table = op(table, num, pointer.y, pointer.x)

    if (resultingTable == null) {
      val alert = new Alert(AlertType.WARNING)
      alert.setTitle("Filter")
      alert.setContentText("Filter cannot be done")
      alert.show()
    } else {
      TableUtil.updateCells(cells, resultingTable)
    }
  }

  def operationOnTable(op: Table => Table): Unit = {
    val table: Table = TableUtil.createTable(cells)
    val resultingTable: Table = op(table)

    if (resultingTable == null) {
      val alert = new Alert(AlertType.WARNING)
      alert.setTitle("Operation")
      alert.setContentText("Operation cannot be done")
      alert.show()
    } else {
      TableUtil.updateCells(cells, resultingTable)
    }
  }

  def setAllFieldsAsOriginals(): Unit = {
    for (x <- 0 until boardSize)
      for (y <- 0 until boardSize) {
        val cell = cells(x)(y)
        if (!cell.field.getText.isEmpty && cell.field.getText.charAt(0).isDigit)
          cell.originalField = true
      }
  }

  def reset(): Unit = {
    for (x <- 0 until boardSize)
      for (y <- 0 until boardSize) {
        val cell = cells(x)(y)
        cell.field.setText("")
        cell.originalField = true
      }
  }
}
