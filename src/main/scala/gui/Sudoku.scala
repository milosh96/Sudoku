package gui

import java.io.File

import javafx.application.Application
import javafx.geometry.Pos
import javafx.scene.Scene
import javafx.scene.control.{Alert, ButtonType, ChoiceDialog, Dialog, Menu, MenuBar, MenuItem, TextInputDialog, ToggleButton, ToggleGroup}
import javafx.scene.layout.{BorderPane, HBox, VBox}
import javafx.scene.paint.Color
import javafx.stage.{FileChooser, Stage}
import util.{DialogUtil, FileUtil, TableUtil}
import java.util

import javafx.scene.control.Alert.AlertType
import javafx.util.Pair
import model.Table

class Sudoku extends Application {
  override def start(stage: Stage): Unit = {
    val root = new BorderPane()
    val scene = new Scene(root, 500, 480, Color.WHITE)

    val grid = createSudokuGrid(root, stage)
    //menu
    createMenuBar(root, stage, grid)

    createGameMode(root, stage, grid)

    scene.getStylesheets.add(getClass.getClassLoader.getResource("styles/sudoku.css").toExternalForm)
    stage.setTitle("Sudoku")
    stage.setScene(scene)
    stage.show()
  }

  def modeDialog(): Unit = {
    val alert = new Alert(AlertType.WARNING)
    alert.setTitle("Game mode")
    alert.setContentText("You are not in a proper game mode for this operation")
    alert.show()
  }

  def boardSolveable() = {
    val alert = new Alert(AlertType.INFORMATION)
    alert.setTitle("Board")
    alert.setContentText("Board is solvable")
    alert.show()
  }

  def invalid(str: String): Unit = {
    val alert = new Alert(AlertType.WARNING)
    alert.setTitle(str)
    alert.setContentText(str + " is not valid")
    alert.show()
  }

  def createMenuBar(root: BorderPane, stage: Stage, grid: Grid): Unit = {
    val menuBar: MenuBar = new MenuBar()
    menuBar.prefWidthProperty().bind(stage.widthProperty())
    root.setTop(menuBar)
    val menuFile = new Menu("File")
    val loadBoard = new MenuItem("Load board")
    loadBoard.setOnAction(action => {
      val fileChooser = new FileChooser()
      fileChooser.setInitialDirectory(new File("src/main/resources/games"))
      fileChooser.setTitle("Choose a game")
      val file: File = fileChooser.showOpenDialog(null)
      if (file != null) {
        val loadedTable: Array[Array[Char]] = FileUtil.getTableFromFile(file)

        try {
          grid.loadTable(loadedTable)
        }
        catch {
          case e: Exception => {
            val alert = new Alert(AlertType.ERROR)
            alert.setTitle("Error")
            alert.setContentText("Table provided is invalid")
            alert.show()
            grid.clearTable()
          }
        }
      }

    })
    val loadSeq = new MenuItem("Load sequence")
    loadSeq.setOnAction(action => {
      val fileChooser = new FileChooser()
      fileChooser.setInitialDirectory(new File("src/main/resources/sequences"))
      fileChooser.setTitle("Choose a sequence")
      val file: File = fileChooser.showOpenDialog(null)
      if (file != null) {
        try {
          val loadedSeq: Array[Char] = FileUtil.getSeqFromFile(file)
          grid.playSequence(loadedSeq)
        }
        catch {
          case e: Exception => {
            val alert = new Alert(AlertType.ERROR)
            alert.setTitle("Error")
            alert.setContentText("Sequence provided is invalid")
            alert.show()
          }
        }
      }
    })
    val saveBoard = new MenuItem("Save board")
    saveBoard.setOnAction(action => {
      if (!grid.gameMode) {
        if (grid.solveable) {
          val fileChooser = new FileChooser()
          fileChooser.setInitialDirectory(new File("src/main/resources/games"))
          fileChooser.setTitle("Save a game")
          val file: File = fileChooser.showSaveDialog(null)
          if (file != null) FileUtil.saveTableToFile(file, TableUtil.createTableWithPointer(grid.cells, grid.pointer))
        } else invalid("Board")
      }
      else modeDialog

    })
    val solveSeq = new MenuItem("Save solve sequence")
    solveSeq.setOnAction(a => {
      val (oldTable: Table, newTable: Table) = grid.solveSeq
      if (newTable != null) {
        val fileChooser = new FileChooser()
        fileChooser.setInitialDirectory(new File("src/main/resources/sequences"))
        fileChooser.setTitle("Save a sequnce")
        val file: File = fileChooser.showSaveDialog(null)
        val seq: Array[Char] = grid.createSeq(oldTable, newTable)
        FileUtil.saveSeqToFile(file, seq)
      } else invalid("Board")
    })
    menuFile.getItems.addAll(loadBoard, loadSeq, saveBoard, solveSeq)
    val menuGame = new Menu("Game")
    val checkGame = new MenuItem("Validate end")
    checkGame.setOnAction(action => {
      if (grid.gameMode) grid.checkEnd
      else modeDialog
    })
    val solve = new MenuItem("Solve")
    solve.setOnAction(action => {
      if (grid.gameMode) grid.solve
      else modeDialog
    })
    menuGame.getItems.addAll(checkGame, solve)
    val menuBoard = new Menu("Board")
    val transpose = new MenuItem("Transpose")
    transpose.setOnAction(action => {
      if (!grid.gameMode) grid.transpose
      else modeDialog
    })
    val substitute = new MenuItem("Substitute")
    substitute.setOnAction(a => {
      if (!grid.gameMode) grid.substitute
      else modeDialog
    })
    val filterRnC = new MenuItem("Filter row & column")
    filterRnC.setOnAction(a => {
      if (!grid.gameMode) {
        //TODO hardcoded
        val dialog: ChoiceDialog[Int] = new ChoiceDialog[Int](1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
        dialog.setTitle("Filter rows and columns")
        dialog.setContentText("Choose value: ")
        val result = dialog.showAndWait()
        if (result.isPresent)
          grid.filterRnC(result.get())
      } else modeDialog
    })
    val filterInnerGrid = new MenuItem("Filter inner grid")
    filterInnerGrid.setOnAction(a => {
      if (!grid.gameMode) {
        //TODO hardcoded
        val dialog: ChoiceDialog[Int] = new ChoiceDialog[Int](1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
        dialog.setTitle("Filter inner grid")
        dialog.setContentText("Choose value: ")
        val result = dialog.showAndWait()
        if (result.isPresent)
          grid.filterInnerGrid(result.get())
      } else modeDialog
    })
    val solveable = new MenuItem("Solvable")
    solveable.setOnAction(a => {
      if (grid.solveable) boardSolveable
      else invalid("Board")
    })
    menuBoard.getItems.addAll(transpose, substitute, filterRnC, filterInnerGrid, solveable)
    val menuCustom: Menu = new Menu("Custom")
    val customSeq: Menu = new Menu("Sequence")
    val customCommand: Menu = new Menu("Command")
    val seqAdd: MenuItem = new MenuItem("Add")
    seqAdd.setOnAction(a => {
      val dialog: Dialog[Pair[String, String]] = DialogUtil.createFormDialog("Add sequence", "Seperate values [ 'd', 'u', 'l', 'r', {num} ] with '-'", "Name", "Sequence")
      val result = dialog.showAndWait()
      if (result.isPresent)
        if (!FileUtil.saveSeqToFile(result.get())) invalid("Sequence")
    })
    val seqPlay: MenuItem = new MenuItem("Play")
    seqPlay.setOnAction(a => {
      val sequences: Seq[String] = FileUtil.getCustomSequences
      val dialog = new ChoiceDialog[String]()
      val items = dialog.getItems
      sequences.foreach(cm => items.add(cm))
      if (sequences != null && sequences.size > 0) dialog.setSelectedItem(sequences(0))
      dialog.setTitle("Custom sequence name")
      dialog.setContentText("Choose sequence: ")
      val result = dialog.showAndWait()
      if (result.isPresent) {
        val commandName: String = result.get().toString
        val file: File = new File("src/main/resources/named/seq/" + commandName + ".txt")
        grid.playSequence(FileUtil.getSeqFromFile(file))
      }
    })
    customSeq.getItems.addAll(seqAdd, seqPlay)
    val commAdd: MenuItem = new MenuItem("Add")
    commAdd.setOnAction(a => {
      val dialog: Dialog[Pair[String, String]] = DialogUtil.createFormDialog("Add command", "Seperate values [ 'transpose', 'substitute', 'filterRnC {num}', 'filterInnerGrid {num}', {customCommand} ] with '-'", "Name", "Command")
      val result = dialog.showAndWait()
      if (result.isPresent)
        if (!FileUtil.saveCommandToFile(result.get())) invalid("Command")
    })
    val commPlay: MenuItem = new MenuItem("Play")
    commPlay.setOnAction(a => {
      val commands: Seq[String] = FileUtil.getCustomCommands
      val dialog = new ChoiceDialog[String]()
      val items = dialog.getItems
      commands.foreach(cm => items.add(cm))
      if (commands != null && commands.size > 0) dialog.setSelectedItem(commands(0))
      dialog.setTitle("Custom command name")
      dialog.setContentText("Choose command: ")
      val result = dialog.showAndWait()
      if (result.isPresent) {
        val commandName: String = result.get().toString
        val file: File = new File("src/main/resources/named/command/" + commandName + ".txt")
        grid.playCommands(FileUtil.getCommandsFromFile(file))
      }
    })
    customCommand.getItems.addAll(commAdd, commPlay)
    menuCustom.getItems.addAll(customSeq, customCommand)
    menuBar.getMenus.addAll(menuFile, menuGame, menuBoard, menuCustom)
  }

  def createSudokuGrid(pane: BorderPane, stage: Stage): Grid = {
    val grid = new Grid
    pane.setCenter(grid)
    grid
  }

  def createGameMode(pane: BorderPane, stage: Stage, grid: Grid): Unit = {
    val play: ToggleButton = new ToggleButton("Play")
    val modify: ToggleButton = new ToggleButton("Modify")
    val toggle: ToggleGroup = new ToggleGroup()
    play.setToggleGroup(toggle)
    play.setOnAction(a => {
      grid.playBoard
    })
    play.getStyleClass.add("game-mode")
    modify.setToggleGroup(toggle)
    modify.setOnAction(a => {
      grid.modifyBoard
    })
    modify.getStyleClass.add("game-mode")
    toggle.selectToggle(play)
    val hBox: HBox = new HBox(play, modify)
    hBox.setAlignment(Pos.BASELINE_CENTER)
    pane.setBottom(hBox)
  }
}
