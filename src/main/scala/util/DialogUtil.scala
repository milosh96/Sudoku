package util

import javafx.application.Platform
import javafx.geometry.Insets
import javafx.scene.control.{ButtonType, Dialog, Label, TextField}
import javafx.scene.layout.GridPane
import javafx.util.Pair

object DialogUtil {

  def createFormDialog(title: String, header: String, field1: String, field2: String): Dialog[Pair[String, String]] = {
    val dialog: Dialog[Pair[String, String]] = new Dialog()
    dialog.setTitle(title)
    dialog.setHeaderText(header)
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val gridPane = new GridPane
    gridPane.setHgap(10)
    gridPane.setVgap(10)
    gridPane.setPadding(new Insets(10, 10, 10, 10))

    val tf1: TextField = new TextField()
    val tf2: TextField = new TextField()
    gridPane.add(new Label(field1), 0, 0)
    gridPane.add(tf1, 1, 0)
    gridPane.add(new Label(field2), 0, 1)
    gridPane.add(tf2, 1, 1)

    dialog.getDialogPane.setContent(gridPane)

    Platform.runLater(() => tf1.requestFocus)

    dialog.setResultConverter(btn =>{
      if (btn == ButtonType.OK && !tf1.getText.isEmpty && !tf2.getText.isEmpty) new Pair[String, String] (tf1.getText, tf2.getText)
      else null
    })
    dialog
  }
}
