package util

import model.Table
import java.io.{File, PrintWriter}

import javafx.util.Pair
import scala.collection.mutable
import scala.io.Source

object FileUtil {

  def getTableFromFile(file: File): Array[Array[Char]] = {
    val table = Source.fromFile(file).getLines.toArray
    table.map(line => line.toArray)
  }

  def saveTableToFile(file: File, table: Table) = {
    val writer: PrintWriter = new PrintWriter(file)
    for (x <- 0 until table.nums.length) {
      for (y <- 0 until table.nums.length) {
        val value: String = table.nums(x)(y) match {
          case 0 => "-"
          case -1 => "P"
          case e => e.toString
        }
        writer.write(value)
      }
      writer.write("\n")
    }
    writer.close()
  }

  def saveSeqToFile(file: File, seq: Array[Char]): Unit = {
    //no check for valid values, only used when saving solve sequence which we generate
    val writer: PrintWriter = new PrintWriter(file)
    for (i <- 0 until seq.length) {
      writer.write(seq(i))
      if (i != seq.length - 1) writer.write('\n')
    }

    writer.close
  }

  def saveSeqToFile(pair: Pair[String, String]): Boolean = {
    val fileName: String = pair.getKey
    val seq: Array[String] = pair.getValue.split("-")
    val validValues: Set[Char] = Set[Char]('d', 'u', 'l', 'r', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    if (seq.exists(sq => sq.length > 1 || !validValues.contains(sq(0))))
      false
    else {
      val writer: PrintWriter = new PrintWriter(new File("src/main/resources/named/seq/" + fileName + ".txt"))
      for (i <- 0 until seq.length) {
        writer.write(seq(i)(0))
        if (i != seq.length - 1) writer.write('\n')
      }
      writer.close()
      true
    }
  }

  def getSeqFromFile(file: File): Array[Char] = {
    val seq = Source.fromFile(file).getLines.toArray
    val validValues: Set[Char] = Set[Char]('d', 'u', 'l', 'r', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    if (seq.exists(line => line.length > 1))
      throw new Exception("Invalid sequence file")
    if (seq.exists(line => !validValues.contains(line.charAt(0))))
      throw new Exception("Invalid sequence file")
    seq.map(line => line.charAt(0))
  }

  def saveCommandToFile(pair: Pair[String, String]): Boolean = {
    val fileName: String = pair.getKey
    val commands: Array[String] = pair.getValue.split("-")
    val customCommands: Set[String] = getCustomCommands().toSet
    val originalCommands: Set[String] = Set[String]("transpose", "substitute")
    if (!checkValidCommands(commands, customCommands, originalCommands)) false
    else {
      val writer: PrintWriter = new PrintWriter(new File("src/main/resources/named/command/" + fileName + ".txt"))
      for (i <- 0 until commands.length) {
        writer.write(commands(i))
        if (i != commands.length - 1) writer.write('\n')
      }
      writer.close()
      true
    }
  }

  def checkValidCommands(commands: Array[String], customCommands: Set[String], originalCommands: Set[String]): Boolean = {
    if (commands.exists(command => !originalCommands.contains(command) && !customCommands.contains(command) && !filterCommand(command))) false
    else true
  }

  def filterCommand(str: String): Boolean = {
    val strings: Array[String] = str.split(" ")
    val filterCommands: Set[String] = Set("filterRnC", "filterInnerGrid")
    val validNums: Set[String] = Set("1", "2", "3", "4", "5", "6", "7", "8", "9")
    if (strings.length != 2) false
    else {
      if (filterCommands.contains(strings(0)) && validNums.contains(strings(1))) true
      else false
    }
  }

  //return only simple commands
  def getCommandsFromFile(file: File): Array[String] = {
    val commands = Source.fromFile(file).getLines.toArray
    val customCommands: Set[String] = getCustomCommands().toSet
    val originalCommands: Set[String] = Set[String]("transpose", "substitute")

    if (!checkValidCommands(commands, customCommands, originalCommands)) throw new Exception("Invalid command file")

    val finalCommands: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
    commands.foreach(command => {
      finalCommands.addAll(getOriginalCommands(command, originalCommands))
    })
    finalCommands.toArray
  }

  def getOriginalCommands(command: String, originalCommands: Set[String]): Array[String] = {
    if (originalCommands.contains(command) || filterCommand(command)) Array(command)
    else {
      val newCommands: Array[String] = Source.fromFile("src/main/resources/named/command/" + command + ".txt").getLines.toArray
      val finalCommands: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
      newCommands.foreach(newCommand => {
        finalCommands.addAll(getOriginalCommands(newCommand, originalCommands))
      })
      finalCommands.toArray
    }
  }

  def getCustomSequences(): Seq[String] = {
    val file: Array[File] = new File("src/main/resources/named/seq").listFiles()
    file.map(f => f.getName.substring(0, f.getName.lastIndexOf("."))).toSeq
  }

  def getCustomCommands(): Seq[String] = {
    val file: Array[File] = new File("src/main/resources/named/command").listFiles()
    file.map(f => f.getName.substring(0, f.getName.lastIndexOf("."))).toSeq
  }

}
