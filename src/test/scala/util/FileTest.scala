package util

import java.io.File

import javafx.util.Pair
import org.junit.{Assert, Test}

class FileTest {

  @Test def getTableFromFileTest(): Unit = {
    val file: File = new File("src/main/resources/games/game1.txt")
    val table: Array[Array[Char]] = FileUtil.getTableFromFile(file)

    //table in file
    //      4-129--75
    //      2--3--8-P
    //      -7--8---6
    //      ---1-3-62
    //      1-5---4-3
    //      73-6-8---
    //        6---2--3-
    //        --7--1--4
    //      89--651-7

    Assert.assertEquals('2', table(1)(0))
    Assert.assertEquals('5', table(0)(8))
    Assert.assertEquals('9', table(8)(1))
    Assert.assertEquals('P', table(1)(8))
  }

  @Test def getSequenceFromFileTest() = {
    val file: File = new File("src/main/resources/sequences/seq1.txt")
    val seq: Array[Char] = FileUtil.getSeqFromFile(file)

    //sequence
    //d l 1 r 5

    Assert.assertEquals('l', seq(1))
    Assert.assertEquals('r', seq(3))
    Assert.assertEquals('5', seq(4))
  }

  @Test def getInvalidSequenceFromFileTest() = {
    val file: File = new File("src/main/resources/sequences/se1_invalid.txt")

    //sequence
    //l k 3

    try {
      val seq: Array[Char] = FileUtil.getSeqFromFile(file)
      Assert.assertEquals(1, 0)
    } catch {
      case e: Exception => Assert.assertEquals(1, 1)
    }
  }

  @Test def saveInvalidCommandToFileTest(): Unit ={
    val pair1: Pair[String, String] = new Pair("filename.txt", "transpoose")
    val pair2: Pair[String, String] = new Pair("filename.txt", "transpose-filterRnC-substitute")
    val pair3: Pair[String, String] = new Pair("filename.txt", "transpose-filterRnC MC-substitute")
    val save1: Boolean = FileUtil.saveCommandToFile(pair1)
    val save2: Boolean = FileUtil.saveCommandToFile(pair2)
    val save3: Boolean = FileUtil.saveCommandToFile(pair3)

    Assert.assertEquals(save1, false)
    Assert.assertEquals(save2, false)
    Assert.assertEquals(save3, false)
  }

  @Test def getCommandsFromFileTest(): Unit ={
    val file : File = new File("src/main/resources/named/command/filter1to2.txt")
    val commands : Array[String] = FileUtil.getCommandsFromFile(file)

    //filter1to2 - returns only original commands
    //filter1s
    //  filterRnC 1
    //  filterInnerGrid 1
    //filter2s
    //  filterRnC 2
    //  filterInnerGrid 2

    Assert.assertEquals(commands(0), "filterRnC 1")
    Assert.assertEquals(commands(2), "filterRnC 2")
    Assert.assertEquals(commands(3), "filterInnerGrid 2")
  }
}
