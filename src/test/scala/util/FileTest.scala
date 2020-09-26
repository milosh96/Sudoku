package util

import java.io.File

import org.junit.{Assert, Test}

class FileTest {

  @Test def getTableFromFileTest(): Unit = {
    val file: File = new File("src/main/resources/games/game1.txt")
    val table: Array[Array[Char]] = FileUtil.getTableFromFile(file)

    //table provided in project problem
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
}
