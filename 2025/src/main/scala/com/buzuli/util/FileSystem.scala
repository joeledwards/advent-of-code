package com.buzuli.util

import java.io.File
import scala.io.Source

object FileSystem {
  def readFileLines(fileName: String): List[String] = {
    val source = Source.fromFile(fileName)
    source.getLines.toList
  }

  def dataFiles: List[File] = {
    val dataDir = new File("./data")
    dataDir.listFiles(_.isFile).toList
  }
}
