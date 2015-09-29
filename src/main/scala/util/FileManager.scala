package util

import scala.io.Source
import scala.tools.nsc.io.File
import scala.tools.nsc.io.Path

/**
 * @author Nemanja
 */
object FileManager {
  def read(filepath: String) = {
    Source.fromFile(filepath).getLines()
  }
  def write(filepath: String, content: String) = {
    Path(filepath).createFile().writeAll(content)
  }
}