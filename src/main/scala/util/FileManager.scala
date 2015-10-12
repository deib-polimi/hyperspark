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
    val f = new java.io.File(filepath);
    if(! f.getParentFile().exists())
      f.getParentFile().mkdirs();
    if (!f.exists())
      //f.createNewFile();
      Path(filepath).createFile().writeAll(content)  
  }
  def append(filepath: String, content: String) = {
    File(filepath).appendAll(content)
  }
}