package util

import scala.io.Source
import scala.tools.nsc.io.File
import scala.tools.nsc.io.Path

/**
 * @author Nemanja
 */
object BestSolutionsGenerator {
  def main(args : Array[String]) {
    val indir:String = "D:/Dropbox/Teza - Nemanja/benchmarks/Talillard-Scala/"
    val outdir:String = "D:/Dropbox/Teza - Nemanja/benchmarks/Talillard-Scala/"
    def filename(prefix: String, i: Int) = {
      val str = i.toString
      str.size match {
        case 1 => prefix+"00"+str+".txt"
        case 2 => prefix+"0"+str+".txt"
        case _ => prefix+str+".txt"
      }
    }
    def getContent(filename: String): Array[String]= {
      val inpath = indir + filename
      val content = Source.fromFile(inpath).getLines().toArray
      content
    }
   def processInstance(content: Array[String], i: Int) = {
      val filecontent = content(i-1)
      val outpath = outdir + filename("sol_ta",i)
      Path(outpath).createFile().writeAll(filecontent)
   }
   val content = getContent("bestFoundSolutions.txt")
   for(i<-1 to 120)
     processInstance(content, i)
  }
}