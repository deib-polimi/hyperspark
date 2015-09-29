package util

import scala.io.Source
import it.polimi.hyperh.problem.Problem
import scala.tools.nsc.io.File
import scala.tools.nsc.io.Path


/**
 * @author Nemanja
 */
object DelphiInputConverter {
  
  def getMString(matrix: Array[Array[Int]]): String = {
    var str: String = ""
    val rsize = matrix.size
    val csize = matrix(0).size
    for(i<-0 until rsize) {
      for(j<-0 until csize) {
        val value = matrix(i)(j).toString
        value.size match {
          case 1 => str = str + " "+ value
          case _ => str = str + value
        }
        if(j<csize-1)
          str = str + " "
      }
      if(i<rsize-1)
        str = str + "\n"
    }
    str
  }
  def main(args : Array[String]) {
    val indir:String = "D:/Dropbox/Teza - Nemanja/benchmarks/Talillard-Delphi/"
    val outdir:String = "D:/Dropbox/Teza - Nemanja/benchmarks/Talillard-Scala/"
    def filename(prefix: String, i: Int) = {
      val str = i.toString
      str.size match {
        case 1 => prefix+"00"+str+".txt"
        case 2 => prefix+"0"+str+".txt"
        case _ => prefix+str+".txt"
      }
    }
   def processInstance(i: Int) = {
      val inpath = indir + filename("Ta",i)
      val p = DelphiProblemParser(Source.fromFile(inpath).getLines().mkString(" x ") + " x ").getOrElse(throw new RuntimeException("ParserError"))
      val filecontent = p.numOfJobs+" "+p.numOfMachines + "\n" + getMString(p.jobTimesMatrix)
      val outpath = outdir + filename("inst_ta",i)
      FileManager.write(outpath, filecontent)
   }
   for(i<-1 to 120)
     processInstance(i)
  }
}