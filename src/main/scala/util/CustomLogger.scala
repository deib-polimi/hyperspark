package util

import org.apache.spark.Logging
/**
 * @author Nemanja
 */
class CustomLogger extends Logging {
  protected var params: List[String] = List()
  protected def reformat(ps: List[String]) = {
    def produceBlanks(N: Int) = {
      if(N==0) ""
      else
        (for(i<-1 to N) yield " ").reduceLeft(_ concat _).concat("\t")
    }
    def fixsize(str: String) = {
      str.concat(produceBlanks(15-str.size))
    }
    ps.map { x => fixsize(x) }
  }
  def setFormat(parameters: List[String]) {
    params = parameters
    params = reformat(params)
  }
  def getFormatString(): String = {
    val toprint = params.reduceLeft(_ concat _).concat("\n")
    toprint
  }
  def printInfo(msg: String) = {
    print(msg)
    logInfo(msg)
  }
  def printFormat() = { printInfo(getFormatString()) }
  def getValuesString(values: List[Any]): String = {
    reformat(values.map { x => x.toString() }).reduceLeft(_ concat _).concat("\n")
  }
  def printValues(values: List[Any]) = { printInfo(getValuesString(values)) }
}
object CustomLogger {
  def apply() = new CustomLogger()
  def apply(parameters: List[String]) = new CustomLogger().setFormat(parameters)
}