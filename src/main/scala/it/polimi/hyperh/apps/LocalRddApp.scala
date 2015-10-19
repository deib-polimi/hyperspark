package it.polimi.hyperh.apps

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import scala.util.Random

/**
 * @author Nemanja
 */
class LocalRddApp {
  def run() {
    val arr = (for(i<-0 to 99) yield (i, Random.nextDouble)).toArray
    val sconf = new SparkConf().setAppName("test").setMaster("local[*]")
    val sc = new SparkContext(sconf)
    val rdd = sc.parallelize(arr)
    println(rdd.count())
    val newrdd = rdd.map(tupple => (tupple._1 / 25, tupple._2))
    val rdd1 = newrdd.filter(t => t._1 == 0)
    val rdd2 = newrdd.filter(t => t._1 == 1)
    val rdd3 = newrdd.filter(t => t._1 == 2)
    val rdd4 = newrdd.filter(t => t._1 == 3)
    println("rdd1 size "+rdd1.count)
    println("rdd2 size "+rdd2.count)
    println("rdd3 size "+rdd3.count)
    println("rdd4 size "+rdd4.count)
  }
}
object LocalRddApp {
  def main(args: Array[String]) {
    new LocalRddApp().run()
  }
}
