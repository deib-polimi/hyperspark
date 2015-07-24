
object testnemanja {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(65); 
  println("Welcome to the Scala worksheet");$skip(18); 

   
   val m = 5;System.out.println("""m  : Int = """ + $show(m ));$skip(12); 
  val n = 5;System.out.println("""n  : Int = """ + $show(n ));$skip(35); 
  val row1 = Array(5, 9, 8, 10, 1);System.out.println("""row1  : Array[Int] = """ + $show(row1 ));$skip(35); 
  val row2 = Array(9, 3, 10, 1, 8);System.out.println("""row2  : Array[Int] = """ + $show(row2 ));$skip(34); 
  val row3 = Array(9, 4, 5, 8, 6);System.out.println("""row3  : Array[Int] = """ + $show(row3 ));$skip(34); 
  val row4 = Array(4, 8, 8, 7, 2);System.out.println("""row4  : Array[Int] = """ + $show(row4 ));$skip(34); 
  val row5 = Array(3, 5, 6, 3, 7);System.out.println("""row5  : Array[Int] = """ + $show(row5 ));$skip(46); 
  val d = Array(row1, row2, row3, row4, row5);System.out.println("""d  : Array[Array[Int]] = """ + $show(d ));$skip(15); 
  def jobs = m;System.out.println("""jobs: => Int""");$skip(19); 
  def machines = n;System.out.println("""machines: => Int""");$skip(17); 
  def delays = d;System.out.println("""delays: => Array[Array[Int]]""");$skip(38); 

  val np = n - 1;System.out.println("""np  : Int = """ + $show(np ));$skip(33);  //num of machines-1
  val mp = m - 1;System.out.println("""mp  : Int = """ + $show(mp ));$skip(1235);  //num of jobs-1
	
  def eval(solution: Solution): EvaluatedSolution = {
    val jobs = solution.permutation
    def encapsulate(value: Value) = new EvaluatedSolution(value, jobs)
    def initTimes(values: Array[Int]): Array[Int] = {
      def iterInitPerm(index: List[Int], acc: List[Int]): List[Int] = index match {
        case List()  => sum(acc).reverse
        case x :: xs => iterInitPerm(xs, values(x) :: acc)
      }
      iterInitPerm(jobs.toList, List()).toArray
    }
    def sum(list: List[Int]): List[Int] = list match {
      case List()  => List()
      case x :: xs => x + xs.foldRight(0)(_ + _) :: sum(xs)
    }
    var jobEndTimes = initTimes(delays(0))
    def apply(machine: Int, index: Int): Unit =
      if (index > 0)
        jobEndTimes(index) = Math.max(jobEndTimes(index), jobEndTimes(index - 1)) + delays(machine)(jobs(index))
      else
        jobEndTimes(index) = jobEndTimes(index) + delays(machine)(jobs(index))
    def iterEval(machine: Int): Value = {
      //println(jobEndTimes.mkString(" "))
      if (machine == np) jobEndTimes(jobEndTimes.length - 1)
      else {
        Array.tabulate(jobEndTimes.length)(i => apply(machine + 1, i))
        iterEval(machine + 1)
      }
    }
    encapsulate(iterEval(0))
  };System.out.println("""eval: (solution: <error>)<error>""")}
}
