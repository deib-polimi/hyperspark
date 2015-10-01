package it.polimi.hyperh.algorithms

import it.polimi.hyperh.problem.Problem
import it.polimi.hyperh.solution.Solution
import it.polimi.hyperh.search.NeighbourhoodSearch
import it.polimi.hyperh.solution.EvaluatedSolution
import util.Timeout
import util.RNG
import it.polimi.hyperh.solution.DummyEvaluatedSolution

/**
 * @author Nemanja
 */
class TSABAlgorithm(
    sd: Option[Solution]
    ) extends TSAlgorithm(sd) {
  /**
   * A secondary constructor.
   */
  private var maxt: Int = 8
  private var maxret: Int = 1000
  private var maxiter: Int = 30000
  private var numOfRandomMoves: Int = 20
  private var neighbourhoodSearch: (List[Int], Int, Int) => List[Int] = NeighbourhoodSearch(random).INSdefineMove
  
  def this(maxt: Int,numOfRandomMoves: Int, neighbourhoodSearch: (List[Int], Int, Int) => List[Int],maxret: Int,maxiter: Int, seed: Option[Solution], rng: RNG) {
    this(seed)
    this.maxt = maxt
    this.numOfRandomMoves = numOfRandomMoves
    this.neighbourhoodSearch = neighbourhoodSearch
    this.maxret = maxret
    this.maxiter = maxiter
    
  }
  def this() {
    this(None)
  }
  def getEpsilon(numOfJobs: Int, numOfMachines: Int): Double = {
    val nOverM = numOfJobs / numOfMachines
    if (nOverM > 3)
      0.0
    else if (nOverM > 2 && nOverM <= 3)
      0.5
    else
      1.0
  }

  def critMatrix(p: Problem, jobsPermutation: Array[Int]): Array[Array[Int]] = {
    val R = Array.ofDim[Int](p.numOfMachines, p.numOfJobs)
    R(0)(0) = p.jobTimesMatrix(0)(0)
    def rgh(g: Int, h: Int): Int = {
      if (g == -1 || h == -1)
        0
      else
        R(g)(h) //0..m-1,0..n-1
    }
    for (h <- 0 until p.numOfJobs; g <- 0 until p.numOfMachines) {
      R(g)(h) = scala.math.max(rgh(g, h - 1), rgh(g - 1, h)) + p.jobTimesMatrix(g)(jobsPermutation(h) - 1)
    }
    R
  }
  def criticalPath(p: Problem, jobsPermutation: Array[Int]) = {
    val R = critMatrix(p, jobsPermutation)
    def rgh(g: Int, h: Int): Int = {
      if (g == 0 || h == 0)
        0
      else
        R(g - 1)(h - 1) //1..m,1..n
    }
    var move = (p.numOfMachines, p.numOfJobs) //start from bottom right corner
    var path: List[(Int, Int)] = List(move)
    while (move._1 != 1 || move._2 != 1) {
      var better = move
      if (rgh(move._1 - 1, move._2) > rgh(move._1, move._2 - 1))
        better = (move._1 - 1, move._2)
      else better = (move._1, move._2 - 1)
      path = List(better) ::: path
      move = better
    }
    (path, rgh(p.numOfMachines, p.numOfJobs))
  }
  def getBlockLimits(path: List[(Int, Int)]): List[(Int, (Int, Int))] = {
    var blocks: List[(Int, (Int, Int))] = List()
    var i = 1
    var same = false
    var machine = path(0)._1
    var leftPos = path(0)._2
    var rightPos = path(0)._2
    while (i < path.size) {
      while ((path(i - 1)._1 != path(i)._1) && (i + 1 < path.size)) {

        i = i + 1
      }
      machine = path(i)._1
      leftPos = path(i - 1)._2
      rightPos = path(i)._2
      var notExceeded = true
      while (notExceeded && (path(i - 1)._1 == path(i)._1)) {
        machine = path(i)._1
        rightPos = path(i)._2
        i = i + 1
        if (i >= path.size)
          notExceeded = false
      }
      if (rightPos - leftPos > 0) {
        if (notExceeded)
          rightPos = path(i)._2
        else rightPos = path(path.size - 1)._2
        blocks = blocks ::: List((machine, (leftPos, rightPos)))
      }
      i = i + 1
    }
    blocks
  }
  def getMachines(blocks: List[(Int, (Int, Int))]): Array[Int] = {
    blocks.map(t => t._1).toArray
  }
  def getU(blocks: List[(Int, (Int, Int))]): Array[Int] = {
    blocks.filter(p => p._2._1 < p._2._2).map(p => p._2).flatMap(t => List(t._1, t._2)).distinct.toArray
  }
  //returns the greatest index on the right inside the block containing jPos
  def lr(jPos: Int, u: Array[Int]): Int = {
    var index = -999999
    for (i <- 0 until u.size - 1) {
      if (u(i) <= jPos && jPos < u(i + 1))
        index = u(i + 1)
    }
    index
  } //> lr: (jPos: Int, u: Array[Int])Int
  def ll(jPos: Int, u: Array[Int]): Int = {
    var index = -999999
    for (i <- 0 until u.size - 1) {
      if (u(i) < jPos && jPos <= u(i + 1))
        index = u(i)
    }
    if (jPos < 2 || jPos > u(u.size - 1))
      index = -999999
    index
  }
  def calculateDelta(u: Array[Int], epsilon: Double): Array[Int] = {
    val k = u.size - 1
    val delta: Array[Int] = Array.ofDim[Int](k + 2)
    delta(0) = 0
    for (l <- 1 to k)
      delta(l) = ((u(l) - u(l - 1)) * epsilon).toInt
    delta(k + 1) = 0
    delta
  } //> calculateDelta: (u: Array[Int], epsilon: Double)Array[Int]

  def calculateZRJ(p: Problem, jPos: Int, uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
    if (jPos < 1 || jPos > uArr(uArr.size - 1) - 1)
      List()
    if (mArr(mArr.size - 1) == p.numOfMachines && (jPos >= (uArr(uArr.size - 2) + 1)))
      List()
    else {
      val delta = calculateDelta(uArr, epsilon)
      val lright = lr(jPos, uArr)
      val deltaNext = delta(uArr.indexOf(lright) + 1)
      var sum = lright + deltaNext
      if (sum > p.numOfJobs)
        sum = p.numOfJobs
      (for (t <- lright to sum) yield (jPos, t)).toList
    }
  }
  def calculateZLJ(jPos: Int, uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
    def w(x: Int) = if (x > 1) 0 else 1
    if (jPos < 2 || jPos > uArr(uArr.size - 1)) {
      List()
    } else {
      if (mArr(0) == 1 && (jPos <= (uArr(1) - 1))) {
        List()
      } else {
        val delta = calculateDelta(uArr, epsilon)
        val lleft = ll(jPos, uArr)
        val deltaL = delta(uArr.indexOf(lleft))
        val lNext = uArr(uArr.indexOf(lleft) + 1)
        var diff1 = lleft - deltaL
        if (diff1 < 1)
          diff1 = 1
        val diff2 = lleft - w(lNext - lleft)
        (for (t <- diff1 to diff2) yield (jPos, t)).toList.filterNot(p => p._1 == p._2)
      }
    }
  }
  def calculateZR(p: Problem, uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
    var movesRight: List[(Int, Int)] = List()
    for (j <- 1 to p.numOfJobs - 1) {
      movesRight = movesRight ::: calculateZRJ(p, j, uArr, mArr, epsilon)
    }
    movesRight
  }
  def calculateZL(p: Problem, uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
    var movesLeft: List[(Int, Int)] = List()
    for (j <- 2 to p.numOfJobs) {
      movesLeft = movesLeft ::: calculateZLJ(j, uArr, mArr, epsilon)
    }
    movesLeft
  }
  def calculateZ(p: Problem, uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
    calculateZR(p, uArr, mArr, epsilon) ::: calculateZL(p, uArr, mArr, epsilon)
  }
  def generateMoves(p: Problem, uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {

    var movesRight: List[(Int, Int)] = List()
    for (j <- 1 to p.numOfJobs - 1) {
      val jMovesRight = calculateZRJ(p, j, uArr, mArr, epsilon)
      if (jMovesRight.size != 0)
        for (i <- 0 until jMovesRight.size)
          movesRight = movesRight ::: List(jMovesRight(i))
    }

    var movesLeft: List[(Int, Int)] = List()
    for (j <- 2 to p.numOfJobs) {
      val jMovesLeft = calculateZLJ(j, uArr, mArr, epsilon)
      if (jMovesLeft.size != 0)
        for (i <- 0 until jMovesLeft.size)
          movesLeft = movesLeft ::: List(jMovesLeft(i))
    }

    var moves = movesRight ::: movesLeft
    moves = moves.map(p => (p._1 - 1, p._2 - 1))
    moves
  }
  def findRepresentatives(p: Problem, evOldSolution: EvaluatedSolution, moves: List[(Int, Int)], uArr: Array[Int], mArr: Array[Int], epsilon: Double): List[(Int, Int)] = {
    var representatives: List[(Int, Int)] = List()
    def bestJRepresentative(j: Int, jMoves: List[(Int, Int)]): (Int, Int) = {
      var bestJRepr = DummyEvaluatedSolution(p)
      var bestJMove = (j - 1, j - 1)
      for (i <- 0 until jMoves.size) {
        val neighbour = neighbourhoodSearch.apply(evOldSolution.solution.toList, jMoves(i)._1, jMoves(i)._2)
        val evNeighbour = p.evaluate(Solution(neighbour))
        if (evNeighbour.value < bestJRepr.value) {
          bestJRepr = evNeighbour
          bestJMove = jMoves(i)
        }
      }
      bestJMove
    }
    if (epsilon == 0.0) {
      representatives = moves
    } else { //possible improvement: filter from moves, where m._1 < m._2 and m._1 = j
      var representativesRight: List[(Int, Int)] = List()
      for (j <- 1 to p.numOfJobs - 1) {
        val jMovesRight = calculateZRJ(p, j, uArr, mArr, epsilon).map(m => (m._1 - 1, m._2 - 1))
        if (jMovesRight.size != 0) {
          val bestJMove = bestJRepresentative(j, jMovesRight)
          representativesRight = representativesRight ::: List(bestJMove)
        }
      }
      //possible improvement: filter from moves, where m._1 > m._2 and m._1 = j
      var representativesLeft: List[(Int, Int)] = List()
      for (j <- 2 to p.numOfJobs) {
        val jMovesLeft = calculateZLJ(j, uArr, mArr, epsilon).map(m => (m._1 - 1, m._2 - 1))
        if (jMovesLeft.size != 0) {
          val bestJMove = bestJRepresentative(j, jMovesLeft)
          representativesLeft = representativesLeft ::: List(bestJMove)
        }
      }

      representatives = representativesRight ::: representativesLeft
    }
    representatives
  }
  def updateTabooList(tabooList: List[((Int, Int), (Int, Int))], move: (Int, Int), evOldSolution: EvaluatedSolution): List[((Int, Int), (Int, Int))] = {
    val a = move._1
    val b = move._2
    val P = evOldSolution.solution
    var t = ((0,0), (1,1))  //(move, jobs), dummy init
    if(a < b)
      t = ((a,b), (P(a), P(a+1)))
    else //b <= a
      t = ((a,b), (P(a-1), P(a)))
    if (tabooList.size == maxt) {
        //remove the oldest forbidden move, and add new move at the end
        tabooList.drop(1) ::: List(t)
      } else
        tabooList ::: List(t)
  }
  def isForbidden(tabooList: List[((Int, Int),(Int, Int))], evOldSolution: EvaluatedSolution, move: (Int,Int)): Boolean = {
    var answer = false
    val P = evOldSolution.solution
    val a = move._1
    val b = move._2
    val forbiddenJobPairs = tabooList.map(t => t._2)
    if(a < b) {
      for(j <- (a+1) to b) {
        val pair = (P(j), P(a))
        if(forbiddenJobPairs.contains(pair))
          answer = true
      }
    }
    else {  //b <= a
      for(j <- b to (a-1)) {
        val pair = (P(a), P(j))
        if(forbiddenJobPairs.contains(pair))
          answer = true
      }
    }
    answer
  }
  
  def filterTabooMoves(allMoves: List[(Int, Int)], tabooList: List[((Int, Int), (Int, Int))], evOldSolution: EvaluatedSolution): List[(Int, Int)] = {
    allMoves.filterNot(move => isForbidden(tabooList, evOldSolution, move))
  }
  //allow a taboo move to be performed if it improves the best solution
  def aspiration(p: Problem, bestSolution: EvaluatedSolution, forbiddenMoves: List[(Int,Int)], expireTimeMillis: Double) = {
    bestImprovement(p, bestSolution, forbiddenMoves, expireTimeMillis)
  }
  //update representative moves list
  def updateRepresentatives(representatives: List[(Int, Int)], move: (Int, Int)): List[(Int, Int)] = {
    representatives.filterNot((m: (Int, Int)) => (m._1 == move._1 && m._2 == move._2))
  }
  //separate allowed and forbidden moves
  def processMoves(moves: List[(Int, Int)], tabooList: List[((Int, Int), (Int, Int))], evOldSolution: EvaluatedSolution) = {
    var allowed: List[(Int, Int)] = List()
    var forbidden: List[(Int, Int)] = List()
    for (i <- 0 until moves.size) {
      if (isForbidden(tabooList, evOldSolution, moves(i)))
        forbidden = forbidden ::: List(moves(i))
      else
        allowed = allowed ::: List(moves(i))
    }
    (allowed, forbidden)
  }
  override def evaluate(p: Problem): EvaluatedSolution = {
    //algorithm time limit
    val timeLimit = p.numOfMachines * (p.numOfJobs / 2.0) * 60 //termination is n*(m/2)*60 milliseconds
    evaluate(p, timeLimit)
    
  }
  override def evaluate(p:Problem, timeLimit: Double):EvaluatedSolution = {
     val epsilon = getEpsilon(p.numOfJobs, p.numOfMachines)
    var evOldSolution = DummyEvaluatedSolution(p)
    var evBestSolution = evOldSolution
    var tabooOld: List[((Int, Int), (Int, Int))] = List.fill(maxt)(((0, 0), (0, 0)))
    var tabooNew: List[((Int, Int), (Int, Int))] = List.fill(maxt)(((0, 0), (0, 0)))
    var updTabooList: List[((Int, Int), (Int, Int))] = List.fill(maxt)(((0, 0), (0, 0)))
    var iter = 0
    var ret = 0
    var skip1 = false
    var moves: List[(Int, Int)] = List()
    var representatives: List[(Int, Int)] = List()
    var updRepresentatives: List[(Int, Int)] = List()
    var forbiddenMoves: List[(Int, Int)] = List()
    var save = true
    var exit = false
    var mArr = Array[Int](1)
    var uArr = Array[Int](1, 8)

    //algorithm time limit
    val expireTimeMillis = Timeout.setTimeout(timeLimit)

    def NSP() = {
      var move = (0, 0)
      var evNewSolution = evOldSolution
      var X = representatives
      var exit = false
      while (!exit) {
        if (X.size != 0 || forbiddenMoves.size != 0) {
          val allowedPair = bestImprovement(p, evOldSolution, representatives, expireTimeMillis)
          val aspirationPair = aspiration(p, evOldSolution, forbiddenMoves, expireTimeMillis)
          evNewSolution = allowedPair._1
          move = allowedPair._2
          if (aspirationPair._1.value < allowedPair._1.value) {
            evNewSolution = aspirationPair._1
            move = aspirationPair._2
          }
          tabooNew = updateTabooList(tabooOld, move, evOldSolution)
          exit = true
        } else { //step 3
          tabooOld = updateTabooList(tabooOld, move, evOldSolution)
          moves = generateMoves(p, uArr, mArr, epsilon)
          val afPair = processMoves(moves, tabooNew, evOldSolution)
          val allowedMoves = afPair._1
          X = findRepresentatives(p, evOldSolution, allowedMoves, uArr, mArr, epsilon)
        }

      } //while
      (evNewSolution, move)
    }

    while (ret < maxret && (!exit) && Timeout.notTimeout(expireTimeMillis)) {
      //step 1
      if (!skip1) {
        iter = iter + 1
        ret = ret + 1
        if(iter == 1) {
          evOldSolution = initialSolution(p)
          evBestSolution = evOldSolution
        }
        val pathC = criticalPath(p, evOldSolution.solution)._1 //pass the permutation
        val blocks = getBlockLimits(pathC)
        mArr = getMachines(blocks)
        uArr = getU(blocks)
        moves = generateMoves(p, uArr, mArr, epsilon)
        val afPair = processMoves(moves, tabooOld, evOldSolution)
        val allowedMoves = afPair._1
        forbiddenMoves = afPair._2
        representatives = findRepresentatives(p, evOldSolution, allowedMoves, uArr, mArr, epsilon)
      }
      skip1 = false
      //step 2
      //find the best representative
      val tupple = NSP()
      val evNewSolution = tupple._1
      val move = tupple._2
      if (save == true) {
        updRepresentatives = updateRepresentatives(representatives, move)
        updTabooList = tabooOld
        save = false
      }
      evOldSolution = evNewSolution
      tabooOld = tabooNew
      //step 3
      if (evOldSolution.value < evBestSolution.value) {
        evBestSolution = evOldSolution
        ret = 0
        save = true
        skip1 = false //go to step 1
      } else if (updRepresentatives.size != 0 && iter < maxiter) {
        evOldSolution = evBestSolution
        representatives = updRepresentatives
        tabooOld = updTabooList
        save = true
        ret = 1
        skip1 = true
      } else exit = true
    }
    evBestSolution
  }

}