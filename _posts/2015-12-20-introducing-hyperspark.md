---
layout: post
title: Introducing Hyperspark
---


### What is this project for? ###

HyperSpark is a framework for running meta-heuristic algorithms on a cluster of commodity computers. The project is written in Scala and the cluster is managed by Spark. 

The current version of the project implements the algorithms for Permutation Flowshop problems (PFSP), but it could be easily extended for any kind of problem. 

The general idea is to use "Big Calculations" paradigm for running algorithms written by the user. What does that mean? It means that we perform some heavy computation algorithms on a cluster of machines, and we strive to optimize the usage of resources and time.

### How to use the framework? ###

- The user writes its Problem, Solution, EvaluatedSolution and Algorithm classes. 
- The user specifies how many algorithms will be run in parallel to evaluate the problem. 
- What user could do is to define how the seeding solution (if exists) is provided to each of the algorithms that are run in parallel and how the results of the algorithms are combined (min, max, sum,etc.) and whether if the final result will be reused for another desired parallel run.

### What do I need to implement? ###

- Extend the "Problem" class (or use the existing one for PFSP). 
- Extend the "Solution" and "EvaluatedSolution" class (or use the existing one for PFSP).
- Implement evaluate(s: Solution) inside Problem class.
- Extend the "Algorithm" trait and implement evaluate function signatures inside your custom algorithm class.
- Write your application that uses HyperSpark framework. For a start, create a Scala object with main function. Inside the main write a FrameworkConf and provide it a problem, an algorithm, a parallelism multiplier, seeding solutions and the time limit for one parallel run. See *it.polimi.hyperh.apps.LocalApp.scala* example.
- Use Framework.run(conf: FrameworkConf) or Framework.multipleRuns(conf: FrameworkConf, runs: Int) to get solution(s). There is an advanced option of using the best solution found of one parallel run as a seeding solution in the next iteration of a parallel run. This option is currently under mantainance.
- Inside MapReduceHandler.class there are two methods: hyperMap(...) - which runs the algorithm.evaluate over the problem, and hyperReduce(...) - which for now takes the solution with minimum value. For purposes other than PFSP problems, this class should be extended and than a custom MapReduceHandler should be set by using Framework.setMapReduceHandler(...).

### Which algorithms are available? ###

- **NEH**, Nawaz, Enscore and Ham (NEHAlgorithm)
- **Iterated Greedy** (IGAlgorithm)
- **Genetic Algorithm**, Reeves 1995 (GAAlgorithm)
- **Hybrid Genetic Algorithm**, Zheng 2003 (HGAAlgorithm)
- **Simulated Annealing**, Osman's addaption for PFSP 1989 (SAAlgorithm)
- **Improved Simulated Annealing**, Xu and Oja (ISAAlgorithm)
- **Taboo Search**, Taillard 1989 (TSAlgorithm)
- **Taboo Search with backjump tracking**, Novicki and Smutnicki 1994 (TSABAlgorithm)
- **Max Min Ant System**, Stutzle (MMASAlgorithm)
- **m-MMAS**, Rajendran and Ziegler 2002 (MMMASAlgorithm)
- **PACO**, Rajendran and Ziegler 2002 (PACOAlgorithm)