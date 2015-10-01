# README #
### What is this repository for? ###

HyperSpark is a framework for running meta-heuristic algorithms on a cluster of commodity computers. The project is written in Scala and the cluster is managed by Spark. 

The current version of the project implements the algorithms for Permutation Flowshop problems (PFSP), but it could be easily extended for any kind of problem. 

The general idea is to use "Big Calculations" paradigm for running algorithms written by the user. What does that mean? It means that we perform some heavy computation algorithms on a cluster of machines, and we strive to optimize the usage of resources and time.

### How to use the framework? ###

The user writes its Problem, Solution, EvaluatedSolution and Algorithm classes. 
The user specifies how many algorithms will be run in parallel to evaluate the problem. 
What user could do is to define how the seeding solution (if exists) is provided to each of the algorithms that are run in parallel and how the results of the algorithms are combined (min, max, sum,etc.) and whether if the final result will be reused for another desired parallel run.

### Implementation guide ###

*Extend the "Problem" class (or use the existing one for PFSP). 
*Extend the "Solution" and "EvaluatedSolution" class (or use the existing one for PFSP).
*Implement evaluate(s: Solution) inside Problem class.
*Extend the "Algorithm" trait and implement evaluate function signatures inside your custom algorithm class.
*Write your application that uses HyperSpark framework. For a start, create a Scala object with main function. Inside the main write a FrameworkConf and provide it a problem, an algorithm, a parallelism multiplier, seeding solutions and the time limit for one parallel run. See *it.polimi.hyperh.App.scala* example.
*Use Framework.run(conf: FrameworkConf) or Framework.multipleRuns(conf: FrameworkConf, runs: Int) to get solution(s). There is an advanced option of using the best solution found of one parallel run as a seeding solution in the next iteration of a parallel run. This option currently under mantainance.
*Inside Framework.class there are two methods: hyperMap(...) - which runs the algorithm.evaluate over the problem, and hyperReduce(...) - which for now takes the solution with minimum value. In the near future these options will be separated in the custom classes, so they can be easily extended and modified.

### Contact info ###
Nemanja Stolic
M.Sc. student at Politecnico di Milano University, Milan, Italy
nemanja.stolic@mail.polimi.it