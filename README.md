# README #
### What is this repository for? ###

HyperSpark is a framework for running meta-heuristic algorithms on a cluster of commodity computers. The project is written in Scala and the cluster is managed by Spark. 

The current version of the project implements the algorithms for Permutation Flowshop problems (PFSP), but it could be easily extended for any kind of problem. 

The general idea is to use "Big Calculations" paradigm for running algorithms written by the user. What does that mean? It means that we perform some heavy computation algorithms on a cluster of machines, and we strive to optimize the usage of resources and time.

### Goals of the project ###

- Examine the performance of popular algorithms for PFSP when run in Scala/Spark environment
- Show that the performance of one parallel run is more efficient than the performance of single algorithm run
- Support the execution of both light-weight and CPU-intensive parallel algorithms.
- Show that reusing the best solution found during a single parallel run for next runs is more efficient than just taking the best solution out of N independent runs.
- Show that Scala/Spark environment suits better for implementation of cooperative algorithms than Hadoop's environment.
- Simplicity: Show that the use of Scala/Spark environment is much simpler than using Hadoop environment, and that there is almost no setup of cluster using our framework.

### How to use the framework? ###

- The user writes its Problem, Solution, EvaluatedSolution and Algorithm classes. 
- Optionally, the user implements a StoppingCondition or uses an existing one "TimeExpired".
- Optionally, the user implements a MapReduceHandler, or uses the default one (already set in FrameworkConf class).
- The user specifies how many algorithms will be run in parallel to evaluate the problem. 
- The user specifies a stopping condition for parallel runs. (S)he uses the implemented TimeExpired stopping condition, or (s)he writes a custom stopping condition class.
- What user could do is to define how the seeding solution (if exists) is provided to each of the algorithms that are run in parallel and how the results of the algorithms are combined (min, max, sum,etc.) and whether if the final result will be reused for another desired parallel run.

### Implementation guide ###

- Extend the "Problem" class (or use the existing one for PFSP, PFSProblem). 
- Extend the "Solution" and "EvaluatedSolution" class (or use the existing one for PFSP, PFSSolution and PFSEvaluatedSolution).
- Implement evaluate(s: Solution) inside Problem class.
- Extend the "Algorithm" trait and implement evaluate function signatures inside your custom algorithm class.
- Extend the "StoppingCondition" and implement isSatisfied() function. Optionally, use already implemented "TimeExpired" stopping condition class.
- Write your application that uses HyperSpark framework. For start, create a Scala object with main function. Inside the main write a FrameworkConf and provide it a problem, an algorithm, a parallelism multiplier, seeding solutions and the stopping condition for one parallel run. See *it.polimi.hyperh.apps.LocalApp.scala* example.
- Use Framework.run(conf: FrameworkConf) or Framework.multipleRuns(conf: FrameworkConf, runs: Int) to get solution(s). There is an advanced option of using the best solution found of one parallel run as a seeding solution in the next iteration of a parallel run. This option can be enabled by creating/using an existing "SeedingStrategy" class, and setting in inside FrameworkConf before calling Framework.run method.
- Inside MapReduceHandler.class there are two methods: hyperMap(...) - which runs the algorithm.evaluate over the problem, and hyperReduce(...) - which for now takes the solution with minimum value. For purposes other than PFSP problems, this class should be extended and than a custom MapReduceHandler should be set by using FrameworkConf.setMapReduceHandler(h: MapReduceHandler).

### Algorithms implemented ###

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

### License info ###

The tool is available for use and improvement under a dual-licensing schema envisioning an Apache 2 license as well as a GPL-V3 License. Please contact the repository owners for further clarifications.
