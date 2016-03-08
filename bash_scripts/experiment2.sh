#!/bin/bash -e


#clear the terminal
clear
a="HGAAlgorithm"	#algorithms ('HGAAlgorithm' 'IGAlgorithm');
s="SameSeeds"		#strategies ('SameSeeds' 'SeedPlusSlidingWindow' 'SeedPlusFixedWindow');
c=20 				#number of cores
#inst_ta031-inst_ta060: 50 jobs,inst_ta061-inst_ta090: 100 jobs,inst_ta091-inst_ta110: 200 jobs
from_inst=31
to_inst=110
for i in $(seq $from_inst $to_inst); do							#for each instance
	for run in $(seq 1 10); do									#run 10 times
		#execute the command
		echo "submitting a jar..."
		log_contents="`/opt/spark/bin/spark-submit --conf spark.root.logger=INFO,console --master spark://master:7077 --conf spark.cores.max=$c  --conf spark.executor.cores=1 --conf spark.executor.memory=2G --class it.polimi.hyperh.experiments.Experiment2  hyperh-0.0.1-SNAPSHOT.jar $i $c $a $s   2>&1`"
		echo "parsing application state to get application id..."
		app_id=`echo "$log_contents" | grep "Connected to Spark cluster with app ID" | head -n1 | cut -c100-122`
		echo "$app_id"
		echo "saving log to a file..."
		filename="${app_id}.txt"
		echo "$filename"
		echo "_algorithm_used_ $a" >> "$filename"
		echo "_strategy_used_ $s" >> "$filename"
		echo "$log_contents" >> "$filename"
		echo "done"
	done
done


#run the script using "$(nohup bash experiment2.sh  &)"
#see running processes using "ps -fu ubuntu"
#stop this script using "pkill -f experiment2.sh"