#!/bin/bash -e


#clear the terminal
clear
declare -a instances=('1' '31' '61' '91');			#inst_ta001: 20 jobs,inst_ta031: 50 jobs,inst_ta061: 100 jobs,inst_ta091: 200 jobs
declare -a cores=('1' '8' '16' '24' '32' '40');
for i in "${instances[@]}"; do							#for each instance
	for c in "${cores[@]}"; do							#for each number of cores to use
		for run in $(seq 1 5); do						#run 10 times
			#execute the command
			echo "submitting a jar..."
			log_contents="`/opt/spark/bin/spark-submit --conf spark.root.logger=INFO,console --master spark://master:7077 --conf spark.cores.max=$c  --conf spark.executor.cores=1 --conf spark.executor.memory=2G --class it.polimi.hyperh.experiments.Experiment1  hyperh-0.0.1-SNAPSHOT.jar $i $c   2>&1`"
			echo "parsing application state to get application id..."
			app_id=`echo "$log_contents" | grep "Connected to Spark cluster with app ID" | head -n1 | cut -c100-122`
			echo "$app_id"
			echo "saving log to a file..."
			filename="${app_id}.txt"
			echo "$filename"
			echo "$log_contents" > "$filename"
			echo "done"
		done
	done
done


#run the script using "$(nohup bash experiment1.sh  &)"
#see running processes using "ps -fu ubuntu"
#stop this script using "pkill -f experiment1.sh"