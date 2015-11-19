#!/bin/bash -e


#clear the terminal
clear
declare -a instances=('1' '31' '61' '91');			#inst_ta001: 20 jobs,inst_ta031: 50 jobs,inst_ta061: 100 jobs,inst_ta091: 200 jobs
declare -a cores=('1' '8' '16' '24' '32' '40' '48');
for i in "${instances[@]}"; do							#for each instance
	for c in "${cores[@]}"; do							#for each number of cores to use
		for run in $(seq 1 10); do						#run 10 times
			#execute the command
			echo "submitting a jar... parsing application state to get application id..."
			app_id=`grep 'tracking' <<< "$(exec spark-submit --conf spark.root.logger=INFO,console --num-executors $c --class it.polimi.hyperh.experiments.Experiment2 hyperh-0.0.1-SNAPSHOT.jar $i $c   2>&1)" | head -n1 | cut -c57-86`
			echo $app_id
			echo "collecting yarn logs...parsing yarn logs to get application's results..."
			log_contents=$(grep 'CustomLogger' <<< "$(exec yarn logs -applicationId $app_id)")
			echo "$log_contents" | sudo tee -a experiment2.txt
		done
	done
done


#run the script using "$(nohup bash experiment2.sh  &)"
#see running processes using "ps -fu ubuntu"
#stop this script using "pkill -f experiment2.sh"