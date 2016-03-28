#!/bin/bash -e


#clear the terminal
clear
declare -a instances=('7' '41' '42' '43' '47' '48' '49' '50' '51' '52' '53' '54' '56' '57' '58' '59' '60' '77' '78' '79' '81' '83' '84' '85' '86' '87' '88' '89' '90' '91' '92' '93' '94' '96' '97' '98' '100' '101' '102' '103' '107' '108' '109' '110' '111' '112' '113' '114' '115' '116' '117' '118' '119' '120');
c=20
for i in "${instances[@]}"; do							#for each instance
	#execute the command
	echo "submitting a jar..."
	log_contents="`/opt/spark/bin/spark-submit --conf spark.root.logger=INFO,console --master spark://master:7077 --conf spark.cores.max=$c  --conf spark.executor.cores=1 --conf spark.executor.memory=2G --class it.polimi.hyperh.experiments.Experiment3  hyperh-0.0.1-SNAPSHOT.jar $i   2>&1`"
	echo "parsing application state to get application id..."
	app_id=`echo "$log_contents" | grep "Connected to Spark cluster with app ID" | head -n1 | cut -c100-122`
	echo "$app_id"
	echo "saving log to a file..."
	filename="${app_id}.txt"
	echo "$filename"
	echo "_algorithm_used_ IGAlgorithm" >> "$filename"
	echo "_strategy_used_ SameSeeds" >> "$filename"
	echo "$log_contents" >> "$filename"
	echo "done"
done


#run the script using "$(nohup bash experiment3.sh  &)"
#see running processes using "ps -fu ubuntu"
#stop this script using "pkill -f experiment3.sh"