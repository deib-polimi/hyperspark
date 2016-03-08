#!/bin/bash -e
i=1
c=20
a="HGAAlgorithm"
s="SameSeeds"
echo "submitting a jar..."
log_contents="`/opt/spark/bin/spark-submit --conf spark.root.logger=INFO,console --master spark://master:7077 --conf spark.cores.max=$c  --conf spark.executor.cores=1 --conf spark.executor.memory=2G --class it.polimi.hyperh.experiments.Experiment2  hyperh-0.0.1-SNAPSHOT.jar $i $c $a $s   2>&1`"
echo "parsing application state to get application id..."
app_id=`echo "$log_contents" | grep "Connected to Spark cluster with app ID" | head -n1 | cut -c100-122`
echo "$app_id"
echo "saving log to a file..."
filename="${app_id}.txt"
echo "$filename"
echo "$log_contents" > "$filename"