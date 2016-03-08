#!/bin/bash -e
for filename in `ls app*`; do 
	num_of_stages=1
	app_id=${filename%.*}
	echo "parsing $filename"
	dataLine=$(grep 'util.CustomLogger: inst_ta' $filename) #determine mode from this line
	#total time provided to the algorithm
	thread_time=`awk '{print $10}' <<< $dataLine`
	#time for first stage
	stage0_T=`awk '{print $12}' <<< $(grep "Stage 0 (reduce at Framework.scala:90) finished" $filename)`
	stage0_OVR=`echo $stage0_T $thread_time | awk '{print $1 - $2}'`
	#calculate average stage time for stages after stage 0
	st_1to9_T=0.0
	st_1to9_OVR=0.0
	avgSt_1to9OVR=0.0 #for parallel log there aren't stages after stage0
	strategy="-" #seeding strategy. to be determined
	#first determine kind of the log
	if [[ $dataLine =~ .*cooperative.* ]]
	then
		echo "cooperative log"
		num_of_stages=10
		for st in $(seq 1 9); do
			stage_T=`awk '{print $12}' <<< $(grep "Stage ${st} (reduce at Framework.scala:90) finished" $filename)`
			st_1to9_T=`echo $st_1to9_T $stage_T | awk '{print $1 + $2}'`
		done
		perSt_TT=`echo $thread_time $num_of_stages | awk '{print $1 / $2}'`
		stage0_OVR=`echo $stage0_T $perSt_TT | awk '{print $1 - $2}'`
		st_1to9_TT=`echo $perSt_TT 9 | awk '{print $1 * $2}'`
		st_1to9_OVR=`echo $st_1to9_T $st_1to9_TT | awk '{print $1 - $2}'`
		avgSt_1to9OVR=`echo $st_1to9_OVR 9 | awk '{print $1 / $2}'`
		strategy=`awk '{print $2}' <<< $(grep "_strategy_used_" $filename)`
	else
		echo "parallel log"
	fi
	
	#measure spark's initalization overhead
	line1=$(grep 'spark.SparkContext: Running Spark version' $filename)
	line2=$(grep 'spark.SparkContext: Starting job: reduce at Framework.scala:90' $filename)
	#line1
	#16/01/21 11:10:43 INFO spark.SparkContext: Running Spark version 1.5.2
	#line2
	#16/01/21 11:10:47 INFO spark.SparkContext: Starting job: reduce at Framework.scala:90
	startDate=`awk '{print $1}' <<< $line1`
	startTime=`awk '{print $2}' <<< $line1`
	endDate=`awk '{print $1}' <<< $line2`
	endTime=`awk '{print $2}' <<< $line2`
	full1="${startDate} ${startTime}"
	full2="${endDate} ${endTime}"
	format1=${full1//[\/]/-}
	format2=${full2//[\/]/-}
	seconds1=$(date -d  "$format1" '+%s')
	seconds2=$(date -d "$format2" '+%s')
	initOverhead=`echo $seconds2 $seconds1 | awk '{print $1 - $2}'`
	echo "initOverhead ${initOverhead} s"
	
	#measure closing time of spark
	line4=$(grep 'Invoking stop() from shutdown hook' $filename)
	line5=$(grep 'Shutdown hook called' $filename)
	stDate=`awk '{print $1}' <<< $line4`
	stTime=`awk '{print $2}' <<< $line4`
	enDate=`awk '{print $1}' <<< $line5`
	enTime=`awk '{print $2}' <<< $line5`
	fl1="${stDate} ${stTime}"
	fl2="${enDate} ${enTime}"
	frm1=${fl1//[\/]/-}
	frm2=${fl2//[\/]/-}
	secs1=$(date -d  "$frm1" '+%s')
	secs2=$(date -d  "$frm2" '+%s')
	closingOverhead=`echo $secs2 $secs1 | awk '{print $1 - $2}'`
	echo "closingOverhead ${closingOverhead} s"
	
	#print to file
	padded1=`printf "\t%15s" $num_of_stages`
	padded2=`printf "\t%15s" $stage0_T`
	padded3=`printf "\t%15s" $stage0_OVR`
	padded4=`printf "\t%15s" $st_1to9_T`
	padded5=`printf "\t%15s" $st_1to9_OVR`
	padded6=`printf "\t%15s" $avgSt_1to9OVR`
	padded7=`printf "\t%15s" $initOverhead`
	padded8=`printf "\t%15s" $closingOverhead`
	padded9=`printf "\t%15s" $paddedInt`
	padded10=`printf "\t%25s" $strategy`
	echo "${dataLine:42} ${padded1}${padded2}${padded3}${padded4}${padded5}${padded6}${padded7}${padded8}${padded9}${padded10}" >> "logs_parsed.txt"
	echo "finished..."
done
#replace all tabs with spaces
sed -i 's/\t/ /g' logs_parsed.txt

#run the script using "$(nohup bash logparser.sh  &)"
#see running processes using "ps -fu ubuntu"
#stop this script using "pkill -f logparser.sh"
