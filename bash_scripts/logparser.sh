#!/bin/bash -e

#MODIFY THREE FOLLOWING VARIABLES FOR YOUR OWN PURPOSES
container="application_1445451111831"	#id of the container
from_id=807								#id of the first application ran
to_id=1086								#id of the last application ran
echo "instance		jobs			machines		algorithm		cores			thread_time		found			best			rpd				mode						 stages		   	stage0_T		stage0_OVR		st_1to9_T		st_1to9_OVR		avgSt_1to9OVR		init			close		   app" >> "logs_parsed.txt"
padtowidth=4	#expand ids with leading zeroes up to 4 digits
stage_limit=0
num_of_stages=1
for i in $(seq $from_id $to_id); do 
	paddedInt="$(printf "%0*d\n" $padtowidth $i)"
	app_id="${container}_${paddedInt}"
	filename="${app_id}.txt"
	echo "parsing $filename"
	dataLine=$(grep 'util.CustomLogger: inst_ta' $filename) #determine mode from this line
	#total time provided to the algorithm
	thread_time=`awk '{print $10}' <<< $dataLine`
	#time for first stage
	stage0_T=`awk '{print $12}' <<< $(grep "Stage 0 (reduce at Framework.scala:91) finished" $filename)`
	stage0_OVR=`echo $stage0_T $thread_time | awk '{print $1 - $2}'`
	#calculate average stage time for stages after stage 0
	st_1to9_T=0.0
	st_1to9_OVR=0.0
	avgSt_1to9OVR=0.0 #for parallel log there aren't stages after stage0
	#first determine kind of the log
	if [[ $dataLine =~ .*cooperative.* ]]
	then
		echo "cooperative log"
		num_of_stages=10
		for st in $(seq 1 9); do
			stage_T=`awk '{print $12}' <<< $(grep "Stage ${st} (reduce at Framework.scala:91) finished" $filename)`
			st_1to9_T=`echo $st_1to9_T $stage_T | awk '{print $1 + $2}'`
		done
		perSt_TT=`echo $thread_time $num_of_stages | awk '{print $1 / $2}'`
		stage0_OVR=`echo $stage0_T $perSt_TT | awk '{print $1 - $2}'`
		st_1to9_TT=`echo $perSt_TT 9 | awk '{print $1 * $2}'`
		st_1to9_OVR=`echo $st_1to9_T $st_1to9_TT | awk '{print $1 - $2}'`
		avgSt_1to9OVR=`echo $st_1to9_OVR 9 | awk '{print $1 / $2}'`
	else
		echo "parallel log"
	fi
	
	#measure spark's initalization overhead
	line1=$(grep 'yarn.ApplicationMaster: Registered signal handlers' $filename)
	line2=$(grep 'postStartHook done' $filename)
	#line1
	#15/10/29 13:10:13 INFO yarn.ApplicationMaster: Registered signal handlers for [TERM, HUP, INT]
	#line2
	#15/10/29 13:10:40 INFO cluster.YarnClusterScheduler: YarnClusterScheduler.postStartHook done
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
	line4=$(grep 'Final app status' $filename)
	line5=$(grep 'Successfully stopped SparkContext' $filename)
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
	echo "${dataLine:42} ${padded1}${padded2}${padded3}${padded4}${padded5}${padded6}${padded7}${padded8}${padded9}" >> "logs_parsed.txt"
	echo "finished..."
done

#run the script using "$(nohup bash logparser.sh  &)"
#see running processes using "ps -fu ubuntu"
#stop this script using "pkill -f logparser.sh"
