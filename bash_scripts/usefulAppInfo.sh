#!/bin/bash -e

#MODIFY THREE FOLLOWING VARIABLES FOR YOUR OWN PURPOSES
container="application_1445451111831"	#id of the container
from_id=807								#id of the first application ran
to_id=807 #1366								#id of the last application ran
#clear the terminal
clear

padtowidth=4	#expand ids with leading zeroes up to 4 digits
stage_limit=0
for i in $(seq $from_id $to_id); do #seq 807 1366
	paddedInt="$(printf "%0*d\n" $padtowidth $i)"
	app_id="${container}_${paddedInt}"
	filename="${app_id}.txt"
	echo "parsing $filename"
	line1=$(grep 'yarn.ApplicationMaster: Registered signal handlers' $filename)
	line2=$(grep 'postStartHook done' $filename)
	line3=$(grep 'util.CustomLogger: inst_ta' $filename) #determine mode from this line
	#execute the command
	#echo "${line1}${line2}${line3}${line4}${line5}" >> "${app_id}_parsed.txt"
	echo "${line1}" >> "${app_id}_parsed.txt"
	echo "${line2}" >> "${app_id}_parsed.txt"
	if [[ $line3 =~ .*cooperative.* ]]
	then
		echo "cooperative log"
		stage_limit=9
		stage0=$(grep 'Stage 0 (reduce at Framework.scala:91) finished' $filename)
		stage1=$(grep 'Stage 1 (reduce at Framework.scala:91) finished' $filename)
		stage2=$(grep 'Stage 2 (reduce at Framework.scala:91) finished' $filename)
		stage3=$(grep 'Stage 3 (reduce at Framework.scala:91) finished' $filename)
		stage4=$(grep 'Stage 4 (reduce at Framework.scala:91) finished' $filename)
		stage5=$(grep 'Stage 5 (reduce at Framework.scala:91) finished' $filename)
		stage6=$(grep 'Stage 6 (reduce at Framework.scala:91) finished' $filename)
		stage7=$(grep 'Stage 7 (reduce at Framework.scala:91) finished' $filename)
		stage8=$(grep 'Stage 8 (reduce at Framework.scala:91) finished' $filename)
		stage9=$(grep 'Stage 9 (reduce at Framework.scala:91) finished' $filename)
		echo "${stage0}" >> "${app_id}_parsed.txt"
		echo "${stage1}" >> "${app_id}_parsed.txt"
		echo "${stage2}" >> "${app_id}_parsed.txt"
		echo "${stage3}" >> "${app_id}_parsed.txt"
		echo "${stage4}" >> "${app_id}_parsed.txt"
		echo "${stage5}" >> "${app_id}_parsed.txt"
		echo "${stage6}" >> "${app_id}_parsed.txt"
		echo "${stage7}" >> "${app_id}_parsed.txt"
		echo "${stage8}" >> "${app_id}_parsed.txt"
		echo "${stage9}" >> "${app_id}_parsed.txt"
	else
		echo "parallel log"
		stage0=$(grep 'Stage 0 (reduce at Framework.scala:91) finished' $filename)
		echo "${stage0}" >> "${app_id}_parsed.txt"
	fi
	#calculate average stage time
	#calculate stopping time
	line4=$(grep 'Final app status' $filename)
	line5=$(grep 'Successfully stopped SparkContext' $filename)
	echo "${line4}" >> "${app_id}_parsed.txt"
	echo "${line5}" >> "${app_id}_parsed.txt"
	
	echo "stages ran from 0 to ${stage_limit}"
	echo "finished..."
done

#run the script using "$(nohup bash logparser.sh  &)"
#see running processes using "ps -fu ubuntu"
#stop this script using "pkill -f logparser.sh"