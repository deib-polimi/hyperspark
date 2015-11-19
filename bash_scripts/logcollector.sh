#!/bin/bash -e

#MODIFY THREE FOLLOWING VARIABLES FOR YOUR OWN PURPOSES
container="application_1445451111831"	#id of the container
from_id=807								#id of the first application ran
to_id=1366								#id of the last application ran
#clear the terminal
clear

padtowidth=4	#expand ids with leading zeroes up to 4 digits
for i in $(seq $from_id $to_id); do #seq 807 1366
	echo "collect data about application id "
	paddedInt="$(printf "%0*d\n" $padtowidth $i)"
	app_id="${container}_${paddedInt}"
	#echo $app_id
	filename="${app_id}.txt"
	#execute the command
	"$(exec yarn logs -applicationId $app_id &> $filename)"
	echo "finished..."
done

#run the script using "$(nohup bash logcollector.sh  &)"
#see running processes using "ps -fu ubuntu"
#stop this script using "pkill -f logcollector.sh"
