for run in {1..25}
do
	../randwalk -S 0xea3495cc76b34acc -n 5 -s 16 -t 14 -v 0 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results4_0_5	
	../randwalk -S 0xea3495cc76b34acc -n 6 -s 16 -t 14 -v 0 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results4_0_6
	../randwalk -S 0xea3495cc76b34acc -n 7 -s 16 -t 14 -v 0 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results4_0_7

	../randwalk -S 0xea3495cc76b34acc -n 5 -s 16 -t 14 -v 1 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results4_1_5	
	../randwalk -S 0xea3495cc76b34acc -n 6 -s 16 -t 14 -v 1 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results4_1_6
	../randwalk -S 0xea3495cc76b34acc -n 7 -s 16 -t 14 -v 1 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results4_1_7
done
