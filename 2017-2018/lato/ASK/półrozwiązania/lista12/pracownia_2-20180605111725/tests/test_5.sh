for run in {0..25}
do
	../bsearch -S 0x5bab3de5da7882ff -n 20 -t 20 -v 0 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results_5_20_20_0
	../bsearch -S 0x5bab3de5da7882ff -n 20 -t 20 -v 1 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results_5_20_20_1

	../bsearch -S 0x5bab3de5da7882ff -n 15 -t 24 -v 0 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results_5_15_24_0
	../bsearch -S 0x5bab3de5da7882ff -n 15 -t 24 -v 1 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results_5_15_24_1

	../bsearch -S 0x5bab3de5da7882ff -n 24 -t 20 -v 0 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results_5_24_20_0
	../bsearch -S 0x5bab3de5da7882ff -n 24 -t 20 -v 1 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results_5_24_20_1
	clear
	echo "$run"
done
