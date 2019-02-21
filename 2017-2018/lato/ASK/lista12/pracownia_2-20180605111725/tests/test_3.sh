for run in {1..100}
do
    ../transpose -n 4096 -v 0 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results3_0
done

for run in {1..100}
do
    ../transpose -n 4096 -v 1 | egrep -o "[0-9][0-9]*\.[0-9]*[0-9]" >> results3_1
done
