#! /bin/zsh
echo "--- Day 1: Chronal Calibration ---"
echo "Running $0"

# The input file name is the first and only arg
filename=$1

f_spaced=temp.spc.txt
f_dup=temp.dup.txt
f_unique=temp.uni.txt

# Clean up any temp files
rm temp*

# Pre-process the file in sed to break up +/- and the number
sed 's/\([\+\-]\)/\1 /' $filename > $f_spaced

freq=0
iter=0

# create zero-length files
touch $f_dup
touch $f_unique

# While the file $f_dup is zero length
while [ ! -s $f_dup ]; do
	echo $iter
	while read line || [ -n "$line" ]; do
		a=( ${(s. .)line} )
		if [[ $a[1] == "+" ]]
		then
		(( freq = $freq + $a[2] ))
		else
		(( freq = $freq - $a[2] ))
		fi
		
		# See if $freq is in $f_unique
		sed -n "/^$freq$/p" $f_unique > $f_dup
		if [ -s $f_dup ]
		then
			echo "Part 2: the result is $freq"
			break
		fi
		echo $freq >> $f_unique
	done < $f_spaced
	
	if [[ $iter == 0 ]] then
		echo "Part 1: the result is $freq"
	fi
	
	(( iter = $iter + 1 ))
done

#rm $f_spaced $f_dup $f_unique
