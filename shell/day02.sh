#! /bin/zsh
echo "--- Day 2: Inventory Management System ---"

# The input file name is the first and only arg
if [[ -z $1 ]] then
	echo "Need to supply the input file as the first argument."
	exit 1
fi
filename=$1

# Clean up any temp files
#rm temp*

letters=('a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z')

twos=0
threes=0

while read line || [ -n "$line" ]; do

	is_two=false
	is_three=false
	
	for letter in $letters
	do
		# The number of $letter's in $line is $count
		pattern="[^$letter]"
		subbed=${line//$~pattern/}
		count=`echo -n $subbed | wc -m` 
		
		if [[ $count -eq 2 ]]
		then
			is_two=true
		fi
		
		if [[ $count -eq 3 ]]
		then
			is_three=true
		fi
	done
	
	if [ $is_two = true ]
	then
		(( twos = $twos + 1 ))
	fi
	if [ $is_three = true ]
	then
		(( threes = $threes + 1 ))
	fi

done < $filename

echo "The number of twos is $twos and the number of threes is $threes"
(( checksum = $twos * $threes ))
echo "Part 1: the checksum is $checksum"