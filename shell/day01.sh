#! /bin/zsh
echo "--- Day 1: Chronal Calibration ---"

# The input file name is the first and only arg
if [[ -z $1 ]] then
	echo "Need to supply the input file as the first argument."
	exit 1
fi
filename=$1

# Clean up any temp files
rm temp*

f_spaced=temp.spc.txt
f_sqlite=temp.sqlite
f_select=temp.sel.sql
f_insert=temp.ins.sql

# Pre-process the input file in sed to break up +/- and the number
sed 's/\([\+\-]\)/\1 /' $filename > $f_spaced

freq=0
iter=0
duplicate=""

# Using SQLite as a way to store frequencies for fast lookup.
# Originally used plain text files and sed, but grew slow as file size increased
# Putting an index on the column made it fast.
sqlite3 $f_sqlite "create table freq(f int); create unique index idx_f on freq(f);"

# Will exit as soon as select returns a record
while true; do
	echo $iter
	echo "select * from freq where f in (" > $f_select
	echo "begin transaction;" > $f_insert
	
	values=() # the frequency offsets in a loop iteration
	ordercases="" # SQLite hack for ordering the results
	count=1
	
	while read line || [ -n "$line" ]; do
		a=( ${(s. .)line} )
		if [[ $a[1] == "+" ]]
		then
		(( freq = $freq + $a[2] ))
		else
		(( freq = $freq - $a[2] ))
		fi
		
		values+=($freq)
		echo "insert into freq values($freq);" >> $f_insert
		
		# Forces the select results to return in the order of the IN clause
		ordercases="$ordercases when $freq then $count"
		(( count = $count + 1 ))
	done < $f_spaced
	
	# Finish the select statement
	joinedvalues=${(j:,:)values}
	echo "$joinedvalues) order by case freq.f $ordercases end limit 1;" >> $f_select
	
	# Finish the insert statement
	echo "commit;" >> $f_insert
	
	# See if $freq is in $f_sqlite freq.f
	duplicate=$(sqlite3 $f_sqlite ".read $f_select")
	
	# If length of $duplicate is non-zero
	if [ ! -z $duplicate ]
	then
		echo "Part 2: the first repeated offset is $duplicate"
		# Clean up any temp files
		rm temp*
		exit 0
	fi
	
	# Insert this iteration's set of frequency offsets
	# Doing this as a batch was the biggest speed boost.
	sqlite3 $f_sqlite ".read $f_insert"
	
	if [[ $iter == 0 ]] then
		echo "Part 1: the offset is $freq"
	fi
	
	(( iter = $iter + 1 ))
done
