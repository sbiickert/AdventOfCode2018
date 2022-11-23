#! /bin/zsh
echo "--- Day 1: Chronal Calibration ---"
echo "Running $0"

# The input file name is the first and only arg
filename=$1

# Pre-process the file in sed to break up +/- and the number
temp01=temp.01.txt
sed 's/\([\+\-]\)/\1 /' $filename > $temp01

freq=0

while read line || [ -n "$line" ]; do
a=( ${(s. .)line} )
if [[ $a[1] == "+" ]]
then
(( freq = $freq + $a[2] ))
else
(( freq = $freq - $a[2] ))
fi
#echo $freq
done < $temp01

echo "Part 1: the result is $freq"

rm $temp01
