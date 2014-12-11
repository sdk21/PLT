#!/bin/sh

SCAN="../qlc -s" 

#files="SemanticFailures/*.ql"
files="SemanticSuccess/*.ql"
#files="SemanticSuccess/binop_int_arith.ql"
count=1



SemanticCheck()
{
    eval "$SCAN $1" 1>> test.null 2>> test.out 
    wc test.out | awk '{print $1}' 
}


rm -f test.out test.null

for file in $files
do
errors=0
errors=$(SemanticCheck $file)
if [ $errors == 0 ]
then
echo "Test " $count ": " $file " passed."
else
echo "Test " $count ": " $file " failed to pass."
fi
count=$((count+1))
done

rm -f *.cpp

