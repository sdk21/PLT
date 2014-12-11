#!/bin/sh

SCAN="../qlc -s" 

files="SemanticFailures/*.ql"
#files="SemanticSuccess/*.ql"


SemanticCheck()
{
    eval "$SCAN $1" 1>> test.null 2>> test.out 
    wc test.out | awk '{print $1}' 
}


rm -f test.out test.null

for file in $files
do
errors=$(SemanticCheck $file)
if [ $errors == 0 ]
then
echo "Test: " $file " passed."
else
echo $file " failed to pass."
fi
done

