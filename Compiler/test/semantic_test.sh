#!/bin/sh


SCAN="../qlc -s" 

#files="SemanticFailures/*.ql"
files="SemanticSuccess/*.ql"


SemanticCheck() {
eval "$SCAN $1" 2> test.out 1> test.null
wc test.out | awk '{print $1}' 
}


for file in $files
do
errors=$(SemanticCheck $file)
SemanticCheck $file
if [ $errors == 0 ]
then
echo "Test: " $file " passed."
else
echo $file " failed to pass."
#exit 1
fi
done
