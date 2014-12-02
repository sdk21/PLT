#!/bin/sh

# REPLACE THIS WITH OUR COMMAND
SCAN="../qlc -s" 


files="test1.ql"


SemanticCheck() {
eval "$SCAN $1" 2> test.out 1> test.null
wc l
test.out | awk '{print $1}'
}


for file in $files
do
errors=$(SemanticCheck $file)
SemanticCheck $file
if [ $errors eq 0 ]
then
echo "Test: " $file " passed."
else
echo $file " failed to pass."
exit 1
fi
done
