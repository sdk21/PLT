#!/bin/sh

GEN="../qlc -g" 

#files="SemanticFailures/*.ql"
files="SemanticSuccess/*.ql"


GenerationCheck() {
eval "$GEN $1" 2>> test.out 
wc test.out | awk '{print $1}'
}


rm -f test.out test.null

for file in $files
do
errors=$(GenerationCheck $file)
if [ $errors == 0 ]
then
echo "Test: " $file " passed."
else
echo $file "could not generate code."
fi
done

#Add these to step2, among other statemnets later : 
#g++ -o out $file ../cpp/qlang.cpp
#echo "Executable generated."
#rm -f *.cpp
