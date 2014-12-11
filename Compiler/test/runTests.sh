#!/bin/sh

GEN="../qlc -g" 

#files="SemanticFailures/*.ql"
files="SemanticSuccess/binop_int_arith.ql"


GenerationCheck() {
eval "$GEN $1" 1>> test.null 2>> test.out 
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

#Add later : 
#g++ -o out $file ../cpp/qlang.cpp
#rm -f *.cpp
