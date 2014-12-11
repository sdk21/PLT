#!/bin/sh

GEN="../qlc -g" 

#files="SemanticFailures/*.ql"
files="SemanticSuccess/*.ql"


GenerationCheck() {
	eval "$GEN $1" 2>> test.out 
	wc test.out | awk '{print $1}'
}

CompilationCheck() {
	eval "g++ -w -o out $1 ../cpp/qlang.cpp" 2>> test.out 
	wc test.out | awk '{print $1}'
}


rm -f test.out test.null

#Step 1
for file in $files
do
errors=$(GenerationCheck $file)
if [ $errors -eq 0 ]
then
echo "Test: " $file " generated code successfully."
else
echo $file "could not generate code."
fi
done

echo "Generating executables now"

#Step 2
files="SemanticSuccess/*.cpp"
for file in $files
do
errors=$(CompilationCheck $file)
if [ $errors -eq 0 ]
then
echo $file "executable generated."
else
echo $file "could not generate executable."
errors=0
fi
done

rm -f SemanticSuccess/*.cpp
