#!/bin/sh

files="SemanticSuccess/*.ql"

AST=0
SAST=0
GEN=0
COMP=0
EXEC=0

if [ $1 == "a" ]
then
AST=1
fi
if [ $1 == "s" ]
then
SAST=1
fi
if [ $1 == "g" ] || [ $1 == "c" ] || [ $1 == "e" ]
then
GEN=1
fi
if [ $1 == "c" ] || [ $1 == "e" ]
then
COMP=1
fi
if [ $1 == "e" ]
then
EXEC=1
fi

ASTCheck()
{
    eval "../qlc -a $1" 1>> ast_output 2>> ast_error_log
    wc ast_error_log | awk '{print $1}'
}

SASTCheck()
{
    eval "../qlc -s $1" 1>> sast_output 2>> sast_error_log
    wc sast_error_log | awk '{print $1}'
}

GenerationCheck()
{
    eval "../qlc -g $1" 2>> gen_error_log
    wc gen_error_log | awk '{print $1}'
}

CompilationCheck()
{
    eval "g++ -w -o out $1 ../cpp/qlang.cpp" 2>> comp_error_log
    wc comp_error_log | awk '{print $1}'
}

ExecutionCheck()
{
    eval "./out" 1>> exec_output
}

#Check AST
if [ $AST == 1 ]
then
rm -f ast_error_log ast_output
for file in $files
do
errors=0
errors=$(ASTCheck $file)
if [ "$errors" -eq 0 ]
then
count=1
echo "Test " $count ": " $file " passed."
else
echo "Test " $count ": " $file " failed to pass."
fi
count=$((count+1))
done
fi

#Check SAST
if [ $SAST == 1 ]
then
rm -f sast_error_log sast_output
for file in $files
do
errors=0
errors=$(SASTCheck $file)
if [ "$errors" -eq 0 ]
then
count=1
echo "Test " $count ": " $file " passed."
else
echo "Test " $count ": " $file " failed to pass."
fi
count=$((count+1))
done
fi

#Check Generation
if [ $GEN == 1 ]
then
rm -f gen_error_log
for file in $files
do
errors=0
errors=$(GenerationCheck $file)
if [ "$errors" -eq 0 ]
then
echo "Test: " $file " generated code successfully."
else
echo $file "could not generate code."
fi
done
fi

#Check Compilation
if [ $COMP == 1 ]
then
rm -f comp_error_log
if [ $EXEC == 1 ]
then
rm -f exec_outputs
fi
files="SemanticSuccess/*.cpp"
for file in $files
do
errors=$(CompilationCheck $file)
if [ "$errors" -eq 0 ]
then
echo $file "executable generated."
if [ $EXEC == 1 ]
then
ExecutionCheck
fi
fi
done
fi
