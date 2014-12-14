#!/bin/bash

AST=0
SAST=0
GEN=0
COMP=0
EXEC=0

if [ $1 == "clean" ]
then
rm -f ast_error_log sast_error_log gen_error_log comp_error_log ast_log sast_log ast_output sast_output exec_output
else

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
if [ $1 == "c" ]
then
COMP=1
fi
if [ $1 == "e" ]
then
EXEC=1
fi

if [ $2 == "ss" ]
then
files="SemanticSuccess/*.ql"
cfiles="SemanticSuccess/*.cpp"
elif [ $2 = "sf" ]
then
files="SemanticFailures/*.ql"
cfiles="SemanticFailures/*.cpp"
elif [ $2 = "al" ]
then
files="SemanticFailures/*.ql"
cfiles="SemanticFailures/*.cpp"
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
    eval "g++ -w -o out $1 -I../../includes/headers -L../../includes/libs -lqlang" 2>> comp_error_log
    wc comp_error_log | awk '{print $1}'
}

ExecutionCheck()
{
    output=$(eval "./out")
    echo "$output" >> exec_output
    echo "$output"
}

#Check AST
if [ $AST == 1 ]
then
echo "* AST Generation *"
rm -f ast_error_log ast_output
errors=0
prev_errors=0
for file in $files
do
errors=0
errors=$(ASTCheck $file)
if [ "$errors" -le "$prev_errors" ]
then
count=1
echo "Pass " $file
else
echo "Fail " $file
fi
prev_errors=$errors
done
echo ""
fi

#Check SAST
if [ $SAST == 1 ]
then
echo "* SAST Generation *"
rm -f sast_error_log sast_output
errors=0
prev_errors=0
for file in $files
do
errors=$(SASTCheck $file)
if [ "$errors" -le "$prev_errors" ]
then
echo "Pass: " $file
else
echo "Fail: " $file
fi
prev_errors=$errors
done
echo ""
fi

#Check Generation
if [ $GEN == 1 ]
then
echo "* Code Generation *"
rm -f gen_error_log
errors=0
prev_errors=0
for file in $files
do
errors=$(GenerationCheck $file)
if [ "$errors" -le "$prev_errors" ]
then
echo "Pass: " $file
else
echo "Fail: " $file
fi
prev_errors=$errors
done
echo ""
fi

#Check Compilation
if [ $COMP == 1 ]
then
echo "* Compilation *"
rm -f comp_error_log
errors=0
prev_errors=0
for file in $cfiles
do
errors=$(CompilationCheck $file)
if [ "$errors" -le "$prev_errors" ]
then
echo "Pass: " $file
else
echo "Fail: " $file
fi
prev_errors=$errors
done
echo ""
fi

# Execution check
if [ $EXEC == 1 ]
then
echo "* Compilation and Execution *"
rm -f comp_error_log exec_output
errors=0
prev_errors=0
exec_output=0
for file in $cfiles
do
errors=$(CompilationCheck $file)
if [ "$errors" -le "$prev_errors" ]
then
echo "Pass (compilation): " $file
exec_output=$(ExecutionCheck)
if [ "$exec_output" != "0" ]
then
echo "Pass (execution): " $file
echo $exec_output
else
echo "Fail (execution): " $file
fi
else
echo "Fail (compilation): " $file
fi
prev_errors=$errors
done
fi

fi
