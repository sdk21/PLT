#! /bin/bash
function test {
	echo "$2"
	echo "$2" | ./qlc -$1;
	echo
	echo ---------------
}

# Ast Tests
function ast_test {
	test a "$1"
}

# Expressions
<<COMMENT
ast_test '1'
ast_test '1.'
ast_test '1.0'
ast_test 'c(1.)'
ast_test 'c(1.2i)'
ast_test 'c(1.+1.2i)'
ast_test '<1|'
ast_test '<1010|'
ast_test '|1>'
ast_test '|1010>'
ast_test '[(1)]'
ast_test '[(1,2,3)]'
ast_test '[(1,2,3)]'
ast_test '[(1,2,3)(1)(1,2,3)]'
ast_test '[(1.2,2.3,3.)(1.7)(1.0,2.,3.1)]'
ast_test 'id '
ast_test '-1'
ast_test '-1.32'
ast_test '-c(1.+1.2i)' #issue
ast_test 'Not(1)'
ast_test 'Not(1.)'
ast_test 'Re(c(1.+1.2i))'
ast_test 'Im(c(1.+1.2i))'
ast_test 'Norm([(1,2,3)(1)(1,2,3)])'
ast_test 'Norm(c(1.+1.2i))'
ast_test 'Norm(<010|)'
ast_test 'Norm(|010>)'
ast_test 'Trans([(1,2,3)(1)(1,2,3)])'
ast_test 'Trans(|010>)'
ast_test 'Det([(1,2,3)(1)(1,2,3)])'
ast_test 'Det(|010>)'
ast_test 'Adj([(1,2,3)(1)(1,2,3)])'
ast_test 'Adj(|010>)'
ast_test 'Conj([(1,2,3)(1)(1,2,3)])'
ast_test 'Conj(|010>)'
ast_test 'Conj(c(1.+1.2i))'
ast_test 'Unit([(1,2,3)(1)(1,2,3)])'
ast_test 'Unit(|010>)'
ast_test 'Sin(1)'
ast_test 'Sin(1.2)'
ast_test 'Sin(c(1.+1.2i))'
ast_test 'Cos(1)'
ast_test 'Cos(1.2)'
ast_test 'Cos(c(1.+1.2i))'
ast_test 'Tan(1)'
ast_test 'Tan(1.2)'
ast_test 'Tan(c(1.+1.2i))'

ast_test '1 + 2'
ast_test '1 - 2'
ast_test '1 * 2'
ast_test '1 / 2'
ast_test '1 % 2'
ast_test '1^2'

# Statements
ast_test '1 + 2;'
ast_test '{1 + 2;}'
ast_test '{1 + 2;3 * 4;}'
ast_test 'if(7 gt 2){3 * 4;}'
ast_test 'for(j from 1 to 10){1 + 2;3 * 4;}'
ast_test 'for(j from 1 to 10 by 1){1 + 2;3 * 4;}'

# Statement Lists
ast_test '1 + 2;3 * 4;172 - 12;'

# Programs
ast_test 'def func_test(int a) : int ret_name { 1 + 2; 3*4; }'
ast_test 'def func_test(int a, int b) : int ret_name { 1 + 2; 3*4; }'
ast_test 'def func_test(int a, int b) : int ret_name
			{
				int test_int;
				1 + 2;
				3*4;
			}'
ast_test 'def func_test(int a, int b) : int ret_name
			{
				int test_int;
				1 + 2;
				3*4;
			}
			def new_test(qub q, mat m) : int ret_name
			{
				float test_float;
				comp test_comp;
				1 + 2;
				3*4;
			}'
COMMENT
			
# Sast Tests
function sast_test {
	test s "$1"
}

# Programs
sast_test 'def func_test(int a, int b) : int ret_name_test
			{
				int test_int;
				float test_float;

				1 + 2;
				3*4;
			}
			def func_test2(int a, int b) : int ret_name_test
			{
				int test_int;
				float test_float;
				qubb qb;
				qubk qk;

				<010|;
				|0011>;
				test_int = 1 + 2;
				test_int + 1;
				3*4;
			}'
