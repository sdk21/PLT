#! /bin/bash
function test {
	echo $2
	echo $2 | ./qlc -$1;
	echo
	echo ---------------
}

# Ast Tests
function ast_test {
	test a $1
}

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
ast_test 'id'
ast_test 'id '
ast_test '-1'
ast_test 'Not(1)'
ast_test 'Re(1)'