def func_test(comp val1, comp val2) : comp ret_name { 

	comp val3;

	val3 = C(1.1);
	
	ret_name = val1 + val2 * val3;
}
def compute() : comp  ret_name {

	comp comp1;
	comp comp2;

	if (1) {1; 2+3;} else {3+6;}

	comp1 = C(7.5I);
	comp2 = C(3.2 + 1.I);

	ret_name = func_test(comp1, comp2);
}
