def func_test(mat z) : float ret_name { 
	float b;
        b = norm(z);
	print(b);
	ret_name = b;
}
def compute():int trial {
        mat m;
        m = [(1,9,9)(4,5,5)];
	
	func_test(m);

	trial = 8;
}
