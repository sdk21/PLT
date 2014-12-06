def func_test(comp z) : comp ret_name { 
	comp a;
	comp b;
	comp c;
	comp d;

	a = C(1.1);
	b = C(1.2I);
	c = C(1.1 + 1.2I);

	d = z;
	
	ret_name = c * d;
}
def execute(int a):int try {
	comp b;

	b = C(3.2 + 1.I);

	func_test(b);
}
