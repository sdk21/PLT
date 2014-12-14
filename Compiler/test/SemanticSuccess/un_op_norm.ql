def func_test(mat z) : comp ret_name { 
        mat a;
	mat m;
        comp b;
        a = [(1,9,9)(4,5,5)];
        b = norm(a);
	b = norm(z);
	print(b);
	ret_name = b;
}
def compute(int a):int trial {
	mat m;

	m = IDT;
	
	func_test(m);

}
