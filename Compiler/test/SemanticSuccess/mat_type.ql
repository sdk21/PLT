def test_func(int a, int b, int c, int d) : mati ret_val { 

	mati mat1;

	mat1 = [(a,b)(c,d)];

	ret_val = mat1;
}

def execute() : mati ret_val {

	int a;
	int b;
	int c;
	int d;

	a = 4;
	b = 7;
	c = 2;
	d = 9;

	ret_val = test_func(a, b, c, d);
}
