def test_func(float a, float b, float c, float d) : mat ret_val { 

	mat x;

	x = [(a,b)(c,d)];

	ret_val = [(a,c)(d,b)];

	ret_val = ret_val * x;
	ret_val = ret_val + x;
	ret_val = ret_val - x;
	ret_val = ret_val / 2;
	
}

def compute() : mat ret_val {


	float a;
	float b;
	float c;
	float d;

	a = 3.4;
	b = 6.;
	c = 5.6;
	d = 100.0;

	ret_val = test_func(a, b, c, d);
}