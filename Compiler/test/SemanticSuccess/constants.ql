def test_func(int a) : mat ret_val { 

	mat x;
	mat z;
	mat y;
	mat w;
	
	x = X;
	z = H;
	y = Y;
	w = IDT;

	print(x);
	print(z);
	print(y);
	print(w);

	ret_val = x * z * y * w;
}

def compute() : mat ret_val {

	ret_val = test_func(0);
}
