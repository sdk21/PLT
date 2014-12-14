def test_func(int a) : mat ret_val { 

	mat x;
	mat y;

	x = <01|;
	y = <10|;

	ret_val = x + y;
}

def compute():mat trial {

	comp a;
	comp b;
	comp c;
	comp d;

	a = C(2.);
	b = C(2.);
	c = C(2.);
	d = C(2.);

	trial = test_func(0);

}





