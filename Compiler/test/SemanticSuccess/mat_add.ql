def test_func(comp a, comp b, comp c, comp d) : mat ret_val { 

	mat x;

	x = [(a,b)(c,d)];

	ret_val = x;
}

def compute():mat trial {
	comp a;
	comp b;
	comp c;
	comp d;
        mat k;

	a = C(2.);
	b = C(2.);
	c = C(2.);
	d = C(2.);

	trial = test_func(a, b, c, d)+test_func(a,b,c,d);

}



