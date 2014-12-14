def test_func() : mat ret_val { 

        mat x;

	x = [(1,2)(3,4)];

	ret_val = x;
}

def compute() : mat ret_val {

	ret_val = test_func();
}
