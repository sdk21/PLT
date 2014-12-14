def func_test(mat a, mat b) : mat ret_name { 
        
        ret_name = a*b;

}


def compute(int a):mat trial {
	
	 mat zero;
	 mat one;

	 zero = |0>;
	 one = |1>;

     trial = func_test(H,zero);
     printq(trial);

     trial = func_test(H,one);
     printq(trial);

}