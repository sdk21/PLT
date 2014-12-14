def func_test(mat z) : mat ret_name { 
        mat a;
        comp b;
        a = [(1,9)(4,5)];
        b = det(a);
        ret_name = a;
}
def compute(int a):mat trial {
	mat x;
  x = [(1,2)(3,4)];
  trial = func_test(x);
}
