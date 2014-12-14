def func_test(comp z) : comp ret_name { 
        comp a;
        comp b;
        a = z;
        b = im(a);
        print(b);
        ret_name = a;  
}

def compute(int a): comp trial {

	trial = func_test(C(4.5I));
}
