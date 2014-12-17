def func_test(comp z) : comp ret_name { 
        comp a;
        a = C(4.5I);
        z = -a;
        ret_name = z ;  
}
def compute(): comp trial {
	trial = func_test(C(3.4I));
}