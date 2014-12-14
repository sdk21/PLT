def func_test(float b) : float ret_name { 
        
        float a; 
	float c;

        a = 5.0;
	c = a * b;

        ret_name = c;  
}

def compute() : float trial {

	trial = func_test(3.7);

}
