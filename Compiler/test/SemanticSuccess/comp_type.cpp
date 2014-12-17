
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
complex<float> func_test (complex<float> val1,complex<float> val2 )
{
	complex<float> val3;
	complex<float> ret_name;
 
	val3 =  complex<float>(1.1,0.) ;
	ret_name = val1 + val2 * val3;

	return ret_name;
}
int main ()
{
	complex<float> comp1;
	complex<float> comp2;
	complex<float> ret_name;
 
	 if(1)
	{
	1;

	2 + 3;

	}
	else
	{
	3 + 6;

	}
	comp1 =  complex<float>(0.,7.5) ;
	comp2 =  complex<float>(3.2,1.) ;
	ret_name = func_test(comp1,comp2);

	std::cout << ret_name << endl;

	return 0;
}