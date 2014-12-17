
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
complex<float> func_test (complex<float> z )
{
	complex<float> a;
	complex<float> ret_name;
 
	a =  complex<float>(0.,4.5) ;
	z =   -a;
	ret_name = z;

	return ret_name;
}
int main ()
{
	complex<float> trial;
 
		trial = func_test( complex<float>(0.,3.4) );

	std::cout << trial << endl;

	return 0;
}