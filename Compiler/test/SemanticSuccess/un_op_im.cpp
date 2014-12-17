
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
	complex<float> b;
	complex<float> ret_name;
 
	a = z;
	b =   imag(a);
	cout << b << endl << endl;
	ret_name = a;

	return ret_name;
}
int main ()
{
	complex<float> trial;
 
		trial = func_test( complex<float>(0.,4.5) );

	std::cout << trial << endl;

	return 0;
}