
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf test_func (complex<float> a,complex<float> b,complex<float> c,complex<float> d )
{
	MatrixXcf x;
	MatrixXcf ret_val;
 
	x = (Matrix<complex<float>, Dynamic, Dynamic>(2,2)<<a,b,c,d).finished();
	ret_val = (Matrix<complex<float>, Dynamic, Dynamic>(2,2)<<a,c,d,b).finished();
	ret_val = ret_val * x;
	ret_val = ret_val + x;
	ret_val = ret_val - x;
	ret_val = ret_val / 2;

	return ret_val;
}
int main ()
{
	complex<float> a;
	complex<float> b;
	complex<float> c;
	complex<float> d;
	MatrixXcf k;
	MatrixXcf ret_val;
 
		a =  complex<float>(4.,5.) ;
	b =  complex<float>(6.,6.) ;
	c =  complex<float>(7.,8.) ;
	d =  complex<float>(9.,10.) ;
	ret_val = test_func(a,b,c,d);

	std::cout << ret_val << endl;

	return 0;
}