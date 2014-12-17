
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
	ret_val = x;

	return ret_val;
}
int main ()
{
	complex<float> a;
	complex<float> b;
	complex<float> c;
	complex<float> d;
	MatrixXcf k;
	MatrixXcf trial;
 
		a =  complex<float>(2.,0.) ;
	b =  complex<float>(2.,0.) ;
	c =  complex<float>(2.,0.) ;
	d =  complex<float>(2.,0.) ;
	trial = test_func(a,b,c,d) - test_func(a,b,c,d);

	std::cout << trial << endl;

	return 0;
}