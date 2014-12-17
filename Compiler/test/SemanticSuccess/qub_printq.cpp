
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf test_func (int a )
{
	MatrixXcf x;
	MatrixXcf y;
	MatrixXcf ret_val;
 
	x = genQubit("01",1);
	y = genQubit("10",1);
	ret_val = x + y;

	return ret_val;
}
int main ()
{
	complex<float> a;
	complex<float> b;
	complex<float> c;
	complex<float> d;
	MatrixXcf trial;
 
		a =  complex<float>(2.,0.) ;
	b =  complex<float>(2.,0.) ;
	c =  complex<float>(2.,0.) ;
	d =  complex<float>(2.,0.) ;
	cout << vectorToBraket(test_func(0)) << endl << endl;
	trial = test_func(0);

	std::cout << trial << endl;

	return 0;
}