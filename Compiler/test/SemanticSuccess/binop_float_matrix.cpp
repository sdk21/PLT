
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf test_func (float a,float b,float c,float d )
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
	float a;
	float b;
	float c;
	float d;
	MatrixXcf ret_val;
 
		a = 3.4;
	b = 6.;
	c = 5.6;
	d = 100.;
	ret_val = test_func(a,b,c,d);

	std::cout << ret_val << endl;

	return 0;
}