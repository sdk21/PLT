
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf test_func ()
{
	MatrixXcf x;
	MatrixXcf ret_val;
 
	x = (Matrix<complex<float>, Dynamic, Dynamic>(2,2)<<1,2,3,4).finished();
	ret_val = x;

	return ret_val;
}
int main ()
{
	MatrixXcf ret_val;
 
		ret_val = test_func();

	std::cout << ret_val << endl;

	return 0;
}