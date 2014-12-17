
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf func_test (MatrixXcf z )
{
	MatrixXcf a;
	MatrixXcf b;
	MatrixXcf ret_name;
 
	a = (Matrix<complex<float>, Dynamic, Dynamic>(2,3)<<1,9,9,4,5,5).finished();
	b =   a.transpose();

	return ret_name;
}
int main ()
{
	int trial;
 
		trial = 8;

	std::cout << trial << endl;

	return 0;
}