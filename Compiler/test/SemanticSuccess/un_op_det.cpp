
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
	complex<float> b;
	MatrixXcf ret_name;
 
	a = (Matrix<complex<float>, Dynamic, Dynamic>(2,2)<<1,9,4,5).finished();
	b =   a.determinant();
	ret_name = a;

	return ret_name;
}
int main ()
{
	MatrixXcf x;
	MatrixXcf trial;
 
		x = (Matrix<complex<float>, Dynamic, Dynamic>(2,2)<<1,2,3,4).finished();
	trial = func_test(x);

	std::cout << trial << endl;

	return 0;
}