
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
float func_test (MatrixXcf z )
{
	float b;
	float ret_name;
 
	b =   z.norm();
	ret_name = b;

	return ret_name;
}
int main ()
{
	MatrixXcf m;
	int trial;
 
		m = (Matrix<complex<float>, Dynamic, Dynamic>(2,3)<<1,9,9,4,5,5).finished();
	func_test(m);
	trial = 8;

	std::cout << trial << endl;

	return 0;
}