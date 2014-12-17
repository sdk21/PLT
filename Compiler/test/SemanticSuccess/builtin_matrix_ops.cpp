
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
int main ()
{
	int num_rows;
	int num_cols;
	complex<float> val;
	MatrixXcf m;
	complex<float> trial;
 
		m = (Matrix<complex<float>, Dynamic, Dynamic>(3,3)<<1,2,3,4,5,6,7,8,9).finished();
	num_rows = m.rows();
	num_cols = m.cols();
	val = m(1,2);
	cout << num_rows << endl << endl;
	cout << num_cols << endl << endl;
	trial = val;

	std::cout << trial << endl;

	return 0;
}