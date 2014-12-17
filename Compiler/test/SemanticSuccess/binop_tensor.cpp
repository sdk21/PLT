
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
int main ()
{
	MatrixXcf a;
	MatrixXcf b;
	MatrixXcf c;
	MatrixXcf out;
 
		a = (Matrix<complex<float>, Dynamic, Dynamic>(2,1)<<1,0).finished();
	b = (Matrix<complex<float>, Dynamic, Dynamic>(2,1)<<0,1).finished();
	c = tensor(a, b);
	cout << c << endl;

	std::cout << out << endl;

	return 0;
}