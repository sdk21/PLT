
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
	MatrixXcf z;
	MatrixXcf y;
	MatrixXcf w;
	MatrixXcf ret_val;
 
	x = X;
	z = H;
	y = Y;
	w = IDT;
	cout << x << endl << endl;
	cout << z << endl << endl;
	cout << y << endl << endl;
	cout << w << endl << endl;
	ret_val = x * z * y * w;

	return ret_val;
}
int main ()
{
	MatrixXcf ret_val;
 
		ret_val = test_func(0);

	std::cout << ret_val << endl;

	return 0;
}