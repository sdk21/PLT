
        #include <Eigen/Dense>
        #include <cmath>
        #include <complex>
        #include <iostream>
        #include "../../cpp/qlang.h"
        using namespace Eigen;
        using namespace std;
        
MatrixXcf test_func (int a,int b,int c,int d )
{
	MatrixXcf mat1;
	MatrixXcf ret_val;
 
	mat1 = (Matrix<complex<float>, Dynamic, Dynamic>(2,2)<<a,b,c,d).finished();
	ret_val = mat1;
	
	return ret_val;
}
int main ()
{
	int a;
	int b;
	int c;
	int d;
	MatrixXcf ret_val;
 
	a = 4;
	b = 7;
	c = 2;
	d = 9;
	ret_val = test_func(a,b,c,d);
	
	std::cout << ret_val << endl;

	return 0;
}