
        #include <Eigen/Dense>
        #include <cmath>
        #include <complex>
        #include <iostream>
        #include "../../cpp/qlang.hpp"
        using namespace Eigen;
        using namespace std;
        
MatrixXcf func_test (MatrixXcf z )
{
	MatrixXcf a;
	MatrixXcf ret_name;
 
		a = genQubit("00",0);
	a = z;
	ret_name = a;

	return ret_name;
}
int main ()
{
	MatrixXcf a;
	MatrixXcf tr;
 
		a = genQubit("001",0);
	tr = func_test(a);

	std::cout << tr << endl;

	return 0;
}