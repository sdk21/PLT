
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf func_test (MatrixXcf a,MatrixXcf b )
{
	MatrixXcf ret_name;
 
	ret_name = a * b;

	return ret_name;
}
int main ()
{
	MatrixXcf zero;
	MatrixXcf one;
	MatrixXcf trial;
 
		zero = genQubit("0",0);
	one = genQubit("1",0);
	trial = func_test(H,zero);
	cout << vectorToBraket(trial) << endl << endl;
	trial = func_test(H,one);
	cout << vectorToBraket(trial) << endl << endl;

	std::cout << trial << endl;

	return 0;
}