
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf measure (MatrixXcf top )
{
	MatrixXcf ad;
	MatrixXcf result;
 
	ad =   top.adjoint();
	result = top * ad;

	return result;
}
int main ()
{
	MatrixXcf top;
	MatrixXcf Ub;
	MatrixXcf Uc;
	int n;
	MatrixXcf outcome;
 
		top = genQubit("00",0);
	outcome = measure(top);

	std::cout << outcome << endl;

	return 0;
}