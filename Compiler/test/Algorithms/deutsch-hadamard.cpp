
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf hadamard (int n )
{
	int i;
	MatrixXcf gate;
 
	gate = H;

    for (int i = 0; i < n - 1; i = i + 1){
        
	{
	gate = tensor(gate, H);

	}

        }
	return gate;
}
int main ()
{
	MatrixXcf top;
	MatrixXcf Ub;
	MatrixXcf Uc;
	int n;
	MatrixXcf outcome;
 
		n = 2;
	outcome = hadamard(n);

	std::cout << outcome << endl;

	return 0;
}