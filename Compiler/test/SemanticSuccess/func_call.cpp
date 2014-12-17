
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
int func_test (int z )
{
	int a;
	int b;
	int d;
	int ret_name;
 
	a = z;
	ret_name = z;

	return ret_name;
}
int main ()
{
	int trial;
 
		trial = func_test(4);

	std::cout << trial << endl;

	return 0;
}