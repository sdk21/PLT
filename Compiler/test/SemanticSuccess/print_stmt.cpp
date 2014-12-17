
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
	int ret_name;
 
	a = 5;
	a = z;
	ret_name = a;

	return ret_name;
}
int main ()
{
	int a;
	int trial;
 
		a = 5;
	cout << a << endl;
	trial = 8;

	std::cout << trial << endl;

	return 0;
}