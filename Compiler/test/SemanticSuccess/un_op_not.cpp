
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
 
	a =   !(z);
	ret_name = a;

	return ret_name;
}
int main ()
{
	int trial;
 
		trial =   !(func_test(20));

	std::cout << trial << endl;

	return 0;
}