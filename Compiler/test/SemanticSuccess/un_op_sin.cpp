
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
	int ret_name;
 
	a = 90;
	b =   sin((double)a);
	ret_name = b;

	return ret_name;
}
int main ()
{
	int trial;
 
		trial = 8;

	std::cout << trial << endl;

	return 0;
}