
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
	b = 10;
	d = a + b * a + b / a - b;
	ret_name = d;

	return ret_name;
}
int main ()
{
	int trial;
 
		trial = func_test(34);

	std::cout << trial << endl;

	return 0;
}