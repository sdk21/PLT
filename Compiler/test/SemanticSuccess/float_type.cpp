
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
float func_test (float b )
{
	float a;
	float c;
	float ret_name;
 
	a = 5.;
	c = a * b;
	ret_name = c;

	return ret_name;
}
int main ()
{
	float trial;
 
		trial = func_test(3.7);

	std::cout << trial << endl;

	return 0;
}