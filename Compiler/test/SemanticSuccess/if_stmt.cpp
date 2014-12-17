
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
 
	a = 10;
 if(z == 5)	a = 0;

	a = a - 2;
 if(z <= 5)
	{
	a = 0;

	}
	else
	{
	a = 10;

	b = 24;

	}
 if(a > 100)
	{
	cout << b << endl << endl;

	}
	else
	{
	cout << a << endl << endl;

	}
	ret_name = 8;

	return ret_name;
}
int main ()
{
	int trial;
 
		trial = func_test(20);

	std::cout << trial << endl;

	return 0;
}