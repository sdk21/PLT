
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
int func_test (int z )
{
	int i;
	int a;
	int ret_name;
 

    for (int i = 0; i < 2; i = i + 1){
        	a = a + 5;

        }
    for (int i = 2; i < 0; i = i +   -1){
        
	{
	a = a * 10;

	cout << a << endl << endl;

	}

        }
    for (int i = 1; i < 10; i = i + 1){
        
	{
	a = a - 3;

	}

        }
    for (int i = 1; i < 100; i = i + 1){
        
	{
	cout << a * 100 << endl << endl;

	}

        }	ret_name = 5;

	return ret_name;
}
int main ()
{
	int trial;
 
		trial = func_test(20);

	std::cout << trial << endl;

	return 0;
}