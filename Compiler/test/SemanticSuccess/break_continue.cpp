
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
int func_test (int a )
{
	int i;
	int ret_name;
 

    for (int i = 0; i < 2; i = i + 1){
        	a = a + 5;

        }
    for (int i = 2; i < 0; i = i +   -1){
        
	{
	a = a * 10;

	cout << a << endl << endl;

break;
	}

        }
    for (int i = 1; i < 5; i = i + 1){
        
	{
	cout << a << endl << endl;

continue;
	a = a * 10;

	}

        }	ret_name = a;

	return ret_name;
}
int main ()
{
	int trial;
 
		trial = func_test(20);

	std::cout << trial << endl;

	return 0;
}