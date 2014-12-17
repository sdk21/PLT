
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
MatrixXcf measure (MatrixXcf top )
{
	MatrixXcf ad;
	MatrixXcf outcome;
 
	ad =   top.adjoint();
	outcome = top * ad;

	return outcome;
}
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
MatrixXcf topqubit (int n )
{
	int i;
	MatrixXcf input;
 
	input = genQubit("0",0);

    for (int i = 0; i < n - 1; i = i + 1){
        
	{
	input = tensor(input, genQubit("0",0));

	}

        }
	return input;
}
float deutsch (int n,MatrixXcf U )
{
	MatrixXcf bottom;
	MatrixXcf top;
	MatrixXcf input;
	MatrixXcf hadtop;
	MatrixXcf meas;
	float outcomeZero;
 
	bottom = genQubit("1",0);
	top = topqubit(n);
	input = tensor(top, bottom);
	hadtop = hadamard(n);
	input = tensor(hadtop, H) * input;
	input = U * input;
	input = tensor(hadtop, IDT) * input;
	meas = measure(top);
	input = tensor(meas, IDT) * input;
	outcomeZero =   input.norm();

	return outcomeZero;
}
int main ()
{
	int n;
	MatrixXcf Ub;
	MatrixXcf Uc;
	float outcome;
 
		n = 1;
	Ub = (Matrix<complex<float>, Dynamic, Dynamic>(4,4)<<1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0).finished();
	Uc = (Matrix<complex<float>, Dynamic, Dynamic>(4,4)<<1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1).finished();
	outcome = deutsch(n,Ub);
	cout << outcome << endl << endl;
	outcome = deutsch(n,Uc);
	cout << outcome << endl << endl;
	n = 2;
	Ub = (Matrix<complex<float>, Dynamic, Dynamic>(8,8)<<1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0).finished();
	outcome = deutsch(n,Ub);

	std::cout << outcome << endl;

	return 0;
}