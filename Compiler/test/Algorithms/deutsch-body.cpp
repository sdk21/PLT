
        #include <iostream>
        #include <complex>
        #include <cmath>
        #include <Eigen/Dense>
        #include <qlang>
        using namespace Eigen;
        using namespace std;
        
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
MatrixXcf measure (MatrixXcf top )
{
	MatrixXcf ad;
	MatrixXcf result;
 
	ad =   top.adjoint();
	result = top * ad;

	return result;
}
MatrixXcf deutsch (int n,MatrixXcf top,MatrixXcf U )
{
	MatrixXcf bottom;
	MatrixXcf input;
	MatrixXcf hadtop;
	MatrixXcf meas;
	MatrixXcf Idt;
	float m;
	MatrixXcf outcomeZero;
 
	Idt = (Matrix<complex<float>, Dynamic, Dynamic>(2,2)<<1,0,0,1).finished();
	bottom = genQubit("0",0);
	input = (Matrix<complex<float>, Dynamic, Dynamic>(4,1)<<0,1,0,0).finished();
	hadtop = hadamard(n);
	input = tensor(hadtop, H) * input;
	input = U * input;
	input = tensor(hadtop, Idt) * input;
	meas = measure(top);
	input = tensor(meas, Idt) * input;
	m =   input.norm();
	cout << m << endl;
	outcomeZero = input;

	return outcomeZero;
}
int main ()
{
	MatrixXcf top;
	int n;
	MatrixXcf Ub;
	MatrixXcf Uc;
	MatrixXcf outcome;
 
		n = 1;
	top = genQubit("0",0);
	Ub = (Matrix<complex<float>, Dynamic, Dynamic>(4,4)<<1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0).finished();
	Uc = (Matrix<complex<float>, Dynamic, Dynamic>(4,4)<<1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1).finished();
	outcome = deutsch(n,top,Ub);

	std::cout << outcome << endl;

	return 0;
}