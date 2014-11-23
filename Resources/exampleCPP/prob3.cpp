#include <complex>
#include <iostream>
#include <Eigen/Dense>
#include <cmath>

using namespace Eigen;
using namespace std;

Matrix<complex<float>,4,4> tensorM2f(Matrix<complex<float>,2,2> a, Matrix<complex<float>,2,2> b);
Matrix<complex<float>,4,1> tensorV2f(Matrix<complex<float>,2,1> a, Matrix<complex<float>,2,1> b);

int main() {

	//imaginary
	complex<float> i = -1;
	i = sqrt(i);

	//top and bottom qubits
	Matrix<complex<float>,2,1> top, bottom;
	top << 1,0;
	bottom << 0,1;

	//hadamard, identity, and X
	Matrix<complex<float>,2,2> H,I,X;
	H << 1, 1,
	     1, -1;
	H *= (1/sqrt(2));

	I = Matrix<complex<float>,2,2>::Identity();

	Matrix<complex<float>, 2, 2> Y;
	Y << 0, -i, 
 	     i, 0;

	X << 0, 1,
	     1, 0;

	//apply H on top
	Matrix<complex<float>,4,1> output = tensorM2f(H,I) * tensorV2f(top, bottom);
	cout << "apply H on top" << endl;
	cout << output << endl;
	cout << endl;

	//initialize CNOT
	Matrix<complex<float>,4,4> CNOT;
	CNOT.topLeftCorner(2,2) = I;
	CNOT.topRightCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	CNOT.bottomLeftCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	CNOT.bottomRightCorner(2,2) = X;
	cout << "CNOT" << endl;
	cout << CNOT<<endl;
	cout << endl;

	//initialize CY
	Matrix<complex<float>, 4, 4> CY;
	/*
	CY.topLeftCorner(2,2) = Y;
	CY.topRightCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	CY.bottomLeftCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	CY.bottomRightCorner(2,2) = I;
	*/
	CY << 1, 0, 0, 0,
	      0, 0, 0, -i,
	      0, 0, 1, 0,
	      0, i, 0, 0;
	cout << "CY" << endl;
	cout << CY << endl;

	//applying control operators
	output = CNOT * output;
	cout << "CNOT on output" << endl;
	cout << output << endl << endl;

	output = CY * output;
	cout << "apply control operators" << endl;
	cout << output << endl;
	cout << endl;

	
	Matrix<complex<float>,2,1> col0;
	col0 << 1, 0;
	Matrix<complex<float>,1,2> row0;
	row0 << 1, 0;
	Matrix<complex<float>,2,2> colTest;
	//colTest << 0, 0;
	colTest = col0 * row0;
	cout << "colTest" << endl;
	cout << colTest << endl;
	cout << endl;

	Matrix<complex<float>,4,4> M = tensorM2f(colTest, I);
//	Matrix<complex<float>,2,2> test = top*colTest;
//	Matrix<complex<float>,4,4> M = tensorM2f(test, I);
	cout << "M" << endl;
	cout << M << endl;
	cout << endl;

	output = M * output;
	cout << "M on output" << endl << output << endl << endl;

	cout << output.norm() << endl;

}

Matrix<complex<float>,4,4> tensorM2f(Matrix<complex<float>,2,2> a, Matrix<complex<float>,2,2> b) {
	Matrix<complex<float>,4,4> out = Matrix<complex<float>,4,4>::Zero();
	Matrix<complex<float>,2,2> tl = a(0,0) * b;
	Matrix<complex<float>,2,2> tr = a(1,0) * b;
	Matrix<complex<float>,2,2> br = a(1,1) * b;
	Matrix<complex<float>,2,2> bl = a(0,1) * b;

	out.topLeftCorner(2,2) = tl;
	out.topRightCorner(2,2) = tr;
	out.bottomRightCorner(2,2) = br;
	out.bottomLeftCorner(2,2) = bl;

	return out;
}

Matrix<complex<float>,4,1> tensorV2f(Matrix<complex<float>,2,1> a, Matrix<complex<float>,2,1> b) {
	Matrix<complex<float>,4,1> out = Matrix<complex<float>,4,1>::Zero();

	Matrix<complex<float>,2,1> top = a(0) * b;
	Matrix<complex<float>,2,1> bot = a(1) * b;

	out.head(2) = top;
	out.tail(2) = bot;

	return out;

}


