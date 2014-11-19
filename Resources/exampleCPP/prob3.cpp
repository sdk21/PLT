#include <complex>
#include <iostream>
#include <Eigen/Dense>
#include <cmath>

using namespace Eigen;
using namespace std;

Matrix<complex<float>,4,4> tensorM2f(Matrix<complex<float>,2,2> a, Matrix<complex<float>,2,2> b);
Matrix<complex<float>,4,1> tensorV2f(Matrix<complex<float>,2,1> a, Matrix<complex<float>,2,1> b);

int main() {

	complex<float> i = -1;
	i = sqrt(i);

	Vector2f top, bottom;
	top << 1,0;
	bottom << 0,1;

	Matrix<complex<float>,2,2> H,I,X;
	H << 1, 1,
	     1, -1;
	H *= (1/sqrt(2));

	I = Matrix<complex<float>,2,2>::Identity();


	Matrix<complex<float>, 2, 2> Y;
	Y << 0, -i, 
 	     i, 0;
	/*
	Y(0,0) = 0;
	Y(1,0) = i;
	Y(0,1) = -1;
	Y(1,1) = 0;
	*/
	
	cout << i << endl;
	     

	X << 0, 1,
	     1, 0;
	
	//Matrix<complex<float>,4,1> output = tensorM2f(H,I) * tensorV2f(top, bottom);
	Matrix<complex<float>,4,1> output;
	output << 1,1,1,1;

	Matrix<complex<float>,4,4> CNOT;
	CNOT.topLeftCorner(2,2) = I;
	CNOT.topRightCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	CNOT.bottomLeftCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	CNOT.bottomRightCorner(2,2) = X;

	Matrix<complex<float>, 4, 4> CY;
	CY.topLeftCorner(2,2) = Y;
	CY.topRightCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	CY.bottomLeftCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	CY.bottomRightCorner(2,2) = I;

	output = CY * CNOT * output;

	Matrix<complex<float>,1,2> colTest;
	colTest << 0, 0;
	Matrix<complex<float>,2,2> test = top*colTest;
	Matrix<complex<float>,4,4> M = tensorM2f(test, I);

	output = M * output;

	cout << output.norm() << endl;

/*
	Vector4f mat0, mat1, mat2, mat3;
	mat0 << 1, 0, 0, 0;
	mat1 << 0, 1, 0, 0;
	mat0 << 0, 0, 1, 0;
	mat0 << 0, 0, 0, 1;

	Matrix4f CNOT, HNOT, allGates;
	CNOT << 1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1;

	HNOT << 1/sqrt(2), 0, 0, 1/sqrt(2),
		0, 1, 0, 0,
		1, 0, 1, -1/sqrt(2), 
		0, 0, 0, 0;

	allGates = CNOT * HNOT * CNOT;

	Matrix4f circuitMat;
	circuitMat.col(0) = allGates * mat0;
	circuitMat.col(1) = allGates * mat1;
	circuitMat.col(2) = allGates * mat2;
	circuitMat.col(3) = allGates * mat3;

	cout << circuitMat << endl;
  */	
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


