#include <iostream>
#include <Eigen/Dense>

using namespace Eigen;
using namespace std;

Matrix4f tensor2f(Matrix2f a, Matrix2f b);

int main() {

	Vector4f mat0, mat1, mat2, mat3;
	mat0 << 1, 0, 0, 0; // |00>
	mat1 << 0, 1, 0, 0; // |01> 
	mat0 << 0, 0, 1, 0; // |10>
	mat0 << 0, 0, 0, 1; // |11>

	//controlled NOT, controlled Hadmard, 
	Matrix4f CNOT, HNOT, allGates;
	CNOT << 1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1;

	HNOT << 1/sqrt(2), 0, 0, 1/sqrt(2),
		0, 1, 0, 0,
		1, 0, 1, -1/sqrt(2), 
		0, 0, 0, 0;

	//composition of the control gates
	allGates = CNOT * HNOT * CNOT;

	//matrix of circuit in Problem 2
	Matrix4f circuitMat;
	circuitMat.col(0) = allGates * mat0;
	circuitMat.col(1) = allGates * mat1;
	circuitMat.col(2) = allGates * mat2;
	circuitMat.col(3) = allGates * mat3;

	cout << circuitMat << endl;
  	
}

Matrix4f tensor2f(Matrix2f a, Matrix2f b) {
	Matrix4f out = Matrix4f::Zero();
	Matrix2f tl = a(0,0) * b;
	Matrix2f tr = a(1,0) * b;
	Matrix2f br = a(1,1) * b;
	Matrix2f bl = a(0,1) * b;

	out.topLeftCorner(2,2) = tl;
	out.topRightCorner(2,2) = tr;
	out.bottomRightCorner(2,2) = br;
	out.bottomLeftCorner(2,2) = bl;
	

	return out;


}

