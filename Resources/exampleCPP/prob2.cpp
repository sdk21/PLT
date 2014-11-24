#include <iostream>
#include <Eigen/Dense>

using namespace Eigen;
using namespace std;


int main() {

	Vector4cf mat0, mat1, mat2, mat3;
	mat0 << 1, 0, 0, 0; // |00>
	mat1 << 0, 1, 0, 0; // |01> 
	mat0 << 0, 0, 1, 0; // |10>
	mat0 << 0, 0, 0, 1; // |11>

	//controlled NOT, controlled Hadamard, 
	Matrix4cf CNOT, HNOT, allGates;
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
	Matrix4cf circuitMat;
	circuitMat.col(0) = allGates * mat0;
	circuitMat.col(1) = allGates * mat1;
	circuitMat.col(2) = allGates * mat2;
	circuitMat.col(3) = allGates * mat3;

	cout << circuitMat << endl;
  	
}
