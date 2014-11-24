#include <complex>
#include <Eigen/Dense>
#include <iostream>
#include "tensorProduct.h"
#include "constants.h"

using namespace Eigen;
using namespace std;

int deutschJozsa(Matrix<complex<float>, Dynamic, 1> top, Matrix<complex<float>,
	Dynamic, Dynamic> U) {

	//input is tensor product of top and bottom register
	MatrixXcf input = tensor(top, Vector2cf(0,1));

	//apply Hadamard gate on both top and bottom inputs
	input = tensor(H,H) * input;

	//apply U on result and then Hadamard gate on the top register
	input = U * input;
	input = tensor(H,I) * input;

	//apply measurement operator on top register
	//top * top.adjoint() is measurement operator
	input = tensor((top * top.adjoint()),I) * input;

	//check if probability of outcome is 0 or 1 by iterating through all
	//values of the new calculated input matrix	
	int outcome = 0;
	for(int index = 0; index < input.rows(); index++) {
		if(input(index,0) != complex<float>(0,0)) {
			outcome = 1;
			break;
		}
	}

	return outcome;
}

//testing
int main() {
	Vector2cf top;
	top << 1,0;

	Matrix4cf mat;
	mat = Matrix4cf::Identity();
	
	cout << deutschJozsa(top,mat) << endl;

	return 0;
}

