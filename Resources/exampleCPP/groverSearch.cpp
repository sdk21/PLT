#include <complex>
#include <Eigen/Dense>
#include <iostream>
#include "tensorProduct.h"
#include "constants.h"

using namespace Eigen;
using namespace std;

MatrixXcf grover(Matrix<complex<float>, Dynamic, 1> top, int x0) {

	//bottom register qubit
	Matrix2cf bottom;
	bottom << 0,1;

	//tensor of top and bottom
	MatrixXcf input = tensor(top,bottom);

	//application of Hadamard
	input = tensor(H,H) * input;

	//define S
	Matrix2cf S;
	S << 1,0,0,1;

	//k is number of times grover operator is applied
	int k = 1;

	//define O operator
	Matrix2cf = I;
	O(x0+1, x0+1) = -1;

	//Grover iteration matrix
	MatrixXcf GO = (G*O);
	for(int index = 1; index < k; index++) {
		GO = GO * GO;
	}

	//application of iteration matrix
	MatrixXcf output = GO * input;
	MatrixXcf result = tensor(H,H) * output;

	return result;

}
