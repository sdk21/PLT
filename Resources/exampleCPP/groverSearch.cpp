#include <complex>
#include <Eigen/Dense>
#include <iostream>
#include "tensorProduct.h"
#include "constants.h"

using namespace Eigen;
using namespace std;

MatrixXcf grover(Matrix<complex<float>, Dynamic, 1> top, int x0) {

	//bottom register qubit
	Vector2cf bottom;
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
	Matrix4cf O = Matrix4cf::Identity();
	O(x0, x0) = -1;

	MatrixXcf G = tensor(H * S * H, I);

	//Grover iteration matrix
	//cout << "G dim " << G.rows() << " " << G.cols() << endl;
	MatrixXcf GO = (G*O);
	for(int index = 1; index < k; index++) {
		GO = GO * GO;
	}

	//application of iteration matrix
	MatrixXcf output = GO * input;
	MatrixXcf result = tensor(H,H) * output;

	return result;
	//return bottom;

}

int main() {

	cout << grover(Vector2cf(1,0),1) << endl;

	return 0;
}

