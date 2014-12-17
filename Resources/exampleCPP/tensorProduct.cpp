#include <complex>
#include <Eigen/Dense>
#include <cmath>
#include <iostream>
#include <sstream>
#include "tensorProduct.h"
#include "constants.h"


using namespace Eigen;
using namespace std;

/*
Matrix<complex<float>, Dynamic, Dynamic> tensor(Matrix<complex<float>, Dynamic,
	Dynamic> mat1, Matrix<complex<float>, Dynamic, Dynamic> mat2) {
	*/
MatrixXcf tensor(MatrixXcf mat1, MatrixXcf mat2) {

	int mat1rows = mat1.rows();
	int mat1cols = mat1.cols();
	int mat2rows = mat2.rows();
	int mat2cols = mat2.cols();

	MatrixXcf output(mat1rows * mat2rows, mat1cols * mat2cols);

	//iterates through one matrix, multiplying each element with the whole
	//2nd matrix
	for(int m = 0; m < mat1rows; m++) {
		for(int n = 0; n < mat1cols; n++) {
			output.block(m*mat2cols,n*mat2rows,mat2rows,mat2cols) = 
				mat1(m,n) * mat2;
		}
	}

	return output;
	
}

Matrix4cf control(Matrix2cf mat) {
	Matrix4cf output;
	output.topLeftCorner(2,2) = I;
	output.topRightCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	output.bottomLeftCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	output.bottomRightCorner(2,2) = mat;

	return output;
}

MatrixXcf genMatrix(string s) {

	//MatrixXcf output;

	std::istringstream iss(s);
	std::string token1, token2;
	int y = 0;
	int x = 0;
	while(std::getline(iss, token1, '\n')) {
		std::istringstream iss2(token1);
		while(y == 0 && std::getline(iss2, token2, ',')) {
			x++;
		//	cout << "x = " << x << endl;
		}
		y++;
		//cout << "y = " << y << endl;
	}

	string ttttt = "0,0,0,0";
	//output = Matrix<complex<float>, y,y>();
	MatrixXcf output = (Matrix<complex<float>, Dynamic,
	Dynamic>(x,y)<< ttttt).finished();
	//cout << x << " " << y << endl;
	//output = Zero();
	/*
	complex<float> cf;
	std::istringstream parse(s);
	y = 0;
	while(std::getline(parse, token1, '\n')) {
		x = 0;
	//	cout << token1 << endl;
		std::istringstream iss2(token1);
		while(std::getline(iss2, token2, ',')) {
	//		cout << token2 << " " << x << " " << y<< endl;
			std::istringstream temp('('+ token2 + ')');
			temp >> cf;
			output(x,y) = cf;
			x++;
		}
		y++;
	}
	*/

	return output;

}

/*
int main() {

	complex<float> c1 = 2;
	complex<float> c2 = 3;
	Matrix2cf mat1, mat2;
	mat1 << c1,c1,c1,c1;
	mat2 << c2,c2,c2,0;

	cout << tensor(mat1,mat2) << endl;


	return 0;
}
*/

