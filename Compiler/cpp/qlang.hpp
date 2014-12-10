#ifndef QLANG_HPP_ 
#define QLANG_HPP_

using namespace Eigen;
using namespace std;

//CONSTANTS
static const Matrix2cf H = (Matrix2cf() << 1/sqrt(2), 1/sqrt(2),
				1/sqrt(2), -1/sqrt(2)).finished();
static const Matrix2cf I = Matrix2cf::Identity();
static const Matrix2cf X = (Matrix2cf() << 0, 1, 1, 0).finished();
static const Matrix2cf Y = (Matrix2cf() << 0, -std::complex<float>(0,1),
				std::complex<float>(0,1), 0).finished();

//METHODS
MatrixXcf tensor(MatrixXcf mat1, MatrixXcf mat2);
Matrix4cf control(Matrix2cf mat);
MatrixXcf genQubit(string s, int bra);
MatrixXcf genQubits(string s);

#endif


