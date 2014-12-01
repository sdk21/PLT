#ifndef QLANG_H_ 
#define QLANG_H_

using namespace Eigen;

MatrixXcf tensor(MatrixXcf mat1, MatrixXcf mat2);
Matrix4cf control(Matrix2cf mat);
MatrixXcf genQubit(string s);

#endif


