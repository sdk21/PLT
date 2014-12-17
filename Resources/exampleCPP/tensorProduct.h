#ifndef TENSORPRODUCT_H_
#define TENSORPRODUCT_H_

using namespace Eigen;
using namespace std;

MatrixXcf tensor(MatrixXcf mat1, MatrixXcf mat2);
Matrix4cf control(Matrix2cf mat);
MatrixXcf genMatrix(string s);

#endif


