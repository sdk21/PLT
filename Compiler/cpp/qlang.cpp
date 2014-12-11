#include <Eigen/Dense>
#include <iostream>
#include <complex>
#include <string>
#include <cmath>
#include "qlang.hpp"

using namespace Eigen;
using namespace std;

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

MatrixXcf genQubit(string s, int bra) {
	
	int slen = s.length();
	//int qstrlen = slen-2; //length of the qubit string removing end chars

	//string qstr = s.substr(1, qstrlen); //binary substring from qubit rep
	//int qlen = pow(2, qstrlen); //length of the vector
	int qlen = pow(2,slen); //length of vector

	int base10num = 0;

	//iterates through qstr. Whenever digit is a 1, it adds the associated 
	//power of 2 for that position to base10num
	const char * cq = s.c_str();
	//const char * cq = qstr.c_str();
	char * c = new char();
	for(int i = 0; i < slen; i++) {
	//for(int i = 0; i < qstrlen; i++) {
		strncpy(c,cq+i,1);
		//base10num += strtol(c,NULL,10) * pow(2,(qstrlen-1-i));  
		base10num += strtol(c,NULL,10) * pow(2,(slen-1-i));  
	}
	delete c;

	//cout << base10num << endl;

	//creates the vector and sets correct bit to 1
	MatrixXcf qub;
	//if(!s.compare(0, 1, "<")) {
	if(bra) {
		qub = MatrixXcf::Zero(1,qlen);
		qub(0,qlen-1-base10num) = 1;
	} else if(!bra){
		qub = MatrixXcf::Zero(qlen,1);
		qub(base10num,0) = 1;
	}

	return qub;
}

string qubitToString(MatrixXcf qub) {
	int bra;
	int qlen;
	if(qub.rows() == 1) { qlen = qub.cols(); bra = 1; }
	//int qlen = qub.cols();
	else if(qub.cols() == 1) { qlen = qub.rows(); bra = 0;}
	else { 
		cerr << "Incorrect matrix size for qubitToString" << endl;
		exit(1);
	}
	//cout << "bra " << bra << endl;

	//gets position of 1 in the qubit
	complex<float> one(1,0);
	int xi = 0;
	int yi = 0;
	int number;
	for(number = 0; number < qlen; number++) {
		if(bra) { xi = number; }
		else { yi = number; }

		if(qub(yi,xi) == one) {
			if(bra) { number = qlen-1-number; }
			//cout << i << endl;
			break;
		}
	}

	//converts position to binary number reversed
	string bin = "";
	do {
		if ( (number & 1) == 0 )
			bin += "0";
		else
			bin += "1";

		number >>= 1;
	} while ( number );

	int outQubLen = sqrt(qlen);
	//if(qlen%2) { outQubLen++; }

	//adds necessary 0s
	for(int i = bin.length(); i < outQubLen; i++) {
		bin += "0";
	}

	reverse(bin.begin(), bin.end()); //reverses

	//generates appropriate bra or ket representation
	string result;
	if(bra) { result = "<" + bin + "|"; }
	else { result = "|" + bin + ">"; }

	return result;
}

MatrixXcf genQubits(string s) {

	int slen = s.length();
	int qstrlen = slen-2; //length of the qubit string removing end chars

	string qstr = s.substr(1, qstrlen); //binary substring from qubit rep
	int qlen = pow(2, qstrlen); //length of the vector
	//int qlen = pow(2,slen); //length of vector

	int base10num = 0;

	//iterates through qstr. Whenever digit is a 1, it adds the associated 
	//power of 2 for that position to base10num
	const char * cq = qstr.c_str();
	char * c = new char();
	for(int i = 0; i < qstrlen; i++) {
		strncpy(c,cq+i,1);
		base10num += strtol(c,NULL,10) * pow(2,(qstrlen-1-i));  
	}
	delete c;

	//creates the vector and sets correct bit to 1
	MatrixXcf qub;
	if(!s.compare(0, 1, "<")) {
		//if(bra)
		qub = MatrixXcf::Zero(1,qlen);
		qub(0,qlen-1-base10num) = 1;
		//	} else if(!bra){
	} else {
		qub = MatrixXcf::Zero(qlen,1);
		qub(base10num,0) = 1;
	}

	return qub;
}

/*
int main() {
	   cout << genQubit("00",1) << endl;
	   cout << genQubit("01",1) << endl;
	   cout << genQubit("10",1) << endl;
	   cout << genQubit("11",1) << endl;
	 cout << genQubit("11",1).isApprox(genQubit("11",1))<< endl;
	 //cout << genQubit("01",1).isApprox( genQubit("10",1)) << endl;
	//cout << genQubit("1101",0) << endl;
	cout << qubitToString(genQubit("01100",0)) << endl;
	complex<float> com(2,1);
	//cout << (Matrix2i()<<1,0,0,1).finished()*(Matrix2cf()<<com,com,com,com).finished()<< endl;

	Matrix<int, 2,2> in; in << 1,0,0,1;
	Matrix<complex<float>,2,2> cM; cM << 2,1.01,com,2;
	cout << cM << endl;


	return 0;
}
*/
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

