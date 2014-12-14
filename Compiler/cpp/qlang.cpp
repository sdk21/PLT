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
	output.topLeftCorner(2,2) = IDT;
	output.topRightCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	output.bottomLeftCorner(2,2) = Matrix<complex<float>,2,2>::Zero();
	output.bottomRightCorner(2,2) = mat;

	return output;
}

MatrixXcf genQubit(string s, int bra) {
	
	int slen = s.length();
	int qlen = pow(2,slen); //length of vector

	int base10num = 0;

	//iterates through qstr. Whenever digit is a 1, it adds the associated 
	//power of 2 for that position to base10num
	const char * cq = s.c_str();
	char * c = new char();
	for(int i = 0; i < slen; i++) {
		strncpy(c,cq+i,1);
		base10num += strtol(c,NULL,10) * pow(2,(slen-1-i));  
	}
	delete c;

	//creates the vector and sets correct bit to 1
	MatrixXcf qub;
	if(bra) {
		qub = MatrixXcf::Zero(1,qlen);
		qub(0,qlen-1-base10num) = 1;
	} else if(!bra){
		qub = MatrixXcf::Zero(qlen,1);
		qub(base10num,0) = 1;
	}

	return qub;
}

string vectorToBraket(MatrixXcf qub) {
	int bra;
	int qlen;

	//determines whether bra or ket
	if(qub.rows() == 1) { qlen = qub.cols(); bra = 1; }
	else if(qub.cols() == 1) { qlen = qub.rows(); bra = 0;}
	else { //prints reg matrix if not row or column vector
		//cerr << "Incorrect matrix size for vectorToBraket" << endl;
		//exit(1);
		ostringstream  test;
		test << qub << endl;
		return test.str();
	}

	//gets position of 1 in the qubit
	complex<float> zero(0,0);
	int xi = 0;
	int yi = 0;
	int number;
	int index;
	string result;
	int count = 0;
	for(index = 0; index < qlen; index++) {
		if(bra) { xi = index; }
		else { yi = index; }

		if(qub(yi,xi) != zero) {
			if(bra) { number = qlen-1-index; }
			else { number = index; }

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

			//adds necessary 0s
			for(int i = bin.length(); i < outQubLen; i++) {
				bin += "0";
			}

			reverse(bin.begin(), bin.end()); //reverses

			ostringstream convert;
			float re = qub(yi,xi).real();
			float im = qub(yi,xi).imag();
			string oper = "";
			string rstr = "";
			string istr = "";

			//adds constant expression
			convert << "(";
			if(re != 0) { convert << re; }
			if(re != 0 && im != 0) { convert << "+"; }
			if(im != 0) { convert << im << "i"; }
			convert << ")";

			//cleans up (1) and (1i) cases
			string constant = convert.str();
			if(constant.compare("(1)") == 0) { constant = ""; }
			else if(constant.compare("(1i)") == 0) { constant = "i"; }

			//generates appropriate bra or ket representation
			string qubstr;
			if(bra) { qubstr = constant + "<" + bin + "|"; }
			else { qubstr = constant + "|" + bin + ">"; }

			if(count > 0) {
				result += " + " + qubstr;
			} else { result = qubstr; }
			count++;
		}
	}
	return result;
}

/*
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
*/
