#ifndef LAME_H
#define LAME_H

#include "matrix.h"

inline void LAMEStrFromScalar(String &s, Scalar x)
{
	ostringstream strs;
	strs<<x;
	s = strs.str();
}

inline void LAMEStrFromMat(String &s, Matrix &m)
{
	s = m.toString();
}

inline void LAMEMatAdd(Matrix &m, Matrix &m1, Matrix &m2)
{
	m = m1 + m2;
}

inline void LAMEMatSub(Matrix &m, Matrix &m1, Matrix &m2)
{
	m = m1 - m2;
}

inline void LAMEMatMul(Matrix &m, Matrix &m1, Matrix &m2)
{
	m = m1 * m2;
}

inline void LAMEScalarMul(Matrix &m, Matrix &m1, Scalar s2)
{
	m = m1 * s2;
}

inline void LAMEMatPow(Matrix &m, Matrix &m1, Scalar s2)
{
	m = m1.power(s2);
}

inline void LAMESetElem(Matrix &m, unsigned int i, unsigned int j, Scalar s)
{
	m.setElement(i,j,s);
}

inline void LAMEGetElem(Scalar &s, Matrix &m, unsigned int i, unsigned int j)
{
	s=m.getElement(i,j);
}

inline void LAMEMatTrans(Matrix &m1, Matrix &m2)
{
	m1=m2.transpose();
}

inline void LAMEMatDim(Matrix &m, unsigned int i, unsigned int j)
{
	m.reDim(i,j);
}

inline void LAMEMatRows(Scalar &s, Matrix &m)
{
	s=m.getNumRows();
}

inline void LAMEMatCols(Scalar &s, Matrix &m)
{
	s=m.getNumCols();
}

inline void LAMEMatEqual(Boolean b, Matrix &m1, Matrix &m2)
{
	b=(m1==m2);
}

inline void LAMEMatNotEqual(Boolean b, Matrix &m1, Matrix &m2)
{
	b=!(m1==m2);
}

inline void LAMEPrint(String s)
{
	cout<<s;
}

inline void LAMEStrCopy(String &s1, String s2)
{
	s1 = s2;
}

inline void LAMEStrConcat(String &s, String &s1, String &s2)
{
	s = s1 + s2;
}

inline void LAMEStrCmp(Boolean &b, String &s1, String &s2)
{
	b = (s1==s2);
}

inline void LAMEStrFree(String &s)
{
}

inline void LAMEStrInit(String &s)
{
	s = "";
}

inline void LAMEMatInit(Matrix &m)
{
}

#endif //LAME_H
