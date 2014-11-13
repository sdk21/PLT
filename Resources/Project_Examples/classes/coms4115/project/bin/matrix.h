#ifndef MATRIX_H
#define MATRIX_H

#include <iostream>
#include <vector>
#include <cmath>
#include <cstring>
#include <sstream>
#include <assert.h>
using namespace std;

typedef double Scalar;
typedef bool Boolean;
typedef string String;

class Matrix
{
	vector< vector <double> > v;
public:
	Matrix(unsigned int r=1, unsigned int c=1)
	{
		assert(r>0);
		assert(c>0);

		for(unsigned int i=0; i<r; i++)
		{
			v.push_back(vector<double>());
			for(unsigned int j=0; j<c; j++)
			{
				v[i].push_back(0);
			}
		}
	}
	Matrix(const Matrix &x)
	{
		v.clear();
		for(unsigned int i=0; i<x.v.size(); i++)
		{
			v.push_back(vector<double>());
			for(unsigned int j=0; j<x.v[i].size(); j++)
			{
				v[i].push_back(x.v[i][j]);
			}
		}
	}
	void reDim(unsigned int i, unsigned int j)
	{
		unsigned int nRows = getNumRows();
		unsigned int nCols = getNumCols();

		for(unsigned int indi=i; indi<nRows; indi++)
		{
			v.pop_back();
		}

		for(unsigned int indj=j; indj<nCols; indj++)
		{
			for(unsigned int indi=0; indi<nRows; indi++)
			{
				v[indi].pop_back();
			}
		}

		nRows = getNumRows();
		nCols = getNumCols();

		if(i<nRows && j<nCols)
		{
			return;
		}

		for(unsigned int indi=0; indi<i; indi++)
		{
			if(indi>=nRows)
			{
				v.push_back(vector<double>());
			}
			for(unsigned int indj=0; indj<j; indj++)
			{
				if(indi>=nRows)
				{
					v[indi].push_back(0);
				}
				else if(indj>=nCols)
				{
					v[indi].push_back(0);
				}
			}
		}
	}
	Matrix transpose()
	{
		unsigned int nRows = getNumRows();
		unsigned int nCols = getNumCols();

		Matrix m(nCols,nRows);

		for(unsigned int i=0; i<m.v.size(); i++)
		{
			for(unsigned int j=0; j<m.v[i].size(); j++)
			{
				m.v[i][j]=v[j][i];
			}
		}
		return m;
	}
	Matrix power(Scalar x)
	{
		unsigned int nRows = getNumRows();
		unsigned int nCols = getNumCols();

		Matrix m(nRows,nCols);

		for(unsigned int i=0; i<m.v.size(); i++)
		{
			for(unsigned int j=0; j<m.v[i].size(); j++)
			{
				m.v[i][j]=pow(v[i][j],x);
			}
		}
		return m;
	}
	void setElement(unsigned int i, unsigned int j, Scalar x)
	{
		//assert(i<v.size());
		//assert(j<v[i].size());

		if(i>=v.size())
		{
			reDim(i+1,getNumCols());
		}

		if(j>=v[i].size())
		{
			reDim(getNumRows(),j+1);
		}

		v[i][j]=x;
	}
	Scalar getElement(unsigned int i, unsigned int j)
	{
		assert(i<v.size());
		assert(j<v[i].size());

		return v[i][j];
	}
	unsigned int getNumRows()
	{
		return v.size();
	}
	String toString()
	{
		ostringstream strs;
		for(unsigned int i=0; i<v.size(); i++)
		{
			for(unsigned int j=0; j<v[i].size(); j++)
			{
				strs<<v[i][j]<<"\t";
			}
			strs<<"\n";
		}
		return strs.str();
	}
	unsigned int getNumCols()
	{
		return v[0].size();
	}
	bool operator == (Matrix& m)
	{
		unsigned int nRows = getNumRows();
		unsigned int nCols = getNumCols();

		assert(nRows==m.getNumRows());
		assert(nCols==m.getNumCols());

		bool isEqual = true;

		for(unsigned int i=0; i<m.v.size(); i++)
		{
			for(unsigned int j=0; j<m.v[i].size(); j++)
			{
				if(v[i][j]!=m.v[i][j])
				{
					isEqual = false;
					break;
				}
			}
			if(!isEqual)
			{
				break;
			}
		}
		return isEqual;
	}
	friend ostream& operator << (ostream& os, const Matrix& m);
	friend Matrix operator + (Matrix& m1, Matrix& m2);
	friend Matrix operator - (Matrix& m1, Matrix& m2);
	friend Matrix operator * (Matrix& m1, Matrix& m2);
	friend Matrix operator * (Matrix& m1, Scalar& s2);
	friend Matrix operator - (Matrix& m1);
};

ostream& operator << (ostream& os, const Matrix& m)
{
	for(unsigned int i=0; i<m.v.size(); i++)
	{
		for(unsigned int j=0; j<m.v[i].size(); j++)
		{
			os<<m.v[i][j]<<"\t";
		}
		os<<"\n";
	}
	return os;
}

Matrix operator + (Matrix& m1, Matrix& m2)
{
	unsigned int nRows = m1.getNumRows();
	unsigned int nCols = m1.getNumCols();

	assert(nRows==m2.getNumRows());
	assert(nCols==m2.getNumCols());

	Matrix m(nRows,nCols);
	for(unsigned int i=0; i<m.v.size(); i++)
	{
		for(unsigned int j=0; j<m.v[i].size(); j++)
		{
			m.v[i][j]=m1.v[i][j]+m2.v[i][j];
		}
	}
	return m;
}

Matrix operator - (Matrix& m1, Matrix& m2)
{
	unsigned int nRows = m1.getNumRows();
	unsigned int nCols = m1.getNumCols();

	assert(nRows==m2.getNumRows());
	assert(nCols==m2.getNumCols());

	Matrix m(nRows,nCols);
	for(unsigned int i=0; i<m.v.size(); i++)
	{
		for(unsigned int j=0; j<m.v[i].size(); j++)
		{
			m.v[i][j]=m1.v[i][j]-m2.v[i][j];
		}
	}
	return m;
}

Matrix operator * (Matrix& m1, Matrix& m2)
{
	unsigned int nCols = m1.getNumCols();
	unsigned int nRows = m2.getNumRows();

	assert(nCols==nRows);

	Matrix m(m1.getNumRows(),m2.getNumCols());
	for(unsigned int i=0; i<m.v.size(); i++)
	{
		for(unsigned int j=0; j<m.v[i].size(); j++)
		{
			double element = 0;
			for(unsigned int k=0; k<nRows; k++)
			{
				element += m1.v[i][k]*m2.v[k][j];
			}
			m.v[i][j] = element;
		}
	}
	return m;
}

Matrix operator * (Matrix& m1, Scalar& s2)
{
	unsigned int nRows = m1.getNumRows();
	unsigned int nCols = m1.getNumCols();

	Matrix m(nRows,nCols);
	for(unsigned int i=0; i<m.v.size(); i++)
	{
		for(unsigned int j=0; j<m.v[i].size(); j++)
		{
			m.v[i][j]=m1.v[i][j]*s2;
		}
	}
	return m;
}

Matrix operator - (Matrix& m1)
{
	unsigned int nRows = m1.getNumRows();
	unsigned int nCols = m1.getNumCols();
	Matrix m(nRows,nCols);
	for(unsigned int i=0; i<m.v.size(); i++)
	{
		for(unsigned int j=0; j<m.v[i].size(); j++)
		{
			m.v[i][j]=0-m1.v[i][j];
		}
	}
	return m;
}

#endif //MATRIX_H
