#if !defined(CONSTANTS_H)
#define CONSTANTS_H 1


using namespace Eigen;

static const Matrix2cf H = (Matrix2cf() << 1/sqrt(2), 1/sqrt(2), 1/sqrt(2),
	-1/sqrt(2)).finished();

static const Matrix2cf I = Matrix2cf::Identity();
static const Matrix2cf X = (Matrix2cf() << 0, 1, 1, 0).finished();
static const Matrix2cf Y = (Matrix2cf() << 0, -std::complex<float>(0,1),
	std::complex<float>(0,1), 0).finished();



#endif
