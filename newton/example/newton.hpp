#ifndef __NEWTON_HPP__
#define __NEWTON_HPP__

#include <complex>

typedef std::complex<double> dcomplex;
typedef dcomplex (*complexfunc)(dcomplex);

double newton(complexfunc f, complexfunc df, dcomplex x0, 
	      double tol, size_t maxiter);

#endif //__NEWTON_HPP__
