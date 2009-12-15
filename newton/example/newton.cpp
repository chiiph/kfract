#include "newton.hpp"
#include <cmath>

double newton(complexfunc f, complexfunc df, dcomplex x0, 
	      double tol, size_t maxiter)
{
  dcomplex x = x0;
  size_t iter = 0;
  while(abs(f(x)) > tol){
    iter++;
    if(iter >= maxiter){
      break;
    }
    x = x - f(x)/df(x);
  }
  
  return iter;
}
