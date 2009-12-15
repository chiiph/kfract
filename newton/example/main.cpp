#include <cmath>
#include <vector>
#include <iostream>

#include "newton.hpp"
#include "utils.hpp" //Look here for specifics on how to plot.

using namespace std;

double r;

//Polynomial whose Newton fractal we shall plot.  (in this case, I
//computed a polynomial with 8 roots, four of which are rotated by the
//amount of rotation rot, 1 rot is one full rotation).
dcomplex f(dcomplex x){
  dcomplex rot = exp(8.0*dcomplex(0,1)*M_PI*r);
  return pow(x,8) + (4.0*rot-1.0)*pow(x,4) - 4.0*rot;
}

//polynomial's derivative
dcomplex df(dcomplex x){
  dcomplex rot = exp(8.0*dcomplex(0,1)*M_PI*r);
  return 8.0*pow(x,7) + 4.0*(4.0*rot-1.0)*pow(x,3);
}


int main(){
  
  //Rotate one quarter turn, later in post-processing stitch all
  //images together four times to get a full turn.
  for(r = 0; r <= 0.250; r += 0.001)
    {
      cout << "r: " << r << endl;
      
      //The view window
      double 
	xmin = -2, xmax = 2;
      double
	ymin = -2, ymax = 2;

      //Resolution
      size_t xsteps = 1280, ysteps = 1024;
      double  
	hx = (xmax - xmin)/(xsteps-1),
	hy = (ymax - ymin)/(ysteps-1);
    

      double tol = 1e-6;
  
      //More iterations for more colours!
      size_t maxiter = 50;
 
      vector<vector<double> > z;

      z.resize(ysteps);
      for(size_t i = 0; i < ysteps; i++)
	z[i].resize(xsteps);

      double x, y;
      y = ymin;
      for(size_t i = 0; i < ysteps; i++){
	x = xmin;
	for(size_t j = 0; j < xsteps; j++){
	  z[i][j] = newton(&f, &df, dcomplex(x,y), tol, maxiter);
	  x += hx;
	}
	y += hy;
      }
  
      //Display the fractal or print file, depending on what
      //display_matrix(...) says nowadays.
      display_matrix(z,&hsv);
    }
 
  return 0;
}

