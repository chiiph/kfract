#include <CImg.h>
#include "utils.hpp"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>

using namespace std;

//An hsv colour scale.
vector<double> hsv(double x){

  vector<double> z(3);
  double h = 1.0/6;
  if(x >= 0 and x < h){
    z[0] = 1;
    z[1] = x/h;
    z[2] = 0;
  }
  else if( x >= h and x < 2*h){
    z[0] = 1 - (x-h)/h;
    z[1] = 1;
    z[2] = 0;
  }
  else if( x >= 2*h and x < 3*h){
    z[0] = 0;
    z[1] = 1;
    z[2] = (x-2*h)/h;
  }
  else if( x >= 3*h and x < 4*h){
    z[0] = 0;
    z[1] = 1 - (x-3*h)/h;
    z[2] = 1;
  }
  else if( x >= 4*h and x < 5*h){
    z[0] = (x-4*h)/h;
    z[1] = 0;
    z[2] = 1;
  }
  else if( x >= 5*h and x <= 6*h){
    z[0] = 1;
    z[1] = 0;
    z[2] = 1 - (x - 5*h)/h;
  }
  return z;
}

void display_matrix(const vector<vector<double> > &z, 
		    vector<double>(*colormap)(double))
{
  using namespace cimg_library;
  size_t rows = z.size();
  size_t cols = z[0].size();
  CImg<double> img(cols, rows, 1,3,1);
  double max = z[0][0];
  double min = z[0][0];
  
  for(size_t i = 0; i < rows; i++){
    for(size_t j = 0; j < cols; j++){
      if(z[i][j] >= max)
	max = z[i][j];
      if(z[i][j] <= min)
	min = z[i][j];
    }
  }
  for(size_t i = 0; i < rows; i++){
    for(size_t j = 0; j < cols; j++){
      double x = (z[i][j]-min)/(max - min);
      vector<double> colours = colormap(x);
       img(j,i,0) = colours[0];
       img(j,i,1) = colours[1];
       img(j,i,2) = colours[2];

       //Use this code instead for "random" colours. Change log(7) or
       //sqrt(42) to any other irrational number for fun and profit
       //
       //      img(j,i,0) = static_cast<int>(floor(log(7)*x*255))%255;
       //      img(j,i,1) = static_cast<int>(floor(sqrt(42)*x*255))%255;
       //      img(j,i,2) = static_cast<int>(floor(x*255))%255;
    }
  }
  
  //Will name the output files fractal0000.jpg, fracta0001.jpg,
  //successively and so on.  
  string filename = "fractal";
  size_t count = 0;
  string c_s;
  stringstream c_ss;
  c_ss << setw(4);
  c_ss << setfill('0');
  c_ss << count;
  c_ss >> c_s;
  ifstream ifs((filename+c_s+".jpg").c_str()) ;
  while(ifs)
    {
      ifs.close();
      c_ss.str(""); c_ss.clear();
      c_ss << setw(4);
      c_ss << setfill('0');
      c_ss << ++count;
      c_ss >> c_s;
      ifs.open((filename+c_s+".jpg").c_str()) ;
    }
  filename += c_s;
  filename += ".jpg";

  img.normalize(0,255);
  img.save(filename.c_str() );

  //Uncomment this code if viewing single images.

//    CImgDisplay disp(img, "fractal");
//    while (!disp.is_closed) {
//      disp.wait();
//    }

}
