#ifndef __UTILS_HPP__
#define __UTILS_HPP__

#include <vector>

std::vector<double> hsv(double in);

void display_matrix(const std::vector<std::vector<double> > &z,
		    std::vector<double>(*colormap)(double));

#endif //__UTILS_HPP__
