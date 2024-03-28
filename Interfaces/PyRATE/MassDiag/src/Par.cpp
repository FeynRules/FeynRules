#include "Par.hpp"

double realpart(double x){return x;};
double imagpart(double x){return 0;};
double realpart(complex<double> x){return real(x);};
double imagpart(complex<double> x){return imag(x);};
