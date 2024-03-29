#ifndef HEADERS_HPP
#define HEADERS_HPP

#include <string.h>   // Strings                        //
#include <sstream>    // String streams                 //
#include <fstream>    // Read/Write files               //
#include <iostream>   // In/Out streams                 //
#include <cstdlib>    // C Standard General Utilities   //
#include <cmath>      // Mathematical functions  
#include <vector>	  // Vectors
#include <stdexcept>  // To throw exeptions
#include <complex>
#include <map>
#include <iomanip>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_complex.h>
	
using namespace std;
	
/*****type definitions*****/
extern ofstream logfile;

//declaration of the class Parameters necessary
class Parameters;
//the rges
typedef int (Parameters::*parFP)(double, const double *, double *, void *);
//functions to initialise the mass matrices
typedef void (Parameters::*initFP)(gsl_matrix_complex*);
//functions to initialise the mass matrices with two mixing matrices
typedef void (Parameters::*initFP2)(gsl_matrix_complex*, gsl_matrix_complex*);

//class to make functions compatible with gsl
template <class T>
class functionWrapper{
    public:
        static void initialize(T* funcPtr)
        {
            _funcPtr = funcPtr;
        }
       
        static int evaluate_rges
        (double x, const double* y, double* z , void* params)
        {
            return (*_funcPtr).rges(x, y, z, params);
        }
 
    private:
        static T* _funcPtr;
};

template <typename T> T* functionWrapper<T>::_funcPtr;

#endif
