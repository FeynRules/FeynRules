#ifndef MASSMATRIXSQ_HPP
#define MASSMATRIXSQ_HPP

#include "headers.hpp"
#include "Parameters.hpp"
#include "MassMatrix.hpp"

class MassMatrixNF : public MassMatrix
{
	public:
        /****constructors****/

        //constructor without comment
        MassMatrixNF(string n, double** mp, double** rm, double** im, int d) : MassMatrix(n, mp, rm, im, d) {}

        /****pointer updating****/

        void exportResults();
};

#endif
