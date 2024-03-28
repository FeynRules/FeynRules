#ifndef MASSMATRIXTWOMIX_HPP
#define MASSMATRIXTWOMIX_HPP

#include "headers.hpp"
#include "MassMatrix.hpp"

//class MassMatrixTwoMix, inheritating from MassMatrix
class MassMatrixTwoMix: public MassMatrix
{
    protected:
        /****parameters****/

        //pointer to output values of the real part of the second mixing matrix
        double** remixpointer2;
        //pointer to output values of the imaginary part of the second mixing matrix
        double** immixpointer2;

    public:
        //Second mass matrix
        gsl_matrix_complex* massm2;
        //Second mixing matrix
        gsl_matrix_complex* mixm2;
        //Eigenvalues
        gsl_vector* eigv2;

        //constructor without comment
        MassMatrixTwoMix(string, double**, double**, double**, double**, double**, int);
        //void initialize
        void initMassMatrix();

        /****destructor****/

        ~MassMatrixTwoMix();

        /****setters****/
        //set the pointer for the real part of the mixing matrix 2
        void setReMixPointer2(double** rm){remixpointer2 = rm;}
        //set the pointer for the imaginary part of the mixing matrix 2
        void setImMixPointer2(double** im){immixpointer2 = im;}

        /****getters****/

        //get the pointer for real part of the mixing matrix
        double** getReMixPointer2(){return remixpointer2;}
        //get the pointer for imaginary part of the mixing matrix
        double** getImMixPointer2(){return immixpointer2;}

        /****diagonalization****/

        //diagonalizes the mass matrix and prints the result
        void diagMatrix();


        /****pointers update****/
        void exportResults();
};


#endif

