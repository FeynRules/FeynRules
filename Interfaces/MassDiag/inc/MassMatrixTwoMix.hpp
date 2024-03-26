#ifndef MASSMATRIXTWOMIX_HPP
#define MASSMATRIXTWOMIX_HPP

#include "headers.hpp"
#include "MassMatrix.hpp"

//class MassMatrixTwoMix, inheritating from MassMatrix
class MassMatrixTwoMix: public MassMatrix
{
    protected:
        /****parameters****/

        //second name of the real part of the mixing matrix
        string remixname2;
        //second name of the imaginary part of the mixing matrix
        string immixname2;
        
    public:
        //Second mass matrix
        gsl_matrix_complex* massm2;
        //Second mixing matrix
        gsl_matrix_complex* mixm2;
        //Eigenvalues
        gsl_vector* eigv2; 
        
        //Constructor
        MassMatrixTwoMix(string, string, string, string, string,
        int, int*, string*, string);
        //constructor without comment
        MassMatrixTwoMix(string, string, string, string, string,
        int, int*, string*);
        //void initialize
        void initMassMatrix();
        
        /****destructor****/
        
        ~MassMatrixTwoMix();  
        
        /****setters****/

        //set the name of the real part of the mixing matrix
        void setReMixName2(string rm){remixname2 = rm;}
        //set the name of the imaginary part of the mixing matrix
        void setImMixName2(string im){immixname2 = im;}
        
        /****getters****/
   
        //get the name of real part of the mixing matrix
        string getReMixName2(){return remixname2;}
        //get the name of imaginary part of the mixing matrix
        string getImMixName2(){return immixname2;}

        /****diagonalization****/
        
        //diagonalizes the mass matrix and prints the result
        void diagMatrix();   
        
        /****printing****/

        //Prints real and imag parts of the mass matrix
        //Prints real and imag part of the mixing matrix
        void printMixm(ofstream&);     
           
};


#endif

