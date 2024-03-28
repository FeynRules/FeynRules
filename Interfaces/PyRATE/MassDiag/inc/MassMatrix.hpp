#ifndef MASSMATRIX_HPP
#define MASSMATRIX_HPP

#include "headers.hpp"
#include "Parameters.hpp"

class MassMatrix
{
    protected:

        /****tools to keep track of the position in the vector****/

        //position in the vector
        int p;
        //set the position in the vector
        void setP(int i) {p = i;}
        //get the position in the vector
        int getP() {return p;}

        /****parameters****/

        //name of the block
        string name;
        //dimension of the matrix
        int dim;

        //pointer to output values of the masses
        double** mpointer;
        //pointer to output values of the real part of the mixing matrix
        double** remixpointer;
        //pointer to output values of the imaginary part of the mixing matrix
        double** immixpointer;


    public:

        static vector<MassMatrix*> MassMatrices;

        /****parameters****/

        /*
        static instance of Parameters, contains all the parameters
        and mass matrix initialisation functions provided by FeynRules
        */

        static Parameters* par;

        //Mass matrix
        gsl_matrix_complex* massm;
        //Mixing matrix
        gsl_matrix_complex* mixm;
        //Eigenvalues
        gsl_vector* eigv;

        /****constructors****/

        //constructor without comment
        MassMatrix(string, double**, double**, double**, int);
        //function for initalisation of the parameters
        void init(string, double**, double**, double**, int);
        //Initialisation of the mass matrix
        virtual void initMassMatrix();


        /****destructor****/

        ~MassMatrix();

        /****setters****/

        //set the name of the MassMatrix
        void setName(string n){name = n;}
        //set the pointer for the masses
        void setMassPointer(double** m){mpointer = m;}
        //set the pointer for the real part of the mixing matrix
        void setReMixPointer(double** rm){remixpointer = rm;}
        //set the pointer for the imaginary part of the mixing matrix
        void setImMixPointer(double** im){immixpointer = im;}
        //set the dimension
        void setDim(int d){dim = d;}


        /****getters****/

        //get the name of the MassMatrix
        string getName(){return name;}
        //set the pointer for the masses
        double** getMassPointer(){return mpointer;}
        //get the pointer for real part of the mixing matrix
        double** getReMixPointer(){return remixpointer;}
        //get the pointer for imaginary part of the mixing matrix
        double** getImMixPointer(){return immixpointer;}
        //get the dimension
        int getDim(){return dim;}


        /****diagonalization****/

        //diagonalizes the mass matrix and prints the result
        virtual void diagMatrix();

        /****pointer updating****/

        virtual void exportResults();

        /****printing****/


        //main routine, checks the input given at the command line
        //diagonalizes massmatrices and generates the output
		static void generateAll()
		{
            //generate all the output
            generateAllOutput(MassMatrices);
        }

        //Prints the output of all MassMatrices
		static void generateAllOutput(vector<MassMatrix*> m)
		{
            initAll(m);
			diagonalizeAll(m);
            exportAll(m);
		}

        //initialize all the mass matrices
		static void initAll(vector<MassMatrix*> m)
		{
		    for(int i = 0; i < m.size() ; i++)
		    {
		        m.at(i)->initMassMatrix();
		    }
		}

        //diagonalizes all the matrices
		static void diagonalizeAll(vector<MassMatrix*> m)
		{
		    for(int i = 0; i < m.size() ; i++)
		    {
		        m.at(i)->diagMatrix();
		    }
		}

        //updates the pointers to all involved externals
		static void exportAll(vector<MassMatrix*> m)
		{
		    for(int i = 0; i < m.size() ; i++)
		    {
		        m.at(i)->exportResults();
		    }
		}

};


#endif
