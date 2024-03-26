#ifndef MASSMATRIXSQ_HPP
#define MASSMATRIXSQ_HPP

#include "headers.hpp"
#include "Parameters.hpp"
#include "MassMatrix.hpp"

class MassMatrixNF : public MassMatrix
{
	public:
        /****constructors****/
        
        //Constructor
        MassMatrixNF(string s, string b, string imb, int d, int* pdg, string* names, string c) : MassMatrix(s, b, imb, d, pdg, names, c) {}
        //constructor without comment
        MassMatrixNF(string s, string b, string imb, int d, int* pdg, string* names) : MassMatrix(s, b, imb, d, pdg, names) {}      	    
        //Prints real and imag parts of the mass matrix
        virtual void printMassm(ofstream&);	  
        //prints a list of the indices and sqrt(value) of a vector
        virtual void printPdgMass(ofstream&);  
};

#endif