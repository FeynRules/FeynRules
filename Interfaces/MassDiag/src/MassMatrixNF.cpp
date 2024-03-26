#include "MassMatrixNF.hpp"

//Prints the real and imaginary parts of the matrix
void MassMatrixNF::printMassm(ofstream& out)
{
    out << "Mass Matrix " << name << " REAL " << "    " << "# " << comment << endl;
    printRealMatrixList(massm, dim, dim, out);
    out << "Mass Matrix " << name << " IMAG " << "    " << "# " << comment << endl;
    printImagMatrixList(massm, dim, dim, out);
}

//prints the sqrt of the vector
void MassMatrixNF::printPdgMass(ofstream& out)
{
    gsl_vector* v = this->eigv;
    int d = dim;
    for(int i = 0; i < d; i++)
    {
        if(abs(gsl_vector_get(v, i)) < pow(10., -5))
        {
            printInt(pdg.at(i), 10, out);
            out << "    " << 0.000000e+00 
            << "    "  << "# " << pdgcom.at(i) << endl;
        }
        else
        {
            printInt(pdg.at(i), 10, out);
            out << "    " << gsl_vector_get(v, i)
            << "    "  << "# " << pdgcom.at(i) <<endl;
        }
    }
}
