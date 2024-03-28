#include "MassMatrixNF.hpp"

/****pointers updating****/

void MassMatrixNF::exportResults()
{
    int c = 0;
    gsl_complex tmp;
    double resu;

    for(int i = 0 ; i < dim ; i++) //row number
    {
        for(int j = 0; j < dim ; j++) //column number
        {
            tmp = gsl_matrix_complex_get(mixm, i, j);
            (*remixpointer[c]) = tmp.dat[0];
            (*immixpointer[c]) = tmp.dat[1];

            c++;
        }

        resu = gsl_vector_get(this->eigv, i);
        if(abs(resu) < pow(10., -10))
            resu = 0;

        if(mpointer[i] != NULL)
            (*mpointer[i]) = sqrt(abs(resu));
    }
}
