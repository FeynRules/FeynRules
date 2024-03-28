#include "MassMatrixTwoMix.hpp"


//Constructor of a (d x d)-MassMatrix with name n
MassMatrixTwoMix::MassMatrixTwoMix(string n, double** mp, double** rm, double** im, double** rm2, double** im2, int d):MassMatrix(n, mp, rm, im, d)
{
    //initialisation of pointers of the 2nd mixing matrix
    setReMixPointer2(rm2);
    setImMixPointer2(im2);

    //assign memory
    massm2 = gsl_matrix_complex_alloc(d, d);
    mixm2 = gsl_matrix_complex_alloc(d, d);
    eigv2 = gsl_vector_alloc(d);
}

//Initialise the mass matrix
void MassMatrixTwoMix::initMassMatrix()
{
    //initialisation of the matrix
    (par->*(par->massInit2[name]))(massm, massm2);
}

//Destructor
MassMatrixTwoMix::~MassMatrixTwoMix()
{
    gsl_matrix_complex_free(massm2);
    gsl_matrix_complex_free(mixm2);
    gsl_vector_free(eigv2);
}

//Diagonalize the matrix
void MassMatrixTwoMix::diagMatrix()
{
    //eigensystem
    gsl_eigen_hermv_workspace *w = gsl_eigen_hermv_alloc(dim);
    gsl_eigen_hermv(massm, eigv, mixm, w);
    gsl_eigen_hermv_sort(eigv, mixm, GSL_EIGEN_SORT_ABS_ASC);
    gsl_eigen_hermv_free(w);

    gsl_eigen_hermv_workspace *w2 = gsl_eigen_hermv_alloc(dim);
    gsl_eigen_hermv(massm2, eigv2, mixm2, w2);
    gsl_eigen_hermv_sort(eigv2, mixm2, GSL_EIGEN_SORT_ABS_ASC);
    gsl_eigen_hermv_free(w2);

    //Since GSL put the eigenvectors as columns, we need to transpose
    gsl_matrix_complex_transpose(mixm);
    gsl_matrix_complex_transpose(mixm2);
}





/****pointers updating****/

void MassMatrixTwoMix::exportResults()
{
    int c = 0;
    gsl_complex tmp, tmp2;
    double resu;

    for(int i = 0 ; i < dim ; i++) //row number
    {
        for(int j = 0; j < dim ; j++) //column number
        {
            tmp = gsl_matrix_complex_get(mixm, i, j);
            tmp2 = gsl_matrix_complex_get(mixm2, i, j);

            (*remixpointer[c]) = tmp.dat[0];
            (*immixpointer[c]) = tmp.dat[1];

            (*remixpointer2[c]) = tmp2.dat[0];
            (*immixpointer2[c]) = tmp2.dat[1];

            c++;
        }

        resu = gsl_vector_get(this->eigv, i);
        if(abs(resu) < pow(10., -5))
            resu = 0;
        if(resu < 0)
            cout << "WARNING: The mass squared of particle ID ?? is negative, sqrt(absolute value) is printed" << endl;

        if(mpointer[i] != NULL)
            (*mpointer[i]) = sqrt(abs(resu));
    }
}


/*

//Prints the real and imaginary parts of the mixing matrix
void MassMatrixTwoMix::printMixm(ofstream& out)
{
    out << "Block " << remixname << "    " << "# " << comment << endl;
    printRealMatrixList(mixm, dim, dim, out);
    out << "Block " << immixname << "    " << "# " << comment << endl;
    printImagMatrixList(mixm, dim, dim, out);
    out << "Block " << remixname2 << "    " << "# " << comment << endl;
    printRealMatrixList(mixm2, dim, dim, out);
    out << "Block " << immixname2 << "    " << "# " << comment << endl;
    printImagMatrixList(mixm2, dim, dim, out);
}


*/
