#include "MassMatrixTwoMix.hpp"

//Constructor of a (d x d)-MassMatrix with name n
MassMatrixTwoMix::MassMatrixTwoMix(string n, string rm, string im, string rm2, string im2, int d, int* pd, string* pc, string c)
:MassMatrix(n, rm, im, d, pd, pc, c)
{ 
    if(debug)
    {
        printLog("Constructing MassMatrixTwoMix ", n, " (MassMatrixTwoMix.cpp line ");
        printLog("", __LINE__, ")\n");
    }
    //initialisation of names of the 2nd mixing matrix
    setReMixName2(rm2);
    setImMixName2(im2);
    
    //assign memory
    massm2 = gsl_matrix_complex_alloc(d, d);
    mixm2 = gsl_matrix_complex_alloc(d, d);
    eigv2 = gsl_vector_alloc(d);
    
    if(debug) printLog("... ", n, " constructed\n\n");
    
}

//Constructor of a (d x d)-MassMatrix with name n
MassMatrixTwoMix::MassMatrixTwoMix(string n, string rm, string im, string rm2, string im2, int d, int* pd, string* pc):MassMatrix(n, rm, im, d, pd, pc, "")
{   
    if(debug)
    {
        printLog("Constructing MassMatrixTwoMix ", n, " (MassMatrixTwoMix.cpp line ");
        printLog("", __LINE__, ")\n");
    }
    //initialisation of names of the 2nd mixing matrix
    setReMixName2(rm2);
    setImMixName2(im2);
    
    //assign memory
    massm2 = gsl_matrix_complex_alloc(d, d);
    mixm2 = gsl_matrix_complex_alloc(d, d);
    eigv2 = gsl_vector_alloc(d);
    
    if(debug) printLog("... ", n, " constructed\n\n");
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
