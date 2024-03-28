#include "MassMatrix.hpp"

//The static variables
Parameters* MassMatrix::par;
vector<MassMatrix*> MassMatrix::MassMatrices;

/****constructors****/

//Constructor of a (d x d)-MassMatrix with name n
MassMatrix::MassMatrix(string n, double** mp, double** rm, double** im, int d)
{
    init(n, mp, rm, im, d);
}

//function for initialisation of the parameters
void MassMatrix::init(string n, double** mp, double** rm, double** im, int d)
{
    //initialisation of name, dimension and comment
    setName(n);
    setMassPointer(mp);
    setReMixPointer(rm);
    setImMixPointer(im);
    setDim(d);

    //assign memory
    massm = gsl_matrix_complex_alloc(d, d);
    mixm = gsl_matrix_complex_alloc(d, d);
    eigv = gsl_vector_alloc(d);

    //store which number the massmatrix has in the list
    p = MassMatrices.size();

    //add to the vector of massmatrices
    MassMatrices.push_back(this);
}

//
void MassMatrix::initMassMatrix()
{
	//initialisation of the matrix
    (par->*(par->massInit[name]))(massm);
}

//Destructor
MassMatrix::~MassMatrix()
{
    gsl_matrix_complex_free(massm);
    gsl_matrix_complex_free(mixm);
    gsl_vector_free(eigv);
    //remove from the vector (erase the pth element)
    MassMatrices.erase(MassMatrices.begin() + p);
    //adjust the indices of the other elements of the class
    for(int i = p; i < MassMatrices.size(); i++)
        MassMatrices.at(i)->setP(i);

}


/****diagonalization****/

//Diagonalize the matrix
void MassMatrix::diagMatrix()
{
    //eigensystem
    gsl_eigen_hermv_workspace *w = gsl_eigen_hermv_alloc(dim);
    gsl_eigen_hermv(massm, eigv, mixm, w);
    gsl_eigen_hermv_sort(eigv, mixm, GSL_EIGEN_SORT_ABS_ASC);
    gsl_eigen_hermv_free(w);

    //Since GSL put the eigenvectors as columns, we need to transpose
    gsl_matrix_complex_transpose(mixm);
}


/****pointers updating****/

void MassMatrix::exportResults()
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
        if(abs(resu) < pow(10., -5))
            resu = 0;
        if(resu < 0)
            cout << "WARNING: The mass squared of particle ID  ?? is negative, sqrt(absolute value) is printed" << endl;

        if(mpointer[i] != NULL)
            (*mpointer[i]) = sqrt(abs(resu));
    }
}

