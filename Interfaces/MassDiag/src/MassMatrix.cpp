#include "MassMatrix.hpp"

//The static variables
Parameters* MassMatrix::par = new Parameters();
vector<MassMatrix*> MassMatrix::MassMatrices;
bool MassMatrix::debug = false;

/****constructors****/

//Constructor of a (d x d)-MassMatrix with name n
MassMatrix::MassMatrix(string n, string rm, string im, int d, int* pd, string* pc, string c)
{ 
    init(n, rm, im, d, pd, pc, c);
}

//Constructor of a (d x d)-MassMatrix with name n
MassMatrix::MassMatrix(string n, string rm, string im, int d, int* pd, string* pc)
{   
    init(n, rm, im, d, pd, pc, "");
}

//function for initialisation of the parameters
void MassMatrix::init(string n, string rm, string im, int d, int* pd, string* pc, string c)
{
    if(debug)
    {
        printLog("Constructing MassMatrix ", n, " (MassMatrix.cpp line ");
        printLog("", __LINE__, ")\n");
    }
    //initialisation of name, dimension and comment
    setName(n);
    setReMixName(rm);
    setImMixName(im);
    setComment(c);
    setDim(d);
    setPdgs(pd, d);
    setPdgComments(pc, d);
    
    //assign memory
    massm = gsl_matrix_complex_alloc(d, d);
    mixm = gsl_matrix_complex_alloc(d, d);
    eigv = gsl_vector_alloc(d);

    //store which number the massmatrix has in the list
    p = MassMatrices.size();
    
    //add to the vector of massmatrices
    MassMatrices.push_back(this);
    
    if(debug) printLog("... ", n, " constructed\n\n");
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

/****setters****/

//set the pdgs
void MassMatrix::setPdgs(int* pd, int d)
{
    for(int i = 0; i < d; i++)
        pdg.push_back(*(pd+i));
}

//set the pdg comments
void MassMatrix::setPdgComments(string* pc, int d)
{
    for(int i = 0; i < d; i++)
        pdgcom.push_back(*(pc+i));
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

/****printing****/

//Prints the complete output of one MassMatrix
void MassMatrix::generateOutput(const string & file, int prec)
{
    ofstream out(file.c_str());
    out.precision(prec);
    if(out)
    {
        printHeader(out);
        par->printParameters(out);
        printEigv(out);
        printMixm(out);
    }
    else
        cout << "WARNING: could not open the mass matrix output file" << endl;
}

//Prints the real and imaginary parts of the matrix
void MassMatrix::printMassm(ofstream& out)
{
    out << "Mass Matrix squared " << name << " REAL " << "    " << "# " << comment << endl;
    printRealMatrixList(massm, dim, dim, out);
    out << "Mass Matrix squared " << name << " IMAG " << "    " << "# " << comment << endl;
    printImagMatrixList(massm, dim, dim, out);
}

//Prints the real and imaginary parts of the mixing matrix
void MassMatrix::printMixm(ofstream& out)
{
    out << "Block " << remixname << "    " << "# " << comment << endl;
    printRealMatrixList(mixm, dim, dim, out);
    out << "Block " << immixname << "    " << "# " << comment << endl;
    printImagMatrixList(mixm, dim, dim, out);
}


//Prints the eigenvalues
void MassMatrix::printEigv(ofstream& out)
{
    out << "Block MASS " << "    " << "# " << comment << endl;
    out << "# pdg code"	<< "    " << "mass" << "    " << "particle"<<  endl;
    printPdgMass(out);
}


//Prints the real part of a matrix m
//as a list with r rows and c columns to a given output file
void MassMatrix::printRealMatrixList(gsl_matrix_complex* m, int r, int c, ofstream& out)
{
    for(int i =0 ; i < r ; i++) //row number
    {
        for(int j = 0; j < c ; j++) //column number
        {
            printInt(i+1, 4, out);
            printInt(j+1, 4, out);
            out << "    " << GSL_REAL(gsl_matrix_complex_get(m, i, j));
            out << "    #" << endl;
        }
    }
}

//Prints the imaginary part of matrix m
//as a list with r rows and c columns to a given output file
void MassMatrix::printImagMatrixList(gsl_matrix_complex* m, int r, int c, ofstream& out)
{
    for(int i =0 ; i < r ; i++) //row number
    {
        for(int j = 0; j < c ; j++) //column number
        {
            printInt(i+1, 4, out);
            printInt(j+1, 4, out);
            out << "    " << GSL_IMAG(gsl_matrix_complex_get(m, i, j));
            out << "    #" << endl;
        }
    }
}

//prints the sqrt of the vector
void MassMatrix::printPdgMass(ofstream& out)
{
    gsl_vector* v = this->eigv;
    int d = dim;
    for(int i = 0; i < d; i++)
    {
        double resu = gsl_vector_get(v, i);
        if(abs(resu) < pow(10., -5))
        {
            printInt(pdg.at(i), 10, out);
            out << "    " << 0.000000e+00 
            << "    "  << "# " << pdgcom.at(i) << endl;
        }
        else
        {
            if(resu < 0)
                cout << "WARNING: The mass squared of particle ID "  << pdg.at(i) << " is negative, sqrt(absolute value) is printed" << endl;            
            printInt(pdg.at(i), 10, out);
            out << "    " << sqrt(abs(gsl_vector_get(v, i)))
            << "    "  << "# " << pdgcom.at(i) <<endl;
        }
    }
}
