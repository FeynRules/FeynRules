#ifndef MATRIX_H
#define MATRIX_H

#include "headers.hpp"
#include "tools.hpp"

template <class T>
class Matrix
{
    protected:
        //the number of indices
        int nrInd;
        //vector of the ranch of each index
        vector<int> dim;
        //total number of elements
        int totDim;
        //vector containing all the elements
        vector<T> matrix;
        
    public:
        /*****Constructors*****/
     
        //generic matrix - (number of indices, array with number of dimensions)
        Matrix(int, int[]);
        //square matrix - (number of indices, dimension)
        Matrix(int, int);
        //square matrix - (number of indices) - default dimension 100
        Matrix(int);
    
        /*****getters*****/
    
        //returns the total number of indices
        int getNrInd(){return nrInd;}
        //returns the total number of elements
        int getTotDim(){return totDim;}
        //returns the dimension in i'th direction
        int getDim(int i){return dim.at(i);} 
        //get the i'th element in the vector of elements
        T getElement(int i) { return matrix.at(i);}
        //get the value of the matrix element at position int[]
        T getElement(int[]);
        //get the value of the matrix element at position vector<int>
        T getElement(vector<int>);
        //get the position indices
        vector<int> getInd(int);
    
        /*****setters*****/
    
        //set the number of indices
        void setNrInd(int n){nrInd = n;} 
        //set the total number of elements
        void setTotDim(int t){totDim = t;}
        //set the value of a matrix element at position determined by int[]    
        void setElement(int*, T);
    
        /*****tools*****/
    
        //add a dimension
        void addDim(int i) {dim.push_back(i);}
        //get the total number of elements
        int getDim(int[]);
        //get the total number of elements
        int getDim(vector<int>);
};
//template class, all the implementations should be given in the same file too

/*****constructors*****/

//Generic matrix with 'size' number of indices and dimensions 'dim'
template <class T> 
Matrix<T>::Matrix(int size, int d[])
{
    nrInd = size; //initialise the number of indices
    for(int i = 0; i < size; i++)
        addDim(d[i]); //add the dimension
    //calculate the total number of elements
    totDim = getDim(d);
    //resize the vector to the appropriate size
    matrix.resize(totDim);
}

//square matrix with 'size' indices and dimension 'sqdim'
template <class T> 
Matrix<T>::Matrix(int size, int sqdim)
{
    nrInd = size; //initialise the number of indices
    for(int i = 0; i < size; i++)
        addDim(sqdim); //add the dimension
    //calculate the total number of elements
    totDim = getDim(dim);
    //resize the matrix to the appropriate size
    matrix.resize(totDim);
}   

//square matrix with 'size' indices and default dimension 100
template <class T> 
Matrix<T>::Matrix(int size)
{
    nrInd = size; //initialise the number of indices
    for(int i = 0; i < size; i++)
        this->addDim(100); //add the dimension with default size
    //calculate the total number of elements
    totDim = getDim(dim);
    //resize the matrix to the appropriate size
    matrix.resize(totDim);
}

/*****getters*****/

//get the element of the matrix element at position 'pos'
template <class T> 
T Matrix<T>::getElement(int pos[])
{
    int index = pos[0];
    for(int i = 1; i < getNrInd(); i ++)
       index = (index-1)*dim[i] + pos[i];
    return matrix.at(index-1);
}

//get the element of the matrix element at position 'pos'
template <class T> 
T Matrix<T>::getElement(vector<int> pos)
{
    int index = pos.at(0);
    for(int i = 1; i < getNrInd(); i ++)
        index = (index-1)*dim[i] + pos.at(i);
    return matrix.at(index-1);
}

/*****setters*****/

//set the element of a matrix element at position determined by 'pos' to 'val'
template <class T> 
void Matrix<T>::setElement(int* pos, T val)
{
    int index = *pos;
    for(int i = 1; i < getNrInd(); i ++)
        index = (index-1)*dim[i] + *(pos+i);
    matrix.at(index-1) = val;
}

/*****tools*****/

//get the total numbers of elements starting from the array with dimensions
template <class T> 
int Matrix<T>::getDim(int d[])
{
    int j = 1;
    for(int i = 0 ; i < getNrInd(); i ++)
        j = j*d[i];
    return j;
}

//get the total number of elements starting from the vector with dimensions
template <class T> 
int Matrix<T>::getDim(vector<int> d)
{
    int j = 1;
    for(int i = 0 ; i < getNrInd(); i ++)
        j = j*d.at(i);
    return j;
}

//returns a vector of the indices corresponding to a certain index i
template <class T> 
vector<int> Matrix<T>::getInd(int i)
{
    //the number of indices
    int n = getNrInd();
    //create a vector with n elements
    vector<int> pos (n);
    //last element
    pos.at(n-1) = i % getDim(n-1)+1;
    int a = (i - pos.at(n-1) + 1)/getDim(n-1);
    //loop for the other indices
    for(int k = n - 2 ; k >= 0; k --)
    {
        pos.at(k) = a % getDim(k) + 1;
        a = (a - pos.at(k) + 1)/getDim(k);
    }
    return pos;
}

#endif
