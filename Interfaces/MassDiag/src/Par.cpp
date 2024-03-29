#include "Par.hpp"

//the static variable
vector<Par*> Par::Pars;
bool Par::debug = false;

/****constructors****/

//default constructor setting everything to zero
Par::Par()
{
   comment = "";
   inblock = NULL;
   outblock = NULL;
   n = Pars.size();
   Pars.push_back(this);
}

/****deconstructor****/
Par::~Par()
{
    delete inblock;
    delete outblock;
    //remove from the vector (erase the nth element)
    Pars.erase(Pars.begin() + n);
    //adjust the indices of the other elements of the class
    for(int i = n; i < Pars.size(); i++)
        Pars.at(i)->setN(i);
}


