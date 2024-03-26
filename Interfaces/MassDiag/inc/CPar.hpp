#ifndef CPAR_HPP
#define CPAR_HPP

#include "headers.hpp"
#include "SLHABlock.hpp"
#include "ParSLHA.hpp"
#include "Par.hpp"

class CPar: public Par {
    private:
        //value
        complex<double> value;
        
    public:
    
        /***constructors****/
        
        //default constructor
        CPar();
        //provide the value
        CPar(complex<double>);
        
        /****setter****/
        
        //set the value
        void setValue(complex<double> v)  {value = v;}

        /****getters****/

        //get the value
        complex<double> getValue() {return value;}
        
        //updates the value that is stored in the block
        virtual void updateOutBlockValue();        
};


#endif

