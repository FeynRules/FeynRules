#ifndef RPAR_HPP
#define RPAR_HPP

#include "headers.hpp"
#include "SLHABlock.hpp"
#include "ParSLHA.hpp"
#include "Par.hpp"

class RPar: public Par {
    private:
        //value
        double value;
        
    public:
    
        /***constructors****/
        
        //default constructor
        RPar();
        //provide the value
        RPar(double);
        //provide the value, outblock, outindex, outcomment
        RPar(double, SLHABlock*, int*, string);
        //provide the value, outblock, outindex
        RPar(double, SLHABlock*, int*);
        
        //provide the inblock and index
        void init(SLHABlock*, int*);
        
        RPar(SLHABlock*, int); //one index
        RPar(SLHABlock*, int, int); // two indices
        RPar(SLHABlock*, int, int, int); // three indices
        
        //provide the inblock and index, the outblock and index
        void init(SLHABlock*, int*, SLHABlock*, int*);
        
        RPar(SLHABlock*, int, SLHABlock*, int);
        RPar(SLHABlock*, int, int, SLHABlock*, int);
        RPar(SLHABlock*, int, int, int, SLHABlock*, int);
        
        RPar(SLHABlock*, int, SLHABlock*, int, int);
        RPar(SLHABlock*, int, int, SLHABlock*, int, int);
        RPar(SLHABlock*, int, int, int, SLHABlock*, int, int);
        
        RPar(SLHABlock*, int, SLHABlock*, int, int, int);        
        RPar(SLHABlock*, int, int, SLHABlock*, int, int, int);
        RPar(SLHABlock*, int, int, int, SLHABlock*, int, int, int);
                        
        //provide the inblock and index, the outblock and index and the comment
        void init(SLHABlock*, int*, SLHABlock*, int*, string);

        RPar(SLHABlock*, int, SLHABlock*, int, string);
        RPar(SLHABlock*, int, int, SLHABlock*, int, string);
        RPar(SLHABlock*, int, int, int, SLHABlock*, int, string);
        
        RPar(SLHABlock*, int, SLHABlock*, int, int, string);
        RPar(SLHABlock*, int, int, SLHABlock*, int, int, string);
        RPar(SLHABlock*, int, int, int, SLHABlock*, int, int, string);
        
        RPar(SLHABlock*, int, SLHABlock*, int, int, int, string); 
        RPar(SLHABlock*, int, int, SLHABlock*, int, int, int, string);
        RPar(SLHABlock*, int, int, int, SLHABlock*, int, int, int, string);
        
        /****setter****/
        
        //set the value
        void setValue(double v)  {value = v;}

        /****getters****/

        //get the value
        double getValue() {return value;}
        
        //updates the value that is stored in the block
        virtual void updateOutBlockValue();
};


#endif
