#ifndef PARSLHA_HPP
#define PARSLHA_HPP

#include "headers.hpp"

//class to store parameters to be used in the SLHABlocks.
class ParSLHA
{
    private:
        /****parameters****/
    
        //the value
        double value;
        //the comment to be used in the parametercard
        string comment;

    public:        
        /****constructors****/
        
        //default constructor
        ParSLHA();
        //constructor
        ParSLHA(double v, string c);
        
        /****setters*****/
        
        //set the value
        void setValue(double v) {value = v;}
        //set the comment
        void setComment(string c) {comment = c;}
        
        /****getters****/
        
        //get the value
        double getValue() {return value;}
        //get the comment
        string getComment() {return comment;}
};

#endif
