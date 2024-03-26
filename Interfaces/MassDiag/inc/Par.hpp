#ifndef PAR_HPP
#define PAR_HPP

#include "headers.hpp"
#include "SLHABlock.hpp"
#include "ParSLHA.hpp"

//type to store parameters
class Par
{
    protected:
        //position in the vector
        int n;
        //set the position in the vector
        void setN(int i) {n = i;}
        //get the position in the vector
        int getN() {return n;}
        
        /****parameters****/
 
        //the comment, to be printed in the parametercard
        string comment;
        //the input block the parameter belongs to
        SLHABlock* inblock;
        //the input index in the block
        vector<int> inpos;
        //the output block the parameter belongs to
        SLHABlock* outblock;
        //the output index in the block
        vector<int> outpos;
    
    public: 
        /****parameter storage****/

        //static vector to store all the parameters
        static vector<Par*> Pars;  
        //debug flag
        static bool debug;

        /***constructors****/

        //default constructor
        Par();        
        
        /****destructor****/
        
        ~Par();
        
        /****setter****/
        
        //set the comment
        void setComment(string c) {comment = c;}
        //set the in block
        void setInBlock(SLHABlock * b) {inblock = b;}
        //set the in position
        void setInPos(vector<int> p) {inpos = p;}
        void setInPos(int d, int* p)
        {
            for(int i = 0; i < d ; i++)
                inpos.push_back(*(p+i));
        }

        //set the out block
        void setOutBlock(SLHABlock * b) {outblock = b;}
        //set the out position
        void setOutPos(vector<int> p) {outpos = p;}
        void setOutPos(int d, int* p)
        {
            for(int i = 0; i < d ; i++)
                outpos.push_back(*(p+i));
        }
        
        /****getters****/
        
        //get the comment
        string getComment() {return comment;}
        //get the SLHABlock for input
        SLHABlock* getInBlock() {return inblock;}
        //get the position of the input
        vector<int> getInPos() {return inpos;}
        //get the SLHABlock for the output
        SLHABlock * getOutBlock() {return outblock;}
        //get the position of the output
        vector<int> getOutPos() {return outpos;}
        
        //updates the value that is stored in the block
        virtual void updateOutBlockValue()
        {cout << "Value " << comment << " was not updated in block "
        << outblock->getBlockName() << endl;}
        //updates all the output blocks
        static void updateOutBlocks()
        {
            if(debug)
            {
                printLog("    update parameters (Par.hpp line ");
                printLog(__LINE__);
                printLog(" function called from Parameters::printParameters)\n");
            }
        
            for(int i = 0; i < Pars.size(); i ++)
            {
                 if(Pars.at(i)->getOutBlock() != NULL)
                     Pars.at(i)->updateOutBlockValue();
            }
            
            if(debug)
            {
                printLog("    ... parameters updated (Par.hpp line ");
                printLog(__LINE__);
                printLog(")\n");
            }
        };
        

};

#endif
