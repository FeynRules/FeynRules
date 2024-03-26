#ifndef MASSMATRIX_HPP
#define MASSMATRIX_HPP

#include "headers.hpp"
#include "Parameters.hpp"

class MassMatrix
{
    protected:

        /****tools to keep track of the position in the vector****/
        
        //position in the vector
        int p;
        //set the position in the vector
        void setP(int i) {p = i;}
        //get the position in the vector
        int getP() {return p;}

        /****parameters****/

        //name of the block
        string name;
        //name of the real part of the mixing matrix
        string remixname;
        //name of the imaginary part of the mixing matrix
        string immixname;
        //comment to be printed in de output
        string comment;
        //dimension of the matrix
        int dim;
        //pdg vector
        vector<int> pdg;
        //comments corresponding to the pdg
        vector<string> pdgcom;


    public:

        static vector<MassMatrix*> MassMatrices;
        static bool debug;

        /****parameters****/

        /*
        static instance of Parameters, contains all the parameters
        and mass matrix initialisation functions provided by FeynRules
        */
        
        static Parameters* par;
        //Mass matrix
        gsl_matrix_complex* massm;
        //Mixing matrix
        gsl_matrix_complex* mixm;
        //Eigenvalues
        gsl_vector* eigv;          
      
        /****constructors****/
        
        //Constructor
        MassMatrix(string, string, string, int, int*, string*, string);
        //constructor without comment
        MassMatrix(string, string, string, int, int*, string*);
        //function for initalisation of the parameters
        void init(string, string, string, int, int*, string*, string);
        //Initialisation of the mass matrix
        virtual void initMassMatrix();
        
        
        /****destructor****/
        
        ~MassMatrix();
        
        /****setters****/

        //set the name of the MassMatrix
        void setName(string n){name = n;}
        //set the name of the real part of the mixing matrix
        void setReMixName(string rm){remixname = rm;}
        //set the name of the imaginary part of the mixing matrix
        void setImMixName(string im){immixname = im;}
        //set the comment
        void setComment(string c){comment = c;}
        //set the pdgs
        void setPdgs(int*, int);
        //set pdg comments
        void setPdgComments(string*, int);
        //set the dimension
        void setDim(int d){dim = d;}
        
        
        /****getters****/

        //get the name of the MassMatrix
        string getName(){return name;}       
        //get the name of real part of the mixing matrix
        string getReMixName(){return remixname;}
        //get the name of imaginary part of the mixing matrix
        string getImMixName(){return immixname;}
        //get the comment
        string getComment(){return comment;}
        //get the pdgs
        vector<int> getPdgs(){return pdg;}
        //get the pdg comments
        vector<string> getPdgComments(){return pdgcom;}
        //get the dimension
        int getDim(){return dim;}
        
        
        /****diagonalization****/
        
        //diagonalizes the mass matrix and prints the result
        virtual void diagMatrix();   
        
        /****printing****/

        //Prints real and imag parts of the mass matrix
        virtual void printMassm(ofstream&);
        //Prints the eigenvalues
        void printEigv(ofstream&);
        //Prints real and imag part of the mixing matrix
        virtual void printMixm(ofstream&);     
        //generates the output of one matrix
        void generateOutput(const string &, int);
        
        //prints the real and imag parts of the matrix elements as
        //a list given the number of rows and columns
        void printRealMatrixList(gsl_matrix_complex*, int, int, ofstream &);
        void printImagMatrixList(gsl_matrix_complex*, int, int, ofstream &);
        //prints a list of the indices and sqrt(value) of a vector
        virtual void printPdgMass(ofstream&); 
        
        //main routine, checks the input given at the command line
        //diagonalizes massmatrices and generates the output
		static void generateAll(int d, char* n[], const string & file)
		{
		     ofstream out(file.c_str());
		     out.precision(6);
		     
		     //create a vector containing the matrices to diagonalize
		     vector<MassMatrix*> m;
    		 
    		 //fill the vector
             if(d >= 1)
             {
                 //get all the matrices in the list
                 for(int i = 0; i < d ; i++)
                     m.push_back(getMassMatrix(*(n+i)));
                     
                 //generate the output for these matrices
                 generateAllOutput(m, out);
             }
             else
             {
                 //generate all the output
                 generateAllOutput(MassMatrices, out);
             }
        }
	
        //Prints the output of all MassMatrices
		static void generateAllOutput(vector<MassMatrix*> m, ofstream & out)
		{
            if(debug)
		        printLog("\n Initialize all the massmatrices (MassMatrix.hpp line ",
		        __LINE__, ")\n");
            
            initAll(m);
            
            if(debug)
		        printLog("\n Initialize all the massmatrices (MassMatrix.hpp line ",
		        __LINE__, ")\n");
                ofstream massout("asperge.log", ios::app);
                printMassMatrices(m, massout);
                massout.close();

		    if(debug)
		        printLog("... initialization done\n\nDiagonalize the massmatrices (MassMatrix.hpp line ", __LINE__, ")\n");
            
			diagonalizeAll(m);
	        
	        if(debug)
	        {
	            printLog("... diagonalization done\n\nGenerating the output (MassMatrix.hpp line ", __LINE__, ")\n");
            }
			
			if(out)
			{
				printHeader(out);
				
				if(debug)
     		        printLog("  * header printed (MassMatrix.hpp line ",
			        __LINE__, ")\n");
				
				par->printParameters(out);
				
				if(debug)
			        printLog("  * parameters printed (MassMatrix.hpp line ",
			        __LINE__, ")\n");
			    
			    generateBlockMass(m, out);
				
				if(debug)
			        printLog("  * mass printed (MassMatrix.hpp line ",
			        __LINE__, ")\n");
			    
			    printAllMixing(m, out);
				
				if(debug)
			        printLog("  * mixings printed (MassMatrix.hpp line ",
			        __LINE__, ")\n");
			}
			else
				cout << "WARNING: could not open the mass matrix output file"
				<< endl; 
            
            if(debug) printLog("... output generated\n\n");
		}

        //initialize all the mass matrices
		static void initAll(vector<MassMatrix*> m)
		{
		    for(int i = 0; i < m.size() ; i++)
		    {
		        if(debug) printLog("  * ", m.at(i)->getName(), " done\n");
		        m.at(i)->initMassMatrix();
		    }
		}
        
        //diagonalizes all the matrices
		static void diagonalizeAll(vector<MassMatrix*> m)
		{
		    for(int i = 0; i < m.size() ; i++)
		    {
		        if(debug) printLog("  * ", m.at(i)->getName(), " done\n");
		        m.at(i)->diagMatrix();
		    }
		}
  		
  		//prints the complete block mass
		static void generateBlockMass(vector<MassMatrix*> m, ofstream& out)
		{
		    out << "Block MASS " << '\t' << endl;
		    out << "# pdg code"	<< '\t' << "mass" << '\t' << "particle"<<  endl;
		    for(int i = 0; i < m.size() ; i ++)
		        m.at(i)->printPdgMass(out);
		}
	
		//prints all the mixing matrices, both the real and imag part
		static void printAllMixing(vector<MassMatrix*> m, ofstream& out)
		{
		    for(int i = 0; i < m.size() ; i++)
		        m.at(i)->printMixm(out);		    
		}
        
        //get the MassMatrix corresponding to a certain name
        static MassMatrix* getMassMatrix(string n)
		{
		    int i = 0;
		    bool found = false;
		    while( !found && i < MassMatrices.size())
		    {
			    if(n == MassMatrices.at(i)->getName() || n == MassMatrices.at(i)->getReMixName() || n == MassMatrices.at(i)->getImMixName())
			    {
				    found = true;
				    return MassMatrices.at(i);
			    }
			    i++;
			}
		    if(!found)
		    {
		       cout << "ERROR: Mass Matrix " << n << 
		       " does not exist." << endl;
		       exit(EXIT_FAILURE);
		    }
                    return NULL;
		}
		
		//prints the mass matrices a file 
        static void printMassMatrices(vector<MassMatrix*> m, ofstream& out)
        {
		    if(out)
                for(int i = 0; i < m.size(); i++)
            	    m.at(i)->printMassm(out);
        }        
};


#endif
