#ifndef SLHABLOCK_HPP
#define SLHABLOCK_HPP

#include "headers.hpp"
#include "ParSLHA.hpp"
#include "Matrix.hpp"
#include "tools.hpp"

//class SLHABlock inheritating from class Matrix
class SLHABlock: public Matrix<ParSLHA*>
{
    private:
        //the name of the block
        string name;
        //short description of the block
        string comment;
        
    public:   
        //parameter to store the name of the inputfile
        static string inputfile;
        
        /*****constructors*****/
    
        //square SLHABlock with name, number of dimensions,
        //dimension and comment
        SLHABlock(string, int, int, string);
        
        //square SLHABlock with name, number of dimensions and dimension
        SLHABlock(string, int, int);
        //square SLHABlock with name, number of dimensions
        //and default dimension 100
		SLHABlock(string n, int s);
     
		/*****getters*****/
		 
		//get the name
		string getBlockName(){return name;}
		//get the comment
		string getBlockComment(){return comment;}
		
		//get the value of an element in the block
		double getElemValue(vector<int>);
		//get the description of an element in the block
		string getElemComment(vector<int>);
	
		/*****setters*****/
	   
		//set the name
		void setBlockName(string n){name = n;}
		//set the comment
		void setBlockComment(string c) {comment = c;}
		
		//set the value of an element of the block
		void setElemValue(vector<int>, double);
		//set the comment of an element of the block
		void setElemComment(vector<int>, string);
	
		/****printing****/
	
		//print the block in a file with demanded precision
		void printSLHABlock(ofstream&);
	
		/*****tools*****/
		
		//initialise the block with parametercard input
		void initialise(const string &);
		//function to read a line of the parametercard
		bool readLine(string);
};

#endif
