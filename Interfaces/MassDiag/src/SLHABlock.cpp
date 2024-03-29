#include "SLHABlock.hpp"

string SLHABlock::inputfile;

/****constructors****/

//square SLHABlock with name n, s number of dimensions, dimension d
//and comment c
SLHABlock::SLHABlock(string n, int s, int d, string c):Matrix<ParSLHA*>(s, d)
{
	name = n;
	comment = c;
	//initialise the values using the input file
	this->initialise(inputfile);
}

//square SLHABlock with name n, s number of dimensions and dimension d
SLHABlock::SLHABlock(string n, int s, int d):Matrix<ParSLHA*>(s, d)
{
  	name = n;
  	comment = "";
	//initialise the values using the input file
	this->initialise(inputfile);
}

//square SLHABlock with name n, s number of dimensions
//and default dimension 100
SLHABlock::SLHABlock(string n, int s):Matrix<ParSLHA*>(s)
{
	name = n;
	comment = "";
	//initialise the values using the input file
	this->initialise(inputfile);
}

/****getters****/

//get the value of an element of the block
double SLHABlock::getElemValue(vector<int> p)
{
    return getElement(p)->getValue();
}

//get the description of an element of the block
string SLHABlock::getElemComment(vector<int> p)
{
    return getElement(p)->getComment();
}

/*****setters*****/

//set the value of an element of the block
void SLHABlock::setElemValue(vector<int> p, double v)
{
    getElement(p)->setValue(v);
}
//set the description of an element of the block
void SLHABlock::setElemComment(vector<int> p, string c)
{
    getElement(p)->setComment(c);
}

/*****printing*****/

//prints the block in SLHA format in 'file'
void SLHABlock::printSLHABlock(ofstream& out)
{  
    if(out)
    {
        //print the blockname and comment
        out << "Block " << getBlockName() << "    "
        << getBlockComment() << endl;
	    
	    //make a vector to store the indices
        vector<int> pos (getNrInd());
        //loop over al the elements of the matrix
        for(int i = 0; i < getTotDim() ; i ++)
		{
		    //calculate and store the indices
			pos = getInd(i);
			//print only if the element is not empty
			if(getElement(i)!=NULL)
			{
			    //print all the indices
			    for(int j = 0; j < getNrInd() ; j ++)
			    {
				    printInt(pos.at(j), 7, out);
				    out << "    ";
				}
                //print the value and the comment
			    out << scientific << getElement(i)->getValue()
			    << "    " << "# " <<
			    getElement(i)->getComment() << endl;
			}
		}
    }
    else
        cout << "WARNING: could not open the SLHA outputfile" << endl;
}

/*****tools*****/

//initialise the block starting from the parametercard 'file'
void SLHABlock::initialise(const string &file)
{
     //opening the file
     ifstream in(file.c_str()); 
     
      if(in) //if file exists
     {
        //boolean to keep track of whether the block has been found
        bool found = false;
        //declare a string to store the lines of the file
        string line;
        //store first line of file in 'line'
        getline(in, line,'\n');
        //loop over the lines in the file
        while(!found && !in.eof())
        {
           if(line.length()==0) //go to next line if line is empty
              getline(in,line,'\n'); 
           else
           {
              //check if line starts with 'b' or 'B'
              if(line[0] == 'b' || line[0] == 'B')
              {
                 //extract the blockname
                 string n = BlockName(line);
                 if(name == ToUpper(n))
                 {   
                    //if this matches the name of the block
                    found = true;
                    bool read = true;
                    comment = getComment(line);
                    //as long as the lines are of the format
                    //int ... int   double   #comment
                    while(read)
                    {
                       //go to the next line
                       getline(in, line, '\n');
                       //sets the value of the matrix element
                       read = readLine(line);
                    }
                 }
                 else
                 {
                    //if the name doesn't match, go to the next line
                    getline(in, line, '\n');
                 }
              }
              else
              {
                 //if the line doesn't start with 'b' or 'B',
                 //go to the next line
                 getline(in, line, '\n');
              }
           }
        }
        if(!found)
           cout << "WARNING: could not initialise block " << name << endl;
   }
   else
      cout << "ERROR: could not open the input file"
      <<  endl;
    // closing the file
    in.close();
}

/*
This function reads a string 'line' of the format
int ... int   double   #comment
and stores it in the appropriate position in this SLHABLock
*/

bool SLHABlock::readLine(string line)
{
    //create an array of with size the number of indices
    int position[getNrInd()];
   
    //counter to keep track of the number of indices already read
    int counter = 0;
    //boolean to indicate whether the line has the expected format
    bool right = true;
   
    //position of the char under consideration in the string
    int i = 0;
    while(counter < getNrInd() && right)
    {
        //while we don't have all the indices
        //and if the line has the expected format
        if(i < int(line.length()))
        {
            //if the index doesn't go beyond the line
            if(line[i] != ' ')
            {
                //and if the char is not empty
                if(isnum(line[i]))
                {
                    //and if the char is a number
                    //store that number in 'number'
                    int number = line[i] - '0';
                    //as long as the following chars are numbers
                    while(isnum(line[i+1]))
                    {
                        //complete the index
                        number = number*10 + (line[i+1] - '0');
                        //go to the next char (a number)
                        i++;
                    }
                    //store this index
                    position[counter] = number;
                    //go to the next index
                    counter++;
                    //go to the next char (no number)
                    i++;
                }
                else
                {
                    //false if the next index is not of the expected format
                    right = false;
                }
           }
           else
           {
               //if char is empty, go the the next one
               i++;
           }
        }
        else
        {
            //false if the index goes beyond the range of the string
            right = false;
        }
    }
   
    //if everything went well
    if(right)
    {
        //find the position of '#' in the string
        int end = line.find("#");
        try
        {
            //create a parameter
            //convert the string in between the last index and '#' to a double
            //extract the comment and assign them to parameter
            ParSLHA* element = new ParSLHA(convertToDouble
            (line.substr(i, end-i)), line.substr(end + 2, line.length()));

            //set the element of the SLHABlock
            setElement(position, element);
        }
        //warning if the conversion didn't work well
        catch (BadConversion)
        {
            cout << "WARNING: could not convert to double in line: "
            << '\n'   << line  << '\n'
            << "initialisation of block "
            << getBlockName()
            << " terminated." << endl;
            right = false;
        }
    }
    return right;
}

