#include "tools.hpp"

//prints an integer occupying 'size' characters
void printInt(int number, int size, ofstream& file)
{
    for(int i =0; i < size-numDigits(number); i++)
        file << " " ;
    file << number;
}

//gets the number of digits of an integer
int numDigits(int number)
{
    int digits = 0;
    if (number < 0) digits = 1; // remove this line if '-' counts as a digit
    while (abs(number) >= 1) {
        number /= 10;
        digits++;
    }
    return digits;
}

/*****complex numer handling*****/
double realpart(double x)
{
    return x;
}

double imagpart(double x)
{
    return 0.;
}

double realpart(complex<double> x)
{
    return real(x);
}

double imagpart(complex<double> x)
{
    return imag(x);
}


/*****matrices*****/

//print a gsl_matrix of size 'size' with precision 'prec'
void PrintMatrix(gsl_matrix *M, const unsigned int &size, const int &prec)
{
  cout << fixed ;
  cout.precision(prec);
  for (unsigned int i=0; i<size; i++)
  {
    for (unsigned int j=0; j<size; j++)
    {
      string blank="\t";
      double mm=gsl_matrix_get(M,i,j);
           if( (mm>0.) && (mm<999.) )     blank="   \t";
      else if( (mm<0.) && (mm>-999.) )    blank="  \t";
      else if( (mm>0.) && (mm<9999.) )    blank="  \t";
      else if( (mm<0.) && (mm>-9999.) )   blank=" \t";
      else if( (mm>0.) && (mm<99999.) )   blank=" \t";
      else if( (mm<0.) && (mm>-99999.) )  blank="\t";
      cout << gsl_matrix_get(M,i,j) << blank;
    }
    cout << endl;
  }
}

//multiply A and B and store in AB, given the size
void MatMul
(gsl_matrix *A, gsl_matrix *B, gsl_matrix *AB, const unsigned int &size)
{
   for (unsigned int i=0; i<size; i++)
   for (unsigned int j=0; j<size; j++)
   {
      double res=0.;
      for (unsigned int k=0; k<size; k++) 
         res += gsl_matrix_get(A,i,k)*gsl_matrix_get(B,k,j);      
      gsl_matrix_set(AB,i,j,res);
   }
}

/****stringhandling*****/

//convert a string to the same string in capitals and removes extra spaces
string ToUpper(string &line)
{
   string temp=line;
   bool flag=false;
   int count =0;
   
   // replace every letter with the capital
   for(int i=0; i<int(line.length()); i++) 
   {
      if( (line[i]>='a') && (line[i]<='z') && !flag) 
         temp[i] = (char) (int(line[i])-32);
      else if( (line[i]=='#') || flag )
      {   
         flag=true;
         temp.erase(i-count,1);
         count++;
      }
      else
         temp[i] = line[i];
   }
   
   // Remove extra spaces
   bool space=true;
   while(space)
   {
      if(temp[temp.length()-1]==' ')
         temp.erase(temp.length()-1,1);
      else
         space=false;
   }
   space=true;
   while(space)
   {
      if(temp[0]==' ')
         temp.erase(0,1);
      else
         space=false;
   }

   // Result
   return temp;
}

//get the blockname out of the relevant line in the parametercard
string BlockName(string &line)
{
   string name=ToUpper(line);

   // Remove the word "block"
   if(name.find("BLOCK ",0)!=string::npos) 
      name.erase(name.find("BLOCK ",0),6);
   else if(name.find("LOCK ",0)!=string::npos) 
      name.erase(name.find("LOCK ",0),5);
   
   // Remove remaining spaces
   bool space=true;
   while(space)
   {
      if(name[name.length()-1]==' ')
         name.erase(name.length()-1,1);
      else
         space=false;
   }
   space=true;
   while(space)
   {
      if(name[0]==' ')
         name.erase(0,1);
      else
         space=false;
   }
   return name;
}

//returns the part after #
string getComment(string & line)
{
    string comment;
    size_t found = line.find('#', 0);
    if (found!=string::npos)
        comment = line.substr(found, line.length());
    return comment;
}

//check whether a char is a number
bool isnum(const char &test)
{
   if( (test=='0') || (test =='1') || (test =='2') || (test =='3') || 
       (test=='4') || (test =='5') || (test =='6') || (test =='7') || 
       (test=='8') || (test =='9') ) 
       { return true; }
   else{ return false;}
}

//convert an int in a string
string convIntToString(int number)
{
   stringstream convert; // stringstream used for the conversion
   convert << number; //add the value of Number to the characters in the stream
   return convert.str();
}

//remove the spaces in the beginning or end of the given string
string removeSpaces(string & line)
{
   bool space=true;
   while(space)
   {
      if(line[line.length()-1]==' ')
         line.erase(line.length()-1,1);
      else
         space=false;
   }
   space=true;
   while(space)
   {
      if(line[0]==' ')
         line.erase(0,1);
      else
         space=false;
   }
   
   // Result
   return line;
}

/*****printing*****/
void printHeader(ofstream& out)
{
    out <<
"#############################################################################"
    << '\n' <<
"##  Version:     0.0.01                                                    ##"
    << '\n' <<
"##  Author:  Adam ALLOUL (IPHC / U. Strasbourg)                            ##"
    << '\n' <<
"##           Karen De Causmaecker (VUB )                                   ##"
    << '\n' <<
"##  Contact: adam.alloul@iphc.cnrs.fr                                      ##"
    << '\n' <<
"##           Karen.De.Causmaecker@vub.ac.be                                ##"
    << '\n' <<
"##                                                                         ##"
    << '\n' <<
"#############################################################################"
    << '\n' << "#" << '\n'
    << "#" << endl;
}

//print instruction for use of the program
void printHelp()
{
	cout << '\n' << "ASperGe looks for:" << endl;
	cout << "    1. An input file" << endl;
	cout << "    2. An output file" << endl;
	cout << "    3. Optionally the names of the mass matrices to diagonalize\n" << endl;
		
	cout << "EXAMPLE 1: ./ASperGe infile outfile" << endl;
	cout << "Diagonalizes all the mass matrices in the model," << endl;
	cout << "relying on \'infile\' and writing the output to \'outfile\'" << endl;
	cout << "\'infile\'(\'outfile\') can be:" << endl;
	cout << "   1. for example input/externals.dat (output/parametercard.dat)" << endl;
	cout << "   2. full path to the input (output) file\n" << endl;

    cout << "EXAMPLE 2: ./ASperGe infile outfile m1 m2" << endl;
	cout << "Diagonalizes only the mass matrices m1 and m2\n" << endl;
}

//compare with help statements
bool isHelp(string help)
{
    string h = ToUpper(help);
    if(h ==(string)("HELP") || h ==(string)("MAN") || h == (string)("TUTORIAL"))
        return true;
    else
        return false;
}

//remove the log file
void removeLog()
{
    remove("asperge.log");
}

//check if a file exists
bool fexists(const char *filename)
{
  ifstream ifile(filename);
  return ifile;
}

//check if the input is correct
void checkArguments(int argc, char* argv[])
{
    switch(argc)
    {
        case 1:
            cout << "Please provide an in- and output file." << endl;
            cout << "Type \'./ASperGe help\' for instructions." << endl;
            exit(EXIT_FAILURE);
            break;
        case 2:
            if(isHelp(*(argv+1)))
            {
                printHelp();
                exit(EXIT_FAILURE);
            }
            else
            {
                cout << "Arguments incorrect,";
                cout << " type \'./ASperGe help\' for instructions." << endl;
                exit(EXIT_FAILURE);
            }
            break;
        default:
            if(!fexists(*(argv+1)))
            {
                cout << "Please provide a valid inputfile." << endl;
                cout << "Type \'./ASperGe help\' for instructions." << endl;
                exit(EXIT_FAILURE);
            }
    }
}
