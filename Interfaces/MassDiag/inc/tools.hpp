#ifndef TOOLS_H
#define TOOLS_H

#include "headers.hpp" 

/*****complex numer handling*****/

double realpart(double );// {return x;}
double imagpart(double );//x) {return 0;}
double imagpart(complex<double>);
double realpart(complex<double>);

/*****matrices*****/

//print a gsl_matrix of a certain size with given precision
void PrintMatrix(gsl_matrix *, const unsigned int &, const int &);

//multiply 2 given matrices of a given size and store in a 3rd one
void MatMul(gsl_matrix *, gsl_matrix *, gsl_matrix *, const unsigned int &);


/*****stringhandling*****/

//convert a string to the capital string and removes extra spaces
string ToUpper(string &);

//get the blockname out of the relevant line in the parametercard
string BlockName(string &);

//get the part after #
string getComment(string &);

//check whether a char is a number
bool isnum(const char &);

//convert an int in a string
string convIntToString(int);

//check if a string is asking for help
bool isHelp(string);

//remove the spaces in the beginning or end of the given string
string removeSpaces(string &);

//conversion of string to double
class BadConversion : public std::runtime_error
{
   public:
      BadConversion(std::string const& s):std::runtime_error(s){ }
};
 
inline double convertToDouble(std::string const& s)
{
   std::istringstream i(s);
   double x;
      if (!(i >> x))
         throw BadConversion("convertToDouble(\"" + s + "\")");
      return x;
}

//get the number of digits of an int
int numDigits(int);

/*****printing*****/
//print the header of the parametercard
void printHeader(ofstream&);
//print help for ASperGe
void printHelp();
//print an integer
void printInt(int, int, ofstream&);


/*****logfile printing*****/
template <typename T>
void printLog(T t)
{
    //if the name of the log file is changed, change it in all the functions
    //as well as in MassMatrix.hpp
    ofstream out("asperge.log", ios::app);
    out << t ;
    out.close();
}

template<typename T>
void printLog(string s1, T t, string s2)
{
    //if the name of the log file is changed, change it in all the functions
    //as well as in MassMatrix.hpp
    ofstream out("asperge.log", ios::app);
    out << s1 << t << s2 ;
    out.close();
}

/***** print the information of a Par *****/
template<typename T>
void printLogPar(string s1, string s2, string s3, T n, string s4, string s5, string s6, vector<int> v)
{
    //if the name of the log file is changed, change it in all the functions
    //as well as in MassMatrix.hpp
    ofstream out("asperge.log", ios::app);
    out << s1 << s2 << s3 << n << s4 << s5 << s6 ;
    for(int i = 0 ; i < v.size(); i++)
        out << v.at(i) << " ";
    out << endl;
    out.close();    
}

//remove the logfile
void removeLog();

//check if a file exists
bool fexists(const char *);

//check if the input is correct
void checkArguments(int, char**);



#endif
