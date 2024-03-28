#ifndef CPAR_HPP
#define CPAR_HPP

#include "headers.hpp"


double realpart(double);
double imagpart(double);
double imagpart(complex<double>);
double realpart(complex<double>);


class RPar {
    private:
        //value
        double value;

    public:
        /***constructors****/

        RPar(double v){setValue(v);}

        /****setter****/

        void setValue(double v)  {value = v;}

        /****getters****/

        double getValue() {return value;}
};

class CPar {
    private:
        //value
        complex<double> value;

    public:
        /***constructors****/

        CPar(complex<double> v){setValue(v);}

        /****setter****/

        void setValue(complex<double> v){value = v;}

        /****getter****/

        complex<double> getValue(){return value;}
};


#endif

