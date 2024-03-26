#include "CPar.hpp"

CPar::CPar()
{
    new Par();
    value = 0.;
}

//provide the value
CPar::CPar(complex<double> v)
{
    new CPar();
    setValue(v);
    if(debug) printLog("CPar\t", value, "\n");
}

//updates the value that is stored in the block
void CPar::updateOutBlockValue()
{
	if(imag(value) != 0)
        cout << "WARNING: CPar.cpp line " << 
        __LINE__ << ": complex part of parameter "
        << this->getComment() << " is lost" << endl;
    outblock->setElemValue(outpos, real(value));
}
