#include "ParSLHA.hpp"

/****constructors****/

//default constructor
ParSLHA::ParSLHA()
{
	value = 0.;
	comment = "";
}
//constructor
ParSLHA::ParSLHA(double v, string c)
{
	value = v;
	comment = c;
}
