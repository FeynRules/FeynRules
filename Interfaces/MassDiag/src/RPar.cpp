#include "RPar.hpp"

RPar::RPar()
{
    new Par();
    value = 0.;
}

//provide the value
RPar::RPar(double v)
{
    new RPar();
    setValue(v);
    if(debug) printLog("RPar\t", value, "\n");
}

//provide the value and output block and position
RPar::RPar(double v, SLHABlock * o, int * op)
{
    new RPar();
    setValue(v);
    setOutBlock(o);
    setOutPos(o->getNrInd(), op);
    if(debug) printLog("RPar\t", value, "\n");

}

//provide the value and output block, position and comment
RPar::RPar(double v, SLHABlock * o, int * op, string c)
{
    new RPar(v, o, op);
    setComment(c);
    if(debug) printLog("RPar\t", comment, "\n");

}


//provide the input block and position
void RPar::init(SLHABlock * i, int * ip)
{
    ParSLHA* ps = i->getElement(ip);
    setValue(ps->getValue());
    setComment(ps->getComment());
    setInBlock(i); 
    setInPos(i->getNrInd(), ip);
    setOutBlock(i);
    setOutPos(i->getNrInd(),ip);
    if(debug)
        printLogPar("RPar\t", comment, "\t", value, "\t",
        outblock->getBlockName(), "\t", outpos);
}

//provide the inblock and one index
RPar::RPar(SLHABlock * i , int p1)
{
    int p[] = {p1};
    new RPar();
    init(i, p);
}

//provide the inblock and two indices
RPar::RPar(SLHABlock * i, int p1, int p2)
{
    int p[] = {p1, p2};
    new RPar();
    init(i, p);
}

//provide the inblock and three indices
RPar::RPar(SLHABlock * i, int p1, int p2, int p3)
{
    int p[] = {p1, p2, p3};
    new RPar();
    init(i, p);
}


//provide the input block, position and output block, position
void RPar::init(SLHABlock * i, int * ip,
SLHABlock * o, int * op)
{
    init(i, ip);
    setOutBlock(o);
    setOutPos(o->getNrInd(), op);
}

RPar::RPar(SLHABlock * i, int ip1,
SLHABlock * o, int op1)
{
	int ip[] = {ip1};
	int op[] = {op1};
	new RPar();
	init(i, ip, o, op);
}
RPar::RPar(SLHABlock * i, int ip1, int ip2,
SLHABlock * o, int op1)
{
	int ip[] = {ip1, ip2};
	int op[] = {op1};
	new RPar();
	init(i, ip, o, op);
}
RPar::RPar(SLHABlock * i, int ip1, int ip2, int ip3,
SLHABlock * o, int op1)
{
	int ip[] = {ip1, ip2, ip3};
	int op[] = {op1};
	new RPar();
	init(i, ip, o, op);
}
        
RPar::RPar(SLHABlock * i, int ip1,
SLHABlock * o, int op1, int op2)
{
	int ip[] = {ip1};
	int op[] = {op1, op2};
	new RPar();
	init(i, ip, o, op);
}

RPar::RPar(SLHABlock * i, int ip1, int ip2,
SLHABlock * o, int op1, int op2)
{
	int ip[] = {ip1, ip2};
	int op[] = {op1, op2};
	new RPar();
	init(i, ip, o, op);
}

RPar::RPar(SLHABlock * i, int ip1, int ip2, int ip3,
SLHABlock * o, int op1, int op2)
{
	int ip[] = {ip1, ip2, ip3};
	int op[] = {op1, op2};
	new RPar();
	init(i, ip, o, op);
}

RPar::RPar(SLHABlock * i, int ip1,
SLHABlock * o, int op1, int op2, int op3)
{
	int ip[] = {ip1};
	int op[] = {op1, op2, op3};
	new RPar();
	init(i, ip, o, op);
}

RPar::RPar(SLHABlock * i, int ip1, int ip2,
SLHABlock * o, int op1, int op2, int op3)
{
	int ip[] = {ip1, ip2};
	int op[] = {op1, op2, op3};
	new RPar();
	init(i, ip, o, op);
}

RPar::RPar(SLHABlock * i, int ip1, int ip2, int ip3,
SLHABlock * o, int op1, int op2, int op3)
{
	int ip[] = {ip1, ip2, ip3};
	int op[] = {op1, op2, op3};
	new RPar();
	init(i, ip, o, op);
}

//provide the input block, position and output block, position and comment
void RPar::init(SLHABlock * i, int * ip,
SLHABlock * o, int * op, string c)
{
    init(i, ip, o, op);
    setComment(c);
}

RPar::RPar(SLHABlock * i, int ip1,
SLHABlock * o, int op1, string c)
{
	int ip[] = {ip1};
	int op[] = {op1};
	new RPar();
	init(i, ip, o, op, c);	
}
RPar::RPar(SLHABlock * i, int ip1, int ip2,
SLHABlock * o, int op1, string c)
{
	int ip[] = {ip1, ip2};
	int op[] = {op1};
	init(i, ip, o, op, c);
}
RPar::RPar(SLHABlock * i, int ip1, int ip2, int ip3,
SLHABlock * o, int op1, string c)
{
	int ip[] = {ip1, ip2, ip3};
	int op[] = {op1};
	init(i, ip, o, op, c);
}
        
RPar::RPar(SLHABlock * i, int ip1,
SLHABlock * o, int op1, int op2, string c)
{
	int ip[] = {ip1};
	int op[] = {op1, op2};
	init(i, ip, o, op, c);
}

RPar::RPar(SLHABlock * i, int ip1, int ip2,
SLHABlock * o, int op1, int op2, string c)
{
	int ip[] = {ip1, ip2};
	int op[] = {op1, op2};
	init(i, ip, o, op, c);
}

RPar::RPar(SLHABlock * i, int ip1, int ip2, int ip3,
SLHABlock * o, int op1, int op2, string c)
{
	int ip[] = {ip1, ip2, ip3};
	int op[] = {op1, op2};
	init(i, ip, o, op, c);
}

RPar::RPar(SLHABlock * i, int ip1,
SLHABlock * o, int op1, int op2, int op3, string c)
{
	int ip[] = {ip1};
	int op[] = {op1, op2, op3};
	init(i, ip, o, op, c);
}

RPar::RPar(SLHABlock * i, int ip1, int ip2,
SLHABlock * o, int op1, int op2, int op3, string c)
{
	int ip[] = {ip1, ip2};
	int op[] = {op1, op2, op3};
	init(i, ip, o, op, c);
}

RPar::RPar(SLHABlock * i, int ip1, int ip2, int ip3,
SLHABlock * o, int op1, int op2, int op3, string c)
{
	int ip[] = {ip1, ip2, ip3};
	int op[] = {op1, op2, op3};
	init(i, ip, o, op, c);
}

//updates the value that is stored in the block
void RPar::updateOutBlockValue()
{
    outblock->setElemValue(outpos, value);
}
