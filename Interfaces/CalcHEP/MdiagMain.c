#include<stdio.h>
#include<stdlib.h>
#include<math.h>


int FRM21reset(double x, double t);
double FRM21Mass(int i);
int FRM22reset(double x, double t);
double FRM22Mass(int i);
int FRM2Tests(int m, int md, double maxD);

int main(){
	double x,t;
	srand(time(0));
	/*Standard*/
	/*
	FRM21reset(0.1,0.1);
	FRM22reset(0.1,0.1);
	printf("\t\tMW=%G\tMWP=%G\n",FRM21Mass(0),FRM21Mass(1));
	printf("MA=%G\tMZ=%G\tMZP=%G\n\n",FRM22Mass(0),FRM22Mass(1),FRM22Mass(2));
	*/
	/*Random*/
	/*
	x=(double)rand()/RAND_MAX;
	t=(double)rand()/RAND_MAX;
	FRM21reset(x,t);
	FRM22reset(x,t);
	printf("\t\tMW=%G\tMWP=%G\n",FRM21Mass(0),FRM21Mass(1));
	printf("MA=%G\tMZ=%G\tMZP=%G\n\n",FRM22Mass(0),FRM22Mass(1),FRM22Mass(2));
	*/
	
	/*Run Tests*/
	FRM2Tests(10000,20,1.0E-10);
	
}
