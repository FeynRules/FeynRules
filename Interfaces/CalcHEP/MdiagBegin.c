#include<stdlib.h>
#include<math.h>
#include<stdio.h>
//cc MdiagMain.c Mdiag2.c -lm

const double FRMIN=1.0E-50;
const double FRMAX=1.0E+50;

/*
	Definition of the struct FReigensystem.
*/
struct FReigensystem {
	int n;
	double *A;
	double *V;
	int *order;
}FRM2T1, FRM2T2;

/*
	Definition of the functions in this file.
*/	
double FRsgn(double x);
double FRabs(double x);
double FRMass(struct FReigensystem FRN, int i);
double FRVector(struct FReigensystem FRN, int i, int j);
int FRdiagonalize(struct FReigensystem FRN);
int FRprint(struct FReigensystem FRN);
int FRM2Tests(int m, int md, double maxD);
int FRM2Test(int n, double maxD);

