/*
	Some helpful routines.
*/
double FRsgn(double x){
	if(x<0.0) return -1.0;
	else return 1.0;
}

double FRabs(double x){
	if(x<0.0) return -x;
	else return x;
}

/*
	Obtains the mass from the diagonalized mass matrix.
*/
double FRMass(struct FReigensystem FRN, int i){
	int j;
	j=FRN.order[i];
	return sqrt(FRabs(FRN.A[FRN.n*j+j]));
}


/*
	Obtains the mass from the diagonalized mass matrix.
*/
double FRVector(struct FReigensystem FRN, int i, int j){
  int k;
  k=FRN.order[i];
  return FRN.V[FRN.n*j+k];
}

/*
	The main diagonalization routine.
*/
int FRdiagonalize(struct FReigensystem FRN){
	int p,q;  //Coords of maximum element where the rotation occurs.
	int i,j;  //Coords of current element.
	double max; //Maximum off-diagonal element.
	double theta; //11.1.8 from Numerical Recipes.
	double t,s,c, tau; //tan, sin and cos of rotation angle.
	
	
	//Find maximum off-diagonal element.
	max=0.0;p=0;q=1;
	for(i=0;i<FRN.n;i++) for(j=i+1;j<FRN.n;j++) {
		if(FRabs(FRN.A[FRN.n*i+j])>max){
			max=FRabs(FRN.A[FRN.n*i+j]);
			p=i;q=j;
		}
	}
	//printf("%i\t%i\n",p,q);
	
	//If max>FRMIN then rotate max off-diagonal element to zero.
	while(max>FRMIN){
		double tmp1, tmp2;
		//Determine the angle.
		theta=(FRN.A[FRN.n*q+q]-FRN.A[FRN.n*p+p])/2.0/FRN.A[FRN.n*p+q];
		if(theta*theta>FRMAX) t=FRsgn(theta)/theta/2.0;
		else t=FRsgn(theta)/(FRabs(theta)+sqrt(1.0+theta*theta));
		c=1.0/sqrt(1.0+t*t);
		s=t*c;
		tau=s/(1.0+c);
		//printf("theta=%G\tsgn(theta)=%G\tt=%G\tc=%G\ts=%G\ttau=%G\n",
			//theta,FRsgn(theta),t,c,s,tau);
		
		
		//Rotate A by the angle.
		FRN.A[FRN.n*p+p]=FRN.A[FRN.n*p+p]-t*FRN.A[FRN.n*p+q];
		FRN.A[FRN.n*q+q]=FRN.A[FRN.n*q+q]+t*FRN.A[FRN.n*p+q];
		for(i=0;i<p;i++){
			tmp1=-s*(FRN.A[FRN.n*i+q]+tau*FRN.A[FRN.n*i+p]);
			tmp2= s*(FRN.A[FRN.n*i+p]-tau*FRN.A[FRN.n*i+q]);
			FRN.A[FRN.n*i+p]=FRN.A[FRN.n*i+p]+tmp1;
			FRN.A[FRN.n*i+q]=FRN.A[FRN.n*i+q]+tmp2;
		}		
		for(i=p+1;i<q;i++){
			tmp1=-s*(FRN.A[FRN.n*i+q]+tau*FRN.A[FRN.n*p+i]);
			tmp2= s*(FRN.A[FRN.n*p+i]-tau*FRN.A[FRN.n*i+q]);
			FRN.A[FRN.n*p+i]=FRN.A[FRN.n*p+i]+tmp1;
			FRN.A[FRN.n*i+q]=FRN.A[FRN.n*i+q]+tmp2;
		}		
		for(i=q+1;i<FRN.n;i++){
			tmp1=-s*(FRN.A[FRN.n*q+i]+tau*FRN.A[FRN.n*p+i]);
			tmp2= s*(FRN.A[FRN.n*p+i]-tau*FRN.A[FRN.n*q+i]);
			FRN.A[FRN.n*p+i]=FRN.A[FRN.n*p+i]+tmp1;
			FRN.A[FRN.n*q+i]=FRN.A[FRN.n*q+i]+tmp2;
		}
		FRN.A[FRN.n*p+q]=0.0;
		
		//Rotate V by the angle.
		for(i=0;i<FRN.n;i++){
		tmp1=((s/tau)-2.0)*FRN.V[FRN.n*i+p]-s*FRN.V[FRN.n*i+q];
		tmp2=((s/tau)-2.0)*FRN.V[FRN.n*i+q]+s*FRN.V[FRN.n*i+p];
		FRN.V[FRN.n*i+p]=FRN.V[FRN.n*i+p]+tmp1;
		FRN.V[FRN.n*i+q]=FRN.V[FRN.n*i+q]+tmp2;
		}
		
		
		//Find maximum off-diagonal element.
		max=0.0;p=0;q=1;
		for(i=0;i<FRN.n;i++) for(j=i+1;j<FRN.n;j++) {
			if(FRabs(FRN.A[FRN.n*i+j])>max){
				max=FRabs(FRN.A[FRN.n*i+j]);
				p=i;q=j;
			}
		}
		
		//Print
		//FRprint(FRN);
	
	}
	
	//Determine the order of the eigenvalues.
	for(i=0;i<FRN.n;i++){
		double smallest = 1E+100;
		int k,ksmallest=0;		
		for(j=0;j<FRN.n;j++){
			if(FRabs(FRN.A[FRN.n*j+j])<smallest){
				int found=0;
				for(k=0;k<FRN.n;k++) if(FRN.order[k]==j) found=1;
				if(found==0){
					ksmallest=j;
					smallest=FRabs(FRN.A[FRN.n*j+j]);
				}
			}
		}
		//printf("%G\n",smallest);
		FRN.order[i]=ksmallest;
		//printf("%i\n",FRN.order[i]);
	}
	
	
}


/*
	A simple print statement useful for debugging.
*/
int FRprint(struct FReigensystem FRN){	
	int i,j;
	//Print A
	printf("A=");
	for(i=0;i<FRN.n;i++){
		printf("\t(");
		for(j=0;j<i;j++) printf("\t%8G",FRN.A[FRN.n*j+i]);
		for(j=i;j<FRN.n;j++) printf("\t%8G",FRN.A[FRN.n*i+j]);
		printf("\t)\n");
	}
	printf("\n");
	
	//Print V
	printf("V=");
	for(i=0;i<FRN.n;i++){
		printf("\t(");
		for(j=0;j<FRN.n;j++) printf("\t%8G",FRN.V[FRN.n*i+j]);
		printf("\t)\n");
	}
	printf("\n");
}




/*
	Tests
*/
int FRM2Tests(int m, int md, double maxD){
	int i,n, agree;
	printf("----------------  Begin Tests  -------------------\n");
	printf("Number of tests             :  %i\n",m);
	printf("Range of dimensions         :  2-%i\n",20);
	printf("Maximum discrepancy allowed : %G\n",maxD);
	printf("--------------------------------------------------\n");
	printf("For each test the following occurs:\n");
	printf("   1) A random matrix dimension N is chosen.\n");
	printf("   2) A random real symmetric N dimensional\n");
	printf("      matrix A is generated.\n");
	printf("   3) The random matrix A is diagonalized to\n");
	printf("      give D and the orthogonal transformation V.\n");
	printf("   4) V^T A V is compared with D.\n");
	printf("   5) A V - V l is compared with 0 (where l is\n");
	printf("      the eigenvalue).\n");
	printf("If a test is failed, the matrices are printed.\n");
	printf("--------------------------------------------------\n");
	
	for(i=0;i<m;i++){
		n=(int) (1.0*(md-2.0)*rand()/RAND_MAX);
		n=n+2;
		if((i+1)%1000==0) printf("%i/%i\n",i+1,m);
		//printf("%i/%i:\tn=%i\n",i+1,m,n);
		agree=FRM2Test(n, maxD);
		if(agree==0){
			printf("%i/%i:\tn=%i\n",i+1,m,n);
		}
	}
	printf("----------------   End Tests   -------------------\n");
	
}

int FRM2Test(int n, double maxD){
	int i, j, k, l, agree;
	double *M1, *D1, *M2;
	srand(time(0));
	
	//Define a new random real symmetric matrix.
	FRM2T1.n=n;FRM2T2.n=n;
	//A
	free(FRM2T1.A);free(FRM2T2.A);
	FRM2T1.A = (double*) malloc(n*n*sizeof(double));
	FRM2T2.A = (double*) malloc(n*n*sizeof(double));
	for(i=0;i<n;i++) for(j=i;j<n;j++){
		FRM2T1.A[n*i+j]=(double)rand()/RAND_MAX;
		FRM2T2.A[n*i+j]=FRM2T1.A[n*i+j];
	}
	//V
	free(FRM2T1.V);free(FRM2T2.V);
	FRM2T1.V = (double*) malloc(n*n*sizeof(double));
	FRM2T2.V = (double*) malloc(n*n*sizeof(double));
	for(i=0;i<n;i++) {
		for(j=i;j<n;j++){
			FRM2T1.V[n*i+j]=0.0;
			FRM2T1.V[n*j+i]=0.0;
			FRM2T2.V[n*i+j]=0.0;
			FRM2T2.V[n*j+i]=0.0;
		}
		FRM2T1.V[n*i+i]=1.0;
		FRM2T2.V[n*i+i]=1.0;
	}
	//order
	free(FRM2T1.order);free(FRM2T2.order);
	FRM2T1.order= (int*) malloc(n*sizeof(int));
	FRM2T2.order= (int*) malloc(n*sizeof(int));
	for(i=0;i<n;i++) {
		FRM2T1.order[i]=-1;
		FRM2T2.order[i]=-1;
	}
	
	//Diagonalize the 2nd one.  Keep the 1st undiagonalized.
	FRdiagonalize(FRM2T2);
	
	//FRprint(FRM2T1);
	//FRprint(FRM2T2);
	
	//Test the resulting orthogonal transformation.
	//Multiply V^T A V
	M1 = (double*) malloc(n*n*sizeof(double));
	for(i=0;i<n;i++) for(j=i;j<n;j++) M1[n*i+j]=0.0;
	for(i=0;i<n;i++){
		for(k=0;k<n;k++) for(l=0;l<n;l++){
			if(k<=l) M1[n*i+i]=M1[n*i+i]+FRM2T2.V[n*k+i]*FRM2T1.A[n*k+l]*FRM2T2.V[n*l+i];
			else M1[n*i+i]=M1[n*i+i]+FRM2T2.V[n*k+i]*FRM2T1.A[n*l+k]*FRM2T2.V[n*l+i];
		}
		for(j=i+1;j<n;j++){
			for(k=0;k<n;k++) for(l=0;l<n;l++){
				if(k<=l) M1[n*i+j]=M1[n*i+j]+FRM2T2.V[n*k+i]*FRM2T1.A[n*k+l]*FRM2T2.V[n*l+j];
				else M1[n*i+j]=M1[n*i+j]+FRM2T2.V[n*k+i]*FRM2T1.A[n*l+k]*FRM2T2.V[n*l+j];
			}
		}
	}
	
	//See if V^T A V is equal to the diagonalized matrix.
	D1 = (double*) malloc(n*n*sizeof(double));
	agree=1;
	for(i=0;i<n;i++) {
		D1[n*i+i]=(M1[n*i+i]-FRM2T2.A[n*i+i])/(M1[n*i+i]+FRM2T2.A[n*i+i]);
		if(D1[n*i+i]>maxD) agree=0;
		for(j=i+1;j<n;j++) {
			D1[n*i+j]=M1[n*i+j];
			if(D1[n*i+j]>maxD) agree=0;
		}
	}
	//Test the eigenvector and eigenvalue.
	//Calculate A V - lambda V
	M2 = (double*) malloc(n*n*sizeof(double));
	for(i=0;i<n;i++) for(j=i;j<n;j++) M2[n*i+j]=0.0;
	for(i=0;i<n;i++) {
		for(j=i;j<n;j++){
			for(k=0;k<n;k++){
				if(k>=i)	M2[n*i+j]=M2[n*i+j]+FRM2T1.A[n*i+k]*FRM2T2.V[n*k+j];
				else M2[n*i+j]=M2[n*i+j]+FRM2T1.A[n*k+i]*FRM2T2.V[n*k+j];
			}
			M2[n*i+j]=M2[n*i+j]-FRM2T2.V[n*i+j]*FRM2T2.A[n*j+j];
			if(M2[n*i+j]>maxD) agree=0;
		}
	}
	
	//If they disagree, print the discprepancy.
	if(agree==0){
		printf("\n");
		FRprint(FRM2T1);
		FRprint(FRM2T2);
		printf("V^TAV=");
		for(i=0;i<n;i++){
			printf("\t(");
			for(j=0;j<i;j++) printf("\t%8G",M1[n*j+i]);
			for(j=i;j<n;j++) printf("\t%8G",M1[n*i+j]);
			printf("\t)\n");
		}
		printf("\n");
		printf("Delta=");
		for(i=0;i<n;i++){
			printf("\t(");
			for(j=0;j<i;j++) printf("\t%8G",D1[n*j+i]);
			for(j=i;j<n;j++) printf("\t%8G",D1[n*i+j]);
			printf("\t)\n");
		}
		printf("\n");
		printf("AV-lV=");
		for(i=0;i<n;i++){
			printf("\t(");
			for(j=0;j<i;j++) printf("\t%8G",M2[n*j+i]);
			for(j=i;j<n;j++) printf("\t%8G",M2[n*i+j]);
			printf("\t)\n");
		}
		printf("\n");
		
	}
	
	
	
	return agree;
}
