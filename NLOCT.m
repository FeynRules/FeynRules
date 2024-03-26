(* ::Package:: *)

(* ::Section:: *)
(*Some printout*)


NLO$Version="1.02";

(*1.01 : fix for the tadpole*)
(*1.02 : color simplification rules added*)

Print[" - NLOCT - "];
Print["Version: "<>NLO$Version];
Print["Authors: C. Degrande"];
Print["Please cite C. Degrande, Comput.Phys.Commun. 197 (2015) 239-262"];

NLO$ngluon=0;
NLO$ncolour=0;

Off[Simplify::time];(*avoid the warning from aborded long simplification*)


(* ::Subtitle:: *)
(*Simplification rules*)


red4v::usage="replacement rules for four 4-vector with loop momentum with lorentz indices not contracted together to the squared of the scalar product 
of the loop momentum times the appropriate lorentz structure and coefficient in M$dim dimensions"
red2v::usage="replacement rules for four 2-vector with loop momentum with lorentz indices not contracted together to the scalar product 
of the loop momentum times the appropriate lorentz structure and coefficient in M$dim dimensions"


red4v[lm_]:={FV[lm,a_]FV[lm,b_]FV[lm,c_]FV[lm,d_]->1/(M$dim+2)/M$dim SP[lm,lm]^2(ME[a,b]ME[c,d]+ME[a,c]ME[b,d]+ME[a,d]ME[b,c]),
             LCivita[lm,a_,c_,d_]FV[lm,b_]FV[lm,e_]FV[lm,f_]->1/M$dim/(M$dim+2)* SP[lm,lm]^2(LCivita[b,a,c,d]ME[e,f]+
             LCivita[e,a,c,d]ME[b,f]+LCivita[f,a,c,d]ME[e,b])};
red2v[lm_]:={FV[lm,a_]FV[lm,b_]->1/M$dim* SP[lm,lm]ME[a,b],SP[lm,a_]FV[lm,b_]:>1/M$dim* FV[a,b]SP[lm,lm]/;Not[a===lm],
             FCh[a__,NonCommutative[DiracSlash[lm]],b__]FV[lm,c_]:>1/M$dim* FCh[a,NonCommutative[DiracMatrix[c]],b]SP[lm,lm],
             LCivita[lm,a_,c_,d_]FV[lm,b_]->1/M$dim* SP[lm,lm]LCivita[b,a,c,d]};


(* ::Subsubtitle::Closed:: *)
(*LeviCivita*)


LCivita[d___,FourMomentum[Incoming,a_],FourMomentum[Internal,b_],c___]:=-LCivita[d,FourMomentum[Internal,b],FourMomentum[Incoming,a],c];

LCivita[d___,FourMomentum[Internal,a_],FourMomentum[Internal,b_],c___]:=-LCivita[d,FourMomentum[Internal,b],FourMomentum[Incoming,a],c]/;a>b;
LCivita[d___,FourMomentum[Incoming,a_],FourMomentum[Incoming,b_],c___]:=-LCivita[d,FourMomentum[Incoming,b],FourMomentum[Incoming,a],c]/;a>b;

LCivita[c___,Index[Lorentz,d_],a_FourMomentum,b___]:=-LCivita[c,a,Index[Lorentz,d],b];

LCivita[c___,Index[Lorentz,d_],Index[Lorentz,a_],b___]:=-LCivita[c,Index[Lorentz,a],Index[Lorentz,d],b]/;d>a;


(* ::Subsubtitle::Closed:: *)
(*Metric*)


ME::usage="Metric tensor in M$dim"


Attributes[ME]=Orderless;
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*ME[Index[Lorentz,a_],Index[Lorentz,c_]]:=ME[Index[Lorentz,c],Index[Lorentz,b]];
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]^2:=M$dim;
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*f_[c___,Index[Lorentz,a_],d___]:=f[c,Index[Lorentz,b],d]/;Not[f===PolarizationVector];
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*FCh[c___,NonCommutative[DiracMatrix[Index[Lorentz,a_]]],d___]:=FCh[c,NonCommutative[DiracMatrix[Index[Lorentz,b]]],d];
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*ME[Index[Lorentz,a_],Index[Lorentz,b_]]:=M$dim;
ME[Index[Lorentz,a_],Index[Lorentz,a_]]:=M$dim;


(* ::Subsubtitle::Closed:: *)
(*Four vector*)


FV::usage="four vector"


FV[-a_,b_]:=-FV[a,b];
FV[a_+b_,c_]:=FV[a,c]+FV[b,c];
FV[a_?(FreeQ[#,FourMomentum]&)*b_,c_]:=a*FV[b,c];
FV/:FV[a_,b_]*FV[c_,b_]:=SP[a,c];
FV/:FV[a_,b_]^2:=SP[a,a];


(* ::Subsubtitle::Closed:: *)
(*Dirac Trace*)


DTr::usage="Trace over gamma matices"


DTr[a___,NonCommutative[DiracSlash[b__]+Mass[yy__]],c___]:=DTr[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]DTr[a,c];
DTr[a___,NonCommutative[DiracSlash[b__]]+Mass[yy__],c___]:=DTr[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]DTr[a,c];

DTr[a___,NonCommutative[DiracSlash[d_?(FreeQ[#,FourMomentum]&)*b__]],c___]:=d*DTr[a,NonCommutative[DiracSlash[b]],c];
DTr[a___,NonCommutative[DiracSlash[d_+b_]],c___]:=DTr[a,NonCommutative[DiracSlash[b]],c]+DTr[a,NonCommutative[DiracSlash[d]],c];

DTr[a___,Mass[yy__],c___]:=Mass[yy]DTr[a,c];

DTr[a___,NonCommutative[DiracSlash[b_]],NonCommutative[DiracSlash[b_]],c___]:=DTr[a,c]SP[b,b];
DTr[NonCommutative[DiracSlash[b_]],a___,NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=DTr[a,NonCommutative[ChiralityProjector[-pm]]]SP[b,b];
DTr[a___,NonCommutative[DiracSlash[0]],c___]:=0;

DTr[a___,b_+c_,d___]:=DTr[a,b,d]+DTr[a,c,d];
DTr[a___,b_*c_?(FreeQ[#,NonCommutative]&),d___]:=c*DTr[a,b,d];
DTr[a___,b_*G[k_][l_][m__][n__],d___]:=G[k][l][m][n]*DTr[a,b,d];
(*Depth of one for the feynman parameters*)
DTr[a___,c_?((FreeQ[#,NonCommutative]&&Depth[#]<2)&),d___]:=c*DTr[a,d];

DTr[a___,NonCommutative[b_,c__],d___]:=DTr[a,NonCommutative[b],NonCommutative[c],d];

DTr[a___,NonCommutative[ChiralityProjector[pm_]],NonCommutative[b:_DiracSlash|_DiracMatrix],d___]:=DTr[a,NonCommutative[b],NonCommutative[ChiralityProjector[-pm]],d];
DTr[a___,NonCommutative[ChiralityProjector[1]],NonCommutative[ChiralityProjector[-1]],d___]:=0;
DTr[a___,NonCommutative[ChiralityProjector[-1]],NonCommutative[ChiralityProjector[1]],d___]:=0;
DTr[a___,NonCommutative[ChiralityProjector[pm_]],NonCommutative[ChiralityProjector[pm_]],d___]:=DTr[a,NonCommutative[ChiralityProjector[pm]],d];

DTr[a___,NonCommutative[DiracSlash[lm_]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracSlash[lm_]],c___]:=2FV[lm,b]*DTr[a,NonCommutative[DiracSlash[lm]],c]-
SP[lm,lm]*DTr[a,NonCommutative[DiracMatrix[b]],c];
DTr[NonCommutative[DiracSlash[lm_]],a___,NonCommutative[DiracSlash[lm_]],NonCommutative[DiracMatrix[b_]],NonCommutative[ChiralityProjector[pm_]]]:=
2FV[lm,b]*DTr[a,NonCommutative[DiracSlash[lm]],NonCommutative[ChiralityProjector[-pm]]]-SP[lm,lm]*DTr[a,NonCommutative[DiracMatrix[b]],NonCommutative[ChiralityProjector[-pm]]];

DTr[NonCommutative[a:_DiracMatrix|_DiracSlash],NonCommutative[b:_DiracMatrix|_DiracSlash],NonCommutative[c:_DiracMatrix|_DiracSlash],NonCommutative[ChiralityProjector[pm_]]]:=0;

DTr[NonCommutative[a:_DiracMatrix|_DiracSlash],NonCommutative[ChiralityProjector[pm_]]]:=0;

DTr[NonCommutative[ChiralityProjector[pm_]]]:=2;

(*Only valid in 4 dimension *)

DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(ME[a,b]ME[c,d]-ME[a,c]ME[b,d]+ME[a,d]ME[b,c]+ I pm*LCivita[a,b,c,d]);
DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[a,b]ME[c,d]-FV[a,c]ME[b,d]+FV[a,d]ME[b,c]+ I pm*LCivita[a,b,c,d]);
DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[b,a]ME[c,d]-ME[a,c]FV[b,d]+ME[a,d]FV[b,c]+ I pm*LCivita[a,b,c,d]);
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[b_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[a,b]ME[c,d]-FV[a,c]ME[b,d]+FV[a,d]ME[b,c]+ I pm*LCivita[a,b,c,d]);
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b__]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[b,a]ME[c,d]-ME[a,c]FV[b,d]+ME[a,d]FV[b,c]+ I pm*LCivita[a,b,c,d]);

DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(SP[a,b]ME[c,d]-FV[a,c]FV[b,d]+FV[a,d]FV[b,c]+ pm*I LCivita[a,b,c,d])/;FreeQ[a,Internal]||FreeQ[b,Internal];
DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracSlash[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[b,a]FV[c,d]-FV[c,a]FV[b,d]+ME[a,d]SP[b,c]+ pm*I LCivita[a,b,c,d])/;FreeQ[c,Internal]||FreeQ[b,Internal];
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(SP[a,b]ME[c,d]-FV[a,c]FV[b,d]+FV[a,d]FV[b,c]+ pm*I LCivita[a,b,c,d])/;FreeQ[a,Internal]||FreeQ[b,Internal];
DTr[NonCommutative[DiracSlash[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b__]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[b,a]FV[c,d]-FV[c,a]FV[b,d]+ME[a,d]SP[b,c]+ pm*I LCivita[a,b,c,d])/;FreeQ[c,Internal]||FreeQ[b,Internal];

DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(-SP[a,b]ME[c,d]+FV[a,c]FV[b,d]+FV[a,d]FV[b,c]- pm*I LCivita[a,b,c,d])/;FreeQ[a,Internal]||FreeQ[b,Internal];
DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[c_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[b,a]FV[c,d]+FV[c,a]FV[b,d]-ME[a,d]SP[b,c]- pm*I LCivita[a,b,c,d])/;FreeQ[c,Internal]||FreeQ[b,Internal];

DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracMatrix[b_]],NonCommutative[ChiralityProjector[pm_]]]:=2*(ME[b,a]);
DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=2*(FV[b,a]);
DTr[NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[a_]],NonCommutative[ChiralityProjector[pm_]]]:=2*(FV[b,a]);
DTr[NonCommutative[DiracSlash[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=2*SP[a,b];/;FreeQ[a,Internal]||FreeQ[b,Internal];


(* ::Subsubtitle::Closed:: *)
(*Fermion Chain*)


FV::usage="Chain with gamma matices between two spinors"


FCh[a___,NonCommutative[DiracSlash[b__]+Mass[yy__]],c___]:=FCh[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]FCh[a,c];
FCh[a___,NonCommutative[DiracSlash[b__]]+Mass[yy__],c___]:=FCh[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]FCh[a,c];

FCh[a___,NonCommutative[DiracSlash[b_]],NonCommutative[DiracSlash[b_]],c___]:=FCh[a,c]SP[b,b];
FCh[a___,NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[b_]],c___]:=FCh[a,c]M$dim;

FCh[a___,NonCommutative[DiracSlash[d_?(FreeQ[#,FourMomentum]&)*b__]],c___]:=d*FCh[a,NonCommutative[DiracSlash[b]],c];
FCh[a___,NonCommutative[DiracSlash[d_+b_]],c___]:=FCh[a,NonCommutative[DiracSlash[b]],c]+FCh[a,NonCommutative[DiracSlash[d]],c];

FCh[a___,NonCommutative[DiracSlash[-b_]],c___]:=-FCh[a,NonCommutative[DiracSlash[b]],c];
FCh[a___,NonCommutative[DiracSlash[0]],c___]:=0;

FCh[a___,b_+c_,d___]:=FCh[a,b,d]+FCh[a,c,d];
FCh[a___,b_*c_?(FreeQ[#,NonCommutative]&),d___]:=c*FCh[a,b,d];
FCh[a___,b_*G[k_][l_][m__][n__],d___]:=G[k][l][m][n]*FCh[a,b,d];
FCh[a___,c_?((FreeQ[#,NonCommutative]&&Depth[#]<2)&),d___]:=c*FCh[a,d];

FCh[a___,NonCommutative[b_,c__],d___]:=FCh[a,NonCommutative[b],NonCommutative[c],d];

FCh[a___,NonCommutative[ChiralityProjector[pm_]],NonCommutative[b:_DiracSlash|_DiracMatrix],d___]:=FCh[a,NonCommutative[b],NonCommutative[ChiralityProjector[-pm]],d];
FCh[a___,NonCommutative[ChiralityProjector[1]],NonCommutative[ChiralityProjector[-1]],d___]:=0;
FCh[a___,NonCommutative[ChiralityProjector[-1]],NonCommutative[ChiralityProjector[1]],d___]:=0;
FCh[a___,NonCommutative[ChiralityProjector[pm_]],NonCommutative[ChiralityProjector[pm_]],d___]:=FCh[a,NonCommutative[ChiralityProjector[pm]],d];

FCh[a___,NonCommutative[DiracSlash[lm_]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracSlash[lm_]],c___]:=2FV[lm,b]*FCh[a,NonCommutative[DiracSlash[lm]],c]-
SP[lm,lm]*FCh[a,NonCommutative[DiracMatrix[b]],c];

FCh[a___,NonCommutative[DiracSlash[lm_]],NonCommutative[DiracSlash[b_]],NonCommutative[DiracSlash[lm_]],c___]:=2SP[lm,b]*FCh[a,NonCommutative[DiracSlash[lm]],c]-
SP[lm,lm]*FCh[a,NonCommutative[DiracSlash[b]],c];

FCh[a___,NonCommutative[DiracMatrix[l_]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[l_]],c___]:=(2-M$dim)*FCh[a,NonCommutative[DiracMatrix[b]],c];

FCh[a___,NonCommutative[DiracMatrix[l_]],NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[l_]],c___]:=(2-M$dim)*FCh[a,NonCommutative[DiracSlash[b]],c];

(*generic case*)

FCh[a___,NonCommutative[DiracSlash[lm_]],d__,NonCommutative[DiracMatrix[b_]],NonCommutative[DiracSlash[lm_]],c___]:=2FV[lm,b]*FCh[a,NonCommutative[DiracSlash[lm]],d,c]-
FCh[a,NonCommutative[DiracSlash[lm]],d,NonCommutative[DiracSlash[lm]],NonCommutative[DiracMatrix[b]],c];

FCh[a___,NonCommutative[DiracSlash[lm_]],d__,NonCommutative[DiracSlash[b_]],NonCommutative[DiracSlash[lm_]],c___]:=2SP[lm,b]*FCh[a,NonCommutative[DiracSlash[lm]],d,c]-
FCh[a,NonCommutative[DiracSlash[lm]],d,NonCommutative[DiracSlash[lm]],NonCommutative[DiracSlash[b]],c];

FCh[a___,NonCommutative[DiracMatrix[l_]],d__,NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[l_]],c___]:=2ME[b,l]*FCh[a,NonCommutative[DiracMatrix[l]],d,c]-
FCh[a,NonCommutative[DiracMatrix[l]],d,NonCommutative[DiracMatrix[l]],NonCommutative[DiracMatrix[b]],c];

FCh[a___,NonCommutative[DiracMatrix[l_]],d__,NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[l_]],c___]:=2FV[b,l]*FCh[a,NonCommutative[DiracMatrix[l]],d,c]-
FCh[a,NonCommutative[DiracMatrix[l]],d,NonCommutative[DiracMatrix[l]],NonCommutative[DiracSlash[b]],c];


(* ::Subsubtitle::Closed:: *)
(*ScalarProduct*)


SP::usage="scalar product"


Attributes[SP]=Orderless;
SP[-a_,b_]:=-SP[a,b];
SP[-a_,a_]:=-SP[a,a];
SP[-a_,-a_]:=SP[a,a];
SP[a_,b_+c_]:=SP[a,b]+SP[a,c];
SP[a_,b_*FourMomentum[c__]]:=b*SP[a,FourMomentum[c]];
SP[a_,b_?(FreeQ[#,FourMomentum]&)*c_]:=b*SP[a,c];


(* ::Subtitle:: *)
(*UV Tools*)


(* ::Subsubtitle:: *)
(*AddWf*)


AddWf[vert_]:=Block[{partlist,incr,a,wflist,pos},
  partlist=vert[[1,All,1]]/.{V[a_,__]->V[a],F[a_,__]->F[a],S[a_,__]->S[a]}/.{-V[a_]->V[a],-S[a_]->S[a],-F[a_]->F[a]}; 
  wflist=UV$Wftlist[[All,1]]/.{V[a_,__]->V[a],F[a_,__]->F[a],S[a_,__]->S[a]}/.{-V[a_]->V[a],-S[a_]->S[a],-F[a_]->F[a]};
  pos=(If[FreeQ[wflist,#],0,Position[wflist,#][[1,1]]]&)/@partlist;
  pos = DeleteCases[pos,0];
  (*remove part if interaction comes from the kinetic term like VVV, VVVV, FFV, SSV, SSVV since all vector are gauge boson*)
  {vert[[1]],vert[[2]],-1/2*vert[[3]]*(Total[UV$Wftlist[[pos,2]]])}
];


(* ::Subsubtitle:: *)
(*MergeVertList*)


MergeVertList[vl1_,vl2_]:=Block[{vsh,vlg,pos,verk},
  vsh=If[Length[vl1]>Length[vl2],vl2,vl1];
  vlg=If[Length[vl1]>Length[vl2],vl1,vl2];
  For[verk=1,verk<=Length[vsh],verk++,
    If[Not[FreeQ[vlg,vsh[[verk,1]]]],
      pos=Position[vlg[[All,1]],vsh[[verk,1]]][[1]];
      vlg[[pos,2]]=vlg[[pos,2]]+vsh[[verk,2]];
      vlg[[pos,3]]=vlg[[pos,3]]+vsh[[verk,3]];,
      vlg=Append[vlg,vsh[[verk]]];
    ];
  ];
  vlg
];


(* ::Subsubtitle::Closed:: *)
(*Xintegrate*)


XIntegrate[num_, del_,x_]:=Block[{m1,m2,p2,c0,c1,c2,nolog,mext,withlog,tempo},

m1 = (del/.{x->1})[[1]]/FR$MU;
m2 = (del/.{x->0})[[1]]/FR$MU;
p2 = Coefficient[del,x^2]/FR$MU^2;
tempo=Expand[num];
If[Head[tempo]===Plus,
  nolog = Integrate[Total[Cases[tempo,_?(FreeQ[#,Log]&)]],{x,0,1}];
  withlog = Total[Cases[tempo,_?(Not[FreeQ[#,Log]]&)]]/.Log[__]->1;
  ,
  If[FreeQ[tempo,Log],
    nolog=Integrate[tempo,{x,0,1}];
    withlog=0;
    ,
    nolog=0;
    withlog=tempo/.Log[__]->1;
  ];
];

nolog+Sum[IntxnLog[m1,m2,p2,nx]Coefficient[withlog,x,nx],{nx,0,2}]
];


IntxnLog::usage="IntxnLog[m1,m2,p2,n] is the integral over x of x^n Log[m2^2+x(m1^2-m2^2)+p2 x(x-1)], m1>=0,m2>=0,p2>=0,n=0,1 or 2. FR$Re should be set to 0 for 
the on-shell scheme and to 1 for the complex mass scheme."


IntxnLog[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,n_]:=
  If[m==0,1,0]IntxnLog2[0,0,0,n]+
  If[m==0,0,1]IntxnLog2[m/FR$MU,m/FR$MU,m^2/FR$MU^2,n]/;FreeQ[m,Mass];

IntxnLog[m_,m_,p2_,n_]:=
  If[m==0,1,0]If[p2==0,1,0]IntxnLog2[0,0,0,n]+
  If[m==0,1,0]If[p2==0,0,1]IntxnLog2[0,0,p2,n]+
  If[m==0,0,1]If[p2==0,1,0]IntxnLog2[m,m,0,n]+
  If[m==0,0,1]If[p2==0,0,1](If[m==p2^(1/2),0,1]IntxnLog2[m,m,p2,n]+If[m==p2^(1/2),1,0]IntxnLog2[m,m,m^2,n])/;(FreeQ[m,Mass]&&FreeQ[p2,FourMomentum]&&(Not[FreeQ[p2,FR$MU]]||p2===0));

IntxnLog[m1_/FR$MU,0,m1_^2/FR$MU^2,n_]:=
  If[m1==0,1,0]IntxnLog2[0,0,0,n]+
  If[m1==0,0,1](IntxnLog2[m1/FR$MU,0,m1^2/FR$MU^2,n])/;FreeQ[m1,Mass];

IntxnLog[m1_/FR$MU,m2_/FR$MU,m1_^2/FR$MU^2,n_]:=
  If[m1==0,1,0]If[m2==0,1,0]IntxnLog2[0,0,0,n]+
  If[m1==0,1,0]If[m2==0,0,1]IntxnLog2[0,m2/FR$MU,0,n]+
  If[m1==0,0,1]If[m2==0,1,0](IntxnLog2[m1/FR$MU,0,m1^2/FR$MU^2,n])+
  If[m1==0,0,1]If[m2==0,0,1](If[m1==m2,1,0]IntxnLog2[m1/FR$MU,m1/FR$MU,m1^2/FR$MU^2,n]+If[m1==m2,0,1]IntxnLog2[m1/FR$MU,m2/FR$MU,m1^2/FR$MU^2,n])/;FreeQ[m1,Mass]&&FreeQ[m2,Mass];

IntxnLog[0,m2_/FR$MU,m2_^2/FR$MU^2,n_]:=
  If[m2==0,1,0]IntxnLog2[0,0,0,n]+
  If[m2==0,0,1]IntxnLog2[0,m2/FR$MU,m2^2/FR$MU^2,n]/;FreeQ[m2,Mass];

IntxnLog[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,n_]:=
  If[m1==0,1,0]If[m2==0,1,0]IntxnLog2[0,0,0,n]+
  If[m1==0,1,0]If[m2==0,0,1]IntxnLog2[0,m2/FR$MU,m2^2/FR$MU^2,n]+
  If[m1==0,0,1]If[m2==0,1,0](IntxnLog2[m1/FR$MU,0,0,n])+
  If[m1==0,0,1]If[m2==0,0,1](If[m1==m2,1,0]IntxnLog2[m1/FR$MU,m1/FR$MU,m1^2/FR$MU^2,n]+If[m1==m2,0,1]IntxnLog2[m1/FR$MU,m2/FR$MU,m2^2/FR$MU^2,n])/;FreeQ[m1,Mass]&&FreeQ[m2,Mass];

IntxnLog[m1_,m2_,p2_,n_]:=
  If[m1==0,1,0]If[m2==0,1,0]If[p2==0,1,0]IntxnLog2[0,0,0,n]+
  If[m1==0,1,0]If[m2==0,1,0]If[p2==0,0,1]IntxnLog2[0,0,p2,n]+
  If[m1==0,1,0]If[m2==0,0,1]If[p2==0,1,0]IntxnLog2[0,m2,0,n]+
  If[m1==0,1,0]If[m2==0,0,1]If[p2==0,0,1](If[m2==p2^(1/2),0,1]IntxnLog2[0,m2,p2,n]+If[m2==p2^(1/2),1,0]IntxnLog2[0,m2,m2^2,n])+
  If[m1==0,0,1]If[m2==0,1,0]If[p2==0,1,0]IntxnLog2[m1,0,0,n]+
  If[m1==0,0,1]If[m2==0,1,0]If[p2==0,0,1](If[m1==p2^(1/2),0,1]IntxnLog2[m1,0,p2,n]+If[m1==p2^(1/2),1,0]IntxnLog2[m1,0,m1^2,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,1,0](If[m1==m2,1,0]IntxnLog2[m1,m1,0,n]+If[m1==m2,0,1]IntxnLog2[m1,m2,0,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,0,1]If[m1==m2,1,0](If[m1==p2^(1/2),0,1]IntxnLog2[m1,m1,p2,n]+If[m1==p2^(1/2),1,0]IntxnLog2[m1,m1,m1^2,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,0,1]If[m1==m2,0,1](If[p2^(1/2)==m1,0,1]If[p2^(1/2)==m2,0,1]IntxnLog2[m1,m2,p2,n]+If[p2^(1/2)==m1,0,1]If[p2^(1/2)==m2,1,0]IntxnLog2[m1,m2,m2^2,n]+
    If[p2^(1/2)==m1,1,0]If[p2^(1/2)==m2,0,1]IntxnLog2[m1,m2,m1^2,n])/;(FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum]&&(Not[FreeQ[p2,FR$MU]]||p2===0));

IntxnLog2[0,0,0,n_Integer]:=-2/FR$Eps*FR$IR/(n+1);

(*p2=mass^2, cannot be on the negative real axix*)
IntxnLog2[0,0,p2_,0]:=(-2 (*+ If[p2>0,0,1]*Log[-p2]*) + (*If[p2>0,1,0]*)(-I*Pi*FR$Re + Log[p2]))/;FreeQ[p2,FourMomentum];
IntxnLog2[0,0,p2_,1]:=(-2 +  ((*If[p2>0,0,1]*Log[-p2] +*) (*If[p2>0,1,0]*)(-I*Pi*FR$Re + Log[p2])))/2/;FreeQ[p2,FourMomentum];
IntxnLog2[0,0,p2_,2]:=(-13 + 6*((*If[p2>0,0,1]*Log[-p2] +*) (*If[p2>0,1,0]*)(-I*Pi*FR$Re + Log[p2])))/18/;FreeQ[p2,FourMomentum];

IntxnLog2[0,m2_,0,0]:=(-1 + 2*Log[m2])/;FreeQ[m2,Mass];
IntxnLog2[0,m2_,0,1]:=(-3/4 + Log[m2])/;FreeQ[m2,Mass];
IntxnLog2[0,m2_,0,2]:=(-11/18 + (2*Log[m2])/3)/;FreeQ[m2,Mass];

IntxnLog2[m1_,0,0,0]:=(-1 + 2*Log[m1])/;FreeQ[m1,Mass];
IntxnLog2[m1_,0,0,1]:=(-1/4 + Log[m1])/;FreeQ[m1,Mass];
IntxnLog2[m1_,0,0,2]:=((-1 + 6*Log[m1])/9)/;FreeQ[m1,Mass];

IntxnLog2[m_,m_,0,0]:=Log[m^2]/;FreeQ[m,Mass];
IntxnLog2[m_,m_,0,1]:=Log[m]/;FreeQ[m,Mass];
IntxnLog2[m_,m_,0,2]:=(2*Log[m])/3/;FreeQ[m,Mass];

IntxnLog2[m_/FR$MU,0,m_^2/FR$MU^2,0]:=2*(-1 + Log[m/FR$MU])/;FreeQ[m,Mass];
IntxnLog2[m_/FR$MU,0,m_^2/FR$MU^2,1]:=(-1/2 + Log[m/FR$MU])/;FreeQ[m,Mass];
IntxnLog2[m_/FR$MU,0,m_^2/FR$MU^2,2]:=(2*(-1 + 3*Log[m/FR$MU]))/9/;FreeQ[m,Mass];

IntxnLog2[0,m_/FR$MU,m_^2/FR$MU^2,0]:=2*(-1 + Log[m/FR$MU])/;FreeQ[m,Mass];
IntxnLog2[0,m_/FR$MU,m_^2/FR$MU^2,1]:=(-3/2 + Log[m/FR$MU])/;FreeQ[m,Mass];
IntxnLog2[0,m_/FR$MU,m_^2/FR$MU^2,2]:=(-11 + 6*Log[m/FR$MU])/9/;FreeQ[m,Mass];

IntxnLog2[m1_,m2_,0,0]:=(-m1^2 + m2^2 + 2*m1^2*Log[m1] - 2*m2^2*Log[m2])/(m1^2 - m2^2)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];
IntxnLog2[m1_,m2_,0,1]:=(-m1^4 + 4*m1^2*m2^2 - 3*m2^4 + 4*(m1^4 - 2*m1^2*m2^2)*Log[m1] + 4*m2^4*Log[m2])/(4*(m1^2 - m2^2)^2)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];
IntxnLog2[m1_,m2_,0,2]:=(-2*m1^6 + 9*m1^4*m2^2 - 18*m1^2*m2^4 + 11*m2^6 + 12*(m1^6 - 3*m1^4*m2^2 + 3*m1^2*m2^4)*Log[m1] - 12*m2^6*Log[m2])/(18*(m1^2 - m2^2)^3)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];

IntxnLog2[m1_,0,p2_,0]:=(-2*p2 + 2*m1^2*Log[m1] + (-m1^2 + p2)*(If[NLO$CMS||m1^2>p2,0,1]*(Log[-m1^2 + p2]+I Pi*FR$Re)+If[NLO$CMS||m1^2>p2,1,0]*Log[m1^2 - p2]))/p2/;FreeQ[p2,FourMomentum];
IntxnLog2[m1_,0,p2_,1]:=1/2/p2(m1^2(2Log[m1]-1)-IntxnLog2[m1,0,p2,0](m1^2-p2))/;FreeQ[m1,Mass]&&FreeQ[p2,FourMomentum];
IntxnLog2[m1_,0,p2_,2]:=((m1^2-p2/3)/2-2(m1^2-p2)IntxnLog2[m1,0,p2,1]-m1^2(1-2Log[m1]))/3/p2/;FreeQ[m1,Mass]&&FreeQ[p2,FourMomentum];

IntxnLog2[0,m2_,p2_,0]:=(-2*p2 + 2*m2^2*Log[m2] + (-m2^2 + p2)*(If[NLO$CMS||m2^2>p2,0,1]*(Log[-m2^2 + p2]+I Pi*FR$Re)+If[NLO$CMS||m2^2>p2,1,0]*Log[m2^2 - p2]))/p2/;FreeQ[p2,FourMomentum];
IntxnLog2[0,m2_,p2_,1]:=1/2/p2(-m2^2(2Log[m2]-1)-IntxnLog2[0,m2,p2,0](-m2^2-p2))/;FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];
IntxnLog2[0,m2_,p2_,2]:=((m2^2-p2/3)/2-m2^2 IntxnLog2[0,m2,p2,0]-2(-m2^2-p2)IntxnLog2[0,m2,p2,1])/3/p2/;FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];

IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,0]:=-2 + Pi/Sqrt[3] + 2*Log[m/FR$MU]/;FreeQ[m,Mass];
IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,1]:=-1 + Pi/(2*Sqrt[3]) + Log[m/FR$MU]/;FreeQ[m,Mass];
IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,2]:=-1/18 + (2*Log[m/FR$MU])/3/;FreeQ[m,Mass];

IntxnLog2[m_,m_,p2_,0]:=-2 + Log[m^2] - (Sqrt[p2*(-4*m^2 + p2)]*(If[NLO$CMS||p2<4m^2,1,0]*Log[(2*m^2 - p2 + Sqrt[p2*(-4*m^2 + p2)])/(2*m^2)]+ 
  If[NLO$CMS||m>0&&p2<4m^2,0,1](Log[-(2*m^2 - p2 + Sqrt[p2*(-4*m^2 + p2)])/(2*m^2)]+I Pi*FR$Re)))/p2/;FreeQ[p2,FourMomentum];

IntxnLog2[m1_/FR$MU,m2_/FR$MU,m1_^2/FR$MU^2,0]:= If[NLO$CMS||m2>2m1,1,0](-2 + (-1 + m2^2/m1^2)*Log[m2/m1] + Log[(m1*m2)/FR$MU^2] - (Sqrt[-4*m1^2*m2^2 + m2^4]*
  Log[(m2^2 + Sqrt[-4*m1^2*m2^2 + m2^4])/(2*m1*m2)])/m1^2)+If[NLO$CMS||m2>2m1,0,1](-2 + (-1 + m2^2/m1^2)*Log[m2/m1] + Log[(m1*m2)/FR$MU^2] - Re[(Sqrt[-4*m1^2*m2^2 + m2^4]*
  Log[(m2^2 + Sqrt[-4*m1^2*m2^2 + m2^4])/(2*m1*m2)])]/m1^2)/;FreeQ[m1,Mass]&&FreeQ[m2,Mass];

IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,0]:=IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0];
IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,1]:=IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0]-IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,1];
IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,2]:=IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0]-2 IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,1]+IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,2];

IntxnLog2[m1_,m2_,p2_,0]:=-2 - ((m1^2 - m2^2)*Log[m2/m1])/p2 + Log[(m1*m2)] + 
 (m1*m2*(-(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2) - (-m1^2 - m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] + p2)/(2*m1*m2))*
   (If[NLO$CMS||p2<(m1-m2)^2,1,0]Log[(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]+
    If[NLO$CMS||p2<=(m1+m2)^2,0,1](Log[-(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]+I Pi*FR$Re)))/p2+
    If[Not[NLO$CMS]&&p2>=(m1-m2)^2&&p2<(m1+m2)^2,1,0]Re[m1*m2*(-(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2) - 
    (-m1^2 - m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] + p2)/(2*m1*m2))*Log[(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]]/p2/;FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];

IntxnLog2[m1_,m2_,p2_,1]:=1/2/p2(-m2^2(2Log[m2]-1)+m1^2(2Log[m1]-1)-IntxnLog2[m1,m2,p2,0](-m2^2+m1^2-p2))/;FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&
FreeQ[p2,FourMomentum];

IntxnLog2[m1_,m2_,p2_,2]:=((m1^2+m2^2-p2/3)/2-m2^2 IntxnLog2[m1,m2,p2,0]-2(-m2^2+m1^2-p2)IntxnLog2[m1,m2,p2,1]-m1^2(1-2Log[m1]))/3/p2/;
FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];


UVLog[0]:=0;(*from the tadpole from x Log[x], x->0*)
UVLog[x_?(Not[Head[#]===Mass]&)^2/FR$MU^2]:=Log[x^2/FR$MU^2];


(* ::Subsubtitle::Closed:: *)
(*Dp2IntxnLog*)


Dp2IntxnLog::usage="Dp2IntxnLog[m1,m2,p2,n] is the derivative with respect to p2 of the real part of the integral over x of x^n Log[m2^2+x(m1^2-m2^2)+p2x(x-1)], 
m1>=0,m2>=0,p2>=0,n=0,1 or 2"


(*warning d=4+eps almost until the end*)
Dp2IntxnLog[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,n_]:=
  If[m==0,1,0]Dp2IntxnLog2[0,0,0,n]+
  If[m==0,0,1]Dp2IntxnLog2[m/FR$MU,m/FR$MU,m^2/FR$MU^2,n]/;FreeQ[m,Mass];

Dp2IntxnLog[m_,m_,p2_,n_]:=
  If[m==0,1,0]If[p2==0,1,0]Dp2IntxnLog2[0,0,0,n]+
  If[m==0,1,0]If[p2==0,0,1]Dp2IntxnLog2[0,0,p2,n]+
  If[m==0,0,1]If[p2==0,1,0]Dp2IntxnLog2[m,m,0,n]+
  If[m==0,0,1]If[p2==0,0,1](If[m==p2^(1/2),0,1]Dp2IntxnLog2[m,m,p2,n]+If[m==p2^(1/2),1,0]Dp2IntxnLog2[m,m,m^2,n])/;FreeQ[m,Mass]&&FreeQ[p2,FourMomentum];

Dp2IntxnLog[m1_/FR$MU,0,m1_^2/FR$MU^2,n_]:=
  If[m1==0,1,0]Dp2IntxnLog2[0,0,0,n]+
  If[m1==0,0,1](Dp2IntxnLog2[m1/FR$MU,0,m1^2/FR$MU^2,n])/;FreeQ[m1,Mass];

Dp2IntxnLog[m1_/FR$MU,m2_/FR$MU,m1_^2/FR$MU^2,n_]:=
  If[m1==0,1,0]If[m2==0,1,0]Dp2IntxnLog2[0,0,0,n]+
  If[m1==0,1,0]If[m2==0,0,1]Dp2IntxnLog2[0,m2/FR$MU,0,n]+
  If[m1==0,0,1]If[m2==0,1,0](Dp2IntxnLog2[m1/FR$MU,0,m1^2/FR$MU^2,n])+
  If[m1==0,0,1]If[m2==0,0,1](If[m1==m2,1,0]Dp2IntxnLog2[m1/FR$MU,m1/FR$MU,m1^2/FR$MU^2,n]+If[m1==m2,0,1]Dp2IntxnLog2[m1/FR$MU,m2/FR$MU,m1^2/FR$MU^2,n])/;FreeQ[m1,Mass]&&FreeQ[m2,Mass];

Dp2IntxnLog[0,m2_/FR$MU,m2_^2/FR$MU^2,n_]:=
  If[m2==0,1,0]Dp2IntxnLog2[0,0,0,n]+
  If[m2==0,0,1]Dp2IntxnLog2[0,m2/FR$MU,m2^2/FR$MU^2,n]/;FreeQ[m2,Mass];

Dp2IntxnLog[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,n_]:=
  If[m1==0,1,0]If[m2==0,1,0]Dp2IntxnLog2[0,0,0,n]+
  If[m1==0,1,0]If[m2==0,0,1]Dp2IntxnLog2[0,m2/FR$MU,m2^2/FR$MU^2,n]+
  If[m1==0,0,1]If[m2==0,1,0](Dp2IntxnLog2[m1/FR$MU,0,0,n])+
  If[m1==0,0,1]If[m2==0,0,1](If[m1==m2,1,0]Dp2IntxnLog2[m1/FR$MU,m1/FR$MU,m1^2/FR$MU^2,n]+If[m1==m2,0,1]Dp2IntxnLog2[m1/FR$MU,m2/FR$MU,m2^2/FR$MU^2,n])/;FreeQ[m1,Mass]&&FreeQ[m2,Mass];

Dp2IntxnLog[m1_,m2_,p2_,n_]:=
  If[m1==0,1,0]If[m2==0,1,0]If[p2==0,1,0]Dp2IntxnLog2[0,0,0,n]+
  If[m1==0,1,0]If[m2==0,1,0]If[p2==0,0,1]Dp2IntxnLog2[0,0,p2,n]+
  If[m1==0,1,0]If[m2==0,0,1]If[p2==0,1,0]Dp2IntxnLog2[0,m2,0,n]+
  If[m1==0,1,0]If[m2==0,0,1]If[p2==0,0,1](If[m2==p2^(1/2),0,1]Dp2IntxnLog2[0,m2,p2,n]+If[m2==p2^(1/2),1,0]Dp2IntxnLog2[0,m2,m2^2,n])+
  If[m1==0,0,1]If[m2==0,1,0]If[p2==0,1,0]Dp2IntxnLog2[m1,0,0,n]+
  If[m1==0,0,1]If[m2==0,1,0]If[p2==0,0,1](If[m1==p2^(1/2),0,1]Dp2IntxnLog2[m1,0,p2,n]+If[m1==p2^(1/2),1,0]Dp2IntxnLog2[m1,0,m1^2,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,1,0](If[m1==m2,1,0]Dp2IntxnLog2[m1,m1,0,n]+If[m1==m2,0,1]Dp2IntxnLog2[m1,m2,0,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,0,1]If[m1==m2,1,0](If[m1==p2^(1/2),0,1]Dp2IntxnLog2[m1,m1,p2,n]+If[m1==p2^(1/2),1,0]Dp2IntxnLog2[m1,m1,m1^2,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,0,1]If[m1==m2,0,1](If[p2^(1/2)==m1,0,1]If[p2^(1/2)==m2,0,1]Dp2IntxnLog2[m1,m2,p2,n]+If[p2^(1/2)==m1,0,1]If[p2^(1/2)==m2,1,0]Dp2IntxnLog2[m1,m2,m2^2,n]+
    If[p2^(1/2)==m1,1,0]If[p2^(1/2)==m2,0,1]Dp2IntxnLog2[m1,m2,m1^2,n])/;FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];

Dp2IntxnLog2[0,0,0,n_]:=0*FR$testdp2000;

Dp2IntxnLog2[m_,0,0,n_Integer]:=-1/m^2/(n+2)/(n+1)/;FreeQ[m,Mass]&&n>=0;

Dp2IntxnLog2[0,m_,0,n_Integer]:=-1/m^2/(n+2)/;FreeQ[m,Mass]&&n>=0;

Dp2IntxnLog2[0,0,p2_,0]:=1/p2/;FreeQ[p2,FourMomentum];
Dp2IntxnLog2[0,0,p2_,1]:=1/2/p2/;FreeQ[p2,FourMomentum];
Dp2IntxnLog2[0,0,p2_,2]:=1/3/p2/;FreeQ[p2,FourMomentum];

Dp2IntxnLog2[m1_/FR$MU,0,m1_^2/FR$MU^2,0]:=-1(1/FR$Eps*FR$IR/m1^2+(-1+Log[m1/FR$MU])/m1^2)FR$MU^2/;FreeQ[m1,Mass];(*Infared divergent*)
Dp2IntxnLog2[m1_/FR$MU,0,m1_^2/FR$MU^2,1]:=-1/2/m1^2*FR$MU^2/;FreeQ[m1,Mass];
Dp2IntxnLog2[m1_/FR$MU,0,m1_^2/FR$MU^2,2]:=-1/m1^2/6*FR$MU^2/;FreeQ[m1,Mass];

Dp2IntxnLog2[0,m2_/FR$MU,m2_^2/FR$MU^2,0]:=-(1/FR$Eps*FR$IR/m2^2+(-1+Log[m2/FR$MU])/m2^2)*FR$MU^2/;FreeQ[m2,Mass];
Dp2IntxnLog2[0,m2_/FR$MU,m2_^2/FR$MU^2,1]:=-(1/FR$Eps*FR$IR/m2^2+(-3/2+Log[m2/FR$MU])/m2^2)*FR$MU^2/;FreeQ[m2,Mass];
Dp2IntxnLog2[0,m2_/FR$MU,m2_^2/FR$MU^2,2]:=-(1/FR$Eps*FR$IR/m2^2+(-11/6+Log[m2/FR$MU])/m2^2)*FR$MU^2/;FreeQ[m2,Mass];

Dp2IntxnLog2[m1_,0,p2_,0]:=(p2 - 2*m1^2*Log[m1] + m1^2*(If[NLO$CMS||m1^2>p2,0,1]*(Log[-m1^2 + p2]+I Pi*FR$Re)+If[NLO$CMS||m1^2>p2,1,0]*Log[m1^2 - p2]))/p2^2/;FreeQ[p2,FourMomentum];
Dp2IntxnLog2[m1_,0,p2_,1]:=-IntxnLog[m1,0,p2,1]/p2+(IntxnLog[m1,0,p2,0]-(m1^2-p2)Dp2IntxnLog2[m1,0,p2,0])/2/p2/;FreeQ[m1,Mass]&&
FreeQ[p2,FourMomentum];
Dp2IntxnLog2[m1_,0,p2_,2]:=-IntxnLog[m1,0,p2,2]/p2+(-1/6+2IntxnLog[m1,0,p2,1]-2(m1^2-p2)Dp2IntxnLog2[m1,0,p2,1])/3/p2/;
FreeQ[m1,Mass]&&FreeQ[p2,FourMomentum];

Dp2IntxnLog2[0,m2_,p2_,0]:=Dp2IntxnLog2[m2,0,p2,0]/;FreeQ[p2,FourMomentum];
Dp2IntxnLog2[0,m2_,p2_,1]:=- IntxnLog[0,m2,p2,1]/p2+(IntxnLog[0,m2,p2,0]-(-m2^2-p2)Dp2IntxnLog2[0,m2,p2,0])/2/p2/;FreeQ[m2,Mass]&&
FreeQ[p2,FourMomentum];
Dp2IntxnLog2[0,m2_,p2_,2]:=-IntxnLog[0,m2,p2,2]/p2+(-1/6-m2^2 Dp2IntxnLog2[0,m2,p2,0]+2IntxnLog[0,m2,p2,1]-2(-m2^2-p2)Dp2IntxnLog2[0,m2,p2,1])/3/p2/;
FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];

Dp2IntxnLog2[m_/FR$MU,m_/FR$MU,0,n_Integer]:=-1/m^2*FR$MU^2/(n+2)/(n+3)/;FreeQ[m,Mass];

Dp2IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,0]:=(9 - 2*Sqrt[3]*Pi)/(9*m^2)*FR$MU^2/;FreeQ[m,Mass];
Dp2IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,1]:=(9 - 2*Sqrt[3]*Pi)/(18*m^2)*FR$MU^2/;FreeQ[m,Mass];
Dp2IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,2]:=(-6 + Sqrt[3]*Pi)/(9*m^2)*FR$MU^2/;FreeQ[m,Mass];

Dp2IntxnLog2[m1_,m2_,0,0]:=(-m1^4 + m2^4 + 4*m1^2*m2^2*Log[m1/m2])/(2*(m1^2 - m2^2)^3)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];
Dp2IntxnLog2[m1_,m2_,0,1]:=-(m1^6 - 6*m1^4*m2^2 + 3*m1^2*m2^4 + 2*m2^6 - 12*m1^2*m2^4*Log[m2/m1])/(6*(m1^2 - m2^2)^4)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];
Dp2IntxnLog2[m1_,m2_,0,2]:=(-m1^8 + 6*m1^6*m2^2 - 18*m1^4*m2^4 + 10*m1^2*m2^6 + 3*m2^8 + 24*m1^2*m2^6*Log[m1/m2])/(12*(m1^2 - m2^2)^5)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];

Dp2IntxnLog2[m_,m_,p2_,0]:=(Sqrt[p2*(-4*m^2 + p2)] - 2*m^2*(
     If[NLO$CMS||p2<4m^2,1,0]*Log[(2*m^2 - p2 + Sqrt[p2*(-4*m^2 + p2)])/(2*m^2)]+ 
     If[NLO$CMS||p2<=4m^2,0,1](Log[-(2*m^2 - p2 + Sqrt[p2*(-4*m^2 + p2)])/(2*m^2)]+I Pi*FR$Re))+
 If[p2==4m^2,1,0]*Sqrt[p2*(-4*m^2 + p2)] )/(p2*Sqrt[p2*(-4*m^2 + p2)])/;FreeQ[p2,FourMomentum];

Dp2IntxnLog2[m1_/FR$MU,m2_/FR$MU,m1_^2/FR$MU^2,0]:= If[NLO$CMS||m2>2m1,1,0]*(FR$MU^2*((m1^2*Sqrt[-4*m1^2*m2^2 + m2^4] + (m1^2 - m2^2)*Sqrt[-4*m1^2*m2^2 + m2^4]*Log[m2/m1] + 
(-3*m1^2*m2^2 + m2^4)*Log[(m2^2 + Sqrt[-4*m1^2*m2^2 + m2^4])/(2*m1*m2)])/(m1^4*Sqrt[-4*m1^2*m2^2 + m2^4])))+If[NLO$CMS||m2>2m1,0,1]*Re[(FR$MU^2*((m1^2*Sqrt[-4*m1^2*m2^2 + m2^4] + (m1^2 - m2^2)*Sqrt[-4*m1^2*m2^2 + m2^4]*Log[m2/m1] + 
(-3*m1^2*m2^2 + m2^4)*Log[(m2^2 + Sqrt[-4*m1^2*m2^2 + m2^4])/(2*m1*m2)])/(m1^4*Sqrt[-4*m1^2*m2^2 + m2^4])))]/;FreeQ[m1,Mass]&&FreeQ[m2,Mass];

Dp2IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,0]:=Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0];
Dp2IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,1]:=Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0]-Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,1];
Dp2IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,2]:=Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0]-2 Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,1]+Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,2];

Dp2IntxnLog2[m1_,m2_,p2_,0]:=((If[Not[NLO$CMS]&&p2>(m1-m2)^2&&p2<(m1+m2)^2,0,1]*p2*Sqrt[m1^4 + (-m2^2 + p2)^2 - 2*m1^2*(m2^2 + p2)] +
    If[Not[NLO$CMS]&&p2>(m1-m2)^2&&p2<(m1+m2)^2,0,1]*(m1^2 - m2^2)*Sqrt[m1^4 + (-m2^2 + p2)^2 - 2*m1^2*(m2^2 + p2)]*Log[m2/m1] + 
  (m1^4 + m2^2*(m2^2 - p2) - m1^2*(2*m2^2 + p2))*
   (If[NLO$CMS||p2<(m1-m2)^2,1,0]Log[(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]+
    If[NLO$CMS||p2<(m1-m2)^2||p2<(m1+m2)^2,0,1](Log[-(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]+I Pi FR$Re)))/
 (Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2]*p2^2))+ If[Not[NLO$CMS]&&p2>(m1-m2)^2&&p2<(m1+m2)^2,1,0]*
  Re[((p2*Sqrt[m1^4 + (-m2^2 + p2)^2 - 2*m1^2*(m2^2 + p2)] + (m1^2 - m2^2)*Sqrt[m1^4 + (-m2^2 + p2)^2 - 2*m1^2*(m2^2 + p2)]*Log[m2/m1] + 
  (m1^4 + m2^2*(m2^2 - p2) - m1^2*(2*m2^2 + p2))*Log[(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]))/
 (Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2]*p2^2)]/;Not[m1^2===p2]&&Not[m2^2===p2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];

Dp2IntxnLog2[m1_,m2_,p2_,1]:=(- IntxnLog2[m1,m2,p2,1]/p2+(IntxnLog2[m1,m2,p2,0]-(m1^2-m2^2-p2)Dp2IntxnLog2[m1,m2,p2,0])/2/p2)/;FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&
FreeQ[p2,FourMomentum];

Dp2IntxnLog2[m1_,m2_,p2_,2]:=-IntxnLog2[m1,m2,p2,2]/p2+(-1/6-m2^2 Dp2IntxnLog2[m1,m2,p2,0]+2IntxnLog2[m1,m2,p2,1]-2(m1^2-m2^2-p2)Dp2IntxnLog2[m1,m2,p2,1])/3/p2/;
FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];


(* ::Subsubtitle:: *)
(*SolveDelta*)


(*Remove dZ that do not correspond to the vertex*)
RemovedZ[vtx_]:=Block[{dZtmp,dZrule},
  dZtmp=DeleteCases[(vtx[[1,All,1]]/.-x_->x),Index[Colour|Gluon,_],\[Infinity]]/.x_[a_,{}]->x[a]/.(FR$ClassesTranslation/.Rule->dZrule/.dZrule[a_,b_]:>Rule[b,a]);
  dZtmp=Cases[Union[Cases[vtx,_FR$deltaZ,\[Infinity]]],_?(FreeQ[#,dZtmp[[1]]]||FreeQ[#,dZtmp[[2]]]&)];
  If[Length[dZtmp]>0,
    Print[Style["Warning : The following deltaZ are going to be put to zero in the "<>ToString[vtx[[1,All,1]]]<>" vertex",Orange]];
    Print[dZtmp];];
  vtx/.((Rule[#,0]&)/@dZtmp)
];


(*Consecutive solving*)
ConsSolve[eq_,var_]:=Block[{cssol={},cstmpeq,cstmpsol,csincr,csekk,csvar,totvar},

totvar=var;
For[csekk=1,csekk<=Length[var],csekk++,
  If[Not[FreeQ[ eq, Conjugate[ var[[csekk]] ] ]](*not*),
    totvar=Append[totvar,Conjugate[var[[csekk]] ] ];(*append*) 
  ];(*if*)
];(*for*)


 For[csekk=1,csekk<=Length[eq],csekk++,
   cstmpeq=Expand[eq[[csekk]]/.cssol];
   cstmpsol={};csincr=1;
   If[Not[cstmpeq===0],
     While[Length[cstmpsol]<1&&csincr<=Length[Intersection[Cases[cstmpeq,_FR$delta|_FR$deltaZ,{0,\[Infinity]}],var]],
       csvar = Intersection[Cases[cstmpeq,_FR$delta|_FR$deltaZ,{0,\[Infinity]}],totvar][[csincr]];
       If[Simplify[Coefficient[cstmpeq,csvar,1],TimeConstraint->0.1]=!=0,
         cstmpsol=csvar->Expand[-Coefficient[cstmpeq,csvar,0]/
           Simplify[Coefficient[cstmpeq,csvar,1],TimeConstraint->0.1]];
       ];
       csincr++;
     ];
     If[Length[cstmpsol]>0,cssol=Append[cssol/.cstmpsol,cstmpsol];]
   ];
 ];
cssol
];


(*vertex list ordering*)
Ordered2VertQ[v1_,v2_]:=Block[{vert1,vert2},
  vert1=Sort[(v1[[1]]/.-x_->x)[[All,1,1]]];
  vert2=Sort[(v2[[1]]/.-x_->x)[[All,1,1]]];
  If[vert1[[1]]<vert2[[1]],True,If[vert1[[1]]==vert2[[1]],vert1[[2]]<vert2[[2]],False]]];


(*check color conservation for 2 point*)
ColCons2Pt[arg_]:=If[Not[FreeQ[arg,Index[Colour,Ext[_]]]&&FreeQ[arg,Index[Gluon,Ext[_]]]],
  Print[Style["Error : two point function not propotional to a delta for external SU(3) indices",Red]];Abort[]];


SolveDelta[vertCT_,vertUV_,verttype_,assumlist_,complex_]:=Module[{fullUV,fl,fr,fsr,fsl,kuv,eqlist,deq,varlist,realvar,res,tRule,Cond,extra, su3rep,interres,
EpsSerie},

su3rep = {IndexDel[Index[Colour, Ext[1]], Index[Colour, Ext[2]]]->1,IndexDel[Index[Gluon, Ext[1]], Index[Gluon, Ext[2]]]->1};

para=False;
(*Cond avoid the replacement of if by piecewice or so*)
EpsSerie[arg_] := Block[{EStmp,ESkk,ESres,ESexp},
  ESexp=Expand[arg];
  If[Head[ESexp]=!=Plus,ESexp={ESexp};];
  If[para,
     ESres=Table[EStmp=ESexp[[ESkk]];
                 ParallelSubmit[{ESkk,EStmp},Normal[Series[(((EStmp/.If->Cond)/.Cond[test_,ca_,cb_]:>Cond[Simplify[(test/.Sqrt[x_^2/FR$MU^2]->x/FR$MU),
                   Assumptions->Join[{FR$MU>0,FR$IR>0,FR$Eps>0},assumlist], TimeConstraint->1],ca,cb])/.
                   {Cond[True,ca_,cb_]->ca,Cond[False,ca_,cb_]->cb}),{FR$Eps,0,0}]]],{ESkk,Length[ESexp]}];
  Plus@@(WaitAll[ESres])
  ,
  ESres=(Normal[Series[(((#/.If->Cond)/.Cond[test_,ca_,cb_]:>Cond[Simplify[(test/.Sqrt[x_^2/FR$MU^2]->x/FR$MU),
                   Assumptions->Join[{FR$MU>0,FR$IR>0,FR$Eps>0},assumlist], TimeConstraint->1]/.Sqrt[x_^2]->x,ca,cb])/.
                   {Cond[True,ca_,cb_]->ca,Cond[False,ca_,cb_]->cb}),{FR$Eps,0,0}]]&)/@ESexp;
  If[Head[ESres]=!=Plus,ESres=Plus@@ESres;];
  ESres
  ]];

fullUV=MergeVertList[RemovedZ/@vertCT,vertUV][[All,{1,3}]];
(*remove p1 using momentum conservation*)
fullUV=fullUV/.{TensDot[a___,SlashedP[FourMomentum[1]], b___][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]->
              -TensDot[a,SlashedP[FourMomentum[2]], b][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]};
fullUV=fullUV/.{FourMomentum[1]->-FourMomentum[2]};

fullUV=Sort[fullUV,Ordered2VertQ];
res={};

For[kuv=1,kuv<=Length[fullUV]-0,kuv++,
(*Print[ToString[kuv]<>" is "<>ToString[fullUV[[kuv,1]]]<>" at "<>ToString[SessionTime[]]];Print[fullUV[[kuv,2]]];*)
   If[kuv==1||Not[Sort[(fullUV[[kuv,1]]/.-x_->x)[[All,1,1]]]===Sort[(fullUV[[kuv-1,1]]/.-x_->x)[[All,1,1]]]],eqlist={};];
   Switch[verttype,
     {F,F},
     (*fermions*)
     fl=Coefficient[fullUV[[kuv,2]],TensDot[SlashedP[FourMomentum[2]], ProjM][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]];
     fr=Coefficient[fullUV[[kuv,2]],TensDot[SlashedP[FourMomentum[2]], ProjP][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]];
     fsl=Coefficient[fullUV[[kuv,2]],ProjM[Index[Spin, Ext[1]], Index[Spin, Ext[2]]]];
     fsr=Coefficient[fullUV[[kuv,2]],ProjP[Index[Spin, Ext[1]], Index[Spin, Ext[2]]]];
     

     (*check a piori on the form of the vertex*)
     On[Simplify::time];
     If[Not[Simplify[fullUV[[kuv,2]]-fl*TensDot[SlashedP[FourMomentum[2]], ProjM][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]-
                    fr*TensDot[SlashedP[FourMomentum[2]], ProjP][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]-
                    fsl ProjM[Index[Spin, Ext[1]], Index[Spin, Ext[2]]]- fsr ProjP[Index[Spin, Ext[1]], Index[Spin, Ext[2]]],TimeConstraint->0.2]===0],
       Print["warning: FF vertex does not seem match expected form, further simplification may be needed"]];
     Off[Simplify::time];

     interres = If[TheMass[fullUV[[kuv,1,1,1]]]===0&&TheMass[fullUV[[kuv,1,2,1]]]===0,fl,fl*TheMass[fullUV[[kuv,1,2,1]]]+fsr]/.su3rep;
     ColCons2pt[interres];
     interres = EpsSerie[interres/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,2,1]]]^2];
     eqlist=Append[eqlist,interres];
     interres = If[TheMass[fullUV[[kuv,1,1,1]]]===0&&TheMass[fullUV[[kuv,1,2,1]]]===0,fr,fr*TheMass[fullUV[[kuv,1,2,1]]]+fsl]/.su3rep;
     ColCons2pt[interres];
     interres = EpsSerie[interres/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,2,1]]]^2];
     eqlist=Append[eqlist,interres];
     (*If[Not[FreeQ[fullUV[[kuv,1,1]],fullUV[[kuv,1,2,1]]]], be careful for non diagonal piece*)
     deq=Simplify[D[(TheMass[fullUV[[kuv,1,1,1]]](fr+fl)+fsl+fsr),SP[FourMomentum[2], FourMomentum[2]]],TimeConstraint->0.01];
     deq=(deq/.Derivative[0, 0, 1, 0][IntxnLog][aa_, bb_, cc_, dd_]:>Dp2IntxnLog[aa,bb,cc,dd]);
     deq=2*TheMass[fullUV[[kuv,1,1,1]]]*deq+(fr+fl)/.su3rep;
     ColCons2pt[deq];
     deq=EpsSerie[deq/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,1,1]]]^2];
     If[DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,_],\[Infinity]]===DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,_],\[Infinity]]||
       DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,_],\[Infinity]]===-DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,_],\[Infinity]](*Same particle or particle and antiparticle*),
       eqlist=Append[eqlist,deq];
     ];
     (*Print[InputForm[{eqlist}]];*)
     ,
	 {S,S},
     If[DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,_],\[Infinity]]===DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,_],\[Infinity]]||
          DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,_],\[Infinity]]===-DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,_],\[Infinity]](*Same particle or particle and antiparticle*),
       If[FreeQ[FR$GoldstoneList,fullUV[[kuv,1,1,1]]]&&FreeQ[FR$GoldstoneList,-fullUV[[kuv,1,1,1]]](*Not a goldstone boson*),
         interres=fullUV[[kuv,2]]/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,1,1]]]^2;
         eqlist=Append[eqlist,EpsSerie[interres]];
       ];
       deq=D[(fullUV[[kuv,2]]),SP[FourMomentum[2], FourMomentum[2]]];
       deq=EpsSerie[(deq/.Derivative[0, 0, 1, 0][IntxnLog][aa_, bb_, cc_, dd_]:>Dp2IntxnLog[aa,bb,cc,dd])/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,1,1]]]^2];
       eqlist=Append[eqlist,deq];, 
       (*different scalars*)
       interres = EpsSerie[(fullUV[[kuv,2]]/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,1,1]]]^2)];
       eqlist=Append[eqlist,interres];
       interres = EpsSerie[(fullUV[[kuv,2]]/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,2,1]]]^2)];
       eqlist=Append[eqlist,interres];
     ];,
     {V,V},
     If[FreeQ[DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,xx_],\[Infinity]],DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,xx_],\[Infinity]]],
       (*different particles*)
       deq = Coefficient[fullUV[[kuv,2]],ME[Index[Lorentz,Ext[1]],Index[Lorentz,Ext[2]]]];
       eqlist=Append[eqlist,(EpsSerie[deq/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,1,1]]]^2])];
       eqlist=Append[eqlist,(EpsSerie[deq/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,2,1]]]^2])];
       ,
       (*Same particle*)
       deq = Coefficient[fullUV[[kuv,2]],ME[Index[Lorentz,Ext[1]],Index[Lorentz,Ext[2]]]];
       If[Not[TheMass[fullUV[[kuv,1,1,1]]]===0],
         (*Massive*)
         eqlist=Append[eqlist,(EpsSerie[deq/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,1,1]]]^2])];
       ];
       deq=D[deq,SP[FourMomentum[2], FourMomentum[2]]];
       deq=(deq/.Derivative[0, 0, 1, 0][IntxnLog][aa_, bb_, cc_, dd_]:>Dp2IntxnLog[aa,bb,cc,dd]);
       deq=deq/.SP[FourMomentum[2],FourMomentum[2]]->TheMass[fullUV[[kuv,1,1,1]]]^2;
       deq=EpsSerie[deq];
       eqlist=Append[eqlist,deq];
    ];
  ];(*end switch*)

  varlist=Union[If[TheMass[fullUV[[kuv,1,1,1]]]===TheMass[fullUV[[kuv,1,2,1]]],Cases[eqlist,FR$delta[{TheMass[fullUV[[kuv,1,1,1]]]},{}],\[Infinity]],{}],Cases[eqlist,_FR$deltaZ,\[Infinity]]];
  realvar=If[complex,{},Cases[varlist,FR$deltaZ[{x_,x_},b__]]];
  extra=Complement[Union[Cases[fullUV[[kuv,2]],_FR$deltaZ,\[Infinity]]],varlist];

  eqlist=(Refine[#/.((Rule[Conjugate[#],#]&)/@realvar)(*/.FR$IR->1*),Assumptions->Append[assumlist,FR$MU>0],TimeConstraint->0.0002]&)/@eqlist;
  If[Length[varlist]+Length[extra]<1&&Not[And@@((#==0&)/@eqlist)===True],
    Print[Style["Warning : no counterterms for this vertex "<>ToString[fullUV[[kuv,1]]] <> ", the following terms should vanish",Red]];
    Print[InputForm[eqlist]];
  , 
    (*Ignore conjugate vertex for boson since they give the same renormalisation condition*)
    If[kuv<Length[fullUV]&&(Sort[(fullUV[[kuv,1]]/.-x_->x)[[All,1,1]]]===Sort[(fullUV[[kuv+1,1]]/.-x_->x)[[All,1,1]]]&&Not[verttype==={F,F}]),kuv++;];
    If[kuv==Length[fullUV]||Not[Sort[(fullUV[[kuv,1]]/.-x_->x)[[All,1,1]]]===Sort[(fullUV[[kuv+1,1]]/.-x_->x)[[All,1,1]]]],
      res=Join[res,ConsSolve[eqlist/.{If->Cond,FR$IR->1},varlist],If[Length[extra]>0,(#->0&)/@extra,{}]];  (*set to zero no usefull delta(Z)*)
    ];
    
  ];
(*Print["before join"];Print[InputForm[If[Length[extra]>0,(#->0&)/@extra,{}]]];*)
];(*end for*)

res=Flatten[res/.Rule->tRule/.{tRule[FR$deltaZ[{a_,b_},xx__],-Conjugate[FR$deltaZ[{b_,a_},xx__]]]->{tRule[FR$deltaZ[{a,b},xx],0],tRule[FR$deltaZ[{b,a},xx],0]}}]/.
  tRule->Rule;
(Refine[#,Assumptions->Append[assumlist,FR$MU>0],TimeConstraint->0.001]&)/@(res/.{Cond->If,If[complex,FR$Re->1,FR$Re->0]})

];


(* ::Subtitle:: *)
(*R2*)


(*Performs q integration*)


(* ::Subsubtitle::Closed:: *)
(*Tadpoles*)


R2Tadpoles::usage="compute the R2 for a Tadpole amplitude after the non contributing terms have been removed"


R2Tadpoles[num_,del_,next_,UVcounter_] := Block[{tmp, coef},
   coef = Times @@ Cases[num, _?((FreeQ[#, MatrixTrace] && FreeQ[#, MetricTensor] && FreeQ[#, FourVector]) &)];
   tmp = num/coef;
   tmp = Expand[tmp /. {MatrixTrace -> DTr, MetricTensor -> ME, FourVector -> FV}];
   Simplify[-2 I \[Pi]^2*coef*del*Coefficient[Normal[Series[tmp /. {M$dim -> 4 + M$eps}, {M$eps, 0, 1}]], M$eps]]*FR$R2-2 I \[Pi]^2*coef*del*Simplify[(FR$UV(1/FR$Eps*
   Normal[Series[(tmp(1-FR$Eps/2)/.M$dim->4+FR$Eps),{FR$Eps,0,If[next<=2&&UVcounter,1,0]}]]+ If[next<=2&&UVcounter,1,0]*UVLog[del/FR$MU^2]/2*tmp/.{M$dim->4}))] /. {DTr -> MatrixTrace, ME -> MetricTensor, 
   FV -> FourVector,LCivita->LeviCivita}
];


(* ::Subsubtitle::Closed:: *)
(*Bubbles*)


R2BubblesF::usage="compute the R2 for a bubble amplitude with no occurence of the loop momentum on the numerator after the non contributing terms have 
been removed"
R2BubblesQ2::usage="compute the R2 for a bubble amplitude with two occurence of the loop momentum on the numerator after the non contributing terms have 
been removed"


R2BubblesF[num_,del_,next_,UVcounter_]:=Block[{tmp,coef},
  coef=Times@@Cases[num,_?((FreeQ[#,MatrixTrace]&&FreeQ[#,MetricTensor]&&FreeQ[#,FourVector]&&FreeQ[#,FermionChain])&)];
  tmp=num/coef;
  tmp=Expand[tmp/.{MatrixTrace->DTr,MetricTensor->ME,FourVector->FV,FermionChain->FCh}];
  tmp=-2I \[Pi]^2*(Simplify[Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]]FR$R2 + 
               FR$UV*Simplify[1/FR$Eps*Normal[Series[(tmp/.M$dim->4+FR$Eps),{FR$Eps,0,If[next===2&&UVcounter,1,0]}]]+ If[next===2&&UVcounter,1,0]*(Normal[Series[Log[del/FR$MU^2]/2*tmp/.{M$dim->4+FR$Eps},{FR$Eps,0,1}]])]/.
               {DTr->MatrixTrace,ME->MetricTensor,FV->FourVector,FCh->FermionChain,LCivita->LeviCivita});
  coef*tmp
];

R2BubblesQ2[num_,lm_,del_,next_,UVcounter_]:=Block[{tmp,coef},
  coef=Times@@Cases[num,_?((FreeQ[#,MatrixTrace]&&FreeQ[#,MetricTensor]&&FreeQ[#,FourVector]&&FreeQ[#,FermionChain])&)];
  tmp=num/coef;
  tmp=Expand[tmp/.{MatrixTrace->DTr,MetricTensor->ME,FourVector->FV,FermionChain->FCh}];
  tmp=tmp/.red2v[lm];
  tmp=Simplify[-4I \[Pi]^2*Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]-I \[Pi]^2tmp/.{M$dim->4}]*del*FR$R2 - 
                I \[Pi]^2*del FR$UV*Simplify[(1/FR$Eps*Normal[Series[((4-FR$Eps)tmp/.M$dim->4+FR$Eps),{FR$Eps,0,If[next===2&&UVcounter,1,0]}]]+ 
                  If[next===2&&UVcounter,1,0]*Normal[Series[(4-FR$Eps)Log[del/FR$MU^2]/2*tmp/.{M$dim->4+FR$Eps},{FR$Eps,0,1}]])]/.
               {SP[lm,lm]->1}/.{DTr->MatrixTrace,ME->MetricTensor,FV->FourVector,FCh->FermionChain,LCivita->LeviCivita};
  coef*tmp
];


(* ::Subsubtitle::Closed:: *)
(*Triangles*)


R2FTriangles::usage="compute the R2 for a Triangle amplitude with only fermions in the loop after the non contributing terms have been removed"
R2FBTriangles::usage="compute the R2 for a Triangle amplitude with both fermions and bosons in the loop after the non contributing terms have been removed"
R2BTriangles::usage="compute the R2 for a Triangle amplitude with only bosons in the loop after the non contributing terms have been removed"


R2FTriangles[num_,lm_]:=Block[{tmp,coef},
  tmp=Cases[num,_MatrixTrace][[1]];
  coef=Times@@Cases[num,Except[_MatrixTrace]];
  tmp=tmp/.{MatrixTrace->DTr};
  tmp=Expand[Expand[tmp]/.red2v[lm]];
  I \[Pi]^2*coef (Simplify[-2Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]-1/2(tmp/.M$dim->4)]FR$R2- 
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,DTr->MatrixTrace,FV->FourVector,LCivita->LeviCivita}
];

R2FBTriangles[num_,lm_]:=Block[{tmp,coef},
  tmp=Times@@Cases[num,_?(Not[FreeQ[#,lm]]&)];
  coef=num/tmp;
  tmp=Times@@Cases[coef,_?(Not[FreeQ[#,MetricTensor]]&)]*tmp;
  coef=num/tmp;
  tmp=Expand[tmp/.{FermionChain->FCh,MetricTensor->ME,FourVector->FV}];
  tmp=Expand[Expand[tmp]/.red2v[lm]];
  I \[Pi]^2*coef (Simplify[-2Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]-1/2(tmp/.M$dim->4)]FR$R2- 
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,DTr->MatrixTrace,FV->FourVector,FCh->FermionChain,LCivita->LeviCivita}
];

R2BTriangles[num_,lm_]:=Block[{coef, tmp},
  tmp=Times@@Cases[num,_?(Not[FreeQ[#,lm]]&)];
  coef=num/tmp;
  tmp=Times@@Cases[coef,_?(Not[FreeQ[#,MetricTensor]]&)]*tmp;
  coef=num/tmp;
  tmp=Expand[tmp]/.{MetricTensor->ME}/.{FourVector->FV};
  tmp=Expand[tmp]/.red2v[lm];
  tmp=Expand[tmp];
  I \[Pi]^2*coef (Simplify[-2Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]-1/2(tmp/.M$dim->4)]FR$R2- 
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,DTr->MatrixTrace,FV->FourVector,LCivita->LeviCivita}
];


(* ::Subsubtitle::Closed:: *)
(*Boxes*)


R2FBoxes::usage="compute the R2 for a boxe amplitude with only fermions in the loop after the non contributing terms have been removed"
R2BBoxes::usage="compute the R2 for a boxe amplitude with only bosons in the loop after the non contributing terms have been removed"


R2FBoxes[num_,lm_]:=Block[{tmp,coef},
  tmp=Cases[num,_MatrixTrace][[1]];
  coef=Times@@Cases[num,Except[_MatrixTrace]];
  tmp=tmp/.{MatrixTrace->DTr};
  tmp=Simplify[Expand[Expand[tmp]/.red4v[lm]/.red2v[lm]]];
  I \[Pi]^2*coef (Simplify[-2Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]-5/6(tmp/.M$dim->4)]FR$R2- 
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,FV->FourVector,LCivita->LeviCivita}
];

R2BBoxes[num_,lm_]:=Block[{coef, tmp},
  tmp=Times@@Cases[num,_?(Not[FreeQ[#,lm]]&)];
  coef=num/tmp;
  tmp=Times@@Cases[coef,_?(Not[FreeQ[#,MetricTensor]]&)]*tmp;
  coef=num/tmp;
  tmp=Expand[tmp]/.{MetricTensor->ME}/.{FourVector->FV}/.red4v[lm]/.red2v[lm];
  tmp=Expand[tmp];
  I \[Pi]^2*coef (Simplify[-2Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]-5/6(tmp/.M$dim->4)]FR$R2- 
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,FV->FourVector,LCivita->LeviCivita}
];


(* ::Subtitle::Closed:: *)
(*GetR2*)


GetR2::usage="compute the R2 for a FeynArts amplitude at the generic level"


GetR2[amp_,next_,UVfin_]:=Block[{temp,loopmom,nden,den,num,kk,extmom,intmom,lmcoef,DisFC,DisMT,DisDS,DisNC,nlmom,x,y,z,rl,lr,delta},
  loopmom=amp[[2,1]];
  den=Cases[amp[[3]],_PropagatorDenominator,\[Infinity]];
  den=Cases[den,_?(Not[FreeQ[#,loopmom]]&),1];
  nden=Length[den];
  num=amp[[3]]/.{FeynAmpDenominator[__]->1}/.LeviCivita->LCivita;
  Switch[nden,
    1,
    num=num/.{NonCommutative[DiracSlash[loopmom] + Mass[yy_,Loop]]->Mass[yy,Loop],NonCommutative[DiracSlash[-loopmom] + Mass[yy_,Loop]]->Mass[yy,Loop]}/.
    {loopmom->0,FourVector[0,x_]->0};
    num=R2Tadpoles[num,den[[1,2]]^2,next,UVfin];
    ,
    2,
    extmom=(Coefficient[#,loopmom]&/@Cases[den,\!\(TraditionalForm\`PropagatorDenominator\)[a_,b_]->a]) . Cases[den,\!\(TraditionalForm\`PropagatorDenominator\)[a_,b_]->a]-2loopmom;
    delta=SP[extmom,extmom]x*(x-1)+Cases[den,_?(Not[FreeQ[#,extmom]]&)][[1,2]]^2*x+Cases[den,_?(FreeQ[#,extmom]&)][[1,2]]^2*(1-x);

    If[FreeQ[num,F],
      lmcoef=Cases[num,_?(Not[FreeQ[#,loopmom]]&)];
      If[Length[lmcoef]==2,
        num={num/Times@@lmcoef*(Times@@lmcoef/.{loopmom->intmom}/.{_FourMomentum->0}/.{intmom->loopmom}), num/.{loopmom->-x*extmom}};
        ,
        If[Length[lmcoef]==1,
          num={0,num/.{loopmom->-x*extmom}};
          ,
          num={0,num};
          ];
        ];
      ,
      If[Count[num,Mass[F[__],Loop],\[Infinity]]==2,
        num={num/.{NonCommutative[DiracSlash[xx_] + Mass[yy__,Loop]]:>NonCommutative[DiracSlash[loopmom]]},(x (x-1)num/.
             {NonCommutative[DiracSlash[xx_] + Mass[yy_,Loop]]:>NonCommutative[DiracSlash[extmom] ]})+num/.
             {NonCommutative[DiracSlash[xx_] + Mass[yy_,Loop]]:> Mass[yy,Loop]}};
        ,
        num={0,(num/.{NonCommutative[DiracSlash[xx_] + Mass[yy_,Loop]]:>NonCommutative[DiracSlash[xx/.{loopmom->0}] + Mass[yy,Loop]]})-
            (x*num/.{NonCommutative[DiracSlash[xx_] + Mass[yy_,Loop]]:>NonCommutative[DiracSlash[Coefficient[xx,loopmom]*extmom]]})};
        ];
      ];
    num=R2BubblesQ2[num[[1]],loopmom,delta,next,UVfin]+R2BubblesF[num[[2]],delta,next,UVfin];
    num=If[next===2,XIntegrate[num,delta,x],Integrate[num,{x,0,1}]];
    ,
    3,
    extmom=(Coefficient[#,loopmom]&/@Cases[den,\!\(TraditionalForm\`PropagatorDenominator\)[a_,b_]->a])*Cases[den,\!\(TraditionalForm\`PropagatorDenominator\)[a_,b_]->a]-loopmom;
    extmom=DeleteCases[extmom,0] . {x,y};
    If[FreeQ[num,F],
      lmcoef=Cases[num,_?(Not[FreeQ[#,loopmom]]&)];
      If[Length[lmcoef]==3,
        num=num/Times@@lmcoef*
            ((Times@@lmcoef[[1;;2]]/.{loopmom->intmom}/.{_FourMomentum->0}/.{intmom->loopmom})*(lmcoef[[3]]/.{loopmom->-extmom})+
            (Times@@lmcoef[[2;;3]]/.{loopmom->intmom}/.{_FourMomentum->0}/.{intmom->loopmom})*(lmcoef[[1]]/.{loopmom->-extmom})+
            (Times@@lmcoef[[{1,3}]]/.{loopmom->intmom}/.{_FourMomentum->0}/.{intmom->loopmom})*(lmcoef[[2]]/.{loopmom->-extmom}));
        num=R2BTriangles[num,loopmom];
        num=2*Integrate[num,{x,0,1},{y,0,1-x}];
        ,
        If[Length[lmcoef]==2,
          num=num/Times@@lmcoef*(Times@@lmcoef/.{loopmom->intmom}/.{_FourMomentum->0}/.{intmom->loopmom});
          num=R2BTriangles[num,loopmom];
          num=2*Integrate[num,{x,0,1},{y,0,1-x}];
          ,
          num=0;
          ];
        ];
      ,
      If[Count[num,Mass[F[_],Loop],\[Infinity]]==3,
        lmcoef=Cases[num,NonCommutative[DiracSlash[xx_] + Mass[yy_,Loop]],\[Infinity]];
        num=Table[num/.{lmcoef[[kk]]->(lmcoef[[kk]]/.(NonCommutative[DiracSlash[xx_] + Mass[yy__,Loop]]:>NonCommutative[DiracSlash[Coefficient[xx,loopmom]*loopmom]])),
                        lmcoef[[Mod[kk,3]+1]]->(lmcoef[[Mod[kk,3]+1]]/.(NonCommutative[DiracSlash[xx_] + Mass[yy__,Loop]]:>NonCommutative[DiracSlash[Coefficient[xx,loopmom]*loopmom]])),
                        lmcoef[[Mod[kk+1,3]+1]]->(lmcoef[[Mod[kk+1,3]+1]]/.{loopmom->-extmom})},{kk,1,3}];
        num=Total[(R2FTriangles[#,loopmom]&)/@num];
        num=2*Integrate[num,{x,0,1},{y,0,1-x}];
        ,
        If[Count[num,Mass[F[_],Loop],\[Infinity]]==2,
          num=num/.{NonCommutative[DiracSlash[xx_] + Mass[yy__,Loop]]:>NonCommutative[DiracSlash[Coefficient[xx,loopmom]*loopmom]]};
          num=R2FBTriangles[num,loopmom];
          num=2*Integrate[num,{x,0,1},{y,0,1-x}];
          ,
          lmcoef=Cases[num,_?((Not[FreeQ[#,loopmom]]&&FreeQ[#,FermionChain])&)];
          If[Length[lmcoef]==1,
            num=num/lmcoef[[1]]*(lmcoef[[1]]/.{loopmom->intmom}/.{_FourMomentum->0}/.{intmom->loopmom})/.
                {NonCommutative[DiracSlash[xx_] + Mass[yy__,Loop]]:>NonCommutative[DiracSlash[Coefficient[xx,loopmom]*loopmom]]};
            num=R2FBTriangles[num,loopmom];
            num=2*Integrate[num,{x,0,1},{y,0,1-x}];
            ,
            num=0;
            ];
          ];
        ];
      ];
    ,
    4,
    If[FreeQ[num,F],
      lmcoef=Cases[num,_?((Not[FreeQ[#,loopmom]])&)];
      If[Length[lmcoef]==4,
        num=num/Times@@lmcoef*(Times@@lmcoef/.{loopmom->intmom}/.{_FourMomentum->0}/.{intmom->loopmom});
        num=R2BBoxes[num,loopmom];
        ,
        num=0;
        ];
      ,
      If[Count[num,Mass[F[_],Loop],\[Infinity]]==4,
        num=num/.{NonCommutative[DiracSlash[xx_] + Mass[yy__,Loop]]:>NonCommutative[DiracSlash[Coefficient[xx,loopmom]*loopmom]]};
        num=R2FBoxes[num,loopmom];
        ,
        num=0;
        ];
      ];
    ,
    _,
    Print[Style["Warning : More that 4 propagators, this diagram is discarded ",Orange]];
    ];(*end of Switch*)
  num

];(*end of getR2*)


(* ::Subtitle:: *)
(*Get Several R2*)


(* from generic to class in FR format*)


(* ::Subsubtitle:: *)
(*R2atClass*)


R2atClass::usage="compute the R2 for a FeynArts amplitude #1 and a topology #2 at the class level. All the contributions are summed but the wave functions keep track of the external particles.
#3 is the list of the generic external particles (F,S,V). If #4 is True, each contribution is multiplyed by IPL[list of the particles in the loop]. The fifth argument is the list of indices to
be kept in IPL like generation. Only qcd corrections are kept if the 6th argument is true and the finite part of the UV counterterms is drop if the last argument is False"


R2atClass[Ampl_,INTopo_,verttype_,lab_,kept_,qcd_,UVfinite_]:=Block[{res,tmp,tmp2,top,gen,rf,rl,fc,scalar,intern,nExtC,ll}, 
  res=0;
  For[kk=1,kk<=Length[Ampl],kk++,
    top=Ampl[[kk,1,1,2]];
    gen=Ampl[[kk,1,2,2]];
    tmp=GetR2[Ampl[[kk]],Length[verttype],UVfinite];
    If[Not[tmp===0],
      For[ll=1,ll<=Length[Ampl[[kk,4,2]]],ll++,
        scalar=1;
        fc=INTopo[[top,2,gen,2,ll]];
        rf={};
        If[OrderedQ[fc[[1;;Length[verttype],2]],(PartOrder[#1,#2]&)],
          For[nn=1,nn<=Length[Ampl[[0,1,2,1]]],nn++,
            rf=Append[rf,Rule[Ampl[[0,1,2,1,nn,1]][Index[Generic,fc[[nn,1,1]]]],fc[[nn,2]]]];
            If[Ampl[[0,1,2,1,nn,1]]===S,
              scalar=scalar*SWF[fc[[nn,2]],fc[[nn,1,1]]];
            ];
            If[Ampl[[0,1,2,1,nn,1]]===F,
              rf=Append[rf,Rule[SpinorType[Index[Generic,fc[[nn,1,1]]]],SpinorType[fc[[nn,2]]]]];
            ];
          ];
          rl=Table[Rule[Ampl[[kk,4,1,nn]],Ampl[[kk,4,2,ll,nn]]],{nn,1,Length[Ampl[[kk,4,1]]]}];
          rl=rl/.{FourVector->FV}/.{FV->FourVector};
          rl=rl/.IndexSum[xx__, {bb_?(StringMatchQ[ToString[#],"Gluon$*"]&), 1, 8}]:>(NLO$ngluon++;SumOver[bb,8]*xx/.
               {bb->Index[Gluon,NLO$ngluon*10+ToExpression[StringCases[ToString[bb],DigitCharacter..][[1]]]]});
          rl=rl/.IndexSum[xx__, {bb_?(StringMatchQ[ToString[#],"Colour$*"]&), 1, 3}]:>(SumOver[bb,3]*xx/.
               {bb->Index[Gluon,NLO$ncolour*10+ToExpression[StringCases[ToString[bb],DigitCharacter..][[1]]]]});
          intern=If[lab,IPL[Sort[List@@Union[DeleteCases[fc[[Length[verttype]+1;;,2]]/.{-1->1},Index[Except[Alternatives@@kept],__],\[Infinity]]],PartOrder]],1];
          tmp2=scalar*intern*tmp/.{FourVector->FV}/.{FV->FourVector}/.rl/.rf/.If[Length[M$FACouplings]>0,M$FACouplings,{}];
          If[qcd,
            nExtC={Length[Union[Cases[tmp2,Index[Colour,a_?(#<=Length[verttype]&)],\[Infinity]]]],Length[Union[Cases[tmp2,Index[Gluon,a_?(#<=Length[verttype]&)],\[Infinity]]]]};
            tmp2= Total[Table[Coefficient[tmp2,GS,nn]*GS^nn,{nn,Max[{2,If[nExtC[[2]]>0,nExtC[[1]]+nExtC[[2]],2]}],4}]];
          ];
          res=res+tmp2;
        ];
      ];
    ];
  ];
  res
]


(* ::Subsubtitle:: *)
(*CTatClass*)


CTatClass::usage="compute the CT for a FeynArts amplitude #1 and a topology #2 at the class level. All the contributions are summed but the wave functions keep track of the external particles.
#3 is the list of the generic external particles (F,S,V). "


CTatClass[Ampl_,INTopo_,verttype_]:=Block[{res,tmp,tmp2,top,gen,rf,rl,fc,scalar,ll}, 
  res=0;
  For[kk=1,kk<=Length[Ampl],kk++,
    top=Ampl[[kk,1,1,2]];
    gen=Ampl[[kk,1,2,2]];
    tmp=Simplify[Expand[Ampl[[kk,3]]/.{FermionChain->FCh}]]/.{FCh->FermionChain};
    
    If[Not[tmp===0],
      For[ll=1,ll<=Length[Ampl[[kk,4,2]]],ll++,
        scalar=1;
        fc=INTopo[[top,2,gen,2,ll]];
        rf={};
        If[OrderedQ[fc[[1;;Length[verttype],2]],(PartOrder[#1,#2]&)],
          For[nn=1,nn<=Length[Ampl[[0,1,2,1]]],nn++,
            rf=Append[rf,Rule[Ampl[[0,1,2,1,nn,1]][Index[Generic,fc[[nn,1,1]]]],fc[[nn,2]]]];
            If[Ampl[[0,1,2,1,nn,1]]===S,
              scalar=scalar*SWF[fc[[nn,2]],fc[[nn,1,1]]];
            ];
            If[Ampl[[0,1,2,1,nn,1]]===F,
              rf=Append[rf,Rule[SpinorType[Index[Generic,fc[[nn,1,1]]]],SpinorType[fc[[nn,2]]]]];
            ];
          ];
          rl=Table[Rule[Ampl[[kk,4,1,nn]],Ampl[[kk,4,2,ll,nn]]],{nn,1,Length[Ampl[[kk,4,1]]]}];
          rl=rl/.{FourVector->FV}/.{FV->FourVector};
          rl=rl/.IndexSum[xx__, {bb_?(StringMatchQ[ToString[#],"Gluon$*"]&), 1, 8}]:>(NLO$ngluon++;SumOver[bb,8]*xx/.
               {bb->Index[Gluon,NLO$ngluon*10+ToExpression[StringCases[ToString[bb],DigitCharacter..][[1]]]]});
          rl=rl/.IndexSum[xx__, {bb_?(StringMatchQ[ToString[#],"Colour$*"]&), 1, 8}]:>(SumOver[bb,8]*xx/.
               {bb->Index[Colour,NLO$ncolour*10+ToExpression[StringCases[ToString[bb],DigitCharacter..][[1]]]]});
          tmp2=scalar*tmp/.{FourVector->FV}/.{FV->FourVector}/.rl/.rf/.If[Length[M$FACouplings]>0,M$FACouplings,{}];
          
          res=res+tmp2;
        ];
      ];
    ];
  ];
  res*FR$UV
]


(* ::Subsubtitle::Closed:: *)
(*PartOrder*)


PartOrder::usage="Ordering function for the particles, fermion goes before vector which goes before scalar, antiparticles goes before particles of the same type, remaining ordering is done 
according to the particle numbers"


PartOrder[V[x_,ex1___],-V[y_,ex2___]]:=False;
PartOrder[V[x_,ex1___],V[y_,ex2___]]:=False/;y<x;
PartOrder[-V[x_,ex1___],-V[y_,ex2___]]:=False/;y<x;
PartOrder[-S[x_,ex1___]|S[x_,ex1___],V[y_,ex2___]|-V[y_,ex2___]]:=False;
PartOrder[S[x_,ex1___],-S[y_,ex2___]]:=False;
PartOrder[S[x_,ex1___],S[y_,ex2___]]:=False/;y<x;
PartOrder[-S[x_,ex1___],-S[y_,ex2___]]:=False/;y<x;
PartOrder[-F[x_,ex1___],-F[y_,ex2___]]:=False/;y<x;
PartOrder[F[x_,ex1___],F[y_,ex2___]]:=False/;y<x;
PartOrder[S[y_,ex1___]|V[y_,ex1___],F[x_,ex2___]]:=False;
PartOrder[-S[y_,ex1___]|-V[y_,ex1___],F[x_,ex2___]]:=False;
PartOrder[-Except[F,X_][y_,ex1___],-F[x_,ex2___]]:=False;
PartOrder[Except[Times,X_][y_,ex1___],-F[x_,ex2___]]:=False;
PartOrder[F[y_,ex1___],-F[x_,ex2___]]:=False;

PartOrder[V[x_,ex1___],-V[x_,ex2___]]:=False;
PartOrder[-S[x_,ex1___]|S[x_,ex1___],V[x_,ex2___]|-V[x_,ex2___]]:=False;
PartOrder[S[x_,ex1___],-S[x_,ex2___]]:=False;
PartOrder[S[x_,ex1___]|V[x_,ex1___],F[x_,ex2___]]:=False;
PartOrder[-S[x_,ex1___]|-V[x_,ex1___],F[x_,ex2___]]:=False;
PartOrder[X_[x_,ex1___],-F[x_,ex2___]]:=False;
PartOrder[F[x_,ex1___],-F[x_,ex2___]]:=False;

PartOrder[x_,x_]:=True;
PartOrder[x_,y_]:=True;


(* ::Subsubtitle:: *)
(*R2vertlist*)


R2vertlist::usage="rewrite the result from Expand[R2atClass] into a list of vertices similar to those of FeynRules"


R2vertlist[vertsum_]:=Block[{vertList,lab,ver,pos,ss,v,ind,indrep,f,indlist,ind2,xx,ll,kk,nn,ff,mm,UVmass,deltarule,indl,aa,bb,s1,s2,extsum,wft,inveps,res},
  vertList={};
  If[Head[vertsum]===Times,res={vertsum};,res=vertsum];
  For[kk=1,kk<=Length[res],kk++,
    lab=Cases[res[[kk]],PolarizationVector[v_,FourMomentum[Incoming,ind_],Index[Lorentz,ind2_]]->{v,ind,ind2}];
    lab=Join[lab,Cases[res[[kk]],SWF[ss_,ind_]->{ss,ind}]];
    lab=Join[lab,Cases[res[[kk]],FermionChain[NonCommutative[SpinorType[f_][-FourMomentum[Incoming,ind_]|FourMomentum[Incoming,ind_],__]],xx__]->{f,ind,1},\[Infinity]]];
    lab=Join[lab,Cases[res[[kk]],FermionChain[xx__,NonCommutative[SpinorType[f_][-FourMomentum[Incoming,ind_]|FourMomentum[Incoming,ind_],__]]]->{f,ind,2},\[Infinity]]];
    lab=Sort[lab,PartOrder[#1[[1]],#2[[1]]]&];
    (*Majorana reordering*)
    If[lab[[1,1,0]]===F&&lab[[2,1,0]]===F&&lab[[1,1,1]]===lab[[2,1,1]]&&lab[[1,2]]>lab[[2,2]],lab[[1;;2]]=lab[[{2,1}]];];
    indrep=Table[FourMomentum[Incoming,lab[[ll,2]]]->FourMomentum[Incoming,ll],{ll,1,Length[lab]}];
    For[nn=1,nn<=Length[lab],nn++,
      If[MatchQ[lab[[nn,1]],V[__]|-V[__]],indrep=Append[indrep,  Index[Lorentz,lab[[nn,3]]]->Index[Lorentz,Ext[nn]]  ]; ];
      If[MatchQ[lab[[nn,1]],F[__]|-F[__]],indrep=Append[indrep,  Index[Spin,Ext[lab[[nn,3]]]]->Index[Spin,Ext[nn]]  ]; ];
      indlist=Cases[lab[[nn,1]],Index[xx_,ind2_],\[Infinity]];
      For[ll=1,ll<=Length[indlist],ll++,indrep=Append[indrep,indlist[[ll]]->Index[indlist[[ll,1]],Ext[nn]]];];
    ];
    ver=DeleteCases[res[[kk]],PolarizationVector[v_,FourMomentum[Incoming,ind_],Index[Lorentz,ind2_]]];
    ver=DeleteCases[ver,SWF[ss_,ind_]];
    ver=Replace[ver,{FermionChain[NonCommutative[SpinorType[-F[f__]|F[f__]][-FourMomentum[Incoming,ind_]|FourMomentum[Incoming,ind_],__]],xx___,
                     NonCommutative[SpinorType[F[f2__]|-F[f2__]][-FourMomentum[Incoming,ind2_]|FourMomentum[Incoming,ind2_],__]]]->
                      -TensDot[xx][Index[Spin,Ext[1]],Index[Spin,Ext[2]]]},\[Infinity]];

    ver=ver/.indrep;
(*Print[ver];*)
    lab[[All,2]]=Table[ll,{ll,1,Length[lab]}];
    lab[[All,1]]=lab[[All,1]]/.indrep;
    lab=Transpose[{lab[[All,1]],lab[[All,2]]}];

    If[FreeQ[vertList[[All,1]],lab],
      vertList=Append[vertList,{lab,(ver/.{FR$UV->0,FR$R2->1}),(ver/.{FR$R2->0,FR$UV->1})}];
      If[Length[lab]===2,UV$Wftlist=Append[UV$Wftlist,{lab[[1,1]],wft}]];
      ,
      pos=Position[vertList[[All,1]],lab][[1]];
      vertList[[pos,2]]=vertList[[pos,2]]+(ver/.{FR$UV->0,FR$R2->1});
      vertList[[pos,3]]=vertList[[pos,3]]+(ver/.{FR$UV->1,FR$R2->0});
      If[Length[lab]===2,
        pos=Position[UV$Wftlist[[All,1]],lab[[1,1]]][[1]];
        UV$Wftlist[[pos,2]]=UV$Wftlist[[pos,2]]+wft;
      ];
    ];
  ];
  vertList
]


(* ::Subsubtitle:: *)
(*ModelR2*)


R2vertlist::usage="generate the list of R2 vertices of a model and a generic model. If #3 is True, each contribution is multiplyed by IPL[list of the particles in the loop]. The last argument 
is the list of indices to be kept in IPL like generation."


DeltaToExp[x_]:=ToExpression[StringReplace[ToString[Head[x]]<>"x"<>StringJoin@@ToString/@Flatten[List@@x],"$"->"CT"]];


ModelR2[mod_,gen_,lab_,kept_,qcd_,zerom_,assumpt_,UVCT_,comas_,ctpa_,no4SCT_]:=Block[{vertlist,genver,kkpl,topo,INTopo,Ampl,tmp,tmpCT,x,ind,indl,rl1,rlf,tdrl,a,b,temp,ckt,topoCT,INTopoCT,AmplCT,CTSol,
vrl,vrl2,momk,vertCTlist,zmsol,Orderl,zmF,pos,check,kct,masslist={},ToParam,kkind,ntopo,vertl,CountC,tadpolerep={}},
CountC[x_]:=Count[x,Gluon|Colour,\[Infinity]];

ToParam[x_]:=If[LeafCount[x[[2]]]>200,CTparam=Append[CTparam,DeltaToExp[x[[1]]]->x[[2]]];x[[1]]->DeltaToExp[x[[1]]],x];

temp=SessionTime[];
  genver={{S},{F,F},{S,S},{V,V},{F,F,S},{F,F,V},{V,V,V},{V,V,S},{V,S,S},{S,S,S},{V,V,V,V},{V,V,S,S},{S,S,S,S}};
  vertlist={};
  vertCTlist={};
  CTSol={};
  tdrl={TensDot[ProjP][s1_,s2_]->ProjP[s1,s2],TensDot[ProjM][s1_,s2_]->ProjM[s1,s2],TensDot[Ga[mu_]][s1_,s2_]->Ga[mu,s1,s2]};
  rlf={TensDot[gm1:_Ga|_SlashedP,gm2:_Ga|_SlashedP,ProjP][Index[Spin,Ext[2]],Index[Spin,Ext[1]]]->-TensDot[gm2,gm1,ProjP][Index[Spin,Ext[1]],Index[Spin,Ext[2]]],
       TensDot[gm1:_Ga|_SlashedP,gm2:_Ga|_SlashedP,ProjM][Index[Spin,Ext[2]],Index[Spin,Ext[1]]]->-TensDot[gm2,gm1,ProjM][Index[Spin,Ext[1]],Index[Spin,Ext[2]]],
       TensDot[gm:_Ga|_SlashedP,ProjP][Index[Spin,Ext[2]],Index[Spin,Ext[1]]]->TensDot[gm,ProjM][Index[Spin,Ext[1]],Index[Spin,Ext[2]]],
       TensDot[gm:_Ga|_SlashedP,ProjM][Index[Spin,Ext[2]],Index[Spin,Ext[1]]]->TensDot[gm,ProjP][Index[Spin,Ext[1]],Index[Spin,Ext[2]]],
       ProjP[Index[Spin,Ext[2]],Index[Spin,Ext[1]]]->-ProjP[Index[Spin,Ext[1]],Index[Spin,Ext[2]]],
       ProjM[Index[Spin,Ext[2]],Index[Spin,Ext[1]]]->-ProjM[Index[Spin,Ext[1]],Index[Spin,Ext[2]]]};
  
  vrl={MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[3]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[3]],Index[Gluon,Ext[2]],Index[Gluon,Ext[4]]]->
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[3]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]],Index[Gluon,Ext[4]]]+
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[3]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[4]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]]],
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]],Index[Gluon,Ext[4]]]->
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[3]],Index[Gluon,Ext[2]],Index[Gluon,Ext[4]]]-
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[4]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]]],
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[4]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[4]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]]]->
       -MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[4]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]],Index[Gluon,Ext[4]]]+
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[4]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[3]],Index[Gluon,Ext[2]],Index[Gluon,Ext[4]]]};

  For[kkpl=1,kkpl<=Length[genver]-If[no4SCT,1,0],kkpl++,
    NLO$ngluon=0;
    NLO$ncolour=0;
    Print[Style["Computing CT for the "<>ToString[genver[[kkpl]]]<>" vertices.",Blue]];
    rl1={MetricTensor->ME,ScalarProduct[FourMomentum[Incoming,a_],FourMomentum[Incoming,b_]]->SP[FourMomentum[a],FourMomentum[b]],ScalarProduct[FourMomentum[Incoming,a_],FourMomentum[Incoming,a_]]->
        SP[FourMomentum[a],FourMomentum[a]],SP[FourMomentum[Incoming,a_],FourMomentum[Incoming,b_]]->SP[FourMomentum[a],FourMomentum[b]],
        SP[FourMomentum[Incoming,a_],FourMomentum[Incoming,a_]]->SP[FourMomentum[a],FourMomentum[a]],LeviCivita[x__]->Eps[x],
	    FourVector[FourMomentum[Incoming,ind_],Index[Lorentz,Ext[indl_]]]:>FV[FourMomentum[ind],Index[Lorentz,Ext[indl]]]/;indl<=Length[genver[[kkpl]]],
	    FourVector[-FourMomentum[Incoming,ind_],Index[Lorentz,Ext[indl_]]]:>-FV[FourMomentum[ind],Index[Lorentz,Ext[indl]]]/;indl<=Length[genver[[kkpl]]],
        NonCommutative[ChiralityProjector[1]]->ProjP,NonCommutative[ChiralityProjector[-1]]->ProjM,NonCommutative[DiracSlash[FourMomentum[Incoming,x_]]]->
        SlashedP[FourMomentum[x]],NonCommutative[DiracMatrix[Index[Lorentz,x_]]]->Ga[Index[Lorentz,x]],SumOver[x__,External]->1,EL->ee,GS->gs};

    topo = CreateTopologies[1,Length[genver[[kkpl]]]->0,Adjacencies->{3,4},ExcludeTopologies->{Internal}];
    INTopo = InsertFields[topo,genver[[kkpl]]->{},Model->mod,GenericModel->gen,ExcludeParticles->Join[If[genver[[kkpl]]==={S,S,S,S},{U},{}],{}],
                          ExcludeFieldPoints->If[Length[genver[[kkpl]]]>3,{FieldPoint[_][S,S,S],FieldPoint[_][S,V,V]},{}]];
    (*add assumptions for masses*)
    If[kkpl===1,masslist=DeleteCases[Union[(Mass/.#[[2]]&)/@M$ClassesDescription],0];];
    
    (*remove diagrams without at least one interaction with three or more color multiplets*)
    If[qcd,
      For[ntopo=1,ntopo<=Length[INTopo],ntopo++,
        vertl=((Cases[INTopo[[ntopo,1]],Propagator[x_][y___,#,z___,f_Field]->f,\[Infinity]]&)/@Union[Cases[INTopo[[ntopo,1]],Vertex[_?(#>2&)][x_],\[Infinity]]]);
       INTopo[[ntopo]]=DeleteCases[INTopo[[ntopo]],f_?(MatchQ[#,FeynmanGraph[_,Classes==_][x__]]&&(Max[CountC/@(vertl/.List@@#)]<3)&),\[Infinity]];
      ];
      INTopo=DeleteCases[DeleteCases[INTopo,_->Insertions[Classes][],{3}],_->Insertions[Generic][],{1}];
    ];

    Ampl= CreateFeynAmp[INTopo];
    For[ckt=1,ckt<=Length[Ampl],ckt++,Ampl[[ckt,4,2]]=Ampl[[ckt,4,2]]/.{aa_[bb__?(FreeQ[#,Colour]&),Index[Colour,cc_]]->aa[bb]};(*remove colour index in masses*)];
    Print["Writing amplitude finished after "<>ToString[SessionTime[]-temp]];
    tmp=R2atClass[Ampl,INTopo,genver[[kkpl]],lab,kept,qcd,UVCT];
    Print["Writing the R2 and UV parts at the class level finished after "<>ToString[SessionTime[]-temp]];
    tmp=R2vertlist[Expand[Expand[Expand[tmp,_SWF],_PolarizationVector],FermionChain]]/.IndexDelta->IndexDel;

	(*Counterterm amplitude*)
    If[UVCT,
      topoCT=CreateCTTopologies[1,Length[genver[[kkpl]]]->0,Adjacencies->{3,4},ExcludeTopologies->{Internal}];
      INTopoCT = InsertFields[topoCT,genver[[kkpl]]->{},Model->mod,GenericModel->gen];
      AmplCT= CreateFeynAmp[INTopoCT];
      tmpCT=CTatClass[AmplCT,INTopoCT,genver[[kkpl]]];
      tmpCT=R2vertlist[Expand[Expand[Expand[tmpCT,_SWF],_PolarizationVector],FermionChain]]/.IndexDelta->IndexDel;
    ];

    Print["Writing the symbolic CT finished after "<>ToString[SessionTime[]-temp]];
    tmp=ExpandAll[ExpandAll[tmp/.{SUNTSum[a_,b_,c_,d_]->IndexDel[a,d]IndexDel[c,b]/2- IndexDel[a,b]IndexDel[c,d]/6}/.{SUNT->SUT,SUNF->SUF},Colour],Gluon];
    tmp=ExpandAll[ExpandAll[tmp,Colour],Gluon];
    If[UVCT,
     tmpCT=ExpandAll[ExpandAll[tmpCT/.{SUNTSum[a_,b_,c_,d_]-> IndexDel[a,d]IndexDel[c,b]/2- IndexDel[a,b]IndexDel[c,d]/6}/.{SUNT->SUT,SUNF->SUF},Colour],SGluon];
     tmpCT=ExpandAll[ExpandAll[tmpCT,Colour],Gluon];
    ];
    (*Print["Expand finished after "<>ToString[SessionTime[]-temp]];*)
    (* Esthetic replacements*)
    vrl2={FourVector[FourMomentum[Incoming, a_Integer], Index[Lorentz, Ext[a_Integer]]]->-(Total[Table[FourVector[FourMomentum[Incoming,momk ], 
          Index[Lorentz, Ext[a]]],{momk,1,Length[genver[[kkpl]]]}]]-FourVector[FourMomentum[Incoming, a], Index[Lorentz, Ext[a]]])};
    tmp=tmp/.vrl/.vrl2;
    tmp=(Replace[#,(SumOver[Index[w:Gluon|Colour,x_],y_Integer]*z___):>y*Times[z]/;FreeQ[Times[z],Index[w,x]],{2}]&)/@tmp;
    If[UVCT,tmpCT=tmpCT/.vrl/.vrl2;];
    Print["SU3 algebra finished after "<>ToString[SessionTime[]-temp]];


    If[Length[tmp]>0||Length[tmpCT]>0,
      tmp[[All,2]]=I*(Simplify[ReplaceRepeated[#/.rl1/.tdrl/.IndexSum->Sum/.rlf,SumOver[ind_?((FreeQ[#,Gluon]&&FreeQ[#,Colour])&),x_Integer]*a__:>
        Total[Table[(Times[a])/.{ind->kkind},{kkind,1,x}]]],TimeConstraint->10]&)/@tmp[[All,2]];

      tmp[[All,3]]=(ReplaceRepeated[#/.rl1/.tdrl/.IndexSum->Sum/.rlf,SumOver[ind_?((FreeQ[#,Gluon]&&FreeQ[#,Colour])&),x_Integer]*a__:>
        Total[Table[(Times[a])/.{ind->kkind},{kkind,1,x}]]]&)/@tmp[[All,3]];
      If[UVCT,
        tmpCT[[All,3]]=(ReplaceRepeated[#/.rl1/.tdrl/.IndexSum->Sum/.rlf,SumOver[ind_?((FreeQ[#,Gluon]&&FreeQ[#,Colour])&),x_Integer]*a__:>
           Total[Table[(Times[a])/.{ind->kkpl},{kkpl,1,x}]]]&)/@tmpCT[[All,3]];

        If[Length[genver[[kkpl]]]===2,
          (*Only the finite piece in the renormalization constants*)
          tmp=tmp/.{TensDot[a___,SlashedP[FourMomentum[1]], b___][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]->
              -TensDot[a,SlashedP[FourMomentum[2]], b][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]}/.{FourMomentum[1]->-FourMomentum[2]};
          tmpCT=tmpCT/.{TensDot[a___,SlashedP[FourMomentum[1]], b___][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]->
              -TensDot[a,SlashedP[FourMomentum[2]], b][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]}/.{FourMomentum[1]->-FourMomentum[2]};
          (*Print["before CTSol close"];Print[InputForm[tmp]];Print[InputForm[tmpCT]];*)
          CTSol=Join[CTSol,
            SolveDelta[tmpCT,({#1,#2,Simplify[#3-Coefficient[#3,inveps,1]*inveps/.inveps->1/FR$Eps,TimeConstraint->0.01]}&)@@@(tmp/.FR$Eps->1/inveps),genver[[kkpl]],assumpt,comas]];

        ];
        (*Only the 1/Eps in the vertlist if not tadpole, minus sign because counter terms have opposite sign than amplitudes*)
        If[Length[genver[[kkpl]]]===1,
 	     tmp=(If[FreeQ[tmpCT[[All,1]],#[[1]]],Print["Remove the following tadpole because it cannot be absorded in a tadpole counterterm :"];Print[{#[[1]],#[[3]]}];{#[[1]],#[[2]],0},#]&)/@tmp; 
 	     tmp = DeleteCases[tmp,{_,0,0}]; 
 	     tadpolerep=MergeVertList[Expand[(tmp/.FR$Eps->1/inveps)]/.inveps->0,tmpCT]; 
 	     tadpolerep = DeleteCases[tadpolerep,_?(FreeQ[#,FR$deltat]&)]; 
 	     tadpolerep=(Cases[#,_FR$deltat,\[Infinity]][[1]]->-Coefficient[#[[3]],Cases[#,_FR$deltat,\[Infinity]][[1]],0]/Coefficient[#[[3]],Cases[#,_FR$deltat,\[Infinity]][[1]]]&)/@tadpolerep; 
 	     tmp[[All,3]]=-I*(Simplify[Coefficient[#/.FR$Eps->1/inveps,inveps,1]/FR$Eps,TimeConstraint->1]&)/@tmp[[All,3]];, 
 	     (*not tadpole*) 
          tmp[[All,3]]=-I*(Simplify[Coefficient[#/.FR$Eps->1/inveps,inveps,1]/FR$Eps,TimeConstraint->1]&)/@tmp[[All,3]];];
        tmpCT[[All,3]]=I*(Simplify[#/.{Conjugate[FR$deltaZ[{x_,x_},b__]]->FR$deltaZ[{x,x},b]},TimeConstraint->1]&)/@tmpCT[[All,3]];,(*Length[tmp]=0*)

		Print["kkpl is"];Print[kkpl];
        If[Length[tmpCT]>0&&Length[genver[[kkpl]]]===2,
          tmpCT[[All,3]]=(ReplaceRepeated[#/.rl1/.tdrl/.IndexSum->Sum/.rlf,SumOver[ind_?((FreeQ[#,Gluon]&&FreeQ[#,Colour])&),x_Integer]*a__:>Total[Table[(Times[a])/.{ind->kkpl},{kkpl,1,x}]]]&)/@tmpCT[[All,3]];
          Print[InputForm[tmpCT]];
          CTSol=Join[CTSol,SolveDelta[tmpCT,{},genver[[kkpl]],assumpt,comas]];
        ];];(*end If UVCT*)
      ];(*end if Length[tmp]>0*)
    Print["Simplification of the vertices finished after "<>ToString[SessionTime[]-temp]];
    
    vertlist=Join[vertlist,tmp];
    If[UVCT,vertCTlist=Join[vertCTlist,tmpCT];];
    Print["Vertices "<>ToString[genver[[kkpl]]]<>" finished after "<>ToString[SessionTime[]-temp]];


  ];
(*Print[InputForm[tadpolerep]];*)


zmsol={};
If[UVCT,
  Attributes[Orderl]={Orderless};
  For[kct=1,kct<=Length[zerom],kct++,
    pos=Position[Orderl@@@DeleteCases[DeleteCases[vertCTlist[[All,1,All,1]],Index[a_?(FreeQ[zerom,#]&),b__],\[Infinity]],{},\[Infinity]],Orderl@@zerom[[kct,2]]];
    zmF=DeleteCases[zerom[[kct,2]],_?(FreeQ[#,F]&)]/.(Reverse/@FR$ClassesTranslation)/.-x_->x;
    If[Not[Length[zmF]==2],Print[Style["Warning : the vertex "<>ToString[zerom[[kct,2]]]<>"do not contain two fermions",Orange]];];
    If[Length[pos]>0,
      pos=pos[[1,1]];
      If[FreeQ[vertCTlist[[pos,3]],zerom[[kct,1]]],
        Print[Style["Warning : the vertex "<>ToString[kct]<>" does not depends on FR$delta[{"<>ToString[zerom[[kct,1]]]<>"},{}] which finite part is then set to zero",Orange]],
        zmsol=Join[zmsol,Solve[(Coefficient[vertCTlist[[pos,3]]/.FR$deltaZ[zmF,x__]->0, TensDot[Ga[Index[Lorentz, Ext[3]]], ProjP][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]]/.CTSol)==0,FR$delta[{zerom[[kct,1]]},{}]][[1]]];
        If[Not[FreeQ[Last[zmsol],Lorentz]&&FreeQ[Last[zmsol],SP]&&FreeQ[Last[zmsol],Spin]],
          Print[Style["Error : Solution for "<>ToString[zerom[[kct,1]]]<>" counterterm depends on the kinematic, finite part is set to zero",Red]];
Print[InputForm[Simplify[(vertCTlist[[pos,3]]/.FR$deltaZ[zmF,x__]->0)/.CTSol,TimeConstraint->10]]];
Print[InputForm[Simplify[(vertCTlist[[pos,3]]/.FR$deltaZ[zmF,x__]->0),TimeConstraint->10]]];
          zmsol=Join[Delete[zmsol,-1],FR$delta[{zerom[[kct,1]]},{}]->0];
        ];
        If[FreeQ[Last[zmsol],Rule],
          Print[Style["Error : No solution for "<>ToString[zerom[[kct,1]]]<>" counterterm, finite part is set to zero",Red]];
          zmsol=Join[Delete[zmsol,-1],FR$delta[{zerom[[kct,1]]},{}]->0];
        ];
      ],
      Print[Style["warning : vertex "<>ToString[kct]<>" not found in the list of counterterms vertices, finite part of the counterterm of the parameter will set to zero",Orange]];
    ]
  ]
];
zmsol=(#[[1]]->Coefficient[#[[2]]*FR$Eps,FR$Eps]&)/@zmsol;

If[ctpa,CTSol=ToParam/@CTSol;tadpolerep=ToParam/@tadpolerep;];

(*Print[InputForm[zmsol]];Print[InputForm[CTSol]];*)

(*force delta m = 0 when m=0 *)
CTSol=CTSol/.Rule[FR$delta[{x_},{}],y_]:>Rule[FR$delta[{x},{}],Expand[y*Simplify[If[x==0,0,1],Assumptions->assumpt]]]/.{If[a_,1,0]*If[a_,0,1]->0,If[a_,0,1]^2->If[a,0,1]};

Print["before merge "<>ToString[SessionTime[]-temp]<>" temp="<>ToString[temp]];
vertlist=MergeVertList[vertlist,If[UVCT,(vertCTlist/.Dispatch[tadpolerep]/.Dispatch[zmsol]//.Dispatch[CTSol]),{}]]/.{FourMomentum[xx_]->xx};
Print["merging done "<>ToString[SessionTime[]-temp]];
check=Union[Cases[vertlist,_FR$delta,\[Infinity]]];
If[Length[check]>0,Print[Style["The finite part of those counterterms will be set to zero",Orange]];Print[check];
  vertlist=vertlist/.((Rule[#,0]&)/@check);
];
(*Print[InputForm[Cases[vertlist,{a_?(Not[FreeQ[#,9]]&&FreeQ[#,S[2]]&&FreeQ[#,S[3]]&),b__}]]];*)
vertlist/.{SUT->SUNT,SUF->SUNF,IndexDel->IndexDelta}
]


(* ::Subsubtitle:: *)
(*WriteCT*)


WriteCT::usage="Write the list of the R2 and UV vertices of the model and generic model in the file named #3.fr2. Tht options are Output, LabelInternal, KeptIndices, QCDOnly,
ZeroMom, Assumptions, UVonshell, ComplexMass, CTparameters and Exclude4ScalarsCT"
Output::usage="Option of WriteCT, string passed is use for naming the output file. By default, this options is set to automatic and therefore the FeynRules model name is used"
LabelInternal::usage="Option of WriteCT. If True, IPL[list of internal particles] multiply each contribution. Default value is True"
KeptIndices::usage="Option of WriteCT. Contain the list of indices that have to be kept in IPL. Default value is {}"
QCDOnly::usage="Option of WriteCT. Keep only the QCD corrections if set to True. Default value is False"
ZeroMom::usage="Option of WriteCT. Contains a list of pairs of a coupling and a vertex. The finite part of the coupling renormalization is fixed by requiring
that the associated vertex counterterm has no finite piece."
Assumptions::usage="Assumptions is an option for functions such as Simplify, Refine, Integrate and WriteCT which specifies default assumptions to be made about symbolic quantities."
MSbar::usage="Option of WriteCT. UV counterterm are calculated in the MSbar scheme if True and therefore no finite part of the amplitude is computed but the R2. 
Default is False"
ComplexMass::usage="Option of WriteCT. The complex mass scheme is used if True. Default is False."
CTparameters::usage="Option of WriteCT. Replace long expression as in UV vertices by parameter if True. Default is False."
Exclude4ScalarsCT::usage="Option of WriteCT. Do not compute the counterterms (R2 and UV) with 4 scalars if True. However, 4 scalars tree-level interactions are kept for
the computation of all the other counterterms. Default is False"


Options[WriteCT] = {LabelInternal -> True, KeptIndices->{},QCDOnly->False,ZeroMom->{},Assumptions->{},MSbar->False,ComplexMass->False,CTparameters->False, Exclude4ScalarsCT->False,Output->Automatic};


WriteCT[mod_,gen_String:"Lorentz", OptionsPattern[]]:=Block[{totlist,outfile,LabInt,KeptInd,qcd,r2list,uvlist,InvEps,ipl1m,wsp,info,zerom,assume,uvos,cms,CTparam,ctp,no4S,out},

  LabInt=OptionValue[LabelInternal];
  KeptInd=OptionValue[KeptIndices];
  qcd=OptionValue[QCDOnly];
  zerom=OptionValue[ZeroMom];
  assume=OptionValue[Assumptions];
  uvos=Not[OptionValue[MSbar]];
  cms=OptionValue[ComplexMass];
  ctp=OptionValue[CTparameters];
  no4S=OptionValue[Exclude4ScalarsCT];
  out=OptionValue[Output];

If[cms,NLO$CMS=True;,NLO$CMS=False;];

  UV$Wftlist = {};
  CTparam={};

  totlist=ModelR2[mod,gen,LabInt,KeptInd,qcd,zerom,assume,uvos,cms,ctp,no4S];

  r2list = Transpose[Delete[Transpose[totlist],{3}]];
  uvlist = Transpose[Delete[Transpose[totlist],{2}]]/.FR$IR->1;
  r2list=DeleteCases[r2list,{a_,0},1];
  uvlist=DeleteCases[uvlist,{a_,0},1]/.FR$Eps->-2FR$Eps;(*change from d=4+eps to d=4-2eps*)
  CTparam = CTparam/.FR$Eps->-2FR$Eps/.FR$IR->1/.{SUT->SUNT,SUF->SUNF,IndexDel->IndexDelta};

  uvlist=uvlist/.(Rule[-Append[#2,xx___],anti[#1]]&)@@@FR$ClassesTranslation/.(Rule[Append[#2,xx___],#1]&)@@@FR$ClassesTranslation;
  r2list=r2list/.(Rule[-Append[#2,xx___],anti[#1]]&)@@@FR$ClassesTranslation/.(Rule[Append[#2,xx___],#1]&)@@@FR$ClassesTranslation;
  CTparam = CTparam/.(Rule[-Append[#2,xx___],anti[#1]]&)@@@FR$ClassesTranslation/.(Rule[Append[#2,xx___],#1]&)@@@FR$ClassesTranslation;

  FR$InteractionOrderPerturbativeExpansion=If[qcd,FR$InteractionOrderPerturbativeExpansion/.{{QCD,0}->{QCD,1}},FR$InteractionOrderPerturbativeExpansion/.{0->1}];
  
  wsp="                                                                                                                             ";
  info = (ToString[#1]<>" : "<>If[Head[#2]===List,StringDrop[StringJoin[(#<>", "&)/@#2],-2],#2]&)@@@FR$ModelInformation;
  If[out===Automatic,out=ModelName/.FR$ModelInformation;];
  outfile=out<>".nlo";
  Print["Writing R2 and UV counterterms from "<>mod<>" and "<>gen<>" in "<>outfile];
  OpenWrite[outfile];
  WriteString[outfile,"(****************************************************************************************************************)\n"];
  WriteString[outfile,"(* Model automalically generated by NLOCT-"<>StringTake[NLO$Version<>wsp,70]<>"*)\n"];
  WriteString[outfile,"(* date and time of generation : "<>StringTake[DateString[]<>wsp,79]<>"*)\n"];
  WriteString[outfile,"(* FeynRules model information : "<>StringTake[wsp,79]<>"*)\n"];
  (WriteString[outfile,"(*   "<>StringTake[#<>wsp,106]<>" *)\n"]&)/@info;
  WriteString[outfile,"(****************************************************************************************************************)\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"(* FeynArts model files used to generated the output *)\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"CT$Model = \""<>mod<>"\";\n"];
  WriteString[outfile,"CT$GenericModel = \""<>gen<>"\";\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"(* Those assumptions where made : *)\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"NLOCT$assumptions = "];
  WriteString[outfile,ToString[InputForm[assume]]<>";\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"(* Perturbed orders *)\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"FR$InteractionOrderPerturbativeExpansion = "<>ToString[InputForm[FR$InteractionOrderPerturbativeExpansion]]<>";\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"(*R2 vertices*)\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"R2$vertlist = {\n"];
  (WriteString[outfile,ToString[InputForm[#]]<>",\n"]&)/@r2list[[1;;Length[r2list]-1]];
  WriteString[outfile,ToString[InputForm[Last[r2list]]]<>"};\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"(*UV vertices*)\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"UV$vertlist = {\n"];
  (WriteString[outfile,ToString[InputForm[#]]<>",\n"]&)/@uvlist[[1;;Length[uvlist]-1]];
  WriteString[outfile,ToString[InputForm[Last[uvlist]]]<>"};\n"];


  WriteString[outfile,"\n"];
  WriteString[outfile,"(*CT parameters*)\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"FR$CTparam = {\n"];
  If[Length[CTparam]>0,
    (WriteString[outfile,ToString[InputForm[#]]<>",\n"]&)/@CTparam[[1;;Length[CTparam]-1]];
    WriteString[outfile,ToString[InputForm[Last[CTparam]]]<>"};\n"];
    ,
    WriteString[outfile,"};\n"];
  ];
  Close[outfile];
  Print["done"];
]


(* ::Subtitle:: *)
(*SUN*)


(*special unitary group simplification rules*)


(*IndexLessQ*)
ILQ[Ext[a_Integer],b_Integer]:=True;
ILQ[b_Integer,Ext[a_Integer]]:=False;
ILQ[a_Integer,b_Integer]:=a<b;
ILQ[Ext[a_Integer],Ext[b_Integer]]:=a<b;


(* ::Subsubtitle:: *)
(*IndexDel *)


Attributes[IndexDel]={Orderless};

IndexDel/:IndexDel[a_,b_]IndexDel[b_,c_]SumOver[b_,n_Integer]:=IndexDel[a,c];
SumOver/:IndexDel[a_,b_]^2 SumOver[b_,n_Integer]:=IndexDel[a,a];
IndexDel/:IndexDel[b_,b_]SumOver[b_,n_Integer]:=n;

IndexDel/:IndexDel[a_,b_]F_[d___,b_,c___]SumOver[b_,n_Integer]:=F[d,a,c];


(* ::Subsubtitle::Closed:: *)
(*SUT Trace*)


SUT[Index[Gluon,b_],Index[Gluon,a_],Index[Gluon,c_]]:=SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,b]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[c,b];
SUT[Index[Gluon,c_],Index[Gluon,b_],Index[Gluon,a_]]:=SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,b]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[c,b];
SUT[Index[Gluon,b_],Index[Gluon,a_],Index[Gluon,c_]]:=SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]-
I/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[b,c];
SUT[Index[Gluon,c_],Index[Gluon,b_],Index[Gluon,a_]]:=SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]-
I/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[b,c];
SUT[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,b_]]:=SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]-
I/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[b,c];

SUT[Index[Gluon,b_],Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,d_]]:=SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[a,d];
SUT[Index[Gluon,d_],Index[Gluon,b_],Index[Gluon,a_],Index[Gluon,c_]]:=SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[a,d];
SUT[Index[Gluon,c_],Index[Gluon,d_],Index[Gluon,b_],Index[Gluon,a_]]:=SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[a,d];

SUT[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,c_],Index[Gluon,b_]]:=-1/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]-
SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]+
SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]/;ILQ[a,b]&&ILQ[b,c]&&ILQ[c,d];

SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,d_],Index[Gluon,c_]]:=1/2 SUF[Index[Gluon,a],Index[Gluon,c],Index[Gluon,b],Index[Gluon,d]]-
SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,b],Index[Gluon,d]]+
SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]]/;ILQ[a,b]&&ILQ[b,c]&&ILQ[c,d];

(*SUT[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,b_],Index[Gluon,d_]]:=-1/2 SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]]-
SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]+
SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]/;ILQ[a,b]&&ILQ[b,c]&&ILQ[c,d];*)


(* ::Subsubtitle:: *)
(*SUT*)


SUT/:SUT[a__,Index[Colour,i_],Index[Colour,j_]]*SUT[b__,Index[Colour,j_],Index[Colour,k_]]*SumOver[Index[Colour,j_],n_]:=SUT[a,b,Index[Colour,i],Index[Colour,k]];
SUT/:SUT[a__,Index[Colour,i_],Index[Colour,j_]]*SUT[b__,Index[Colour,j_],Index[Colour,i_]]*SumOver[Index[Colour,j_],n_]:=SUT[a,b]/SumOver[Index[Colour,i],n];

SUT/:SUT[Index[Gluon,a_],Index[Colour,i_],Index[Colour,j_]]*SUT[Index[Gluon,a_],Index[Colour,k_],Index[Colour,l_]]*SumOver[Index[Gluon,a_],n2m1_Integer]:=
IndexDel[Index[Colour,i],Index[Colour,l]]IndexDel[Index[Colour,k],Index[Colour,j]]/2-IndexDel[Index[Colour,i],Index[Colour,j]]IndexDel[Index[Colour,k],Index[Colour,l]]/2/Sqrt[n2m1+1];

SUT/:SUT[a__,Index[Colour,i_],Index[Colour,i_]]*SumOver[Index[Colour,i_],n_]:=SUT[a];

SUT/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUT[Index[Gluon,a_],Index[Gluon,a_],Index[Colour,i_],Index[Colour,l_]]:=(n2m1/2/Sqrt[n2m1+1])IndexDel[Index[Colour,i],Index[Colour,l]];

SUT/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,a_],Index[Colour,i_],Index[Colour,l_]]:= 
-1/2/(Sqrt[n2m1+1]) SUT[Index[Gluon,b],Index[Colour,i],Index[Colour,l]];

SUT/:SumOver[Index[Colour,j_],n_Integer]SUT[Index[Gluon,a_],Index[Gluon,a_],Index[Colour,i_],Index[Colour,l_]]:=
1/2(n-1/n) IndexDel[Index[Colour,i],Index[Colour,l]]/SumOver[Index[Gluon,a],n^2-1];

SUT[Index[Gluon,a_],Index[Colour,i_],Index[Colour,i_]]:=0;

SUT[Index[Colour,i_],Index[Colour,j_]]:=IndexDel[Index[Colour,i],Index[Colour,j]];

SUT[Index[Gluon,a_]]:=0;

SUT/:SUT[x___,Index[Gluon,a_],Index[Gluon,a_],y___]SumOver[Index[Gluon,a_],n2m1_Integer]:=n2m1/(2Sqrt[n2m1+1])SUT[x,y];
SUT/:SUT[x___,Index[Gluon,a_],w__,Index[Gluon,a_],y___]SumOver[Index[Gluon,a_],n2m1_Integer]:=1/2SUT[w]SUT[x,y]-1/(2Sqrt[n2m1+1])SUT[x,w,y];

SUT/:SumOver[Index[Gluon,a_],n2m1_]SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Colour,i_],Index[Colour,j_]]SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Colour,k_],
Index[Colour,l_]]:= (5/18 IndexDel[Index[Colour,i],Index[Colour,j]]IndexDel[Index[Colour,k],Index[Colour,l]]-1/6IndexDel[Index[Colour,i],Index[Colour,l]]*
IndexDel[Index[Colour,k],Index[Colour,j]])/SumOver[Index[Gluon,b],n2m1];

SUT/:SumOver[Index[Gluon,a_],n2m1_]SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Colour,i_],Index[Colour,j_]]SUT[Index[Gluon,b_],Index[Gluon,a_],Index[Colour,k_],
Index[Colour,l_]]:=(1/36IndexDel[Index[Colour,i],Index[Colour,j]]IndexDel[Index[Colour,k],Index[Colour,l]]+7/12IndexDel[Index[Colour,i],Index[Colour,l]]*
IndexDel[Index[Colour,k],Index[Colour,j]])/SumOver[Index[Gluon,b],n2m1];

SUT/:SUT[Index[Gluon,c_],Index[Gluon,d_]]:= IndexDel[Index[Gluon,c],Index[Gluon,d]]/2;

SUT/:IndexDel[Index[Colour, a_], Index[Colour, b_]]SumOver[Index[Colour, b_], n_]*SUT[h__,Index[Colour, b_],k___]:=SUT[h,Index[Colour, a],k];

SUT/:SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]*SUT[Index[Gluon,b_],Index[Gluon,c_],Index[Colour,j_],Index[Colour,i_]]*SumOver[Index[Gluon,c_],n2m1_]:=
I Sqrt[n2m1+1]*SUT[Index[Gluon,a],Index[Colour,j],Index[Colour,i]]/2/(SumOver[Index[Gluon,b],n2m1]);

SUT/:SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]*SUT[Index[Gluon,c_],Index[Gluon,b_],Index[Colour,i_],Index[Colour,j_]]*SumOver[Index[Gluon,c_],n2m1_]:=
-I Sqrt[n2m1+1]*SUT[Index[Gluon,a],Index[Colour,i],Index[Colour,j]]/2/(SumOver[Index[Gluon,b],n2m1]);

SUT/: SUT[Index[Gluon, m_],Index[Gluon,l_],Index[Gluon,k_],Index[Gluon,m_], Index[Colour, e_], Index[Colour, d_]]*SumOver[Index[Gluon, m_],n2m1_]:= 
  1/2(IndexDel[Index[Colour, e],Index[Colour, d]]IndexDel[Index[Gluon, l],Index[Gluon, k]]/2-
          1/Sqrt[n2m1+1]  SUT[Index[Gluon,l],Index[Gluon,k], Index[Colour, e], Index[Colour, d]]);


(* ::Subsubtitle::Closed:: *)
(*SUF*)


SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_]]:=Signature[{e,c,a}]*Signature[Sort[{e,c,a},ILQ]]*
SUF[Sequence@@Sort[{Index[Gluon,e],Index[Gluon,c],Index[Gluon,a]},(ILQ[#1[[2]],#2[[2]]]&)]]/;Not[OrderedQ[{e,c,a},ILQ]];


SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:=- SUF[Index[Gluon,e],Index[Gluon,c],Index[Gluon,b],Index[Gluon,a]]/;ILQ[b,a]&&ILQ[e,c]&&ILQ[e,b];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:=- SUF[Index[Gluon,c],Index[Gluon,e],Index[Gluon,a],Index[Gluon,b]]/;ILQ[c,e]&&ILQ[a,b]&&ILQ[c,a];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:= SUF[Index[Gluon,c],Index[Gluon,e],Index[Gluon,b],Index[Gluon,a]]/;ILQ[c,e]&&ILQ[b,a]&&ILQ[c,b];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:= SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,e],Index[Gluon,c]]/;ILQ[a,e]&&ILQ[e,c]&&ILQ[a,b];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:=- SUF[Index[Gluon,b],Index[Gluon,a],Index[Gluon,e],Index[Gluon,c]]/;ILQ[b,e]&&ILQ[e,c]&&ILQ[b,a];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:= SUF[Index[Gluon,b],Index[Gluon,a],Index[Gluon,c],Index[Gluon,e]]/;ILQ[b,c]&&ILQ[c,e]&&ILQ[b,a];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:=- SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,e]]/;ILQ[a,c]&&ILQ[c,e]&&ILQ[a,b];


SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]*
SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,b_]]:=(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,a_]]:=
(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUF[Index[Gluon,d_],Index[Gluon,a_],Index[Gluon,b_]]*SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,c_]]*
SUF[Index[Gluon,f_],Index[Gluon,b_],Index[Gluon,c_]]:=-3/2 SUF[Index[Gluon,d],Index[Gluon,f],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1]/SumOver[Index[Gluon,c],n2m1];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, d_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:= Sqrt[n2m1+1]SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,b_],Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_],n2m1_Integer]:= 3/2/SumOver[Index[Gluon,f],n2m1]SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,f_],Index[Gluon,e_]]*SUF[Index[Gluon,b_],Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_],n2m1_Integer]:=-3/2/SumOver[Index[Gluon,f],n2m1]SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,b_],Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_],n2m1_Integer]:= Sqrt[n2m1+1]/SumOver[Index[Gluon,f],n2m1]SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]];

SUF/:SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,h_]]*
SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,f_],Index[Gluon,h_]]*SumOver[Index[Gluon,e_],n2m1_Integer]:=-3/2*
SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]/SumOver[Index[Gluon,f],n2m1]/SumOver[Index[Gluon,h],n2m1];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,h_]]*
SUF[Index[Gluon,b_],Index[Gluon,f_],Index[Gluon,c_],Index[Gluon,h_]]*SumOver[Index[Gluon,e_],n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]*
IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+
IndexDel[Index[Gluon,a],Index[Gluon,b]]*IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*(SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]+
SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]))/(SumOver[Index[Gluon,f],n2m1]*SumOver[Index[Gluon,h],n2m1]);

SUF/:SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_], Index[Gluon, f_]]*
SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, f_]]*SumOver[Index[Gluon, e_], n2m1_Integer]:=
(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+
IndexDel[Index[Gluon,a],Index[Gluon,d]]IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+
(Sqrt[n2m1+1])*(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/
SumOver[Index[Gluon, f], n2m1];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,f_],Index[Gluon,b_],Index[Gluon,e_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,d_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,b_],Index[Gluon,g_],Index[Gluon,h_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,g_]]*
SUF[Index[Gluon,d_],Index[Gluon,f_],Index[Gluon,h_]]*SumOver[Index[Gluon,e_],n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]*
IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+
IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*(SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]]+
SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,b],Index[Gluon,d]]))/(SumOver[Index[Gluon,f],n2m1]*SumOver[Index[Gluon,g],n2m1]*SumOver[Index[Gluon,h],n2m1]);

SUF/:SUF[Index[Gluon, a_], Index[Gluon, c_], Index[Gluon,b_], Index[Gluon, d_]]*SUT[Index[Gluon, c_],Index[Gluon, d_], Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=(Sqrt[n2m1+1]/2 SUT[Index[Gluon, a],Index[Gluon, b], Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, a],Index[Gluon, b]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, c], n2m1];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, c_], Index[Gluon, d_]]*SUT[Index[Gluon, d_], Index[Gluon, b_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=(Sqrt[n2m1+1]/2 SUT[Index[Gluon, c], Index[Gluon, a],Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, c], Index[Gluon, a]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, c_], Index[Gluon, d_]]*SUT[Index[Gluon, c_], Index[Gluon, d_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=I Sqrt[n2m1+1]/2 SUT[Index[Gluon, c],Index[Colour, i], Index[Colour, j]]SUF[Index[Gluon, a], Index[Gluon, b], Index[Gluon, c]];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_], Index[Gluon, c_]]*SUT[Index[Gluon, c_], Index[Gluon, d_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=-I Sqrt[n2m1+1]/2 SUT[Index[Gluon, c],Index[Colour, i], Index[Colour, j]]SUF[Index[Gluon, a], Index[Gluon, b], Index[Gluon, c]];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,b_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,d_]]*SumOver[Index[Gluon,e_],n2m1_]:=
 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,e_]]*SUF[Index[Gluon,c_],Index[Gluon,d_],Index[Gluon,e_]]*SumOver[Index[Gluon,e_],n2m1_]:=
 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]];

SUF/:SumOver[Index[Gluon, a_], n2m1_Integer]*SUF[Index[Gluon, c_], Index[Gluon, a_], Index[Gluon, b_]]* 
SUT[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_], Index[Colour,i_],Index[Colour,j_]]:= 
 Sqrt[n2m1+1] I SUT[Index[Gluon, c],Index[Gluon, d],Index[Colour,i],Index[Colour,j]]/2/SumOver[Index[Gluon, b], n2m1];

SUF/:SumOver[Index[Gluon, a_], n2m1_Integer]*SUF[Index[Gluon, c_], Index[Gluon, a_], Index[Gluon, b_]]* 
SUT[Index[Gluon, d_], Index[Gluon, b_],Index[Gluon, a_], Index[Colour,i_], Index[Colour,j_]]:= 
- Sqrt[n2m1+1] I SUT[Index[Gluon, c],Index[Gluon, d],Index[Colour,i],Index[Colour,j]]/2/SumOver[Index[Gluon, b], n2m1];

SUF/:SumOver[Index[Gluon, b_],n2m1_Integer]SUF[Index[Gluon, a_],Index[Gluon, b_],Index[Gluon, c_]]*
SUT[Index[Gluon, b_],Index[Gluon, d_],Index[Gluon, c_],Index[Colour,i_],Index[Colour,j_]]:= -I  IndexDel[Index[Gluon, a],Index[Gluon, d]]*
IndexDel[Index[Colour,i],Index[Colour,j]]/4/SumOver[Index[Gluon, c],n2m1];

SUF/:SumOver[Index[Gluon, b_],n2m1_Integer]SUF[Index[Gluon, a_],Index[Gluon, b_],Index[Gluon, c_]]*
SUT[Index[Gluon, c_],Index[Gluon, d_],Index[Gluon, b_],Index[Colour,i_],Index[Colour,j_]]:= I IndexDel[Index[Gluon, a],Index[Gluon, d]]*
IndexDel[Index[Colour,i],Index[Colour,j]]/4/SumOver[Index[Gluon, c],n2m1];

SUF/:SumOver[Index[Gluon, g_], n2m1_Integer]*SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, g_]]*SUT[Index[Gluon, g_], Index[Colour, i_],Index[Colour, j_]]:=
-I (SUT[Index[Gluon, a],Index[Gluon, b], Index[Colour, i],Index[Colour, j]]-SUT[Index[Gluon, b],Index[Gluon, a], Index[Colour, i],Index[Colour, j]]);


SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]*SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,b_]]:=
(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];


(* ::Subsubtitle:: *)
(*dSUN*)


SetAttributes[dSUN,Orderless];

dSUN/:dSUN[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,b_]]:=3/2*
dSUN[Index[Gluon,c],Index[Gluon,e],Index[Gluon,d]]/SumOver[Index[Gluon,a],8]/SumOver[Index[Gluon,b],8];

dSUN/:dSUN[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]SUF[Index[Gluon,e_],Index[Gluon,d_],Index[Gluon,a_],Index[Gluon,b_]]:=0;
dSUN/:dSUN[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,b_]]:=0;

dSUN/:dSUN[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]SUF[Index[Gluon,e_],Index[Gluon,b_],Index[Gluon,f_]]*
SUF[Index[Gluon,d_],Index[Gluon,f_],Index[Gluon,a_]]:=-3/2*dSUN[Index[Gluon,c],Index[Gluon,e],Index[Gluon,d]]/SumOver[Index[Gluon,a],8]/SumOver[Index[Gluon,b],8]/
SumOver[Index[Gluon,f],8];
dSUN/:dSUN[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]SUF[Index[Gluon,e_],Index[Gluon,b_],Index[Gluon,f_]]SUF[Index[Gluon,d_],Index[Gluon,a_],Index[Gluon,f_]]:=3/2*
dSUN[Index[Gluon,c],Index[Gluon,e],Index[Gluon,d]]/SumOver[Index[Gluon,a],8]/SumOver[Index[Gluon,b],8]/SumOver[Index[Gluon,f],8];

dSUN/:dSUN[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Colour,i_],Index[Colour,j_]]:=5/6*
SUT[Index[Gluon,c],Index[Colour,i],Index[Colour,j]]/SumOver[Index[Gluon,a],8]/SumOver[Index[Gluon,b],8];


(* ::Subtitle:: *)
(*Protection*)


Protect[ME,SUF,SUT,SUNTr,GetR2,R2Tadpoles,R2BubblesF,R2BubblesQ2,R2FTriangles,R2BTriangles,R2FBTriangles,R2BBoxes,R2FBoxes,ILQ,WriteCT,ModelR2,PartOrder,R2atClass,
SP,FCh,DTr,FV,LCivita,M$dim,red2v,red4v,FR$UV,FR$R2,FR$MU,FR$Eps,MergeVertList,UVwfatClass,AddWf,FR$UV,FR$R2,ReInt,IntxnLog,IntxnLog2,Dp2IntxnLog,Dp2IntxnLog2,
IndexDel];
