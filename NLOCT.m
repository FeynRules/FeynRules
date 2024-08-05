(* ::Package:: *)

(* ::Section:: *)
(*Some printout*)


NLO$Version="1.0.1";

Print[" - NLOCT - "];
Print["Version: "<>NLO$Version];
Print["Authors: C. Degrande"];
Print["Please cite C. Degrande, Comput.Phys.Commun. 197 (2015) 239-262"];

Off[Simplify::time];(*avoid the warning from aborded long simplification*)

NLOCT$notParal=False;

NLO$mix={};
FR$photonSE=0;

NLO$DebugCount=0;

NLO$Print=False;
DPrint[x_]:=If[NLO$Print,Print[x]];

NLO$AlreadyLaunch=False;


(* ::Subtitle:: *)
(*Simplification rules*)


red4v::usage="replacement rules for four 4-vector with loop momentum with lorentz indices not contracted together to the squared of the scalar product 
of the loop momentum times the appropriate lorentz structure and coefficient in M$dim dimensions"
red2v::usage="replacement rules for four 2-vector with loop momentum with lorentz indices not contracted together to the scalar product 
of the loop momentum times the appropriate lorentz structure and coefficient in M$dim dimensions"


red4v[lm_]:={FV[lm,a_]FV[lm,b_]FV[lm,c_]FV[lm,d_]->1/(M$dim+2)/M$dim SP[lm,lm]^2(ME[a,b]ME[c,d]+ME[a,c]ME[b,d]+ME[a,d]ME[b,c]),
             DTr[a___,NonCommutative[DiracSlash[lm]],b___]FV[lm,c_]FV[lm,d_]FV[lm,e_]:>1/(M$dim+2)/M$dim*(DTr[a,NonCommutative[DiracMatrix[c]],b]ME[d,e]+
             DTr[a,NonCommutative[DiracMatrix[d]],b]ME[c,e]+DTr[a,NonCommutative[DiracMatrix[e]],b]ME[d,c])SP[lm,lm]^2,
             LCivita[FV[lm],a_,c_,d_]FV[lm,b_]FV[lm,e_]FV[lm,f_]->1/M$dim/(M$dim+2)* SP[lm,lm]^2(LCivita[b,a,c,d]ME[e,f]+
             LCivita[e,a,c,d]ME[b,f]+LCivita[f,a,c,d]ME[e,b])};
red2v[lm_]:={FV[lm,a_]FV[lm,b_]->1/M$dim* SP[lm,lm]ME[a,b],SP[lm,a_]FV[lm,b_]:>1/M$dim* FV[a,b]SP[lm,lm]/;Not[a===lm],
             FCh[a__,NonCommutative[DiracSlash[lm]],b__]FV[lm,c_]:>1/M$dim* FCh[a,NonCommutative[DiracMatrix[c]],b]SP[lm,lm],
             FCh[a__,NonCommutative[DiracSlash[lm]],b__]SP[lm,c_]:>1/M$dim* FCh[a,NonCommutative[DiracSlash[c]],b]SP[lm,lm],
             DTr[a___,NonCommutative[DiracSlash[lm]],b___]FV[lm,c_]:>1/M$dim* DTr[a,NonCommutative[DiracMatrix[c]],b]SP[lm,lm],
             DTr[a___,NonCommutative[DiracSlash[lm]],b___]SP[lm,c_]:>1/M$dim* DTr[a,NonCommutative[DiracSlash[c]],b]SP[lm,lm],
             LCivita[FV[lm],a_,c_,d_]FV[lm,b_]->1/M$dim* SP[lm,lm]LCivita[b,a,c,d],
             LCivita[FV[lm],a_,c_,d_]SP[lm,b_]->1/M$dim* SP[lm,lm]LCivita[FV[b],a,c,d]};


CMSConj[-x_]:=-CMSConj[x];
CMSConj[CMSConj[x_]]:=x;
CMSConj[a_+b_]:=CMSConj[a]+CMSConj[b];
CMSConj[a_*b_]:=CMSConj[a]*CMSConj[b];
CMSConj[a_^b_]:=CMSConj[a]^CMSConj[b]/;N[b]\[Element]Reals;
CMSConj[a_/b_]:=CMSConj[a]/CMSConj[b];
CMSConj[FR$IR]:=FR$IR;
CMSConj[FR$IRLog]:=FR$IRLog;
CMSConj[FR$Eps]:=FR$Eps;
CMSConj[IPL[x_]]:=IPL[x];
CMSConj[Log[x_]]:=Log[x];
CMSConj[a_]:=Conjugate[a]/;NumericQ[a];
CMSConj[x_If]:=x;


(* ::Subsubtitle:: *)
(*LCivita*)


LCivita[d___,FV[FourMomentum[Incoming,a_]],FV[FourMomentum[Internal,b_]],c___]:=-LCivita[d,FV[FourMomentum[Internal,b]],FV[FourMomentum[Incoming,a]],c];

LCivita[d___,FV[FourMomentum[Internal,a_]],FV[FourMomentum[Internal,b_]],c___]:=-LCivita[d,FV[FourMomentum[Internal,b]],FV[FourMomentum[Incoming,a]],c]/;a>b;
LCivita[d___,FV[FourMomentum[Incoming,a_]],FV[FourMomentum[Incoming,b_]],c___]:=-LCivita[d,FV[FourMomentum[Incoming,b]],FV[FourMomentum[Incoming,a]],c]/;a>b;

LCivita[c___,Index[Lorentz,d_],a_FV,b___]:=-LCivita[c,a,Index[Lorentz,d],b];

LCivita[c___,Index[Lorentz,d_],Index[Lorentz,a_],b___]:=-LCivita[c,Index[Lorentz,a],Index[Lorentz,d],b]/;d>a;

LCivita/:LCivita[Index[Lorentz,a_],Index[Lorentz,b_],Index[Lorentz,c_],Index[Lorentz,d_]]*
  LCivita[Index[Lorentz,a_],Index[Lorentz,b_],Index[Lorentz,c_],Index[Lorentz,e_]]:=-6(ME[Index[Lorentz,d],Index[Lorentz,e]])/;FreeQ[{a,b,c,d,e},Int];

LCivita/:LCivita[a___,b_,c___]FV[FourMomentum[Incoming,d_],b_]:=LCivita[a,FV[FourMomentum[Incoming,d]],c];

LCivita[a___,b_+d_,c___]:=LCivita[a,b,c]+LCivita[a,d,c];
LCivita[a___,-b_,c___]:=-LCivita[a,b,c];
LCivita[a___,d_?(FreeQ[#,FourMomentum]&)*b_,c___]:=d*LCivita[a,b,c];

LCivita[a___,b_,c___,b_,d___]:=0;


(* ::Subsubtitle::Closed:: *)
(*Metric*)


ME::usage="Metric tensor in M$dim"


(*Attributes[ME]=Orderless;*)
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*ME[Index[Lorentz,a_],Index[Lorentz,c_]]:=ME[Index[Lorentz,c],Index[Lorentz,b]];
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]^2:=M$dim;
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*f_[c___,Index[Lorentz,a_],d___]:=f[c,Index[Lorentz,b],d]/;Not[f===PolarizationVector]&&Not[f===PolarizationTensor];
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*FCh[c___,NonCommutative[DiracMatrix[Index[Lorentz,a_]]],d___]:=FCh[c,NonCommutative[DiracMatrix[Index[Lorentz,b]]],d];
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*DTr[c___,NonCommutative[DiracMatrix[Index[Lorentz,a_]]],d___]:=DTr[c,NonCommutative[DiracMatrix[Index[Lorentz,b]]],d];
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*ME[Index[Lorentz,a_],Index[Lorentz,b_]]:=M$dim;

ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*ME[Index[Lorentz,c_],Index[Lorentz,a_]]:=ME[Index[Lorentz,c],Index[Lorentz,b]];
ME/:ME[Index[Lorentz,b_],Index[Lorentz,a_]]*ME[Index[Lorentz,c_],Index[Lorentz,a_]]:=ME[Index[Lorentz,c],Index[Lorentz,b]];
ME/:ME[Index[Lorentz,a_],Index[Lorentz,b_]]*ME[Index[Lorentz,b_],Index[Lorentz,a_]]:=M$dim;
ME/:ME[Index[Lorentz,b_],Index[Lorentz,a_]]*f_[c___,Index[Lorentz,a_],d___]:=f[c,Index[Lorentz,b],d]/;Not[f===PolarizationVector]&&Not[f===PolarizationTensor];
ME/:ME[Index[Lorentz,b_],Index[Lorentz,a_]]*FCh[c___,NonCommutative[DiracMatrix[Index[Lorentz,a_]]],d___]:=FCh[c,NonCommutative[DiracMatrix[Index[Lorentz,b]]],d];
ME/:ME[Index[Lorentz,b_],Index[Lorentz,a_]]*DTr[c___,NonCommutative[DiracMatrix[Index[Lorentz,a_]]],d___]:=DTr[c,NonCommutative[DiracMatrix[Index[Lorentz,b]]],d];
ME/:ME[Index[Lorentz,b_],Index[Lorentz,a_]]*ME[Index[Lorentz,a_],Index[Lorentz,b_]]:=M$dim;

ME[Index[Lorentz,a_],Index[Lorentz,a_]]:=M$dim;

ME[Index[Lorentz,Ext[b_]],Index[Lorentz,Ext[a_]]]:=ME[Index[Lorentz,Ext[a]],Index[Lorentz,Ext[b]]]/;a<b;


(* ::Subsubtitle::Closed:: *)
(*Four vector*)


FV::usage="four vector"


FV[-a_,b_]:=-FV[a,b];
FV[a_+b_,c_]:=FV[a,c]+FV[b,c];
FV[a_?(FreeQ[#,FourMomentum]&)*b_,c_]:=a*FV[b,c];
(*in Eps*)
FV[-a_]:=-FV[a];
FV[a_+b_]:=FV[a]+FV[b];
FV[a_?(FreeQ[#,FourMomentum]&)*b_]:=a*FV[b];

FV/:FV[a_,b_]*FV[c_,b_]:=SP[a,c];
FV/:FV[a_,b_]^2:=SP[a,a];


(* ::Subsubtitle::Closed:: *)
(*Dirac Trace*)


DTr::usage="Trace over gamma matices"


DTr[a___,NonCommutative[DiracSlash[b__]+Mass[yy__]],c___]:=DTr[a,NonCommutative[DiracSlash[b]/.{Internal->PropInternal,Incoming->PropIncoming}],c]+Mass[yy]DTr[a,c];
DTr[a___,NonCommutative[DiracSlash[b__]]+Mass[yy__],c___]:=DTr[a,NonCommutative[DiracSlash[b]/.{Internal->PropInternal,Incoming->PropIncoming}],c]+Mass[yy]DTr[a,c];

DTr[a___,NonCommutative[DiracSlash[d_?(FreeQ[#,FourMomentum]&)*b__]],c___]:=d*DTr[a,NonCommutative[DiracSlash[b]],c];
DTr[a___,NonCommutative[DiracSlash[d_+b_]],c___]:=DTr[a,NonCommutative[DiracSlash[b]],c]+DTr[a,NonCommutative[DiracSlash[d]],c];

DTr[a___,Mass[yy__],c___]:=Mass[yy]DTr[a,c];

DTr[a___,NonCommutative[DiracSlash[b_]],NonCommutative[DiracSlash[b_]],c___]:=DTr[a,c]SP[b,b];
(*not allowed for anti-commuting gamma_5*)
(*DTr[NonCommutative[DiracSlash[b_]],a___,NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=DTr[a,NonCommutative[ChiralityProjector[-pm]]]SP[b,b];*)
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

DTr[a___,NonCommutative[DiracMatrix[lm_]],NonCommutative[DiracMatrix[lm_]],c___]:=M$dim*DTr[a,c];

DTr[a___,NonCommutative[DiracSlash[lm_]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracSlash[lm_]],c___]:=(2FV[lm,b]*DTr[a,NonCommutative[DiracSlash[lm]],c]-
SP[lm,lm]*DTr[a,NonCommutative[DiracMatrix[b]],c]);


(*not allowed for anti-commuting gamma_5*)
(*DTr[NonCommutative[DiracSlash[lm_]],a___,NonCommutative[DiracSlash[lm_]],NonCommutative[DiracMatrix[b_]],NonCommutative[ChiralityProjector[pm_]]]:=
2FV[lm,b]*DTr[a,NonCommutative[DiracSlash[lm]],NonCommutative[ChiralityProjector[-pm]]]-SP[lm,lm]*DTr[a,NonCommutative[DiracMatrix[b]],NonCommutative[ChiralityProjector[-pm]]];*)
DTr[a___,NonCommutative[DiracMatrix[lm_]],d___,NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[lm_]],c___]:=
2ME[lm,b]*DTr[a,NonCommutative[DiracMatrix[lm]],d,c]-DTr[a,NonCommutative[DiracMatrix[lm]],d,NonCommutative[DiracMatrix[lm]],NonCommutative[DiracMatrix[b]],c]/;FreeQ[{d},b];
DTr[a___,NonCommutative[DiracSlash[lm_]],d___,NonCommutative[DiracMatrix[b_]],NonCommutative[DiracSlash[lm_]],c___]:=
(2FV[lm,b]*DTr[a,NonCommutative[DiracSlash[lm]],d,c]-DTr[a,NonCommutative[DiracSlash[lm]],d,NonCommutative[DiracSlash[lm]],NonCommutative[DiracMatrix[b]],c])/;FreeQ[{d},b];
DTr[a___,NonCommutative[DiracMatrix[lm_]],d___,NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[lm_]],c___]:=
2FV[b,lm]*DTr[a,NonCommutative[DiracMatrix[lm]],d,c]-DTr[a,NonCommutative[DiracMatrix[lm]],d,NonCommutative[DiracMatrix[lm]],NonCommutative[DiracSlash[b]],c]/;FreeQ[{d},b];
DTr[a___,NonCommutative[DiracSlash[lm_]],d___,NonCommutative[DiracSlash[b_]],NonCommutative[DiracSlash[lm_]],c___]:=
(2SP[lm,b]*DTr[a,NonCommutative[DiracSlash[lm]],d,c]-DTr[a,NonCommutative[DiracSlash[lm]],d,NonCommutative[DiracSlash[lm]],NonCommutative[DiracSlash[b]],c])/;FreeQ[{d},b];

DTr[NonCommutative[a:_DiracMatrix|_DiracSlash],NonCommutative[b:_DiracMatrix|_DiracSlash],NonCommutative[c:_DiracMatrix|_DiracSlash],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b,c},Internal]&&FreeQ[{a,b,c},Int];

DTr[NonCommutative[a:_DiracMatrix|_DiracSlash],NonCommutative[ChiralityProjector[pm_]]]:=0;

DTr[NonCommutative[ChiralityProjector[pm_]]]:=2;

(*Only valid in 4 dimension, so condition if loop momentum or summed index present*)

DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
 2*(ME[a,b]ME[c,d]-ME[a,c]ME[b,d]+ME[a,d]ME[b,c]- I pm*LCivita[a,b,c,d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];
DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
 2*(FV[a,b]ME[c,d]-FV[a,c]ME[b,d]+FV[a,d]ME[b,c]- I pm*LCivita[FV[a],b,c,d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];
DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
 2*(FV[b,a]ME[c,d]-ME[a,c]FV[b,d]+ME[a,d]FV[b,c]- I pm*LCivita[a,FV[b],c,d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[b_]],NonCommutative[ChiralityProjector[pm_]]]:=
 2*(FV[a,b]ME[c,d]-FV[a,c]ME[b,d]+FV[a,d]ME[b,c]- I pm*LCivita[FV[a],b,c,d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b__]],NonCommutative[ChiralityProjector[pm_]]]:=
 2*(FV[b,a]ME[c,d]-ME[a,c]FV[b,d]+ME[a,d]FV[b,c]- I pm*LCivita[a,FV[b],c,d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];

DTr[NonCommutative[DiracSlash[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(SP[a,b]ME[c,d]-FV[a,c]FV[b,d]+FV[a,d]FV[b,c]- pm*I LCivita[FV[a],FV[b],c,d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];
DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[DiracSlash[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[b,a]FV[c,d]-FV[c,a]FV[b,d]+ME[a,d]SP[b,c]- pm*I LCivita[a,FV[b],FV[c],d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(SP[a,b]ME[c,d]-FV[a,c]FV[b,d]+FV[a,d]FV[b,c]- pm*I LCivita[FV[a],FV[b],c,d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];
DTr[NonCommutative[DiracSlash[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[b,a]FV[c,d]-FV[c,a]FV[b,d]+ME[a,d]SP[b,c]- pm*I LCivita[a,FV[b],FV[c],d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];

DTr[NonCommutative[DiracSlash[a_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[d_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(-SP[a,b]ME[c,d]+FV[a,c]FV[b,d]+FV[a,d]FV[b,c]+ pm*I LCivita[FV[a],FV[b],c,d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];
DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[c_]],NonCommutative[ChiralityProjector[pm_]]]:=
2*(FV[b,a]FV[c,d]+FV[c,a]FV[b,d]-ME[a,d]SP[b,c]+ pm*I LCivita[a,FV[b],FV[c],d])/;FreeQ[{a,b,c,d},Internal]&&FreeQ[{a,b,c,d},Int];

DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracMatrix[b_]],NonCommutative[ChiralityProjector[pm_]]]:=2*(ME[b,a])/;FreeQ[{a,b},Internal]&&FreeQ[{a,b},Int];
DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=2*(FV[b,a])/;FreeQ[{a,b},Internal]&&FreeQ[{a,b},Int];
DTr[NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[a_]],NonCommutative[ChiralityProjector[pm_]]]:=2*(FV[b,a])/;FreeQ[{a,b},Internal]&&FreeQ[{a,b},Int];
DTr[NonCommutative[DiracSlash[a_]],NonCommutative[DiracSlash[b_]],NonCommutative[ChiralityProjector[pm_]]]:=2*SP[a,b]/;FreeQ[{a,b},Internal]&&FreeQ[{a,b},Int];

(*SigmaMatrix definition in DTr*)
DTr[a___,NonCommutative[SigmaMatrix[ind1_,ind2_]],b___]:=I/2(DTr[a,NonCommutative[DiracMatrix[ind1]],NonCommutative[DiracMatrix[ind2]],b]-DTr[a,NonCommutative[DiracMatrix[ind2]],NonCommutative[DiracMatrix[ind1]],b]);

(*DTr[NonCommutative[DiracMatrix[a_]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;Not[FreeQ[a,Ext]]&&Not[FreeQ[b,Ext]]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];

DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[a,Internal]&&Not[FreeQ[b,Ext]]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracMatrix[b_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[a,Internal]&&Not[FreeQ[b,Ext]]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[a,Internal]&&Not[FreeQ[b,Ext]]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[a,Internal]&&Not[FreeQ[b,Ext]]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[DiracSlash[a__]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[a,Internal]&&Not[FreeQ[b,Ext]]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];

DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[DiracSlash[b__]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];

DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[DiracSlash[b__]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];

DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracSlash[b__]],NonCommutative[DiracMatrix[e_]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracMatrix[e_]],NonCommutative[DiracSlash[b__]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];
DTr[NonCommutative[DiracMatrix[c_]],NonCommutative[DiracMatrix[d_]],NonCommutative[DiracMatrix[e_]],NonCommutative[DiracSlash[a__]],NonCommutative[DiracSlash[b__]],NonCommutative[ChiralityProjector[pm_]]]:=0/;FreeQ[{a,b},Internal]&&Not[FreeQ[c,Ext]]&&Not[FreeQ[d,Ext]]&&Not[FreeQ[e,Ext]];

DTr2[NonCommutative[DiracSlash[a_]], NonCommutative[DiracMatrix[b_]], NonCommutative[DiracSlash[c_]], NonCommutative[DiracMatrix[d_]], NonCommutative[DiracMatrix[e_]], 
NonCommutative[DiracMatrix[f_]], NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracSlash[a]], NonCommutative[DiracMatrix[b]], NonCommutative[DiracSlash[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracSlash[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
SP[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];

DTr2[ NonCommutative[DiracSlash[c_]], NonCommutative[DiracMatrix[d_]], NonCommutative[DiracMatrix[e_]], NonCommutative[DiracMatrix[f_]],NonCommutative[DiracSlash[a_]], 
NonCommutative[DiracMatrix[b_]], NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]],
NonCommutative[DiracMatrix[f]], NonCommutative[DiracSlash[a]], NonCommutative[DiracMatrix[b]], NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracSlash[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
SP[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];

DTr2[NonCommutative[DiracMatrix[e_]], NonCommutative[DiracMatrix[f_]], NonCommutative[DiracSlash[a_]], NonCommutative[DiracMatrix[b_]], NonCommutative[DiracSlash[c_]], 
NonCommutative[DiracMatrix[d_]], NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[DiracSlash[a]], 
NonCommutative[DiracMatrix[b]], NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[d]], NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracSlash[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
SP[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracSlash[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];

DTr2[NonCommutative[DiracSlash[a_]], NonCommutative[DiracMatrix[b_]], NonCommutative[DiracMatrix[c_]], NonCommutative[DiracMatrix[d_]], NonCommutative[DiracMatrix[e_]], 
NonCommutative[DiracMatrix[f_]], NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracSlash[a]], NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracMatrix[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
FV[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];

DTr2[NonCommutative[DiracMatrix[f_]],NonCommutative[DiracSlash[a_]], NonCommutative[DiracMatrix[b_]], NonCommutative[DiracMatrix[c_]], NonCommutative[DiracMatrix[d_]], 
NonCommutative[DiracMatrix[e_]],  NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracMatrix[f]], NonCommutative[DiracSlash[a]], NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]],  NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracMatrix[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
FV[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];

DTr2[NonCommutative[DiracMatrix[e_]], NonCommutative[DiracMatrix[f_]], NonCommutative[DiracSlash[a_]], NonCommutative[DiracMatrix[b_]], NonCommutative[DiracMatrix[c_]], 
NonCommutative[DiracMatrix[d_]], NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[DiracSlash[a]], 
NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracMatrix[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
FV[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];

DTr2[NonCommutative[DiracMatrix[d_]], NonCommutative[DiracMatrix[e_]], NonCommutative[DiracMatrix[f_]], NonCommutative[DiracSlash[a_]], NonCommutative[DiracMatrix[b_]], 
NonCommutative[DiracMatrix[c_]], NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[DiracSlash[a]], NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracMatrix[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
FV[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];

DTr2[NonCommutative[DiracMatrix[c_]], NonCommutative[DiracMatrix[d_]], NonCommutative[DiracMatrix[e_]], NonCommutative[DiracMatrix[f_]], NonCommutative[DiracSlash[a_]], 
NonCommutative[DiracMatrix[b_]], NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], 
NonCommutative[DiracMatrix[f]], NonCommutative[DiracSlash[a]], NonCommutative[DiracMatrix[b]], NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracMatrix[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
FV[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];

DTr2[NonCommutative[DiracMatrix[b_]], NonCommutative[DiracMatrix[c_]], NonCommutative[DiracMatrix[d_]], NonCommutative[DiracMatrix[e_]], NonCommutative[DiracMatrix[f_]], 
NonCommutative[DiracSlash[a_]], NonCommutative[ChiralityProjector[-1]]]:=-DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[DiracSlash[a]], NonCommutative[ChiralityProjector[+1]]]+FV[a,b]DTr[ NonCommutative[DiracMatrix[c]], 
NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-
FV[a,c]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ 
NonCommutative[ChiralityProjector[-1]]]+FV[a,d]DTr[NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[e]], NonCommutative[DiracMatrix[f]], 
NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]-FV[a,e]DTr[ NonCommutative[DiracMatrix[b]], NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], 
NonCommutative[DiracMatrix[f]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]+FV[a,f]DTr[ NonCommutative[DiracMatrix[b]], 
NonCommutative[DiracMatrix[c]], NonCommutative[DiracMatrix[d]], NonCommutative[DiracMatrix[e]], NonCommutative[ChiralityProjector[+1]]+ NonCommutative[ChiralityProjector[-1]]]/;FreeQ[{a,b,c,d,e,f},Internal]&&FreeQ[{a,b,c,d,e,f},Int];*)

fdtr[a_FourMomentum,b_FourMomentum]:=SP[a,b];
fdtr[a_FourMomentum,b_Index]:=FV[a,b];
fdtr[b_Index,a_FourMomentum]:=FV[a,b];
fdtr[b_Index,a_Index]:=ME[a,b];

DTr2[nc__NonCommutative,NonCommutative[ChiralityProjector[pm_]]]:=
  1/2Sum[
    (-1)^kd fdtr[{nc}[[1,1,1]],{nc}[[kd,1,1]]]Delete[DTr2[nc,NonCommutative[ChiralityProjector[+1]]+NonCommutative[ChiralityProjector[-1]]],{{1},{kd}}],
  {kd,2,Length[{nc}]}]+
  pm/2 Sum[Sum[(-1)^((kd-kd2-1)+(kd2-1)kd2/2) fdtr[{nc}[[kd2,1,1]],{nc}[[kd,1,1]]]*
    DTr2[Sequence@@Join[Delete[{nc},kd][[kd2+1;;Length[{nc}]-1]],If[kd2>1,{nc}[[kd2-1;;1;;-1]],{}]],NonCommutative[ChiralityProjector[+1]]-NonCommutative[ChiralityProjector[-1]]],
  {kd,kd2+1,Length[{nc}]}],{kd2,1,Length[{nc}]-1}]/;(Complement[Head/@({nc}/.NonCommutative->Identity),{DiracSlash,DiracMatrix}]==={}&&EvenQ[Length[{nc}]]&&Length[{nc}]>4&&
FreeQ[{nc},Internal]&&FreeQ[{nc},Int]);

DTr2[nc__NonCommutative,NonCommutative[ChiralityProjector[pm_]]]:=0/;(Complement[Head/@({nc}/.NonCommutative->Identity),{DiracSlash,DiracMatrix}]==={}&&OddQ[Length[{nc}]]&&
Length[{nc}]>4&&FreeQ[{nc},Internal]&&FreeQ[{nc},Int])


(* ::Subsubtitle:: *)
(*Distributive Dirac Trace*)


DTrDist0::usage="Trace over gamma matices, only distribution of the sum, no gamma algebra no mass coming out and keeping track of the propagator for the 
trace ordering related to the axial anomaly"


DTrDist0[a___,b_+c_,d___]:=DTrDist0[a,b,d]+DTrDist0[a,c,d]/;FreeQ[{b,c},Loop];
DTrDist0[a___,b_*c_?(FreeQ[#,NonCommutative]&),d___]:=c*DTrDist0[a,b,d];
DTrDist0[a___,b_*G[k_][l_][m__][n__],d___]:=G[k][l][m][n]*DTrDist0[a,b,d];


(* ::Subsubtitle:: *)
(*Distributive Dirac Trace*)


DTrDist::usage="Trace over gamma matices, only distribution of the sum, no gamma algebra"


DTrDist[a___,b_+c_,d___]:=DTrDist[a,b,d]+DTrDist[a,c,d];
DTrDist[a___,b_*c_?(FreeQ[#,NonCommutative]&),d___]:=c*DTrDist[a,b,d];
DTrDist[a___,b_*G[k_][l_][m__][n__],d___]:=G[k][l][m][n]*DTrDist[a,b,d];


(* ::Subsubtitle::Closed:: *)
(*Distributive Dirac Trace 2*)


DTrDist2::usage="Trace over gamma matices"


DTrDist2[a___,NonCommutative[DiracSlash[b__]+Mass[yy__]],c___]:=DTrDist2[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]DTrDist2[a,c];
DTrDist2[a___,NonCommutative[DiracSlash[b__]]+Mass[yy__],c___]:=DTrDist2[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]DTrDist2[a,c];

DTrDist2[a___,NonCommutative[DiracSlash[d_?(FreeQ[#,FourMomentum]&)*b__]],c___]:=d*DTrDist2[a,NonCommutative[DiracSlash[b]],c];
DTrDist2[a___,NonCommutative[DiracSlash[d_+b_]],c___]:=DTrDist2[a,NonCommutative[DiracSlash[b]],c]+DTrDist2[a,NonCommutative[DiracSlash[d]],c];

DTrDist2[a___,Mass[yy__],c___]:=Mass[yy]DTrDist2[a,c];

DTrDist2[a___,NonCommutative[DiracSlash[0]],c___]:=0;

DTrDist2[a___,b_+c_,d___]:=DTrDist2[a,b,d]+DTrDist2[a,c,d];
DTrDist2[a___,b_*c_?(FreeQ[#,NonCommutative]&),d___]:=c*DTrDist2[a,b,d];
DTrDist2[a___,b_*G[k_][l_][m__][n__],d___]:=G[k][l][m][n]*DTrDist2[a,b,d];
(*Depth of one for the feynman parameters*)
DTrDist2[a___,c_?((FreeQ[#,NonCommutative]&&Depth[#]<2)&),d___]:=c*DTrDist2[a,d];

DTrDist2[a___,NonCommutative[b_,c__],d___]:=DTrDist2[a,NonCommutative[b],NonCommutative[c],d];

DTrDist2[a___,NonCommutative[ChiralityProjector[1]],NonCommutative[ChiralityProjector[-1]],d___]:=0;
DTrDist2[a___,NonCommutative[ChiralityProjector[-1]],NonCommutative[ChiralityProjector[1]],d___]:=0;
DTrDist2[a___,NonCommutative[ChiralityProjector[pm_]],NonCommutative[ChiralityProjector[pm_]],d___]:=DTrDist2[a,NonCommutative[ChiralityProjector[pm]],d];


(* ::Subsubtitle::Closed:: *)
(*Fermion Chain*)


FCh::usage="Chain with gamma matices between two spinors"


FCh[a___,NonCommutative[DiracSlash[b__]+Mass[yy__]],c___]:=FCh[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]FCh[a,c];
FCh[a___,NonCommutative[DiracSlash[b__]]+Mass[yy__],c___]:=FCh[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]FCh[a,c];
FCh[a___,Mass[yy__],c___]:=Mass[yy]FCh[a,c];

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

FCh[a___,NonCommutative[DiracSlash[lm_]],d__,NonCommutative[DiracMatrix[b_]],NonCommutative[DiracSlash[lm_]],c___]:=(2FV[lm,b]*FCh[a,NonCommutative[DiracSlash[lm]],d,c]-
FCh[a,NonCommutative[DiracSlash[lm]],d,NonCommutative[DiracSlash[lm]],NonCommutative[DiracMatrix[b]],c])/;FreeQ[{d},b];

FCh[a___,NonCommutative[DiracSlash[lm_]],d__,NonCommutative[DiracSlash[b_]],NonCommutative[DiracSlash[lm_]],c___]:=(2SP[lm,b]*FCh[a,NonCommutative[DiracSlash[lm]],d,c]-
FCh[a,NonCommutative[DiracSlash[lm]],d,NonCommutative[DiracSlash[lm]],NonCommutative[DiracSlash[b]],c])/;FreeQ[{d},b];

FCh[a___,NonCommutative[DiracMatrix[l_]],d__,NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[l_]],c___]:=(2ME[b,l]*FCh[a,NonCommutative[DiracMatrix[l]],d,c]-
FCh[a,NonCommutative[DiracMatrix[l]],d,NonCommutative[DiracMatrix[l]],NonCommutative[DiracMatrix[b]],c])/;FreeQ[{d},b];

FCh[a___,NonCommutative[DiracMatrix[l_]],d__,NonCommutative[DiracSlash[b_]],NonCommutative[DiracMatrix[l_]],c___]:=(2FV[b,l]*FCh[a,NonCommutative[DiracMatrix[l]],d,c]-
FCh[a,NonCommutative[DiracMatrix[l]],d,NonCommutative[DiracMatrix[l]],NonCommutative[DiracSlash[b]],c])/;FreeQ[{d},b];


(* ::Subsubtitle::Closed:: *)
(*Distributive Fermion Chain*)


FChDist::usage="Chain with gamma matices between two spinors, only distribution of the sum, no gamma algebra"


FChDist[a___,b_+c_,d___]:=FChDist[a,b,d]+FChDist[a,c,d];
FChDist[a___,b_*c_?(FreeQ[#,NonCommutative]&),d___]:=c*FChDist[a,b,d];
FChDist[a___,b_*G[k_][l_][m__][n__],d___]:=G[k][l][m][n]*FChDist[a,b,d];


(* ::Subsubtitle::Closed:: *)
(*Distributive Fermion Chain 2*)


FChDist2::usage="Chain with gamma matices between two spinors which distribute all the sums"


FChDist2[a___,NonCommutative[DiracSlash[b__]+Mass[yy__]],c___]:=FChDist2[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]FChDist2[a,c];
FChDist2[a___,NonCommutative[DiracSlash[b__]]+Mass[yy__],c___]:=FChDist2[a,NonCommutative[DiracSlash[b]],c]+Mass[yy]FChDist2[a,c];

FChDist2[a___,NonCommutative[DiracSlash[d_?(FreeQ[#,FourMomentum]&)*b__]],c___]:=d*FChDist2[a,NonCommutative[DiracSlash[b]],c];
FChDist2[a___,NonCommutative[DiracSlash[d_+b_]],c___]:=FChDist2[a,NonCommutative[DiracSlash[b]],c]+FChDist2[a,NonCommutative[DiracSlash[d]],c];

FChDist2[a___,NonCommutative[DiracSlash[-b_]],c___]:=-FChDist2[a,NonCommutative[DiracSlash[b]],c];
FChDist2[a___,NonCommutative[DiracSlash[0]],c___]:=0;

FChDist2[a___,b_+c_,d___]:=FChDist2[a,b,d]+FChDist2[a,c,d];
FChDist2[a___,b_*c_?(FreeQ[#,NonCommutative]&),d___]:=c*FChDist2[a,b,d];
FChDist2[a___,b_*G[k_][l_][m__][n__],d___]:=G[k][l][m][n]*FChDist2[a,b,d];
FChDist2[a___,c_?((FreeQ[#,NonCommutative]&&Depth[#]<2)&),d___]:=c*FChDist2[a,d];

FChDist2[a___,NonCommutative[b_,c__],d___]:=FChDist2[a,NonCommutative[b],NonCommutative[c],d];

FChDist2[a___,NonCommutative[ChiralityProjector[1]],NonCommutative[ChiralityProjector[-1]],d___]:=0;
FChDist2[a___,NonCommutative[ChiralityProjector[-1]],NonCommutative[ChiralityProjector[1]],d___]:=0;
FChDist2[a___,NonCommutative[ChiralityProjector[pm_]],NonCommutative[ChiralityProjector[pm_]],d___]:=FChDist2[a,NonCommutative[ChiralityProjector[pm]],d];


(* ::Subsubtitle::Closed:: *)
(*ScalarProduct*)


SP::usage="scalar product"


(*Attributes[SP]=Orderless; does not work for parallel*)
SP[-a_,b_]:=-SP[a,b];
SP[-a_,a_]:=-SP[a,a];
SP[-a_,-a_]:=SP[a,a];
SP[a_,b_+c_]:=SP[a,b]+SP[a,c];
SP[a_,b_*FourMomentum[c__]]:=b*SP[a,FourMomentum[c]];
SP[a_,b_*c_]:=b*SP[a,c]/;Depth[b]<2;
SP[a_,b_?(FreeQ[#,FourMomentum]&)*c_]:=b*SP[a,c];
SP[b_,-a_]:=-SP[b,a];
SP[a_,-a_]:=-SP[a,a];
SP[b_+c_,a_]:=SP[b,a]+SP[c,a];
SP[b_*FourMomentum[c__],a_]:=b*SP[FourMomentum[c],a];
SP[b_*c_,a_]:=b*SP[c,a]/;Depth[b]<2;
SP[b_?(FreeQ[#,FourMomentum]&)*c_,a_]:=b*SP[a,c];
SP[FourMomentum[a_,b_],FourMomentum[c_,d_]]:=SP[FourMomentum[c,d],FourMomentum[a,b]]/;d<b;
SP[FourMomentum[a_,b_],FourMomentum[a_,d_]]:=SP[FourMomentum[a,d],FourMomentum[a,b]]/;d<b;
SP[FourMomentum[Incoming,b_],FourMomentum[Internal,b_]]:=SP[FourMomentum[Internal,b],FourMomentum[Incoming,b]];


I


(* ::Subsubtitle::Closed:: *)
(*DiracOpenChain*)


DiracOpenChain::usage="product of gamma matrices with free external dirac indices"


DiracOpenChain[a___,ChiralityProjector[pm_],b___][ind1_,ind2_]:=DiracOpenChain[a,NonCommutative[ChiralityProjector[pm]],b][ind1,ind2];
DiracOpenChain[a___,DiracMatrix[lo_],b___][ind1_,ind2_]:=DiracOpenChain[a,NonCommutative[DiracMatrix[lo]],b][ind1,ind2];
DiracOpenChain[a___,DiracSlash[mo_],b___][ind1_,ind2_]:=DiracOpenChain[a,NonCommutative[DiracSlash[mo]],b][ind1,ind2];

DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, i_], Index[Dirac, j_]]DiracOpenChain[b___][Index[Dirac, j_], Index[Dirac, k_]]:=DiracOpenChain[a,b][Index[Dirac, i], Index[Dirac, k]];

(*(*reverse the flow of the first fermion + fermion flip*)
DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, 2], Index[Dirac, 1]]DiracObject[SpinorType[b_][w__]][Index[Dirac, 2]]*
DiracObject[SpinorType[c_][-FourMomentum[Incoming,mom_],z__]][Index[Dirac, 1]]:=FS$1*FermionChain[NonCommutative[SpinorType[b][w]],a,NonCommutative[SpinorType[c][FourMomentum[Incoming,mom],z]]];

(*reverse the flow of the last fermion*)
DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, 1], Index[Dirac, 2]]DiracObject[SpinorType[b_][w__]][Index[Dirac, 1]]*
DiracObject[SpinorType[c_][-FourMomentum[Incoming,mom_],z__]][Index[Dirac, 2]]:=-FS$1bis*FermionChain[NonCommutative[SpinorType[b][w]],a,NonCommutative[SpinorType[c][FourMomentum[Incoming,mom],z]]];*)

(*do the two commented above and more*)
DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, i_], Index[Dirac, j_]]DiracObject[(SpinorType[b_]|DiracSpinor)[w__]][Index[Dirac, i_]]*
DiracObject[(SpinorType[c_]|DiracSpinor)[-FourMomentum[Incoming,mom_],z__]][Index[Dirac, j_]]:=(-1)FS$1*Signature[{i,j}]*
FermionChain[NonCommutative[SpinorType[b][w]],a,NonCommutative[SpinorType[c][FourMomentum[Incoming,mom],z]]]/;OddQ[Min[{i,j}]]&&Abs[i-j]===1;

(*same but for 4-fermions (for 2n fermions, (-1)^2\[Rule](-1)^n, and so on)*)
DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, i_], Index[Dirac, j_]]DiracObject[(SpinorType[b_]|DiracSpinor)[w__]][Index[Dirac, i_]]*
DiracObject[(SpinorType[c_]|DiracSpinor)[-FourMomentum[Incoming,mom_],z__]][Index[Dirac, j_]]*
DiracOpenChain[a2___][Index[Dirac, i2_], Index[Dirac, j2_]]DiracObject[(SpinorType[b2_]|DiracSpinor)[w2__]][Index[Dirac, i2_]]*
DiracObject[(SpinorType[c2_]|DiracSpinor)[-FourMomentum[Incoming,mom2_],z2__]][Index[Dirac, j2_]]:=(-1)^2*Signature[{i,j,i2,j2}]*(-1)FS$1bis*
FermionChain[NonCommutative[SpinorType[b][w]],a,NonCommutative[SpinorType[c][FourMomentum[Incoming,mom],z]]]*
FermionChain[NonCommutative[SpinorType[b2][w2]],a2,NonCommutative[SpinorType[c2][FourMomentum[Incoming,mom2],z2]]]/;OddQ[Min[{i,j,i2,j2}]]&&
Sort[{i,j,i2,j2}]===Table[kkdch,{kkdch,Min[{i,j,i2,j2}],Max[{i,j,i2,j2}]}];

DiracOpenChain[a___,b_+c_,d___][ind1_,ind2_]:=DiracOpenChain[a,b,d][ind1,ind2]+DiracOpenChain[a,c,d][ind1,ind2]/;FreeQ[b+c,Loop];
DiracOpenChain[a___,b_+c_,d___][ind1_,ind2_]:=DiracOpenChain[a,(b/.{Internal->PropInternal,Incoming->PropIncoming}),d][ind1,ind2]+
  DiracOpenChain[a,(c/.{Internal->PropInternal,Incoming->PropIncoming}),d][ind1,ind2]/;Not[FreeQ[b+c,Loop]];
DiracOpenChain[a___,-b_,d___][ind1_,ind2_]:=-DiracOpenChain[a,b,d][ind1,ind2];

(*DiracOpenChain[a___,b_Mass,c___][ind1_,ind2_]:=b DiracOpenChain[a,c][ind1,ind2];*)

(*reversing the flow of a sub chain*)
DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, i_], Index[Dirac, j_]]*DiracOpenChain[b___][Index[Dirac, k_], Index[Dirac, j_]]:=
If[EvenQ[j-k],-1*FS$2,1*FS$2bis]*DiracOpenChain[a,Sequence@@CC2/@Reverse[{b}]][Index[Dirac, i], Index[Dirac, k]]/;j>k&&j>i&&i<k;

DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, j_], Index[Dirac, i_]]*DiracOpenChain[b___][Index[Dirac, j_], Index[Dirac, k_]]:=
If[EvenQ[j-i],-1*FS$3,1*FS$3bis]*DiracOpenChain[Sequence@@CC2/@Reverse[{a}],b][Index[Dirac, i], Index[Dirac, k]]/;j>i&&j>k&&i<k;

(*make it faster*)
DiracOpenChain[a___,NonCommutative[ChiralityProjector[1]],NonCommutative[ChiralityProjector[-1]],b___][Index[Dirac, i_], Index[Dirac, j_]]:=0;
DiracOpenChain[a___,NonCommutative[ChiralityProjector[-1]],NonCommutative[ChiralityProjector[1]],b___][Index[Dirac, i_], Index[Dirac, j_]]:=0;


(*Fermion loop*)
DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, i_], Index[Dirac, j_]]*DiracOpenChain[b___][Index[Dirac, i_], Index[Dirac, j_]]:= 
-(FS$4bis*MatrixTrace[a,Sequence@@CC2/@Reverse[{b}]])/;i<j;

DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, i_], Index[Dirac, j_]]*DiracOpenChain[b___][Index[Dirac, i_], Index[Dirac, j_]]:= 
-(FS$4ter*MatrixTrace[b,Sequence@@CC2/@Reverse[{a}]])/;i>j;

DiracOpenChain/:DiracOpenChain[a___][Index[Dirac, i_], Index[Dirac, i_]]:=-FS$4*MatrixTrace[a];


CC2[NonCommutative[ChiralityProjector[pm_]]] :=NonCommutative[ChiralityProjector[pm]];
CC2[NonCommutative[a:_DiracMatrix|_DiracSlash]] := -NonCommutative[a];
CC2[a_Mass] := a;


(* ::Subsubtitle:: *)
(*Eps replacement*)


(*eps and 3 gamma*)
Eps3gamrep={FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, ccc_], Index[Lorentz, ddd_], Index[Lorentz, aaa_], Index[Lorentz, bbb_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm1*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  ddd_], Index[Lorentz, ccc_], Index[Lorentz, aaa_], Index[Lorentz, bbb_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm2*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  ccc_], Index[Lorentz, aaa_], Index[Lorentz, ddd_], Index[Lorentz, bbb_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm3*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  ccc_], Index[Lorentz, ddd_], Index[Lorentz, bbb_], Index[Lorentz, aaa_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm4*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  aaa_], Index[Lorentz, ddd_], Index[Lorentz, ccc_], Index[Lorentz, bbb_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm5*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  bbb_], Index[Lorentz, ddd_], Index[Lorentz, aaa_], Index[Lorentz, ccc_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm6*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  ccc_], Index[Lorentz, bbb_], Index[Lorentz, aaa_], Index[Lorentz, ddd_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm7*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, ddd_], Index[Lorentz, aaa_], Index[Lorentz, ccc_], Index[Lorentz, bbb_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm8*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  ccc_], Index[Lorentz, aaa_], Index[Lorentz, bbb_], Index[Lorentz, ddd_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm9*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, Incoming, aaa_], Index[Lorentz, ccc_], Index[Lorentz, ddd_], Index[Lorentz, bbb_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm10*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  ccc_], Index[Lorentz, bbb_], Index[Lorentz, ddd_], Index[Lorentz, aaa_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm11*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, ddd_], Index[Lorentz, aaa_], Index[Lorentz, bbb_], Index[Lorentz, ccc_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm12*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  aaa_], Index[Lorentz, bbb_], Index[Lorentz, ccc_], Index[Lorentz, ddd_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm13*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  bbb_], Index[Lorentz, ccc_], Index[Lorentz, ddd_], Index[Lorentz, aaa_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm14*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, aaa_], Index[Lorentz, bbb_], Index[Lorentz, ddd_], Index[Lorentz, ccc_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm15*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  aaa_], Index[Lorentz, ccc_], Index[Lorentz, bbb_], Index[Lorentz, ddd_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm16*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  aaa_], Index[Lorentz, ddd_], Index[Lorentz, bbb_], Index[Lorentz, ccc_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm17*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  bbb_], Index[Lorentz, aaa_], Index[Lorentz, ccc_], Index[Lorentz, ddd_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm18*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  bbb_], Index[Lorentz, aaa_], Index[Lorentz, ddd_], Index[Lorentz, ccc_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm19*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, bbb_], Index[Lorentz, ccc_], Index[Lorentz, aaa_], Index[Lorentz, ddd_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm20*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, bbb_], Index[Lorentz, ddd_], Index[Lorentz, ccc_], Index[Lorentz, aaa_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm21*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, ddd_], Index[Lorentz, bbb_], Index[Lorentz, aaa_], Index[Lorentz, ccc_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm22*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz, ddd_], Index[Lorentz, bbb_], Index[Lorentz, ccc_], Index[Lorentz, aaa_]]:>
-NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm23*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, ddd_]]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[Index[Lorentz,  ddd_], Index[Lorentz, ccc_], Index[Lorentz, bbb_], Index[Lorentz, aaa_]]:>
NLO$Eps3gamsign*mypm*(2I(M$dim-1)(1+NLO$Eps3gam(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]]};

(*eps and 3 gamma with FV*)
Eps3gamrepFV={FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb_]]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[FV[FourMomentum[Incoming, ddd_]], Index[Lorentz, aaa_], Index[Lorentz, bbb_], Index[Lorentz, ccc_]]:>
NLO$Eps3gamsignFV*mypm*(2I(M$dim-1)(1+NLO$Eps3gamFV(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracSlash[FourMomentum[Incoming, ddd]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm1*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb_]]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[FV[FourMomentum[Incoming, ddd_]], Index[Lorentz, aaa_], Index[Lorentz, ccc_], Index[Lorentz, bbb_]]:>
-NLO$Eps3gamsignFV*mypm*(2I(M$dim-1)(1+NLO$Eps3gamFV(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracSlash[FourMomentum[Incoming, ddd]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm2*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb_]]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[FV[FourMomentum[Incoming, ddd_]], Index[Lorentz, bbb_], Index[Lorentz, ccc_], Index[Lorentz, aaa_]]:>
NLO$Eps3gamsignFV*mypm*(2I(M$dim-1)(1+NLO$Eps3gamFV(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracSlash[FourMomentum[Incoming, ddd]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm3*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb_]]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[FV[FourMomentum[Incoming, ddd_]], Index[Lorentz, bbb_], Index[Lorentz, aaa_], Index[Lorentz, ccc_]]:>
-NLO$Eps3gamsignFV*mypm*(2I(M$dim-1)(1+NLO$Eps3gamFV(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracSlash[FourMomentum[Incoming, ddd]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm4*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb_]]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[FV[FourMomentum[Incoming, ddd_]], Index[Lorentz, ccc_], Index[Lorentz, aaa_], Index[Lorentz, bbb_]]:>
NLO$Eps3gamsignFV*mypm*(2I(M$dim-1)(1+NLO$Eps3gamFV(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracSlash[FourMomentum[Incoming, ddd]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]],
(*perm5*)
FCh[NonCommutative[SpinorType[mygen1_][myfspec1__]], NonCommutative[DiracMatrix[Index[Lorentz, aaa_]]], 
NonCommutative[DiracMatrix[Index[Lorentz, bbb_]]], NonCommutative[DiracMatrix[Index[Lorentz, ccc_]]], 
NonCommutative[ChiralityProjector[mypm_]], NonCommutative[SpinorType[mygen2_][myfsepc2__]]]*
LCivita[FV[FourMomentum[Incoming, ddd_]], Index[Lorentz, ccc_], Index[Lorentz, bbb_], Index[Lorentz, aaa_]]:>
-NLO$Eps3gamsignFV*mypm*(2I(M$dim-1)(1+NLO$Eps3gamFV(4-M$dim)))FCh[NonCommutative[SpinorType[mygen1][myfspec1]], 
NonCommutative[DiracSlash[FourMomentum[Incoming, ddd]]],  NonCommutative[ChiralityProjector[mypm]], 
NonCommutative[SpinorType[mygen2][myfsepc2]]]}


(* ::Subsubtitle:: *)
(*Same amp testing*)


(*just permutation of ext BOSONS for fermion be carefull for the sign : so far only vector, how to collect all the scalar indices and mom*)
AmpRenaming[amp1_,amp2_,next_]:=Block[{artmp,arden1,arden2,arext1,arext2,arv1,arv2,arrep,arverttype2,arverttype1,arc},
DPrint[Style["in AR",Green]];
(*Check if same particles in the loop*)
arden1=Cases[amp1,_FeynAmpDenominator,\[Infinity]][[1]];
arden2=Cases[amp2,_FeynAmpDenominator,\[Infinity]][[1]];
If[Sort[Cases[arden1,Mass[a_[x_],Loop]->a,\[Infinity]]]=!=Sort[Cases[arden2,Mass[a_[x_],Loop]->a,\[Infinity]]],Return[False]];
Print[Style["in AR, same deno",Green]];
(*list of vertex type*)
arverttype1=((#/.-1->1)[[1;;,0]]&)/@Union[Cases[#,G[ara_][arb_][arc__][ard__]*arx__->List[arc]]]&/@Cases[amp1,G[_][_][__][__]*__+__];
arverttype2=((#/.-1->1)[[1;;,0]]&)/@Union[Cases[#,G[ara_][arb_][arc__][ard__]*arx__->List[arc]]]&/@Cases[amp2,G[_][_][__][__]*__+__];
If[Sort[arverttype1]=!=Sort[arverttype2],Return[False]];
Print[Style["in AR, same vert type",Green]];
DPrint[InputForm[arden1]];DPrint[InputForm[arden2]];
(*List of external but the Scalar?*)
arext1=Cases[amp1,a:_PolarizationVector|_PolarizationTensor|SpinorType[a_][b__],\[Infinity]];
arext2=Cases[amp2,a:_PolarizationVector|_PolarizationTensor|SpinorType[a_][b__],\[Infinity]];
(*Check for permutation of the external vectors*)
arv1=Cases[arext1,_PolarizationVector];
arv2=Permutations[Cases[arext2,_PolarizationVector]];
For[arkk=1,arkk<=Length[arv2],arkk++,
  arrep=Rule@@@Transpose[{Flatten[arv1/.{V->Identity,PolarizationVector->List}],Flatten[arv2[[arkk]]/.{V->Identity,PolarizationVector->List}]}];
  artmp=amp2/.arrep/.FeynAmpDenominator->FeynAmpDenominator2;
  If[artmp===(amp1/.FeynAmpDenominator->FeynAmpDenominator2),Return[Reverse/@arrep]];
  If[artmp===(amp1/.FeynAmpDenominator->FeynAmpDenominator2)/.-FourMomentum[Incoming,aaa_]->Sum[FourMomentum[Incoming,kk],{kk,1,next}]-FourMomentum[Incoming,aaa],Return[Reverse/@arrep]];
  (*check that the difference cannot be removed by momentum conservation*)
  artmp=amp2/.FeynAmpDenominator->FeynAmpDenominator2/.-FourMomentum[Incoming,aaa_]->Sum[FourMomentum[Incoming,kk],{kk,1,next}]-FourMomentum[Incoming,aaa]/.arrep;
  If[artmp===(amp1/.FeynAmpDenominator->FeynAmpDenominator2),Return[Reverse/@arrep]];
  If[artmp===((amp1/.FeynAmpDenominator->FeynAmpDenominator2)/.-FourMomentum[Incoming,aaa_]->Sum[FourMomentum[Incoming,kk],{kk,1,next}]-FourMomentum[Incoming,aaa]),Return[Reverse/@arrep]];
  DPrint[InputForm[(amp1/.FeynAmpDenominator->FeynAmpDenominator2)/.-FourMomentum[Incoming,aaa_]->Sum[FourMomentum[Incoming,kk],{kk,1,next}]-FourMomentum[Incoming,aaa]]];
  DPrint[InputForm[artmp]];
];
DPrint[InputForm[(amp1/.FeynAmpDenominator->FeynAmpDenominator2)/.-FourMomentum[Incoming,aaa_]->Sum[FourMomentum[Incoming,kk],{kk,1,next}]-FourMomentum[Incoming,aaa]]];
DPrint[InputForm[artmp]];
(*no match found*)
False
];
SetAttributes[FeynAmpDenominator2,Orderless];
NLO$currentvert={};
NLO$ampR2={};


(* ::Subsection:: *)
(*Rotate fermion trace *)


RotateTr[tr_,rtind_]:=Block[{v1pos,splitpos,splitpos2,rtres},
  (*Print[tr];*)
  If[rtind===Null,
    splitpos=Sort[Flatten[{0,Position[tr,Mass][[1;;,1]],Position[tr,PropIncoming][[1;;,1]],
    Position[tr,PropInternal][[1;;,1]],Length[tr]+1}]];
    v1pos=0;
    rtres=1;
    While[v1pos<1&&rtres<Length[splitpos],
      If[(splitpos[[rtres]]+1)<=(splitpos[[rtres+1]]-1)&&FreeQ[tr[[(splitpos[[rtres]]+1);;(splitpos[[rtres+1]]-1)]],Lorentz],
        v1pos=splitpos[[rtres]]+1;];
      rtres++;
    ];
    ,
    v1pos=Position[tr,Index[Lorentz,rtind]][[1,1]];
  ];
  splitpos=Max[DeleteCases[Flatten[{Position[tr,Mass][[1;;,1]],Position[tr,PropIncoming][[1;;,1]],
    Position[tr,PropInternal][[1;;,1]],0}],_?(#>v1pos&)]];
  splitpos2=Min[DeleteCases[Flatten[{Position[tr,Mass][[1;;,1]],Position[tr,PropIncoming][[1;;,1]],
    Position[tr,PropInternal][[1;;,1]],Length[tr]+1}],_?(#<v1pos&)]];
  If[NLO$DebugCount<0,Print[splitpos];Print[splitpos2];Print[v1pos];];
  
  If[(FreeQ[tr[[Max[splitpos,1];;v1pos]],ChiralityProjector]&&Not[FreeQ[tr[[v1pos;;Min[splitpos2,Length[tr]]]],ChiralityProjector]])||
    Not[FreeQ[tr[[v1pos]],ChiralityProjector]]&&Length[tr[[v1pos]]]===Position[tr[[v1pos]],ChiralityProjector][[1,1]],
     rtres=dumtr1*If[splitpos<1,tr,Join[tr[[splitpos+1;;]],tr[[;;splitpos]]]]/.{PropInternal->Internal,PropIncoming->Incoming};
     ,
     If[Not[FreeQ[tr[[Max[splitpos,1];;v1pos]],ChiralityProjector]]&&FreeQ[tr[[v1pos;;Min[splitpos2,Length[tr]]]],ChiralityProjector]||
       Not[FreeQ[tr[[v1pos]],ChiralityProjector]]&&1===Position[tr[[v1pos]],ChiralityProjector][[1,1]],
       rtres=dumtr2*If[splitpos2>Length[tr],tr,Join[tr[[splitpos2;;]],tr[[;;splitpos2-1]]]]/.{PropInternal->Internal,PropIncoming->Incoming};
       ,
       Print[Style["The following trace cannot be rotated as needed for the anomaly",Orange]];
       Print[InputForm[tr]];
       rtres=dumtr3*tr/.{PropInternal->Internal,PropIncoming->Incoming};
     ];
   ];
   If[Not[FreeQ[rtres,PropInternal]],Print[Style["PropInternal1",Pink]];Print[InputForm[rtres]];];
   If[Not[FreeQ[rtres,PropIncoming]],Print[Style["PropIncoming1",Pink]];Print[InputForm[rtres]];];
   NLO$DebugCount++;
   If[NLO$DebugCount<0(*||rtind===Null*),Print["tr check"];Print[InputForm[tr]];Print[rtres];];
   rtres
];


(* ::Subtitle:: *)
(*4 Fermions*)


(*basis of Four-Fermions structure*) 
NLO$DiracBasis = {NLO$4FDS[{ChiralityProjector[+1]},{ChiralityProjector[+1]}],
                  NLO$4FDS[{ChiralityProjector[+1]},{ChiralityProjector[-1]}],
                  NLO$4FDS[{ChiralityProjector[-1]},{ChiralityProjector[+1]}],
                  NLO$4FDS[{ChiralityProjector[-1]},{ChiralityProjector[-1]}],
                  NLO$4FDS[{DiracMatrix[Index[Lorentz,Int[NLO$Eva1]]],ChiralityProjector[+1]},{DiracMatrix[Index[Lorentz,Int[NLO$Eva1]]],ChiralityProjector[+1]}],
                  NLO$4FDS[{DiracMatrix[Index[Lorentz,Int[NLO$Eva1]]],ChiralityProjector[+1]},{DiracMatrix[Index[Lorentz,Int[NLO$Eva1]]],ChiralityProjector[-1]}],
                  NLO$4FDS[{DiracMatrix[Index[Lorentz,Int[NLO$Eva1]]],ChiralityProjector[-1]},{DiracMatrix[Index[Lorentz,Int[NLO$Eva1]]],ChiralityProjector[+1]}],
                  NLO$4FDS[{DiracMatrix[Index[Lorentz,Int[NLO$Eva1]]],ChiralityProjector[-1]},{DiracMatrix[Index[Lorentz,Int[NLO$Eva1]]],ChiralityProjector[-1]}],
                  NLO$4FDS[{SigmaMatrix[Index[Lorentz,Int[NLO$Eva1]],Index[Lorentz,Int[NLO$Eva2]]],ChiralityProjector[+1]},{SigmaMatrix[Index[Lorentz,Int[NLO$Eva1]],Index[Lorentz,Int[NLO$Eva2]]],ChiralityProjector[+1]}],
                  NLO$4FDS[{SigmaMatrix[Index[Lorentz,Int[NLO$Eva1]],Index[Lorentz,Int[NLO$Eva2]]],ChiralityProjector[-1]},{SigmaMatrix[Index[Lorentz,Int[NLO$Eva1]],Index[Lorentz,Int[NLO$Eva2]]],ChiralityProjector[-1]}]};

(*List of evanescent operators as {I:initial structure, R:4d reduction}, Evanescent = I - R, in the same format as NLO$DiracBasis, pair of List of Dirac matrices in NLO$4FDS,
Summed Lorentz indices are Int[NLO$Eva1],Int[NLO$Eva2],... ; R is a sum of element in that format*)
NLO$EvaList={};

(*should be a minus in front of LCicita*)
NLO$EvaMF={FCh[x_,NonCommutative[DiracMatrix[a_]],NonCommutative[DiracMatrix[b_]],NonCommutative[DiracMatrix[c_]],y__]:>
  FCh[x,NonCommutative[DiracMatrix[a]],y]ME[b,c]-FCh[x,NonCommutative[DiracMatrix[b]],y]ME[a,c]+FCh[x,NonCommutative[DiracMatrix[c]],y]ME[b,a]+
  I Module[{NLOelc},LCivita[a,b,c,Index[Lorentz,NLOelc]]FCh[x,NonCommutative[DiracMatrix[Index[Lorentz,NLOelc]]],(NonCommutative[ChiralityProjector[+1]]-NonCommutative[ChiralityProjector[-1]]),y]]};

(*Reduce 4F operators to the one of the basis and an evanescent operators, assuming dim6 operators i.e. no DiracSlashed*)
EvanescentReduce[f1_FCh*f2_FCh]:=Block[{f1mat,f2mat,d4op,evop,lorep,erpos,MyInt},

(*Print["in Eva red"];*)
(*Print[InputForm[{f1,f2}]];*)

lorep = Cases[f1,Index[Lorentz,_],\[Infinity]];
lorep = Table[Rule[lorep[[lorepii]],Index[Lorentz,Int[ ToExpression["NLO$Eva"<>ToString[lorepii]] ] ] ],{lorepii,Length[lorep]}];
(*set in the format of the basis*)
f1mat=Cases[f1/.NonCommutative->Identity,_DiracMatrix|_ChiralityProjector]/.lorep;
f2mat=Cases[f2/.NonCommutative->Identity,_DiracMatrix|_ChiralityProjector]/.lorep;
(*Print[InputForm[lorep]];Print[InputForm[{f1mat,f2mat}]];*)

(*If match one of the element of the basis, nothing to do*)
If[MatchQ[NLO$4FDS[f1mat,f2mat], Alternatives@@NLO$DiracBasis],(*DPrint["Nothing to do"];*)Return[Plus@@{f1*f2,0}]];

(*If already in the list of evanescent operators, use the previous reduction*)
If[MatchQ[NLO$4FDS[f1mat,f2mat],Alternatives@@(NLO$EvaList[[All,1]])],
  (*Print["Already in the List"];*)
  erpos = Position[NLO$EvaList,NLO$4FDS[f1mat,f2mat]][[1,1]];
  d4op = NLO$EvaList[[erpos,2]];
  evop = NLO$EvaList[[erpos,1]];
  evop = FCh[f1[[1]],NonCommutative[NLO$EO[erpos,1]],Last[f1]]*FCh[f2[[1]],NonCommutative[NLO$EO[erpos,2]],Last[f2]];,
  (*DPrint["Not in the list of Evanescent yet"];*)
  (*If product of 2 gamma with same projectors (Basis dependent) replace by sigma and eta*)
  If[MatchQ[{f1mat,f2mat},{{DiracMatrix[Index[Lorentz,a_]],DiracMatrix[Index[Lorentz,b_]],ChiralityProjector[pm_]},{DiracMatrix[Index[Lorentz,a_]],DiracMatrix[Index[Lorentz,b_]],ChiralityProjector[pm_]}}],
    (*if 2 2 gamma with same projectors, not a new evanesecent op*)
    d4op = FixAwithTr[NLO$4FDS[f1mat, f2mat],0];
    evop = 0;,
    d4op = FixAwithTr[NLO$4FDS[f1mat, f2mat],Length[NLO$EvaList]+1];  
    (*add new evanescent op to the list*)
    NLO$EvaList=Append[NLO$EvaList,{NLO$4FDS[f1mat,f2mat],d4op}];
    (*put evanescent op with its number (position in NLO$EvaList) in FCh*)
    evop = FCh[f1[[1]],NonCommutative[NLO$EO[Length[NLO$EvaList],1]],Last[f1]]*FCh[f2[[1]],NonCommutative[NLO$EO[Length[NLO$EvaList],2]],Last[f2]];
  ];
  (*If product of 2 gamma with different projectors (Basis dependent)*)
  (*If[MatchQ[{f1mat,f2mat},{{DiracMatrix[Index[Lorentz,a_]],DiracMatrix[Index[Lorentz,b_]],ChiralityProjector[pm_]},{DiracMatrix[Index[Lorentz,a_]],DiracMatrix[Index[Lorentz,b_]],ChiralityProjector[pm2_]}}/;pm2==-pm],
    (*DPrint["2 dirac matrices with proj not in the basis"];*)
    (*Change to split in sigma and eta*)
    d4op = NLO$4FDS[DeleteCases[f1mat,_DiracMatrix],DeleteCases[f2mat,_DiracMatrix]];,
    (*DPrint["More than 2 dirac matrices and a projector"];*)
    If[Length[f1mat]<4,Print[Style["Error: less than 3 dirac matrices but not in the basis or 2 dirac matrices and opposite projectors",Red]];Abort[]];
    (*Use 4 dim relation for product of 3 gamma to get the f (ga[mu]ga[nu]ga[rho]=ME[mu,nu]ga[rho]+ME[rho,nu]ga[mu]-ME[mu,rho]ga[nu]+I Eps[mu,nu,rho,sig]ga[sig]ga5)*)
    (*Replace Int because use d=4 algebra*)
    d4op = Expand[FCh[f1[[1]],Sequence@@NonCommutative/@f1mat,Last[f1]]*(FCh[f2[[1]],Sequence@@NonCommutative/@f2mat,Last[f2]]//.NLO$EvaMF)/.Int->MyInt];
    d4op = Expand[d4op//.NLO$EvaMF]/.MyInt->Int/.M$dim->4;
    (*in the notation of the basis and same name for summed indices*)
    d4op = d4op/.fa_FCh*fb_FCh:>NLO$4FDS[Cases[fa/.NonCommutative->Identity,_DiracMatrix|_ChiralityProjector],Cases[fb/.NonCommutative->Identity,_DiracMatrix|_ChiralityProjector]];
    d4op = d4op/.Rule@@@Transpose[{(NLO$DiracBasis/.Int[a_]:>Int[ToExpression[ToString[a]<>"x2"],_]/.Int->Pattern),NLO$DiracBasis}];
    (*DPrint[InputForm[d4op]];*)
  ];*)
  (*Only an A(Number of Evan, Number of el of basis) for the operators that have an F*)
  (*For[erpos=1,erpos<=Length[NLO$DiracBasis],erpos++,
    d4op = d4op + If[Not[FreeQ[d4op,NLO$DiracBasis[[erpos]]]],(4-M$dim)/2 NLO$EvaA[Length[NLO$EvaList]+1,erpos] NLO$DiracBasis[[erpos]],0];
  ];(*end for*)*)

];(*end if already*)
(*Print[InputForm[d4op]];*)
  (* add nonCommutative,Spinors,FCh to the 4d part *)
  d4op = d4op /. NLO$4FDS[fa_,fb_]:>FCh[f1[[1]],Sequence@@NonCommutative/@fa,Last[f1]]FCh[f2[[1]],Sequence@@NonCommutative/@fb,Last[f2]]/.FR$Eps->(M$dim-4);
(*Print[InputForm[{d4op,evop}]];Print[InputForm[lorep]];*)
  Return[Plus@@{d4op,evop}/.Reverse/@lorep]

];(*end Block ER*)

(*Sort the indices in the same way on both fermion chain*)
Sort4FermionChain[f1_FCh*f2_FCh]:=Block[{tmpf2,order,lowerf,f2order,count,s4fsign},
  order = Cases[f1,Index[Lorentz,b_],\[Infinity]];
  tmpf2=f2;
  f2order = Cases[tmpf2,Index[Lorentz,b_],\[Infinity]];
  lowerf=0;

  If[Sort[order]=!=Sort[f2order],
    Print[Style["Error : 2 fermions chain do not have the same lorentz indices",Red]];Print[InputForm[{f1,f2}]];Return[];];
  count=1;(*avoid infinite loop*)
  s4fsign=1;(*sign of the fermion chain*)
  While[f2order=!=order&&count<100,
  count++;
    For[ii=1,ii<Length[order],ii++,
      If[Position[order,f2order[[ii]]][[1,1]]>Position[order,f2order[[ii+1]]][[1,1]],
        lowerf = lowerf+s4fsign * f1*(Delete[tmpf2,{{ii+1},{ii+2}}])*2*ME[f2order[[ii]],f2order[[ii+1]]];
        tmpf2 = tmpf2/.{f2order[[ii]]->f2order[[ii+1]],f2order[[ii+1]]->f2order[[ii]]};
        s4fsign = -s4fsign;
        f2order = Cases[tmpf2,Index[Lorentz,b_],\[Infinity]];
     ];
    ];
  ];
If[count>99,Print[Style["Error : 2 fermions chain could not be sorted",Red]];];
  lowerf+f1*tmpf2*s4fsign
]

(*Tr [ Ga_basis(m) Ga_basis(k) Ga_basis(m) Ga_basis(k)] *)
NLO$Basis2BasisTr=Table[
  Normal[Series[Expand[DTr[Sequence@@NonCommutative/@(NLO$DiracBasis[[nn,1]]/.Int[xx_]->Int[xx,2]),Sequence@@NonCommutative/@NLO$DiracBasis[[mm,1]],
   Sequence@@NonCommutative/@(NLO$DiracBasis[[nn,2]]/.Int[xx_]->Int[xx,2]),Sequence@@NonCommutative/@NLO$DiracBasis[[mm,2]]]]/.M$dim->4+FR$Eps,{FR$Eps,0,1}]],
{nn,1,Length[NLO$DiracBasis]},{mm,1,Length[NLO$DiracBasis]}];

(*fix the a such that Tr [Ga_basis(m) FCh1 Ga_basis(m) FCh2]=Sum_k (f_k+a_k eps) Tr [ Ga_basis(m) Ga_basis(k) Ga_basis(m) Ga_basis(k)] 
for each m input format is the same as NLO$DiracBasis , a coefficient (NLO$EvaA[#eva=neva,#basis]) are added if neva\[NotEqual]0*)
FixAwithTr[x_,neva_]:= Block[{tmp,coef},
  coef=Table[
    Normal[Series[Expand[DTr[Sequence@@NonCommutative/@(NLO$DiracBasis[[nn,1]]/.Int[xx_]->Int[xx,2]),Sequence@@NonCommutative/@x[[1]],
       Sequence@@NonCommutative/@(NLO$DiracBasis[[nn,2]]/.Int[xx_]->Int[xx,2]),Sequence@@NonCommutative/@x[[2]]]]/.M$dim->4+FR$Eps,{FR$Eps,0,1}]],
  {nn,1,Length[NLO$DiracBasis]}];
  (*Print[InputForm[coef]];*)
  (Expand[(Normal[Series[Inverse[NLO$Basis2BasisTr] . coef,{FR$Eps,0,1}]]) . (NLO$DiracBasis*Table[NLO$EvaA2[neva,kk],{kk,Length[NLO$DiracBasis]}])]/.FR$Eps*NLO$EvaA2[xx__]->FR$Eps*NLO$EvaA[xx]/.NLO$EvaA2[__]->1/.NLO$EvaA[0,_]->1)
];


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


(* ::Subsubtitle::Closed:: *)
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
Print["before expand"];
tempo=Expand[num];
Print["after expand"];
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
IntxnLog2[0,0,p2_,0]:=(-2 (*+ If[p2>0,0,1]*Log[-p2]*) + (*If[p2>0,1,0]*)If[NLO$CMS,1,0]*Log[-p2]+If[NLO$CMS,0,1](-I*Pi*FR$Re + Log[p2]))/;FreeQ[p2,FourMomentum];
IntxnLog2[0,0,p2_,1]:=(-2 +  ((*If[p2>0,0,1]*Log[-p2] +*) (*If[p2>0,1,0]*)If[NLO$CMS,1,0]*Log[-p2]+If[NLO$CMS,0,1](-I*Pi*FR$Re + Log[p2])))/2/;FreeQ[p2,FourMomentum];
IntxnLog2[0,0,p2_,2]:=(-13 + 6*((*If[p2>0,0,1]*Log[-p2] +*) (*If[p2>0,1,0]*)If[NLO$CMS,1,0]*Log[-p2]+If[NLO$CMS,0,1](-I*Pi*FR$Re + Log[p2])))/18/;FreeQ[p2,FourMomentum];

IntxnLog2[0,m2_,0,0]:=(-1 + Log[m2^2])/;FreeQ[m2,Mass];
IntxnLog2[0,m2_,0,1]:=(-3/4 + Log[m2^2]/2)/;FreeQ[m2,Mass];
IntxnLog2[0,m2_,0,2]:=(-11/18 + Log[m2^2]/3)/;FreeQ[m2,Mass];

IntxnLog2[m1_,0,0,0]:=(-1 + Log[m1^2])/;FreeQ[m1,Mass];
IntxnLog2[m1_,0,0,1]:=(-1/4 + Log[m1^2]/2)/;FreeQ[m1,Mass];
IntxnLog2[m1_,0,0,2]:=((-1 + 3*Log[m1^2])/9)/;FreeQ[m1,Mass];

IntxnLog2[m_,m_,0,0]:=Log[m^2]/;FreeQ[m,Mass];
IntxnLog2[m_,m_,0,1]:=Log[m^2]/2/;FreeQ[m,Mass];
IntxnLog2[m_,m_,0,2]:=Log[m^2]/3/;FreeQ[m,Mass];

IntxnLog2[m_/FR$MU,0,m_^2/FR$MU^2,0]:=2*(-1 + Log[m^2/FR$MU^2]/2)/;FreeQ[m,Mass];
IntxnLog2[m_/FR$MU,0,m_^2/FR$MU^2,1]:=(-1/2 + Log[m^2/FR$MU^2]/2)/;FreeQ[m,Mass];
IntxnLog2[m_/FR$MU,0,m_^2/FR$MU^2,2]:=(2*(-1 + 3*Log[m^2/FR$MU^2]/2))/9/;FreeQ[m,Mass];

IntxnLog2[0,m_/FR$MU,m_^2/FR$MU^2,0]:=2*(-1 + Log[m^2/FR$MU^2]/2)/;FreeQ[m,Mass];
IntxnLog2[0,m_/FR$MU,m_^2/FR$MU^2,1]:=(-3/2 + Log[m^2/FR$MU^2]/2)/;FreeQ[m,Mass];
IntxnLog2[0,m_/FR$MU,m_^2/FR$MU^2,2]:=(-11 + 3*Log[m^2/FR$MU^2])/9/;FreeQ[m,Mass];

IntxnLog2[m1_,m2_,0,0]:=(-m1^2 + m2^2 + m1^2*Log[m1^2] - m2^2*Log[m2^2])/(m1^2 - m2^2)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];
IntxnLog2[m1_,m2_,0,1]:=(-m1^4 + 4*m1^2*m2^2 - 3*m2^4 + 2*(m1^4 - 2*m1^2*m2^2)*Log[m1^2] + 2*m2^4*Log[m2^2])/(4*(m1^2 - m2^2)^2)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];
IntxnLog2[m1_,m2_,0,2]:=(-2*m1^6 + 9*m1^4*m2^2 - 18*m1^2*m2^4 + 11*m2^6 + 12*(m1^6 - 3*m1^4*m2^2 + 3*m1^2*m2^4)*Log[m1^2]/2 - 6*m2^6*Log[m2^2])/(18*(m1^2 - m2^2)^3)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];

IntxnLog2[m1_,0,p2_,0]:=(-2*p2 + m1^2*Log[m1^2] + (-m1^2 + p2)*(If[NLO$CMS||m1^2>p2,0,1]*(Log[-m1^2 + p2]+I Pi*FR$Re)+If[NLO$CMS||m1^2>p2,1,0]*Log[m1^2 - p2]))/p2/;FreeQ[p2,FourMomentum];
IntxnLog2[m1_,0,p2_,1]:=1/2/p2(m1^2(Log[m1^2]-1)-IntxnLog2[m1,0,p2,0](m1^2-p2))/;FreeQ[m1,Mass]&&FreeQ[p2,FourMomentum];
IntxnLog2[m1_,0,p2_,2]:=((m1^2-p2/3)/2-2(m1^2-p2)IntxnLog2[m1,0,p2,1]-m1^2(1-Log[m1^2]))/3/p2/;FreeQ[m1,Mass]&&FreeQ[p2,FourMomentum];

IntxnLog2[0,m2_,p2_,0]:=(-2*p2 + m2^2*Log[m2^2] + (-m2^2 + p2)*(If[NLO$CMS||m2^2>p2,0,1]*(Log[-m2^2 + p2]+I Pi*FR$Re)+If[NLO$CMS||m2^2>p2,1,0]*Log[m2^2 - p2]))/p2/;FreeQ[p2,FourMomentum];
IntxnLog2[0,m2_,p2_,1]:=1/2/p2(-m2^2(Log[m2^2]-1)-IntxnLog2[0,m2,p2,0](-m2^2-p2))/;FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];
IntxnLog2[0,m2_,p2_,2]:=((m2^2-p2/3)/2-m2^2 IntxnLog2[0,m2,p2,0]-2(-m2^2-p2)IntxnLog2[0,m2,p2,1])/3/p2/;FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];

IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,0]:=-2 + Pi/Sqrt[3] + 2*Log[m/FR$MU]/;FreeQ[m,Mass];
IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,1]:=-1 + Pi/(2*Sqrt[3]) + Log[m/FR$MU]/;FreeQ[m,Mass];
IntxnLog2[m_/FR$MU,m_/FR$MU,m_^2/FR$MU^2,2]:=-1/18 + (2*Log[m/FR$MU])/3/;FreeQ[m,Mass];

IntxnLog2[m_,m_,p2_,0]:=-2 + Log[m^2] - (Sqrt[p2*(-4*m^2 + p2)]*(If[NLO$CMS||p2<4m^2,1,0]*Log[(2*m^2 - p2 + Sqrt[p2*(-4*m^2 + p2)])/(2*m^2)]+ 
  If[NLO$CMS||m>0&&p2<4m^2,0,1](Log[-(2*m^2 - p2 + Sqrt[p2*(-4*m^2 + p2)])/(2*m^2)]+I Pi*FR$Re)))/p2/;FreeQ[p2,FourMomentum];

IntxnLog2[m1_/FR$MU,m2_/FR$MU,m1_^2/FR$MU^2,0]:= If[NLO$CMS||m2>2m1,1,0](-2 + (-1 + m2^2/m1^2)*Log[m2^2/m1^2]/2 + Log[(m1^2*m2^2)/FR$MU^4]/2 - (Sqrt[-4*m1^2*m2^2 + m2^4]*
  Log[(m2^2 + Sqrt[-4*m1^2*m2^2 + m2^4])/(2*m1*m2)])/m1^2)+If[NLO$CMS||m2>2m1,0,1](-2 + (-1 + m2^2/m1^2)*Log[m2^2/m1^2]/2 + Log[(m1*m2)^2/FR$MU^4]/2 - Re[(Sqrt[-4*m1^2*m2^2 + m2^4]*
  Log[(m2^2 + Sqrt[-4*m1^2*m2^2 + m2^4])/(2*m1*m2)])]/m1^2)/;FreeQ[m1,Mass]&&FreeQ[m2,Mass];

IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,0]:=IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0];
IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,1]:=IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0]-IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,1];
IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,2]:=IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0]-2 IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,1]+IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,2];

(* CORRECTED BY BENJ *)
IntxnLog2[m1_,m2_,p2_,0]:=-2 - ((m1^2 - m2^2)*Log[m2^2/m1^2]/2)/p2 + Log[m1^2*m2^2]/2 + 
 (m1*m2*(-(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2) - (-m1^2 - m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] + p2)/(2*m1*m2))*
   (If[NLO$CMS||p2<(m1-m2)^2,1,0]Log[(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]+
    If[NLO$CMS||p2<=(m1+m2)^2,0,1](Log[-(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]+I Pi*FR$Re)))/p2+
    If[Not[NLO$CMS]&&p2>=(m1-m2)^2&&p2<(m1+m2)^2,1,0]Re[m1*m2*(-(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2) - 
    (-m1^2 - m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] + p2)/(2*m1*m2))*Log[(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]]/p2/;FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];
(* END CORRECTION BY BENJ *)

IntxnLog2[m1_,m2_,p2_,1]:=1/2/p2(-m2^2(Log[m2^2]-1)+m1^2(Log[m1^2]-1)-IntxnLog2[m1,m2,p2,0](-m2^2+m1^2-p2))/;FreeQ[m1,Mass]&&FreeQ[m2,Mass]&& FreeQ[p2,FourMomentum]; 

IntxnLog2[m1_,m2_,p2_,2]:=((m1^2+m2^2-p2/3)/2-m2^2 IntxnLog2[m1,m2,p2,0]-2(-m2^2+m1^2-p2)IntxnLog2[m1,m2,p2,1]-m1^2(1-Log[m1^2]))/3/p2/;
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
  If[m1==0,1,0]If[m2==0,0,1]If[p2==0,0,1]*(If[m2==p2^(1/2),0,1]Dp2IntxnLog2[0,m2,p2,n]+If[m2==p2^(1/2),1,0]Dp2IntxnLog2[0,m2,m2^2,n])+
  If[m1==0,0,1]If[m2==0,1,0]If[p2==0,1,0]Dp2IntxnLog2[m1,0,0,n]+
  If[m1==0,0,1]If[m2==0,1,0]If[p2==0,0,1](If[m1==p2^(1/2),0,1]Dp2IntxnLog2[m1,0,p2,n]+If[m1==p2^(1/2),1,0]Dp2IntxnLog2[m1,0,m1^2,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,1,0](If[m1==m2,1,0]Dp2IntxnLog2[m1,m1,0,n]+If[m1==m2,0,1]Dp2IntxnLog2[m1,m2,0,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,0,1]If[m1==m2,1,0](If[m1==p2^(1/2),0,1]Dp2IntxnLog2[m1,m1,p2,n]+If[m1==p2^(1/2),1,0]Dp2IntxnLog2[m1,m1,m1^2,n])+
  If[m1==0,0,1]If[m2==0,0,1]If[p2==0,0,1]If[m1==m2,0,1](If[p2^(1/2)==m1,0,1]If[p2^(1/2)==m2,0,1]Dp2IntxnLog2[m1,m2,p2,n]+If[p2^(1/2)==m1,0,1]If[p2^(1/2)==m2,1,0]Dp2IntxnLog2[m1,m2,m2^2,n]+
    If[p2^(1/2)==m1,1,0]If[p2^(1/2)==m2,0,1]Dp2IntxnLog2[m1,m2,m1^2,n])/;FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];

Dp2IntxnLog2[0,0,0,n_]:=0;

Dp2IntxnLog2[m_,0,0,n_Integer]:=-1/m^2/(n+2)/(n+1)/;FreeQ[m,Mass]&&n>=0;

Dp2IntxnLog2[0,m_,0,n_Integer]:=-1/m^2/(n+2)/;FreeQ[m,Mass]&&n>=0;

Dp2IntxnLog2[0,0,p2_,0]:=1/p2/;FreeQ[p2,FourMomentum];
Dp2IntxnLog2[0,0,p2_,1]:=1/2/p2/;FreeQ[p2,FourMomentum];
Dp2IntxnLog2[0,0,p2_,2]:=1/3/p2/;FreeQ[p2,FourMomentum];

Dp2IntxnLog2[m1_/FR$MU,0,m1_^2/FR$MU^2,0]:=-1(1/FR$Eps*FR$IR/m1^2+(-1+ FR$IRLog Log[m1^2/FR$MU^2]/2)/m1^2)FR$MU^2/;FreeQ[m1,Mass];(*Infared divergent*)
Dp2IntxnLog2[m1_/FR$MU,0,m1_^2/FR$MU^2,1]:=-1/2/m1^2*FR$MU^2/;FreeQ[m1,Mass];
Dp2IntxnLog2[m1_/FR$MU,0,m1_^2/FR$MU^2,2]:=-1/m1^2/6*FR$MU^2/;FreeQ[m1,Mass];

Dp2IntxnLog2[0,m2_/FR$MU,m2_^2/FR$MU^2,0]:=-(1/FR$Eps*FR$IR/m2^2+(-1+FR$IRLog Log[m2^2/FR$MU^2]/2)/m2^2)*FR$MU^2/;FreeQ[m2,Mass];
Dp2IntxnLog2[0,m2_/FR$MU,m2_^2/FR$MU^2,1]:=-(1/FR$Eps*FR$IR/m2^2+(-3/2+ FR$IRLog Log[m2^2/FR$MU^2]/2)/m2^2)*FR$MU^2/;FreeQ[m2,Mass];
Dp2IntxnLog2[0,m2_/FR$MU,m2_^2/FR$MU^2,2]:=-(1/FR$Eps*FR$IR/m2^2+(-11/6+FR$IRLog Log[m2^2/FR$MU^2]/2)/m2^2)*FR$MU^2/;FreeQ[m2,Mass];

Dp2IntxnLog2[m1_,0,p2_,0]:=(p2 - m1^2*Log[m1^2] + m1^2*(If[NLO$CMS||m1^2>p2,0,1]*(Log[-m1^2 + p2]+I Pi*FR$Re)+If[NLO$CMS||m1^2>p2,1,0]*Log[m1^2 - p2]))/p2^2/;FreeQ[p2,FourMomentum];
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

Dp2IntxnLog2[m1_,m2_,0,0]:=(-m1^4 + m2^4 + 2*m1^2*m2^2*Log[m1^2/m2^2])/(2*(m1^2 - m2^2)^3)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];
Dp2IntxnLog2[m1_,m2_,0,1]:=-(m1^6 - 6*m1^4*m2^2 + 3*m1^2*m2^4 + 2*m2^6 - 6*m1^2*m2^4*Log[m2^2/m1^2])/(6*(m1^2 - m2^2)^4)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];
Dp2IntxnLog2[m1_,m2_,0,2]:=(-m1^8 + 6*m1^6*m2^2 - 18*m1^4*m2^4 + 10*m1^2*m2^6 + 3*m2^8 + 12*m1^2*m2^6*Log[m1^2/m2^2])/(12*(m1^2 - m2^2)^5)/;Not[m1===m2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass];

(* CORRECTED BY BENJ: adding the p2 = 4 m^2 pieces *)
Dp2IntxnLog2[m_,m_,p2_,0]:=(Sqrt[p2*(-4*m^2 + p2)] - 2*m^2*(
     If[NLO$CMS||p2<4m^2,1,0]*Log[(2*m^2 - p2 + Sqrt[p2*(-4*m^2 + p2)])/(2*m^2)]+ 
     If[NLO$CMS||p2<=4m^2,0,1](Log[-(2*m^2 - p2 + Sqrt[p2*(-4*m^2 + p2)])/(2*m^2)]+I Pi*FR$Re))+
  If[p2==4m^2,1,0]*Sqrt[p2*(-4*m^2 + p2)] )/(p2*Sqrt[p2*(-4*m^2 + p2)])/;FreeQ[p2,FourMomentum];
(* END CORRECTION BY BENJ *)

Dp2IntxnLog2[m1_/FR$MU,m2_/FR$MU,m1_^2/FR$MU^2,0]:=  If[m2==2m1,0,1]*If[NLO$CMS||m2>2m1,1,0]*(FR$MU^2*((m1^2*Sqrt[-4*m1^2*m2^2 + m2^4] + (m1^2 - m2^2)* Sqrt[-4*m1^2*m2^2 + m2^4]*Log[m2^2/m1^2]/2 + (-3*m1^2*m2^2 + m2^4)*Log[(m2^2 + Sqrt[-4*m1^2*m2^2 + m2^4])/(2*m1*m2)])/(m1^4*Sqrt[-4*m1^2*m2^2 + m2^4])))+
  If[m2==2m1,0,1]*If[NLO$CMS||m2>2m1,0,1]*Re[(FR$MU^2*((m1^2*Sqrt[-4*m1^2*m2^2 + m2^4] + (m1^2 - m2^2)*Sqrt[-4*m1^2*m2^2 + m2^4]*Log[m2/m1] + (-3*m1^2*m2^2 + m2^4)*Log[(m2^2 + Sqrt[-4*m1^2*m2^2 + m2^4])/(2*m1*m2)])/(m1^4*Sqrt[-4*m1^2*m2^2 + m2^4])))]+
  If[m2==2m1,1,0]*(-((FR$MU^2*(-2 + Log[8]))/m1^2))/;FreeQ[m1,Mass]&&FreeQ[m2,Mass];

Dp2IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,0]:=Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0];
Dp2IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,1]:=Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0]-Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,1];
Dp2IntxnLog2[m1_/FR$MU,m2_/FR$MU,m2_^2/FR$MU^2,2]:=Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,0]-2 Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,1]+Dp2IntxnLog2[m2/FR$MU,m1/FR$MU,m2^2/FR$MU^2,2];


(* CORRECTED BY BENJ*)
Dp2IntxnLog2[m1_,m2_,p2_,0]:=((If[Not[NLO$CMS]&&p2>=(m1-m2)^2&&p2<=(m1+m2)^2,0,1]*
(p2*Sqrt[m1^4 + (-m2^2 + p2)^2 - 2*m1^2*(m2^2 + p2)] +(m1^2 - m2^2)*Sqrt[m1^4 + (-m2^2 + p2)^2 - 2*m1^2*(m2^2 + p2)]*Log[m2/m1]) +
 (m1^4 + m2^2*(m2^2 - p2) - m1^2*(2*m2^2 + p2))*
   (If[NLO$CMS||p2<(m1-m2)^2,1,0]Log[(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]+
    If[NLO$CMS||p2<=(m1+m2)^2,0,1](Log[-(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]+I Pi FR$Re)))/
 (Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2]*p2^2))+ If[Not[NLO$CMS]&&p2>(m1-m2)^2&&p2<(m1+m2)^2,1,0]*
  Re[((p2*Sqrt[m1^4 + (-m2^2 + p2)^2 - 2*m1^2*(m2^2 + p2)] + (m1^2 - m2^2)*Sqrt[m1^4 + (-m2^2 + p2)^2 - 2*m1^2*(m2^2 + p2)]*Log[m2/m1] + 
  (m1^4 + m2^2*(m2^2 - p2) - m1^2*(2*m2^2 + p2))*Log[(m1^2 + m2^2 + Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2] - p2)/(2*m1*m2)]))/
 (Sqrt[-4*m1^2*m2^2 + (m1^2 + m2^2 - p2)^2]*p2^2)]+ If[p2==(m1-m2)^2,1,0]*(2/p2+(m1^2-m2^2)/p2^2*Log[m2/m1])+
  If[p2==(m1+m2)^2,1,0]*(2/p2+(m1^2-m2^2)/p2^2*(Log[m2/m1]+I Pi FR$Re))/;Not[m1^2===p2]&&Not[m2^2===p2]&&FreeQ[m1,Mass]&&FreeQ[m2,Mass]&&FreeQ[p2,FourMomentum];
(* END CORRECTION BY BENJ *)

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
  If[Length[dZtmp]>0&&Not[Head/@(vtx[[1,All,1]]/.-x_->x)==={V,S}],
    Print[Style["Warning : The following deltaZ are going to be put to zero in the "<>ToString[vtx[[1,All,1]]]<>" vertex",Orange]];
    Print[dZtmp];  (*Abort[];*)
  vtx/.((Rule[#,0]&)/@dZtmp),
  vtx]
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
EpsSerie,majo},

su3rep = {IndexDel[Index[Colour, Ext[1]], Index[Colour, Ext[2]]]->1,IndexDel[Index[Gluon, Ext[1]], Index[Gluon, Ext[2]]]->1};

para=False;
(*Cond avoid the replacement of if by piecewice or so*)
EpsSerie[arg_] := Block[{EStmp,ESkk,ESres,ESexp},
  ESexp=Expand[arg];
  If[Head[ESexp]=!=Plus,ESexp={ESexp};];
  If[para,
     ESres=Table[EStmp=ESexp[[ESkk]];
                 ParallelSubmit[{ESkk,EStmp},Normal[Series[(((EStmp/.If->Cond)/.Cond[test_,ca_,cb_]:>Cond[Simplify[(test/.Sqrt[x_^2/FR$MU^2]->x/FR$MU),
                   Assumptions->Join[{FR$MU>0,FR$IR>0,FR$IRLog>0,FR$Eps>0},assumlist], TimeConstraint->1],ca,cb])/.
                   {Cond[True,ca_,cb_]->ca,Cond[False,ca_,cb_]->cb}),{FR$Eps,0,0}]]],{ESkk,Length[ESexp]}];
  Plus@@(WaitAll[ESres])
  ,
  ESres=(Normal[Series[(((#/.If->Cond)/.Cond[test_,ca_,cb_]:>Cond[Simplify[(test/.Sqrt[x_^2/FR$MU^2]->x/FR$MU),
                   Assumptions->Join[{FR$MU>0,FR$IR>0,FR$IRLog>0,FR$Eps>0},assumlist], TimeConstraint->1]/.Sqrt[x_^2]->x,ca,cb])/.
                   {Cond[True,ca_,cb_]->ca,Cond[False,ca_,cb_]->cb}),{FR$Eps,0,0}]]&)/@ESexp;
  If[Head[ESres]=!=Plus,ESres=Plus@@ESres;];
  ESres
  ]];

fullUV=MergeVertList[RemovedZ/@vertCT,vertUV][[All,{1,3}]];

(*remove p1 using momentum conservation*)
fullUV=fullUV/.{TensDot[a___,SlashedP[FourMomentum[Incoming,1]], b___][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]->
              -TensDot[a,SlashedP[FourMomentum[Incoming,2]], b][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]};

fullUV=fullUV/.{FourMomentum[Incoming,1]->-FourMomentum[Incoming,2]};

fullUV=Sort[fullUV,Ordered2VertQ];

res={};

If[Length[NLO$mix]>0,fullUV=DeleteCases[fullUV,{{{a_,1},{a_,2}},__}|{{{-a_,1},{a_,2}},__}];
  fullUV=DeleteCases[fullUV,_?(FreeQ[NLO$mix,Sort[#[[1,;;,1]]/.{-x_->x}]]&)]];

DPrint[fullUV[[;;,1]]];

For[kuv=1,kuv<=Length[fullUV]-0,kuv++,Print[ToString[kuv]<>" is "<>ToString[fullUV[[kuv,1]]]<>" at "<>ToString[SessionTime[]]];
DPrint[InputForm[fullUV[[kuv]]]];
   If[kuv==1||Not[Sort[(fullUV[[kuv,1]]/.-x_->x)[[All,1,1]]]===Sort[(fullUV[[kuv-1,1]]/.-x_->x)[[All,1,1]]]],eqlist={};];

   Switch[verttype,
     {F,F},
     (*fermions*)
     fl=Coefficient[fullUV[[kuv,2]],TensDot[SlashedP[FourMomentum[Incoming,2]], ProjM][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]];
     fr=Coefficient[fullUV[[kuv,2]],TensDot[SlashedP[FourMomentum[Incoming,2]], ProjP][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]];
     fsl=Coefficient[fullUV[[kuv,2]],ProjM[Index[Spin, Ext[1]], Index[Spin, Ext[2]]]];
     fsr=Coefficient[fullUV[[kuv,2]],ProjP[Index[Spin, Ext[1]], Index[Spin, Ext[2]]]];

     (*check a piori on the form of the vertex*)
     On[Simplify::time];
     If[Not[Simplify[fullUV[[kuv,2]]-fl*TensDot[SlashedP[FourMomentum[Incoming,2]], ProjM][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]-
                    fr*TensDot[SlashedP[FourMomentum[Incoming,2]], ProjP][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]-
                    fsl ProjM[Index[Spin, Ext[1]], Index[Spin, Ext[2]]]- fsr ProjP[Index[Spin, Ext[1]], Index[Spin, Ext[2]]],TimeConstraint->0.2]===0],
       Print["warning: FF vertex does not seem match expected form, further simplification may be needed"]];
     Off[Simplify::time];
Print[SessionTime[]];

     interres = If[Simplify[TheMass[fullUV[[kuv,1,1,1]]],Assumptions->assumlist]===0&&Simplify[TheMass[fullUV[[kuv,1,2,1]]],Assumptions->assumlist]===0,
                  fl,fl*TheMass[fullUV[[kuv,1,2,1]]]+fsr]/.su3rep;
     ColCons2pt[interres];
     interres = EpsSerie[interres/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,2,1]]]^2];
     eqlist=Append[eqlist,interres];
     interres = If[Simplify[TheMass[fullUV[[kuv,1,1,1]]],Assumptions->assumlist]===0&&Simplify[TheMass[fullUV[[kuv,1,2,1]]],Assumptions->assumlist]===0,
                  fr,fr*TheMass[fullUV[[kuv,1,2,1]]]+fsl]/.su3rep;
     ColCons2pt[interres];
     interres = EpsSerie[interres/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,2,1]]]^2];
     eqlist=Append[eqlist,interres];
     (*If[Not[FreeQ[fullUV[[kuv,1,1]],fullUV[[kuv,1,2,1]]]], be careful for non diagonal piece*)
     deq=Simplify[D[(TheMass[fullUV[[kuv,1,1,1]]](fr+fl)+fsl+fsr),SP[FourMomentum[Incoming,2], FourMomentum[Incoming,2]]],TimeConstraint->0.01];
     deq=(deq/.Derivative[0, 0, 1, 0][IntxnLog][aa_, bb_, cc_, dd_]:>Dp2IntxnLog[aa,bb,cc,dd]);
     deq=2*TheMass[fullUV[[kuv,1,1,1]]]*deq+(fr+fl)/.su3rep;
     ColCons2pt[deq];
     deq=EpsSerie[deq/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,1,1]]]^2];
     If[DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,_],\[Infinity]]===DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,_],\[Infinity]]||
       DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,_],\[Infinity]]===-DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,_],\[Infinity]](*Same particle or particle and antiparticle*),
       eqlist=Append[eqlist,deq];
       If[SelfConjugate[fullUV[[kuv,1,1,1]]],
         majo=Union[Cases[deq,FR$deltaZ[{x_,x_},y___],\[Infinity]]];
(*DPrint[InputForm[majo]];*)
         If[Length[majo]>2||(Length[majo]==2 && majo[[1,1]]!=majo[[2,1]]),Print[Style["Error : majorana CT does not have the expected form",Red]];];
         If[Length[majo]==2,eqlist=Append[eqlist,Subtract@@majo]];
       ];
     ];
DPrint[InputForm[Union[Cases[eqlist,Cond[__],\[Infinity]]]]];
     ,
	 {S,S},
     If[DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,_],\[Infinity]]===DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,_],\[Infinity]]||
          DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,_],\[Infinity]]===-DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,_],\[Infinity]](*Same particle or particle and antiparticle*),
       If[FreeQ[FR$GoldstoneList,fullUV[[kuv,1,1,1]]]&&FreeQ[FR$GoldstoneList,-fullUV[[kuv,1,1,1]]](*Not a goldstone boson*),
         interres=fullUV[[kuv,2]]/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,1,1]]]^2;
         eqlist=Append[eqlist,EpsSerie[interres]];
       ];
       deq=D[(fullUV[[kuv,2]]),SP[FourMomentum[Incoming,2], FourMomentum[Incoming,2]]];
       deq=EpsSerie[(deq/.Derivative[0, 0, 1, 0][IntxnLog][aa_, bb_, cc_, dd_]:>Dp2IntxnLog[aa,bb,cc,dd])/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,1,1]]]^2];
       eqlist=Append[eqlist,deq];
, 
       (*different scalars*)
       interres = EpsSerie[(fullUV[[kuv,2]]/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,1,1]]]^2)];
       eqlist=Append[eqlist,interres];
       interres = EpsSerie[(fullUV[[kuv,2]]/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,2,1]]]^2)];
       eqlist=Append[eqlist,interres];
     ];,
     {V,V},
     If[FreeQ[DeleteCases[fullUV[[kuv,1,1,1]],Index[Gluon|Colour,xx_],\[Infinity]],DeleteCases[fullUV[[kuv,1,2,1]],Index[Gluon|Colour,xx_],\[Infinity]]],
       (*different particles*)
Print["not same 0"];
       deq = Coefficient[fullUV[[kuv,2]],ME[Index[Lorentz,Ext[1]],Index[Lorentz,Ext[2]]]];
Print["not same 1"];
       eqlist=Append[eqlist,(EpsSerie[deq/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,1,1]]]^2])];
       eqlist=Append[eqlist,(EpsSerie[deq/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,2,1]]]^2])];
Print["not same 2"];
       ,
       (*Same particle*)
       deq = Coefficient[fullUV[[kuv,2]],ME[Index[Lorentz,Ext[1]],Index[Lorentz,Ext[2]]]];
       (*If photon, save self-energy from the light (m<MZ) SM fermion at the Z mass*)
       If[(fullUV[[kuv,1,1,1]]/.(Reverse/@FR$ClassesTranslation))===A,
         DPrint["Save Photon Self-Energy"];
         FR$photonSE=-deq/.{IPL[a_]:>0/;(FreeQ[{u,d,s,c,b,e,mu,ta}/.FR$ClassesTranslation,a[[1]]/.{}->Sequence[]]),(*
                            IPL[a_]\[RuleDelayed]If[Simplify[TheMass[a[[1]]]\[Equal]0,Assumptions->Join[{FR$MU>0,FR$IR>0,FR$Eps>0},assumlist]],1,0]IPL[a],*)FR$deltaZ[__]->0};
         FR$photonSE = (FR$photonSE/MZ^2/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->MZ^2/.If->Cond)/.Cond[test_,ca_,cb_]:>Cond[Simplify[(test/.Sqrt[x_^2/FR$MU^2]->x/FR$MU),
                   Assumptions->Join[{FR$MU>0,FR$IR>0,FR$IRLog>0,FR$Eps>0},assumlist], TimeConstraint->1],ca,cb]/.Cond->If/.FR$Re->0;
         FR$photonSE = Expand[FR$photonSE]/.If[a_,1,0]*If[a_,0,1]->0;
         (*DPrint[InputForm[FR$photonSE]];*)
       ];
       If[Not[TheMass[fullUV[[kuv,1,1,1]]]===0],
         (*Massive*)
         eqlist=Append[eqlist,(EpsSerie[deq/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,1,1]]]^2])];
       ];
       deq=D[deq,SP[FourMomentum[Incoming,2], FourMomentum[Incoming,2]]];
       deq=(deq/.Derivative[0, 0, 1, 0][IntxnLog][aa_, bb_, cc_, dd_]:>Dp2IntxnLog[aa,bb,cc,dd]);
       deq=deq/.SP[FourMomentum[Incoming,2],FourMomentum[Incoming,2]]->TheMass[fullUV[[kuv,1,1,1]]]^2;
       deq=EpsSerie[deq];
       eqlist=Append[eqlist,deq];
    ];,
    {V,S},
DPrint[InputForm[fullUV[[kuv]]]];

  ];(*end switch*)
DPrint["ES"];

  varlist=Union[If[TheMass[fullUV[[kuv,1,1,1]]]===TheMass[fullUV[[kuv,1,2,1]]],Cases[eqlist,FR$delta[{TheMass[fullUV[[kuv,1,1,1]]]},{}],\[Infinity]],{}],Cases[eqlist,_FR$deltaZ,\[Infinity]]];
  realvar=If[complex,{},Cases[varlist,FR$deltaZ[{x_,x_},b__]]];
(*make sure nothing is done for the VS case*)
  extra=If[verttype==={V,S},{},Complement[Union[Cases[fullUV[[kuv,2]],_FR$deltaZ,\[Infinity]]],varlist]];

  eqlist=(Refine[#/.((Rule[Conjugate[#],#]&)/@realvar)(*/.FR$IR->1*),Assumptions->Append[assumlist,FR$MU>0],TimeConstraint->0.0002]&)/@eqlist;
  If[Length[varlist]+Length[extra]<1&&Not[And@@((#==0&)/@eqlist)===True],
    Print[Style["Warning : no counterterms for this vertex "<>ToString[fullUV[[kuv,1]]] <> ", the following terms should vanish",Red]];
    Print[InputForm[eqlist]];
  , 
    (*Ignore conjugate vertex for boson since they give the same renormalisation condition*)
    If[kuv<Length[fullUV]&&(Sort[(fullUV[[kuv,1]]/.-x_->x)[[All,1,1]]]===Sort[(fullUV[[kuv+1,1]]/.-x_->x)[[All,1,1]]]&&Not[verttype==={F,F}]),kuv++;];
    If[kuv==Length[fullUV]||Not[Sort[(fullUV[[kuv,1]]/.-x_->x)[[All,1,1]]]===Sort[(fullUV[[kuv+1,1]]/.-x_->x)[[All,1,1]]]],
      res=Join[res,ConsSolve[eqlist/.{If->Cond(*,FR$IR->1*)},varlist],If[Length[extra]>0,(#->0&)/@extra,{}]];  (*set to zero no usefull delta(Z)*)
    ];
  ];
(*DPrint["before join"];DPrint[InputForm[If[Length[extra]>0,(#->0&)/@extra,{}]]];*)
];(*end for*)
DPrint["end for in SolveDelta"];

res=Flatten[res/.Rule->tRule/.{tRule[FR$deltaZ[{a_,b_},xx__],-Conjugate[FR$deltaZ[{b_,a_},xx__]]]->{tRule[FR$deltaZ[{a,b},xx],0],tRule[FR$deltaZ[{b,a},xx],0]}}/.
   {tRule[FR$deltaZ[{a_,b_},xx__],-CMSConj[FR$deltaZ[{b_,a_},xx__]]]->{tRule[FR$deltaZ[{a,b},xx],0],tRule[FR$deltaZ[{b,a},xx],0]}}]/.
  tRule->Rule;
(Refine[#,Assumptions->Append[assumlist,FR$MU>0],TimeConstraint->0.001]&)/@(res/.{Cond->If,If[complex,FR$Re->1,FR$Re->0]})

];


(* ::Subtitle::Closed:: *)
(*R2*)


(*Performs q integration*)


(* ::Subsubtitle:: *)
(*Tadpoles*)


R2Tadpoles::usage="compute the R2 for a Tadpole amplitude after the non contributing terms have been removed"


R2Tadpoles[num_,del_,next_,UVcounter_] := Block[{tmp, coef},
   coef = Times @@ Cases[num, _?((FreeQ[#, MatrixTrace] && FreeQ[#, MetricTensor] && FreeQ[#, FourVector]) &)];
   tmp = num/coef;
   tmp = Expand[tmp /. {MatrixTrace -> DTr, MetricTensor -> ME, FourVector -> FV}];
   Simplify[-2 I \[Pi]^2*coef*del*Coefficient[Normal[Series[tmp /. {M$dim -> 4 + M$eps}, {M$eps, 0, 1}]], M$eps]]*FR$R2-2 I \[Pi]^2*coef*del*Simplify[(FR$UV(1/FR$Eps*
   Normal[Series[(tmp(1-FR$Eps/2)/.M$dim->4+FR$Eps),{FR$Eps,0,If[next<=2&&UVcounter,1,0]}]]+ If[next<=2&&UVcounter,1,0]*UVLog[del/FR$MU^2]/2*tmp/.{M$dim->4}))] /. {DTr -> MatrixTrace, ME -> MetricTensor, 
   FV -> FourVector}
];


(* ::Subsubtitle:: *)
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
               {DTr->MatrixTrace,ME->MetricTensor,FV->FourVector,FCh->FermionChain});
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
               {SP[lm,lm]->1}/.{DTr->MatrixTrace,ME->MetricTensor,FV->FourVector,FCh->FermionChain};
  coef*tmp
];


(* ::Subsubtitle:: *)
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
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,DTr->MatrixTrace,FV->FourVector}
];

R2FBTriangles[num_,lm_]:=Block[{tmp,coef},
  tmp=Times@@Cases[num,_?(Not[FreeQ[#,lm]]&)];
  coef=num/tmp;
  tmp=Times@@Cases[coef,_?(Not[FreeQ[#,MetricTensor]]&)]*tmp;
  coef=num/tmp;
  tmp=Expand[tmp/.{FermionChain->FCh,MetricTensor->ME,FourVector->FV}];
  tmp=Expand[Expand[tmp]/.red2v[lm]];
  I \[Pi]^2*coef (Simplify[-2Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]-1/2(tmp/.M$dim->4)]FR$R2- 
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,DTr->MatrixTrace,FV->FourVector,FCh->FermionChain}
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
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,DTr->MatrixTrace,FV->FourVector}
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
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,FV->FourVector}
];

R2BBoxes[num_,lm_]:=Block[{coef, tmp},
  tmp=Times@@Cases[num,_?(Not[FreeQ[#,lm]]&)];
  coef=num/tmp;
  tmp=Times@@Cases[coef,_?(Not[FreeQ[#,MetricTensor]]&)]*tmp;
  coef=num/tmp;
  tmp=Expand[tmp]/.{MetricTensor->ME}/.{FourVector->FV}/.red4v[lm]/.red2v[lm];
  tmp=Expand[tmp];
  I \[Pi]^2*coef (Simplify[-2Coefficient[Normal[Series[tmp/.{M$dim->4+M$eps},{M$eps,0,1}]],M$eps]-5/6(tmp/.M$dim->4)]FR$R2- 
               2 FR$UV*Simplify[(tmp/.{M$dim->4})]/FR$Eps)/.SP[lm,lm]->1/.{ME->MetricTensor,FV->FourVector}
];


(* ::Subtitle:: *)
(*GetR2*)


GetR2::usage="compute the R2 for a FeynArts amplitude at the generic level"


(* ::Subsubtitle:: *)
(*4d*)


GetR2[amp_,next_,UVfin_]:=Block[{temp,loopmom,nden,den,num,kk,extmom,intmom,lmcoef,DisFC,DisMT,DisDS,DisNC,nlmom,x,y,rl,lr,delta},
Print["In reno and fast"];
  loopmom=amp[[2,1]];
  den=Cases[amp[[3]],_PropagatorDenominator,\[Infinity]];
  den=Cases[den,_?(Not[FreeQ[#,loopmom]]&),1];
  nden=Length[den];
  num=amp[[3]]/.{FeynAmpDenominator[__]->1}/.LeviCivita->LCivita;
  NLO$DebugCount=0;

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
(*DPrint["in 2 F"];DPrint[InputForm[num]];DPrint[extmom];*)
        num={num/.{NonCommutative[DiracSlash[xx_] + Mass[yy__,Loop]]:>NonCommutative[DiracSlash[loopmom]]},(*(x (x-1)num/.
             {NonCommutative[DiracSlash[xx_] + Mass[yy_,Loop]]:>NonCommutative[DiracSlash[extmom] ]})+num/.
             {NonCommutative[DiracSlash[xx_] + Mass[yy_,Loop]]:> Mass[yy,Loop]}*)num/.{loopmom->-x*extmom}};
(*DPrint[num];*)
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
  num/.{FCh->FermionChain,FV->FourVector,ME->MetricTensor,SP->ScalarProduct,LCivita->LeviCivita}

];(*end of getR2*)


(* ::Subsubtitle:: *)
(*more than 4d*)


(*generic version for EFT*)
GetR2[amp_,next_,UVfin_,evenOnly_,maxDim_,gaugef_]:=Block[{temp,loopmom,nden,den,num,kk,extmom,extmom2,intmom,lmcoef,DisFC,DisMT,DisDS,DisNC,nlmom,x,y,rl,lr,delta,noextpos,feynp,PW,pairtmp,
                    perm,mep,mep2,fvp,nq,LoopInt,Epsi,feynpar,RemoveHDim,FChain,num2,tempUV,FullLoopInt,findfp,fplist,fp1,numtmp,kkpar,tmppar,exprm,MF,momshift,tmp,tmppara,tmppara2,tmprep,
                    find4Findex,vectindlist,svectindlist},

(*pole of the integral of (k^2)^nm/(k^2-dlta)^nm/Gamma[2-d/2]*)
  LoopInt[nm_,nd_,dlta_]:=(-1)^(nm+nd)*(\[Pi])^2 I Product[M$dim+2kk,{kk,0,nm-1}]/2^(nm)dlta^(2-nd+nm)/Gamma[nd]*
                          If[nd-nm-2>0,0,1/Product[nd-(*M$dim/2*)2-nm+ll,{ll,0,1-nd+nm}]];
(*the integral of (k^2)^nm/(k^2-dlta)^nm/Gamma[2-d/2]*)
  FullLoopInt[nm_,nd_,dlta_]:=(-1)^(nm+nd)*(\[Pi])^2 I Product[M$dim+2kk,{kk,0,nm-1}]/2^(nm)dlta^(2-nd+nm)/Gamma[nd]*
                          If[nd-nm-2>0,Gamma[nd-2-nm],1/(2-M$dim/2)(1-(2-M$dim/2)Log[dlta/FR$MU^2])/Product[nd-M$dim/2-nm+ll,{ll,0,1-nd+nm}]];

  RemoveHDim[am_]:= If[If[evenOnly,
    Total[Ceiling[(Cases[am,G[_][_][x__][y__]:>Count[{x},F[__],\[Infinity]]*3/2+Count[{x},T[__],\[Infinity]]+Count[{x},V[__],\[Infinity]]+Count[{x},S[__],\[Infinity]]+Count[{y},Mom[_],\[Infinity],Heads->True]]-4)/2]*2],
    Total[(Cases[am,G[_][_][x__][y__]:>Count[{x},F[__],\[Infinity]]*3/2+Count[{x},T[__],\[Infinity]]+Count[{x},V[__],\[Infinity]]+Count[{x},S[__],\[Infinity]]+Count[{y},Mom[_],\[Infinity],Heads->True]]-4)]]>maxDim-4,0,am];

  loopmom=amp[[2,1]];
  den=Cases[amp[[3]],_PropagatorDenominator,\[Infinity]];
  nden=Length[den];
  NLO$DebugCount=0;
Print["In EFT"];
Print["starting at "<>ToString[SessionTime[]]];

  (*assume at most two summed lorentz index in the lorentz structure*)
  num=Expand[amp[[3]]/.LeviCivita->LCivita/.{IndexSum[b___*DiracObject[c___,DiracMatrix[a_],d___][Index[Dirac,i_],Index[Dirac,j_]],{a_,1,4}]:>
    Module[{lo},((b*DiracObject[c,DiracMatrix[a],d][Index[Dirac,i],Index[Dirac,j]])/.a->Index[Lorentz,lo])],
    IndexSum[b___*DiracObject[c___,DiracMatrix[a_],DiracMatrix[e_],d___][Index[Dirac,i_],Index[Dirac,j_]],{a_,1,4},{e_,1,4}]:>
    Module[{lo,lo2},((b*DiracObject[c,DiracMatrix[a],DiracMatrix[e],d][Index[Dirac,i],Index[Dirac,j]])/.{a->Index[Lorentz,lo],e->Index[Lorentz,lo2]})],
    IndexSum[b___*DiracObject[c___,DiracMatrix[e_],DiracMatrix[a_],d___][Index[Dirac,i_],Index[Dirac,j_]],{a_,1,4},{e_,1,4}]:>
    Module[{lo,lo2},((b*DiracObject[c,DiracMatrix[e],DiracMatrix[a],d][Index[Dirac,i],Index[Dirac,j]])/.{a->Index[Lorentz,lo],e->Index[Lorentz,lo2]})]}];

Print["after total expand 0 "<>ToString[SessionTime[]]];Print[Length[num]];
  num=Expand[num/.{FeynAmpDenominator[__]->1}/.DiracObject[a__][Index[Dirac, i_], Index[Dirac, j_]]->DiracOpenChain[a][Index[Dirac, i], Index[Dirac, j]]];
DPrint["before total expand 1 "<>ToString[SessionTime[]]];
DPrint["Not free form Dirac"];
DPrint[InputForm[Cases[num,_?(Not[FreeQ[#,Dirac]]&)]]];
  num=Expand[num];
Print["after total expand 1 "<>ToString[SessionTime[]]];DPrint[Length[num]];
(*compute momentum shift and delta*)
  Switch[nden,
    1,
    delta = den[[1,2]]^2;
    extmom = 0;
    ,
    _,
    feynpar=Table[ToExpression["x"<>ToString[fcount]],{fcount,1,nden-1}]; (*list of feynman parameters*)
    extmom2=(Coefficient[#,loopmom]&/@Cases[den,\!\(TraditionalForm\`PropagatorDenominator\)[a_,b_]->a])*Cases[den,\!\(TraditionalForm\`PropagatorDenominator\)[a_,b_]->a]-loopmom;(*vector of external momenta*)
    noextpos=Position[extmom2,0][[1,1]];
    feynp=Insert[feynpar,(1-Total[feynpar]),noextpos];
    extmom=extmom2 . feynp;(*momentum shift*)
    delta = -Simplify[Cases[den,\!\(TraditionalForm\`PropagatorDenominator\)[a_,b_]->SP[a,a]-b^2] . feynp-SP[loopmom+extmom,loopmom+extmom]];
    (*num=num/.loopmom ->loopmom-extmom;*)
(*DPrint[InputForm[loopmom-extmom]];DPrint[InputForm[delta]];*)
  ];(*end of Switch*)
  Print[Style["after switch delta extmom "<>ToString[SessionTime[]],Red]];
  Print[Length[num]];Print[ByteCount[num]];
  
  (*num2=num;
  FR$DoPara=True;
  If[FR$DoPara && Head[num2]===Plus && ByteCount[num2]>200000, 
     Print["in para"];
     DistributeDefinitions[DTrDist,FChDist];
     tmp=Table[tmppara=num2[[itp;;Min[itp+99,Length[num2]]]];
       ParallelSubmit[{itp,tmppara},
         If[Head[tmppara]===Plus,
           tmppara2=(Expand[#/.{MatrixTrace->DTrDist,FermionChain->FChDist}]&)/@tmppara;           
           If[Head[tmppara2]===Plus,tmppara2=RemoveHDim/@tmppara2;,tmppara2=RemoveHDim[tmppara2];];,
           tmppara2=(Expand[#/.{MatrixTrace->DTrDist,FermionChain->FChDist}]&)[tmppara];
           If[Head[tmppara2]===Plus,tmppara2=RemoveHDim/@tmppara;,tmppara2=RemoveHDim[tmppara2];];];
           tmppara2],{itp,1,Length[num2],100}];
     num2=Plus@@(WaitAll[tmp]);
     ,
     num2 = If[Head[num2]===Plus,(Expand[#/.{MatrixTrace->DTrDist,FermionChain->FChDist}]&)/@num2,Expand[num2/.{MatrixTrace->DTrDist,FermionChain->FChDist}]];
     If[Head[num2]===Plus,num2=RemoveHDim/@num2;,num2=RemoveHDim[num2];];
  ];
  Print["after expand para"<>ToString[SessionTime[]]];DPrint[Length[num]];*)
  (*By default the start of the trace is Lorentz 1, the index of the first vector*)
  num=num/.MatrixTrace->DTrDist0;
  num=num/.DTrDist0->MatrixTrace;
  If[Not[FreeQ[num,DTrDist]],Print["no free from DtrDist2"];Print[num];];
  
  Print["before if rotate"];
  If[next>=3&&Not[FreeQ[num,PolarizationVector]]&&Not[FreeQ[num,MatrixTrace]],
    Print["in more than 3 and vector and trace"];
    vectindlist = Union[Cases[num,_PolarizationVector,\[Infinity]]];
    tmprep={};
    If[Not[FreeQ[num,Index[Lorentz,1]]]&&FreeQ[num,FermionChain]&&FreeQ[num,S[Index[Generic,_]]],
      Print["external vector only"];
      tmprep=((#->RotateTr[#,1]&)/@Cases[num,_MatrixTrace,\[Infinity]]);
    ];
    If[Not[FreeQ[num,G[a_][b_][_F,_F,_F,_F][c__]]]&&next>=4&&FreeQ[num,S[Index[Generic,_]]],
      Print["in trace with 4F no scalar"];
      find4Findex[x_]:=(Append[Cases[x,a___*NonCommutative[DiracMatrix[Index[Lorentz,d_]]]|NonCommutative[DiracMatrix[Index[Lorentz,d_]]]:>d/;StringMatchQ[ToString[d],"lo$"~~___]],Null][[1]]);
      tmprep=((#->RotateTr[#,find4Findex[#]]&)/@Cases[num,_MatrixTrace,\[Infinity]]);
    ];
    If[Not[FreeQ[num,S[Index[Generic,_]]]],
    Print["scalar conditions"];Print[next];Print[vectindlist];
    Print[FreeQ[num,G[a_][b_][_F,_F,_F,_F][c__]]];];
    If[next>=3&&next-1===Length[vectindlist]&&Not[FreeQ[num,S[Index[Generic,_]]]]&&FreeQ[num,G[a_][b_][_F,_F,_F,_F][c__]],
        Print["1 scalar and no 4F and only vector"];
        svectindlist = Union[DeleteCases[Cases[num,G[a_][b_][ccc__][d__]->{ccc},\[Infinity]],_?(FreeQ[#,S]&)]][[1]];
        svectindlist = Intersection[svectindlist,vectindlist[[1;;,1]]];
        If[Length[svectindlist]>0,svectindlist = svectindlist[[1,1,2]];,svectindlist = Null;];
        tmprep=((#->RotateTr[#,svectindlist]&)/@Cases[num,_MatrixTrace,\[Infinity]]);
        If[nden>=next,Print[InputForm[svectindlist]]];
    ];
    num=num/.tmprep;
    (*Replace by the normal Internal/Incoming when not in the trace*)
    num=num/.{PropIncoming->Incoming,PropInternal->Internal};
    ,
    Print["no fermion"];Print[InputForm[FreeQ[num,F]]];
    num=dumnotr*num/.{PropIncoming->Incoming,PropInternal->Internal};
  ];
  
  If[Not[FreeQ[num,PropInternal]],Print[Style["PropInternal2",Pink]];Print[InputForm[num]];];
  If[Not[FreeQ[num,PropIncoming]],Print[Style["PropIncoming2",Pink]];Print[InputForm[num]];];
  num = If[Head[num]===Plus,(Expand[#/.{MatrixTrace->DTrDist,FermionChain->FChDist}]&)/@num,Expand[num/.{MatrixTrace->DTrDist,FermionChain->FChDist}]];
  Print["after dist"];
  DPrint["before rm H dim "<>ToString[SessionTime[]]];DPrint[Length[num]];
  DPrint["function of couplings :"];
  DPrint[InputForm[Cases[num,Except[Times,f_][___,G[_][_][b__][c__],___],\[Infinity]]]];
  If[Head[num]===Plus,num=RemoveHDim/@num;,num=RemoveHDim[num];];
  (*Print["check para"];
  Print[InputForm[num2-num]];
  DPrint[Length[num]];
  Print["after rm H dim "<>ToString[SessionTime[]]];DPrint[Length[num]];*)
  (*num=num/.loopmom ->loopmom-extmom;*)
Print["after switch (momentum shift) "<>ToString[SessionTime[]]];DPrint[Length[num]];
   (*remove for para*)
  (*Attributes[pairtmp]=Orderless;*)
DPrint["after mom shift"];DPrint[Head[num]];DPrint[Length[num]];
(*to do condition on maxdim and nden for no mom shift or return 0 directly if[nden-maxdim\[Equal]0,no shift,If[nedn-maxdim>0,0]], feynmangauge only?*)
  If[gaugef&&nden-maxDim==0,DPrint["no ext shift"];extmom=0;];
  If[gaugef&&nden-maxDim>0,DPrint["zero"];Return[0];];
  Print["before exprm"];
  exprm[x_]:=Block[{texprm},
    (*fake shift for the loop momentum counting to reduce the number of terms*)
    texprm=Expand[x/.loopmom ->loopmom-If[extmom===0,0,FourMomentum[momshift]](*extmom*)/.{MetricTensor->ME,FourVector->FV,
      ScalarProduct->SP,FChDist->FChDist2,DTrDist->DTrDist2}];
   
    texprm = texprm//.Power[aa_?(Not[FreeQ[#,loopmom]]&),n_Integer]:>Product[PW[aa,kk],{kk,1,n}];
    (*texprm=If[Head[texprm]===Plus,DeleteCases[texprm,_?(OddQ[Count[#,loopmom,\[Infinity]]]&)],If[OddQ[Count[texprm,loopmom,\[Infinity]]],0,texprm]];*)
    
    If[next>2,
      texprm=If[Head[texprm]===Plus,DeleteCases[texprm,_?(Count[#,loopmom,\[Infinity]]<2nden-4&)],If[Count[texprm,loopmom,\[Infinity]]<2nden-4,0,texprm]];
    ];

    texprm = texprm/.PW[a_,b_]->a/.FourMomentum[momshift]->extmom;
    (*DPrint[InputForm[texprm]];*)
    
    texprm=Expand[texprm/.{FChDist2->FCh,DTrDist2->DTr}];

    texprm = (texprm/.{FCh[aa__,NonCommutative[DiracSlash[loopmom]],bb__]:>Module[{sumind},
             FCh[aa,NonCommutative[DiracMatrix[Index[Lorentz,Int[sumind]]]],bb]*pairtmp[loopmom,Index[Lorentz, Int[sumind]]]],
             DTr[aa___,NonCommutative[DiracSlash[loopmom]],bb___]:>Module[{sumind},
             DTr[aa,NonCommutative[DiracMatrix[Index[Lorentz,Int[sumind]]]],bb]*pairtmp[loopmom,Index[Lorentz, Int[sumind]]]]});
             
    texprm=(texprm/.{FV[loopmom,aa_]->pairtmp[loopmom,aa], SP[loopmom,aa_?(FreeQ[#,Internal]&)]->pairtmp[loopmom,aa]});
    If[Length[DeleteCases[Union[Cases[texprm,_pairtmp,\[Infinity]]],pairtmp[FourMomentum[Internal, 1],_?(FreeQ[#,Internal]&)]]]>0,
    Print["list of pairtmp"];
    Print[InputForm[DeleteCases[Union[Cases[texprm,_pairtmp,\[Infinity]]],pairtmp[FourMomentum[Internal, 1],_?(FreeQ[#,Internal]&)]]]];
    ];
    
    texprm = texprm/.{Power[pairtmp[loopmom,aa_?(FreeQ[#,loopmom]&)],n_Integer]:>Product[pairtmp[loopmom,PW[kk]*aa],{kk,1,n}],
             Power[SP[loopmom,loopmom],n_Integer]:>Product[SP[loopmom,PW[loopmom,kk]],{kk,1,n}]};

    texprm=If[Head[texprm]===Plus,DeleteCases[texprm,_?(OddQ[Count[#,loopmom,\[Infinity]]]&)],If[OddQ[Count[texprm,loopmom,\[Infinity]]],0,texprm]];
    
    If[next>2,texprm=If[Head[texprm]===Plus,DeleteCases[texprm,_?(Count[#,loopmom,\[Infinity]]<2nden-4&)],If[Count[texprm,loopmom,\[Infinity]]<2nden-4,0,texprm]]];

    texprm/.PW[loopmom,_]->loopmom
  ];
  Print["after exprm"];
  num=If[Head[num]===Plus,
    Print["sum"];
    If[True(*NLOCT$notParal*),exprm/@num,
       DistributeDefinitions[exprm];
       numtmp = Table[tmppar=num[[kkpar]];ParallelSubmit[{kkpar,tmppar},DPrint[{kkpar,SessionTime[]}];exprm[tmppar]],{kkpar,Length[num]}];
       Plus@@WaitAll[numtmp]
    ],
    Print["not sum"];exprm[num]];
Print["before tens red "<>ToString[SessionTime[]]];DPrint[Length[num]];



(*replace k^mu1 ... k^mu2n by (k^2)^n*metric combination*)
  For[nq=If[Head[num]===Plus,Max[(Count[#,loopmom,\[Infinity]]&)/@List@@num],Count[num,loopmom,\[Infinity]]],nq>0,nq=nq-2,
    (*Print["nq is " <>ToString[nq]<> " at "<>ToString[SessionTime[]]];*)
    perm=Permutations[Table[ToExpression["ind"<>ToString[i]],{i,nq}]];
    DPrint["step1 "<>ToString[SessionTime[]]];
    mep=Simplify[Plus@@Table[Times@@Table[ME[Index[Lorentz,ToExpression["ind"<>ToString[2*i-1]]],Index[Lorentz,ToExpression["ind"<>ToString[2*i]]]],
       {i,nq/2}]/.Table[perm[[1,kk]]->perm[[nn,kk]],{kk,nq}],{nn,Length[perm]}]/Factorial[nq/2]/2^(nq/2)];
    mep=Simplify[mep/.ME[Index[Lorentz,aaa_],Index[Lorentz,bbb_]]:>
       ME[Index[Lorentz,bbb],Index[Lorentz,aaa]]/;Not[Sort[{ToString[aaa],ToString[bbb]}]==={ToString[aaa],ToString[bbb]}]];
    
    DPrint["step2 "<>ToString[SessionTime[]]];
    mep2=Simplify[mep/Expand[mep*If[Head[mep]===Plus,mep[[1]],mep]]]*SP[loopmom,loopmom]^(nq/2);
    DPrint["step3 "<>ToString[SessionTime[]]];
    fvp=Times@@(pairtmp[loopmom,Pattern[#,_]]&/@perm[[1]]);
    DPrint["step4 "<>ToString[SessionTime[]]];
    num = num/.LCivita[a___,FV[loopmom],b___]:>Module[{lcind},LCivita[a,Index[Lorentz,lcind],b]pairtmp[loopmom,Index[Lorentz,lcind]]];
    DPrint[InputForm[Coefficient[num,NLO$dum,2]]];
    DPrint[InputForm[{fvp->(mep2/.{Index[Lorentz,aa_]->aa})}]];
    DPrint[InputForm[Coefficient[num,NLO$dum,2]]/.{fvp->(mep2/.{Index[Lorentz,aa_]->aa})}];
    DPrint["step5 "<>ToString[SessionTime[]]];
    (*If[False&&Length[num]>1000 && Length[Kernels]>1,
     
     num=Table[numtmp=num[[ESkk;;;;Length[Kernels[]]]];
                 ParallelSubmit[{ESkk,numtmp,rep},Print[ESkk];numtmp/.rep],{ESkk,1,Length[Kernels[]]}];
     num=Plus@@(WaitAll[num]);,
    Print[InputForm[num[[1;;10]]]];Print[InputForm[{fvp->(mep2/.{Index[Lorentz,aa_]->aa})}]];*)
    
    num=num/.{fvp->(mep2/.{Index[Lorentz,aa_]->aa})};
    (*];*)
  ];

Print["after tens red "<>ToString[SessionTime[]]];DPrint[Length[num]];

  num=Expand[num]/.{PW[_]->1};
  num=num/.{ME[a_?(Not[FreeQ[#,FourMomentum]]&),Index[Lorentz,b_]]->FV[a,Index[Lorentz,b]],
            ME[a_?(Not[FreeQ[#,FourMomentum]]&),b_?(Not[FreeQ[#,FourMomentum]]&)]->SP[a,b],
            ME[Index[Lorentz,b_],a_?(Not[FreeQ[#,FourMomentum]]&)]->FV[a,Index[Lorentz,b]],
            ME[a_?(Not[FreeQ[#,FourMomentum]]&),a_?(Not[FreeQ[#,FourMomentum]]&)]->SP[a,a]};
            
  num=num/.Eps3gamrep/.Eps3gamrepFV;
  Print["LCivita after tens red"];
  Print[InputForm[Cases[num,_?(Not[FreeQ[#,LCivita]]&&Not[FreeQ[#,FCh]]&&(Count[#,DiracMatrix,\[Infinity]]>2)&)]]];

DPrint["4F handeling "<>ToString[SessionTime[]]];
  If[next>3&&Not[FreeQ[num,FCh[a__]*FCh[b__]]],Print["in 4F"];num = Expand[Expand[Expand[num]/.a_FCh*b_FCh:>Sort4FermionChain[a*b]]/.a_FCh*b_FCh:>EvanescentReduce[a*b]];];

Print["before mom integration "<>ToString[SessionTime[]]];
  temp=0;
  tempUV=0;
  For[kk=Max[Join[Cases[num,SP[loopmom, loopmom]^nexp_Integer->nexp,\[Infinity]],Cases[num,SP[loopmom, loopmom]->1,\[Infinity]],{0}]],kk>=0,kk--,
  (*-2 factor is from -2/Epsilon*)
    temp=temp-2 *If[Head[num]===Plus,
      (Coefficient[Normal[Series[Coefficient[#,SP[loopmom,loopmom],kk]*LoopInt[kk,nden,delta]/.M$dim->4+Epsi,{Epsi,0,1}]],Epsi]&)/@num,
      (Coefficient[Normal[Series[Coefficient[#,SP[loopmom,loopmom],kk]*LoopInt[kk,nden,delta]/.M$dim->4+Epsi,{Epsi,0,1}]],Epsi]&)[num]];

    tempUV=tempUV+If[Head[num]===Plus,
      If[next>2||Not[UVfin],-2/FR$Eps (Normal[Series[Coefficient[#,SP[loopmom,loopmom],kk]*(*ftest[kk]*)LoopInt[kk,nden,delta]/.M$dim->4+Epsi,{Epsi,0,0}]]&)/@num,
         ((Normal[Series[Coefficient[#,SP[loopmom,loopmom],kk]*FullLoopInt[kk,nden,delta]/.M$dim->4+FR$Eps,{FR$Eps,0,1}]]&)/@num)/.Log[Mass[x__]^2/FR$MU^2]->UVLog[Mass[x]^2/FR$MU^2]],
      If[next>2||Not[UVfin],-2/FR$Eps (Normal[Series[Coefficient[#,SP[loopmom,loopmom],kk]*LoopInt[kk,nden,delta]/.M$dim->4+Epsi,{Epsi,0,0}]]&)[num],
         ((Normal[Series[Coefficient[#,SP[loopmom,loopmom],kk]*FullLoopInt[kk,nden,delta]/.M$dim->4+FR$Eps,{FR$Eps,0,1}]]&)[num])/.Log[Mass[x__]^2/FR$MU^2]->UVLog[Mass[x]^2/FR$MU^2]]
     ];
  ];

Print["before x integration "<>ToString[SessionTime[]]];

  Switch[nden,
    1,
    num=temp*FR$R2+tempUV*FR$UV;,
    _,
    num = If[next>2,
            temp=Expand[FR$R2*temp+tempUV*FR$UV];
            findfp[x_]:=DeleteCases[x,_?(FreeQ[#,Alternatives@@feynpar]&)];
            fplist=DeleteCases[If[Head[temp]===Plus,Union[findfp/@List@@temp],{findfp[temp]}],1];
            fplist=Sort[fplist,(Length[#1]+Total[Cases[#1,Power[a_,b_]->b-1,Heads->True]]>Length[#2]+Total[Cases[#2,Power[a_,b_]->b-1,Heads->True]]&)];
            fp1=Integrate[1,Sequence@@Table[{feynpar[[kk]],0,1-Total[feynpar[[1;;kk-1]]]},{kk,nden-1}]];
            fplist=(#->Integrate[#,Sequence@@Table[{feynpar[[kk]],0,1-Total[feynpar[[1;;kk-1]]]},{kk,nden-1}]]/fp1&)/@fplist;
            (Factorial[nden-1]*fp1*#/.fplist&)/@temp
            (*(Factorial[nden-1]*Integrate[#,Sequence@@Table[{feynpar[[kk]],0,1-Total[feynpar[[1;;kk-1]]]},{kk,nden-1}]]&)/@temp*),
            Print["Before XIntegrate"];
		    XIntegrate[FR$R2*temp+tempUV*FR$UV,delta,x1]
          ];
  ];(*end second switch*)

Print["before last replacement "<>ToString[SessionTime[]]];
(*DPrint[InputForm[tempUV/.FR$R2\[Rule]0]];
DPrint[FreeQ[num/.FR$R2\[Rule]0,M$dim]];*)

DPrint["num free of Internal? "<>ToString[FreeQ[num,Internal]]];
  num = num/.FourVector->FV;
  num = num//.{FCh[a___,NonCommutative[DiracMatrix[b_]],c___]FV[d_FourMomentum,b_]:>FCh[a,NonCommutative[DiracSlash[d]],c],
    DTr[a___,NonCommutative[DiracMatrix[b_]],c___]FV[d_FourMomentum,b_]:>DTr[a,NonCommutative[DiracSlash[d]],c],
    DTr[a___, NonCommutative[DiracMatrix[Index[Lorentz, Int[xx_]]]], b___]*FCh[c___, NonCommutative[DiracMatrix[Index[Lorentz, Int[xx_]]]],d___]:>
     Module[{inddf},DTr[a, NonCommutative[DiracMatrix[Index[Lorentz,inddf]]], b]*FCh[c, NonCommutative[DiracMatrix[Index[Lorentz, inddf]]],d]]};
(*DPrint[InputForm[Cases[num,_?(Not[FreeQ[#,DTr]]&)]]];*)
  num = Expand[num/.DTr->DTr2];
  num = Expand[num/.DTr2->DTr]//.{FCh[a___,NonCommutative[DiracMatrix[b_]],c___]FV[d_FourMomentum,b_]:>FCh[a,NonCommutative[DiracSlash[d]],c]};
  If[next>4,
    num=num//.FCh[a___, NonCommutative[DiracMatrix[Index[Lorentz, mu_]]], b___]FCh[c___,NonCommutative[DiracMatrix[Index[Lorentz, mu_]]],d___]:>
      Module[{nlomu},FCh[a, NonCommutative[DiracMatrix[Index[Lorentz, MF[nlomu]]]],b]FCh[c, NonCommutative[DiracMatrix[Index[Lorentz, MF[nlomu]]]],d]]/;FreeQ[mu,MF];
  ,(*if 4-fermions same label for all fist, second, ... summend indices*)
    num=num//.FCh[a___, NonCommutative[DiracMatrix[Index[Lorentz, mu_]]], b___]FCh[c___,NonCommutative[DiracMatrix[Index[Lorentz, mu_]]],d___]:>
      FCh[a, NonCommutative[DiracMatrix[Index[Lorentz, MF[ToExpression["nlomu"<>ToString[Length[{a}]]]]]]],b]*
      FCh[c, NonCommutative[DiracMatrix[Index[Lorentz, MF[ToExpression["nlomu"<>ToString[Length[{a}]]]]]]],d]/;FreeQ[mu,MF]&&FreeQ[{b},MF](*to avoid twice the same ind*);
  ];
DPrint["Not summed ME"];
DPrint[InputForm[Cases[num,c_*ME[Index[Lorentz,a_?(FreeQ[#,Ext]&)],Index[Lorentz,Ext[b_]]]]]];
DPrint["Not summed FV"];
DPrint[InputForm[Cases[num,c_*FV[FourMomentum[x_],Index[Lorentz,a_?(FreeQ[#,Ext]&)]]]]];
DPrint["LCivita after getR2"];
DPrint[InputForm[Cases[num,_?(Not[FreeQ[#,LCivita]]&&Not[FreeQ[#,FCh]]&)]]];
  (*same index for all the terms with exactly one levicivita with one summed index with a fermion chain*)
  num = Replace[Expand[num],{a_*FCh[d__, NonCommutative[DiracMatrix[Index[Lorentz, lo_]]],f__]*LCivita[b___, Index[Lorentz, lo_], c___]:>
    a*FCh[d, NonCommutative[DiracMatrix[Index[Lorentz, nloEpsF]]],f]*LCivita[b, Index[Lorentz, nloEpsF], c]/;FreeQ[a,LCivita]},{1}];

Print["LCivita after getR2"];
Print[InputForm[Cases[num,_?(Not[FreeQ[#,LCivita]]&&Not[FreeQ[#,FCh]]&)]]];
  num=num/.MF->Identity;
  num/.{FCh->FermionChain,FV->FourVector,ME->MetricTensor,SP->ScalarProduct,LCivita->LeviCivita}
];(*end of getR2*)


(* ::Subtitle:: *)
(*Get Several R2*)


(* from generic to class in FR format*)


(* ::Subsubtitle:: *)
(*R2atClass*)


R2atClass::usage="compute the R2 and UV parts for a FeynArts amplitude #1 and a topology #2 at the class level. All the contributions are summed but the wave functions keep track of the external particles.
#3 is the list of the generic external particles (F,S,V). If #4 is True, each contribution is multiplyed by IPL[list of the particles in the loop]. The fifth argument is the list of indices to
be kept in IPL like generation. Only qcd corrections are kept if the 6th argument is true and the finite part of the UV counterterms is drop if the 6th argument is False, the fast algorithm is used if the
Lagrangian is 4-dimensional and in the Feynman gauge (if #7 is True), #8 is True if the higher dimensional operators have only even dimension, the 8th argument is the highest dimension of the operators"


R2atClass[Ampl_,INTopo_,verttype_,lab_,kept_,qcd_,UVfinite_,fg_,evenDonly_,maxD_,qcdord_,FAdir_]:=Block[{res,tmp,tmp2,top,gen,rf,rl,fc,scalar,intern,nExtC,ll,
  notalreadydone, repl,ampkk,nbsuccess,tmpbis,GetR241amp,paramp,tmppara,itp,ptmp}, 
  res=0;
  nbsuccess=0;
  DPrint[InputForm[Ampl]];
  paramp=Not[FAdir===None]&&Length[verttype]>1;
  If[paramp,
  Print["In paramp true"];
  GetR241amp[myamp_]:=Block[{myres,gratop,gragen,gratmp,gratmp2,grascalar,grafc,grarf,grarl},
  myres=0;
    gratop=myamp[[1,1,2]];
    gragen=myamp[[1,2,2]];
    gratmp=If[fg&&(maxD<=4),
        GetR2[myamp,Length[verttype],UVfinite],
        GetR2[myamp,Length[verttype],UVfinite,evenDonly,maxD,fg]];
        If[Not[FreeQ[gratmp,Internal]],Print[Style["Error : loop momentum appear in the result",Red]];Print[InputForm[gratmp]];Print[InputForm[Ampl[[kk]]]]];
Print["after get R2 "<>ToString[SessionTime[]]];Print[InputForm[gratmp]];

    If[Not[gratmp===0],
      For[ll=1,ll<=Length[myamp[[4,2]]],ll++,
        grascalar=1;
        grafc=INTopo[[gratop,2,gragen,2,ll]];
        grarf={};
        If[OrderedQ[grafc[[1;;Length[verttype],2]],(PartOrder[#1,#2]&)],
          For[nn=1,nn<=Length[Ampl[[0,1,2,1]]],nn++,
            grarf=Append[grarf,Rule[Ampl[[0,1,2,1,nn,1]][Index[Generic,grafc[[nn,1,1]]]],grafc[[nn,2]]]];
            If[Ampl[[0,1,2,1,nn,1]]===S,
              grascalar=grascalar*SWF[grafc[[nn,2]],grafc[[nn,1,1]]];
            ];
            If[Ampl[[0,1,2,1,nn,1]]===F,
              grarf=Append[grarf,Rule[SpinorType[Index[Generic,grafc[[nn,1,1]]]],SpinorType[grafc[[nn,2]]]]];
            ];
          ];
          grarl=Table[Rule[myamp[[4,1,nn]],myamp[[4,2,ll,nn]]],{nn,1,Length[myamp[[4,1]]]}];
If[Not[FreeQ[gratmp,_IndexDelta]],DPrint[Style[InputForm[grarl],Green]];];
          grarl=grarl/.{FourVector->FV}/.{FV->FourVector}/.IndexDelta->IndexDel;
          intern=If[lab,IPL[Sort[List@@Union[DeleteCases[grafc[[Length[verttype]+1;;,2]]/.{-1->1},Index[Except[Alternatives@@kept],__],\[Infinity]]],PartOrder]],1,
            (*for debugging*)IPL[Join[{kk,ll},List@@DeleteCases[grafc[[Length[verttype]+1;;,2]]/.{-1->1},Index[Except[Alternatives@@kept],__],\[Infinity]]]]];
          (*add the sign for the ghost loop when $FermionLines is set to False*)
          If[Not[$FermionLines]&&Union[List@@Head/@(grafc[[Length[verttype]+1;;,2]]/.{-1->1})]==={U},intern=-G$L*intern;];
If[Not[FreeQ[gratmp,_IndexDelta]],DPrint[Style[InputForm[gratmp],Green]];];
          gratmp2=grascalar*intern*gratmp/.{FourVector->FV}/.{FV->FourVector}/.grarl/.grarf/.If[Length[M$FACouplings]>0,M$FACouplings,{}];

          If[qcd,
            nExtC={Length[Union[Cases[gratmp2,Index[Colour,a_?(#<=Length[verttype]&)],\[Infinity]]]],Length[Union[Cases[gratmp2,Index[Gluon,a_?(#<=Length[verttype]&)],\[Infinity]]]]};
            (*Print["nExtC"];Print[InputForm[nExtC]];Print[Max[{2,(Append[Cases[qcdord,{nExtC[[1]],nExtC[[2]],a_}->a],0][[1]]+2)}]];*)
            gratmp2= Total[Table[Coefficient[gratmp2,GS,nn]*GS^nn,{nn,Max[{2,(Append[Cases[qcdord,{nExtC[[1]],nExtC[[2]],a_}->a],0][[1]]+2)}],100}]];
          ];
          myres=myres+gratmp2;
        ];
      ];
    ];
    myres
  ](*GetR241amp*);(*
  For[kk=1,kk<=Length[Ampl],kk++,
    res=res+GetR241amp[Ampl[[kk]]];
  ];*)
  If[Not[NLO$AlreadyLaunch],
    NLO$AlreadyLaunch=True;
    (*LaunchKernels[];*)
    $DistributedContexts=Automatic;
    Print[$DistributedContexts];
    DistributeDefinitions[$Packages];
    ParallelEvaluate[SetDirectory[FAdir];
      <<FeynArts`];
  ]
  (*DistributeDefinitions["FeynArts`"];*)
  ParallelNeeds["FeynArts`"];
  $DistributedContexts={$Context,"FeynArts`"};
  SetSharedVariable[NLO$DiracBasis,NLO$EvaList,NLO$EvaMF,NLO$Basis2BasisTr,$FermionLines];
  DistributeDefinitions[GetR241amp,GetR2,PartOrder,XIntegrate,red2v,red4v,LCivita,ME,FV,DTr,DTr2,DTrDist,
    DTrDist2,DTrDist0,FCh,FChDist,FChDist2,SP,DiracOpenChain,CC2,DPrint,EvanescentReduce,RotateTr];
  (*ParallelEvaluate[Print["testSP"];
    Print[InputForm[SP[ FourMomentum[Internal, 1],x1*FourMomentum[Incoming, 3]]]];
    Print[InputForm[CC2[NonCommutative[DiracMatrix[mu]]]]];];*)
     ptmp=Table[tmppara=Ampl[[itp]];
       ParallelSubmit[{itp,tmppara},
       GetR241amp[tmppara]]
       ,{itp,1,Length[Ampl]}];
     res=Plus@@(WaitAll[ptmp]);
  ,
  Print["In paramp false"];
  For[kk=1,kk<=Length[Ampl],kk++,
Print["amplitude "<>ToString[kk]<>" at "<>ToString[SessionTime[]]];
    top=Ampl[[kk,1,1,2]];
    gen=Ampl[[kk,1,2,2]];
Print["before get R2 "<>ToString[SessionTime[]]];
(*If[kk==2,DumpSave["~/Desktop/testdump","NLOCT2`"]];*)
    If[verttype===NLO$currentvert,
DPrint["current vert is same as prev"];
      repl=False;
      ampkk=Length[NLO$ampR2];
	DPrint[InputForm[Ampl[[kk,3]]]];
      While[Length[repl]<1&&ampkk>=1, repl=AmpRenaming[NLO$ampR2[[ampkk,1]],Ampl[[kk,3]],Length[verttype]];DPrint[repl];DPrint[ampkk];ampkk--;];
      If[repl===False,notalreadydone=True;,nbsuccess++;notalreadydone=False;];
      ,
Print["current vert is not same as prev"];
      (*first time with this vertex type*)
      notalreadydone=True;NLO$currentvert=verttype;NLO$ampR2={};
   ];
    If[True(*notalreadydone*),
     Print[fg&&(maxD<=4)];Print["feynman and maxdim"];
      tmp=If[fg&&(maxD<=4),
        GetR2[Ampl[[kk]],Length[verttype],UVfinite],
        GetR2[Ampl[[kk]],Length[verttype],UVfinite,evenDonly,maxD,fg]];
      If[Not[notalreadydone],DPrint[Style[ampkk,Orange]];DPrint[InputForm[Simplify[tmp-(NLO$ampR2[[ampkk+1,2]]/.repl),TimeConstraint->1]]]];
      NLO$ampR2=Append[NLO$ampR2,{Ampl[[kk,3]],tmp}];,
      (*already computed*)
      Print[Style["already done",Orange]];
      tmp=NLO$ampR2[[ampkk+1,2]]/.repl;
      tmpbis=If[fg&&(maxD<=4),GetR2[Ampl[[kk]],Length[verttype],UVfinite],GetR2[Ampl[[kk]],Length[verttype],UVfinite,evenDonly,maxD,fg]];
      If[Not[notalreadydone],DPrint[Style[ampkk,Orange]];DPrint[InputForm[Simplify[tmp-(NLO$ampR2[[ampkk+1,2]]/.repl),TimeConstraint->1]]]];
      NLO$ampR2=Append[NLO$ampR2,{Ampl[[kk,3]],tmp}];
      Print[Style["check renaming",Magenta]];
      Print[InputForm[tmp-tmpbis]];
    ];


(*Sanity check*)
If[Not[FreeQ[tmp,Internal]],Print[Style["Error : loop momentum appear in the result",Red]];Print[InputForm[tmp]];Print[InputForm[Ampl[[kk]]]]];
Print["after get R2 "<>ToString[SessionTime[]]];
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
If[Not[FreeQ[tmp,_IndexDelta]],DPrint[Style[InputForm[rl],Green]];];
          rl=rl/.{FourVector->FV}/.{FV->FourVector}/.IndexDelta->IndexDel;
          intern=If[lab,IPL[Sort[List@@Union[DeleteCases[fc[[Length[verttype]+1;;,2]]/.{-1->1},Index[Except[Alternatives@@kept],__],\[Infinity]]],PartOrder]],1,
            (*for debugging*)IPL[Join[{kk,ll},List@@DeleteCases[fc[[Length[verttype]+1;;,2]]/.{-1->1},Index[Except[Alternatives@@kept],__],\[Infinity]]]]];
          (*add the sign for the ghost loop when $FermionLines is set to False*)
          If[Not[$FermionLines]&&Union[List@@Head/@(fc[[Length[verttype]+1;;,2]]/.{-1->1})]==={U},intern=-G$L*intern;];
If[Not[FreeQ[tmp,_IndexDelta]],DPrint[Style[InputForm[tmp],Green]];];
          tmp2=scalar*intern*tmp/.{FourVector->FV}/.{FV->FourVector}/.rl/.rf/.If[Length[M$FACouplings]>0,M$FACouplings,{}];

          If[qcd,
            nExtC={Length[Union[Cases[tmp2,Index[Colour,a_?(#<=Length[verttype]&)],\[Infinity]]]],Length[Union[Cases[tmp2,Index[Gluon,a_?(#<=Length[verttype]&)],\[Infinity]]]]};
            tmp2= Total[Table[Coefficient[tmp2,GS,nn]*GS^nn,{nn,Max[{2,(Append[Cases[qcdord,{nExtC[[1]],nExtC[[2]],a_}->a],0][[1]]+2)}],100}]];
          ];
          res=res+tmp2;
        ];
      ];
    ];
  ];
  Print[Style["nb of amp match",Red]];
  Print[nbsuccess];Print[Length[Ampl]];
  ];(*end if paraamp*)
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
    (*assume at most two summed lorentz index in the lorentz structure*)
    tmp=Expand[Expand[Ampl[[kk,3]]/.{IndexSum[b___*DiracObject[c___,DiracMatrix[a_],d___][Index[Dirac,i_],Index[Dirac,j_]],{a_,1,4}]:>
    Module[{lo},((b*DiracObject[c,DiracMatrix[a],d][Index[Dirac,i],Index[Dirac,j]])/.a->Index[Lorentz,lo])],
    IndexSum[b___*DiracObject[c___,DiracMatrix[a_],DiracMatrix[e_],d___][Index[Dirac,i_],Index[Dirac,j_]],{a_,1,4},{e_,1,4}]:>
    Module[{lo,lo2},((b*DiracObject[c,DiracMatrix[a],DiracMatrix[e],d][Index[Dirac,i],Index[Dirac,j]])/.{a->Index[Lorentz,lo],e->Index[Lorentz,lo2]})],
    IndexSum[b___*DiracObject[c___,DiracMatrix[e_],DiracMatrix[a_],d___][Index[Dirac,i_],Index[Dirac,j_]],{a_,1,4},{e_,1,4}]:>
    Module[{lo,lo2},((b*DiracObject[c,DiracMatrix[e],DiracMatrix[a],d][Index[Dirac,i],Index[Dirac,j]])/.{a->Index[Lorentz,lo],e->Index[Lorentz,lo2]})]}]/.
      DiracObject[a__][Index[Dirac, i_], Index[Dirac, j_]]->DiracOpenChain[a][Index[Dirac, i], Index[Dirac, j]]]/.{FermionChain->FCh}/.{FCh->FermionChain};
    
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
          rl=rl/.{FourVector->FV}/.{FV->FourVector}/.IndexDelta->IndexDel;
          tmp2=scalar*tmp/.{FourVector->FV}/.{FV->FourVector}/.rl/.rf/.If[Length[M$FACouplings]>0,M$FACouplings,{}];
                 
          res=res+tmp2;
        ];
      ];
    ];
  ];
  res*FR$UV
]


(* ::Subsubtitle:: *)
(*PartOrder*)


PartOrder::usage="Ordering function for the particles, fermion goes before tensor which goes before vector which goes before scalar, 
antiparticles goes before particles of the same type, remaining ordering is done according to the particle numbers"


PartOrder[V[x_,ex1___],-V[y_,ex2___]]:=False;
PartOrder[V[x_,ex1___],V[y_,ex2___]]:=False/;y<x;
PartOrder[-V[x_,ex1___],-V[y_,ex2___]]:=False/;y<x;
PartOrder[T[x_,ex1___],-T[y_,ex2___]]:=False;
PartOrder[T[x_,ex1___],T[y_,ex2___]]:=False/;y<x;
PartOrder[-T[x_,ex1___],-T[y_,ex2___]]:=False/;y<x;
PartOrder[-S[x_,ex1___]|S[x_,ex1___]|V[y_,ex2___]|-V[y_,ex2___],T[y_,ex2___]|-T[y_,ex2___]]:=False;
PartOrder[-S[x_,ex1___]|S[x_,ex1___],V[y_,ex2___]|-V[y_,ex2___]]:=False;
PartOrder[S[x_,ex1___],-S[y_,ex2___]]:=False;
PartOrder[S[x_,ex1___],S[y_,ex2___]]:=False/;y<x;
PartOrder[-S[x_,ex1___],-S[y_,ex2___]]:=False/;y<x;
PartOrder[-F[x_,ex1___],-F[y_,ex2___]]:=False/;y<x;
PartOrder[F[x_,ex1___],F[y_,ex2___]]:=False/;y<x;
PartOrder[S[y_,ex1___]|V[y_,ex1___]|T[y_,ex1___],F[x_,ex2___]]:=False;
PartOrder[-S[y_,ex1___]|-V[y_,ex1___]|-T[y_,ex1___],F[x_,ex2___]]:=False;
PartOrder[-Except[F,X_][y_,ex1___],-F[x_,ex2___]]:=False;
PartOrder[Except[Times,X_][y_,ex1___],-F[x_,ex2___]]:=False;
PartOrder[F[y_,ex1___],-F[x_,ex2___]]:=False;

PartOrder[V[x_,ex1___],-V[x_,ex2___]]:=False;
PartOrder[T[x_,ex1___],-T[x_,ex2___]]:=False;
PartOrder[-S[x_,ex1___]|S[x_,ex1___],V[x_,ex2___]|-V[x_,ex2___]]:=False;
PartOrder[-S[x_,ex1___]|S[x_,ex1___]|V[x_,ex2___]|-V[x_,ex2___],T[x_,ex2___]|-T[x_,ex2___]]:=False;
PartOrder[S[x_,ex1___],-S[x_,ex2___]]:=False;
PartOrder[S[x_,ex1___]|V[x_,ex1___]|T[x_,ex1___],F[x_,ex2___]]:=False;
PartOrder[-S[x_,ex1___]|-V[x_,ex1___]|-T[x_,ex1___],F[x_,ex2___]]:=False;
PartOrder[X_[x_,ex1___],-F[x_,ex2___]]:=False;
PartOrder[F[x_,ex1___],-F[x_,ex2___]]:=False;

PartOrder[x_,x_]:=True;
PartOrder[x_,y_]:=True;


(*true if a class insertion and external fields are not ordered*)
InTopClassNotOrderedQ[intop_,n_]:=((*DPrint[intop];
DPrint[MatchQ[Head[intop],FeynmanGraph[a_,Classes\[Equal]b_]]&&Not[OrderedQ[intop[[1;;n,2]],(PartOrder[#1,#2]&)]]];*)
MatchQ[Head[intop],FeynmanGraph[a_,Classes==b_]]&&(Not[OrderedQ[intop[[1;;n,2]],(PartOrder[#1,#2]&)]]||Not[FreeQ[intop[[n+1;;,2]],_T]]));


(* ::Subsubtitle:: *)
(*R2vertlist*)


R2vertlist::usage="rewrite the result from Expand[R2atClass] into a list of vertices similar to those of FeynRules"


R2vertlist[vertsum_]:=Block[{vertList,lab,ver,pos,ss,v,tt,ind,indrep,f,indlist,ind2,ind3,xx,ll,kk,nn,ff,mm,UVmass,deltarule,indl,aa,bb,s1,
                             s2,extsum,wft,inveps,res,ftmpind,nferm},
  vertList={};
  If[Head[vertsum]===Times,res={vertsum};,res=vertsum];
  For[kk=1,kk<=Length[res],kk++,
    lab=Cases[res[[kk]],PolarizationTensor[tt_,FourMomentum[Incoming,ind_],Index[Lorentz,ind2_],Index[Lorentz,ind3_]]->{tt,ind,ind2,ind3}];
    lab=Join[lab,Cases[res[[kk]],PolarizationVector[v_,FourMomentum[Incoming,ind_],Index[Lorentz,ind2_]]->{v,ind,ind2}]];
    lab=Join[lab,Cases[res[[kk]],SWF[ss_,ind_]->{ss,ind}]];
    lab=Join[lab,Sort[Cases[res[[kk]],FermionChain[NonCommutative[SpinorType[f_][-FourMomentum[Incoming,ind_]|
                       FourMomentum[Incoming,ind_],__]],xx__]->{f,ind,ftmpind},\[Infinity]],#1[[2]]<#2[[2]]&]];
    nferm=1;
    While[Not[FreeQ[lab,ftmpind]],lab[[Sequence@@Position[lab,ftmpind][[1]]]]=2nferm-1;nferm++;];
    lab=Join[lab,Sort[Cases[res[[kk]],FermionChain[xx__,NonCommutative[SpinorType[f_][-FourMomentum[Incoming,ind_]|FourMomentum[Incoming,ind_],__]]]->{f,ind,ftmpind},\[Infinity]],#1[[2]]<#2[[2]]&]];
    nferm=1;
    While[Not[FreeQ[lab,ftmpind]],lab[[Sequence@@Position[lab,ftmpind][[1]]]]=2nferm;nferm++;];
    lab=Sort[lab,PartOrder[#1[[1]],#2[[1]]]&];

    (*Majorana reordering for 2 fermions only*)
    If[lab[[1,1,0]]===F&&lab[[2,1,0]]===F&&lab[[1,1,1]]===lab[[2,1,1]]&&lab[[1,2]]>lab[[2,2]]&&Count[lab,_F,\[Infinity]]===2,lab[[1;;2]]=lab[[{2,1}]];];
    indrep=Table[FourMomentum[Incoming,lab[[ll,2]]]->FourMomentum[Incoming,ll],{ll,1,Length[lab]}];
    For[nn=1,nn<=Length[lab],nn++,
      If[MatchQ[lab[[nn,1]],T[__]|-T[__]],indrep=Join[indrep, { Index[Lorentz,lab[[nn,3]]]->Index[Lorentz,Ext[nn,1]] ,Index[Lorentz,lab[[nn,4]]]->Index[Lorentz,Ext[nn,2]] } ]; ];
      If[MatchQ[lab[[nn,1]],V[__]|-V[__]],indrep=Append[indrep,  Index[Lorentz,lab[[nn,3]]]->Index[Lorentz,Ext[nn]]  ]; ];
      If[MatchQ[lab[[nn,1]],F[__]|-F[__]],indrep=Append[indrep,  Index[Spin,Ext[lab[[nn,3]]]]->Index[Spin,Ext[nn]]  ]; ];
      indlist=Cases[lab[[nn,1]],Index[xx_,ind2_],\[Infinity]];
      For[ll=1,ll<=Length[indlist],ll++,indrep=Append[indrep,indlist[[ll]]->Index[indlist[[ll,1]],Ext[nn]]];];
    ];

    ver=res[[kk]](*/.{PolarizationTensor[tt_,FourMomentum[Incoming,ind_],Index[Lorentz,ind2_],Index[Lorentz,ind2_]]\[Rule]MetricTensor[Index[Lorentz, Ext[ind,1]], Index[Lorentz, Ext[ind,2]]]}*);
    ver=DeleteCases[ver,PolarizationTensor[tt_,FourMomentum[Incoming,ind_],Index[Lorentz,ind2_],Index[Lorentz,ind3_]]];
    ver=DeleteCases[ver,PolarizationVector[v_,FourMomentum[Incoming,ind_],Index[Lorentz,ind2_]]];
    ver=DeleteCases[ver,SWF[ss_,ind_]];
    ver=Replace[ver,{FermionChain[NonCommutative[SpinorType[-F[f__]|F[f__]][-FourMomentum[Incoming,ind_]|FourMomentum[Incoming,ind_],__]],xx___,
                     NonCommutative[SpinorType[F[f2__]|-F[f2__]][-FourMomentum[Incoming,ind2_]|FourMomentum[Incoming,ind2_],__]]]:>
                      -TensDot[xx][Index[Spin,Ext[lab[[ (Position[lab[[All,2]],ind][[1,1]]),3 ]]]],Index[Spin,Ext[lab[[ (Position[lab[[All,2]],ind2][[1,1]]),3 ]]]]]},\[Infinity]];

    ver=ver/.indrep;
(*DPrint[ver];*)
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


ModelR2[mod_,gen_,lab_,kept_,qcd_,zerom_,assumpt_,UVCT_,comas_,ctpa_,no4SCT_,fga_,evenD_,mdim_,gvlist_,susy_,mixon_,mzs_,FAdirname_,MaxProp_]:=Block[{vertlist,genver,kkpl,topo,INTopo,Ampl,tmp,tmpCT,x,ind,indl,rl1,rlf,tdrl,a,b,temp,ckt,topoCT,INTopoCT,AmplCT,CTSol,
vrl,vrl2,momk,indt,vertCTlist,zmsol,Orderl,zmF,pos,check,kct,masslist={},ToParam,kkind,ntopo,vertl,CountC,qcdorders,tadpolerep={},rep2pt,zerom2,lili,kik,nF},
CountC[x_]:=Count[x,Gluon|Colour,\[Infinity]];

(*ToParam[x_]:=If[LeafCount[x[[2]]]>200,CTparam=Append[CTparam,DeltaToExp[x[[1]]]->x[[2]]];x[[1]]->DeltaToExp[x[[1]]],x];*)
ToParam[x_]:=Block[{},CTparam=Append[CTparam,DeltaToExp[x[[1]]]->x[[2]]];x[[1]]->DeltaToExp[x[[1]]]];

temp=SessionTime[];
  If[gvlist===Automatic,
    genver=Table[Table[(Join[Flatten[Table[{F,F},{ii,kk/3}]],#]&)/@Union[Reverse/@Sort/@Tuples[{S,V},ll-kk]],{kk,0,ll,3}],{ll,mdim}];
    genver=Sort[DeleteCases[Flatten[genver,2],{V}|If[no4SCT,{S,S,S,S},{}]],(Length[#1]<Length[#2]&)];,
    genver=(Sort[#,((#1<#2)/.{F->1,V->2,S->3}&)]&)/@gvlist;];
  vertlist={};
  vertCTlist={};
  CTSol={};
  tdrl={TensDot[ProjP][s1_,s2_]->ProjP[s1,s2],TensDot[ProjM][s1_,s2_]->ProjM[s1,s2],TensDot[Ga[mu_]][s1_,s2_]->Ga[mu,s1,s2]};
  rlf={TensDot[gm__,ProjP][Index[Spin,Ext[s2_]],Index[Spin,Ext[s1_]]]:>(-1)^(Length[List[gm]]-1)*
         TensDot[Sequence@@Reverse[List[gm]],If[EvenQ[Length[List[gm]]],ProjP,ProjM]][Index[Spin,Ext[s1]],Index[Spin,Ext[s2]]]/;s2>s1,
       TensDot[gm__,ProjM][Index[Spin,Ext[s2_]],Index[Spin,Ext[s1_]]]:>(-1)^(Length[List[gm]]-1)*
         TensDot[Sequence@@Reverse[List[gm]],If[EvenQ[Length[List[gm]]],ProjM,ProjP]][Index[Spin,Ext[s1]],Index[Spin,Ext[s2]]]/;s2>s1,
       ProjP[Index[Spin,Ext[s2_]],Index[Spin,Ext[s1_]]]:>-ProjP[Index[Spin,Ext[s1]],Index[Spin,Ext[s2]]]/;s2>s1,
       ProjM[Index[Spin,Ext[s2_]],Index[Spin,Ext[s1_]]]:>-ProjM[Index[Spin,Ext[s1]],Index[Spin,Ext[s2]]]/;s2>s1};
  
  vrl={MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[3]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[3]],Index[Gluon,Ext[2]],Index[Gluon,Ext[4]]]->
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[3]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]],Index[Gluon,Ext[4]]]+
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[3]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[4]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]]],
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]],Index[Gluon,Ext[4]]]->
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[3]],Index[Gluon,Ext[2]],Index[Gluon,Ext[4]]]-
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[4]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]]],
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[4]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[4]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]]]->
       -MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[4]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[2]],Index[Gluon,Ext[3]],Index[Gluon,Ext[4]]]+
       MetricTensor[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[4]]]SUF[Index[Gluon,Ext[1]],Index[Gluon,Ext[3]],Index[Gluon,Ext[2]],Index[Gluon,Ext[4]]]};

(*change with simplification otherwise not simplified*)
rep2pt={TensDot[a___,SlashedP[FourMomentum[Incoming,1]], b___][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]->
        -TensDot[a,SlashedP[FourMomentum[Incoming,2]], b][Index[Spin, Ext[1]], Index[Spin, Ext[2]]],
        Eps[b___,FourVector[FourMomentum[Incoming,1]],FourVector[FourMomentum[Incoming,2]],a___]->0,
        Eps[b___,FourVector[FourMomentum[Incoming,1]],a___]:>-Eps[b,FourVector[FourMomentum[Incoming,2]],a]};

  For[kkpl=1,kkpl<=Length[genver],kkpl++,
    Print[Style["Computing CT for the "<>ToString[genver[[kkpl]]]<>" vertices.",Blue]];
    rl1={MetricTensor->ME,ScalarProduct->SP,LeviCivita[x__]->Eps[x],
	    FourVector[FourMomentum[Incoming,ind_],Index[Lorentz,Ext[indl_]]]:>FV[FourMomentum[Incoming,ind],Index[Lorentz,Ext[indl]]]/;indl<=Length[genver[[kkpl]]],
	    FourVector[-FourMomentum[Incoming,ind_],Index[Lorentz,Ext[indl_]]]:>-FV[FourMomentum[Incoming,ind],Index[Lorentz,Ext[indl]]]/;indl<=Length[genver[[kkpl]]],
        FourVector[FourMomentum[Incoming,ind_],Index[Lorentz,Ext[indl_,indt_]]]:>FV[FourMomentum[Incoming,ind],Index[Lorentz,Ext[indl,indt]]]/;indl<=Length[genver[[kkpl]]],
	    FourVector[-FourMomentum[Incoming,ind_],Index[Lorentz,Ext[indl_,indt_]]]:>-FV[FourMomentum[Incoming,ind],Index[Lorentz,Ext[indl,indt]]]/;indl<=Length[genver[[kkpl]]],
        NonCommutative[ChiralityProjector[1]]->ProjP,NonCommutative[ChiralityProjector[-1]]->ProjM,NonCommutative[DiracSlash[FourMomentum[Incoming,x_]]]->
        SlashedP[FourMomentum[Incoming,x]],NonCommutative[DiracMatrix[Index[Lorentz,x_]]]->Ga[Index[Lorentz,x]],NonCommutative[NLO$EO[x_]]->NLO$EO[x],SumOver[x__,External]->1,EL->ee,GS->gs};

    (*Counterterm amplitude*)
DPrint["before CT"];
    If[UVCT,
      topoCT=CreateCTTopologies[1,Length[genver[[kkpl]]]->0,Adjacencies->Table[kka,{kka,3,mdim}],ExcludeTopologies->{Internal}];
      INTopoCT = InsertFields[topoCT,genver[[kkpl]]->{},Model->mod,GenericModel->gen];
      INTopoCT=DeleteCases[INTopoCT,_?(InTopClassNotOrderedQ[#,Length[genver[[kkpl]]]]&),{5}];
      INTopoCT=DeleteCases[INTopoCT,FeynmanGraph[a__][b__]->Insertions[Classes][],\[Infinity]];
      AmplCT= CreateFeynAmp[INTopoCT];

      tmpCT=CTatClass[AmplCT,INTopoCT,genver[[kkpl]]];

(*Print["before R2vertlist"];
Print[InputForm[tmpCT]];*)
      tmpCT=R2vertlist[Expand[Expand[Expand[tmpCT,_SWF],_PolarizationVector],FermionChain]]/.IndexDelta->IndexDel;
    Print["factor 3 check"];
DPrint["before qcdorder"];
(*Print[InputForm[tmpCT]];*)
      If[Length[tmpCT]>0,
        qcdorders = First/@Sort/@Gather[Union[(({Length[Cases[#[[1]],Index[Colour,_],\[Infinity]]],Length[Cases[#[[1]],Index[Gluon,_],\[Infinity]]],
        If[FreeQ[#[[3]],GS],0,Series[#[[3]],{GS,0,99}][[4]]]}&)/@tmpCT)],(#1[[1]]==#2[[1]]&&#1[[2]]==#2[[2]]&)];
        Print["qcdorders"];Print[InputForm[qcdorders]];,
        qcdorders = {};DPrint["no CT"];
      ];
    ];

Print["qcdorders2"];Print[InputForm[qcdorders]];

    (*loop amplitude*)
DPrint["loop amp"];

    topo = CreateTopologies[1,Length[genver[[kkpl]]]->0,Adjacencies->Table[kka,{kka,3,mdim}],ExcludeTopologies->{Internal}];
    topo=DeleteCases[topo,_?(Count[#,Propagator[Loop[1]],\[Infinity],Heads->True]>MaxProp&)];
    INTopo = InsertFields[topo,genver[[kkpl]]->{},Model->mod,GenericModel->gen,ExcludeParticles->Join[If[genver[[kkpl]]==={S,S,S,S}&&mdim<=4,{U},{}],{}],
                          ExcludeFieldPoints->If[Length[genver[[kkpl]]]>3&&mdim<=4,{FieldPoint[_][S,S,S],FieldPoint[_][S,V,V]},{}]];
    (*Print[InputForm[INTopo]];*)
    INTopo=DeleteCases[INTopo,_?(InTopClassNotOrderedQ[#,Length[genver[[kkpl]]]]&),{5}];
    INTopo=DeleteCases[INTopo,FeynmanGraph[a__][b__]->Insertions[Classes][],\[Infinity]];
    (*add assumptions for masses*)
    If[kkpl===1,masslist=DeleteCases[Union[(Mass/.#[[2]]&)/@M$ClassesDescription],0];];
    
    (*remove diagrams without at least one interaction with three or more color multiplets*)
    If[qcd,
      For[ntopo=1,ntopo<=Length[INTopo],ntopo++,
        vertl=((Cases[INTopo[[ntopo,1]],Propagator[x_][y___,#,zz___,f_Field]->f,\[Infinity]]&)/@Union[Cases[INTopo[[ntopo,1]],Vertex[_?(#>2&)][x_],\[Infinity]]]);
       INTopo[[ntopo]]=DeleteCases[INTopo[[ntopo]],f_?(MatchQ[#,FeynmanGraph[_,Classes==_][x__]]&&(Max[CountC/@(vertl/.List@@#)]<3)&),\[Infinity]];
      ];
      INTopo=DeleteCases[DeleteCases[INTopo,_->Insertions[Classes][],{3}],_->Insertions[Generic][],{1}];
    ];


    Ampl= CreateFeynAmp[INTopo];
(*Print[InputForm[Ampl]];*)
    For[ckt=1,ckt<=Length[Ampl],ckt++,Ampl[[ckt,4,2]]=Ampl[[ckt,4,2]]/.{aa_[bb__?(FreeQ[#,Colour]&),Index[Colour,cc_]]->aa[bb]};(*remove colour index in masses*)];
    Print["Writing amplitude finished after "<>ToString[SessionTime[]-temp]];
    tmp=R2atClass[Ampl,INTopo,genver[[kkpl]],lab,kept,qcd,UVCT,fga,evenD,mdim,qcdorders,FAdirname];
Print["amp after R2atClass"];(*Print[InputForm[tmp]];*)
    
(*Print[InputForm[tmp]];*)
    Print["Writing the R2 and UV parts at the class level finished after "<>ToString[SessionTime[]-temp]];
    tmp=R2vertlist[Expand[Expand[Expand[Expand[tmp,_SWF],_PolarizationVector],_FermionChain],_PolarizationTensor]]/.IndexDelta->IndexDel;
    Print["factor 3 check"];


    Print["Writing the symbolic CT finished after "<>ToString[SessionTime[]-temp]];
Print[ByteCount[tmp]];
    tmp=tmp/.IndexSum[x_,{b_,1,8}]:>Module[{gl},(x/.b->Index[Gluon,gl])SumOver[Index[Gluon,gl],8]]/;FreeQ[x,dSUN];
Print[ByteCount[tmp]];

    tmp=ExpandAll[ExpandAll[tmp/.{SUNTSum[a_,b_,c_,d_]->IndexDel[a,d]IndexDel[c,b]/2- IndexDel[a,b]IndexDel[c,d]/6}/.{SUNT->SUT,SUNF->SUF},Colour],Gluon];
Print["Writing the symbolic CT finished after 0"<>ToString[SessionTime[]-temp]];
    tmp=ExpandAll[ExpandAll[tmp,Colour],Gluon];
Print["Writing the symbolic CT finished after 01"<>ToString[SessionTime[]-temp]];
    tmp=ReplaceAll[tmp,IndexSum[dSUN[a___,b_,c___]*x___,{b_,1,8}]:>Module[{gl},dSUN[a,Index[Gluon,gl],c](x/.b->Index[Gluon,gl])SumOver[Index[Gluon,gl],8]]];
Print["Writing the symbolic CT finished after 1"<>ToString[SessionTime[]-temp]];
Print["Writing the symbolic CT finished after 2"<>ToString[SessionTime[]-temp]];

    If[UVCT,
     tmpCT=ExpandAll[ExpandAll[tmpCT/.{SUNTSum[a_,b_,c_,d_]-> IndexDel[a,d]IndexDel[c,b]/2- IndexDel[a,b]IndexDel[c,d]/6}/.{SUNT->SUT,SUNF->SUF},Colour],Gluon];
     tmpCT=ExpandAll[ExpandAll[tmpCT,Colour],Gluon];
    ];
    (*Print["Expand finished after "<>ToString[SessionTime[]-temp]];*)
    (* Esthetic replacements*)
    vrl2={FourVector[FourMomentum[Incoming, a_Integer], Index[Lorentz, Ext[a_Integer]]]->-(Total[Table[FourVector[FourMomentum[Incoming,momk ], 
          Index[Lorentz, Ext[a]]],{momk,1,Length[genver[[kkpl]]]}]]-FourVector[FourMomentum[Incoming, a], Index[Lorentz, Ext[a]]])};
    tmp=tmp/.FourVector->FV/.FV->FourVector/.vrl/.vrl2;
    tmp=(Replace[#,(SumOver[Index[w:Gluon|Colour,x_],y_Integer]*zz___):>y*Times[zz]/;FreeQ[Times[zz],Index[w,x]],\[Infinity]]&)/@tmp;
    If[UVCT,tmpCT=tmpCT/.vrl/.vrl2;];
    Print["factor 3 check"];
    Print["SU3 algebra finished after "<>ToString[SessionTime[]-temp]];

    If[Length[tmp]>0||Length[tmpCT]>0,
      tmp[[All,2]]=I*(Simplify[ReplaceRepeated[#/.rl1/.tdrl/.IndexSum->Sum/.rlf,SumOver[ind_?((FreeQ[#,Gluon]&&FreeQ[#,Colour])&),x_Integer]*a__:>
        dum1 Total[Table[(Times[a])/.{ind->kkind},{kkind,1,x}]]],TimeConstraint->10]&)/@tmp[[All,2]];
      tmp[[All,3]]=(ReplaceRepeated[#/.rl1/.tdrl/.IndexSum->Sum/.rlf,SumOver[ind_?((FreeQ[#,Gluon]&&FreeQ[#,Colour])&),x_Integer]*a__:>
        dum1 Total[Table[(Times[a])/.{ind->kkind},{kkind,1,x}]]]&)/@tmp[[All,3]];
        
      If[UVCT,
        tmpCT[[All,3]]=(ReplaceRepeated[#/.rl1/.tdrl/.IndexSum->Sum/.rlf,SumOver[ind_?((FreeQ[#,Gluon]&&FreeQ[#,Colour])&),x_Integer]*
          a__:>dum1 Total[Table[(Times[a])/.{ind->kkpl},{kkpl,1,x}]]]&)/@tmpCT[[All,3]];
          
        If[Length[genver[[kkpl]]]===2,
          (*Only the finite piece in the renormalization constants*)
          tmp=tmp/.rep2pt/.{FourMomentum[Incoming,1]->-FourMomentum[Incoming,2]};
          tmpCT=tmpCT/.rep2pt/.{FourMomentum[Incoming,1]->-FourMomentum[Incoming,2]};
          DPrint["before CTSol"];
          If[mixon,DPrint["in mix only"];
            (*remove finite part of the UV if there is no CT*)
            tmp = ({#1,#2,If[FreeQ[tmpCT,#1],Simplify[Coefficient[#3,inveps,1]*inveps/.inveps->1/FR$Eps,TimeConstraint->0.01],#3]}&)@@@(tmp/.FR$Eps->1/inveps);
          ];
DPrint["after if"];
          (*If FR$epsUV=0 no UV in the 2 point reno cond, does if no value*)
          FR$epsUV=0;
          CTSol=Join[CTSol,SolveDelta[tmpCT,({#1,#2,Simplify[#3-(1-FR$epsUV)Coefficient[#3,inveps,1]*inveps/.inveps->1/FR$Eps,TimeConstraint->0.01]}&)@@@(tmp/.FR$Eps->1/inveps),
                                      genver[[kkpl]],assumpt,comas]];
          If[FR$epsUV===0,
            tmp[[All,3]]=-I*(Simplify[Coefficient[#/.FR$Eps->1/inveps,inveps,1]/FR$Eps,TimeConstraint->1]&)/@tmp[[All,3]];,
            tmpCT = Sort[tmpCT,((Position[tmp[[All,1]],#1[[1]]][[1,1]]<Position[tmp[[All,1]],#2[[1]]][[1,1]])&)];
            tmp[[All,3]]=I (Coefficient[#,FR$epsUV,1]&)/@(tmpCT[[All,3]]/.{Conjugate[FR$deltaZ[{x_,x_},b__]]->FR$deltaZ[{x,x},b]}/.CTSol);
          ];
          ,(*not 2 ext*)
          If[Length[genver[[kkpl]]]===1,
            tmp=(If[FreeQ[tmpCT[[All,1]],#[[1]]],Print["Remove the following tadpole because it cannot be absorded in a tadpole counterterm :"];Print[{#[[1]],#[[3]]}];{#[[1]],#[[2]],0},#]&)/@tmp;
            tmp = DeleteCases[tmp,{_,0,0}];
            tmp[[1;;,3]]=Normal[Series[tmp[[1;;,3]],{FR$Eps,0,0}]];
            tadpolerep=MergeVertList[Expand[(tmp/.FR$Eps->1/inveps)]/.inveps->0,tmpCT];
            tadpolerep = DeleteCases[tadpolerep,_?(FreeQ[#,FR$deltat]&)];
            tadpolerep=(Cases[#,_FR$deltat,\[Infinity]][[1]]->-Coefficient[#[[3]],Cases[#,_FR$deltat,\[Infinity]][[1]],0]/Coefficient[#[[3]],Cases[#,_FR$deltat,\[Infinity]][[1]]]&)/@tadpolerep;
          ];(*if tadpole*)
          tmp[[All,3]]=-I*(Simplify[Coefficient[#/.FR$Eps->1/inveps,inveps,1]/FR$Eps,TimeConstraint->1]&)/@tmp[[All,3]];
        ];
        tmpCT[[All,3]]=I*(Simplify[#/.{Conjugate[FR$deltaZ[{x_,x_},b__]]->FR$deltaZ[{x,x},b]},TimeConstraint->1]&)/@tmpCT[[All,3]];
(*if 2 ext*),
(*Length[tmp]=0*)
        If[Length[tmpCT]>0&&Length[genver[[kkpl]]]===2,
          tmpCT[[All,3]]=(ReplaceRepeated[#/.rl1/.tdrl/.IndexSum->Sum/.rlf,SumOver[ind_?((FreeQ[#,Gluon]&&FreeQ[#,Colour])&),x_Integer]*a__:>
                         dum1 Total[Table[(Times[a])/.{ind->kkpl},{kkpl,1,x}]]]&)/@tmpCT[[All,3]];
          
          CTSol=Join[CTSol,SolveDelta[tmpCT,{},genver[[kkpl]],assumpt,comas]];
        ];
      ];(*end If UVCT*)
    ];(*end if Length[tmp]>0*)
    Print["Simplification of the vertices finished after "<>ToString[SessionTime[]-temp]];
    
    (*remove sign from fermion ordering in the amplitude*)
    nF=Count[genver[[kkpl]],F]/2;
    If[nF>1,
      For[lil=1,lil<=Length[tmp],lil++,
         If[Head[ExpandAll[tmp[[lil,3]],TensDot]]===Plus,
           tmp[[lil,3]]=(If[Sort[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]!=Table[Index[Spin,Ext[kik]],{kik,2nF}],
                              Print[Style["Error:Wrong Spin indices",Red]],
                              FR$4F[(-1)^Floor[nF/2]Signature[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]]*#,
                              Print["Error:Wrong Spin indices",Red]]&)/@ExpandAll[tmp[[lil,3]],TensDot];,
             If[tmp[[lil,3]]=!=0,
                tmp[[lil,3]]=(If[Sort[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]!=Table[Index[Spin,Ext[kik]],{kik,2nF}],
                              Print[Style["Error:Wrong Spin indices",Red]],
                               FR$4F[(-1)^Floor[nF/2]Signature[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]]*#,
                               Print["Error:Wrong Spin indices",Red]]&)[ExpandAll[tmp[[lil,3]],TensDot]];
             ];
            ];
          If[Head[ExpandAll[tmp[[lil,2]],TensDot]]===Plus,
            tmp[[lil,2]]=(If[Sort[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]!=Table[Index[Spin,Ext[kik]],{kik,2nF}],
                              Print[Style["Error:Wrong Spin indices",Red]],
                               FR$4F[(-1)^Floor[nF/2]Signature[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]]*#,
                               Print["Error:Wrong Spin indices",Red]]&)/@ExpandAll[tmp[[lil,2]],TensDot];,
             If[tmp[[lil,2]]=!=0,
               tmp[[lil,2]]=(If[Sort[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]!=Table[Index[Spin,Ext[kik]],{kik,2nF}],
                              Print[Style["Error:Wrong Spin indices",Red]],
                               FR$4F[(-1)^Floor[nF/2]Signature[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]]*#,
                              Print["Error:Wrong Spin indices",Red]]&)[ExpandAll[tmp[[lil,2]],TensDot]];
                ];
            ];
        ];
      For[lil=1,lil<=Length[tmpCT],lil++,
         If[Head[ExpandAll[tmpCT[[lil,3]],TensDot]]===Plus,
           tmpCT[[lil,3]]=(If[Sort[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]!=Table[Index[Spin,Ext[kik]],{kik,2nF}],
                              Print[Style["Error:Wrong Spin indices",Red]],
                              FR$4FCT[(-1)^Floor[nF/2]Signature[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]]*#,
                              Print["Error:Wrong Spin indices",Red]]&)/@ExpandAll[tmpCT[[lil,3]],TensDot];,
            If[tmpCT[[lil,3]]=!=0,
                tmpCT[[lil,3]]=(If[Sort[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]!=Table[Index[Spin,Ext[kik]],{kik,2nF}],
                              Print[Style["Error:Wrong Spin indices",Red]],
                               FR$4FCT[(-1)^Floor[nF/2]Signature[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]]*#,
                               Print["Error:Wrong Spin indices",Red]]&)[ExpandAll[tmpCT[[lil,3]],TensDot]];
                ];
            ];
          If[Head[ExpandAll[tmpCT[[lil,2]],TensDot]]===Plus,
            tmpCT[[lil,2]]=(If[Sort[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]!=Table[Index[Spin,Ext[kik]],{kik,2nF}],
                              Print[Style["Error:Wrong Spin indices",Red]],
                               FR$4FCT[(-1)^Floor[nF/2]Signature[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]]*#,
                               Print["Error:Wrong Spin indices",Red]]&)/@ExpandAll[tmpCT[[lil,2]],TensDot];,
             If[tmpCT[[lil,2]]=!=0,
             Print["fermion flip"];
             Print[InputForm[tmpCT[[lil,2]]]];
               tmpCT[[lil,2]]=(If[Sort[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]!=Table[Index[Spin,Ext[kik]],{kik,2nF}],
                              Print[Style["Error:Wrong Spin indices",Red]],
                               FR$4FCT[(-1)^Floor[nF/2]Signature[Cases[#,Index[Spin,Ext[a_]],\[Infinity]]]]*#,
                              Print["Error:Wrong Spin indices",Red]]&)[ExpandAll[tmpCT[[lil,2]],TensDot]];
                ];
            ];
        ];
        Print[InputForm[tmp/.FR$4F[_]->0]];Print[InputForm[tmpCT/.FR$4FCT[_]->0]];
   ];


    vertlist=Join[vertlist,tmp];
    
    If[UVCT,vertCTlist=Join[vertCTlist,tmpCT];];
    Print["Vertices "<>ToString[genver[[kkpl]]]<>" finished after "<>ToString[SessionTime[]-temp]];


  ];

(*for the MZ scheme first in the zero momentum scheme*)
DPrint["mz scheme 1"];
If[mzs,
  zerom2 = If[FreeQ[zerom,aEWM1],Append[zerom,{aEWM1,{e,A,-e}/.FR$ClassesTranslation}],
    Append[DeleteCases[zerom,_?(Not[FreeQ[#,aEWM1]]&)],{aEWM1,{e,A,-e}/.FR$ClassesTranslation}]];,
  zerom2 = zerom;
];
DPrint["mz scheme 1 done"];

(*zero momentum*)
zmsol={};
If[UVCT,
  Attributes[Orderl]={Orderless};
  For[kct=1,kct<=Length[zerom2],kct++,
    pos=Position[Orderl@@@DeleteCases[DeleteCases[vertCTlist[[All,1,All,1]],Index[a_?(FreeQ[zerom2,#]&),b__],\[Infinity]],{},\[Infinity]],Orderl@@zerom2[[kct,2]]];
    zmF=DeleteCases[zerom2[[kct,2]],_?(FreeQ[#,F]&)]/.(Reverse/@FR$ClassesTranslation)/.-x_->x;
    If[Not[Length[zmF]==2],Print[Style["Warning : the vertex "<>ToString[zerom[[kct,2]]]<>"do not contain two fermions",Orange]];];
    If[Length[pos]>0,
      pos=pos[[1,1]];
      If[FreeQ[vertCTlist[[pos,3]],zerom2[[kct,1]]],
        Print[Style["Warning : the vertex "<>ToString[kct]<>" does not depends on FR$delta[{"<>ToString[zerom2[[kct,1]]]<>"},{}] which finite part is then set to zero",Orange]],
        zmsol=Join[zmsol,Solve[(Coefficient[vertCTlist[[pos,3]]/.FR$deltaZ[zmF,x__]->0, TensDot[Ga[Index[Lorentz, Ext[3]]], ProjP][Index[Spin, Ext[1]], Index[Spin, Ext[2]]]]/.CTSol)==0,FR$delta[{zerom2[[kct,1]]},{}]][[1]]];
        If[Not[FreeQ[Last[zmsol],Lorentz]&&FreeQ[Last[zmsol],SP]&&FreeQ[Last[zmsol],Spin]],
          Print[Style["Error : Solution for "<>ToString[zerom2[[kct,1]]]<>" counterterm depends on the kinematic, finite part is set to zero",Red]];
Print[InputForm[Simplify[(vertCTlist[[pos,3]]/.FR$deltaZ[zmF,x__]->0)/.CTSol,TimeConstraint->10]]];
Print[InputForm[Simplify[(vertCTlist[[pos,3]]/.FR$deltaZ[zmF,x__]->0),TimeConstraint->10]]];
          zmsol=Join[Delete[zmsol,-1],FR$delta[{zerom2[[kct,1]]},{}]->0];
        ];
        If[FreeQ[Last[zmsol],Rule],
          Print[Style["Error : No solution for "<>ToString[zerom2[[kct,1]]]<>" counterterm, finite part is set to zero",Red]];
          zmsol=Join[Delete[zmsol,-1],FR$delta[{zerom2[[kct,1]]},{}]->0];
        ];
      ],
      Print[Style["warning : vertex "<>ToString[kct]<>" not found in the list of counterterms vertices, finite part of the counterterm of the parameter will set to zero",Orange]];
    ]
  ]
];
zmsol=(#[[1]]->Coefficient[#[[2]]*FR$Eps,FR$Eps]&)/@zmsol;

(*for the MZ scheme now remove the photon se at MZ and subtract only light degree of freedom as massless for the zero mom*)
DPrint["mz scheme 2"];
If[mzs, zmsol[[-1,2]]=FR$xxx*(zmsol[[-1,2]]/.IPL[a_]:>0/;(Not[FreeQ[{u,d,s,c,b,e,mu,ta}/.FR$ClassesTranslation,a[[1]]/.{}->Sequence[]]]))-(aEWM1*FR$photonSE)];
DPrint["mz scheme 2 done"];

(*DPrint[InputForm[zmsol]];DPrint[InputForm[CTSol]];*)

(*force delta m = 0 when m=0 *)
CTSol=CTSol/.Rule[FR$delta[{x_},{}],y_]:>Rule[FR$delta[{x},{}],Expand[y*Simplify[If[x==0,0,1],Assumptions->assumpt]]]/.{If[a_,1,0]*If[a_,0,1]->0,If[a_,0,1]^2->If[a,0,1]};

If[susy==="OS", Block[{tmpCTsol,mixingfields,renocst,allMLRcst},
  tmpCTsol=Cases[CTSol/.Rule->myr,myr[FR$deltaZ[_List?(#[[1]]=!=#[[2]]&),{{}}],_]]/.myr->Rule;
  tmpCTsol=Select[tmpCTsol,((#[[1,1]]/.FR$ClassesTranslation/.S[_]->S)==={S,S})&];
  CTSol=Complement[CTSol,tmpCTsol];
  tmpCTsol=GatherBy[tmpCTsol,Sort[#[[1,1]]]&];
  (* all mixing fields *)
  mixingfields=Sort/@tmpCTsol[[All,1,1,1]];
  (* all off diagonal mass RC *)
  renocst=Select[vertCTlist,FreeQ[#[[1]],F[__]]&& FreeQ[#[[1]],V[__]]&]/.{{-S[arg_,__],_}->S[arg],{S[arg_,__],_}->S[arg]}; 
  renocst=Select[renocst/.(FR$ClassesTranslation/.Rule->dZrule/.dZrule[a_,b_]:>Rule[b,a]),MemberQ[mixingfields,Sort[#[[1]]]]&];
  renocst=GatherBy[renocst,Sort[#[[1]]]&];
  renocst={#[[1,1]],DeleteDuplicates[Cases[#,_FR$delta,\[Infinity]]]}&/@renocst;
  allMLRcst=renocst[[All,2,1]];
  CTSol=CTSol/.(Rule[#,0]&/@allMLRcst);
  tmpCTsol=tmpCTsol/.(Rule[#,0]&/@allMLRcst);
  renocst=Rule[
      #[[2,1]],
      (FR$deltaZ[#[[1]],{{}}]-FR$deltaZ[Reverse[#[[1]]],{{}}])/4*
        Subtract@@Flatten[(Block[{mysym=#},Mass^2/.(Select[(M$ClassesDescription),#[[1]]===mysym&]/.Equal[_,a_]:>a)]&/@(renocst[[1,1]]/.FR$ClassesTranslation))]
  ]&/@renocst;
  renocst=renocst/.Flatten[tmpCTsol,1];
  (* the off-diagonal delta Z *)
  tmpCTsol=Block[{sum=Expand[1/2Plus@@#[[All,2]]]},#/.Rule[a_,b_]:>Rule[a,sum]]&/@tmpCTsol;
  (* Putting everything together *)
  CTSol=Join[CTSol,Flatten[tmpCTsol,1],Flatten[renocst,1]];
]];
(*Print[InputForm[CTSol]];*)

If[ctpa,CTSol=Join[CTSol,zmsol,tadpolerep]; zmsol={};tadpolerep={}; CTSol=ToParam/@CTSol;
  If[comas,
    CTparam=Join[CTparam,(ToExpression[ToString[#1]<>"CMSConj"]->CMSConj[#2]&)@@@Cases[CTparam,_?(Not[FreeQ[Cases[CTSol,_?(Not[FreeQ[#,FR$deltaZ]]&)][[1;;,2]],#[[1]]]]&)]];
    CTSol=Join[(CMSConj[#1]->ToExpression[ToString[#2]<>"CMSConj"]&)@@@Cases[CTSol,_?(Not[FreeQ[#,FR$deltaZ]]&)],CTSol];
    Print[InputForm[CTSol]];Print[InputForm[CTparam]];
  ];
];

Print["before merge "<>ToString[SessionTime[]-temp]<>" temp="<>ToString[temp]];
(*after vertCTlist below /.{FR$delta[{MB}, {}]->ymbMSbar*FR$delta[{MB}, {}],FR$delta[{MT}, {}]->ymtMSbar*FR$delta[{MT}, {}]} (*for the 2HDM with ymb in the MSbar*)*)
vertlist=MergeVertList[vertlist,If[UVCT,(vertCTlist//.Dispatch[Join[CTSol,zmsol,tadpolerep]/.FR$epsUV->0]),{}]];
Print["merging done "<>ToString[SessionTime[]-temp]];

check=Union[Cases[vertlist,_FR$delta,\[Infinity]]];
If[Length[check]>0,Print[Style["The finite part of those counterterms will be set to zero",Orange]];Print[check];
  vertlist=vertlist/.((Rule[#,0]&)/@check);
];
(*Print[InputForm[Cases[vertlist,{a_?(Not[FreeQ[#,9]]&&FreeQ[#,S[2]]&&FreeQ[#,S[3]]&),b__}]]];*)
vertlist=vertlist//.Eps[a___,FourVector[FourMomentum[Incoming,x_]],b___]:>Module[{mu},FV[x,Index[Lorentz,mu]]Eps[a,Index[Lorentz,mu],b]];
DPrint[InputForm[Union[Cases[vertlist,_Eps,\[Infinity]]]]];
vertlist/.{SUT->SUNT,SUF->SUNF,IndexDel->IndexDelta,FourMomentum[Incoming,xx_]->xx}
]


(* ::Subsubtitle:: *)
(*WriteCT*)


WriteCT::usage="Write the list of the R2 and UV vertices of the model and generic model in the file named modelname.nlo. The options are Output, LabelInternal, KeptIndices, QCDOnly,
ZeroMom, Assumptions, MSbar, ComplexMass, CTparameters, Exclude4ScalarsCT, IsFeynmanGauge, EvenOnly, MaxDim, GenericVertexList, MixingOnly."
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
IsFeynmanGauge::usage="Option of WriteCT. It should be True if the model is in the Feynman gauge and False otherwise."
EvenOnly::usage="Option of WriteCT. It should be True if all the higher dimensional operators have even dimension and False otherwise."
MaxDim::usage="Option of WriteCT. The maximum of the dimension of the operators in the Lagrangian."
GenericVertexList::usage="Option of WriteCT. List of generic vertices for which the R2 and UV counterterms have to be computed."
MixingOnly::usage="Option of WriteCT. Only the mixing are renormalised on-shell."
MZscheme::usage="Option of WriteCT. Renormalized aEWM1 in the MZ scheme (remove SM light fermion contribution to the photon self energy assuming the fermions lighter that the Z are named u,d,s,c,b,e,mu,ta)"
FADirectory::usage="Give the path to the FeynArts directory for the parallel evaluation"
MaxPropagators::usage="Give the maximum number of propagators allowed in the loop"


Options[WriteCT] = {LabelInternal -> True, KeptIndices->{},QCDOnly->False,ZeroMom->{},Assumptions->{},MSbar->False,ComplexMass->False,CTparameters->False, 
Exclude4ScalarsCT->False, Output->Automatic, IsFeynmanGauge->True, EvenOnly->False, MaxDim->4, GenericVertexList->Automatic,SupersymmetryScheme->MR$Null,
MixingOnly->False,MZscheme->False,FADirectory->None,MaxPropagators->100};


WriteCT[mod_,gen_String:"Lorentz", OptionsPattern[]]:=Block[{NC$totlist,outfile,LabInt,KeptInd,NC$qcd,r2list,uvlist,InvEps,ipl1m,NC$wsp,NC$info,NC$zerom,
assume,uvos,NC$cms,CTparam,NC$ctp,no4S,NC$out,feynmang,evenO,mdimen,genvertl,susy,mixo,mzsch,FAdirect},

  LabInt=OptionValue[LabelInternal];
  KeptInd=OptionValue[KeptIndices];
  NC$qcd=OptionValue[QCDOnly];
  NC$zerom=OptionValue[ZeroMom];
  assume=OptionValue[Assumptions];
  uvos=Not[OptionValue[MSbar]];
  NC$cms=OptionValue[ComplexMass];
  NC$ctp=OptionValue[CTparameters];
  no4S=OptionValue[Exclude4ScalarsCT];
  NC$out=OptionValue[Output];
  feynmang=OptionValue[IsFeynmanGauge];
  evenO=OptionValue[EvenOnly];
  mdimen=OptionValue[MaxDim];
  genvertl=OptionValue[GenericVertexList];
  susy=OptionValue[SupersymmetryScheme];
  mixo=OptionValue[MixingOnly];
  mzsch=OptionValue[MZscheme];
  FAdirect=OptionValue[FADirectory];
  maxpropa=OptionValue[MaxPropagators];

If[Head[mixo]===List,NLO$mix=Sort/@mixo;];


If[NC$cms,NLO$CMS=True;,NLO$CMS=False;];

  UV$Wftlist = {};
  CTparam={};

  NC$totlist=ModelR2[mod,gen,LabInt,KeptInd,NC$qcd,NC$zerom,assume,uvos,NC$cms,NC$ctp,no4S,feynmang,evenO,mdimen,genvertl,susy,mixo,mzsch,FAdirect,maxpropa];

  r2list = Transpose[Delete[Transpose[NC$totlist],{3}]];
  uvlist = Transpose[Delete[Transpose[NC$totlist],{2}]](*/.FR$IR->1*);
  r2list=DeleteCases[r2list,{a_,0},1];
  uvlist=DeleteCases[uvlist,{a_,0},1]/.FR$Eps->-2FR$Eps;(*change from d=4+eps to d=4-2eps*)
  CTparam = CTparam/.FR$Eps->-2FR$Eps(*/.FR$IR->1*)/.{SUT->SUNT,SUF->SUNF,IndexDel->IndexDelta};

  uvlist=uvlist/.(Rule[-Append[#2,xx___],anti[#1]]&)@@@FR$ClassesTranslation/.(Rule[Append[#2,xx___],#1]&)@@@FR$ClassesTranslation;
  r2list=r2list/.(Rule[-Append[#2,xx___],anti[#1]]&)@@@FR$ClassesTranslation/.(Rule[Append[#2,xx___],#1]&)@@@FR$ClassesTranslation;
  CTparam = CTparam/.(Rule[-Append[#2,xx___],anti[#1]]&)@@@FR$ClassesTranslation/.(Rule[Append[#2,xx___],#1]&)@@@FR$ClassesTranslation;

  FR$InteractionOrderPerturbativeExpansion=If[NC$qcd,FR$InteractionOrderPerturbativeExpansion/.{{QCD,0}->{QCD,1}},FR$InteractionOrderPerturbativeExpansion/.{0->1}];
  
  NC$wsp="                                                                                                                             ";
  NC$info = (ToString[#1]<>" : "<>If[Head[#2]===List,StringDrop[StringJoin[(#<>", "&)/@#2],-2],#2]&)@@@FR$ModelInformation;
  If[NC$out===Automatic,NC$out=ModelName/.FR$ModelInformation;];
  outfile=NC$out<>".nlo";
  Print["Writing R2 and UV counterterms from "<>mod<>" and "<>gen<>" in "<>outfile];
  OpenWrite[outfile];
  WriteString[outfile,"(****************************************************************************************************************)\n"];
  WriteString[outfile,"(* Model automalically generated by NLOCT-"<>StringTake[NLO$Version<>NC$wsp,70]<>"*)\n"];
  WriteString[outfile,"(* date and time of generation : "<>StringTake[DateString[]<>NC$wsp,79]<>"*)\n"];
  WriteString[outfile,"(* FeynRules model information : "<>StringTake[NC$wsp,79]<>"*)\n"];
  (WriteString[outfile,"(*   "<>StringTake[#<>NC$wsp,106]<>" *)\n"]&)/@NC$info;
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

  WriteString[outfile,"\n"];
  WriteString[outfile,"(*Evanescent operators*)\n"];
  WriteString[outfile,"\n"];
  WriteString[outfile,"FR$EvaOp = {\n"];
  NLO$EvaList=NLO$EvaList/.FR$Eps->-2FR$Eps;
  If[Length[NLO$EvaList]>0,
    (WriteString[outfile,ToString[InputForm[#]]<>",\n"]&)/@(NLO$EvaList[[1;;Length[NLO$EvaList]-1]]);
    WriteString[outfile,ToString[InputForm[Last[NLO$EvaList]]]<>"};\n"];
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


(* ::Subsubtitle::Closed:: *)
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
SUT[Index[Gluon,b_],Index[Gluon,a_],Index[Gluon,c_]]:=(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]-
I/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]])/;ILQ[a,c]&&ILQ[a,b]&&ILQ[b,c];
SUT[Index[Gluon,c_],Index[Gluon,b_],Index[Gluon,a_]]:=(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]-
I/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]])/;ILQ[a,c]&&ILQ[a,b]&&ILQ[b,c];
SUT[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,b_]]:=(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]-
I/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]])/;ILQ[a,c]&&ILQ[a,b]&&ILQ[b,c];

SUT[Index[Gluon,b_],Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,d_]]:=SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[a,d];
SUT[Index[Gluon,d_],Index[Gluon,b_],Index[Gluon,a_],Index[Gluon,c_]]:=SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[a,d];
SUT[Index[Gluon,c_],Index[Gluon,d_],Index[Gluon,b_],Index[Gluon,a_]]:=SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]/;ILQ[a,c]&&ILQ[a,b]&&ILQ[a,d];

SUT[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,c_],Index[Gluon,b_]]:=(-1/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]-
SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]+
SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]])/;ILQ[a,b]&&ILQ[b,c]&&ILQ[c,d];

SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,d_],Index[Gluon,c_]]:=(1/2 SUF[Index[Gluon,a],Index[Gluon,c],Index[Gluon,b],Index[Gluon,d]]-
SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,b],Index[Gluon,d]]+
SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]])/;ILQ[a,b]&&ILQ[b,c]&&ILQ[c,d];

(*SUT[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,b_],Index[Gluon,d_]]:=-1/2 SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]]-
SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]+
SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]/;ILQ[a,b]&&ILQ[b,c]&&ILQ[c,d];*)


(* ::Subsubtitle::Closed:: *)
(*SUT*)


SUT/:SUT[a__,Index[Colour,i_],Index[Colour,j_]]*SUT[b__,Index[Colour,j_],Index[Colour,k_]]*SumOver[Index[Colour,j_],n_]:=SUT[a,b,Index[Colour,i],Index[Colour,k]];
SUT/:SUT[a__,Index[Colour,i_],Index[Colour,j_]]*SUT[b__,Index[Colour,j_],Index[Colour,i_]]*SumOver[Index[Colour,j_],n_]:=SUT[a,b]/SumOver[Index[Colour,i],n];

SUT/:SUT[Index[Gluon,a_],Index[Colour,i_],Index[Colour,j_]]*SUT[Index[Gluon,a_],Index[Colour,k_],Index[Colour,l_]]*SumOver[Index[Gluon,a_],n2m1_Integer]:=(
IndexDel[Index[Colour,i],Index[Colour,l]]IndexDel[Index[Colour,k],Index[Colour,j]]/2-IndexDel[Index[Colour,i],Index[Colour,j]]IndexDel[Index[Colour,k],Index[Colour,l]]/2/Sqrt[n2m1+1]);

SUT/:SUT[a__,Index[Colour,i_],Index[Colour,i_]]*SumOver[Index[Colour,i_],n_]:=SUT[a];

SUT/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUT[Index[Gluon,a_],Index[Gluon,a_],Index[Colour,i_],Index[Colour,l_]]:=(n2m1/2/Sqrt[n2m1+1])IndexDel[Index[Colour,i],Index[Colour,l]];

SUT/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,a_],Index[Colour,i_],Index[Colour,l_]]:= 
-1/2/(Sqrt[n2m1+1])  SUT[Index[Gluon,b],Index[Colour,i],Index[Colour,l]];

SUT/:SumOver[Index[Colour,j_],n_Integer]SUT[Index[Gluon,a_],Index[Gluon,a_],Index[Colour,i_],Index[Colour,l_]]:=
1/2(n-1/n)  IndexDel[Index[Colour,i],Index[Colour,l]]/SumOver[Index[Gluon,a],n^2-1];

SUT[Index[Gluon,a_],Index[Colour,i_],Index[Colour,i_]]:=0;

SUT[Index[Colour,i_],Index[Colour,j_]]:=IndexDel[Index[Colour,i],Index[Colour,j]];

SUT[Index[Gluon,a_]]:=0;

SUT/:SUT[x___,Index[Gluon,a_],Index[Gluon,a_],y___]SumOver[Index[Gluon,a_],n2m1_Integer]:= n2m1/(2Sqrt[n2m1+1])SUT[x,y];
SUT/:SUT[x___,Index[Gluon,a_],w__,Index[Gluon,a_],y___]SumOver[Index[Gluon,a_],n2m1_Integer]:=(1/2SUT[w]SUT[x,y]- 1/(2Sqrt[n2m1+1])SUT[x,w,y]);

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
SUF[Sequence@@Sort[{Index[Gluon,e],Index[Gluon,c],Index[Gluon,a]},(ILQ[#1[[2]],#2[[2]]]&)]]/;Not[OrderedQ[{e,c,a},ILQ]]&&a=!=c&&a=!=e&&e=!=c;

SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,a_]]:=0;
SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,a_]]:=0;
SUF[Index[Gluon,a_],Index[Gluon,a_],Index[Gluon,e_]]:=0;


SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:=- SUF[Index[Gluon,e],Index[Gluon,c],Index[Gluon,b],Index[Gluon,a]]/;ILQ[b,a]&&ILQ[e,c]&&ILQ[e,b];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:=- SUF[Index[Gluon,c],Index[Gluon,e],Index[Gluon,a],Index[Gluon,b]]/;ILQ[c,e]&&ILQ[a,b]&&ILQ[c,a];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:= SUF[Index[Gluon,c],Index[Gluon,e],Index[Gluon,b],Index[Gluon,a]]/;ILQ[c,e]&&ILQ[b,a]&&ILQ[c,b];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:= SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,e],Index[Gluon,c]]/;ILQ[a,e]&&ILQ[e,c]&&ILQ[a,b];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:=- SUF[Index[Gluon,b],Index[Gluon,a],Index[Gluon,e],Index[Gluon,c]]/;ILQ[b,e]&&ILQ[e,c]&&ILQ[b,a];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:= SUF[Index[Gluon,b],Index[Gluon,a],Index[Gluon,c],Index[Gluon,e]]/;ILQ[b,c]&&ILQ[c,e]&&ILQ[b,a];
SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]:=- SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,e]]/;ILQ[a,c]&&ILQ[c,e]&&ILQ[a,b];


SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUF[Index[Gluon,d_],Index[Gluon,a_],Index[Gluon,c_]]*SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,f_]]:=
SUF[Index[Gluon,c],Index[Gluon,d],Index[Gluon,f],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUF[Index[Gluon,d_],Index[Gluon,c_],Index[Gluon,a_]]*SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,f_]]:=
-SUF[Index[Gluon,c],Index[Gluon,d],Index[Gluon,f],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUF[Index[Gluon,d_],Index[Gluon,c_],Index[Gluon,a_]]*SUF[Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,a_]]:=
SUF[Index[Gluon,c],Index[Gluon,d],Index[Gluon,f],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,c_]]*SUF[Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,a_]]:=
SUF[Index[Gluon,c],Index[Gluon,d],Index[Gluon,f],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,c_]]*SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,f_]]:=
SUF[Index[Gluon,c],Index[Gluon,d],Index[Gluon,f],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,c_]]*SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,f_]]:=
-SUF[Index[Gluon,c],Index[Gluon,d],Index[Gluon,f],Index[Gluon,e]];


SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]*
SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,b_]]:=(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]*SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,b_]]:=
(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,b_]]*SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,b_]]:=
-(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]*
SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,e_]]:=(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]*SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,e_]]:=
(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,b_]]*SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,e_]]:=
-(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,b_]]*
SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,b_]]:=-(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]*SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,b_]]:=
-(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,b_]]*SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,b_]]:=
(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1];


SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,a_]]:=
(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,a_]]:=
-(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,a_],Index[Gluon,e_]]:=
-(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]];

SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]SUF[Index[Gluon,a_],Index[Gluon,c_],Index[Gluon,a_],Index[Gluon,e_]]:=
(Sqrt[n2m1+1])*IndexDel[Index[Gluon,c],Index[Gluon,e]];


SUF/:SumOver[Index[Gluon,a_],n2m1_Integer]*SUF[Index[Gluon,d_],Index[Gluon,a_],Index[Gluon,b_]]*SUF[Index[Gluon,e_],Index[Gluon,a_],Index[Gluon,c_]]*
SUF[Index[Gluon,f_],Index[Gluon,b_],Index[Gluon,c_]]:=-3/2 SUF[Index[Gluon,d],Index[Gluon,f],Index[Gluon,e]]/SumOver[Index[Gluon,b],n2m1]/SumOver[Index[Gluon,c],n2m1];


SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_], Index[Gluon, d_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[ Index[Gluon, d_], Index[Gluon, c_],Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[ Index[Gluon, d_],Index[Gluon, c_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_], Index[Gluon, d_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, e_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[ Index[Gluon, d_], Index[Gluon, c_],Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, e_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[ Index[Gluon, d_],Index[Gluon, c_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[ Index[Gluon, d_],Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[ Index[Gluon, d_], Index[Gluon, c_],Index[Gluon, e_]]*SUF[Index[Gluon, d_],Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, d_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[ Index[Gluon, d_], Index[Gluon, c_],Index[Gluon, e_]]*SUF[Index[Gluon, d_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];


SUF/:SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,h_]]*
SUF[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,f_],Index[Gluon,h_]]*SumOver[Index[Gluon,e_],n2m1_Integer]:=-3/2*
SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]/SumOver[Index[Gluon,f],n2m1]/SumOver[Index[Gluon,h],n2m1];

SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, g_]]*
SUF[ Index[Gluon, b_], Index[Gluon, g_], Index[Gluon, e_]]*SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 *
SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1]/SumOver[Index[Gluon, g], n2m1];


SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, d_], Index[Gluon, e_],Index[Gluon, f_],Index[Gluon, c_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_], Index[Gluon, d_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, d_], Index[Gluon, e_],Index[Gluon, f_],Index[Gluon, c_]]*SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_], Index[Gluon, d_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];


SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, d_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, d_], Index[Gluon, e_],Index[Gluon, f_],Index[Gluon, c_]]*SUF[Index[Gluon, d_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, d_], Index[Gluon, e_],Index[Gluon, f_],Index[Gluon, c_]]*SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];


SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, e_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, d_], Index[Gluon, e_],Index[Gluon, f_],Index[Gluon, c_]]*SUF[Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, e_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, d_], Index[Gluon, e_],Index[Gluon, f_],Index[Gluon, c_]]*SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];


SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, d_], Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, d_], Index[Gluon, e_],Index[Gluon, f_],Index[Gluon, c_]]*SUF[Index[Gluon, d_], Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, f_],Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

SUF/:SUF[Index[Gluon, d_], Index[Gluon, e_],Index[Gluon, f_],Index[Gluon, c_]]*SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, b_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:=-3/2 SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon, f],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];


SUF/:SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,b_],Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_],n2m1_Integer]:= Sqrt[n2m1+1]/SumOver[Index[Gluon,f],n2m1]SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]];

SUF/:SUF[Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,d_]]*SUF[Index[Gluon,b_],Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_],n2m1_Integer]:= Sqrt[n2m1+1]/SumOver[Index[Gluon,f],n2m1]SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]];

SUF/:SUF[Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,d_]]*SUF[Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,b_],Index[Gluon,c_]]*
SumOver[Index[Gluon,e_],n2m1_Integer]:= Sqrt[n2m1+1]/SumOver[Index[Gluon,f],n2m1]SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]];


SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, e_]]*SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_], Index[Gluon, e_]]*
  SumOver[Index[Gluon, d_], n2m1_Integer]:= Sqrt[n2m1+1]SUF[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]/SumOver[Index[Gluon, e], n2m1];

(*SUF/:SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,b_],Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_],n2m1_Integer]:= 3/2/SumOver[Index[Gluon,f],n2m1]SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,d_],Index[Gluon,f_],Index[Gluon,e_]]*SUF[Index[Gluon,b_],Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_],n2m1_Integer]:=-3/2/SumOver[Index[Gluon,f],n2m1]SUF[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]];*)


(*with SUT*)

SUF/:SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,h_]]*
SUF[Index[Gluon,b_],Index[Gluon,f_],Index[Gluon,c_],Index[Gluon,h_]]*SumOver[Index[Gluon,e_],n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]*
IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+
IndexDel[Index[Gluon,a],Index[Gluon,b]]*IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*(SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]+
SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]))/(SumOver[Index[Gluon,f],n2m1]*SumOver[Index[Gluon,h],n2m1]);

SUF/:SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,b_],Index[Gluon,g_],Index[Gluon,h_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,g_]]*
SUF[Index[Gluon,d_],Index[Gluon,f_],Index[Gluon,h_]]*SumOver[Index[Gluon,e_],n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]*
IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+
IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*(SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,b],Index[Gluon,c]]+
SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,b],Index[Gluon,d]]))/(SumOver[Index[Gluon,f],n2m1]*SumOver[Index[Gluon,g],n2m1]*SumOver[Index[Gluon,h],n2m1]);

(*4 indices SUNF*)

SUF/:SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, b_], Index[Gluon, f_]]*SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, f_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,f_],Index[Gluon,b_],Index[Gluon,e_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,d_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, f_], Index[Gluon, b_]]*SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, f_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,f_],Index[Gluon,e_],Index[Gluon,b_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,d_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];

SUF/:SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, f_]]*SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, f_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

SUF/:SUF[Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,e_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,d_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];

SUF/:SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, f_], Index[Gluon, b_]]*SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, d_], Index[Gluon, f_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

SUF/:SUF[Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,b_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,d_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, e_], Index[Gluon, f_], Index[Gluon, b_]]*SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, f_], Index[Gluon, d_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

SUF/:SUF[Index[Gluon,a_],Index[Gluon,f_],Index[Gluon,e_],Index[Gluon,b_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,d_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];

SUF/:SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, f_]]*SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, f_], Index[Gluon, d_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

SUF/:SUF[Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,e_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,d_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];

SUF/:SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, f_], Index[Gluon, b_]]*SUF[Index[Gluon, c_], Index[Gluon, e_], Index[Gluon, f_], Index[Gluon, d_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

SUF/:SUF[Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,b_]]*SUF[Index[Gluon,c_],Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,d_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];

SUF/:SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, f_]]*SUF[Index[Gluon, e_], Index[Gluon, c_], Index[Gluon, f_], Index[Gluon, d_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

(*SUF/:SUF[Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,e_]]*SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,f_],Index[Gluon,d_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];*)

SUF/:SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, f_], Index[Gluon, b_]]*SUF[Index[Gluon, e_], Index[Gluon, c_], Index[Gluon, f_], Index[Gluon, d_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

(*SUF/:SUF[Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,b_]]*SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,f_],Index[Gluon,d_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];*)

SUF/:SUF[Index[Gluon, e_], Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, f_]]*SUF[Index[Gluon, e_], Index[Gluon, c_], Index[Gluon, d_], Index[Gluon, f_]]*
SumOver[Index[Gluon, e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,c],Index[Gluon,d],Index[Gluon,b]]+SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,d],Index[Gluon,c]]))/SumOver[Index[Gluon, f], n2m1];

(*SUF/:SUF[Index[Gluon,f_],Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,e_]]*SUF[Index[Gluon,e_],Index[Gluon,c_],Index[Gluon,d_],Index[Gluon,f_]]*
SumOver[Index[Gluon,e_], n2m1_Integer]:=(IndexDel[Index[Gluon,a],Index[Gluon,c]]IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]*
IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,b]]IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*
(SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]+SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]))/SumOver[Index[Gluon,f], n2m1];*)


SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon,c_], Index[Gluon, d_]]*SUT[Index[Gluon, b_],Index[Gluon, d_], Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=(Sqrt[n2m1+1]/2 SUT[Index[Gluon, a],Index[Gluon, c], Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, a],Index[Gluon, c]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, c_], Index[Gluon, d_]]*SUT[Index[Gluon, d_], Index[Gluon, b_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=(Sqrt[n2m1+1]/2 SUT[Index[Gluon, c], Index[Gluon, a],Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, c], Index[Gluon, a]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];

SUF/:SUF[Index[Gluon, b_], Index[Gluon, a_], Index[Gluon,c_], Index[Gluon, d_]]*SUT[Index[Gluon, b_],Index[Gluon, d_], Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=-(Sqrt[n2m1+1]/2 SUT[Index[Gluon, a],Index[Gluon, c], Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, a],Index[Gluon, c]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];

SUF/:SUF[Index[Gluon, b_], Index[Gluon, a_], Index[Gluon, c_], Index[Gluon, d_]]*SUT[Index[Gluon, d_], Index[Gluon, b_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=-(Sqrt[n2m1+1]/2 SUT[Index[Gluon, c], Index[Gluon, a],Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, c], Index[Gluon, a]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon,d_], Index[Gluon, c_]]*SUT[Index[Gluon, b_],Index[Gluon, d_], Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=-(Sqrt[n2m1+1]/2 SUT[Index[Gluon, a],Index[Gluon, c], Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, a],Index[Gluon, c]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_], Index[Gluon, c_]]*SUT[Index[Gluon, d_], Index[Gluon, b_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=-(Sqrt[n2m1+1]/2 SUT[Index[Gluon, c], Index[Gluon, a],Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, c], Index[Gluon, a]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];

SUF/:SUF[Index[Gluon, b_], Index[Gluon, a_], Index[Gluon,d_], Index[Gluon, c_]]*SUT[Index[Gluon, b_],Index[Gluon, d_], Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=(Sqrt[n2m1+1]/2 SUT[Index[Gluon, a],Index[Gluon, c], Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, a],Index[Gluon, c]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];

SUF/:SUF[Index[Gluon, b_], Index[Gluon, a_], Index[Gluon, d_], Index[Gluon, c_]]*SUT[Index[Gluon, d_], Index[Gluon, b_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=(Sqrt[n2m1+1]/2 SUT[Index[Gluon, c], Index[Gluon, a],Index[Colour, i], Index[Colour, j]]+
IndexDel[Index[Gluon, c], Index[Gluon, a]]IndexDel[Index[Colour, i], Index[Colour, j]]/4)/SumOver[Index[Gluon, b], n2m1];


SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, c_], Index[Gluon, d_]]*SUT[Index[Gluon, c_], Index[Gluon, d_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=I Sqrt[n2m1+1]/2 SUT[Index[Gluon, c],Index[Colour, i], Index[Colour, j]]SUF[Index[Gluon, a], Index[Gluon, b], Index[Gluon, c]];

SUF/:SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_], Index[Gluon, c_]]*SUT[Index[Gluon, c_], Index[Gluon, d_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=-I Sqrt[n2m1+1]/2 SUT[Index[Gluon, c],Index[Colour, i], Index[Colour, j]]SUF[Index[Gluon, a], Index[Gluon, b], Index[Gluon, c]];

SUF/:SUF[Index[Gluon, c_], Index[Gluon, d_],Index[Gluon, a_], Index[Gluon, b_]]*SUT[Index[Gluon, c_], Index[Gluon, d_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=I Sqrt[n2m1+1]/2 SUT[Index[Gluon, c],Index[Colour, i], Index[Colour, j]]SUF[Index[Gluon, a], Index[Gluon, b], Index[Gluon, c]];

SUF/:SUF[ Index[Gluon, d_], Index[Gluon, c_], Index[Gluon, a_], Index[Gluon, b_]]*SUT[Index[Gluon, c_], Index[Gluon, d_],Index[Colour, i_], Index[Colour, j_]]*
SumOver[Index[Gluon, d_], n2m1_]:=-I Sqrt[n2m1+1]/2 SUT[Index[Gluon, c],Index[Colour, i], Index[Colour, j]]SUF[Index[Gluon, a], Index[Gluon, b], Index[Gluon, c]];


SUF/:SumOver[Index[Gluon, a_], n2m1_Integer]*SUF[Index[Gluon, c_], Index[Gluon, a_], Index[Gluon, b_]]* 
SUT[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, d_], Index[Colour,i_],Index[Colour,j_]]:= 
 Sqrt[n2m1+1] I SUT[Index[Gluon, c],Index[Gluon, d],Index[Colour,i],Index[Colour,j]]/2/SumOver[Index[Gluon, b], n2m1];

SUF/:SumOver[Index[Gluon, a_], n2m1_Integer]*SUF[Index[Gluon, c_], Index[Gluon, a_], Index[Gluon, b_]]* 
SUT[Index[Gluon, d_], Index[Gluon, b_],Index[Gluon, a_], Index[Colour,i_], Index[Colour,j_]]:= 
- Sqrt[n2m1+1] I SUT[Index[Gluon, d],Index[Gluon, c],Index[Colour,i],Index[Colour,j]]/2/SumOver[Index[Gluon, b], n2m1];

SUF/:SumOver[Index[Gluon, b_],n2m1_Integer]SUF[Index[Gluon, a_],Index[Gluon, b_],Index[Gluon, c_]]*
SUT[Index[Gluon, b_],Index[Gluon, d_],Index[Gluon, c_],Index[Colour,i_],Index[Colour,j_]]:= -I  IndexDel[Index[Gluon, a],Index[Gluon, d]]*
IndexDel[Index[Colour,i],Index[Colour,j]]/4/SumOver[Index[Gluon, c],n2m1];

SUF/:SumOver[Index[Gluon, b_],n2m1_Integer]SUF[Index[Gluon, a_],Index[Gluon, b_],Index[Gluon, c_]]*
SUT[Index[Gluon, c_],Index[Gluon, d_],Index[Gluon, b_],Index[Colour,i_],Index[Colour,j_]]:= I IndexDel[Index[Gluon, a],Index[Gluon, d]]*
IndexDel[Index[Colour,i],Index[Colour,j]]/4/SumOver[Index[Gluon, c],n2m1];

SUF/:SumOver[Index[Gluon, g_], n2m1_Integer]*SUF[Index[Gluon, a_], Index[Gluon, b_], Index[Gluon, g_]]*SUT[Index[Gluon, g_], Index[Colour, i_],Index[Colour, j_]]:=
-I (SUT[Index[Gluon, a],Index[Gluon, b], Index[Colour, i],Index[Colour, j]]-SUT[Index[Gluon, b],Index[Gluon, a], Index[Colour, i],Index[Colour, j]]);

(*new for tensor vvvv*)
SUF/:SUF[Index[Gluon,a_],Index[Gluon,f_],Index[Gluon,e_]]*SUF[Index[Gluon,d_],Index[Gluon,e_],Index[Gluon,h_]]*
SUF[Index[Gluon,b_],Index[Gluon,f_],Index[Gluon,c_],Index[Gluon,h_]]*SumOver[Index[Gluon,e_],n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]*
IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+
IndexDel[Index[Gluon,a],Index[Gluon,b]]*IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*(SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]+
SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]))/(SumOver[Index[Gluon,f],n2m1]*SumOver[Index[Gluon,h],n2m1]);

SUF/:SUF[Index[Gluon,a_],Index[Gluon,e_],Index[Gluon,f_]]*SUF[Index[Gluon,d_],Index[Gluon,h_],Index[Gluon,e_]]*
SUF[Index[Gluon,b_],Index[Gluon,f_],Index[Gluon,c_],Index[Gluon,h_]]*SumOver[Index[Gluon,e_],n2m1_Integer]:=-(IndexDel[Index[Gluon,a],Index[Gluon,c]]*
IndexDel[Index[Gluon,d],Index[Gluon,b]]/2+IndexDel[Index[Gluon,a],Index[Gluon,d]]IndexDel[Index[Gluon,c],Index[Gluon,b]]/2+
IndexDel[Index[Gluon,a],Index[Gluon,b]]*IndexDel[Index[Gluon,c],Index[Gluon,d]]/2+(Sqrt[n2m1+1])*(SUT[Index[Gluon,a],Index[Gluon,d],Index[Gluon,c],Index[Gluon,b]]+
SUT[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c],Index[Gluon,d]]))/(SumOver[Index[Gluon,f],n2m1]*SumOver[Index[Gluon,h],n2m1]);


(* ::Subsubtitle::Closed:: *)
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
dSUN/:dSUN[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]SUF[Index[Gluon,f_],Index[Gluon,b_],Index[Gluon,e_]]SUF[Index[Gluon,a_],Index[Gluon,f_],Index[Gluon,d_]]:=-3/2*
  dSUN[Index[Gluon,c],Index[Gluon,e],Index[Gluon,d]]/SumOver[Index[Gluon,a],8]/SumOver[Index[Gluon,b],8]/SumOver[Index[Gluon,f],8];
dSUN/:dSUN[Index[Gluon,b_],Index[Gluon,a_],Index[Gluon,c_]]SUF[Index[Gluon,e_],Index[Gluon,f_],Index[Gluon,b_]]*SUF[Index[Gluon,d_],Index[Gluon,f_],Index[Gluon,a_]]:=3/2*
  dSUN[Index[Gluon,c],Index[Gluon,e],Index[Gluon,d]]/SumOver[Index[Gluon,a],8]/SumOver[Index[Gluon,b],8]/SumOver[Index[Gluon,f],8];

dSUN/:dSUN[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]SUT[Index[Gluon,a_],Index[Gluon,b_],Index[Colour,i_],Index[Colour,j_]]:=5/6*
SUT[Index[Gluon,c],Index[Colour,i],Index[Colour,j]]/SumOver[Index[Gluon,a],8]/SumOver[Index[Gluon,b],8];


(* ::Subtitle:: *)
(*Protection*)


Protect[ME,SUF,SUT,SUNTr,GetR2,R2Tadpoles,R2BubblesF,R2BubblesQ2,R2FTriangles,R2BTriangles,R2FBTriangles,R2BBoxes,R2FBoxes,ILQ,WriteCT,ModelR2,PartOrder,R2atClass,
SP,FCh,FChDist,DTr,DTrDist,DTr2,fdtr,FV,LCivita,M$dim,red2v,red4v,FR$UV,FR$R2,FR$MU,FR$Eps,MergeVertList,UVwfatClass,AddWf,FR$UV,FR$R2,ReInt,IntxnLog,IntxnLog2,Dp2IntxnLog,Dp2IntxnLog2,
IndexDel,Int,NLO$EO,NLO$EvaA,Sort4FermionChain,EvanescentReduce,NLO$EvaMF,NLO$DiracBasis];
