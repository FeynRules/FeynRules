(* ::Package:: *)

(* ::Title:: *)
(*Miscellaneous*)


(* ::Text:: *)
(*This file contains a set of functions that are need to write down the lagrangian and to calculate the Feynman rules.*)


(* ::Section::Closed:: *)
(*Date*)


FR$DateFormat[] := Block[{date = Date[]},
     ToString[date[[3]]] <> ". " <> ToString[date[[2]]] <> ". " <> ToString[date[[1]]] <> ",    " <> ToString[date[[4]]]<>":" <> ToString[date[[5]]]];


(* ::Section::Closed:: *)
(*NumericalValue*)


(* ::Text:: *)
(*NumericalValue[p] calculates the numerical value of the parameter p from the numerical values specified in the model file.*)


(* ::Subsection:: *)
(*NumericalValue*)


NumericalValue[n_?NumericQ]:=N[n];
NumericalValue[NoValue[1]]=NoValue[1];

NumericalValue[Plus[a_, b__]]:= NumericalValue /@ Plus[a, b];
NumericalValue[Times[a_, b__]]:= NumericalValue /@ Times[a, b];

NumericalValue[Power[a_,b_]]:=Power[NumericalValue[a],NumericalValue[b]];

NumericalValue[Sin[a_]]:=Sin[NumericalValue[a]];
NumericalValue[Cos[a_]]:=Cos[NumericalValue[a]];
NumericalValue[Tan[a_]]:=Tan[NumericalValue[a]];
NumericalValue[Csc[a_]]:=Csc[NumericalValue[a]];
NumericalValue[Sec[a_]]:=Sec[NumericalValue[a]];
NumericalValue[Cot[a_]]:=Cot[NumericalValue[a]];

NumericalValue[Sinh[a_]]:=Sinh[NumericalValue[a]];
NumericalValue[Cosh[a_]]:=Cosh[NumericalValue[a]];
NumericalValue[Tanh[a_]]:=Tanh[NumericalValue[a]];
NumericalValue[Coth[a_]]:=Coth[NumericalValue[a]];

NumericalValue[Log[a_]]:=Log[NumericalValue[a]];

NumericalValue[Exp[a_]]:=Exp[NumericalValue[a]];

NumericalValue[Sqrt[a_]]:=Sqrt[NumericalValue[a]];

NumericalValue[ArcSin[a_]]:=ArcSin[NumericalValue[a]];
NumericalValue[ArcCos[a_]]:=ArcCos[NumericalValue[a]];
NumericalValue[ArcTan[a_]]:=ArcTan[NumericalValue[a]];
NumericalValue[ArcCot[a_]]:=ArcCot[NumericalValue[a]];

NumericalValue[ArcSinh[a_]]:=ArcSinh[NumericalValue[a]];
NumericalValue[ArcCosh[a_]]:=ArcCosh[NumericalValue[a]];
NumericalValue[ArcTanh[a_]]:=ArcTanh[NumericalValue[a]];
NumericalValue[ArcCoth[a_]]:=ArcCoth[NumericalValue[a]];

NumericalValue[Conjugate[a_]] := Conjugate[NumericalValue[a]];

NumericalValue[Abs[x_]] := Abs[NumericalValue[x]];
NumericalValue[Re[x_]] := Re[NumericalValue[x]];
NumericalValue[Im[x_]] := Im[NumericalValue[x]];

NumericalValue[tt_?(TensQ)[ind__?(Not[FreeQ[{##}, Index[__]]]&)]] := NumericalValue[tt[ind] //. Index[_, i_] :> i];
NumericalValue[tt_?(NoTensQ)[ind__?(Not[FreeQ[{##}, Index[__]]]&)]] := NumericalValue[tt[ind] //. Index[_, i_] :> i];

SetAttributes[NumericalValue, Listable];

NumericalValue[ff_?FieldQ]:=ff;

NumericalValue[Abs[x_]] := Abs[NumericalValue[x]];



(* ::Section:: *)
(*Dot*)


(* ::Text:: *)
(*Added 31.08 .09.*)


Unprotect[Dot];

SetAttributes[Dot,Flat]

Dot[___, 0, ___] := 0;

Dot[xx___, kk_?(numQ[#] === True &)* zz_, yy___] := kk Dot[xx, zz,yy];


Ga[Index[Lorentz, 0], inds___]:= Ga[0, inds];

Dot[Ga[0],field_][s_, inds___] := Module[{r}, Ga[0,s,r]field[r,inds]];

Dot[field_ ,Ga[0]][s_, inds___] := Module[{r}, Ga[0,r,s]field[r,inds]];
TensDot[xx___,Dot[y1_,yy___],zz___]:=TensDot[xx,y1,yy,zz];

Dot[xx___,Ga[0],Ga[0],yy___]:=Dot[xx,yy];
TensDot[xx___,Ga[0],Ga[0],yy___]:=TensDot[xx,yy];

Dot[xx___, gg_?(GammaMatrixQ[#] === True &), tt_?(GammaMatrixQ[#] =!= True && TensQ[#]===True&),yy___]:=Dot[xx,tt,gg,yy];
TensDot[xx___, gg_?(GammaMatrixQ[#] === True &), tt_?(GammaMatrixQ[#] =!= True&& TensQ[#]===True&),yy___]:=TensDot[xx,tt,gg,yy];

(*del[Dot[field_?(FieldQ[#]===True&),Ga[0]],mu_] := Dot[del[field,mu],Ga[0]]
del[Dot[Ga[0]],field_?(FieldQ[#]===True&),mu_] := Dot[Ga[0],del[field,mu]]*)
(* CD, 26.10.10: changed Dot[yy, Ga[0]]  to Dot[Ga[0],yy]*)
Ga/:Dot[xx__,Ga[0],yy_?(FreeQ[#,_?GammaMatrixQ]&),zz__]:=Dot[xx,yy,Ga[0],zz]

Dot/: Dot[Ga[0],x1_,xx___]+Dot[Ga[0],y1_,yy___]:=Dot[Ga[0],Dot[x1,xx]+Dot[y1,yy]]
Dot/: Dot[Ga[0],x1_,xx___]-Dot[Ga[0],y1_,yy___]:=Dot[Ga[0],Dot[x1,xx]-Dot[y1,yy]]
Dot/: Dot[x1_,xx___,Ga[0]]+Dot[y1_,yy___]:=Dot[Dot[x1,xx]+Dot[y1,yy],Ga[0]]
Dot/: Dot[x1_,xx___,Ga[0]]-Dot[y1_,yy___,Ga[0]]:=Dot[Dot[x1,xx]-Dot[y1,yy],Ga[0]]


Unprotect[Dot];


(* ::Section::Closed:: *)
(*Conjugate*)


Unprotect[Conjugate];
Conjugate[t_?(HermitianQ)[i_,j_]] := t[j,i];
Protect[Conjugate];


(* ::Section::Closed:: *)
(*Mass*)


Mass[ff_?AntiFieldQ]:=Mass[anti[ff]];


(* ::Section::Closed:: *)
(*IndexDelta*)


IndexDelta /: IndexDelta[xx_, yy_] gg_[cc___, ff_?(NoTensQ[#] =!= True &)[aa___, yy_, bb___], dd___] := gg[cc, ff[aa, xx, bb], dd];

IndexDelta /: IndexDelta[xx_, yy_] gg_[cc___, ff_?(NoTensQ[#] === True &)[aa___, yy_, bb___], dd___] := gg[cc, ff[aa, xx, bb], dd] IndexDelta[xx,yy];




IndexDelta /: IndexDelta[xx_, yy_] gg_[cc___, ff_?(NoTensQ[#] =!= True &)[aa___, yy_, bb___], dd___][inds___] := gg[cc, ff[aa, xx, bb], dd][inds];

IndexDelta /: IndexDelta[xx_, yy_] gg_[cc___, ff_?(NoTensQ[#] === True &)[aa___, yy_, bb___], dd___][inds___] := gg[cc, ff[aa, xx, bb], dd][inds] IndexDelta[xx,yy];

IndexDelta /: IndexDelta[xx_, yy_] ff_?(NoTensQ[#] =!= True &)[aa___, yy_, bb___] := ff[aa, xx, bb];

IndexDelta /: IndexDelta[xx_, yy_] ff_?(NoTensQ[#] === True &)[aa___, yy_, bb___] := ff[aa, xx, bb] IndexDelta[xx,yy];

IndexDelta/:Power[IndexDelta[aa_, bb_],2] := 1;

(*SetAttributes[IndexDelta, Orderless];*)

HC[IndexDelta[ind__]] := IndexDelta[ind];
HC[IndexDelta][ind__] := IndexDelta[ind];

Unprotect[Conjugate];
Conjugate[IndexDelta[ind__]] := IndexDelta[ind];
Conjugate[IndexDelta][ind__] := IndexDelta[ind];
Protect[Conjugate];

(*second condition added to avoid conflict with IndexDelta[a_?(FreeQ[#,Ext]&), Index[name_, Ext[k_]]] := IndexDelta[Index[name,Ext[k]], a]; *)
IndexDelta[Except[Index[Spin, _], i1_], Except[Index[Spin, _], i2_]] := IndexDelta[i2,i1] /; Not[OrderedQ[{i1,i2}]]&&(FreeQ[i2,Ext]&&FreeQ[i1,Ext]||Not[FreeQ[i2,Ext]]&&Not[FreeQ[i1,Ext]]);

IndexDelta[a_?(FreeQ[#,Index]&), Index[name_, b_]] := IndexDelta[Index[name, a], Index[name, b]];
IndexDelta[Index[name_, b_], a_?(FreeQ[#,Index]&)] := IndexDelta[Index[name, a], Index[name, b]];

IndexDelta[a_?(FreeQ[#,Ext]&), Index[name_, Ext[k_]]] := IndexDelta[Index[name,Ext[k]], a];

(*IndexDelta[Index[name_, Ext[k1_]], Index[name_, Ext[k2_]]] := IndexDelta[Index[name,Ext[k2]], Index[name, Ext[k1]]] /; Not[OrderedQ[{k1,k2}]];*)

IndexDelta[a_?NumericQ, b_?NumericQ] := 0 /; a != b;
IndexDelta[a_?NumericQ, a_] := 1;

IndexDelta[Index[name_, a_?NumericQ], Index[name_, b_?NumericQ]] := 0 /; a != b;
IndexDelta[Index[name_, a_?NumericQ], Index[name_, a_]] := 1;

IndexDelta[Index[Lorentz, mu_], Index[Lorentz, nu_]] := ME[Index[Lorentz, mu], Index[Lorentz, nu]];


NoTensQ[AST[_]] := True;

IndexDelta /: AST[t_][i_] * IndexDelta[i_, j_] := t[i] IndexDelta[i,i,j];
IndexDelta /: g_[AST[t_][i_]] * IndexDelta[i_, j_] := g[t[i]] IndexDelta[i,i,j];
IndexDelta /: h_[g_[AST[t_][i_]]] * IndexDelta[i_, j_] := h[g[t[i]]] IndexDelta[i,i,j];
IndexDelta /: k_[h_[g_[AST[t_][i_]]]] * IndexDelta[i_, j_] := k[h[g[t[i]]]] IndexDelta[i,i,j];

IndexDelta /: AST[t_][j_] * IndexDelta[i_, j_] := t[j] IndexDelta[j,j,i];
IndexDelta /: g_[AST[t_][j_]] * IndexDelta[i_, j_] := g[t[j]] IndexDelta[j,j,i];
IndexDelta /: h_[g_[AST[t_][j_]]] * IndexDelta[i_, j_] := h[g[t[j]]] IndexDelta[j,j,i];
IndexDelta /: k_[h_[g_[AST[t_][j_]]]] * IndexDelta[i_, j_] := k[h[g[t[j]]]] IndexDelta[j,j,i];

IndexDelta /: IndexDelta[xx_, xx_, yy_] gg_[cc___, ff_?(NoTensQ[#] =!= True &)[aa___, yy_, bb___], dd___] := gg[cc, ff[aa, xx, bb], dd];

IndexDelta /: IndexDelta[xx_, xx_,  yy_] gg_[cc___, ff_?(NoTensQ[#] =!= True &)[aa___, yy_, bb___], dd___][inds___] := gg[cc, ff[aa, xx, bb], dd][inds];

IndexDelta /: IndexDelta[xx_, xx_, yy_] ff_?(NoTensQ[#] =!= True &)[aa___, yy_, bb___] := ff[aa, xx, bb];


del[IndexDelta[_, _], _] := 0;


(* ::Section:: *)
(*numQ*)


numQ[xx_?NumericQ] := True;

numQ /: numQ[a_*b_] := numQ[a] && numQ[b];
numQ /: numQ[a_ + b_] := numQ[a] && numQ[b];
numQ /: numQ[a_ - b_] := numQ[a] && numQ[b];
numQ /: numQ[a_/b_] := numQ[a] && numQ[b];
numQ /: numQ[1/a_] := numQ[a];
numQ /: numQ[f_?(MemberQ[{Sqrt, Exp, Log, Sin, Cos, Tan, Csc, Sec, Cot, ArcSin, ArcCos, ArcTan, ArcCsc, ArcSec, ArcCot, Sinh, Cosh, 
Tanh, Csch, Sech, Coth, ArcSinh, ArcCosh, ArcTanh, ArcCsch, ArcSech, ArcCoth}, #]&)[a_]] := numQ[a];
numQ /: numQ[Power[a_, n_]] := numQ[a] && numQ[n];

numQ[FV[__]] := True;

numQ[delta[__]] := True;

numQ[ME[__]] := True;

numQ[ff_?StrucConstQ[__]] := True;

numQ[q_?(QuantumNumberQ[#] === True &)[_]] := True;

numQ[GaAlgebra[__]] := True;

numQ[HC[tt_]] := numQ[tt];
numQ[HC[tt_][ind__]] := numQ[tt[ind]];

numQ[Conjugate[tt_]] := numQ[tt];

CnumQ[_?(Not[numQ[#] === True] &)] := False;

CnumQ[Complex[_, Except[0, _]]] := True;

CnumQ /: CnumQ[a_*b_] := CnumQ[a] || CnumQ[b];
CnumQ /: CnumQ[a_ + b_] := CnumQ[a] || CnumQ[b];
CnumQ /: CnumQ[a_ - b_] := CnumQ[a] || CnumQ[b];
CnumQ /: CnumQ[a_/b_] := CnumQ[a] || CnumQ[b];
CnumQ /: CnumQ[1/a_] := CnumQ[a];
CnumQ /: CnumQ[f_?(MemberQ[{Sqrt, Exp, Log, Sin, Cos, Tan, Csc, Sec, Cot, ArcSin, ArcCos, ArcTan, ArcCsc, ArcSec, ArcCot, Sinh, Cosh, 
Tanh, Csch, Sech, Coth, ArcSinh, ArcCosh, ArcTanh, ArcCsch, ArcSech, ArcCoth}, #]&)[a_]] := CnumQ[a];
CnumQ /: CnumQ[Power[a_, n_]] := CnumQ[a];

CnumQ[Conjugate[tt_]] := CnumQ[tt];

CnumQ[q_?(QuantumNumberQ[#] === True &)[_]] := False;

CnumQ[pp_?(Element[#, Reals] === True &)] := False;
CnumQ[pp_?((Element[#, Reals] =!= True) && (Element[#, Complexes] === True) &)] := True;

CnumQ[Exp[x_]] := CnumQ[x]



numQ[TensDot[xx_, yy___][i_,j_]] := True;


CnumQ[FR$deltaZ[argx__]] := True;
numQ[FR$deltaZ[argx__]] := True;

CnumQ[FR$deltat[argx__]] := True;
numQ[FR$deltat[argx__]] := True;


CnumQ[FR$delta[argx__]] := True;
numQ[FR$delta[argx__]] := True;


(* ::Section::Closed:: *)
(*del*)


DelQ[__] := False;
DelQ[del[__]] := True;

(*anti[field_?FieldQ] := ToExpression[ToString[field] <> "bar"] /; Not[AntiFieldQ[field]] && Not[Head[field] === WeylL]  && Not[Head[field] === WeylR] && Not[FieldComponentQ[field]];
anti[del[field_, mu_]] := del[anti[field], mu];
anti[field_?(FieldQ)[inds__]] := anti[field][inds] /. {Spin1 -> Spin2, Spin2 -> Spin1};*)

anti /: anti[fi_?(FieldQ[#]===True&)[inds__]] := anti[fi][inds];
(*Mod. 22/01/2012: Added the case anti[CC[field]] which was not well taken into account*)
anti[field_] := ToExpression[ToString[field] <> "bar"] /; Not[AntiFieldQ[field]] && Not[Head[field] === WeylL]  && Not[Head[field] === WeylR]&&Not[Head[field]===CC];

anti[a_ + b_] := anti[a] + anti[b];
anti[a_ - b_] := anti[a] - anti[b];
anti[a_ * b_] := anti[a] * anti[b];
anti[a_/b_] := anti[a]/anti[b];
anti[Power[a_, n_]] := Power[anti[a], n];
anti[Conjugate[exp_]]:=Conjugate[anti[exp]];
anti[c_?(CnumQ[#] === True &)] := Conjugate[c];
anti[c_?(numQ[#] && (CnumQ[#] =!= True)&)] := c;
anti[fmat_?(VectorQ)] := anti /@ fmat;
anti[fmat_?(MatrixQ)] := anti /@ Transpose[fmat];

merge[expr1_, expr2_] := ToExpression[ToString[expr1] <> ToString[expr2]];
merge[expr1_, expr2_, expr3_] := ToExpression[ToString[expr1] <> ToString[expr2] <> ToString[expr3]];


del /: del[c_ * f_, mu_] := c * del[f, mu] /; numQ[c] && FieldQ[f];
del /: del[f_ + g_, mu_] := del[f, mu] + del[g, mu] ;
del /: del[f_ - g_, mu_] := del[f, mu] - del[g, mu] ;
del /: del[f_ * g_, mu_] := f * del[g, mu] + del[f, mu] * g;

del /: del[Dot[a_,b_,c___],mu_] := Plus @@ Table[MapAt[del[#,mu]&,Dot[a,b,c],kk], {kk}]

del /: del[(f_ - g_) / a_, mu_] := del[f/a - g/a, mu] /; numQ[a] && FieldQ[f] && FieldQ[g];
del /: del[(f_ + g_) / a_, mu_] := del[f/a + g/a, mu] /; numQ[a];
del /: del[(b_ * f_ - c_ * g_) / a_, mu_] := del[b*f/a - c*g/a, mu] /; numQ[a] && numQ[b] && numQ[c];
del /: del[(b_ * f_ + c_ * g_) / a_, mu_] := del[b * f/a + c * g/a, mu] /; mumQ[a] && numQ[b] && numQ[c];
del /: del[(b_ * f_ - g_) / a_, mu_] := del[b*f/a - g/a, mu] /; numQ[a] && numQ[b];
del /: del[(b_ * f_ + g_) / a_, mu_] := del[b * f/a +  g/a, mu] /; numQ[a] && numQ[b];
del /: del[(f_ - c_ * g_) / a_, mu_] := del[f/a - c*g/a, mu] /; numQ[a] && numQ[c];
del /: del[(f_ + c_ * g_) / a_, mu_] := del[f/a + c * g/a, mu] /; numQ[a] && numQ[c];
del /: del[f_ / a_ - g_ / b_, mu_] := del[f, mu]/a - del[g, mu]/b /; numQ[a] && numQ[b];
del /: del[f_ / a_ + g_ / b_, mu_] := del[f, mu]/a + del[g, mu]/b /; numQ[a] && numQ[b];
del /: del[c_ * f_ / a_ - d_ *  g_ / b_, mu_] := c * del[f, mu]/a - d * del[g, mu]/b /; numQ[a] && numQ[b] && numQ[c] && numQ[d];
del /: del[c_ * f_ / a_ + d_ * g_ / b_, mu_] := c * del[f, mu]/a + d * del[g, mu]/b /; numQ[a] && numQ[b] && numQ[c] && numQ[d];
del /: del[f_ / a_ - d_ * g_ / b_, mu_] := del[f, mu]/a - d * del[g, mu]/b /; numQ[a] && numQ[b] && numQ[d];
del /: del[f_ / a_ + d_ * g_ / b_, mu_] := del[f, mu]/a + d * del[g, mu]/b /; numQ[a] && numQ[b] && numQ[d];
del /: del[c_ * f_ / a_ - g_ / b_, mu_] := c * del[f, mu]/a - del[g, mu]/b /; numQ[a] && numQ[b] && numQ[c];
del /: del[c_ * f_ / a_ + g_ / b_, mu_] := c * del[f, mu]/a + del[g, mu]/b /; numQ[a] && numQ[b] && numQ[c];

del /: del[Power[f_?FieldQ, 2], mu_] := 2 * f * del[f, mu];
del /: del[Power[f_?(ScalarFieldQ), n_], mu_] := n * Power[f, n-1] * del[f, mu];
del /: del[Power[Plus[xx_, y__], n_], mu_] := del[Expand[Power[Plus[xx,y], n]], mu];
del /: del[f1_?(ScalarFieldQ) * f2_?(ScalarFieldQ), mu_] := f1 * del[f2, mu] + del[f1, mu] * f2;
del /: del[f1_?(ScalarFieldQ) * f2_?(ScalarFieldQ), mu_] := f1 * del[f2, mu] + del[f1, mu] * f2;
del /: del[f1_?(ScalarFieldQ) * f2_?(ScalarFieldQ) * f3_?(ScalarFieldQ), mu_] := f1 * del[f2 * f3, mu] + del[f1, mu] * f2 * f3;
del /: del[f1_?(ScalarFieldQ) * f2_?(ScalarFieldQ) * f3_?(ScalarFieldQ) * f4_?(ScalarFieldQ), mu_] := f1 * del[f2 * f3 * f4, mu] + del[f1, mu] * f2 * f3 * f4;

del /: del[f_ * a_ - g_ * b_, mu_] := del[f, mu]*a - del[g, mu]*b /; numQ[a] && numQ[b];
del /: del[f_ * a_ + g_ * b_, mu_] := del[f, mu]*a + del[g, mu]*b /; numQ[a] && numQ[b];
del /: del[f_ - g_ , mu_] := del[f, mu] - del[g, mu] ;
del /: del[f_ + g_ , mu_] := del[f, mu] + del[g, mu] ;
del /: del[a_ ,_] := 0 /; numQ[a];

(*del[Dot[Ga[mu_],fields__], nu_] := Dot[Ga[mu], del[fields,nu]];
del[Dot[ProjP[mu_],fields__], nu_] := Dot[ProjP[mu], del[fields,nu]];
del[Dot[ProjP,fields__], nu_] := Dot[ProjP, del[fields,nu]];
del[Dot[ProjM[mu_],fields__], nu_] := Dot[ProjM[mu], del[fields,nu]];
del[Dot[ProjM,fields__], nu_] := Dot[ProjM, del[fields,nu]];
del[Dot[GaAlgebra[mu_],fields__], nu_] := Dot[GaAlgebra[mu], del[fields,nu]];
del[Dot[Sig[mu_, nu_],fields__], nu_] := Dot[Sig[mu, nu], del[fields,nu]];*)

del[field_, mu_, nu_] := del[del[field, mu], nu];

del[{fields__},mu_] := (del[#,mu]&)/@List[fields];
(*
del[Dot[a_, bb___, f_], mu_] := Dot[a, del[Dot[bb, f], mu]] /; FieldQ[f] && Not[FieldQ[a]];

del[Dot[f_, bb___, a_], mu_] := Dot[del[Dot[f, bb], mu], a] /; FieldQ[f] && Not[FieldQ[a]];
*)

del[_?TensQ, _] := 0;
del[Dot[a_,b_,c___], mu_] := Plus @@ Table[MapAt[del[#,mu]&, Dot[a,b,c], kk] ,{kk, Length[Dot[a,b,c]]}];

del /: qn_?(QuantumNumberQ[#] === True&)[del[field_, _]] := qn[field];

del[FR$CT,mu_]:=0;



(* ::Section::Closed:: *)
(*Covariant derivative*)


(* ::Text:: *)
(*Linearity of the Covariant derivative*)


DC[grp_List][field_,mu_,side___] := del[field, mu] + (Plus @@ (DCint[#][field, mu,side]& /@ grp));

DC[field_, mu_] := DC[MR$GaugeGroupList][field, mu] /; FieldQ[field]&&((Not[WeylFieldQ[field //. del[ff_,_]:>ff] === True]) || FieldComponentQ[field] === True);
DC[field_, mu_] := DCWeyl[field, mu] /; (WeylFieldQ[field //. del[ff_,_]:>ff] === True) && Not[FieldComponentQ[field] === True];
DCWeyl[field_, mu_,side_] := DC[MR$GaugeGroupList][field, mu,side];


DCWeyl /: Dot[DCWeyl[field_, mu_], terms__] := Dot[DCWeyl[field,mu,Left],terms];
DCWeyl /: Dot[terms__, DCWeyl[field_, mu_]] := Dot[terms,DCWeyl[field,mu,Right]];


DC[sum_Plus, mu_] := DC[#, mu]& /@ sum;
DC[a_ b_, mu_] := a DC[b,mu] + DC[a,mu]b;
DC[a_?(numQ[#] === True&), _] := 0;


ContstructNonAbelianFieldComponentDC[group_,field_,complexconjugate_,fieldindices_,rep_,coupling_,boson_,lorentz_]:=Module[{

   pos = Position[fieldindices,rep[[2]]][[1]],
   matrix = rep[[1]],
   connectedindex,
   i,
   newfield,
   a
   
   },

   (* Get the field index that is connected *)
   connectedindex = Extract[field//.del[ff_,_]:>ff,pos];

   (* construct the new field*)
   newfield = field /. connectedindex -> i;

   (* Construct the matrix. !!!! we need to be careful about complex conjugated representations!!! *)
   matrix = If[complexconjugate,
               -matrix[a,i,connectedindex],
                matrix[a,connectedindex,i]
              ];

   Return[-I FR$DSign coupling boson[lorentz,a]matrix newfield];
];


AdjustContractionSide[field_,side_]:=Block[{tempside},

    If[(side === Left)||(side === Right),
       Return[side]];

    tempside = If[((Spin32FieldQ[field]===True)||(DiracFieldQ[field]===True)||(MajoranaFieldQ[field]===True))&&(AntiFieldQ[field]===True),
                  Left,Right];

     Return[tempside];
];


ContstructNonAbelianNonFieldComponentDC[group_,field_,complexconjugate_,fieldindices_,rep_,coupling_,boson_,lorentz_,side_]:=Module[{

    matrix=rep[[1]],
    a,
    sign = If[complexconjugate,-1,1],
    tempfield=field//.del[ff_,_]:>ff,
    tempside,contraction},

    (* Construct the matrix. !!!! we need to be careful about complex conjugated representations!!! *)
    matrix = If[MatchQ[tempfield,CC[_]]||((WeylFieldQ[tempfield]===True)&&((complexconjugate&&(side===Right))||(Not[complexconjugate]&&side===Left))),
                TP[matrix][a],
                   matrix[a]
               ];

    (* the fermion fields need ot have the gauge matrix contracted on the correct side.
       If the side is Automatic, it means that we contract barred Dirac fermions to the left,
       and unbarred oens ot the right *)
    tempside = AdjustContractionSide[tempfield,side];
    contraction = If[tempside === Left, 
                     field.matrix, 
                     matrix.field
                    ];

    Return[-sign I FR$DSign coupling boson[lorentz,a]contraction];
];


DCint[group_][field_?(FieldQ[#] === True&), mu_,side_:Automatic] := Block[{

    boson = GroupToBoson[group],
    coup  = GroupToCoup[group],
    rep,basicfield,fieldindices,
    charge,complexconjugate

    },

    (* If the gauge group is abelian, then we are done *)
    If[AbelianQ[group],
       Return[- I FR$DSign coup boson[mu] GroupToCharge[group][field]field];
      ];

    (* If not, we read out the group representations that have been declared *)
    rep = GroupToReps[group];
    rep = Which[rep === MR$Null,{},
                VectorQ[rep],{rep},
                MatrixQ[rep],rep
               ];

    rep = Join[{{AdjointRep[group], GroupToAdj[group]}},rep];

   (* Read out the basic field, i.e., without indices or derivatives *)
    basicfield = field //.del[ff_,_]:>ff;
    If[FieldComponentQ[basicfield] === True,basicfield = Head[basicfield]];
    fieldindices=$IndList[basicfield] /. Index:>Identity;
    (*If the basifield is an antiparticle, we need the complex conjugate representation *)
    complexconjugate=PRIVATE`IsAntiParticleQ[basicfield];

    (* Find the charge of the particle *)
    charge = Intersection[fieldindices, Last /@ rep];

     (*If the particle is not charge, stop *)
    If[charge=={},Return[0]];

    (* Check if the is at most one representation for each aprticle *)
    If[Length[charge]>1,
       Message[DC::Rep];
       charge=Take[charge,1]
      ];

     (* Get the representation matrix *)
    rep = Cases[rep,{_,charge[[1]]}][[1]];

     (* We now split into compoenent fields and non-component fields*)
    If[FieldComponentQ[field//.del[ff_,_]:>ff] === True,
       Return[ContstructNonAbelianFieldComponentDC[group,field,complexconjugate,fieldindices,rep,coup,boson,mu]],
       Return[ContstructNonAbelianNonFieldComponentDC[group,field,complexconjugate,fieldindices,rep,coup,boson,mu,side]]
      ];
];



(* ::Section::Closed:: *)
(*Canonical Dimension*)


(* ::Text:: *)
(*!! !! ! CanonicalDimension calculates the canonical dimension of ONE TERM!!!!!*)


CanonicalDimension[del[ff_?FieldQ, _]] := CanonicalDimension[ff] + 1;

CanonicalDimension[Times[term1_, terms__]] := CanonicalDimension /@ Plus[term1, terms];
CanonicalDimension[Dot[term1_, terms__]] := CanonicalDimension /@ Plus[term1, terms];

CanonicalDimension[_?numQ] := 0;
CanonicalDimension[_?TensQ] := 0;
CanonicalDimension[TensDot[tt1_, tt2__][inds___]] := 0;
CanonicalDimension[Power[term_,n_]] := Times[n, CanonicalDimension[term]];

CanonicalDimension[IndexDelta[__]] := 0;

CanonicalDimension[CC[ff_]] := CanonicalDimension[ff];
CanonicalDimension[CC[ff_][ind___]] := CanonicalDimension[ff[ind]];


(* ::Section::Closed:: *)
(*FieldQ & co.*)


FermionQ[del[field_, _]] := FermionQ[field];
BosonQ[del[field_, _]] := BosonQ[field];
VectorFieldQ[del[field_, _]] := VectorFieldQ[field];
ScalarFieldQ[del[field_, _]] := ScalarFieldQ[field];
DiracFieldQ[del[field_, _]] := DiracFieldQ[field];
MajoranaFieldQ[del[field_, _]] := MajoranaFieldQ[field];
GhostFieldQ[del[field_, _]] := GhostFieldQ[field];
Spin2FieldQ[del[field_, _]] := Spin2FieldQ[field];
FieldQ[del[field_, _]] := FieldQ[field];
RSpin32FieldQ[del[field_,_]] := RSpin32FieldQ[field];
CSpin32FieldQ[del[field_,_]] := CSpin32FieldQ[field];
Spin32FieldQ[del[field_,_]] := Spin32FieldQ[field];
RFermionFieldQ[del[field_,_]] := RFermionFieldQ[field];
CFermionFieldQ[del[field_,_]] := CFermionFieldQ[field];

FieldQ[_] := False;
FieldQ[a_^n_.] := FieldQ[a] /; (n > 1);
FieldQ[a_^n_] := FieldQ[a] /; (n > 1);
FieldQ[Times[a__]] := FieldQ /@ And[a];
FieldQ[Dot[a_, b__]] := FieldQ /@ And[a, b];

FieldComponentQ[Except[_[___]]] := False;
FieldComponentQ[_[__]] := True;
FieldComponentQ[_[]] := True;
FieldComponentQ[del[_[__], _]^_.] := True;

FieldQ[del[field_,_]^_.] := FieldQ[field];

TensQ[Conjugate[tt_]] := TensQ[tt];


(* MW edit: For DiagProd pass all queries to the field *)

AntiFieldQ[DiagProd[g_,f_]] := AntiFieldQ[f];
FermionQ[DiagProd[g_,f_]] := FermionQ[f];
Spin32FieldQ[DiagProd[g_,f_]] := Spin32FieldQ[f];
RSpin32FieldQ[DiagProd[g_,f_]] := RSpin32FieldQ[f];
CSpin32FieldQ[DiagProd[g_,f_]] := CSpin32FieldQ[f];
RFermionFieldQ[DiagProd[g_,f_]] := RFermionFieldQ[f];
CFermionFieldQ[DiagProd[g_,f_]] := CFermionFieldQ[f];
FieldQ[DiagProd[g_,f_]] := FieldQ[f];
DiracFieldQ[DiagProd[g_,f_]] := DiracFieldQ[f];
anti[DiagProd[g_,f_]] := DiagProd[g,anti[f]];
SpinorFieldQ[DiagProd[g_,f_]] := SpinorFieldQ[f];
CanonicalDimension[DiagProd[g_,f_]] := CanonicalDimension[f];
NInd[DiagProd[g_,f_]] := NInd[f];
MajoranaField[DiagProd[g_,f_]] := MajoranaField[f];
BosonQ[DiagProd[g_,f_]] := BosonQ[f];
VectorFieldQ[DiagProd[g_,f_]] := VectorFieldQ[f];
ScalarFieldQ[DiagProd[g_,f_]] := ScalarFieldQ[f];
Spin2FieldQ[DiagProd[g_,f_]] := Spin2FieldQ[f];
GhostFieldQ[DiagProd[g_,f_]] := GhostFieldQ[f];
$IndList[DiagProd[g_,f_]] := $IndList[f];
FieldComponentQ[DiagProd[g_,f_]] := FieldComponentQ[f];

AntiFieldQ[DiagProd[g_,f_][ii___]] := AntiFieldQ[f[ii]];
FermionQ[DiagProd[g_,f_][ii___]] := FermionQ[f[ii]];
Spin32FieldQ[DiagProd[g_,f_][ii___]] := Spin32FieldQ[f[ii]];
RSpin32FieldQ[DiagProd[g_,f_][ii___]] := RSpin32FieldQ[f[ii]];
CSpin32FieldQ[DiagProd[g_,f_][ii___]] := CSpin32FieldQ[f[ii]];
RFermionFieldQ[DiagProd[g_,f_][ii___]] := RFermionFieldQ[f[ii]];
CFermionFieldQ[DiagProd[g_,f_][ii___]] := CFermionFieldQ[f[ii]];
FieldQ[DiagProd[g_,f_][ii___]] := FieldQ[f[ii]];
DiracFieldQ[DiagProd[g_,f_][ii___]] := DiracFieldQ[f[ii]];
anti[DiagProd[g_,f_][ii___]] := DiagProd[g,anti[f]][ii];
SpinorFieldQ[DiagProd[g_,f_][ii___]] := SpinorFieldQ[f[ii]];
CanonicalDimension[DiagProd[g_,f_][ii___]] := CanonicalDimension[f[ii]];
NInd[DiagProd[g_,f_][ii___]] := NInd[f[ii]];
MajoranaField[DiagProd[g_,f_][ii___]] := MajoranaField[f[ii]];
BosonQ[DiagProd[g_,f_][ii___]] := BosonQ[f[ii]];
VectorFieldQ[DiagProd[g_,f_][ii___]] := VectorFieldQ[f[ii]];
ScalarFieldQ[DiagProd[g_,f_][ii___]] := ScalarFieldQ[f[ii]];
Spin2FieldQ[DiagProd[g_,f_][ii___]] := Spin2FieldQ[f[ii]];
GhostFieldQ[DiagProd[g_,f_][ii___]] := GhostFieldQ[f[ii]];
$IndList[DiagProd[g_,f_][ii___]] := $IndList[f[ii]];
FieldComponentQ[DiagProd[g_,f_][ii___]] := FieldComponentQ[f[ii]];

DiagProd[g_,f_][ii___] := DiagProd[g,f[ii]];
DiagProd[g_,f_[jj___]][ii___] := DiagProd[g,f[jj,ii]];
DiagProd[g_,c_?(!FieldQ[#]&)*f_?FieldQ] := c*DiagProd[g,f];
DiagProd[g_,s_Plus] := DiagProd[g,#]& /@ s;
DiagProd[g_?NumericQ, f_] = g f;



(* ::Section::Closed:: *)
(*left and right*)


(******* Lef and right handed projectors  ********)
    

left[field_] := Dot[ProjM, field] /; (Spin32FieldQ[field] || DiracFieldQ[field] || MajoranaFieldQ[field]) && Not[FieldComponentQ[field]] && Not[AntiFieldQ[field]];
left[field_[s_,i___]] := Module[{r}, ProjM[s,r] field[r,i]] /; (Spin32FieldQ[field] || DiracFieldQ[field] || MajoranaFieldQ[field]) && Not[AntiFieldQ[field]];
left[field_] := Dot[field, ProjP] /; (Spin32FieldQ[field] || DiracFieldQ[field] || MajoranaFieldQ[field]) && Not[FieldComponentQ[field]] && AntiFieldQ[field];
left[field_[s_,i___]] := Module[{r}, ProjP[r,s] field[r,i]] /; (Spin32FieldQ[field] || DiracFieldQ[field] || MajoranaFieldQ[field]) && AntiFieldQ[field];

right[field_] := Dot[ProjP, field] /; (Spin32FieldQ[field] || DiracFieldQ[field] || MajoranaFieldQ[field]) && Not[FieldComponentQ[field]] && Not[AntiFieldQ[field]];
right[field_[s_,i___]] := Module[{r}, ProjP[s,r] field[r,i]] /; (Spin32FieldQ[field] || DiracFieldQ[field] || MajoranaFieldQ[field]) && Not[AntiFieldQ[field]];
right[field_] := Dot[field, ProjM] /; (Spin32FieldQ[field] || DiracFieldQ[field] || MajoranaFieldQ[field]) && Not[FieldComponentQ[field]] && AntiFieldQ[field];
right[field_[s_,i___]] := Module[{r}, ProjM[r,s] field[r,i]] /; (Spin32FieldQ[field] || DiracFieldQ[field] || MajoranaFieldQ[field]) && AntiFieldQ[field];



(* ::Section::Closed:: *)
(*Pauli \[Sigma] matrices*)


(******* Pauli matrices  ********)

PauliSigma[1] = {{0,1},{1,0}};
PauliSigma[2] = {{0,-I},{I,0}};
PauliSigma[3] = {{1,0},{0,-1}};

PauliSigma[i_Integer, j_Integer, k_Integer] := PauliSigma[i][[j, k]];

PauliSigma[xx___, Index[_, i_Integer], yy___] := PauliSigma[xx, i, yy];

PauliSigma /: PauliSigma[i1_, i2_, i3_?(Not[NumericQ[#]]&)]PauliSigma[j1_, i3_, j3_] := PauliSigma[i1, i2, 1]PauliSigma[j1, 1, j3] + PauliSigma[i1, i2, 2]PauliSigma[j1, 2, j3];

(*added by celine*)
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&), 1, 1]PauliSigma[i1_, 1, 2] :=0;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&), 1, 1]PauliSigma[i1_, 2, 1] :=0;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&), 2, 2]PauliSigma[i1_, 1, 2] :=0;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&), 2, 2]PauliSigma[i1_, 2, 1] :=0;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&), 1, 2]^2 :=0;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&),2,1]^2 :=0;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&),2,1]PauliSigma[i1_?(Not[NumericQ[#]]&),1,2] :=2;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&),1,1]PauliSigma[i1_?(Not[NumericQ[#]]&),2,2] :=-1;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&),1,1]^2 :=1;
PauliSigma /: PauliSigma[i1_?(Not[NumericQ[#]]&),2,2]^2 :=1;

(*PerformPauliAlgebra[expr_] := expr //.PauliRules1//.PauliRules2;*)

PerformPauliAlgebra[expr_] := expr/. PauliSigma[k_?(Not[NumericQ[#]]&)] :> IndexDelta[k,1]PauliSigma[1]+ IndexDelta[k,2]PauliSigma[2] + IndexDelta[k,3]PauliSigma[3];

PauliRules1 = Dispatch[{PauliSigma[i1___, Except[Index[__], i_], i2___]h_[ww___, Index[name_, i_],tt___] -> PauliSigma[i1, Index[name, i], i2]h[ww,Index[name,i],tt],
PauliSigma[i1___, Except[Index[__], i_], i2___]gun_[xx1___, h_[ww___, Index[name_, i_], tt___], yy1___] -> PauliSigma[i1, Index[name, i], i2]gun[xx1, h[ww,Index[name,i],tt],yy1],
func_[xx___, PauliSigma[i1___, Except[Index[__], i_], i2___], yy__]h_[ww___, Index[name_, i_], tt___] -> func[xx, PauliSigma[i1, Index[name, i], i2], yy]h[ww,Index[name,i],tt],
func_[xx___, PauliSigma[i1___, Except[Index[__], i_], i2___], yy__]gun_[xx1___, h_[ww___, Index[name_, i_], tt___], yy1___] -> func[xx, PauliSigma[i1, Index[name, i], i2], yy]gun[xx1, h[ww,Index[name,i],tt],yy1],
gun_[xx1___, func_[xx___, PauliSigma[i1___, Except[Index[__], i_], i2___], yy__], yy1___]h_[ww___, Index[name_, i_], tt___] -> gun[xx1, func[xx, PauliSigma[i1, Index[name, i], i2], yy], yy1]h[ww,Index[name,i],tt],
PauliSigma[i1___, Except[Index[__], i_], i2___]func_[xx2___, gun_[xx1___, h_[ww___, Index[name_, i_], tt___], yy1___], yy2___] -> PauliSigma[i1, Index[name, i], i2]func[xx2, gun[xx1, h[ww,Index[name,i],tt],yy1],yy2]}];

PauliRules2 = Dispatch[{PauliSigma[i1_?((Not[NumericQ[#]] && Not[MatchQ[#,Index[_,_?NumericQ]]])&), i2___]h_[ww___, i1_,tt___] -> Sum[PauliSigma[ksigexp, i2]h[ww,ksigexp,tt], {ksigexp,3}],
PauliSigma[i1_?((Not[NumericQ[#]] && Not[MatchQ[#,Index[_,_?NumericQ]]])&), i2___]gun_[xx1___, h_[ww___, i1_, tt___], yy1___] -> Sum[PauliSigma[ksigexp, i2]gun[xx1, h[ww,ksigexp,tt],yy1], {ksigexp,3}],
func_[xx___, PauliSigma[i1_?((Not[NumericQ[#]] && Not[MatchQ[#,Index[_,_?NumericQ]]])&), i2___], yy__]h_[ww___, i1_, tt___] -> Sum[func[xx, PauliSigma[ksigexp, i2], yy]h[ww,ksigexp,tt], {ksigexp,3}],
func_[xx___, PauliSigma[i1_?((Not[NumericQ[#]] && Not[MatchQ[#,Index[_,_?NumericQ]]])&), i2___], yy__]gun_[xx1___, h_[ww___, i1_, tt___], yy1___] -> Sum[func[xx, PauliSigma[ksigexp, i2], yy]gun[xx1, h[ww,ksigexp,tt],yy1], {ksigexp,3}],
gun_[xx1___, func_[xx___, PauliSigma[i1_?((Not[NumericQ[#]] && Not[MatchQ[#,Index[_,_?NumericQ]]])&), i2___], yy__], yy1___]h_[ww___, i1_, tt___] -> Sum[gun[xx1, func[xx, PauliSigma[ksigexp, i2], yy], yy1]h[ww,ksigexp,tt], {ksigexp,3}],
PauliSigma[i1_?((Not[NumericQ[#]] && Not[MatchQ[#,Index[_,_?NumericQ]]])&), i2___]func_[xx2___, gun_[xx1___, h_[ww___, i1_, tt___], yy1___], yy2___] -> Sum[PauliSigma[ksigexp, i2]func[xx2, gun[xx1, h[ww,ksigexp,tt],yy1],yy2], {ksigexp,3}],
hunc_[xx2___, func_[xx___, PauliSigma[i1_?((Not[NumericQ[#]] && Not[MatchQ[#,Index[_,_?NumericQ]]])&), i2___], yy__], yy2___]gun_[xx1___, h_[ww___, i1_, tt___], yy1___] -> Sum[hunc[xx2, func[xx, PauliSigma[ksigexp, i2], yy], yy2]gun[xx1, h[ww,ksigexp,tt],yy1], {ksigexp,3}],
func_[xx___, PauliSigma[i1_?((Not[NumericQ[#]] && Not[MatchQ[#,Index[_,_?NumericQ]]])&), i2___], yy__]hunc_[xx2___,gun_[xx1___, h_[ww___, i1_, tt___], yy1___],yy2___] -> Sum[func[xx, PauliSigma[ksigexp, i2], yy]hunc[xx2,gun[xx1, h[ww,ksigexp,tt],yy1],yy2], {ksigexp,3}]}];


(* Added by Benj *)
Conjugate[si[a_,b_,c_]]^:=si[a,c,b];
Conjugate[sibar[a_,b_,c_]]^:=sibar[a,c,b];

si[mu_,a_,b_] sibar[nu_,b_,c_] si[ro_,c_,d_] ^:= Module[{sg}, 
   ME[mu,nu] si[ro,a,d] + ME[ro,nu] si[mu,a,d] - ME[mu,ro] si[nu,a,d] - I Eps[mu,nu,ro,sg] si[sg,a,d] ];

sibar[mu_,a_,b_] si[nu_,b_,c_] sibar[ro_,c_,d_] ^:= Module[{sg}, 
   ME[mu,nu] sibar[ro,a,d] + ME[ro,nu] sibar[mu,a,d] - ME[mu,ro] sibar[nu,a,d] + I Eps[mu,nu,ro,sg] sibar[sg,a,d] ];

numQ[PauliSigma[_,_,_]]:=True;
CnumQ[PauliSigma[_,_,_]]:=True;


(* ::Section::Closed:: *)
(*TensDot*)


TensDot[v_] := v;
TensDot[][i_, j_] := delta[i, j];

TensQ[TensDot[xx_, yy_]] := And[TensQ[xx], TensQ[yy]];
TensQ[TensDot[xx_, yy__]] := And[TensQ[xx], TensQ /@ And[yy]];

TensDot[x___, -u_, y___][ind___] := -TensDot[x, u, y][ind];
TensDot[x___, a_?(numQ) * u_, y___][ind___] := a * TensDot[x, u, y][ind];

TensDot[xx___, IndexDelta, yy___] := TensDot[xx, yy];





TensDot /: TensDot[a1_, as__][ii_, jj_] TensDot[b1_, bs__][jj_, kk_] :=
  TensDot[a1, as, b1, bs][ii, kk]


(* ::Section::Closed:: *)
(*Gamma matrices*)


(* ::Text:: *)
(*Conventions :*)
(*    ProjP = (1 + Ga[5])/2                                         *)
(*    ProjM = (1 - Ga[5])/2                                               *)
(*    ProjP[mu] = Ga[mu].(1 + Ga[5])/2                                    *)
(*    ProjM[mu] = Ga[mu].(1 - Ga[5])/2  *)


(* ::Subsection:: *)
(*Tensor declaration of the gamma matrices*)


$TensClass[Ga] = MR$GammaMatrices;
$TensClass[Ga[___]] := MR$GammaMatrices;
$TensClass[GaAlgebra] = MR$GammaMatrices;
$TensClass[GaAlgebra[___]] := MR$GammaMatrices;
$TensClass[ProjP] = MR$GammaMatrices;
$TensClass[ProjP[___]] := MR$GammaMatrices;
$TensClass[ProjM] = MR$GammaMatrices;
$TensClass[ProjM[___]] := MR$GammaMatrices;
$TensClass[Sig] = MR$GammaMatrices;
$TensClass[Sig[___]] := MR$GammaMatrices;

numQ[Ga[mu_, r_, s_]] := True;
numQ[ProjP[r_, s_]] := True;
numQ[ProjP[mu_, r_, s_]] := True;
numQ[ProjM[r_, s_]] := True;
numQ[ProjM[mu_, r_, s_]] := True;
numQ[Sig[mu_, nu_, r_, s_]] := True;

TensQ[Ga] = True;
TensQ[Ga[___]] := True;
TensQ[GaAlgebra] = True;
TensQ[GaAlgebra[___]] := True;
TensQ[ProjP] = True;
TensQ[ProjP[___]] := True;
TensQ[ProjM] = True;
TensQ[ProjM[___]] := True;
TensQ[Sig] = True;
TensQ[Sig[___]] := True;

Ga[xx___][yy___] := Ga[xx, yy];
ProjP[xx___][yy___] := ProjP[xx, yy];
ProjM[xx___][yy___] := ProjM[xx, yy];
Sig[xx___][yy___] := Sig[xx, yy];
SlashedP[xx___][yy___] := SlashedP[xx,yy];

MR$GammaMatrixQ[___] := False;
MR$GammaMatrixQ[Ga] = True;
MR$GammaMatrixQ[Ga[_]] := True;
MR$GammaMatrixQ[ProjP] = True;
MR$GammaMatrixQ[ProjP[_]] := True;
MR$GammaMatrixQ[ProjM] = True;
MR$GammaMatrixQ[ProjM[_]] := True;
MR$GammaMatrixQ[Sig] = True;
MR$GammaMatrixQ[Sig[___]] := True;

GammaMatrixQ[Ga] := True;
GammaMatrixQ[Ga[_]] := True;
GammaMatrixQ[ProjP[_]] := True;
GammaMatrixQ[ProjM[_]] := True;
GammaMatrixQ[ProjP] = True;
GammaMatrixQ[ProjM] = True;
GammaMatrixQ[Sig[__]] := True;
GammaMatrixQ[Sig]=True;

GammaMatrixQ[TensDot[xx_, yy__]] := GammaMatrixQ /@ And[xx, yy];

MR$GammaMatricesRules = {Ga[mu_?(((FreeQ[#, Index]) && (# =!= 5))&)] :> Ga[Index[Lorentz, mu]],
                         Ga[mu_?(((FreeQ[#, Index]) && (# =!= 5))&), r_] :> Ga[Index[Lorentz, mu], r],
                         Ga[mu_, Except[Index[___], r_]] :> Ga[mu, Index[Spin, r]],
                         Ga[mu_?(((FreeQ[#, Index]) && (# =!= 5))&), r_, s_] :> Ga[Index[Lorentz, mu], r, s],
                         Ga[mu_, Except[Index[___], r_], s_] :> Ga[mu, Index[Spin, r], s],
                         Ga[mu_, r_, Except[Index[___], s_]] :> Ga[mu, r, Index[Spin, s]],
                         GaAlgebra[mu___, nu_?(((FreeQ[#, Index]) && (# != 5))&), rho___, r_, s_] :> GaAlgebra[mu, Index[Lorentz, nu], rho, r, s],
                         GaAlgebra[mu___, Except[Index[___], r_], s_] :> GaAlgebra[mu, Index[Spin, r], s],
                         GaAlgebra[mu___, r_, Except[Index[___], s_]] :> GaAlgebra[mu, r, Index[Spin, s]],
                         ProjP[Except[Index[___], mu_]] :> ProjP[Index[Lorentz, mu]],
                         ProjP[Except[Index[___], r_], s_] :> ProjP[Index[Spin, r], s],
                         ProjP[r_, Except[Index[___], s_]] :> ProjP[r, Index[Spin, s]],
                         ProjP[Except[Index[___], mu_], r_, s_] :> ProjP[Index[Lorentz, mu], r, s],
                         ProjP[mu_, Except[Index[___], r_], s_] :> ProjP[mu, Index[Spin, r], s],
                         ProjP[mu_, r_, Except[Index[___], s_]] :> ProjP[mu, r, Index[Spin, s]],
                         ProjM[Except[Index[___], mu_]] :> ProjM[Index[Lorentz, mu]],
                         ProjM[Except[Index[___], r_], s_] :> ProjM[Index[Spin, r], s],
                         ProjM[r_, Except[Index[___], s_]] :> ProjM[r, Index[Spin, s]],
                         ProjM[Except[Index[___], mu_], r_, s_] :> ProjM[Index[Lorentz, mu], r, s],
                         ProjM[mu_, Except[Index[___], r_], s_] :> ProjM[mu, Index[Spin, r], s],
                         ProjM[mu_, r_, Except[Index[___], s_]] :> ProjM[mu, r, Index[Spin, s]],
                         Sig[Except[Index[___], mu_], nu_] :> Sig[Index[Lorentz, mu], nu],
                         Sig[mu_, Except[Index[___], nu_]] :> Sig[mu, Index[Lorentz, nu]],
                         Sig[Except[Index[___], mu_], nu_, r_, s_] :> Sig[Index[Lorentz, mu], nu, r ,s], 
                         Sig[mu_, Except[Index[___], nu_], r_, s_] :> Sig[mu, Index[Lorentz, nu], r, s],
                         Sig[mu_, nu_, Except[Index[___], r_], s_] :> Sig[mu, nu, Index[Spin, r], s],
                         Sig[mu_, nu_, r_, Except[Index[___], s_]] :> Sig[mu, nu, r, Index[Spin, s]]};

$TensIndRules = If[FreeQ[$TensIndRules, Ga], Join[$TensIndRules, MR$GammaMatricesRules], $TensIndRules];

CnumQ[ProjP[r_,s_]] := True;
CnumQ[ProjM[r_,s_]] := True;
CnumQ[ProjP[mu_,r_,s_]] := True;
CnumQ[ProjM[mu_,r_,s_]] := True;
CnumQ[Ga[mu_,r_,s_]] := True;

(* added by Benj *)
Conjugate[ProjM[inds__]]^:=ProjM[inds];
Conjugate[ProjP[inds__]]^:=ProjP[inds];


(* ::Subsection:: *)
(*Dirac matrix algebra*)


GaAlgebra /: GaAlgebra[x___,Ga[mu_],y___] := GaAlgebra[x,mu,y];
GaAlgebra /: GaAlgebra[x___,ProjP[mu_],y___] := GaAlgebra[x,mu,ProjP,y];
GaAlgebra /: GaAlgebra[x___,ProjM[mu_],y___] := GaAlgebra[x,mu,ProjM,y]; 
      
GaAlgebra /: GaAlgebra[]*Ga[mu_, r_, s_] := GaAlgebra[mu, r, s];
GaAlgebra /: GaAlgebra[]*Ga[mu_, r_, s_] := GaAlgebra[mu, r, s];
GaAlgebra /: GaAlgebra[]*GaAlgebra[mu__, r_, s_] := GaAlgebra[mu, r, s];
GaAlgebra /: GaAlgebra[]*ProjP[r_, s_] := GaAlgebra[ProjP, r, s];
GaAlgebra /: GaAlgebra[]*ProjM[r_, s_] := GaAlgebra[ProjM, r, s];
GaAlgebra /: GaAlgebra[]*ProjP[mu_, r_, s_] := GaAlgebra[mu, ProjP, r, s];
GaAlgebra /: GaAlgebra[]*ProjM[mu_, r_, s_] := GaAlgebra[mu, ProjM, r, s];

GaAlgebra /: GaAlgebra[mu__, r_, s_]*Ga[nu_, s_, t_] := GaAlgebra[mu, nu, r, t];
GaAlgebra /: Ga[nu_, r_, s_]*GaAlgebra[mu__, s_, t_] := GaAlgebra[nu, mu, r, t];
GaAlgebra /: GaAlgebra[mu__, r_, s_]*ProjP[s_, t_] := GaAlgebra[mu, ProjP, r, t];
GaAlgebra /: ProjP[r_, s_]*GaAlgebra[mu__, s_, t_] := GaAlgebra[ProjP, mu, r, t];
GaAlgebra /: GaAlgebra[mu__, r_, s_]*ProjM[s_, t_] := GaAlgebra[mu, ProjM, r, t];
GaAlgebra /: ProjM[r_, s_]*GaAlgebra[mu__, s_, t_] := GaAlgebra[ProjM, mu, r, t];
GaAlgebra /: GaAlgebra[mu__, r_, s_]*ProjP[nu_, s_, t_] := GaAlgebra[mu, nu, ProjP, r, t];
GaAlgebra /: ProjP[nu_, r_, s_]*GaAlgebra[mu__, s_, t_] := GaAlgebra[nu, ProjP, mu, r, t];
GaAlgebra /: GaAlgebra[mu__, r_, s_]*ProjM[nu_s _, t_] := GaAlgebra[mu, nu, ProjM, r, t];
GaAlgebra /: ProjM[nu_r _, s_]*GaAlgebra[mu__, s_, t_] := GaAlgebra[nu, ProjM, mu, r, t];
GaAlgebra /: GaAlgebra[mu___, r_, s_] * GaAlgebra[nu___, s_, t_] := GaAlgebra[mu, nu, r, t];

GaAlgebra /: GaAlgebra[mu___, 5, 5, nu___, r_, s_] := GaAlgebra[mu, nu, r, s];
GaAlgebra /: GaAlgebra[mu___, 5, ProjP, nu___, r_, s_] := GaAlgebra[mu, ProjP, nu, r, s];
GaAlgebra /: GaAlgebra[mu___, 5, ProjM, nu___, r_, s_] := -GaAlgebra[mu, ProjM, nu, r, s];
GaAlgebra /: GaAlgebra[mu___, ProjP, 5, nu___, r_, s_] := GaAlgebra[mu, ProjP, nu, r, s];
GaAlgebra /: GaAlgebra[mu___, ProjM, 5, nu___, r_, s_] := -GaAlgebra[mu, ProjM, nu, r, s];

GaAlgebra /: GaAlgebra[mu___, ProjP, ProjP, nu___, r_, s_] := GaAlgebra[mu, ProjP, nu, r, s];
GaAlgebra /: GaAlgebra[mu___, ProjM, ProjM, nu___, r_, s_] := GaAlgebra[mu, ProjM, nu, r, s];
GaAlgebra /: GaAlgebra[mu___, ProjP, ProjM, nu___, r_, s_] := 0;
GaAlgebra /: GaAlgebra[mu___, ProjM, ProjP, nu___, r_, s_] := 0;

GaAlgebra /: GaAlgebra[mu___, 5, rho_?(# =!= 5 &), nu___, r_, s_] := -GaAlgebra[mu, rho, 5, nu, r, s];
GaAlgebra /: GaAlgebra[mu___, ProjP, rho_?(# =!= 5 &), nu___, r_, s_] := GaAlgebra[mu, rho, ProjM, nu, r, s];
GaAlgebra /: GaAlgebra[mu___, ProjM, rho_?(# =!= 5 &), nu___, r_, s_] := GaAlgebra[mu, rho, ProjP, nu, r, s];

RemoveGa[expr_] := If[Head[expr] === Ga, Identity@@expr,expr];

MR$PeformGaAlgebraRules1 = {GaAlgebra[r_, s_] -> delta[r, s],
                            GaAlgebra[Except[ProjP|ProjM, mu_], r_, s_] -> Ga[mu, r, s],
                            GaAlgebra[ProjP, r_, s_] -> ProjP[r, s],
                            GaAlgebra[ProjM, r_, s_] -> ProjM[r, s],
                            GaAlgebra[u_, v__, r_, s_] :> GaTensDot[u, v][r, s]};
                            

MR$PeformGaAlgebraRules2 = {GaTensDot[u__, Index[Lorentz, mu__], v__] :> GaTensDot[u, Ga[Index[Lorentz, mu]], v],
                            GaTensDot[Index[Lorentz, mu__], v__] :> GaTensDot[Ga[Index[Lorentz, mu]], v],
                            GaTensDot[u__, Index[Lorentz, mu__]] :> GaTensDot[u, Ga[Index[Lorentz, mu]]],
                            GaTensDot[u__, 5, v__] :> GaTensDot[u, Ga[5], v],
                            GaTensDot[5, v__] :> GaTensDot[Ga[5], v],
                            GaTensDot[u__, 5] :> GaTensDot[u, Ga[5]]};

MR$PeformGaAlgebraRules3 = {Ga[mu_, r_, s_] :> GaAlgebra[mu, r, s], ProjP[r_, s_] :> GaAlgebra[ProjP, r, s], ProjM[r_, s_] :> GaAlgebra[ProjM, r, s], ProjP[mu_, r_, s_] :> GaAlgebra[ProjP[mu], r, s], ProjM[mu_, r_, s_] :> GaAlgebra[ProjM[mu], r, s]};
                           

PerformGaAlgebra[expr_] := expr //. Dispatch[MR$PeformGaAlgebraRules3] //. TensDot[u_?(MR$GammaMatrixQ),v___][r_,s_] -> GaAlgebra[u,v,r,s] //. Dispatch[MR$PeformGaAlgebraRules1] //. Dispatch[MR$PeformGaAlgebraRules2] //. GaTensDot -> TensDot //. Dispatch[MR$PeformGaAlgebraRules1];

ProjP /: ProjP[r_, s_] + ProjM[r_, s_] := IndexDelta[r, s];
ProjP /: ProjP[r_, s_] - ProjM[r_, s_] := Ga[5, r, s];
ProjM /: ProjM[r_, s_] - ProjP[r_, s_] := -Ga[5, r, s];
ProjP /: ProjP[r_, s_] * ProjP[s_, t_] := ProjP[r, t];
ProjM /: ProjM[r_, s_] * ProjM[s_, t_] := ProjM[r, t];
ProjP /: ProjP[r_, s_] * ProjM[s_, t_] := 0;
ProjP /: ProjM[r_, s_] * ProjP[s_, t_] := 0;

(* include transformation between chiral and dirac bases for Ga[5]Ga[mu] etc... *)
(* check if FR automatically renames products Ga[5,mu] etc... *)

TensDot /: TensDot[g1_, ProjP][r_, s_] + TensDot[g1_, ProjM][r_, s_] := g1[r,s];
TensDot /: TensDot[g11_, g1__, ProjP][r_, s_] + TensDot[g11_, g1__, ProjM][r_, s_] := TensDot[g11, g1][r,s];
TensDot /: TensDot[g1__, ProjP][r_, s_] - TensDot[g1__, ProjM][r_, s_] := TensDot[g1, Ga[5]][r,s]
TensDot /: TensDot[g1__, ProjM][r_, s_] - TensDot[g1__, ProjP][r_, s_] := -TensDot[g1, Ga[5]][r,s];
TensDot /: TensDot[g1__, ProjP][r_,s_] ProjP[s_,t_] := TensDot[g1, ProjP][r, t];
TensDot /: TensDot[g1__, ProjM][r_,s_] ProjM[s_,t_] := TensDot[g1, ProjM][r, t];
TensDot /: TensDot[g1__, ProjP][r_,s_] ProjM[s_,t_] := 0;
TensDot /: TensDot[g1__, ProjM][r_,s_] ProjP[s_,t_] := 0;

TensDot /: TensDot[g1_, g2__][r_, s_] IndexDelta[s_, t_] := TensDot[g1, g2][r, t];
TensDot /: TensDot[g1_, g2__][r_, s_] IndexDelta[t_, s_] := TensDot[g1, g2][r, t];

TensDot /: TensDot[g1_, g2__][s_, r_] IndexDelta[s_, t_] := TensDot[g1, g2][t, r];
TensDot /: TensDot[g1_, g2__][s_, r_] IndexDelta[t_, s_] := TensDot[g1, g2][t, r];

TensDot /: Ga[mu_, r_, s_] TensDot[g1_, g2__][s_,t_] := TensDot[Ga[mu], g1, g2][r, t];
TensDot /: ProjP[r_, s_] TensDot[g1_, g2__][s_,t_] := TensDot[ProjP, g1, g2][r, t];
TensDot /: ProjM[r_, s_] TensDot[g1_, g2__][s_,t_] := TensDot[ProjM, g1, g2][r, t];
TensDot /: Ga[mu_, s_, r_] TensDot[g1_, g2__][t_,s_] := TensDot[g1, g2, Ga[mu]][t, r];
TensDot /: ProjP[s_, r_] TensDot[g1_, g2__][t_,s_] := TensDot[g1, g2, ProjP][t, r];
TensDot /: ProjM[s_, r_] TensDot[g1_, g2__][t_,s_] := TensDot[g1, g2, ProjM][t, r];
TensDot[g1___, ProjP, Ga[mu_?((#=!=5)&)], g2___][s_, t_] := TensDot[g1, Ga[mu], ProjM, g2][s,t];
TensDot[g1___, ProjM, Ga[mu_?((#=!=5)&)], g2___][s_, t_] := TensDot[g1, Ga[mu], ProjP, g2][s,t];
TensDot[g1___, ProjP, SlashedP[i_], g2___][s_, t_] := TensDot[g1, SlashedP[i], ProjM, g2][s,t];
TensDot[g1___, ProjM, SlashedP[i_], g2___][s_, t_] := TensDot[g1, SlashedP[i], ProjP, g2][s,t];
TensDot[g1___, ProjP, Ga[5], g2___][s_, t_] := TensDot[g1, ProjP, g2][s,t];
TensDot[g1___, ProjM, Ga[5], g2___][s_, t_] := -TensDot[g1, ProjM, g2][s,t];
TensDot[g1___, Ga[5], ProjP, g2___][s_, t_] := TensDot[g1, ProjP, g2][s,t];
TensDot[g1___, Ga[5], ProjM, g2___][s_, t_] := -TensDot[g1, ProjM, g2][s,t];
TensDot[g1___, ProjP, ProjP, g2___][s_, t_] := TensDot[g1, ProjP, g2][s,t];
TensDot[g1___, ProjP, ProjM, g2___][r_,s_] := 0;
TensDot[g1___, ProjM, ProjP, g2___][r_,s_] := 0;
TensDot[g1___, ProjM, ProjM, g2___][s_, t_] := TensDot[g1, ProjM, g2][s,t];
TensDot[g1___, Ga[5], Ga[mu_?((# =!= 5) &)], g2___][s_, t_] := -TensDot[g1, Ga[mu], Ga[5], g2][s,t];

(* commenting this out to see if can avoid bug *)
TensDot[g1___, Ga[5], Ga[5], g2___][s_, t_] := TensDot[g1, g2][s,t]; (* fixed this in basis file - liam *)

(*Traces added by celine*)
ProjP[s_, s_] := 2;
ProjM[s_, s_] := 2;
Ga[5][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)], ProjP][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)], ProjM][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)], Ga[5]][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)]][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)],Ga[nu_?((#=!=5)&)], ProjP][s_, s_] := 2 ME[mu,nu];
TensDot[Ga[mu_?((#=!=5)&)],Ga[nu_?((#=!=5)&)], ProjM][s_, s_] := 2 ME[mu,nu];
TensDot[Ga[mu_?((#=!=5)&)],Ga[nu_?((#=!=5)&)], Ga[5]][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)],Ga[nu_?((#=!=5)&)]][s_, s_] := 4 ME[mu,nu];
TensDot[Ga[mu_?((#=!=5)&)],Ga[nu_?((#=!=5)&)],Ga[rho_?((#=!=5)&)], ProjP][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)],Ga[nu_?((#=!=5)&)],Ga[rho_?((#=!=5)&)], ProjM][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)],Ga[nu_?((#=!=5)&)],Ga[rho_?((#=!=5)&)], Ga[5]][s_, s_] := 0;
TensDot[Ga[mu_?((#=!=5)&)],Ga[nu_?((#=!=5)&)],Ga[rho_?((#=!=5)&)]][s_, s_] := 0;


(*Commented for 4F operators at NLO*)
(*GaTensDot[g1__, Ga[nu_], Ga[mu_], g2__][s_, t_] := -GaTensDot[g1, Ga[mu], Ga[nu], g2][s,t] + 2 ME[mu, nu] GaTensDot[g1,g2][s,t] /; Not[OrderedQ[{nu, mu}]] && (mu=!= 5) && (nu=!= 5);
GaTensDot[g1__, Ga[nu_], Ga[mu_]][s_, t_] := -GaTensDot[g1, Ga[mu], Ga[nu]][s,t] + 2 ME[mu, nu] GaTensDot[g1][s,t] /; (Length[{g1}]>1) && Not[OrderedQ[{nu, mu}]] && (mu=!= 5) && (nu=!= 5);
GaTensDot[Ga[nu_], Ga[mu_], g1__][s_, t_] := -GaTensDot[Ga[mu], Ga[nu], g1][s,t] + 2 ME[mu, nu] GaTensDot[g1][s,t] /; (Length[{g1}]>1) && Not[OrderedQ[{nu, mu}]] && (mu=!= 5) && (nu=!= 5);
GaTensDot[g1_, Ga[nu_], Ga[mu_]][s_, t_] := -GaTensDot[g1, Ga[mu], Ga[nu]][s,t] + 2 ME[mu, nu] GaAlgebra[RemoveGa[g1], s,t] /; Not[OrderedQ[{nu, mu}]] && (mu=!= 5) && (nu=!= 5);
GaTensDot[Ga[nu_], Ga[mu_], g1_][s_, t_] := -GaTensDot[Ga[mu], Ga[nu],g1][s,t] + 2 ME[mu, nu] GaAlgebra[RemoveGa[g1], s,t] /; Not[OrderedQ[{nu, mu}]] && (mu=!= 5) && (nu=!= 5);
GaTensDot[Ga[nu_], Ga[mu_]][s_, t_] := -GaTensDot[Ga[mu], Ga[nu]][s,t] + 2 ME[mu, nu] IndexDelta[s,t] /; Not[OrderedQ[{nu, mu}]] && (mu=!= 5) && (nu=!= 5);*)

Ga /: Dot[g1___, Ga[5], ProjP, g2___] := Dot[g1, ProjP, g2];
Ga /: Dot[g1___, Ga[5], ProjM, g2___] := - Dot[g1, ProjM, g2];
Ga /: Dot[g1___, ProjP, Ga[5], g2___] := Dot[g1, ProjP, g2];
Ga /: Dot[g1___, ProjM, Ga[5], g2___] := - Dot[g1, ProjM, g2];
ProjP /: Dot[g1___, ProjP[mu_], g2___] := Dot[g1, Ga[mu], ProjP, g2];
ProjM /: Dot[g1___, ProjM[mu_], g2___] := Dot[g1, Ga[mu], ProjM, g2];
Ga /: Dot[g1___, Ga[5], Ga[mu_?(#=!=5&)], g2___] := - Dot[g1, Ga[mu], Ga[5], g2];
Ga /: Dot[g1___, ProjP, Ga[mu_?(#=!=5&)], g2___] := Dot[g1, Ga[mu], ProjM, g2];
Ga /: Dot[g1___, ProjM, Ga[mu_?(#=!=5&)], g2___] := Dot[g1, Ga[mu], ProjP, g2];

Ga /: Dot[g1___, Ga[mu_,r_,s_], g2___] := Ga[mu,r,s] Dot[g1, g2];
ProjP /: Dot[g1___, ProjP[r_,s_], g2___] := ProjP[r,s] Dot[g1, g2];
ProjM /: Dot[g1___, ProjM[r_,s_], g2___] := ProjM[r,s] Dot[g1, g2];





(* LRM 16/06/16: Fixed an issue in Mathematica 10.4 by which TensDot would erroneously take leptons as an argument (since these carry 2 indices and accidentally 
match these definitions) by adding a qualifier here to limit these definitions to Dirac algebra objects. (Internal functions with variables shared with gamma matrices
were also susceptible). *)

SpinMatrixObject:=Ga|ProjP|ProjM|Sig;

Ga /: Ga[mu_, r_,s_] tt_[ind__, s_,u_] := TensDot[Ga[mu], tt[ind]][r, u] /; MatchQ[tt,SpinMatrixObject];
Ga /: Ga[mu_, s_,r_] tt_[ind__, u_,s_] := TensDot[tt[ind], Ga[mu]][u, r] /; MatchQ[tt,SpinMatrixObject];

ProjP /: ProjP[r_,s_] tt_[ind__, s_,u_] := TensDot[ProjP, tt[ind]][r, u] /; MatchQ[tt,SpinMatrixObject];
ProjP /: ProjP[s_,r_] tt_[ind__, u_,s_] := TensDot[tt[ind], ProjP][u, r] /; MatchQ[tt,SpinMatrixObject];
ProjM /: ProjM[r_,s_] tt_[ind__, s_,u_] := TensDot[ProjM, tt[ind]][r, u] /; MatchQ[tt,SpinMatrixObject];
ProjM /: ProjM[s_,r_] tt_[ind__, u_,s_] := TensDot[tt[ind], ProjM][u, r] /; MatchQ[tt,SpinMatrixObject];

ProjP /: ProjP[mu_, r_,s_] tt_[ind__, s_,u_] := TensDot[ProjP[mu], tt[ind]][r, u] /; MatchQ[tt,SpinMatrixObject];
ProjP /: ProjP[mu_, s_,r_] tt_[ind__, u_,s_] := TensDot[tt[ind], ProjP[mu]][u, r] /; MatchQ[tt,SpinMatrixObject];
ProjM /: ProjM[mu_, r_,s_] tt_[ind__, s_,u_] := TensDot[ProjM[mu], tt[ind]][r, u] /; MatchQ[tt,SpinMatrixObject];
ProjM /: ProjM[mu_, s_,r_] tt_[ind__, u_,s_] := TensDot[tt[ind], ProjM[mu]][u, r] /; MatchQ[tt,SpinMatrixObject];

Ga /: Ga[mu_, r_,s_] tt_[s_,u_] := TensDot[Ga[mu], tt][r, u] /; MatchQ[tt,SpinMatrixObject];
Ga /: Ga[mu_, s_,r_] tt_[u_,s_] := TensDot[tt, Ga[mu]][u, r] /; MatchQ[tt,SpinMatrixObject];

ProjP /: ProjP[r_,s_] tt_[s_,u_] := TensDot[ProjP, tt][r, u] /; MatchQ[tt,SpinMatrixObject];
ProjP /: ProjP[s_,r_] tt_[u_,s_] := TensDot[tt, ProjP][u, r] /; MatchQ[tt,SpinMatrixObject];
ProjM /: ProjM[r_,s_] tt_[s_,u_] := TensDot[ProjM, tt][r, u] /; MatchQ[tt,SpinMatrixObject];
ProjM /: ProjM[s_,r_] tt_[u_,s_] := TensDot[tt, ProjM][u, r] /; MatchQ[tt,SpinMatrixObject];

ProjP /: ProjP[mu_, r_,s_]  tt_[s_,u_] := TensDot[ProjP[mu], tt][r, u] /; MatchQ[tt,SpinMatrixObject];
ProjP /: ProjP[mu_, s_,r_] tt_[u_,s_] := TensDot[tt, ProjP[mu]][u, r] /; MatchQ[tt,SpinMatrixObject];
ProjM /: ProjM[mu_, r_,s_] tt_[s_,u_] := TensDot[ProjM[mu], tt][r, u] /; MatchQ[tt,SpinMatrixObject];
ProjM /: ProjM[mu_, s_,r_] tt_[u_,s_] := TensDot[tt, ProjM[mu]][u, r] /; MatchQ[tt,SpinMatrixObject];

MakeSlashedMatrix[expr_] := Block[{tempexpr},
   tempexpr = If[FR$FExpand,Expand[expr],Expand[Expand[Expand[expr,Lorentz],FV],TensDot]];
   tempexpr = tempexpr /. {TensDot[g1__, delta, g2___] :> TensDot[g1,g2],
         TensDot[g1___, delta, g2__] :> TensDot[g1,g2],
         TensDot[delta] :> delta};
   tempexpr = tempexpr /. {FV[kk_, mumu_] Ga[mumu_, rr___] :> SlashedP[kk, rr], 
         FV[kk_, mumu_] TensDot[gg1___, Ga[mumu_], gg2___][r_,s_] :> TensDot[gg1, SlashedP[kk], gg2][r,s]}/.{
         FV[kk_, mumu_] TensDot[gg1___, Ga[mumu_], gg2___] :> TensDot[gg1, SlashedP[kk], gg2]}];


(* ::Section::Closed:: *)
(*Levi - Civita*)


(***********************************************************************************************************************)

(* Here we declare the Levi - Civita tensor Eps *)

(* Contraction properties of the Levi - Civita *)

Eps /: Eps[ii___, Except[Index[___]|_?NumericQ, jj_], kk___] f_[aa___, Index[name_, jj_], cc___] := Eps[ii, Index[name, jj], kk] f[aa, Index[name, jj], cc];
Eps /: Eps[ii___, Except[Index[___]|_?NumericQ, jj_], kk___] f_[aa___, Index[name_, jj_], cc___][ind___] := Eps[ii, Index[name, jj], kk] f[aa, Index[name, jj], cc][ind];
Eps /: Eps[ii___, Except[Index[___]|_?NumericQ, jj_], kk___] f_[aa___, g_[xx___,Index[name_, jj_], yy___], cc___] := Eps[ii, Index[name, jj], kk] f[aa, g[xx, Index[name, jj], yy], cc];
Eps /: Eps[ii___, Except[Index[___]|_?NumericQ, jj_], kk___] f_[aa___, g_[xx___,Index[name_, jj_], yy___], cc___][ind___] := Eps[ii, Index[name, jj], kk] f[aa, g[xx, Index[name, jj], yy], cc][ind];

Eps[ii___, Except[_Index|_Done[Index]|_FV, jj_?(Not[NumericQ[#]]&)], kk___, Index[name_, ll_], mm___] := Eps[ii,Index[name,jj],kk,Index[name,ll],mm];
Eps[ii___, Index[name_, ll_], kk___, Except[_Index|_Done[Index]|_FV, jj_?(Not[NumericQ[#]]&)], mm___] := Eps[ii,Index[name,ll],kk,Index[name,jj],mm];

Eps /: Eps[i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&),j_,k_] Eps[i_,m_,n_] := IndexDelta[j,m]IndexDelta[k,n] - IndexDelta[j,n]IndexDelta[k,m];
Eps /: Eps[i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&),j_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]])&),k_] Eps[i_,j_,n_] := 2 IndexDelta[k,n];
Eps /: Eps[i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&),j_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]])&),k_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]])&)] Eps[i_,j_,k_] := 6;
Eps /: Power[Eps[i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&), j_, k_], 2] := 6;

Eps /: Eps[i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&),j_,k_] Eps[m_,n_,i_] := Eps[i,j,k] Eps[i,m,n];
Eps /: Eps[i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&),j_,k_] Eps[m_,i_,n_] := Eps[i,j,k] Eps[i,n,m];
Eps /: Eps[j_,i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&),k_] Eps[m_,i_,n_] := Eps[i,k,j] Eps[i,n,m];
Eps /: Eps[j_,i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&),k_] Eps[m_,n_,i_] := Eps[i,k,j] Eps[i,m,n];
Eps /: Eps[j_,k_,i_?((Not[NumericQ[#]] && Not[MatchQ[#, Index[_, _?NumericQ]]] && Not[MatchQ[#, Done[Index][_, _?NumericQ]]])&)] Eps[m_,n_,i_] := Eps[i,j,k] Eps[i,m,n];

Eps /: Eps[___, i_,___,j_,___] FV[a_,i_] FV[a_,j_] := 0;
Eps /: Eps[___, i_,___,j_,___] del[del[_,i_],j_] := 0;
Eps /: Eps[___, i_,___,j_,___] del[del[_,j_],i_] := 0;

Eps[xx___, Index[name_, i_?NumericQ], yy___] := Eps[xx, i, yy];

(*added by celine*)
(* Benj: problem with the global sign. I changed it to match Peskin and Schroeder *)

(* Liam commenting out to test *)



Eps /: Eps[___, i_,___,i_,___] := 0;


Eps /: Eps[ i_,k_,l_,j_] Eps[ i_,g_,h_,j_] := -2 (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,i_,l_,j_] Eps[ i_,g_,h_,j_] := 2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,l_,i_,j_] Eps[ i_,g_,h_,j_] := -2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,l_,j_,i_] Eps[ i_,g_,h_,j_] := 2 (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ i_,k_,l_,j_] Eps[ g_,i_,h_,j_] := 2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,i_,l_,j_] Eps[ g_,i_,h_,j_] := -2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,l_,i_,j_] Eps[ g_,i_,h_,j_] := 2 (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,l_,j_,i_] Eps[ g_,i_,h_,j_] := -2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ i_,k_,l_,j_] Eps[ g_,h_,i_,j_] := -2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,i_,l_,j_] Eps[ g_,h_,i_,j_] := 2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,l_,i_,j_] Eps[ g_,h_,i_,j_] := -2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,l_,j_,i_] Eps[ g_,h_,i_,j_] := 2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ i_,k_,l_,j_] Eps[ g_,h_,j_,i_] := 2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,i_,l_,j_] Eps[ g_,h_,j_,i_] := -2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,l_,i_,j_] Eps[ g_,h_,j_,i_] := 2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);
Eps /: Eps[ k_,l_,j_,i_] Eps[ g_,h_,j_,i_] := -2  (ME[k,g]ME[l,h]-ME[k,h]ME[l,g]);



(* Reordering of the Levi  - Civita *)

OrderEps[expr_] := expr //. Eps -> (Signature[List[##]]*(Epstmp @@ Sort[List[##]]) &) //. Epstmp -> Eps;

KillIndex[xx__] := {xx} //. Index[_, k_] -> k; 

Eps[xx__] := Signature[{xx}] /; (And @@ (NumericQ /@ KillIndex[xx]));

$TensClass[Eps] = MR$LeviCivita;
numQ[Eps[___]] := True;
TensQ[Eps[___]] := True;
(* MW edit *)
TensQ[Eps] = True;


(* ::Section::Closed:: *)
(*Metric tensor and four-vectors*)


(************************************************************************************************)

(*  The metric tensor ME *)

FR$SpaceTimeDimension = 4;

ME[Except[Index[___], mu_], Index[Lorentz, nu_]] := ME[Index[Lorentz, mu], Index[Lorentz, nu]];
ME[Index[Lorentz, mu_], Except[Index[___], nu_]] := ME[Index[Lorentz, mu], Index[Lorentz, nu]];
ME[Except[Index[___], mu_], Except[Index[___], nu_]] := ME[Index[Lorentz, mu], Index[Lorentz, nu]];

SetAttributes[ME, Orderless];

SetAttributes[SP, Orderless];

(* Adding special cases of metric contractions *)

(***********)

(*(* Fully contracted *)
ME /:ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]]:=FR$SpaceTimeDimension^2;
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,sig_]]:=FR$SpaceTimeDimension;
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,sig_]] ME[Index[Lorentz,nu_],Index[Lorentz,rho_]]:=FR$SpaceTimeDimension;
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,sig_]] ME[Index[Lorentz,nu_],Index[Lorentz,rho_]]:=FR$SpaceTimeDimension;

(* Two Indices left *)
ME/: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,eps_]]:=FR$SpaceTimeDimension ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,eps_]]:=ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,eps_]] ME[Index[Lorentz,nu_],Index[Lorentz,rho_]]:=ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,eps_]] ME[Index[Lorentz,nu_],Index[Lorentz,rho_]]:=ME[Index[Lorentz,sig],Index[Lorentz,eps]];*)

(* Adding redundant looking identites to see if this will fix bug *)
(*ME/:ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,eps_]]:=ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME/:ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,eps_]]:=ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME/:ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,eps_]] ME[Index[Lorentz,nu_],Index[Lorentz,rho_]]:=ME[Index[Lorentz,sig],Index[Lorentz,eps]];*)

(* Try permuting orders *)
(*ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]]ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]]  ME[Index[Lorentz,nu_],Index[Lorentz,eps_]]:=ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]]ME[Index[Lorentz,nu_],Index[Lorentz,eps_]]ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]]  :=ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]]ME[Index[Lorentz,nu_],Index[Lorentz,eps_]]ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] :=ME[Index[Lorentz,sig],Index[Lorentz,eps]];*)

(* Try doing backwards *)
(*ME/: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,eps_]]ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] :=FR$SpaceTimeDimension ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,eps_]]ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] :=ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,eps_]] ME[Index[Lorentz,nu_],Index[Lorentz,rho_]]ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] :=ME[Index[Lorentz,sig],Index[Lorentz,eps]];
ME /:  ME[Index[Lorentz,mu_],Index[Lorentz,eps_]] ME[Index[Lorentz,nu_],Index[Lorentz,rho_]]ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,sig_]]:=ME[Index[Lorentz,sig],Index[Lorentz,eps]];*)


(* Four Indices left *)
(*ME/: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,kap_],Index[Lorentz,eps_]]:=FR$SpaceTimeDimension ME[Index[Lorentz,rho],Index[Lorentz,sig]] ME[Index[Lorentz,kap],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,kap_]] ME[Index[Lorentz,nu_],Index[Lorentz,eps_]]:=ME[Index[Lorentz,rho],Index[Lorentz,sig]] ME[Index[Lorentz,kap],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,nu_]] ME[Index[Lorentz,rho_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,eps_]] ME[Index[Lorentz,nu_],Index[Lorentz,kap_]]:=ME[Index[Lorentz,rho],Index[Lorentz,sig]] ME[Index[Lorentz,kap],Index[Lorentz,eps]];
ME /: ME[Index[Lorentz,mu_],Index[Lorentz,rho_]] ME[Index[Lorentz,nu_],Index[Lorentz,sig_]] ME[Index[Lorentz,mu_],Index[Lorentz,eps_]] ME[Index[Lorentz,nu_],Index[Lorentz,kap_]]:=ME[Index[Lorentz,rho],Index[Lorentz,sig]] ME[Index[Lorentz,kap],Index[Lorentz,eps]];*)


(**********)

MEME /: func_[x___, mu_, y___]MEME[mu_, nu_] := func[x, nu, y];
MEME /: func_[x___, nu_, y___]MEME[mu_, nu_] := func[x, mu, y];
MEME /: func1_[x___, func2_[y___, mu_, zz___], u___]MEME[mu_, nu_] := func1[x, func2[y, nu, zz], u];
MEME /: func1_[x___, func2_[y___, nu_, zz___], u___]MEME[mu_, nu_] := func1[x, func2[y, mu, zz], u];
MEME /: func1_[x___, func2_[y___, mu_, zz___], u___][ind___]MEME[mu_, nu_] := func1[x, func2[y, nu, zz], u][ind];
MEME /: func1_[x___, func2_[y___, nu_, zz___], u___][ind___]MEME[mu_, nu_] := func1[x, func2[y, mu, zz], u][ind];
MEME /: func1_[x___, func2_[y___, mu_, zz___][ind___], u___]MEME[mu_, nu_] := func1[x, func2[y, nu, zz][ind], u];
MEME /: func1_[x___, func2_[y___, nu_, zz___][ind___], u___]MEME[mu_, nu_] := func1[x, func2[y, mu, zz][ind], u];
MEME /: func1_[x___, func2_[y___, mu_, zz___][ind1___], u___][ind2___]MEME[mu_, nu_] := func1[x, func2[y, nu, zz][ind1], u][ind2];
MEME /: func1_[x___, func2_[y___, nu_, zz___][ind1___], u___][ind2___]MEME[mu_, nu_] := func1[x, func2[y, mu, zz][ind1], u][ind2];

ME /: Power[ME[mu_, nu_], 2] := FR$SpaceTimeDimension;
ME /: Power[ME[Index[Lorentz, mu__], Index[Lorentz, nu__]], 2] := FR$SpaceTimeDimension;

FVFV /: FVFV[k_, mu_]FVFV[p_, mu_] := SP[k, p];


(* Added by Benj *)
ME[mu_,nu_] si[mu_,a_,b_]^:=si[nu,a,b];
ME[nu_,mu_] si[mu_,a_,b_]^:=si[nu,a,b];

ME[mu_,nu_] sibar[mu_,a_,b_]^:=sibar[nu,a,b];
ME[nu_,mu_] sibar[mu_,a_,b_]^:=sibar[nu,a,b];
ME[mu_,nu_] del[f_,mu_]^:=del[f,nu];
ME[nu_,mu_] del[f_,mu_]^:=del[f,nu];

ME[mu_,nu_] ff_[indx___,del[f_,mu_],indy___]^:=ff[indx,del[f,nu],indy];
ME[nu_,mu_] ff_[indx___,del[f_,mu_],indy___]^:=ff[indx,del[f,nu],indy];
ME[mu_,nu_]f_?(FieldQ[#]===True&)[indx___,mu_,inds___] ^:=f[indx,nu,inds];
ME[nu_,mu_]f_?(FieldQ[#]===True&)[indx___,mu_,inds___] ^:=f[indx,nu,inds];

(* Added by Celine *)

ME[Index[Lorentz,mu__],Index[Lorentz,nu__]] Eps[Index[Lorentz,mu__],Index[Lorentz,del__],Index[Lorentz,rho__],Index[Lorentz,gam__]]^:=Eps[Index[Lorentz,nu],Index[Lorentz,del],Index[Lorentz,rho],Index[Lorentz,gam]];
ME[Index[Lorentz,mu__],Index[Lorentz,nu__]] Eps[Index[Lorentz,del__],Index[Lorentz,mu__],Index[Lorentz,rho__],Index[Lorentz,gam__]]^:=Eps[Index[Lorentz,del],Index[Lorentz,nu],Index[Lorentz,rho],Index[Lorentz,gam]];
ME[Index[Lorentz,mu__],Index[Lorentz,nu__]] Eps[Index[Lorentz,del__],Index[Lorentz,rho__],Index[Lorentz,mu__],Index[Lorentz,gam__]]^:=Eps[Index[Lorentz,del],Index[Lorentz,rho],Index[Lorentz,nu],Index[Lorentz,gam]];
ME[Index[Lorentz,mu__],Index[Lorentz,nu__]] Eps[Index[Lorentz,del__],Index[Lorentz,rho__],Index[Lorentz,gam__],Index[Lorentz,mu__]]^:=Eps[Index[Lorentz,del],Index[Lorentz,rho],Index[Lorentz,gam],Index[Lorentz,nu]];

ME[Index[Lorentz,mu__],Index[Lorentz,nu__]] Eps[del___,Index[Lorentz,mu__],rho___,Index[Lorentz,nu__],gam___]^:=0;

ME /: ME[mu_, mu_] := FR$SpaceTimeDimension;

ME[Index[Lorentz,mu__],Index[Lorentz,nu__]] FV[aa_,Index[Lorentz,mu__]]^:=FV[aa,Index[Lorentz,nu]];

FV[bb_,Index[Lorentz,mu__]] FV[aa_,Index[Lorentz,mu__]]^:=SP[bb,aa];
FV[aa_,Index[Lorentz,mu__]] FV[aa_,Index[Lorentz,mu__]]^:=SP[aa,aa];


(* ::Section::Closed:: *)
(*Sorting*)


(* Here we declare the properties for structure constants *)

SortStrucConst[ff_][a_, b_, c_] := Signature[{a, b, c}]ff @@ Sort[{a, b, c}];
SortStrucConst[ff_[a_, b_, c_]] := Signature[{a, b, c}]ff @@ Sort[{a, b, c}];

(**************************************************************************************)

(* How to deal with totally symmetric tensors *)

SortSymTens[dd_][ind__]:= dd @@ Sort[{ind}];
SortSymTens[dd_[ind__]]:= dd @@ Sort[{ind}];

(* How to deal with totally symmetric tensors *)

SortAntiSymTens[dd_][ind__] := Signature[{ind}] dd @@ Sort[{ind}];
SortAntiSymTens[dd_[ind__]] := Signature[{ind}] dd @@ Sort[{ind}];





(* ::Section:: *)
(*Hermitian conjugate*)


(***********************************************************************************************************************)

(* Here we declare the our hermitian conjugate, HC *)
(* Notice that Mathmatica has also a hermitian conjugate, ConjugateTranpose *)

HC[Except[_[___], field_?((FieldQ[#] && (Not[GhostFieldQ[#] === True]) && (Not[Spin32FieldQ[#] === True]) && (Not[DiracFieldQ[#] === True]) && (Not[MajoranaFieldQ[#] === True]))&)]] := anti[field];

HC[Except[_[___], field_?(((Spin32FieldQ[#] === True) || (DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True)) && AntiFieldQ[#] === True &)]] := Ga[0].anti[field];
HC[Except[_[___], field_?(((Spin32FieldQ[#] === True) || (DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True)) && Not[AntiFieldQ[#]] === True &)]] := anti[field].Ga[0];

HC[CC[field_?(AntiFieldQ[#] === True &)]] := Ga[0].CC[anti[field]];
HC[CC[field_?(AntiFieldQ[#] =!= True &)]] := CC[anti[field]].Ga[0];

HC[Except[_[___], field_?((AntiFieldQ[#] && (GhostFieldQ[#] === True))&)]] := FR$HCMinus[HCanti[GBToGh[GhToGB[field]]]];
HC[Except[_[___], field_?((Not[AntiFieldQ[#]] && (GhostFieldQ[#] === True))&)]] := GBToGh[HCanti[GhToGB[field]]];

HC[FR$CT]:=FR$CT;


Unprotect[Dot];

Dot[tt__, FR$HCMinus[field_]] := -Dot[tt, field];
Dot[tt__, FR$HCMinus[field_][ind___]] := -Dot[tt, field[ind]];
Dot[tt__, del[FR$HCMinus[field_][ind___],mu_]] := -Dot[tt, del[field[ind],mu]];
Dot[tt__, del[FR$HCMinus[field_],mu_]] := -Dot[tt, del[field,mu]];
Dot[FR$HCMinus[field_], tt__] := -Dot[field, tt];
Dot[FR$HCMinus[field_][ind___], tt__] := -Dot[field[ind], tt];
Dot[del[FR$HCMinus[field_],mu_], tt__] := -Dot[del[field,mu], tt];
Dot[del[FR$HCMinus[field_][ind___],mu_], tt__] := -Dot[del[field[ind],mu], tt];

Dot[gh1_?(((Not[AntiFieldQ[#]] === True) && (GhostFieldQ[#] === True))&)[ind1___], gh2_?(((AntiFieldQ[#] === True) && GhostFieldQ[#] === True)&)[ind2___]] := -Dot[gh2[ind2], gh1[ind1]];
Dot[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&)[ind1___], del[gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&)[ind2___], mu_]] := -Dot[del[gh2[ind2], mu], gh1[ind1]];
Dot[del[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&)[ind1___], mu_], gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&)[ind2___]] := -Dot[gh2[ind2], del[gh1[ind1], mu]];
Dot[del[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&)[ind1___], mu_], del[gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&)[ind2___], mu_]] := -Dot[del[gh2[ind2], mu], del[gh1[ind1], mu]];
Dot[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&)[ind1___], del[del[gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&)[ind2___], mu_],nu_]] := -Dot[del[del[gh2[ind2], mu],nu], gh1[ind1]];
Dot[del[del[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&)[ind1___], mu_],nu_], gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&)[ind2___]] := -Dot[gh2[ind2], del[del[gh1[ind1], mu],nu]];

Dot[gh1_?(((Not[AntiFieldQ[#]] === True) && (GhostFieldQ[#] === True))&), gh2_?((AntiFieldQ[#] === True && (GhostFieldQ[#] === True))&)] := -Dot[gh2, gh1];
Dot[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&), del[gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&), mu_]] := -Dot[del[gh2, mu], gh1];
Dot[del[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&), mu_], gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&)] := -Dot[gh2, del[gh1, mu]];
Dot[del[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&), mu_], del[gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&), mu_]] := -Dot[del[gh2, mu], del[gh1, mu]];
Dot[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&), del[del[gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&), mu_],nu_]] := -Dot[del[del[gh2, mu],nu], gh1];
Dot[del[del[gh1_?(((Not[AntiFieldQ[#]] === True) && GhostFieldQ[#] === True)&), mu_],nu_], gh2_?((AntiFieldQ[#] === True && GhostFieldQ[#] === True)&)] := -Dot[gh2, del[del[gh1, mu],nu]];

Protect[Dot];

HC[HC[tt_]] := tt;


HC /: TensDot[xx___, HC[tt_?(UnitaryQ)], tt_, yy___] := TensDot[xx, yy];
HC /: TensDot[xx___, tt_, HC[tt_?(UnitaryQ)], yy___] := TensDot[xx, yy];

HC[Dot[xx_, y__]] := Dot @@ (HC /@ Reverse[List[xx,y]]);
HC[TensDot[xx_, y___]][r_,s_] := (HC /@ Reverse[TensDot[xx, y]])[s,r];
(*HC[a_ + b_] := HC[a] + HC[b];*)
(*HC[a_ * b_] := HC[a] * HC[b];*)
HC[sum_Plus]:=HC /@ sum;
(* CD Feb 2017: Added Expand[ ] *)
HC[prod_Times] := Expand[HC /@ prod]; 
(* End CD Feb 2017 *)
(*/; Not[MatchQ[a, _?(TensQ)[_,_]] || MatchQ[b, _?(TensQ)[_,_]]] ;
HC[tt_?TensQ[i1_,i2_]ff_[jj___,i2_, kk___]] := HC[ff[jj, i2, kk]]HC[tt[i2,i1]];*)
HC[Power][a_, n_] := Power[HC[a], n];

HC[Except[TensDot[_,__], tt_?((TensQ[#] && Not[StrucConstQ[#] === True] && Not[GaugeMatrixQ[#] === True])&)]][ind__, r_,s_] := Conjugate[tt[ind,r,s]] /; (Length[List[ind]] + 2) == Length[$IndList[tt]];
HC[Except[TensDot[_,__], tt_?((TensQ[#] && Not[StrucConstQ[#] === True] && Not[GaugeMatrixQ[#] === True])&)]][r_,s_] := Conjugate[tt[r,s]] /; Length[$IndList[tt]] == 2;
HC[tt_?((StrucConstQ[#] === True)&)][r_,s_,t_] := tt[r,s,t];
HC[tt_?((GaugeMatrixQ[#] === True)&)][r_,s_,t_] := tt[r,t,s];
HC[tt_?((GaugeMatrixQ[#] === True)&)][r_] := tt[r];

HC[tt_?(TensQ[#] && ($IndList[#]===List[_])&)][r_] := Conjugate[tt[r]];

(* !!!! HCPI is needed to get the right behavior for stuff like ubar.HC[CKM].d -> ubar[i].d[j] CKM[j,i]*, i.e. we need to flip the order of the indices!!!! *)
(* This is called by PutIndices *)
HCPI[Except[TensDot[_,__], tt_?((TensQ[#] && Not[StrucConstQ[#] === True] && Not[GaugeMatrixQ[#] === True])&)]][ind__, r_,s_] := Conjugate[tt[ind,s,r]] /; (Length[List[ind]] + 2) == Length[$IndList[tt]];
HCPI[Except[TensDot[_,__], tt_?((TensQ[#] && Not[StrucConstQ[#] === True] && Not[GaugeMatrixQ[#] === True])&)]][r_,s_] := Conjugate[tt[s,r]] /; Length[$IndList[tt]] == 2;
$TensClass[HCPI[t_]] := $TensClass[t];
$TensClass[HCPI[t_][ind___]] := $TensClass[t];
TensQ[HCPI[tt_]] := TensQ[tt];

HC[ProjP][r_,s_] := ProjP[s,r];
HC[ProjM][r_,s_] := ProjM[s,r];

HC /: Dot[tt1___, HC[ProjM], tt2___] := Dot[tt1, ProjM, tt2];
HC /: Dot[tt1___, HC[ProjP], tt2___] := Dot[tt1, ProjP, tt2];
HC /: TensDot[tt1___, HC[ProjM], tt2___] := TensDot[tt1, ProjM, tt2];
HC /: TensDot[tt1___, HC[ProjP], tt2___] := TensDot[tt1, ProjP, tt2];
HC[Ga][mu_?(#=!=5 &)] := Ga[0].Ga[mu].Ga[0];
HC[Ga[5]]:= Ga[5];
HC[Ga][mu_, s_, r_] := Module[{s1, r1}, Ga[0,r,r1]Ga[mu, r1, s1] Ga[0,s1,s]];
HC[Ga][5, s_, r_] := Ga[5, r, s] ;
HC[Sig][mu_, nu_] := Ga[0].Sig[mu, nu].Ga[0];
(*Edit Benj: a zero was missing in the rule below *)
HC[Sig][mu_, nu_, r_, s_] := Module[{r1,s1}, Ga[0,s,s1]Sig[mu, nu, s1, r1]Ga[0,r1,r]];

HC[Eps[ind__]] := Eps[ind];
HC[ME[mu_, nu_]] := ME[mu, nu];
HC[FV[k_, mu_]] := FV[k, mu];
HC[SP[k1_, k2_]] := SP[k1,k2];

HC[Conjugate[pp_?numQ]] := pp;

Unprotect[Conjugate];
Conjugate[pp_?(((numQ[#] === True) && Not[CnumQ[#] === True])&)] := pp;
$TensClass[Conjugate[t_]] := $TensClass[t];
$TensClass[Conjugate[t_][ind___]] := $TensClass[t];
Protect[Conjugate];



TensQ[HC[t_]] := TensQ[t];
FieldQ[HC[f_]] := FieldQ[f];
FermionQ[HC[f_]] := FermionQ[f];
BosonQ[HC[f_]] := BosonQ[f];
AntiFieldQ[HC[f_]] := Not[AntiFieldQ[f]];
ScalarFieldQ[HC[f_]] := ScalarFieldQ[f];
DiracFieldQ[HC[f_]] := DiracFieldQ[f];
SpinorFieldQ[HC[f_]] := SpinorFieldQ[f];
VectorFieldQ[HC[f_]] := VectorFieldQ[f];
MajoranaFieldQ[HC[f_]] := MajoranaFieldQ[f];
Spin2FieldQ[HC[f_]] := Spin2FieldQ[f];
Spin32FieldQ[HC[f_]] := Spin32FieldQ[f];
RSpin32FieldQ[HC[f_]] := RSpin32FieldQ[f];
CSpin32FieldQ[HC[f_]] := CSpin32FieldQ[f];
RFermionFieldQ[HC[f_]] := RFermionFieldQ[f];
CFermionFieldQ[HC[f_]] := CFermionFieldQ[f];
(*HC[f_?(Not[AntiFieldQ[#]]&)] := f;*)


$TensClass[HC[t_]] := $TensClass[t];
$TensClass[HC[t_][ind___]] := $TensClass[t];
HC[t_?((FieldQ[#]||TensQ[#])&)[ind___]] := HC[t][ind];


(* CD: Feb 2017: Added /; FreeQ[x, _?GammaMatrixQ], because otherwise it triggers on Gamma matrices without transposing them.
   Old version is commented out.
*)
(*HC[Except[_?(TensQ)[___], x_?(CnumQ[#] === True &)]] := Conjugate[x];
HC[Except[_?(TensQ)[___], x_?((Not[CnumQ[#] === True] && (numQ[#] === True))&)]] := x;*)
HC[Except[_?(TensQ)[___], x_?(CnumQ[#] === True &)]] := Conjugate[x] /; FreeQ[x, _?GammaMatrixQ];
HC[Except[_?(TensQ)[___], x_?((Not[CnumQ[#] === True] && (numQ[#] === True))&)]] := x /; FreeQ[x, _?GammaMatrixQ];
(* End CD Feb 2017 *)
HC[x_?(Element[#,Reals] === True &)] := x;
HC[x_?(Element[#,Complexes] === True &)] := Conjugate[x];
HC[Complex][a_,b_] := Conjugate[Complex[a,b]];
HC[Rational][a_,b_] := Rational[a,b];
(*HC[f_?(Not[CompTensQ[#]]&)] := f;*)

(* MW edit: this rule only, if the indices are not written out *)
HC[t_?((HermitianQ[#] && SymbolQ[#])&)] := t;

HC[t_?VectorQ] := HC /@ t;
HC[t_?MatrixQ] := HC /@ Transpose[t];

HC[del[f_, mu_]] := del[HC[f], mu];

HC[Power[a_, b_]] := Power[HC[a], HC[b]];
HC[Exp[a_]] := Exp[HC[a]];

(*HC[ProjP][r_,s_] := ProjM[s, r];
HC[ProjM][r_,s_] := ProjP[s, r];*)


(* Added by Benj *)

HC[p_?(numQ[#[_]]===True&)][in_]:=Conjugate[p[in]];

HC[f_?(SuperfieldQ[#]===True&)[inds___]]:=HC[f][inds];


(* ::Section:: *)
(*Hermitian conjugate for definitions*)


(***********************************************************************************************************************)

(* Here we declare the our hermitian conjugate, HCanti. It is a version of HC, which deals differently with the ghosts *)
(* Notice that Mathmatica has also a hermitian conjugate, ConjugateTranpose *)

HCanti[Except[_[___], field_?(FieldQ[#]  && (Not[Spin32FieldQ[#] === True]) && (Not[DiracFieldQ[#] === True]) && (Not[MajoranaFieldQ[#] === True]) &)]] := anti[field];


HCanti[Except[_[___], field_?(((Spin32FieldQ[#] === True) || (DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True)) && AntiFieldQ[#] === True &)]] := Ga[0].anti[field];
HCanti[Except[_[___], field_?(((Spin32FieldQ[#] === True) || (DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True)) && Not[AntiFieldQ[#]] === True &)]] := anti[field].Ga[0];

HCanti[CC[field_?(AntiFieldQ[#] === True &)]] := Ga[0].CC[anti[field]];
HCanti[CC[field_?(AntiFieldQ[#] =!= True &)]] := CC[anti[field]].Ga[0];

HCanti[HCanti[tt_]] := tt;


HCanti /: TensDot[xx___, HCanti[tt_?(UnitaryQ)], tt_, yy___] := TensDot[xx, yy];
HCanti /: TensDot[xx___, tt_, HCanti[tt_?(UnitaryQ)], yy___] := TensDot[xx, yy];

HCanti[Dot[xx_, y__]] := Dot @@ (HCanti /@ Reverse[List[xx,y]]);
HCanti[TensDot[xx_, y___]][r_,s_] := (HCanti /@ Reverse[TensDot[xx, y]])[s,r];
HCanti[a_ + b_] := HCanti[a] + HCanti[b];
HCanti[a_ * b_] := HCanti[a] * HCanti[b]; 
(*/; Not[MatchQ[a, _?(TensQ)[_,_]] || MatchQ[b, _?(TensQ)[_,_]]] ;
HCanti[tt_?TensQ[i1_,i2_]ff_[jj___,i2_, kk___]] := HCanti[ff[jj, i2, kk]]HCanti[tt[i2,i1]];*)
HCanti[Power][a_, n_] := Power[HCanti[a], n];
HCanti[Exp[a_]] := Exp[HCanti[a]];

HCanti[Except[TensDot[_,__], tt_?((TensQ[#] && Not[StrucConstQ[#] === True] && Not[GaugeMatrixQ[#] === True])&)]][ind__, r_,s_] := Conjugate[tt[ind,r,s]] /; (Length[List[ind]] + 2) == Length[$IndList[tt]];
HCanti[Except[TensDot[_,__], tt_?((TensQ[#] && Not[StrucConstQ[#] === True] && Not[GaugeMatrixQ[#] === True])&)]][r_,s_] := Conjugate[tt[r,s]] /; Length[$IndList[tt]] == 2;
HCanti[tt_?((StrucConstQ[#] === True)&)][r_,s_,t_] := tt[r,s,t];
HCanti[tt_?((GaugeMatrixQ[#] === True)&)][r_,s_,t_] := tt[r,t,s];
HCanti[tt_?((GaugeMatrixQ[#] === True)&)][r_] := tt[r];

(* !!!! HCantiPI is needed to get the right behavior for stuff like ubar.HCanti[CKM].d -> ubar[i].d[j] CKM[j,i]*, i.e. we need to flip the order of the indices!!!! *)
(* This is called by PutIndices *)
HCantiPI[Except[TensDot[_,__], tt_?((TensQ[#] && Not[StrucConstQ[#] === True] && Not[GaugeMatrixQ[#] === True])&)]][ind__, r_,s_] := Conjugate[tt[ind,s,r]] /; (Length[List[ind]] + 2) == Length[$IndList[tt]];
HCantiPI[Except[TensDot[_,__], tt_?((TensQ[#] && Not[StrucConstQ[#] === True] && Not[GaugeMatrixQ[#] === True])&)]][r_,s_] := Conjugate[tt[s,r]] /; Length[$IndList[tt]] == 2;
$TensClass[HCantiPI[t_]] := $TensClass[t];
$TensClass[HCantiPI[t_][ind___]] := $TensClass[t];
TensQ[HCantiPI[tt_]] := TensQ[tt];

HCanti[ProjP][r_,s_] := ProjP[s,r];
HCanti[ProjM][r_,s_] := ProjM[s,r];
HCanti /: Dot[tt1___, HCanti[ProjM], tt2___] := Dot[tt1, ProjM, tt2];
HCanti /: Dot[tt1___, HCanti[ProjP], tt2___] := Dot[tt1, ProjP, tt2];
HCanti[Ga][mu_?(# =!= 5&)] := Ga[0].Ga[mu].Ga[0];
HCanti[Ga][5] = Ga[5];
HCanti[Ga][mu_?(#=!=5&), s_, r_] := Module[{s1,r1}, Ga[0,r,r1]Ga[mu, r1, s1]Ga[0,s1,s]];
HCanti[Ga][5, s_, r_] :=Ga[5, r, s];
HCanti[Sig][mu_, nu_] := Ga[0].Sig[mu, nu].Ga[0];
HCanti[Sig][mu_, nu_, r_, s_] := Module[{r1,s1},Ga[0,s,s1]Sig[mu, nu, s1, r1]Ga[0,r1,r]];

HCanti[Eps[ind__]] := Eps[ind];
HCanti[ME[mu_, nu_]] := ME[mu, nu];
HCanti[FV[k_, mu_]] := FV[k, mu];
HCanti[SP[k1_, k2_]] := SP[k1,k2];

HCanti[Conjugate[pp_?numQ]] := pp;

Unprotect[Conjugate];
Conjugate[pp_?((numQ[#] && Not[CnumQ[#]])&)] := pp;
$TensClass[Conjugate[t_]] := $TensClass[t];
$TensClass[Conjugate[t_][ind___]] := $TensClass[t];
Protect[Conjugate];



TensQ[HCanti[t_]] := TensQ[t];
FieldQ[HCanti[f_]] := FieldQ[f];
FermionQ[HCanti[f_]] := FermionQ[f];
BosonQ[HCanti[f_]] := BosonQ[f];
AntiFieldQ[HCanti[f_]] := Not[AntiFieldQ[f]];
ScalarFieldQ[HCanti[f_]] := ScalarFieldQ[f];
DiracFieldQ[HCanti[f_]] := DiracFieldQ[f];
SpinorFieldQ[HCanti[f_]] := SpinorFieldQ[f];
VectorFieldQ[HCanti[f_]] := VectorFieldQ[f];
MajoranaFieldQ[HCanti[f_]] := MajoranaFieldQ[f];
Spin2FieldQ[HCanti[f_]] := Spin2FieldQ[f];
Spin32FieldQ[HCanti[f_]] := Spin32FieldQ[f];
RSpin32FieldQ[HCanti[f_]] := RSpin32FieldQ[f];
CSpin32FieldQ[HCanti[f_]] := CSpin32FieldQ[f];
RFermionFieldQ[HCanti[f_]] := RFermionFieldQ[f];
CFermionFieldQ[HCanti[f_]] := CFermionFieldQ[f];

(* change to check for source of lepton/W/vl vertex bug - liam *)
(*HCanti[f_?((Not[AntiFieldQ[#]] && Not[MajoranaFieldQ[#]])&)] := f;*)


$TensClass[HCanti[t_]] := $TensClass[t];
$TensClass[HCanti[t_][ind___]] := $TensClass[t];
HCanti[t_?((FieldQ[#]||TensQ[#]) && (Not[# === WeylL] && Not[# === WeylR] )&)[ind___]] := HCanti[t][ind];


HCanti[Except[_?(TensQ)[___], x_?(CnumQ)]] := Conjugate[x];
HCanti[Except[_?(TensQ)[___], x_?((Not[CnumQ[#]] && numQ[#])&)]] := x;
HCanti[x_?(Element[#,Reals] === True &)] := x;
HCanti[x_?(Element[#,Complexes] === True &)] := Conjugate[x];
HCanti[Complex][a_,b_] := Conjugate[Complex[a,b]];
HCanti[Rational][a_,b_] := Rational[a,b];

(* change to check for source of lepton/W/vl vertex bug - liam *)
(*HCanti[f_?(Not[CompTensQ[#]]&)] := f;*)


HCanti[t_?(HermitianQ)] := t;

HCanti[List[t__]] := HCanti /@ List[t];

HCanti[del[f_, mu_]] := del[HCanti[f], mu];

HCanti[Power[a_, b_]] := Power[HCanti[a], HCanti[b]];


(*HCanti[ProjP][r_,s_] := ProjM[s, r];
HCanti[ProjM][r_,s_] := ProjP[s, r];*)


(* ::Section:: *)
(*Charge conjugation*)


FieldQ[CC[field_]] := FieldQ[field];
FieldComponentQ[CC[field_]] := FieldComponentQ[field];
DiracFieldQ[CC[field_]] := DiracFieldQ[field];
FermionQ[CC[field_]] := FermionQ[field];
BosonQ[CC[field_]] := BosonQ[field];
SpinorFieldQ[CC[field_]] := SpinorFieldQ[field];
AntiFieldQ[CC[field_]] := AntiFieldQ[field];
Spin32FieldQ[CC[field_]] := Spin32FieldQ[field];
RSpin32FieldQ[CC[field_]] := RSpin32FieldQ[field];
CSpin32FieldQ[CC[field_]] := CSpin32FieldQ[field];

FieldQ[CC[field_][___]] := FieldQ[field];
FieldComponentQ[CC[field_][___]] := True;
DiracFieldQ[CC[field_][___]] := DiracFieldQ[field];
FermionQ[CC[field_][___]] := FermionQ[field];
BosonQ[CC[field_][___]] := BosonQ[field];
SpinorFieldQ[CC[field_][___]] := SpinorFieldQ[field];
AntiFieldQ[CC[field_][___]] := AntiFieldQ[field];

NGaugeInd[CC[field_]] := NGaugeInd[field];

CC[f_?VectorFieldQ] := f;
CC[f_?ScalarFieldQ] := f;
CC[f_?Spin2FieldQ] := f;
(*CC[f_?MajoranaFieldQ] := f;*)
(*CC[f_?RSpin32FieldQ] := f;*)

$IndList[CC[ff_]] := $IndList[ff];

NInd[CC[ff_]] := NInd[ff];

CC/:qq_?QuantumNumberQ[CC[ff_]]:=-qq[ff];
CC/:qq_?QuantumNumberQ[CC[ff_][___]]:=-qq[ff];

CC[ff_?FieldQ[ind___]]:=CC[ff][ind];

CC[ff1_+ff2_] := CC[ff1] + CC[ff2];
CC[Except[_ProjP|_ProjM|Ga[5, _,_], aa_?(numQ)]*ff_] := Conjugate[aa] * CC[ff];

CC[ProjP[s_,r_]*ff_] := ProjM[s,r] * CC[ff];
CC[ProjM[s_,r_]*ff_] := ProjP[s,r] * CC[ff];
CC[Ga[5,s_,r_]*ff_] := -Ga[5, s,r] * CC[ff];

(*HC[CC[ff_]] := CC[HC[ff]];
HCanti[CC[ff_]] := CC[HCanti[ff]];*)
anti[CC[ff_]] := CC[anti[ff]];

CC[CC[ff_]] := ff;

Except[CC[_][___], CC[ff_?MajoranaFieldQ]] := E^(I MajoranaPhase[ff])ff;
CC[ff_?MajoranaFieldQ][ind__] := E^(I MajoranaPhase[ff])ff[ind];

Except[CC[_][___], CC[ff_?RSpin32FieldQ]] := E^(I MajoranaPhase[ff])ff;
CC[ff_?RSpin32FieldQ][ind__] := E^(I MajoranaPhase[ff])ff[ind];


CGa[Ga[mu_]] := -Ga[mu] /; (mu =!= Index[Lorentz, 5]) && (mu =!= 5);
CGa[Ga[mu_, s_, r_]] := -Ga[mu, s, r] /; (mu =!= Index[Lorentz, 5]) && (mu =!= 5);
CGa[Ga[5]] = Ga[5];
CGa[Ga[5, s_, r_]] := Ga[5, s, r];
CGa[ProjP] = ProjP;
CGa[ProjM] = ProjM;
CGa[SlashedP[i_]] := -SlashedP[i];
CGa[ProjP[s_, r_]] := ProjP[s,r];
CGa[ProjM[s_, r_]] := ProjM[s,r];
CGa[ProjP[mu_,s_,r_]] := -ProjM[mu,s,r];
CGa[ProjM[mu_,s_,r_]] := -ProjP[mu,s,r];
CGa[ProjP[mu_]] := -ProjM[mu];
CGa[ProjM[mu_]] := -ProjP[mu];
CGa[Sig[mu_, nu_]] := -Sig[mu, nu];
CGa[Sig[mu_, nu_,s_,r_]] := -Sig[mu, nu,s,r];
CGa[SlashedP[i_, s_, r_]] := -SlashedP[i,s,r];
CGa[IndexDelta[s1_, s2_]] := IndexDelta[s1,s2];

CGa[glist_List] := CGa /@ Reverse[glist];

CGa[TensDot[xx_, yy__][s_, r_]] := (CGa /@ TensDot[Sequence @@ Reverse[{yy}], xx])[s, r];



MajoranaFieldQ[_] := False;

ContainsMajoranasQ[ll_List] := RFermionFieldQ /@ (Or @@ ll);

ConvertMajoranas[ll_List] := ll //. x_?((RFermionFieldQ[#] && AntiFieldQ[#])&) :> anti[x];

GetFermionFlow[cl_, vertex_] := Block[{output,temp, cphase},
    cphase = Times @@ ((Exp[I*MajoranaPhase[#]]&) /@ Cases[cl, _?RFermionFieldQ]);
    temp = Expand[vertex];
    If[(CC /@ cl) === cl,
       output  = {{ConvertMajoranas[cl], Expand[cphase * temp]}},
       temp = temp //.{gg_?GammaMatrixQ[mu___, r_, s_] :> CGa[GotFlow[gg][mu, r, s]]};
       temp = temp //. GotFlow[x_] -> Identity[x];
       temp = temp //. Dispatch[{TensDot[xx___, -gg_?GammaMatrixQ[mu___], yy___] :> -TensDot[xx, gg[mu], yy], 
                     TensDot[xx___, -gg_?GammaMatrixQ[mu___], yy___][ss___] :> -TensDot[xx, gg[mu], yy][ss],
                     TensDot[xx___, -gg_?GammaMatrixQ, yy___] :> -TensDot[xx, gg, yy],
                     TensDot[xx___, -gg_?GammaMatrixQ[mu___], yy___][ss___] :> -TensDot[xx, gg[mu], yy][ss]}];
       output = {{ConvertMajoranas[cl], vertex}, {ConvertMajoranas[CC /@ cl], Expand[cphase * temp]}}];
    output];
           
           
numQ[MajoranaPhase[__]] := True;

MajoranaPhase[f_?AntiFieldQ] := - MajoranaPhase[anti[f]];




Options[TreatMajoranasAndCC] = {SymmetryFactor -> True};

TreatMajoranasAndCC[expr_, options___] := Block[{tempexpr, MajDone, Majexpr, NoMajexpr, DelDot, DelDotUndo,factor},

  factor = 1/2(*If[SymmetryFactor /. {options} /. Options[TreatMajoranasAndCC], 1/2, 1]*);

 If[Head[expr] === Plus, 
    Majexpr = Select[expr, Not[FreeQ[#, _?RFermionFieldQ] && FreeQ[#,CC]]&]; 
    If[ Majexpr ===0, NoMajexpr = expr, NoMajexpr = Expand[expr- Majexpr]],
    If[FreeQ[expr, _?RFermionFieldQ] && FreeQ[expr, CC], Majexpr = 0; NoMajexpr = expr , Majexpr = expr; NoMajexpr = 0]];

 If[Majexpr =!= 0, 

   DelDot[del[ff1_, mu1_], ff2_, aa1_, aa2_] := DelDot[ff1, ff2, Append[aa1, mu1], aa2];
   DelDot[ff1_, del[ff2_, mu1_], aa1_, aa2_] := DelDot[ff1, ff2, aa1, Append[aa2, mu1]];

   DelDotUndo[ff1_, ff2_, {xx1___, mu1_}, aa2_] := DelDotUndo[del[ff1, mu1], ff2, {xx1}, aa2];
   DelDotUndo[ff1_, ff2_, aa1_, {xx2___, mu1_}] := DelDotUndo[ff1, del[ff2, mu1], aa1, {xx2}];

   Majexpr = Majexpr /. Dot[ff1_, ff2_] :> DelDot[ff1, ff2, {}, {}];

   
(* This subroutine takes care of fermion chains involving Majorana fermions and explicit charge conjugated Dirac fermions.
   lam denotes a Majorana, psi a Dirac, and EI a Majorana phase. *)
(* lambar.G.lam -> lambar.G.lam + lambar.Gc.lam 
   Notice that we include already here the factor 2 coming formt he fact that lam and lambar are identical particles *)
    tempexpr = Majexpr //. {
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[f1_?((AntiFieldQ[#] && RFermionFieldQ[#])&)[Index[Spin, s_], ind1___], f2_?((Not[AntiFieldQ[#]] && RFermionFieldQ[#])&)[Index[Spin, r_], ind2___], aa1_, aa2_]:> factor gg[indgg, Index[Spin, s], Index[Spin, r]]DelDot[MajDone[f1][Index[Spin, s],ind1], MajDone[f2][Index[Spin, r],ind2], aa1, aa2]+ factor CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[anti[f2]][Index[Spin, r],ind2], MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1]/;anti[f1]===f2,
          DelDot[f1_?((AntiFieldQ[#] && RFermionFieldQ[#])&)[Index[Spin, s_], ind1___], f2_?((Not[AntiFieldQ[#]] && RFermionFieldQ[#])&)[Index[Spin, s_], ind2___], aa1_, aa2_]:> factor DelDot[MajDone[f1][Index[Spin, s],ind1], MajDone[f2][Index[Spin, s],ind2], aa1, aa2]+ factor DelDot[MajDone[anti[f2]][Index[Spin, s],ind2],MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1]/;anti[f1]===f2}; 
(* lambar2.G.lam1 -> EI lambar1.Gc.lam2 
   We bring all chains containing two Majoranas in canonical order, so we don't miss any piece. Notice the appearance of the phases! *)
    tempexpr = tempexpr //. {
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[f1_?((AntiFieldQ[#] && RFermionFieldQ[#])&)[Index[Spin, s_], ind1___],  f2_?((Not[AntiFieldQ[#]] && RFermionFieldQ[#])&)[Index[Spin, r_], ind2___], aa1_, aa2_]:> Exp[I (MajoranaPhase[anti[f1]] -MajoranaPhase[f2])] * CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[anti[f2]][Index[Spin, r],ind2], MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1]/;(anti[f1]=!=f2)&&Not[OrderedQ[{anti[f1],f2}]],
          DelDot[f1_?((AntiFieldQ[#] && RFermionFieldQ[#])&)[Index[Spin, s_], ind1___],f2_?((Not[AntiFieldQ[#]] && RFermionFieldQ[#])&)[Index[Spin, s_], ind2___], aa1_, aa2_]:> Exp[I (MajoranaPhase[anti[f1]] - MajoranaPhase[f2])] * DelDot[MajDone[anti[f2]][Index[Spin, s],ind2],MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1]/;(anti[f1]=!=f2)&&Not[OrderedQ[{anti[f1],f2}]]};
(* CC[psibar].G.psi -> CC[psibar].G.psi + CC[psibar].Gc.psi 
   Notice that we include already here the factor 2 coming formt he fact that lam and lambar are identical particles. 
   Idem for psibar.G.CC[psi] *)
    tempexpr=tempexpr //.{DelDot[CC[f1_][Index[Spin,s_],ind1___],f2_[Index[Spin,s_],ind2___], aa1_ ,aa2_]:>factor DelDot[MajDone[CC[anti[f2]]][Index[Spin,s],ind2],MajDone[anti[f1]][Index[Spin,s],ind1], aa2, aa1]+ factor DelDot[MajDone[CC[f1]][Index[Spin,s],ind1],MajDone[f2][Index[Spin,s],ind2], aa1, aa2]/;(anti[f1]===f2),
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[CC[f1_][Index[Spin, s_], ind1___], f2_[Index[Spin, r_], ind2___], aa1_, aa2_]:>factor CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[CC[anti[f2]]][Index[Spin, r],ind2], MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1] + factor gg[indgg, Index[Spin, s],Index[Spin, r]]DelDot[MajDone[CC[f1]][Index[Spin, s], ind1], MajDone[f2][Index[Spin, r], ind2], aa1, aa2]/;(anti[f1]===f2)};
    tempexpr=tempexpr //.{DelDot[f1_[Index[Spin,s_],ind1___],CC[f2_][Index[Spin,s_],ind2___], aa1_, aa2_]:>factor DelDot[MajDone[anti[f2]][Index[Spin,s],ind2],MajDone[CC[anti[f1]]][Index[Spin,s],ind1], aa2, aa1]+factor DelDot[MajDone[f1][Index[Spin,s],ind1],MajDone[CC[f2]][Index[Spin,s],ind2], aa1, aa2]/;(anti[f1]===f2),
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[f1_[Index[Spin, s_], ind1___], CC[f2_][Index[Spin, r_], ind2___], aa1_, aa2_]:>factor CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[anti[f2]][Index[Spin, r],ind2], MajDone[CC[anti[f1]]][Index[Spin, s],ind1], aa2, aa1] + factor gg[indgg, Index[Spin, s],Index[Spin, r]]DelDot[MajDone[f1][Index[Spin, s], ind1], MajDone[CC[f2]][Index[Spin, r], ind2], aa1, aa2]/;(anti[f1]===f2)};
(* CC[psi1bar].G.CC[psi2] -> psi2bar.Gc.psi1 *)
    tempexpr=tempexpr//.{
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[CC[f1_][Index[Spin, s_], ind1___], CC[f2_][Index[Spin, r_], ind2___], aa1_, aa2_]:>CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[anti[f2]][Index[Spin, r],ind2],  MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1],
          DelDot[CC[f1_][Index[Spin, s_], ind1___],CC[f2_][Index[Spin, s_], ind2___], aa1_, aa2_]:> DelDot[MajDone[anti[f2]][Index[Spin, s],ind2], MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1]};
(* CC[psi1bar ].G.lam -> CC[lambar].Gc.psi if not orderedQ[psi1,psi2] 
   Idem for lambar.G.CC[psi1] *)
     tempexpr = tempexpr //. {
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[CC[f1_][Index[Spin, s_], ind1___] ,f2_?RFermionFieldQ[Index[Spin, r_], ind2___], aa1_, aa2_]:>CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[CC[anti[f2]]][Index[Spin, r],ind2], MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1],
          DelDot[CC[f1_][Index[Spin, s_], ind1___], f2_?RFermionFieldQ[Index[Spin, s_], ind2___], aa1_, aa2_]:> DelDot[MajDone[CC[anti[f2]]][Index[Spin, s],ind2], MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1]};     
     tempexpr=tempexpr//.{
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[f1_?RFermionFieldQ[Index[Spin, s_], ind1___], CC[f2_][Index[Spin,r_],ind2___], aa1_, aa2_]:>CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[anti[f2]][Index[Spin, r],ind2],  MajDone[CC[anti[f1]]][Index[Spin, s],ind1], aa2, aa1],
          DelDot[f1_?RFermionFieldQ[Index[Spin, s_], ind1___], CC[f2_][Index[Spin, s_], ind2___], aa1_, aa2_]:> DelDot[MajDone[anti[f2]][Index[Spin, s],ind2], MajDone[CC[anti[f1]]][Index[Spin, s],ind1], aa2, aa1]};
(* CC[psi1bar ].G.psi2 -> CC[psi2bar].Gc.psi if not orderedQ[psi1,psi2] 
   Idem for psi2bar.G.CC[psi1] *)
     tempexpr = tempexpr //. {
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[CC[f1_][Index[Spin, s_], ind1___] ,f2_[Index[Spin, r_], ind2___], aa1_, aa2_]:>CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[CC[anti[f2]]][Index[Spin, r],ind2], MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1]/;Not[OrderedQ[{anti[f1],f2}]],
          DelDot[CC[f1_][Index[Spin, s_], ind1___], f2_[Index[Spin, s_], ind2___], aa1_, aa2_]:> DelDot[MajDone[CC[anti[f2]]][Index[Spin, s],ind2], MajDone[anti[f1]][Index[Spin, s],ind1], aa2, aa1]/;Not[OrderedQ[{anti[f1],f2}]]};     
     tempexpr=tempexpr//.{
          gg_?(GammaMatrixQ)[indgg___, Index[Spin, s_],Index[Spin, r_]]DelDot[f1_[Index[Spin, s_], ind1___], CC[f2_][Index[Spin,r_],ind2___], aa1_, aa2_]:>CGa[gg[indgg, Index[Spin, r], Index[Spin, s]]]DelDot[MajDone[anti[f2]][Index[Spin, r],ind2],  MajDone[CC[anti[f1]]][Index[Spin, s],ind1], aa2, aa1]/;Not[OrderedQ[{anti[f1],f2}]],
          DelDot[f1_[Index[Spin, s_], ind1___], CC[f2_][Index[Spin, s_], ind2___], aa1_, aa2_]:> DelDot[MajDone[anti[f2]][Index[Spin, s],ind2], MajDone[CC[anti[f1]]][Index[Spin, s],ind1], aa2, aa1]/;Not[OrderedQ[{anti[f1],f2}]]};    
     tempexpr = tempexpr //. MajDone -> Identity;
(* We finally include the Majorana phases:
   lambar -> EI lambar *)
(*     tempexpr = tempexpr /. {DelDot[f1_?((RFermionFieldQ[#] && AntiFieldQ[#])&)[ind1___],f2___]:>E^(I MajoranaPhase[anti[f1]])DelDot[f1[ind1],f2]};*)

     Majexpr = tempexpr /. DelDot -> DelDotUndo;

     Majexpr = Majexpr /. DelDotUndo[ff__, {}, {}] :> Dot[ff]];
   
 Plus[Majexpr, NoMajexpr]  ];


(* ::Section::Closed:: *)
(*2D-Epsilon tensors*)


(* ::Subsubsection::Closed:: *)
(*Hermitian conjugation*)


HC[Ueps[a_,b_]]:=Ueps[a,b];

HC[Deps[a_,b_]]:=Deps[a,b];


(* ::Subsubsection::Closed:: *)
(*Delta*)


Deps[a_,b_] Ueps[b_,c_] ^:= EDelta[a,c];
Deps[a_,b_] Ueps[c_,b_] ^:= -EDelta[a,c];
Deps[b_,a_] Ueps[b_,c_] ^:= -EDelta[a,c];
Deps[b_,a_] Ueps[c_,b_] ^:= EDelta[a,c];

EDelta[a_,b_] Ueps[c_,a_] ^:= Ueps[c,b];
EDelta[a_,b_] Ueps[a_,c_] ^:= Ueps[b,c];

EDelta[b_,a_] Deps[c_,a_] ^:= Deps[c,b];
EDelta[b_,a_] Deps[a_,c_] ^:= Deps[b,c];

EDelta[a_,a_] =2;

EDelta[a_,b_] si[mu_,b_,c_] ^:= si[mu,a,c];
EDelta[a_,b_] si[mu_,c_,b_] ^:= si[mu,c,a];


(* ::Subsubsection::Closed:: *)
(*Derivatives*)


del[Ueps[__],_] =0;
del[Deps[__],_] =0;
del[EDelta[__],_] =0;


(* ::Subsubsection::Closed:: *)
(*Misc*)


numQ[EDelta[__]]:=True;
numQ[Ueps[_,_]]:=True;
numQ[Deps[_,_]]:=True;


(* ::Section::Closed:: *)
(*Sextet and Triplet Clebsch-Gordons.*)


(* ::Text:: *)
(*This is based on Ref.   arXiv:0909.2666*)


(* ::Subsection:: *)
(*Sextet Clebsch-Gordans*)


K6 /: Conjugate[K6[sext_, i_, j_]] := K6bar[sext, i, j];
K6bar /: Conjugate[K6bar[sext_, i_, j_]] := K6[sext, i, j];

numQ[K6[__]] := True;
CnumQ[K6[__]] := True;
TensQ[K6[__]] := True;

numQ[K6bar[__]] := True;
CnumQ[K6bar[__]] := True;
TensQ[K6bar[__]] := True;


If[FreeQ[$TensIndRules, K6], 
   $TensIndRules = Join[$TensIndRules, {K6[Except[Index[__],a_], b_, c_] :> K6[Index[Sextet,a],b,c],
                                        K6[a_, Except[Index[__],b_], c_] :> K6[a,Index[Colour,b],c],
                                        K6[a_, b_, Except[Index[__],c_]] :> K6[a,b,Index[Colour,c]]
                                        }]];

If[FreeQ[$TensIndRules, K6bar], 
   $TensIndRules = Join[$TensIndRules, {K6bar[Except[Index[__],a_], b_, c_] :> K6bar[Index[Sextet,a],b,c],
                                        K6bar[a_, Except[Index[__],b_], c_] :> K6bar[a,Index[Colour,b],c],
                                        K6bar[a_, b_, Except[Index[__],c_]] :> K6bar[a,b,Index[Colour,c]]
                                        }]];

K6[a_, i_, j_] := K6[a, j, i] /; Not[OrderedQ[{i,j}]];
K6bar[a_, i_, j_] := K6bar[a, j, i] /; Not[OrderedQ[{i,j}]];


K6 /: K6[sext_, a_, b_]K6bar[sext_, c_, d_] := 1/2(IndexDelta[a,c]IndexDelta[b,d] + IndexDelta[a,d]IndexDelta[b,c]);


(* ::Subsection::Closed:: *)
(*Triplet Clebsch-Gordan*)


(* ::Text:: *)
(*According to Ref. 0909.2666, triplet Clesch Gordons are proportinal to Ep^ijk*)


K3[i_,j_,k_] := Eps[i,j,k]/Sqrt[2];
K3bar[i_,j_,k_] := Eps[i,j,k]/Sqrt[2];


(* ::Section::Closed:: *)
(*Transpose*)


TP[TP[t_]] := t;
$IndList[TP[t_]] := $IndList[t];
TensQ[TP[t_]] := TensQ[t];
GaugeMatrixQ[TP[t_]]:=GaugeMatrixQ[t];

Format[TP[t_], StandardForm] := Transpose[t];
Format[TP[t_], TraditionalForm] := Transpose[t]; 
