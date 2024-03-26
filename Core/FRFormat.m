(* ::Package:: *)

(* ::Title:: *)
(*FeynRules formatting*)


(* ::Text:: *)
(*This file contains the formatting rules for FeynRules.*)


(* ::Section:: *)
(*General print out for TraditionalForm*)


Format[Ga[mu_], TraditionalForm] := Superscript[\[Gamma], mu];
Format[Ga[mu_, r_, s_], TraditionalForm] := Superscript[Subscript[\[Gamma], r, s], mu];
Format[GaAlgebra[mu_, r_, s_], TraditionalForm] := Power[Subscript[\[Gamma], r, s], mu];

Format[Sig[mu_,nu_], TraditionalForm] := Superscript[Superscript[\[Sigma], mu], nu];
Format[Sig[mu_,nu_, r_, s_], TraditionalForm] :=  Subscript[Superscript[Superscript[\[Sigma], mu], nu],r,s];

Format[ProjP, TraditionalForm] = Subscript[P, "+"];
Format[ProjM, TraditionalForm] = Subscript[P, "-"];
Format[ProjP[ind__], TraditionalForm] := Subscript[(Subscript[P,"+"]), ind];
Format[ProjM[ind__], TraditionalForm] := Subscript[(Subscript[P,"-"]), ind];

Format[PauliSigma[i_], TraditionalForm] := Superscript[\[Sigma], i];

Format[si[mu_], TraditionalForm] := Superscript[\[Sigma], mu];
Format[si[mu_, r__], TraditionalForm] := Subscript[Power[\[Sigma], mu], r];
Format[sibar[mu_], TraditionalForm] := Superscript[Overscript[\[Sigma], "-"], mu];
Format[sibar[mu_,r_,s_],TraditionalForm]:=Superscript[Power[Overscript[\[Sigma],"-"],mu],Row[{r,",",s}]];

Format[IndexDelta[ind__], TraditionalForm] := Subscript[\[Delta], ind];

Format[TensDot[t_, u_], TraditionalForm] := Dot[t, u];
Format[TensDot[t_, u___][ind___], TraditionalForm] := Subscript[(Dot[t, u]),ind];

Format[ME[ind__], TraditionalForm] := Subscript[\[Eta], ind];

Format[Eps[ind__], TraditionalForm] := Subscript[\[Epsilon], ind];

Format[HC[t_], TraditionalForm] := Superscript[t,\[Dagger]];

Format[HC[t_][ind__], TraditionalForm] := Superscript[Subscript[t, ind],\[Dagger]];

Format[CC[t_], TraditionalForm] := Power[t, C];
Format[CC[t_][ind___], TraditionalForm] := Power[Subscript[t, ind], C];

Format[del[f_, mu_], TraditionalForm] := Subscript["\[PartialD]", mu][f];

Format[FV[k_, mu_], TraditionalForm] := Subsuperscript["p",k,mu];
Format[SP[k1_, k2_], TraditionalForm] := Dot[Subscript["p", k1], Subscript["p", k2]];

Unprotect[Conjugate];
Format[Conjugate[zz_], TraditionalForm] := Power[zz, "*"];
Protect[Conjugate];

Format[SlashedP[k_,i_,j_], TraditionalForm] := Subscript[Dot[\[Gamma], Subscript["p",k]], i,j];


(* ::Section::Closed:: *)
(*Index formatting*)


Format[Index[_, Except[Ext[___], k_]], TraditionalForm] := k;
Format[Index[_, i_?(Not[NumericQ[#]]&), j_], TraditionalForm] := merge[i, j];
Format[Index[_, i_?(NumericQ), _], TraditionalForm] := i; 

Index[name_, __][j_] := Index[name, Ext[j]];

IndexStyle[name_, symb_] := Block[{output},
      SetDelayed[Format[Index[name, Ext[i__]], TraditionalForm], Subscript[ToString[symb], i]];
      SetDelayed[Format[Index[name, Ext[i__]], StandardForm], Subscript[ToString[symb], i]];
      MR$IndForm[name] = symb];
      
IndexStyle[Lorentz, \[Mu]]

MRIndexRange[Index[Lorentz]] = Range[4];

IndexStyle[Spin, s]

MRIndexRange[Index[Spin]] = Range[2]; 

IndexStyle[Spin1, \[Alpha]];
IndexStyle[Spin2, \[Beta]];

(************ Renaming internal indices **********)

IntLor[i_, ki_] := If[ValueQ[StoreIntLor[i, ki]], StoreIntLor[i, ki],
   IntLor[i] = Block[{output, temp, counter},
      counter = ++MR$IntLorCount;
      temp = Which[0 < counter < 25,
                         IntDone[ToExpression[FromCharacterCode[944 + counter]]],
                  24 < counter < 49,
                         IntDone[ToExpression[FromCharacterCode[912 + counter]]],
                  48 < counter < 74,
                         IntDone[ToExpression[FromCharacterCode[96 + counter]]],
                  73 < counter < 99,
                         IntDone[ToExpression[FromCharacterCode[64 + counter]]],
                  True,
                         IntDone[i]];
       StoreIntLor[i, ki] = temp;
       output = temp]];

Int[i_, ki_] := If[ValueQ[StoreInt[i, ki]], StoreInt[i, ki],
   Int[i] = Block[{output, temp, counter},
      counter = ++MR$IntCount;
      temp = Which[0 < counter < 27,
                         IntDone[ToExpression[FromCharacterCode[96 + counter]]],
                  26 < counter < 53,
                         IntDone[ToExpression[FromCharacterCode[38 + counter ]]],
                  52 < counter < 78,
                         IntDone[ToExpression[FromCharacterCode[892 + counter]]],
                  77 < counter < 99,
                         IntDone[ToExpression[FromCharacterCode[835 + counter]]],
                  True,
                         IntDone[i]];
      StoreInt[i, ki] = temp;
      output = temp]];




(* ::Section:: *)
(*TeX formatting*)


(* ::Text:: *)
(*To get the correct TeX output, I have to fix the StandardForm*)


MySuperscript[aa_, b_] := Superscript[aa,b];
MySuperscript[aa__, b_, c_] := MySuperscript[MySuperscript[aa, b], c];

TeXFormat[$xx_, $yy_] := Block[{tmp},
     SetDelayed[Format[$xx, StandardForm], $yy];
     SetDelayed[Format[$xx, TraditionalForm], $yy]];

MakeTeXIndex[$xx_, $yy_] := Block[{tmp}, SetDelayed[Format[$xx[indind___], StandardForm], Subscript[$yy, indind]];
                                  SetDelayed[Format[$xx[indind___], TraditionalForm], Subscript[$yy, indind]]];

Format[Index[_, Except[Ext[___], k_]], StandardForm] := k;
Format[Index[_, Except[Ext[___], k1_], k2_], StandardForm] := merge[k1, k2];

Format[Ga[mu_], StandardForm] := Superscript[\[Gamma], mu];
Format[Ga[mu_, r_, s_], StandardForm] := Superscript[Subscript[\[Gamma], r, s],mu];

Format[Sig[mu_,nu_], StandardForm] := Superscript[Superscript[\[Sigma], mu], nu];
Format[Sig[mu_,nu_, r_, s_], StandardForm] :=  Subscript[Superscript[Superscript[\[Sigma], mu], nu],r,s];

Format[ProjP, StandardForm] := Subscript[P,"+"];
Format[ProjM, StandardForm] := Subscript[P,"-"];

Format[ProjP[ind__], StandardForm] := Subscript[(Subscript[P,"+"]), ind];
Format[ProjM[ind__], StandardForm] := Subscript[(Subscript[P,"-"]), ind];

Format[PauliSigma[i_], StandardForm] := Power[\[Sigma], i];

Format[si[mu_], StandardForm] := Power[\[Sigma], mu];
Format[si[mu_, r__], StandardForm] := Subscript[Power[\[Sigma], mu], r];
Format[sibar[mu_], StandardForm] := Power[Overscript[\[Sigma], "-"], mu];
Format[sibar[mu_,r_,s_],StandardForm]:=Superscript[Power[Overscript[\[Sigma],"-"],mu],Row[{r,",",s}]];


Format[IndexDelta[ind__], StandardForm] := Subscript[\[Delta], ind];

Format[TensDot[t_, u_], StandardForm] := Dot[t, u];
Format[TensDot[t_, u___][ind___], StandardForm] := Subscript[(Dot[t, u]),ind];

Format[ME[ind__], StandardForm] := Subscript[\[Eta], ind];

Format[Eps[ind__], StandardForm] := Subscript[\[Epsilon], ind];

Format[HC[t_], StandardForm] := Power[t,\[Dagger]];

Format[HC[t_][ind__], StandardForm] := Power[Subscript[t, ind],\[Dagger]];

Format[CC[t_], StandardForm] := Power[t, C];
Format[CC[t_][ind___], StandardForm] := Power[Subscript[t, ind], C];

Format[del[f_, mu_], StandardForm] := Subscript["\[PartialD]", mu][f];

Unprotect[Conjugate];
Format[Conjugate[zz_], StandardForm] := Power[zz, "*"];
Protect[Conjugate];

Format[FV[k_, mu_], StandardForm] := Power[Subscript["p",k], mu];
Format[SP[k1_, k2_], StandardForm] := Dot[Subscript["p", k1], Subscript[p, k2]];

Format[SlashedP[k_,i_,j_], StandardForm] := Subscript[Dot[\[Gamma], Subscript["p",k]], i,j];


(* ::Section::Closed:: *)
(*Sherpa formatting*)


(***************************************************************)
(* This is the Format used for the Sherpa files *)

Format[FRSHPi, CForm] = "M_PI";
Format[FRSHE, CForm] = "M_E";
Format[FRSHSqrt, CForm] = sqrt;
Format[FRSHSin, CForm] = sin;
Format[FRSHCos, CForm] = cos;
Format[FRSHTan, CForm] = tan;
Format[FRSHCot, CForm] = cot;
Format[FRSHSinh, CForm] = sinh;
Format[FRSHCosh, CForm] = cosh;
Format[FRSHTanh, CForm] = tanh;
Format[FRSHCoth, CForm] = coth;
Format[FRSHExp, CForm] = exp;
Format[FRSHLog, CForm] = log;
Format[FRSHArcSin, CForm] = asin;
Format[FRSHArcCos, CForm] = acos;
Format[FRSHArcTan, CForm] = atan;
Format[FRSHArcCot, CForm] = acot;
Format[FRSHArcSinh, CForm] = asinh;
Format[FRSHArcSinh, CForm] = acosh;
Format[FRSHArcSinh, CForm] = atanh;
Format[FRSHArcSinh, CForm] = acoth;
Format[FRSHPower, CForm] = pow;
Format[FRSHComplex01, CForm] = "Complex(0,1)";

ToFRSHRules = Dispatch[{Pi -> FRSHPi, E -> FRSHE, Power[n1_, n2_Integer] -> FRSHPower[n1, n2], Power[$xx_, Rational[1,2]] -> FRSHSqrt[$xx], Power[$xx_, Rational[-1,2]] -> 1/FRSHSqrt[$xx], Power[$xx_, Rational[1,-2]] -> 1/FRSHSqrt[$xx], Sin -> FRSHSin, Cos -> FRSHCos, Tan -> FRSHTan, Cot -> FRSHCot, Exp -> FRSHExp, Log -> FRSHLog, Sinh -> FRSHSinh, 
                        Cosh -> FRSHCosh, Tanh -> FRSHTanh, Coth -> FRSHCoth, ArcSin -> FRSHArcSin, ArcCos -> FRSHArcCos, ArcTan -> FRSHArcTan, ArcCot -> FRSHArcCot, 
                        ArcSinh -> FRSHArcSinh, ArcCosh -> FRSHArcCosh, ArcTanh -> FRSHArcTanh, ArcCot -> FRSHArcCoth, Complex[0, b_] -> b*FRSHComplex01, Conjugate -> FRSHConjugate}];

ToFRSHString[expr_] := StringReplace[ToString[N[expr//.ToFRSHRules], CForm], {" " -> "", "\"M_PI\"" -> "M_PI", "\"M_E\"" -> "E", "\"Complex(0,1)\""->"(0,1)", "FRSHConjugate" -> "Conj"}];


(* ::Section:: *)
(*CalcHEP Formatting*)


(* ::Subsubsection:: *)
(*CFunctionReplacements*)


CFunctionReplacements={
Cos->cos,Sin->sin,Tan->tan,
Sec->sec,Csc->csc,Cot->cot,
Cosh->cosh,Sinh->sinh,Tanh->tanh,
Sech->sech,Csch->csch,Coth->coth,
ArcCos->acos,ArcSin->asin,ArcTan->atan,
ArcSec->asec,ArcCsc->acsc,ArcCot->acot,
ArcCosh->acosh,ArcSinh->asinh,ArcTanh->atanh,
ArcSech->asech,ArcCsch->acsch,ArcCoth->acoth,
Sqrt->sqrt,Power->pow,Exp->exp,Log->log,
Abs->fabs,Ceiling->ceil,Floor->floor,Max->fmax,Min->fmin,
(*CalcHEP only allows complex numbers in a very limited way.
Parameters and Functions have to be real.
Imaginary numbers can only be present in the vertices.*)
Conjugate[aa_]->aa
};


(* ::Section:: *)
(*FortranForm*)


(* ::Subsection:: *)
(*FortranForm*)


Unprotect[Re];
Format[Re, FortranForm] := REAL;
Protect[Re];

Unprotect[Im];
Format[Im, FortranForm] := AIMAG;
Protect[Im];

Unprotect[ArcTan];
Format[ArcTan,FortranForm] := ATAN;
Protect[ArcTan];

Unprotect[ArcSin];
Format[ArcSin,FortranForm] := ASIN;
Protect[ArcSin];

Unprotect[ArcCos];
Format[ArcCos,FortranForm] := ACOS;
Protect[ArcCos];

Unprotect[ArcCot];
Format[ArcCot,FortranForm] := ACOT;
Protect[ArcCot];

Format[FortranSec[x_], FortranForm] := 1/FortranCos[x];
Format[FortranCsc[x_], FortranForm] := 1/FortranSin[x];

Format[FortranSin[x_], FortranForm] := Sin[x];
Format[FortranCos[x_], FortranForm] := Cos[x];


Unprotect[Csc];
Format[Csc[x_], FortranForm] := FortranCsc[x];
Protect[Csc];

Unprotect[Sec];
Format[Sec[x_], FortranForm] := FortranSec[x];
Protect[Sec];


(* ::Subsection:: *)
(*Optimization*)


NumericalValue[Sqrt2] = NumericalValue[Sqrt[2]];
NumericalValue[SqrtPi] = NumericalValue[Sqrt[Pi]];


MakeNewSymb[symb_[inds__]] := StringJoin[ToString[symb /. ParamRules], Sequence@@ (ToString /@ ({inds} /. Index[_, $kk_] :> $kk))];

$MakeOptimizedParamRules[] := Block[{tempopr, TempRule},
     tempopr = ParamRules /. Rule -> TempRule;
     tempopr = Cases[tempopr, TempRule[_[_],_]];
     tempopr = tempopr /. Index[_, $kk_] :> $kk;
     tempopr = tempopr /. TempRule -> Rule;
     Join[ParamRules, tempopr]];

                         

MakeOptimizedParam[Power[ex_, $nn_?((IntegerQ[#] && (#>0))&)]] := Block[{tempex, newsymb, getord, noindex}, 
    noindex = ex /. Index[_,$kk_] :> $kk;
    If[Not[MemberQ[FR$OptimizeParams,noindex]], Power[ex,$nn],
         (*Create new internal parameter *)
         newsymb = If[MatchQ[noindex, _[__]], MakeNewSymb[noindex], ToString[ex /. ParamRules]];
         newsymb = Symbol["P" <> newsymb <> ToString[$nn]];
    (* Assign the value to NumericalValue *)
    NumericalValue[newsymb] = NumericalValue[Power[ex,$nn]];
         If[Not[MemberQ[FR$OptimizeParams, newsymb]],
            AppendTo[FR$OptimizeParams, newsymb]; 
            getord = GetOrder[Power[noindex,$nn]/.$OptimizedParamRules];
            If[getord === {},
               AppendTo[$OptimizedParams, {newsymb ,Power[noindex,$nn], (CnumQ[ex] === True)} /. $OptimizedParamRules],
               MGOrdertemp[newsymb] =Flatten[getord];
               AppendTo[$OptimizedParams, {newsymb ,Power[noindex,$nn],Sequence @@ Flatten[getord], (CnumQ[ex] === True)} /. $OptimizedParamRules]]];
         newsymb]];

MakeOptimizedParam[Power[ex_, $nn_?((IntegerQ[#] && (#<0) && (# =!= -1))&)]] := Block[{tempex, newsymb, getord, noindex}, 
         (*Create new internal parameter *)
    noindex = ex /. Index[_,$kk_] :> $kk;
    If[Not[MemberQ[FR$OptimizeParams,noindex]], Power[ex,$nn],
         newsymb = If[MatchQ[ex, _[__]], MakeNewSymb[noindex], ToString[ex /. ParamRules]];
         newsymb = Symbol["P" <> newsymb <> ToString[-$nn]];
    (* Assign the value to NumericalValue *)
    NumericalValue[newsymb] = NumericalValue[Power[ex,$nn]];
         If[Not[MemberQ[FR$OptimizeParams, newsymb]],
            AppendTo[FR$OptimizeParams, newsymb]; 
            getord = GetOrder[Power[noindex,-$nn]/.$OptimizedParamRules];
            If[getord === {},
               AppendTo[$OptimizedParams, {newsymb ,Power[noindex,-$nn], (CnumQ[ex] === True)}/.$OptimizedParamRules],
               MGOrdertemp[newsymb] =Flatten[getord];
               AppendTo[$OptimizedParams, {newsymb ,Power[noindex,-$nn],Sequence @@ Flatten[getord], (CnumQ[ex] === True)}/.$OptimizedParamRules]]];
         1/newsymb]];

MakeOptimizedParam[Power[ex_, 1/2]] := Block[{tempex, newsymb, getord, noindex}, 
         (*Create new internal parameter *)
    noindex = ex /. Index[_,$kk_] :> $kk;
    If[Not[MemberQ[FR$OptimizeParams,noindex]], Power[ex,1/2],
         newsymb = If[MatchQ[noindex, _[__]], MakeNewSymb[noindex], ToString[ex /. ParamRules]];
         newsymb = Symbol["SQRT" <> newsymb];
         If[Not[MemberQ[FR$OptimizeParams, newsymb]],
            AppendTo[FR$OptimizeParams, newsymb]; 
            getord = GetOrder[Power[noindex,1/2]/.$OptimizedParamRules];
    (* Assign the value to NumericalValue *)
    NumericalValue[newsymb] = NumericalValue[Power[ex,1/2]];
            If[getord === {},
               AppendTo[$OptimizedParams, {newsymb ,Power[noindex,1/2], (CnumQ[noindex] === True)} /. $OptimizedParamRules],
               MGOrdertemp[newsymb] =Flatten[getord];
               AppendTo[$OptimizedParams, {newsymb ,Power[noindex,1/2],Sequence @@ Flatten[getord], (CnumQ[noindex] === True)} /. $OptimizedParamRules]]];
         newsymb]];

MakeOptimizedParam[Power[ex_, -1/2]] := Block[{tempex, newsymb, getord, noindex}, 
         (*Create new internal parameter *)
    noindex = ex /. Index[_,$kk_] :> $kk;
    If[Not[MemberQ[FR$OptimizeParams,noindex]], Power[ex,-1/2],
         newsymb = If[MatchQ[noindex, _[__]], MakeNewSymb[noindex], ToString[ex /. ParamRules]];
         newsymb = Symbol["SQRT" <> newsymb];
    (* Assign the value to NumericalValue *)
    NumericalValue[newsymb] = NumericalValue[Power[ex,-1/2]];
         If[Not[MemberQ[FR$OptimizeParams, newsymb]],
            AppendTo[FR$OptimizeParams, newsymb]; 
            getord = GetOrder[Power[noindex,-1/2]/.$OptimizedParamRules];
            If[getord === {},
               AppendTo[$OptimizedParams, {newsymb ,Power[noindex,-1/2], (CnumQ[noindex] === True)} /. $OptimizedParamRules],
               MGOrdertemp[newsymb] =Flatten[getord];
               AppendTo[$OptimizedParams, {newsymb ,Power[noindex,-1/2],Sequence @@ Flatten[getord], (CnumQ[noindex] === True)} /. $OptimizedParamRules]]];
         1/newsymb]];

MakeOptimizedParam[$ff_?(MemberQ[{Sin,Cos,Tan,Cot}, #]&)[ex_]] := Block[{tempex, newsymb, getord, noindex}, 
         (*Create new internal parameter *)
    noindex = ex /. Index[_,$kk_] :> $kk;
    If[Not[MemberQ[FR$OptimizeParams,noindex]], $ff[ex],
         newsymb = If[MatchQ[noindex, _[__]], MakeNewSymb[noindex], ToString[ex /. ParamRules]];
         newsymb = Symbol[ToString[$ff] <> newsymb];
    (* Assign the value to NumericalValue *)
    NumericalValue[newsymb] = NumericalValue[$ff[ex]];
         If[Not[MemberQ[FR$OptimizeParams, newsymb]],
            AppendTo[FR$OptimizeParams, newsymb]; 
            AppendTo[$OptimizedParams, {newsymb ,$ff[noindex], (CnumQ[noindex] === True)} /. $OptimizedParamRules]];
         newsymb]];

MakeOptimizedParam[$ff_?(MemberQ[{Sec,Csc}, #]&)[ex_]] := Block[{tempex, newsymb, getord, noindex}, 
         (*Create new internal parameter *)
    noindex = ex /. Index[_,$kk_] :> $kk;
    If[Not[MemberQ[FR$OptimizeParams,noindex]], $ff[ex],
         newsymb = If[MatchQ[noindex, _[__]], MakeNewSymb[noindex], ToString[ex /. ParamRules]];
         newsymb = Symbol[If[$ff === Sec, "Cos", "Sin"] <> newsymb];
    (* Assign the value to NumericalValue *)
    NumericalValue[newsymb] = NumericalValue[$ff[ex]];     
         If[Not[MemberQ[FR$OptimizeParams, newsymb]],
            AppendTo[FR$OptimizeParams, newsymb]; 
            AppendTo[$OptimizedParams, {newsymb ,If[$ff === Sec, Cos[noindex], Sin[noindex]], (CnumQ[noindex] === True)} /. $OptimizedParamRules]];
         1/newsymb]];

MakeOptimizedParam[Conjugate[ex_]] := Block[{tempex, newsymb, getord, noindex}, 
         (*Create new internal parameter *)
    noindex = ex /. Index[_,$kk_] :> $kk;
    If[Not[MemberQ[FR$OptimizeParams,noindex]], Conjugate[ex],
         newsymb = If[MatchQ[noindex, _[__]], MakeNewSymb[noindex], ToString[ex /. ParamRules]];
         newsymb = Symbol["CONJ"<> newsymb];
    (* Assign the value to NumericalValue *)
    NumericalValue[newsymb] = NumericalValue[Conjugate[ex]];
     If[Not[MemberQ[FR$OptimizeParams, newsymb]],
            AppendTo[FR$OptimizeParams, newsymb]; 
            getord = GetOrder[noindex/.$OptimizedParamRules];
            If[getord === {},
               AppendTo[$OptimizedParams, {newsymb ,Conjugate[noindex], (CnumQ[noindex] === True)} /. $OptimizedParamRules],
               MGOrdertemp[newsymb] =Flatten[getord];
               AppendTo[$OptimizedParams, {newsymb ,Conjugate[noindex],Sequence @@ Flatten[getord], (CnumQ[noindex] === True)} /. $OptimizedParamRules]]];
         newsymb]];

MakeOptimization[expr_]:= Block[{tempexpr},
      tempexpr = PowerExpand[expr] /. {Sqrt[2] :> Sqrt2, Sqrt[Pi] :> SqrtPi, Power[2, -1/2] :> 1/Sqrt2, Power[Pi, -1/2] :> 1/SqrtPi};
     
      tempexpr = tempexpr //. {Power[exp_, $mm_?((IntegerQ[#] && (#=!=-1))&)] :> MakeOptimizedParam[Power[exp,$mm]],
                              Power[exp_, 1/2] :> MakeOptimizedParam[Power[exp,1/2]],
                              Power[exp_, -1/2] :> MakeOptimizedParam[Power[exp,-1/2]],          
                              $ff_?(MemberQ[{Sin,Cos,Tan,Cot,Conjugate}, #]&)[exp_] :> MakeOptimizedParam[$ff[exp]]}
];



(* ::Section:: *)
(*Superfield module related formats*)


(* ::Subsubsection:: *)
(*Format of the Grassman variables (theta and thetabar)*)


Format[theta[aa_]]:=Subscript[\[Theta],aa];

Format[thetabar[aa_]]:=Subscript[OverBar[\[Theta]],aa]


(* ::Subsubsection:: *)
(*2-D epsilon tensor*)


Format[Ueps[Index[type_,aa_],Index[type_,b_]]]:=Superscript[\[Epsilon],ToString[aa]<>" " <>ToString[b]];

Format[Deps[Index[type_,aa_],Index[type_,b_]]]:=Subscript[\[Epsilon],ToString[aa]<>" " <>ToString[b]];


Format[Ueps[aa_,b_]]:=Superscript[\[Epsilon],ToString[aa]<>" " <>ToString[b]];

Format[Deps[aa_,b_]]:=Subscript[\[Epsilon],ToString[aa]<>" " <>ToString[b]];


(* ::Section:: *)
(*Renormalization constants formatting*)


(* ::Subsubsection:: *)
(*Wave function renormalization constants*)


Format[FR$deltaZ[field_List,{}]]:=Subscript["\[Delta]Z",field];
Format[FR$deltaZ[field_List,inds_List]]:=Subscript[Subscript["\[Delta]Z",field],Sequence@@inds];
Format[FR$deltaZ[field_List,{},opt_String]]:=Subsuperscript["\[Delta]Z",field,opt];
Format[FR$deltaZ[field_List,inds_List,opt_String]]:=Subscript[Subsuperscript["\[Delta]Z",field,opt],Sequence@@inds];


(* ::Subsubsection:: *)
(*Parameter renormalization constants*)


Format[FR$delta[{prm_},{}]]:=Subscript["\[Delta]",prm];
Format[FR$delta[{prm_},inds_List]]:=Subscript[Subscript["\[Delta]",prm],Sequence@@inds];


(* ::Subsubsection:: *)
(*Coefficient of the expansion of the renormalization constants*)


Format[FR$deltaZ[argx__][$aa__]] := Superscript[FR$deltaZ[argx],List[$aa]];
Format[FR$delta[argx__][$aa__]] := Superscript[FR$delta[argx],List[$aa]];
