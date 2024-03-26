(* ::Package:: *)

(* ::Title:: *)
(*Functions for vertex calculations*)


(* ::Text:: *)
(*This file contains a set of functions that are needed to calculate Feynman rules*)


(* ::Section:: *)
(*Useful functions*)


FermionSign[field1_, field2_] := 1 /; BosonQ[field1] || BosonQ[field2];
FermionSign[field1_, field2_] := -1 /; FermionQ[field1] && FermionQ[field2]; 

listdelta[list1_, list2_] := 1 /; Length[list1] != Length[list2];
listdelta[list1_, list2_] := 
    Times @@ ((NTIdelta @@ # &) /@ Transpose[{list1, list2}]) /; 
      Length[list1] == Length[list2] ;
     
KronDelta[psi_[i1_Index, is___],phi_, inds_] := KronDelta[psi,phi,inds] listdelta[{i1, is}, inds];

KronDelta[Except[_[_Index, ___], psi_], phi_, _] := Block[{phase, kd},

    kd = If[MakeIdenticalFermions[psi] === phi, 1, Return[0]];

    phase = If[(RFermionFieldQ[psi] === True) && (AntiFieldQ[psi] === True),
               E^(-I MajoranaPhase[psi]), 1];

    Return[kd phase];
];


            

delta /: delta[i_, j_]ff_[x___, j_, y___] := ff[x, i, y] /; ff =!= uwave;
delta /: delta[i_, j_]ff_[x___, i_, y___] := ff[x, j, y] /; ff =!= uwave;
(*if hh is a sum replace all the i by j and so on*)
delta /: delta[i_, j_]hh_[x1___, ff_[x___, j_, y___], y1___] := (hh[x1, ff[x, j, y], y1] /.j->i)/; ff =!= uwave;
delta /: delta[i_, j_]hh_[x1___, ff_[x___, i_, y___], y1___] :=(hh[x1, ff[x, i, y], y1] /.i->j) /; ff =!= uwave;

IntTermQ[expr_] := If[FR$Loop===True,
  (Length[GetFieldContent[expr]] != 1) && (Length[GetFieldContent[expr]] != 0),
  (Length[GetFieldContent[expr]] != 2) && (Length[GetFieldContent[expr]] != 1) && (Length[GetFieldContent[expr]] != 0)];

KillDoubles[xx_] := Block[{output, templist ={}, templist2= xx, elem},
      If[xx =!= {},
         templist = {xx[[1]]}; 
         Do[If[Not[MemberQ[templist, xx[[kk]]]], templist = Append[templist, xx[[kk]]]], {kk, 2, Length[xx]}]];
        (* While[Not[templist2 === {}],
               elem = templist2[[1]];
               AppendTo[templist, elem]; 
               templist2 = DeleteCases[templist2, elem]]];*)
      output = templist];

(*ToMomentumSpace[expr_] := 
    expr /. Dispatch[{del[uwave[field_, indices___, k_], mu_] -> -I FV[k, mu], 
       del[del[uwave[field_, indices___, k_], mu_], nu_] -> - FV[k, mu] FV[k, nu]}];*)

ToMomentumSpace[expr_] := 
    expr //. {uwave[delCom[field1_, mu_], indices___, k_] :> - I FV[k,mu] uwave[field1, indices, k]};





NameIndices[expr_] := Block[{temp},
    temp = expr //. $TensIndRules ;
   temp = temp//. {ff_?((FieldQ[#] || TensQ[#]) && ValueQ[$IndList[#]] && ($IndList[#] =!= {}) &)[ind1___, ii_?(FreeQ[#,Index]&), ind2___] :> ff[ind1, Index[Identity @@ $IndList[ff][[Length[{ind1}]+1]], ii], ind2]}];     

ApplyDefinitions[expr_] := Block[{temp},
   temp = expr /. Dot -> FR$Dot /. FR$Dot -> Dot;
   temp = temp //.Dispatch[Prepend[Prepend[Prepend[MR$Definitions,Power[ex_, n_?(((#>0) && IntegerQ[#])&)] :> ExpPower @@ Table[ex, {n}]],CC[ff_][inds___]:>FR$CC[ff[inds]]], FRModule -> Module]];
   temp = temp //. FR$CC -> CC;
   temp = temp //.ExpPower -> Times;
   temp = temp /. Dot -> FR$Dot /. FR$Dot -> Dot; 
(*   temp = ReleaseASIndex[temp];*)
   temp = NameIndices[FieldExpand[temp]]]


ReplaceIndex[expr_, x_, y_] := expr //. {x :> y};


(* ::Section:: *)
(*Operator chain handling*)


NTIdelta /: NTIdelta[Index[name_, i_], Index[name_, j_, k_]]*f_[NTIndex[name_, j_]] := NTIdelta[Index[name, i], Index[name, j, k]]*f[NTI[name, i]];  
NTIdelta /: NTIdelta[Index[name_, i_, k_], Index[name_, j_]]*f_[NTIndex[name_, i_]] := NTIdelta[Index[name, i, k], Index[name, j]]*f[NTI[name, j]];     
    
NTIdelta /: NTIdelta[Index[name_, i_], Index[name_, j_, k_]]*g_[u___, f_[NTIndex[name_, j_]], v___] := NTIdelta[Index[name, i], Index[name, j, k]]*g[u, f[NTI[name, i]], v];  
NTIdelta /: NTIdelta[Index[name_, i_, k_], Index[name_, j_]]*g_[u___, f_[NTIndex[name_, i_]], v___] := NTIdelta[Index[name, i, k], Index[name, j]]*g[u, f[NTI[name, j]], v];     

NTIdelta /: NTIdelta[Index[name_, i_], Index[name_, j_, k_]]*h_[w___, g_[u___, f_[NTIndex[name_, j_]], v___], t___] := NTIdelta[Index[name, i], Index[name, j, k]]*h[w, g[u, f[NTI[name, i]], v], t];  
NTIdelta /: NTIdelta[Index[name_, i_, k_], Index[name_, j_]]*h_[w___, g_[u___, f_[NTIndex[name_, i_]], v___], t___] := NTIdelta[Index[name, i, k], Index[name, j]]*h[w, g[u, f[NTI[name, j]], v], t];     

NTIdelta /: NTIdelta[Index[name_, i_], Index[name_, j_, k_]]*h2_[w2___, h_[w___, g_[u___, f_[NTIndex[name_, j_]], v___], t___],t2___] := NTIdelta[Index[name, i], Index[name, j, k]]*h2[w2,h[w, g[u, f[NTI[name, i]], v], t], t2];  
NTIdelta /: NTIdelta[Index[name_, i_, k_], Index[name_, j_]]*h2_[w2___, h_[w___, g_[u___, f_[NTIndex[name_, i_]], v___], t___],t2___] := NTIdelta[Index[name, i, k], Index[name, j]]*h2[w2,h[w, g[u, f[NTI[name, j]], v], t], t2];   


OperatorChain[x___, a_, y___] := a OperatorChain[x, y] /; numQ[a];
OperatorChain[x___, a_*y_, zz___] := a OperatorChain[x, y, zz] /; numQ[a];
OperatorChain[] = 1;
OperatorChain[_crea, ___] := 0;
OperatorChain[___,0,___] = 0

OperatorChain[x___,field1_,cr_crea,y___] := (*FermionSign[field1, cr[[1]]]*) OperatorChain[x, cr, field1,y] + 
            Commutator[field1,cr] OperatorChain[x,y] /; FieldQ[field1];

OperatorChain[x___, b_, cr_crea, y___] := OperatorChain[x, cr, b, y] /; Not[FieldQ[b]];



ToOperatorChain /: ToOperatorChain[x___,a_,y___] := a ToOperatorChain[x,y] /; Not[FieldQ[a]];
ToOperatorChain /: ToOperatorChain[x___,a_^n_,y___] := ToOperatorChain[x, Sequence @@ Table[a, {n}],y] /; FieldQ[a];
ToOperatorChain /: ToOperatorChain[x___,a_*b_,y___] := ToOperatorChain[x,a,b,y] /; FieldQ[a] && FieldQ[b];
ToOperatorChain /: ToOperatorChain[x___,Dot[a_, b_],y___] := ToOperatorChain[x,a,b,y] /; FieldQ[a] && FieldQ[b];
ToOperatorChain /: ToOperatorChain[x___,a_[],y___] := ToOperatorChain[x,a,y] /; FieldQ[a];

ToCheckOperatorChain[expr_] := Module[{output,temp},
    If[Not[Head[expr] === Times||Head[expr] === Power], Message[CheckOpChain::Head]; Abort[]];
    output = If[Head[expr]===Times,ToOperatorChain @@ expr,ToOperatorChain[expr]]];



(* ::Section:: *)
(*Commutation relations*)


Commutator[del[psi_,mu_], aa_] := delCom[Commutator[psi, aa], mu];


Commutator[psi1_, crea[psi2_, inds_, i_]] := KronDelta[psi1, psi2, inds] * uwave[psi2, i] /; 
      FieldQ[psi1] && FreeQ[psi1, del];


delCom[Except[_uwave, a_] uwave[psi1_,i__], mu_] := a delCom[uwave[psi1,i], mu];
delCom[uwave[psi1_,inds___, k_], mu_] := - I FV[k,mu]uwave[psi1,inds,k];
delCom[0, _] := 0;


(* ::Section:: *)
(*Creation operator list (CreaList)*)


MakeFunc[a_,x_] := a[x];




crea[Except[_del, field_[indices___]], {}, k_] := crea[MakeIdenticalFermions[field], {indices} /. Index[name_, _]:> Index[name, Ext[k]], k];
crea[del[field_, _], {}, k_] := crea[field, {}, k];


RenameSpin2[crealist_] := crealist //. {crea[h_, List[ind1___, Index[name_, Ext[k_]], ind2___, Index[name_, Ext[k_]], ind3___], l__] -> crea[h, List[ind1, Index[name, Ext[k, 1]], ind2, Index[name, Ext[k, 2]], ind3], l]};
RenameSpin3[crealist_] := crealist //. {crea[h_, List[ind1___, Index[name_, Ext[k_]], ind2___, Index[name_, Ext[k_]], ind3___, Index[name_, Ext[k_]], ind4___], l__] -> crea[h, List[ind1, Index[name, Ext[k, 1]], ind2, Index[name, Ext[k, 2]], ind3, Index[name, Ext[k, 3]], ind4], l]};
RenameSpin4[crealist_] := crealist //. {crea[h_, List[ind1___, Index[name_, Ext[k_]], ind2___, Index[name_, Ext[k_]], ind3___, Index[name_, Ext[k_]], ind4___, Index[name_, Ext[k_]], ind5___], l__] -> crea[h, List[ind1, Index[name, Ext[k, 1]], ind2, Index[name, Ext[k, 2]], ind3, Index[name, Ext[k, 3]], ind4, Index[name, Ext[k, 4]], ind5], l]};

MakeCreaListoutput[cl_] := Reverse[cl] /. {crea[f_, _, k_] :> {f, k}};

MakeCreaList /: MakeCreaList[x___, a_*b_, y___] := MakeCreaList[x, b, y] /; Not[FieldQ[a]];
MakeCreaList /: MakeCreaList[x___, a_*b_, y___] := MakeCreaList[x, a, b, y] /; FieldQ[a] && FieldQ[b];
MakeCreaList /: MakeCreaList[x___, Dot[a_, b_], y___] := MakeCreaList[x, a, b, y] /; FieldQ[a] && FieldQ[b];
MakeCreaList /: MakeCreaList[x___, a_^n_, y___] := MakeCreaList[x, Sequence @@ Table[a,{n}], y] /; FieldQ[a] && (n > 1);
MakeCreaList /: MakeCreaList[x___, a_, y___] := MakeCreaList[x, y] /; Not[FieldQ[a]];

(* Reordering of the CreaList, 28, 02, 08 *)
ReOrderCL[cl_] := Block[{temp, temp2, tempgh, tempdirac, tempantigh, tempantidirac, myantifiledQ},
       myantifiledQ[ff_?(#=!=CC&)[___]] := AntiFieldQ[ff];
       myantifiledQ[ff_?(Not[FieldComponentQ[#] === True]&)] := AntiFieldQ[ff];
       myantifiledQ[CC[ff_][___]] := AntiFieldQ[ff];
       temp = cl //. del[ff_,_] :> ff /. Index[name_, __] :> Index[name, noindname];
       If[Not[FreeQ[temp, _?FermionQ]],
          tempgh = Sort[Select[temp, (GhostFieldQ[#] && Not[myantifiledQ[#]])&]];
          tempantigh = Sort[Select[temp, (GhostFieldQ[#] && myantifiledQ[#])&]];
          tempdirac = Sort[Select[temp, ((Spin32FieldQ[#] || DiracFieldQ[#] || MajoranaFieldQ[#]) && Not[myantifiledQ[#]])&]];
          tempantidirac = Sort[Select[temp, ((Spin32FieldQ[#] || DiracFieldQ[#] || MajoranaFieldQ[#]) && myantifiledQ[#])&]];
          temp = Sort[DeleteCases[temp, _?FermionQ]];
          temp = (*Join[temp, Flatten[Table[{tempantigh[[tip]], tempgh[[tip]]}, {tip, Length[tempgh]}]], Flatten[Table[{tempantidirac[[tip]], tempdirac[[tip]]}, {tip, Length[tempdirac]}]]]];*)
          temp = Join[Flatten[Table[{tempantidirac[[tip]], tempdirac[[tip]]}, {tip, Length[tempdirac]}]], temp, Flatten[Table[{tempantigh[[tip]], tempgh[[tip]]}, {tip, Length[tempgh]}]]]];
       $CreaListSign = Signature[Select[temp, FermionQ]];
       temp];



(* ::Section:: *)
(*Field content handling, and FCList*)


GetFCOP/:GetFCOP[x___,Times[a_,b_],y___]:=GetFCOP[x,a,b,y];
GetFCOP/:GetFCOP[x___,NonCommutativeMultiply[a_,b_],y___]:=GetFCOP[x,a,b,y];
GetFCOP/:GetFCOP[x___,Dot[a_,b_],y___]:=GetFCOP[x,a,b,y];
GetFCOP/:GetFCOP[x___,Power[a_,b_Integer],y___]:=GetFCOP[x,Sequence @@ Table[a, {b}],y];

FCOPAllowed = {Times, Dot, Power, NonCommutativeMultiply};

GetFC /: GetFC[x___, a_, y___] := GetFC[x, y] /; Not[FieldQ[a]];
GetFC /: GetFC[x___, a_*b_, y___] := GetFC[x, b, y] /; Not[FieldQ[a]];
GetFC /: GetFC[x___, a_[indices__], y___] := GetFC[x, a, y] /; FieldQ[a];
GetFC /: GetFC[x___, a_^n_, y___] := GetFC[x, a^(n - 1), a, y] /; FieldQ[a] && (n > 1);
GetFC /: GetFC[x___, del[a_, mu_], y___] := GetFC[x, a, y] /; FieldQ[a];

GetFieldContent[expr_] := Block[{output,temp},
      If[Not[MemberQ[FCOPAllowed, Head[expr]]], Message[GetFC::Head] Print[" ", Head[expr]]; Abort[]];
      temp = If[Head[expr] === Power, GetFCOP[expr], GetFCOP @@ expr];
      temp = GetFC @@ temp;
      output = Sort[List @@ temp]];


(* ::Section:: *)
(*THE CORE*)


FromVertexTerm[expr_, CreaList_, kin_] := Block[{temp, temp2, temp3, output, old$IterationLimit = $IterationLimit},
    Clear[StoreInt];
    $IterationLimit = Infinity;

    temp = ToCheckOperatorChain[expr]; 
    temp3 = I temp /. ToOperatorChain -> (OperatorChain @@ Join[List[##], CreaList] &);
    temp3 = FieldExpand[temp3 /. uwave[___] :> 1];

    temp3 = temp3 //. NTIdelta -> delta;
    temp3 = temp3 /. MRIndexDelta -> delta;

    If[FR$FExpand,
      temp3 = LorentzContract[Expand[temp3]]; 
      Off[Simplify::time];
      temp3 = If[$VersionNumber>8,Factor[Expand[temp3]],Simplify[Expand[temp3],TimeConstraint->0.01]]; 
      On[Simplify::time];
      ,
      temp3 = LorentzContract[Expand[temp3,_?(Not[FreeQ[#,Lorentz]]&&Not[FreeQ[#,Spin]]&&Not[FreeQ[#,FV]]&&Not[FreeQ[#,Ga]]&&Not[FreeQ[#,ME]]&&Not[FreeQ[#,Eps]]&)]];(*pattern added by celine*)
      temp3 = Expand[temp3,Indices]; (*Indices added by celine*)
    ];
    temp3 = temp3 /. {dd_?(SymTensQ[#] === True &)[ind__] :> SortSymTens[dd][ind], dd_?(AntiSymTensQ[#] === True &)[ind__] :> SortAntiSymTens[dd][ind],
                            ff_?(StrucConstQ[#] === True &) :> SortStrucConst[ff]};

    temp3 = If[FR$FExpand,Expand[temp3],Expand[temp3,Indices]];
    temp3 = temp3 //. {nt_?(NoTensQ)[NTIndex[name_, jj_]] tt_[ind1___, Index[name_, jj_, kk_], ind2___] :> nt[NTI[name, jj,kk]] tt[ind1, Index[name,jj,kk],ind2],
             func_[xx___, nt_?(NoTensQ)[NTIndex[name_, jj_]], yy___] tt_[ind1___, Index[name_, jj_, kk_], ind2___] :> func[xx, nt[NTI[name, jj,kk]], yy] tt[ind1, Index[name,jj,kk],ind2],
             func1_[xx1___, func2_[xx2___, nt_?(NoTensQ)[NTIndex[name_, jj_]], yy2___], yy1___] tt_[ind1___, Index[name_, jj_, kk_], ind2___] :> func1[xx1, func2[xx2, nt[NTI[name, jj,kk]], yy2], yy1] tt[ind1, Index[name,jj,kk],ind2],
             nt_?(NoTensQ)[NTIndex[name_, jj_]] func_[xx___, tt_[ind1___, Index[name_, jj_, kk_], ind2___], yy___] :> nt[NTI[name, jj,kk]] func[xx, tt[ind1, Index[name,jj,kk],ind2], yy],
             nt_?(NoTensQ)[NTIndex[name_, jj_]] func1_[xx1___, func2_[xx2___, tt_[ind1___, Index[name_, jj_, kk_], ind2___], yy2___], yy1___] :> nt[NTI[name, jj,kk]] func1[xx1,func2[xx2, tt[ind1, Index[name,jj,kk],ind2], yy2],yy1],
             func1_[xx1___, nt_?(NoTensQ)[NTIndex[name_, jj_]], yy1___] func2_[xx2___, tt_[ind1___, Index[name_, jj_, kk_], ind2___], yy2___] :> func1[xx1, nt[NTI[name, jj,kk]], yy1] func2[xx2, tt[ind1, Index[name,jj,kk],ind2], yy2]};
    temp3 = OrderEps[temp3];

    temp3= temp3 /. Index[Lorentz, Except[Ext[_]|Ext[_, _]|IntDone[_], i_?(Not[NumericQ[#]]&)], j___] :> Index[Lorentz, IntLor[i, kin], j];
    temp3= temp3 /. Index[Except[Lorentz, name_], Except[Ext[_]|Ext[_,_]|IntDone[_], i_?(Not[NumericQ[#]]&)], j___] :> Index[name, Int[i, kin], j];
    temp3= temp3 /. NTI[Except[Lorentz, name_], Except[Ext[_]|Ext[_,_]|IntDone[_], i_?(Not[NumericQ[#]]&)], j___] :> NTI[name, Int[i, kin], j];
    temp3 = temp3 /. IntDone -> Identity;

    $IterationLimit = old$IterationLimit;
    temp3 = FieldExpand[temp3];
    output = temp3];


(* ::Section:: *)
(*Index handling*)


LorentzContract[expr_] := Block[{output, temp},
      (*temp = Expand[expr] //. {ME -> MEME, FV -> FVFV};
      output = Factor[temp] //. {MEME -> ME, FVFV -> FV}];*)
    If[FR$FExpand,
       Expand[expr]/.{ME->MEME,FV->FVFV}/.{MEME->ME,FVFV->FV}
       ,
       Expand[expr,_?(Not[FreeQ[#,FV]]&&Not[FreeQ[#,ME]]&&Not[FreeQ[#,Ga]]&)]/.{ME->MEME,FV->FVFV}/.{MEME->ME,FVFV->FV}]
];

ReplaceFieldIndex[f_[ind___], l1_List, l2_List] := Block[{output, temp},
    If[Length[l1] != Length[l2], Message[Sym::NoSym]; Abort[]];
    tempfield = f[ind];
    Do[tempfield = ReplacePart[tempfield, l2[[kk]], l1[[kk]]], {kk, Length[l1]}];
    output = $done[tempfield] //. { $done[f[iddi___]] -> $done[f][iddi]}];
    
KillMapIndexed[{mu_, {_}}] := mu;

AllPermutations[ll_List] := (KillMapIndexed[#] & /@ # &) /@ Permutations[MapIndexed[List, ll]];

SymmetrizeFieldIndex[f_[ind___], l_] := Block[{permlist, tempfield, output, lll},
    If[Length[l] == 1,
      lll = Flatten[Position[$IndList[f], l[[1]]]];
      permlist = AllPermutations[Table[Extract[f[ind], lll[[kk]]], {kk, Length[lll]}]];
      tempfield = (Plus @@ ((ReplaceFieldIndex[f[ind], lll, #] &) /@ permlist))/Factorial[Length[lll]],
      lll = Flatten[Position[$IndList[f], l[[1]]]];
      permlist = AllPermutations[Table[Extract[f[ind], lll[[kk]]], {kk, Length[lll]}]];
      tempfield = (Plus @@ ((ReplaceFieldIndex[f[ind], lll, #] &) /@ permlist))/Factorial[Length[lll]];
      tempfield = tempfield //. {$done[f][iddi___] :> SymmetrizeFieldIndex[f[iddi], Rest[l]]}];
    output = Expand[tempfield]];

AntiSymmetrizeFieldIndex[f_[ind___], l_] := Block[{permlist, tempfield, output, lll},
    If[Length[l] == 1,
      lll = Flatten[Position[$IndList[f], l[[1]]]];
      permlist = AllPermutations[Table[Extract[f[ind], lll[[kk]]], {kk, Length[lll]}]];
      tempfield = (Plus @@ (((Signature[#]ReplaceFieldIndex[f[ind], lll, #]) &) /@ permlist))/Factorial[Length[lll]],
      lll = Flatten[Position[$IndList[f], l[[1]]]];
      permlist = AllPermutations[Table[Extract[f[ind], lll[[kk]]], {kk, Length[lll]}]];
      tempfield = (Plus @@ (((Signature[#]ReplaceFieldIndex[f[ind], lll, #]) &) /@ permlist))/Factorial[Length[lll]];
      tempfield = tempfield //. {$done[f][iddi___] :> AntiSymmetrizeFieldIndex[f[iddi], Rest[l]]}];
   output = Expand[tempfield]];


(* ::Section::Closed:: *)
(*Quantum number conservation*)


ConserveQN[fc_List] := ConserveQN[fc, MR$QuantumNumbers];

ConserveQN[fc_List, qnumbers_] := Block[{output = {}, qcons},
                             Do[If[(Plus @@ (qnumbers[[xx]] /@ fc)) != 0, 
                                   Message[QN::NonConserv];
                                   Print["Quantum number ", qnumbers[[xx]], " not conserved in vertex ", fc, "."];
                                   qcons = False,
                                   (* else *) qcons = True];
                             AppendTo[output, {fc, qnumbers[[xx]], qcons}],                                   
                             {xx, Length[qnumbers]}];
                             Return[output]];


(* ::Section:: *)
(*Tensor structure handling*)


        
GetLorentzSquares = 
    {f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___]^2 -> f[a, Index[Lorentz, mu, 1], b] ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]] f[a, Index[Lorentz, mu, 2], b]};

GetIndexSquares = {f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___]^2 -> f[a, Index[name, mu, 1], b] MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]] f[a, Index[name, mu, 2], b]};
      

GetLorentzStructureRules = 
    Dispatch[{f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___]g_[c___, Index[Lorentz, mu_], d___] -> f[a, Index[Lorentz, mu, 1], b]ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]]g[c, Index[Lorentz, mu, 2], d], 
        f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___, Index[Lorentz, mu_], c___] -> f[a, Index[Lorentz, mu, 1],b, Index[Lorentz, mu, 2], c] ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]],
        func_[f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], Index[Lorentz, mu_]] -> func[f[a, Index[Lorentz, mu, 1], b], Index[Lorentz, mu, 2]] ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]],
        func_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___]g_[c___, Index[Lorentz, mu_], d___] -> func[x, f[a, Index[Lorentz, mu, 1], b], y]ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]]g[c, Index[Lorentz, mu, 2], d], 
        func1_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___]func2_[zz___, g_[c___, Index[Lorentz, mu_], d___], t___] -> func1[x, f[a, Index[Lorentz, mu, 1], b], y]ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]]func2[zz, g[c, Index[Lorentz, mu, 2], d], t],
        func_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___][ind___]g_[c___, Index[Lorentz, mu_], d___] -> func[x, f[a, Index[Lorentz, mu, 1], b], y][ind]ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]]g[c, Index[Lorentz, mu, 2], d], 
        func1_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___][ind___]func2_[zz___, g_[c___, Index[Lorentz, mu_], d___], t___] -> func1[x, f[a, Index[Lorentz, mu, 1], b], y][ind]ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]]func2[zz, g[c, Index[Lorentz, mu, 2], d], t],
        func1_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___][ind1___]func2_[zz___, g_[c___, Index[Lorentz, mu_], d___], t___][ind2___] -> func1[x, f[a, Index[Lorentz, mu, 1], b], y][ind1]ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]]func2[zz, g[c, Index[Lorentz, mu, 2], d], t][ind2],
        gun_[u___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], v___, g_[c___, Index[Lorentz, mu_], d___], w___] -> gun[u, f[a, Index[Lorentz, mu, 1], b], v, g[c, Index[Lorentz, mu, 2],d], w]ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]], 
        gun_[u___, func_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___], v___, g_[c___, Index[Lorentz, mu_], d___], w___] -> gun[u, func[x, f[a, Index[Lorentz, mu, 1], b], y], v, g[c, Index[Lorentz, mu, 2], d], w] ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]], 
        gun_[u___, func1_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___], v___, func2_[zz___, g_[c___, Index[Lorentz, mu_], d___], t___], w___] -> gun[u, func1[x, f[a, Index[Lorentz, mu, 1], b], y], v, func2[zz, g[c, Index[Lorentz, mu, 2], d], t], w] ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]],
        gun_[u___, func_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___][ind___], v___, g_[c___, Index[Lorentz, mu_], d___], w___] -> gun[u, func[x, f[a, Index[Lorentz, mu, 1], b], y][ind], v, g[c, Index[Lorentz, mu, 2], d], w] ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]], 
        gun_[u___, func1_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___][ind___], v___, func2_[zz___, g_[c___, Index[Lorentz, mu_], d___], t___], w___] -> gun[u, func1[x, f[a, Index[Lorentz, mu, 1], b], y][ind], v, func2[zz, g[c, Index[Lorentz, mu, 2], d], t], w] ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]],
        gun_[u___, func1_[x___, f_[a___, Index[Lorentz, mu_?(Not[NumericQ[#]]&)], b___], y___][ind1___], v___, func2_[zz___, g_[c___, Index[Lorentz, mu_], d___], t___][ind2___], w___] -> gun[u, func1[x, f[a, Index[Lorentz, mu, 1], b], y][ind1], v, func2[zz, g[c, Index[Lorentz, mu, 2], d], t][ind2], w] ME[Index[Lorentz, mu, 1], Index[Lorentz, mu, 2]]}];
        



GetIndexStructureRules = 
    Dispatch[{f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___]g_[c___, Index[name_, mu_], d___] -> f[a, Index[name, mu, 1], b]MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]]g[c, Index[name, mu, 2],d], 
        f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___, Index[name_, mu_], c___] -> f[a, Index[name, mu, 1],b, Index[name, mu, 2],c] MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]],
        func_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___]g_[c___, Index[name_, mu_], d___] -> func[x, f[a, Index[name, mu, 1], b], y]MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]]g[c, Index[name, mu, 2], d], 
        func1_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___]func2_[zz___, g_[c___, Index[name_, mu_], d___], t___] -> func1[x, f[a, Index[name, mu, 1], b], y]MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]]func2[zz, g[c, Index[name, mu, 2], d], t],
        func_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___][ind___]g_[c___, Index[name_, mu_], d___] -> func[x, f[a, Index[name, mu, 1], b], y][ind]MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]]g[c, Index[name, mu, 2], d], 
        func1_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___][ind___]func2_[zz___, g_[c___, Index[name_, mu_], d___], t___] -> func1[x, f[a, Index[name, mu, 1], b], y][ind]MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]]func2[zz, g[c, Index[name, mu, 2], d], t],
        func1_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___][ind1___]func2_[zz___, g_[c___, Index[name_, mu_], d___], t___][ind2___] -> func1[x, f[a, Index[name, mu, 1], b], y][ind1]MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]]func2[zz, g[c, Index[name, mu, 2], d], t][ind2],
        gun_[u___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], v___, g_[c___, Index[name_, mu_], d___], w___] -> gun[u, f[a, Index[name, mu, 1], b], v, g[c, Index[name, mu, 2],d], w]MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]], 
        gun_[u___, func_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___], v___, g_[c___, Index[name_, mu_], d___], w___] -> gun[u, func[x, f[a, Index[name, mu, 1], b], y], v, g[c, Index[name, mu, 2], d], w] MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]], 
        gun_[u___, func1_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___], v___, func2_[zz___, g_[c___, Index[name_, mu_], d___], t___], w___] -> gun[u, func1[x, f[a, Index[name, mu, 1], b], y], v, func2[zz, g[c, Index[name, mu, 2], d], t], w] MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]],
        gun_[u___, func_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___][ind___], v___, g_[c___, Index[name_, mu_], d___], w___] -> gun[u, func[x, f[a, Index[name, mu, 1], b], y][ind], v, g[c, Index[name, mu, 2], d], w] MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]], 
        gun_[u___, func1_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___][ind___], v___, func2_[zz___, g_[c___, Index[name_, mu_], d___], t___], w___] -> gun[u, func1[x, f[a, Index[name, mu, 1], b], y][ind], v, func2[zz, g[c, Index[name, mu, 2], d], t], w] MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]],
        gun_[u___, func1_[x___, f_[a___, Index[Except[Lorentz, name_], mu_?(Not[NumericQ[#]]&)], b___], y___][ind1___], v___, func2_[zz___, g_[c___, Index[name_, mu_], d___], t___][ind2___], w___] -> gun[u, func1[x, f[a, Index[name, mu, 1], b], y][ind1], v, func2[zz, g[c, Index[name, mu, 2], d], t][ind2], w] MRIndexDelta[Index[name, mu, 1], Index[name, mu, 2]]}];




ExpandTensorStructure[expr_] := Block[{temp},
(* Then we must disentangle all the squares which corespond to contracted indices *)
    temp = expr //. GetLorentzSquares //. GetIndexSquares;
(* Finally we make the tensor structure obvious by inserting ME's and delta's *)
    temp = temp //. GetLorentzStructureRules //.GetIndexStructureRules];
