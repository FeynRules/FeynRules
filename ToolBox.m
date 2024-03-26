(* ::Package:: *)

(* ::Title:: *)
(*ToolBox V1.2*)


(* ::Text:: *)
(*This is the FeynRules ToolBox.*)
(* It contains tools useful for model building.     *)
(**)
(* 17.02.09, CD - Added the function BuildPDGs[].*)
(* *)
(* 06.10.08, CD - Changed printing in SelectVertices. It does no longer complain about not finding charge conjugated vertices.*)


(* ::Section:: *)
(*Useful functions and boolean functions*)


(* ::Text:: *)
(*For defining new ToolBox functions, the following set of boolean functions might be useful:*)
(**)
(*   numQ: returns True for all parameters in the model, as well as for all numerical values.*)
(*               Note that the components of a tensor a considered as parameters, e.g. numQ[T[a, i, j] = True, and   *)
(*               numQ[Ga[mu, r, s]] = True.*)
(**)
(*   CnumQ: returns True if a parameter is a complex parameter.*)
(**)
(*   TensQ: returns True for a tensor.*)
(**)
(*   CompTensQ: returns True for a complex parameter.*)
(**)
(*   FieldQ: FieldQ[\[Psi]] is True, is \[Psi] is a field.*)
(**)
(*   FermionQ: returns True for fermions.*)
(**)
(*   BosonQ: returns True for bosons.*)
(**)
(*   DiracFieldQ: returns True for Dirac fermions.*)
(**)
(*   MajoranaFieldQ: returns True for Majorana fermions.*)
(**)
(*   ScalarFieldQ: returns True for scalar fields.*)
(**)
(*   VectorFieldQ: returns True for vector fields.*)
(**)
(*   Spin2FieldQ: returns True for spin2 fields.*)
(**)
(*   GhostFieldQ: returns True for ghost fields.*)
(**)
(*   UnitaryQ: returns True for unitary tensors.*)
(**)
(*   HermitianQ: returns True for hermitian tensors.*)
(**)
(*   OrthogonalQ: returns True for orthogonal tensors.  *)
(**)
(*Furthermore, the following function can be useful*)
(**)
(*   GetFieldContent: extracts the fields contained in a a piece of lagrangian.*)
(**)
(*   MR$QuantumNumbers: a list containing all quantum numbers defined in the model-file.*)
(**)
(*   $IndList: $IndList[\[Psi]] is the list of indices declared for the field or the tensor \[Psi].*)


(* ::Section:: *)
(*ExpandIndices*)


(* ::Text:: *)
(*ExpandIndices[L] makes the index structure of the lagrangian L explicit.*)
(**)
(*       E.g. HC[\[Psi]] . Ga[mu] . \[Psi]   ->   Ga[mu, s, r] HC[\[Psi]][s] . \[Psi][r].*)
(*       *)
(* The option FlavorExpand is available, and works in the same way as for FeynmanRules[L]. *)
(* *)
(* ExpandIndices has the following options:*)
(*      *)
(*      1) FlavorExpand:                       FlavorExpand -> {....} expands the lagrangian of the indices specified in the list. *)
(*                                                         FlavorExpand -> True expands the lagrangian of all flavor indices. *)
(*      2) IndexExpand:                        Similar to FlavorExpand -> {...}, but expands over all indices in the list, not only flavor.*)
(*      3) MaxParticles:                        MaxParticles -> n selects only those pieces of the lagrangian which contain at most n particles.*)
(*      4) MinParticles:                         MinParticles -> n selects only those pieces of the lagrangian which contain at least n particles.*)
(*      5) MaxCanonicalDimension:    MaxCanonicalDimension -> n selects only those pieces of the lagrangian with canonical dimension less than n.*)
(*      6) MaxCanonicalDimension:    MinCanonicalDimension -> n selects only those pieces of the lagrangian with canonical dimension greater than n.*)
(*      7) SelectParticles:                     SelectParticles -> {{...}, {...}, ...} selects only those pieces of the lagrangian specified in the list*)


(* ::Section:: *)
(*Selection functions*)


(* ::Text:: *)
(*GetKineticTerms :       GetKineticTerms[L] selects all kinetic terms in the lagrangian.*)
(*                                     Options: FlavorExpand, MinParticles.*)
(**)
(*GetMassTerms :          GetMassTerms[L] selects all mass terms in the lagrangian.*)
(*                                     Options: FlavorExpand, MinParticles.*)
(**)
(*GetQuadraticTerms :  GetQuadraticTerms[L] selects all quadratic terms in the lagrangian.*)
(*                                    Options: FlavorExpand, MinParticles.*)
(**)
(*GetInteractionTerms : GetInteractionTerms[L] selects all interactions terms in the lagrangian.*)
(*                                    Options: FlavorExpand, MaxParticles.*)
(*                                    *)
(* SelectFieldContent:    SelectFieldContent[L, {{...}, {...},...}] is an alias for ExpandIndices[L,  SelectParticles -> {{...}, {...},...}]*)
(*                                     Options: FlavorExpand, MaxParticles, MinParticles.*)


(* ::Subsection::Closed:: *)
(*The GetQuadraticTerms routine*)


Options[GetQuadraticTerms] = {FlavorExpand -> False, MinParticles -> Automatic};

GetQuadraticTerms[lagrangian_, options___] := ExpandIndices[lagrangian, options, MaxParticles -> 2, MinParticles -> 2, SymmetryFactor -> True];


(* ::Subsection:: *)
(*The GetKineticTerms routine*)


Options[GetKineticTerms] = {FlavorExpand -> False, MinParticles -> Automatic};

GetKineticTerms[lagrangian_, options___] := Block[{lag},
      lag = ExpandIndices[lagrangian, options, MaxParticles -> 2, SymmetryFactor -> True];
      lag = If[Head[lag] === Plus, List @@ lag, {lag}];
      lag = Select[lag, Not[FreeQ[#, del]]&];
      If[Length[lag]==1, Plus@@Join[{0},lag], Plus @@ Select[lag, Length[GetFieldContent[#]]==2&]]];


(* ::Subsection:: *)
(*The GetMassTerms routine*)


Options[GetMassTerms] = {FlavorExpand -> False, MinParticles -> Automatic, Fields -> Automatic, SymmetryFactor -> True};

GetMassTerms[lagrangian_, options___] := Block[{lag, ffield, tempoptions, fieldlist, 
      symfac = SymmetryFactor /. {options} /. Options[GetMassTerms]},
      lag = ExpandIndices[lagrangian, options, MaxParticles -> 2, SymmetryFactor -> symfac];
      lag = If[Head[lag] === Plus, List @@ lag, {lag}];
      lag = Select[lag, FreeQ[#, del]&];
      If[Length[lag]==1, Plus@@Join[{0},lag], Plus @@ Select[lag, Length[GetFieldContent[#]]==2&]]];


(* ::Subsection::Closed:: *)
(*The GetInteractionTerms routine*)


Options[GetInteractionTerms] = {FlavorExpand -> False, MaxParticles -> Automatic, SymmetryFactor -> True};

GetInteractionTerms[lagrangian_, options___] := ExpandIndices[lagrangian, options, MinParticles -> 3, SymmetryFactor -> True];





(* ::Subsection::Closed:: *)
(*SelectFieldContent*)


SelectFieldContent[lagrangian_, FClist_, options___] := ExpandIndices[lagrangian, options, SelectParticles -> FClist]


(* ::Section:: *)
(*CheckQuantumNumbers*)


(* ::Text:: *)
(*CheckQuantumNumbers[L] checks if all quantum numbers are conserved in the lagrangian L.*)
(**)
(*Options:*)
(*1) FlavorExpand*)
(*2) MaxParticles*)
(*3) MinParticles*)
(*4) QN :  QN -> {Q1, Q1, ...} checks only if the quantum numbers Q1, Q2, ... are conserved*)
(* *)
(*NeutralCombinationQ[prtcls__List]  takes a list of particles and checks whether all indices can be contracted (checks whether each index appears an even number of times) and checks that the sum of the U(1) quantum numbers is 0 for each quantum number.*)


(* ::Subsection:: *)
(*The CheckQuantumNumbers routine*)


Options[CheckQuantumNumbers] = {FlavorExpand -> False, MaxParticles -> Automatic, MinParticles -> Automatic, QN -> MR$QuantumNumbers}

CheckQuantumNumbers[lagrangian_, options___] := Block[{lag, QNC = True, qn = {}, qnlist},
      qnlist = Flatten[{QN /. {options} /. QN -> MR$QuantumNumbers}];
      Do[If[Not[MemberQ[MR$QuantumNumbers, qnlist[[$kk]]]], Print[qnlist[[$kk]], " is not a quantum number. No check will be performed."], qn = Append[qn, qnlist[[$kk]]]],
         {$kk, Length[qnlist]}];
      lag = ExpandIndices[lagrangian, options];
      lag = If[Head[lag] === Plus, List @@ lag, {lag}];
      lag = GetFieldContent /@ Expand[lag];
      Do[If[(Plus @@ (qn[[$xx]] /@ lag[[$kk]])) != 0, QNC = False;
                                   Print["Quantum number ", qn[[$xx]], " not conserved in vertex ", lag[[$kk]], "."]],
                             {$kk, Length[lag]}, {$xx, Length[qn]}];
      If[QNC, Print["All quantum numbers are conserved."]]];


(* ::Subsection:: *)
(*NeutralCombinationQ routine*)


NeutralCombinationQ[prtcls__List]:=Module[{qns,nm},
(*First check that all the indices can be contracted in principle.*)
qns=Flatten[$IndList/@prtcls];(*Print[InputForm[qns]];*)
If[MemberQ[qns,Index[Spin]],qns=DeleteCases[qns,Index[Lorentz]]];(*Print[InputForm[qns]];*)
nm[ind_]:=Length[Cases[qns,ind]];
If[Length[Select[nm/@DeleteDuplicates[qns],OddQ]]>0,Return[False]];

(*Next, check all the U (1) quantum numbers.*)
qns=(Plus@@(#/@prtcls))& /@ MR$QuantumNumbers;(*Print[qns];*)
If[Length[Select[qns,PossibleZeroQ]]=!=Length[qns],Return[False]];

(*If passed, return True.*)
True
];


(* ::Section:: *)
(*Check for diagonal lagrangians*)


(* ::Text:: *)
(*CheckDiagonalQuadraticTerms[L] checks if all quadratic terms in L are flavor diagonal. Returns True, if the kinetic terms are flavor diagonal, False otherwise.*)
(*      Options: FlavorExpand, MinParticles.*)
(*      *)
(*CheckDiagonalKineticTerms[L] checks if all kinetic terms in L are flavor diagonal. Returns True, if the kinetic terms are flavor diagonal, False otherwise.*)
(*      Options: FlavorExpand, MinParticles.*)
(*      *)
(* CheckDiagonalMassTerms[L] checks if all mass terms in L are flavor diagonal. Returns True, if the kinetic terms are flavor diagonal, False otherwise.*)
(*      Options: FlavorExpand, MinParticles.*)


(* ::Subsection:: *)
(*The CheckDiagonalMassTerms routine*)


Options[CheckDiagonalMassTerms] = {FlavorExpand -> False, MinParticles -> Automatic};

(*CheckDiagonalMassTerms[lagrangian_, options___] := Block[{lag, DMT = True},
      lag = GetMassTerms[lagrangian, options];
      If[lag === 0, Print["No Mass terms found."],
         lag = If[Head[lag] === Plus, List @@ lag, {lag}];
         lag = GetFieldContent /@ Expand[lag];
         Do[If[lag[[$kk,1]] =!= anti[lag[[$kk,2]]], DMT = False; 
               Print["Non diagonal mass term found: ", lag[[$kk]]]],
            {$kk, Length[lag]}];
         If[DMT, Print["All mass terms are diagonal."]; True, False]]];*)

CheckDiagonalMassTerms[lagrangian_, options___] := Block[{lag, DMT = True, tmp ={}, tmptmp, fcl, fcl1},
      lag = GetMassTerms[lagrangian, options];
      If[lag === 0, Print["No Mass terms found."],
         lag = If[Head[lag] === Plus, List @@ lag, {lag}];
         fcl=Sort/@(GetFieldContent/@lag);
         fcl1=KillDoubles[fcl];
         Do[tmp=Append[tmp,Plus@@((lag[[#]]&)@@@Position[fcl,fcl1[[nfcl]]])],{nfcl,Length[fcl1]}];
            Do[If[fcl1[[$kk,1]] =!= anti[fcl1[[$kk,2]]],
                  tmptmp = tmp[[$kk]] //. psi_?FieldQ -> 1 //. (ProjP|ProjM)[_,_] -> 1/2; 
                  If[Not[NumericQ[NumericalValue[tmptmp]]],Print["Warning: not numerical value encountered. Unable to decide whether mass term is diagonal"]];
                  If[(Abs[NumericalValue[tmptmp]] < N[10^(-8)])=!=True, DMT = False;
                     Print["Non diagonal mass term found: ", tmp[[$kk]]]]],
            {$kk, Length[tmp]}];
         If[DMT, Print["All mass terms are diagonal."]; True, False]]];


(* ::Subsection:: *)
(*The CheckDiagonalKineticTerms routine*)


Options[CheckDiagonalKineticTerms] = {FlavorExpand -> False, MinParticles -> Automatic};

(*CheckDiagonalKineticTerms[lagrangian_, options___] := Block[{lag, DKT = True, nondiagpiece},
      lag = GetKineticTerms[lagrangian, options];
      If[lag === 0, Print["No kinetic terms found."],
         lag = If[Head[lag] === Plus, List @@ lag, {lag}];
         lag = GetFieldContent /@ Expand[lag];
         Do[If[lag[[$kk,1]] =!= anti[lag[[$kk,2]]], DKT = False; 
               Print["Non diagonal kinetic term found: ", lag[[$kk]]]],
            {$kk, Length[lag]}];
         If[DKT, Print["All kinetic terms are diagonal."]; True, False]]];*)

      

CheckDiagonalKineticTerms[lagrangian_, options___] := Block[{lag, DKT = True, fcl, fcl1, tmp={}, tmptmp},
      lag = GetKineticTerms[lagrangian, options];
      If[lag === 0, Print["No kinetic terms found."],
         lag = If[Head[lag] === Plus, List @@ lag, {lag}];
      fcl=Sort/@(GetFieldContent/@lag);
      fcl1=KillDoubles[fcl];
      (* We delete gauge fixing terms like del[phi,mu] A[mu] *)
      fcl1 = DeleteCases[fcl1,{_?ScalarFieldQ,_?VectorFieldQ}|{_?VectorFieldQ,_?ScalarFieldQ}];
      Do[tmp=Append[tmp,Plus@@((lag[[#]]&)@@@Position[fcl,fcl1[[nfcl]]])],{nfcl,Length[fcl1]}];
         Do[If[fcl1[[$kk,1]] =!= anti[fcl1[[$kk,2]]],
               tmptmp = tmp[[$kk]] //. del[psi_, mu_] -> psi //. psi_?FieldQ -> 1 //.TensDot[Ga[_],ProjM|ProjP][_,_]->1/2//. Ga[__] -> 1 //. (ProjP|ProjM)[_,_] -> 1/2;
               If[Not[NumericQ[NumericalValue[tmptmp]]], Print["Warning: not numerical value encountered. Unable to decide whether kinetic term is diagonal"]];
               If[(Abs[NumericalValue[tmptmp]] < N[10^(-8)])=!=True, DKT = False;
                  Print["Non diagonal kinetic term found: ", tmp[[$kk]]]]],
            {$kk, Length[tmp]}];
         If[DKT, Print["All kinetic terms are diagonal."]; True, False]]];


(* ::Subsection:: *)
(*The CheckDiagonalQuadraticTerms routine*)


Options[CheckDiagonalQuadraticTerms] = {FlavorExpand -> False, MinParticles -> Automatic};

CheckDiagonalQuadraticTerms[lagrangian_, options___] := Block[{lag, DQT = True, tmp = {}, tmptmp, fcl, fcl1},
      lag = GetQuadraticTerms[lagrangian, options];
      If[lag === 0, Print["No quadratic terms found."],
         lag = If[Head[lag] === Plus, List @@ lag, {lag}];
         fcl=Sort/@(GetFieldContent/@lag);
         fcl1=KillDoubles[fcl];
         Do[tmp=Append[tmp,Plus@@((lag[[#]]&)@@@Position[fcl,fcl1[[nfcl]]])],{nfcl,Length[fcl1]}];
         Do[If[fcl1[[$kk,1]] =!= anti[fcl1[[$kk,2]]],
               tmptmp = tmp[[$kk]] //. del[psi_, mu_] -> psi //. psi_?FieldQ -> 1//.TensDot[Ga[_],ProjM|ProjP][_,_]->1/2//. Ga[__] -> 1 //. (ProjP|ProjM)[_,_] -> 1/2;
               If[Not[NumericQ[NumericalValue[tmptmp]]],Print["Warning: not numerical value encountered. Unable to decide whether quadratic term is diagonal"]];
                  If[(NumericalValue[tmptmp] < N[10^(-8)])=!=True, DQT = False;
                     Print["Non diagonal quadratic term found: ", tmp[[$kk]]]]],
               {$kk, Length[tmp]}];
         If[DQT, Print["All quadratic terms are diagonal."]; True, False]]];


(* ::Section:: *)
(*Field Strength tensors*)


(* ::Subsection:: *)
(*Field Strength tensor*)


(*Non abelian vector field *)

FR$DSign = 1;

FS[bos_, mu_, nu_, aa_, ff_, cpco_] := Module[{bb, cc}, del[bos[nu, aa],mu] - del[bos[mu,aa] ,nu] + FR$DSign *cpco ff[aa ,bb ,cc ]bos[mu,bb]bos[nu,cc]];

(* U (1) vector field *)

FS[bos_, mu_, nu_] := del[bos[nu],mu] - del[bos[mu] ,nu];


(* ::Subsection:: *)
(*Dual tensor*)


Dual[FS][g_, mu_, nu_] := Module[{xx, yy}, 1/2 Eps[mu, nu, xx, yy] FS[g, xx, yy]];
Dual[FS][g_, mu_, nu_, aa_] := Module[{xx, yy}, 1/2 Eps[mu, nu, xx, yy] FS[g, xx, yy, aa]];
Dual[FS][ww_, mu_, nu_, aa_, f_, gg_] := Module[{xx, yy},  1/2 Eps[mu, nu, xx, yy] FS[ww, xx, yy, aa, f, gg]];


(* ::Section:: *)
(*Momentum conservation*)


(* ::Text:: *)
(*MomentumReplace[{{part1, 1}, {part2, 2}, ...}, vertex}, n] replaces the momentum of the particle n everywhere by minus the sum of all other momenta.*)
(**)
(*ApplyMomentumConservation[verts] tries out every way to apply MomentumReplace, and keeps the shortest result for each vertex.*)


(* ::Subsection:: *)
(*MomentumReplace*)


MomentumReplace[{parts_, vert_}, n_] := Block[{momentumrules, nparts = Length[parts]},
     momentumrules = Which[n == 1, {FV[1, mu_] :> -Sum[FV[$kk, mu], {$kk, 2, nparts}], 
                                    SP[1, j_] :> -Sum[SP[$kk, j], {$kk, 2, nparts}], 
                                    SP[j_, 1] :> -Sum[SP[$kk, j], {$kk, 2, nparts}],
                                    SlashedP[1,inds___] :> -Sum[SlashedP[$kk,inds], {$kk, 2, nparts}]},
                           n == nparts, {FV[nparts, mu_] :> -Sum[FV[$kk, mu], {$kk, 1, nparts-1}], 
                                    SP[nparts, j_] :> -Sum[SP[$kk, j], {$kk, 1, nparts-1}], 
                                    SP[j_, nparts] :> -Sum[SP[$kk, j], {$kk, 1, nparts-1}],
                                    SlashedP[nparts,inds___] :> -Sum[SlashedP[$kk,inds], {$kk, 1, nparts-1}]},
                           1 < n < nparts, {FV[n, mu_] :> -Sum[FV[$kk, mu], {$kk, 1, n-1}]-Sum[FV[$kk, mu], {$kk, n+1, nparts}], 
                                    SP[n, j_] :> -Sum[SP[$kk, j], {$kk, 1, n-1}]-Sum[SP[$kk, j], {$kk, n+1, nparts}], 
                                    SP[j_, n] :> -Sum[SP[$kk, j], {$kk, 1, n-1}]-Sum[SP[$kk, j], {$kk, n+1, nparts}],
                                    SlashedP[n,inds___] :> -Sum[SlashedP[$kk,inds], {$kk, 1, n-1}] -Sum[SlashedP[$kk,inds], {$kk, n+1, nparts}]}];
     {parts, Factor[Expand[vert //. momentumrules]]}];
                          


(* ::Subsection:: *)
(*ApplyMomentumConservation*)


ApplyMomentumConservationSimplify[vert_] := Block[{tempvert = Expand[vert,_?(Not[FreeQ[#,FV]]||Not[FreeQ[#,SP]]||Not[FreeQ[#,SlashedP]]&)],templength,curvert},
   templength=Length[tempvert[[2]]];
   Do[curvert=Expand[MomentumReplace[tempvert,$kk],_?(Not[FreeQ[#,FV]]||Not[FreeQ[#,SP]]||Not[FreeQ[#,SlashedP]]&)]; (*pattern added by celine*)
      If[Length[curvert[[2]]]<templength,tempvert=curvert;templength=Length[tempvert[[2]]]],
      {$kk,1,Length[vert[[1]]]}];
   tempvert];

ApplyMomentumConservation[verts_] := If[MatrixQ[verts[[1]]], ApplyMomentumConservationSimplify[verts],ApplyMomentumConservationSimplify/@verts]


(* ::Section:: *)
(*CheckHermiticity*)


(* ::Text:: *)
(*CheckHermiticity[L] checks if the Lagrangian L is hermitian.*)
(**)
(*CheckHermiticity has the same options than FeynmanRules.*)


(* ::Subsection:: *)
(*CheckHermiticity*)


CheckHermiticity[lagrangian_, options___] := Block[{tempoptions, results, llag, hcllag},
     tempoptions = DeleteCases[{options}, Rule[Name, _] || Rule[ScreenOutput,_]];
     Print["Checking for hermiticity by calculating the Feynman rules contained in L-HC[L]."];
     Print["If the lagrangian is hermitian, then the number of vertices should be zero."];
     (* Improvements added by Benj *)
     llag = OptimizeIndex[ExpandIndices[lagrangian]]/.{Eps[args1__] :> Signature[{args1} ] Eps[Sequence @@ Sort[{args1}]]};  (* CD: Added ExpandIndices *)
     hcllag = OptimizeIndex[ExpandIndices[HC[lagrangian]]]/.{Eps[args1__] :> Signature[{args1} ] Eps[Sequence @@ Sort[{args1}]]};

     (* End of improvements added by Benj *)
     results = Simplify[FeynmanRules[ llag - hcllag, options, ScreenOutput -> False]];
     If[results === {}, Print["The lagrangian is hermitian."],
        Print["The lagrangian appears not to be hermitian."];
        Print["Non vanishing terms during the Feynman rule calculation for L - HC[L]:"];
        Do[Print[results[[$kk]]], {$kk, Length[results]}]];
     results];

     


(* ::Section:: *)
(*CheckKineticTermNormlisation*)


(* ::Text:: *)
(*CheckKineticTermNormalisation[L] checks whether all kinetic terms in the lagrangian L are correctly nomalised.*)


(* ::Subsection:: *)
(*CheckKineticTermNormalisation*)


CheckKineticTermNormalisation[lagrangian_, options___]:=Block[{lag,isdiag,fcl,fcl1,tmp={},tmpterm,ck,cktot=False,FR$mu,FR$nu,tmp1,tmp2,numtmp},
  (* Initialization *)
   FR$Message={True,True,True,True,True,True}; 
   isdiag=CheckDiagonalKineticTerms[lagrangian,options];
   If[isdiag,
      cktot = True;
      lag=Expand[GetKineticTerms[lagrangian,options]];
      lag=If[Head[lag]===Plus,List@@lag,{lag}];
      fcl=Sort/@(GetFieldContent/@lag);
      fcl1=KillDoubles[fcl];
      (* As we know already that the kinetic terms are diagonal, we removed non diagonal remnants (e.g. proportinal to cw^2 + sw^2 -1 *)
      fcl1 = Select[fcl1, #[[1]] === anti[#[[2]]]&];
      Do[tmp=Append[tmp,Plus@@((lag[[#]]&)@@@Position[fcl,fcl1[[nfcl]]])],{nfcl,Length[fcl1]}];
      fcl=fcl1/._?GhostFieldQ->"U"/.{_?ScalarFieldQ->"S",_?VectorFieldQ->"V",_?DiracFieldQ->"F",_?MajoranaFieldQ->"F",_?Spin2FieldQ->"T"};
      (* Loop over all terms *)
      Do[Which[fcl[[$nn]] === {"S","S"},
         If[Not[FreeQ[tmp[[$nn]],del[del[_,_],_]]],
            tmpterm=tmp[[$nn]]/.del[del[_,_],_]->1/._?FieldQ->1;
               numtmp = NumericalValue[tmpterm];
               If[Not[NumericQ[numtmp]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to check normalisation."]];
               ck=If[fcl1[[$nn,1]]===fcl1[[$nn,2]],numtmp == -0.5, numtmp == -1.],
            tmpterm=tmp[[$nn]]/.del[_,_]->1/._?FieldQ->1;
               numtmp = NumericalValue[tmpterm];
               If[Not[NumericQ[numtmp]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to check normalisation."]];
               ck=If[fcl1[[$nn,1]]===fcl1[[$nn,2]],numtmp == 0.5, numtmp == 1.]];
      If[ck=!=True, Print["Warning: Kinetic term for ",ToString[fcl1[[$nn]]]," seems not to be correctly normalized."]];
      cktot=cktot&&ck,
      fcl[[$nn]] === {"F","F"},
      tmpterm=tmp[[$nn]]/.Dot[del[psi1_,mu_],psi2_]:>-Dot[psi1,del[psi2,mu]]//.Dot[psi1_[aa___,b_,c___],del[psi2_[d___,e_,f___],g_]]IndexDelta[b_,e_]->Dot[psi1[aa,b,c],del[psi2[d,b,f],g]]/.del[__]->1/._?FieldQ->1/.Dot[1,1]->1//. Ga[__] -> 1 //. (ProjP|ProjM)[_,_] -> 1/2 //. TensDot[___,(ProjP|ProjM)][__]->1/2;
      numtmp = NumericalValue[tmpterm];
      If[Not[NumericQ[numtmp]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to check normalisation."]];
      ck=If[MajoranaFieldQ[fcl1[[$nn,1]]], numtmp == 0.+0.5 I, numtmp == 0.+1. I];
      If[ck=!=True, Print["Warning: Kinetic term for ",ToString[fcl1[[$nn]]]," seems not to be correctly normalized."]];
      cktot=cktot&&ck,
      fcl[[$nn]] === {"U","U"},
      tmpterm=tmp[[$nn]]/.Dot[del[psi1_,mu_],psi2_]:>-Dot[psi1,del[psi2,mu]]/.Dot[psi1_?(Not[AntiFieldQ[#]]&),del[del[psi2_?AntiFieldQ,mu1_]mu2_]]:>-Dot[psi2,del[del[psi,mu1]mu2]]/.del[__]->1/._?FieldQ->1/.Dot[1,1]->1;
      numtmp = NumericalValue[tmpterm];
      If[Not[NumericQ[numtmp]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to check normalisation."]];
      ck=(numtmp == -1.);
      If[ck=!=True, Print["Warning: Kinetic term for ",ToString[fcl1[[$nn]]]," seems not to be correctly normalized."]];
      cktot=cktot&&ck,
      fcl[[$nn]] === {"V","V"},
      tmpterm=tmp[[$nn]]//.del[a1_[mu_,ind___],nu_]del[a2_[nu_,ind___],mu_]:>-a1[FR$mu]del[del[a2[FR$nu],FR$nu],FR$mu]//.del[a1_[mu_,ind___],mu_]del[a2_[nu_,ind___],nu_]:>-a1[FR$mu]del[del[a2[FR$nu],FR$nu],FR$mu]//.a1_[mu_,ind___]del[del[a2_[nu_,ind___],mu_],nu_]:>a1[FR$mu]del[del[a2[FR$nu],FR$nu],FR$mu]//.a1_[mu_,ind___]del[del[a2_[nu_,ind___],nu_],mu_]:>a1[FR$mu]del[del[a2[FR$nu],FR$nu],FR$mu]//.del[a1_[mu_,ind___],nu_]del[a2_[mu_,ind___],nu_]:>-a1[FR$mu]del[del[a2[FR$mu],FR$nu,FR$nu]]//.del[aa_[mu_,ind___],nu_]^2:>-aa[FR$mu]del[del[aa[FR$mu],FR$nu,FR$nu]]//.a1_[mu_,ind___]del[del[a2_[mu_,ind___],nu_,nu_]]:>a1[FR$mu]del[del[a2[FR$mu],FR$nu],FR$nu];
      tmpterm=tmpterm//.a1_[FR$mu]del[del[a2_?(AntiFieldQ)[FR$mu],FR$nu],FR$nu]:>a2[FR$mu]del[del[a1[FR$mu],FR$nu],FR$nu]//.a1_[FR$mu]del[del[a2_?(AntiFieldQ)[FR$nu],FR$nu],FR$mu]:>a2[FR$mu]del[del[a1[FR$nu],FR$nu],FR$mu];
      tmp1=tmpterm/.a1_[FR$mu]del[del[a2_[FR$mu],FR$nu],FR$nu]->0/.a1_[FR$mu]del[del[a2_[FR$nu],FR$nu],FR$mu]->1;
      tmp2=tmpterm/.a1_[FR$mu]del[del[a2_[FR$mu],FR$nu],FR$nu]->1/.a1_[FR$mu]del[del[a2_[FR$nu],FR$nu],FR$mu]->0;
      If[Not[NumericQ[NumericalValue[tmp1]] && NumericQ[NumericalValue[tmp2]]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to check normalisation."]];
      ck=If[fcl1[[$nn,1]]===fcl1[[$nn,2]],(NumericalValue[tmp1] == -0.5)&&(NumericalValue[tmp2] == 0.5),(NumericalValue[tmp1] == -1.)&&(NumericalValue[tmp2] == 1.)];
      If[ck=!=True, Print["Warning: Kinetic term for ",ToString[fcl1[[$nn]]]," seems not to be correctly normalized."]];
      cktot=cktot&&ck],{$nn,Length[fcl]}];
      If[cktot,Print["All kinetic terms are correctly normalized."];True]]]


(* ::Section:: *)
(*GetMassSpectrum*)


(* ::Text:: *)
(*GetSprectrum[L] calculates the mass spectrum of the lagrangian L. Notice that in order to calculate the mass spectrum, the mass eigenstates must be known, i.e. the relations to the mass eigenstates via mixing matrice must be given in the model file.*)


(* ::Subsection:: *)
(*GetMassSpectrum*)


GetMassSpectrum[lagrangian_, options___]:=Block[{lag,isdiag,fcl,fcl1,tmp={},tmpterm,numtmp,mval,mnumval,parttmp,res={}, opt},
   opt=Append[DeleteCases[{options},FlavorExpand->_],FlavorExpand->True];
   isdiag=CheckDiagonalMassTerms[lagrangian,Sequence@@opt];
   If[isdiag === True,
      lag=Expand[GetMassTerms[lagrangian,Sequence@@opt]];
      lag=If[Head[lag]===Plus,List@@lag,{lag}];
      fcl=Sort/@(GetFieldContent/@lag);
      fcl1=KillDoubles[fcl];
      (* As we know already that the kinetic terms are diagonal, we removed non diagonal remnants (e.g. proportinal to cw^2 + sw^2 -1 *)
      fcl1 = Select[fcl1, #[[1]] === anti[#[[2]]]&];
      Do[tmp=Append[tmp,Plus@@((lag[[#]]&)@@@Position[fcl,fcl1[[nfcl]]])],{nfcl,Length[fcl1]}];
      fcl=fcl1/._?GhostFieldQ->"U"/.{_?ScalarFieldQ->"S",_?VectorFieldQ->"V",_?DiracFieldQ->"F",_?MajoranaFieldQ->"F",_?Spin2FieldQ->"T"};
      (* Loop over all terms *)
      Do[Which[fcl[[$nn]] === {"S","S"},
            tmpterm=tmp[[$nn]]/.{FR$deltat[xx_]->FR$deltat[xx],_?FieldQ->1};
               numtmp = NumericalValue[tmpterm];
               If[Not[NumericQ[numtmp]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to get numerical value."]];
               If[(Re[numtmp]>0)&&(Im[numtmp]<N[10^(-12)]), Print["Warning: mass term for ", ToString[fcl1[[$nn]]], "seems not to have the correct sign."]];
               If[fcl1[[$nn,1]]===fcl1[[$nn,2]],
                  mval=Sqrt[-2tmpterm];
                    mnumval=NumericalValue[mval];
                    If[Re[mnumval]^2<N[10^(-10)],mnumval=0.],
                  mval=Sqrt[-tmpterm];
                    mnumval=NumericalValue[mval];
                    If[Re[mnumval]^2<N[10^(-10)],mnumval=0.]];
               parttmp=If[AntiFieldQ[fcl1[[$nn,1]]], anti[fcl1[[$nn,1]]], fcl1[[$nn,1]]];
               res=Append[res,{parttmp,mval,mnumval}],
         fcl[[$nn]] === {"F","F"},
               tmpterm=tmp[[$nn]]/.{FR$deltat[xx_]->FR$deltat[xx],_?FieldQ->1}/.Dot[1,1]->1/.(ProjP|ProjM)[__]->1/2;
               numtmp = NumericalValue[tmpterm];
               If[Not[NumericQ[numtmp]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to get numerical value."]];
               If[(Re[numtmp]>0)&&(Im[numtmp]<N[10^(-12)]), Print["Warning: mass term for ", ToString[fcl1[[$nn]]], "seems not to have the correct sign."]];
               If[MajoranaFieldQ[fcl1[[$nn,1]]],
                  mval=-2tmpterm;
                      mnumval=NumericalValue[mval],
                  mval =-tmpterm;
                      mnumval=NumericalValue[mval]];
               parttmp=If[AntiFieldQ[fcl1[[$nn,1]]], anti[fcl1[[$nn,1]]], fcl1[[$nn,1]]];
               If[Re[mnumval]<N[10^(-10)],mnumval=0.];
               res=Append[res,{parttmp,mval,mnumval}],
         fcl[[$nn]] === {"U","U"},
               tmpterm=tmp[[$nn]]/.{FR$deltat[xx_]->FR$deltat[xx],_?FieldQ->1}/.Dot[1,1]->1;
               numtmp = NumericalValue[tmpterm];
               If[Not[NumericQ[numtmp]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to get numerical value."]];
               If[(Re[numtmp]>0)&&(Im[numtmp]<N[10^(-12)]), Print["Warning: mass term for ", ToString[fcl1[[$nn]]], "seems not to have the correct sign."]];
               mval=Sqrt[-tmpterm];
               mnumval=NumericalValue[mval];
               If[Re[mnumval]^2<N[10^(-10)],mnumval=0.];
               parttmp=If[AntiFieldQ[fcl1[[$nn,1]]], anti[fcl1[[$nn,1]]], fcl1[[$nn,1]]];
               res=Append[res,{parttmp,mval,mnumval}],
         fcl[[$nn]] === {"V","V"},
               tmpterm=tmp[[$nn]]/.{FR$deltat[xx_]->FR$deltat[xx],_?FieldQ->1};
               numtmp = NumericalValue[tmpterm];
               If[Not[NumericQ[numtmp]], Print["Warning: non numerical value encountered for ", ToString[fcl1[[$nn]]], ". Unable to get numerical value."]];
               If[(Re[numtmp]<0)&&(Im[numtmp]<N[10^(-12)]), Print["Warning: mass term for ", ToString[fcl1[[$nn]]], "seems not to have the correct sign."]];
               If[fcl1[[$nn,1]]===fcl1[[$nn,2]],mval=Sqrt[2tmpterm];
               mnumval=NumericalValue[mval],
               mval=Sqrt[tmpterm];
               mnumval=NumericalValue[mval]];
               If[Re[mnumval]^2<N[10^(-10)],mnumval=0.];
               parttmp=If[AntiFieldQ[fcl1[[$nn,1]]], anti[fcl1[[$nn,1]]], fcl1[[$nn,1]]];
               res=Append[res,{parttmp,mval,mnumval}]],{$nn,Length[fcl1]}];
               res=({#1,#2,If[Im[#3]<N[10^(-13)]Re[#3],Re[#3],#3]}&)@@@res;
               res=DeleteCases[res,{_,_,0.}];
               res=Prepend[res,{"Particle", "Analytical value", "Numerical value"}];
      TableForm[res]]]


(* ::Section:: *)
(*CheckMassSpectrum*)


(* ::Text:: *)
(*CheckMassSpectrum[L] checks whether the numerical mass values given in the model - file agree within 0.1 % with those calculated directly form the lagrangian using GetMassSpectrum.*)


(* ::Subsection:: *)
(*CheckMassSpectrum*)


CheckMassSpectrum[lagrangian_,options___]:=Block[{spectre,speccheck,tmp},
    FR$Message={True,True,True,True,True,True};
    tmp=GetMassSpectrum[lagrangian,options];
    If[tmp =!= Null,spectre=Rest[Identity@@tmp];
    Print["Getting mass spectrum."];
    Print["Checking for less then 0.1% agreement with model file values."];
    spectre=({#1,#2,#3,NumericalValue[Mass[#1]]}&)@@@spectre;
    spectre=({#1,#2,#3,If[Im[#4]<N[10^(-13)]Re[#4],Re[#4],#4]}&)@@@spectre;
    speccheck=Table[If[(spectre[[$nn,3]]>N[spectre[[$nn,4]]*0.999])&&(spectre[[$nn,3]]<N[spectre[[$nn,4]]*1.001]),"OK",StyleForm["Discrepency!",FontColor->RGBColor[1,0,0]]],{$nn,Length[spectre]}];
    spectre=Table[Append[spectre[[$nn]],speccheck[[$nn]]],{$nn,Length[spectre]}]//.{ind__,"OK"}->{ind};
    spectre=Prepend[spectre,{"Particle", "Analytic value","Numerical value", "Model-file value"}];
    TableForm[spectre]]];



(* ::Section:: *)
(*Vertices*)


(* ::Text:: *)
(*Vertices[Verts, options] selects subsets from Verts specified in options. *)
(**)
(*Available options:*)
(*      MinParticles, MaxParticles*)
(*      SelectParticles*)
(*      Contains, Free*)


(* ::Subsection:: *)
(*SelectVertices*)


Options[SelectVertices] = {MinParticles -> Automatic, MaxParticles -> Automatic, SelectParticles -> Automatic, Contains -> Automatic, Free -> Automatic};

SelectVerts[verts_, selection_] := Block[{sel = selection, out = {}, fclist},
     If[VectorQ[sel], sel = {sel}];
     If[Not[FreeQ[sel, _?MajoranaFieldQ]], sel = KillDoubles[Join[sel, sel /. {maj_?MajoranaFieldQ :> HC[maj]}]]];
     sel = KillDoubles[Join[sel, sel /. {$ff_?DiracFieldQ :> CC[anti[$ff]]}]];
     sel =  Sort /@ sel;
     fclist = (Sort[DeleteCases[Flatten[#], _?NumericQ]]& /@ verts[[All,1]]);
     Do[If[And @@ ((Not[MemberQ[fclist, #]] & ) /@sel), Print["Vertex ", sel[[$kk]], " not found."], If[MemberQ[fclist,sel[[$kk]]],out = Append[out, (verts[[#]]&) @@ Flatten[Position[fclist, sel[[$kk]]]]]]], 
        {$kk, Length[sel]}];
     out];

SelectVertices[verts_List, options___] := Block[{minpart, maxpart, selpart, tempvert = verts, conpart, freepart},
    minpart = MinParticles /. {options} /. Options[SelectVertices];
    maxpart = MaxParticles /. {options} /. Options[SelectVertices];
    selpart = SelectParticles /. {options} /. Options[SelectVertices];
    conpart = Contains /. {options} /. Options[SelectVertices];
    freepart = Free /. {options} /. Options[SelectVertices];
    If[KillDoubles[{minpart, maxpart, selpart, conpart, freepart}] =!= {Automatic}, 
       Print["Applying seclection rules..."];
        If[minpart =!= Automatic, tempvert = Select[tempvert, Length[#[[1]]] >= minpart &]];
        If[maxpart =!= Automatic, tempvert = Select[tempvert, Length[#[[1]]] <= maxpart &]];   
        If[selpart =!= Automatic, tempvert = SelectVerts[tempvert, selpart]];
        If[conpart =!= Automatic, 
           conpart = Flatten[{conpart}];
           Do[Which[DiracFieldQ[conpart[[kpart]]] === True, tempvert = Select[tempvert, Not[FreeQ[#, conpart[[kpart]] | CC[HC[conpart[[kpart]]]]]]&],
                    MajoranaFieldQ[conpart[[kpart]]] === True, tempvert = Select[tempvert, Not[FreeQ[#, conpart[[kpart]] | HC[conpart[[kpart]]]]]&],
                    True, tempvert = Select[tempvert, Not[FreeQ[#, conpart[[kpart]]]]&]], 
              {kpart, Length[conpart]}]];
        If[Free =!= Automatic, 
           freepart = Flatten[{freepart}];
           freepart = Union[freepart, freepart /. {$ff_?MajoranaFieldQ :> HC[$ff]}, freepart /. {$ff_?DiracFieldQ :> CC[HC[$ff]]}];
           Do[tempvert = Select[tempvert, FreeQ[#, freepart[[kpart]]]&], {kpart, Length[freepart]}]],
       Print["No selection rules defined."]];
    tempvert]
     
      


(* ::Section:: *)
(*BuildPDGs[]*)


(* ::Text:: *)
(*Creates three functions,*)
(**)
(*      PartPDG*)
(*      PDGtoPart*)
(*      PDGtoSymb*)
(*      *)
(* which relate the particle names and symbols to the PDG codes of the particles, and vice-versa.*)


(* ::Subsection:: *)
(*BuildPDGs[]*)


BuildPDGs[]:=Block[{plist,aplist},
    plist=Join@@PartList[[All,2]];
    aplist=Select[plist,#[[1]]=!=#[[2]]&];
    aplist=Transpose[{aplist[[All,2]],-aplist[[All,9]]}];
    plist=Transpose[{plist[[All,1]],plist[[All,9]]}];
    plist = plist /. NoPDG -> Identity;
    aplist = aplist /. NoPDG -> Identity;
    Clear[PartPDG,PDGtoPart,PDGtoSymb];

    Do[PartPDG[plist[[j,1]]]=plist[[j,2]];
       PDGtoPart[plist[[j,2]]]=plist[[j,1]],{j,Length[plist]}];

    Do[PartPDG[aplist[[j,1]]]=aplist[[j,2]];
       PDGtoPart[aplist[[j,2]]]=aplist[[j,1]],{j,Length[aplist]}];

    PartPDG[s_Symbol]:=PartPDG[PartName[s]];
    PDGtoSymb[n_]:=PartSymbol[PDGtoPart[n]]
];


(* ::Section:: *)
(*Generate 2->2 Processes*)


(* ::Text:: *)
(*Generate2to2Processes[options] generates a list of all possible processes that conserves the quantum numbers (both abelian and nonabelian).  It only generates processes that can not be rotated into one another.  The user can further specify other constraints through the following options:*)
(*1) SelectFieldTypes can be used to set the field type of each particle.  For example, SelectFieldTypes->{F,F,S,V} means that 2 particles are fermionic, 1 is scalar and 1 is vector.  SlectFieldTypes->{F,F,!V,!V} means that 2 particles are fermionic and 2 are not vectors. *)
(*2) SelectIndices can be used to set the field type of each particle.  For example, SelectIndices->{Colour,Colour,!Colour,!Colour} means that 2 partices must have index Colour and the other 2 must not.  It is determined index by index.  Indices can't be corrolated.  Any indices defined in the model can be used.*)
(*3) SelectCharges can be used to set whether each particle is charged.  For example, SelectCharges->{Q,Q,!Q,!Q,LeptonNumber,LeptonNumber} means that 2 particles have charge Q and the other 2 have Q=0.  Furthermore, 2 of the particles must be leptons.  The charges can't be correlated.  Any charge defined in the model can be used.*)


(* ::Subsubsection:: *)
(*Generate2to2Processes*)


SelectFieldTypes::usage="Testing";
SelectIndices::usage="Testing";
SelectCharges::usage="Testing";


Options[Generarte2to2Processes]={SelectFieldTypes->{},SelectIndices->{},SelectCharges->{}};

Generate2to2Processes[opts___]:=Module[{isOk,partsTmp,i,j,k,l,i1,j1,k1,l1,i2,j2,k2,l2,N=0,fldTp,fldTpN={},indcs,indcsN={},tindL={},chrgs,chrgsN={},tmp1,tmp2,tmp3,procList={}},
fldTp=SelectFieldTypes/.{opts}/.Options[Generate2to2Processes];
If[Length[fldTp]>0,
	fldTpN=Select[fldTp,Head[#]==Not&]/.Not[aa_]->aa;
	fldTp=Select[fldTp,Head[#]=!=Not&];
];
indcs=SelectIndices/.{opts}/.Options[Generate2to2Processes];
If[Length[indcs]>0,
	indcsN=Flatten/@Index/@(Select[indcs,Head[#]==Not&]/.Not[aa_]->aa);
	indcs=Flatten/@Index/@(Select[indcs,Head[#]=!=Not&]);
	tindL=DeleteDuplicates[Flatten[{indcs,indcsN}]];
];
chrgs=SelectCharges/.{opts}/.Options[Generarte2to2Processes];
If[Length[chrgs]>0,
	chrgsN=Select[chrgs,Head[#]==Not&]/.Not[aa_]->aa;
	chrgs=Select[chrgs,Head[#]=!=Not&];
];
partsTmp=Flatten[PartList[[All,2]],1];
(*Print[fldTp,"\t",indcs,"\t",chrgs];
Print[fldTpN,"\t",indcsN,"\t",chrgsN];*)

(*Loop through available particles.*)
Do[
	isOk=True;

	(*******************Exclude Goldstones.*********************)
	If[isOk&&partsTmp[[i,13]]===NoGS&&partsTmp[[j,13]]===NoGS&&partsTmp[[k,13]]===NoGS&&partsTmp[[l,13]]===NoGS,isOk=True,isOk=False];

	(*******************Exclude Ghosts.**************************)
	If[isOk&&partsTmp[[i,3]]=!=U&&partsTmp[[j,3]]=!=U&&partsTmp[[k,3]]=!=U&&partsTmp[[l,3]]=!=U,isOk=True,isOk=False];

	(*******************Check Field Type*************************)
	If[isOk,
		(*Find the number of each field type*)
		tmp1=(Length[#] &/@(Cases[{partsTmp[[i,3]],partsTmp[[j,3]],partsTmp[[k,3]],partsTmp[[l,3]]},#]&/@{S,F,V,T}));
		(*Find the number selected*)
		tmp2=(Length[#] &/@(Cases[fldTp,#]&/@{S,F,V,T}));
		(*Remove ith entry from tmp1-tmp2, sum and subtract the number of !field type selected.*)
		tmp3=(Drop[tmp1-tmp2,{#}]/.List->Plus&/@{1,2,3,4})-(Length[#] &/@(Cases[fldTpN,#]&/@{S,F,V,T}));
		(*Print[tmp1-tmp2,"\t",tmp3];*)
		If[Length[fldTp]>0&&!(NonNegative[tmp1-tmp2]/.List->And),isOk=False];
		If[Length[fldTpN]>0&&!(NonNegative[tmp3]/.List->And),isOk=False];
	];

	(*******************Check indices****************************)
	If[isOk,
		(*Find the number of each index minus the number selected.*)
		tmp1=(Count[DeleteDuplicates/@$IndList/@{PartSymbol[partsTmp[[i,1]]],PartSymbol[partsTmp[[j,1]]],PartSymbol[partsTmp[[k,1]]],PartSymbol[partsTmp[[l,1]]]},#,2]&/@tindL);
		(*Find the number of each index selected.*)
		tmp2=(Count[indcs,#]&/@tindL);
		(*Remove ith entry from tmp1, sum and subtract the number of !index selected.*)
		tmp3=tmp1+(Count[indcsN,#]&/@tindL)-Table[4,{i3,Length[tindL]}];
		(*Print[tmp1-tmp2,"\t",tmp3];*)
		If[Length[indcs]>0&&!(NonNegative[tmp1-tmp2]/.List->And),isOk=False];
		If[Length[indcsN]>0&&!(NonPositive[tmp3]/.List->And),isOk=False];
	];

	(*******************Look for a neutral combination.***********)
	If[isOk,
		isOk=False;
		Do[
			If[!isOk&&NeutralCombinationQ[{PartSymbol[partsTmp[[i,i1]]],PartSymbol[partsTmp[[j,j1]]],anti[PartSymbol[partsTmp[[k,k1]]]],anti[PartSymbol[partsTmp[[l,l1]]]]}],
				(*************Check Charges************************)
				(*Number of particles for each charge.*)
				tmp1=Count[Positive[Abs[#]],True]&/@((#/@{PartSymbol[partsTmp[[i,i1]]],PartSymbol[partsTmp[[j,j1]]],anti[PartSymbol[partsTmp[[k,k1]]]],anti[PartSymbol[partsTmp[[l,l1]]]]})& /@ MR$QuantumNumbers);
				(*Number of particles requested for each charge.*)
				tmp2=Count[chrgs,#]&/@MR$QuantumNumbers;
				(*Number of particles for each charge plus the number requested not to be present minus 4.*)
				tmp3=tmp1+(Count[chrgs,#]&/@MR$QuantumNumbers)-Table[4,{i3,Length[MR$QuantumNumbers]}];
				(*Print[tmp1-tmp2,"\t",tmp3];*)
				If[(NonNegative[tmp1-tmp2]/.List->And)&&(NonPositive[tmp3]/.List->And),isOk=True;i2=i1;j2=j1;k2=k1;l2=l1;];
			];
		,{i1,1,2},{j1,1,2},{k1,1,2},{l1,1,2}];
	];

	(*******************If Ok, add to list*************************)
	If[isOk,
		AppendTo[procList,{PartSymbol[partsTmp[[i,i2]]],PartSymbol[partsTmp[[j,j2]]],PartSymbol[partsTmp[[k,k2]]],PartSymbol[partsTmp[[l,l2]]]}];
		(*Print[partsTmp[[i,i2]],",",partsTmp[[j,j2]],"->",partsTmp[[k,k2]],",",partsTmp[[l,l2]]];*)
		N++;
	];

,{i,1,Length[partsTmp]},{j,i,Length[partsTmp]},{k,j,Length[partsTmp]},{l,k,Length[partsTmp]}];

Print[N," processes found."];
procList
];


(* ::Subsubsection:: *)
(*Generate1to2Decays*)


(* ::Text:: *)
(*Generate1to2Decays[options] generates a list of all possible decays that conserves the quantum numbers (both abelian and nonabelian).  It only generates decays where the decaying particle is more massive than the daughters.  The user can further specify other constraints through the following options:*)
(*1) SelectFieldTypes can be used to set the field type of each particle.  For example, SelectFieldTypes->{F,F,S} means that 2 particles are fermionic and 1 is scalar.  SlectFieldTypes->{F,F,!V,!V} means that 2 particles are fermionic and 2 are not vectors. *)
(*2) SelectIndices can be used to set the field type of each particle.  For example, SelectIndices->{Colour,Colour,!Colour} means that 2 partices must have index Colour and the other 1 must not.  It is determined index by index.  Indices can't be corrolated.  Any indices defined in the model can be used.*)
(*3) SelectCharges can be used to set whether each particle is charged.  For example, SelectCharges->{Q,Q,!Q,LeptonNumber,LeptonNumber} means that 2 particles have charge Q and the other 1 have Q=0.  Furthermore, 2 of the particles must be leptons.  The charges can't be correlated.  Any charge defined in the model can be used.*)


SelectFieldTypes::usage="Testing";
SelectIndices::usage="Testing";
SelectCharges::usage="Testing";


Options[Generate1to2Decays]={SelectFieldTypes->{},SelectIndices->{},SelectCharges->{}};

Generate1to2Decays[opts___]:=Module[{isOk,partsTmp,i,j,k,i1,j1,k1,i2,j2,k2,N=0,fldTp,fldTpN={},indcs,indcsN={},tindL={},chrgs,chrgsN={},tmp1,tmp2,tmp3,procList={}},
fldTp=SelectFieldTypes/.{opts}/.Options[Generate1to2Decays];
If[Length[fldTp]>0,
	fldTpN=Select[fldTp,Head[#]==Not&]/.Not[aa_]->aa;
	fldTp=Select[fldTp,Head[#]=!=Not&];
];
indcs=SelectIndices/.{opts}/.Options[Generate1to2Decays];
If[Length[indcs]>0,
	indcsN=Flatten/@Index/@(Select[indcs,Head[#]==Not&]/.Not[aa_]->aa);
	indcs=Flatten/@Index/@(Select[indcs,Head[#]=!=Not&]);
	tindL=DeleteDuplicates[Flatten[{indcs,indcsN}]];
];
chrgs=SelectCharges/.{opts}/.Options[Generate1to2Decays];
If[Length[chrgs]>0,
	chrgsN=Select[chrgs,Head[#]==Not&]/.Not[aa_]->aa;
	chrgs=Select[chrgs,Head[#]=!=Not&];
];
partsTmp=Flatten[PartList[[All,2]],1];
(*Print[fldTp,"\t",indcs,"\t",chrgs];
Print[fldTpN,"\t",indcsN,"\t",chrgsN];*)

(*Loop through available particles.*)
Do[
	isOk=True;
         
	(*******************Exclude Goldstones.*********************)
	If[isOk&&partsTmp[[i,13]]===NoGS&&partsTmp[[j,13]]===NoGS&&partsTmp[[k,13]]===NoGS,isOk=True,isOk=False];

	(*******************Exclude Ghosts.**************************)
	If[isOk&&partsTmp[[i,3]]=!=U&&partsTmp[[j,3]]=!=U&&partsTmp[[k,3]]=!=U,isOk=True,isOk=False];
	
	(*******************Check Field Type*************************)
	If[isOk,
		(*Find the number of each field type*)
		tmp1=(Length[#] &/@(Cases[{partsTmp[[i,3]],partsTmp[[j,3]],partsTmp[[k,3]]},#]&/@{S,F,V,T}));
		(*Find the number selected*)
		tmp2=(Length[#] &/@(Cases[fldTp,#]&/@{S,F,V,T}));
		(*Remove ith entry from tmp1-tmp2, sum and subtract the number of !field type selected.*)
		tmp3=(Drop[tmp1-tmp2,{#}]/.List->Plus&/@{1,2,3,4})-(Length[#] &/@(Cases[fldTpN,#]&/@{S,F,V,T}));
		(*Print[tmp1,"\t-\t",tmp2,"\t=\t",tmp3];*)
		If[Length[fldTp]>0&&!(NonNegative[tmp1-tmp2]/.List->And),isOk=False];
		If[Length[fldTpN]>0&&!(NonNegative[tmp3]/.List->And),isOk=False];
	];
	
	(*******************Check indices****************************)
	If[isOk,
		(*Find the number of each index minus the number selected.*)
		tmp1=(Count[DeleteDuplicates/@$IndList/@{PartSymbol[partsTmp[[i,1]]],PartSymbol[partsTmp[[j,1]]],PartSymbol[partsTmp[[k,1]]]},#,2]&/@tindL);
		(*Find the number of each index selected.*)
		tmp2=(Count[indcs,#]&/@tindL);
		(*Remove ith entry from tmp1, sum and subtract the number of !index selected.*)
		tmp3=tmp1+(Count[indcsN,#]&/@tindL)-Table[3,{i3,Length[tindL]}];
		(*Print[tmp1-tmp2,"\t",tmp3];*)
		If[Length[indcs]>0&&!(NonNegative[tmp1-tmp2]/.List->And),isOk=False];
		If[Length[indcsN]>0&&!(NonPositive[tmp3]/.List->And),isOk=False];
	];

	(****************Check that phase space is available*******************)
If[isOk,
isOk=False;
If[NumericalValue[partsTmp[[i,5]]/.ZERO->0]>NumericalValue[partsTmp[[j,5]]/.ZERO->0]+NumericalValue[partsTmp[[k,5]]/.ZERO->0],isOk=True];
];

	(*******************Look for a neutral combination.***********)
	If[isOk,
		isOk=False;
		Do[(*Print["Testing : ",partsTmp[[i,i1]],"->",partsTmp[[j,j1]],",",partsTmp[[k,k1]]];*)
			If[!isOk&&NeutralCombinationQ[{PartSymbol[partsTmp[[i,i1]]],anti[PartSymbol[partsTmp[[j,j1]]]],anti[PartSymbol[partsTmp[[k,k1]]]]}],
				(*************Check Charges************************)
				(*Number of particles for each charge.*)						tmp1=Count[Positive[Abs[#]],True]&/@((#/@{PartSymbol[partsTmp[[i,i1]]],anti[PartSymbol[partsTmp[[j,j1]]]],anti[PartSymbol[partsTmp[[k,k1]]]]})& /@ MR$QuantumNumbers);
				(*Number of particles requested for each charge.*)
				tmp2=Count[chrgs,#]&/@MR$QuantumNumbers;
				(*Number of particles for each charge plus the number requested not to be present minus 3.*)
				tmp3=tmp1+(Count[chrgs,#]&/@MR$QuantumNumbers)-Table[3,{i3,Length[MR$QuantumNumbers]}];
				(*Print[tmp1-tmp2,"\t",tmp3];*)
				If[(NonNegative[tmp1-tmp2]/.List->And)&&(NonPositive[tmp3]/.List->And),isOk=True;i2=i1;j2=j1;k2=k1;];
			];
		,{i1,1,2},{j1,1,2},{k1,1,2}];
	];

(*******************If Ok, add to list*************************)
	If[isOk,
		AppendTo[procList,{PartSymbol[partsTmp[[i,i2]]],PartSymbol[partsTmp[[j,j2]]],PartSymbol[partsTmp[[k,k2]]]}];
		Print[partsTmp[[i,i2]],"->",partsTmp[[j,j2]],",",partsTmp[[k,k2]]];
		N++;
	];

,{i,1,Length[partsTmp]},{j,1,Length[partsTmp]},{k,j,Length[partsTmp]}];

Print[N," processes found."];
procList
];
