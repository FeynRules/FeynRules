(* ::Package:: *)

(* Split up color/Lorentzes *) 

(* Patterns to match for Lorentzes.  *)
PatternLorentz = {Ga -> L$Ga,
                  IndexDelta[Index[Spin,Ext[x_]], Index[Spin,Ext[y_]]]:> L$IndexDelta[Index[Spin,Ext[x]],Index[Spin,Ext[y]]],
                  ME -> L$ME,
                  Si ->L$Si, 
                  FV -> L$FV,
                  SlashedP -> L$SlashedP,
                  Sig -> L$Sig,
                  SP->L$SP,
                  ProjP -> L$ProjP,
                  ProjM->L$ProjM,
                  Eps[Index[Lorentz,aa__],Index[Lorentz,b__],Index[Lorentz,c__],Index[Lorentz,d__]]:> L$Eps[Index[Lorentz,aa],Index[Lorentz,b],Index[Lorentz,c],Index[Lorentz,d]]
};

(* Patterns for Color *)
PatternColor = {f[x_,y_,zz_] :> C$f[x,y,zz],
                f[x_,y_,zz_]f[aa_,b_,zz_]:> C$ff[x,y,aa,b],
                T -> C$T,
                IndexDelta[Index[Colour,Ext[x_]],Index[Colour,Ext[y_]]]:> C$IndexDelta[Index[Colour,Ext[x]],Index[Colour,Ext[y]]],
                IndexDelta[Index[Gluon,Ext[x_]],Index[Gluon,Ext[y_]]]:> C$IndexDelta[Index[Gluon,Ext[x]],Index[Gluon,Ext[y]]],
                IndexDelta[Index[Sextet,Ext[x_]],Index[Sextet,Ext[y_]]]:> C$IndexDelta[Index[Sextet,Ext[x]],Index[Sextet,Ext[y]]],
                dSUN -> C$dSUN,
                Eps[Index[Colour,aa__],Index[Colour,b__],Index[Colour,c__]]:> C$Eps[Index[Colour,aa],Index[Colour,b],Index[Colour,c]],
                T6 :> C$T6,
                K6 :> C$K6,
                K6bar :> C$K6bar
};

(* Left over index deltas *)
PatternElse = {IndexDelta-> P$IndexDelta, TensDot -> PRIVATE`PYTensDotOpen};

(* Patterns to match for colors, just standard model right now *)


(* Match Lorentzes *)
MatchLorentz[x_]:=MatchQ[StringTake[ToString[Head[x]]<>" ",{1,2}],"L$"]
GrabLorentz[input_] := Select[input,MatchLorentz[#]||(FormFactorQ[#] === True)||MatchQ[#,Power[x_?(MatchLorentz[#]&),n_]]&](*Power added by celine*)

(* Match Color *)
MatchColor[x_]:=MatchQ[StringTake[ToString[Head[x]]<>" ",{1,2}],"C$"]
GrabColor[input_] := Select[input,MatchColor[#]&]

(* Split into list,not list *) 
SplitLorentz[input_] := {GrabLorentz[input],Complement[input,GrabLorentz[input]]}
SplitColor[input_] := {GrabColor[input],Complement[input,GrabColor[input]]}
(* Replace Plus, Times with List and use rules above to facilitate splitting *) 
(* Added this routine to correct for the way Will coded this, which fails for phibar.phi^3 *)
PlusToList[expr_] := Block[{temp = Expand[expr]},
       temp = If[Head[temp]===Plus, List@@temp, temp];
       Return[temp];
     ];

MakeTimesList[expr_]:=If[Head[expr]===Times,
             List @@ expr, expr];
      

PutAddpadtoFront[list_] := If[list[[1]] =!= Addpad Multpad, Prepend[DeleteCases[list, Addpad Multpad], Addpad Multpad], list];
RoutineL[input_] := SplitLorentz /@ Flatten /@ (MakeTimesList /@ PutAddpadtoFront[PlusToList[Expand[input] //.PatternLorentz //. PatternColor //. PatternElse]])

(*RoutineC[input_] := SplitColor /@ (First/@ input/. Times -> List)*)
RoutineC[input_] := SplitColor /@ (MakeTimesList /@ (First/@ input))
(* Reassemble multiplication *)
assemble[input_] := Replace[input,List->Times,{3},Heads->True]

(* make a vertex list from feynrules *) 
PYVertexList[input_]  := First /@(Take[#,{2,2}]&/@ input);

(* Gather terms with similar coupling except for vertices with more than two fermions (exception added by celine)*)
gatherLikes[input_]:= If[Length[Union[Cases[input[[All,1]],Index[Spin,Ext[aa_]],\[Infinity]]]]<3,Gather[input,NumericQ[#1[[2]]/#2[[2]]]&],input]

(* Adjust Couplings, Lorentzes *)
Renorm[input_,input2_] := {input[[1]]*input[[2]]/input2,input2}

ShiftConstants[input_] := Module[{temp = First[input][[2]]},If[Length[input]>1,Renorm[#,temp]&/@ input,input]]

(* Reassemble addition except for vertices with more than two fermions (exception added by celine) *)
ReassemblePlus[input_] := Module[{TempL = Length[input],TempI = Replace[input,List->Plus,1,Heads->True]},If[TempL>1,{TempI[[1]],TempI[[2]]/TempL},First[input]]]
PutTogether[input_] := If[Length[Union[Cases[input[[All,1]],Index[Spin,Ext[aa_]],\[Infinity]]]]<3,Drop[ReassemblePlus /@ ShiftConstants /@ input ,1],Drop[ input ,1]]

(*Test if two lorentz structures are the same up to the name of the summed spin indices*)
SameLorentzQ[l1_,l2_]:=(
If[Length[l1]===Length[l2],
  MatchQ[l1,l2/.Index[Spin,x_?(FreeQ[#,Ext]&)]->Index[Spin,PatternX[x,_]]/.PatternX->Pattern],
  False])

(*Return the number of different lorentz structures*)
NumberOfLorentz[ls_]:=Length[Union[Flatten[
  ls[[All,All,1]]]//.{L$Ga[aa__,b_,c_]L$Ga[d__,c_,e_]->L$Ga[aa,d,b,e],
                       L$Ga[aa__,b_,c_]L$SlashedP[d_,c_,e_]->L$Ga[aa,L$Mom[d],b,e],
                       L$SlashedP[aa_,b_,c_]L$Ga[d__,c_,e_]->L$Ga[aa,L$Mom[d],b,e],
                       L$SlashedP[aa_,b_,c_]L$SlashedP[d_,c_,e_]->L$Ga[L$Mom[aa],L$Mom[d],b,e]}/.
   {L$Ga[aa__,b_,c_]L$ProjM[c_,e_]->L$GaM[aa,d,b,e],L$Ga[aa__,b_,c_]L$ProjP[c_,e_]->L$GaP[aa,d,b,e]},
  SameTest->SameLorentzQ]];

(* Structure list uses the above to make a list of just lorentzes,colors,couplings *) 
FullSplitL[input_] := Block[{FSLtemp,FSLgather},
FSLtemp = ((assemble/@RoutineL/@(Multpad*(PYVertexList[input]+Addpad)))/.Multpad->1);
FSLgather=PutTogether/@gatherLikes/@FSLtemp;
If[NumberOfLorentz[FSLgather]<NumberOfLorentz[FSLtemp],FSLgather,(Drop[#,1]&)/@FSLtemp]
];
FullSplitC[input_] := assemble/@RoutineC/@(Multpad*input)/.Multpad->1
(*StructureList[input_] := Map[Take[#,{1,1}]&,FullSplitL[input],{2}]
ElseList[input_] := Map[Take[#,{2,2}]&,FullSplitL[input],{2}]
ColorList[input_] := Map[Take[#,{1,1}]&,FullSplitC[ElseList[input]],{2}]
CoupleList[input_] := Map[Take[#,{2,2}]&,FullSplitC[ElseList[input]],{2}]*)





(* Rules to write the Python format for Helas -> Maybe some of these should be moved to python form? *) 
IndexReplace = Index[x_,Ext[y_Integer]]:> y;

HelasConvention = {L$TensDot[x_[aa___],y_[b___]][Index[Spin,Ext[p_Integer]],Index[Spin,Ext[q_Integer]]] :> x[aa,p,int$]y[b,int$,q],
L$TensDot[x_[aa___],y_][Index[Spin,Ext[p_Integer]],Index[Spin,Ext[q_Integer]]] :> x[aa,p,int$]y[int$,q],
L$SlashedP[x_Integer,Index[Spin,Ext[y_Integer]],Index[Spin,Ext[zz_Integer]]]:> L$Ga[a$,y,zz]L$FV[a$,x],
L$SP[x_,y_] :> L$FV[a$,x]L$FV[a$,y],L$FV[x_,y_]:> L$FV[y,x]};

HelasString  = {"L$ME"->"Metric","L$Ga"->"Gamma","L$FV"->"P","int$"->"'a'","L$ProjP"-> "ProjP","L$ProjM"->"ProjM","L$IndexDelta"->"Indentity"};

LorentzStrings[input_] := StringReplace[#,HelasString]& /@ ToString/@  Map[FortranForm,(StructureList[input] /. HelasConvention /.IndexReplace),{3}]


(* ::Section:: *)
(*PYOpenTensDot*)
