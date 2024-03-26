(* ::Package:: *)

(* ::Title:: *)
(*FeynArts Interface*)


(* ::Text:: *)
(*C. Degrande, C. Duhr, *)


(* ::Section::Closed:: *)
(*Messages*)


FAInt::Dirac = "Warning : Forcing no use of dirac indices while there are more than 2 fermions in some vertices";


(* ::Section::Closed:: *)
(*Useful functions and definitions*)


FA$FAOptions = {SelfConjugate, Indices, Mass, QuantumNumbers, MixingPartners, PropagatorLabel, PropagatorType, PropagatorArrow, MatrixTraceFactor, InsertOnly};

Name2Field[xx_]:=If[FreeQ[MR$ClassesDescription,ClassName->xx],xx/.FA$MemberToClass,Cases[MR$ClassesDescription,_?(Not[FreeQ[#,ClassName->xx]]&)][[1,1]]];

If[Not[ValueQ[FA$ClassesList]], FA$ClassesList = {}, FA$ClassesList];

If[Not[ValueQ[FA$ClassToName]], FA$ClassToName = {}, FA$ClassToName];

If[Not[ValueQ[FA$MemberToClass]], FA$MemberToClass = {}, FA$MemberToClass];

MergeFAClassToBlank[class_, bl_] := ToExpression[StringJoin[StringDrop[ToString[class, InputForm], -1], ", ", ToString[bl, InputForm], " ]"]];

MakeFABlankList[flavpos_, k_, class_] := Join[ Table[Blank[], {(ToExpression @@ FA$FlavPos[class]) - 1}], {k}, Table[Blank[], {NInd[class /. FA$ClassToName] - LorIndNumber[class /. FA$ClassToName] - (ToExpression @@ FA$FlavPos[class])}]];      

AddCommas[lisi_List] := Drop[Flatten[Table[{lisi[[kkll]], ", "}, {kkll, Length[lisi]}]], -1];

MatrixChain0[][xx___][inds___]:=MatrixChain0[xx][xx][inds];

MatrixChain1[tt___][uu1___,uu2_,uu3___][inds___]:=MatrixChain1[tt][uu1,uu3][inds]/;Not[MatchQ[uu2,_[___]]];

MatrixChain2[uu___][tt1___,tt_?(#=!=done&)[xxx___],tt2___][inds___]:=MatrixChain2[uu][tt1,done[xxx],tt2][inds];
MatrixChain2[uu1___,uu2_[___],uu3___][xx___][inds___]:=MatrixChain2[uu1,uu2,uu3][xx][inds];

MatrixChain2a[uu___][xx___][inds___]:=MatrixChain2a[First[{uu}]][xx][inds]/;(Length[{uu}]>1)&&(And@@((#===SUNT&)/@{uu}));

MatrixChain3[uu___][xxx___][inds___]:=MatrixChain3[uu][xxx,inds];
MatrixChain3[uu___][xx___,done[yy___],zz___]:=MatrixChain3[uu][xx,yy,zz];

MatrixChain4[uu___][inds___]:=Block[{temp},
   temp=Symbol[StringJoin@@(ToString/@{uu})];
   temp[inds]];

MakeMatrixChain[expr_] := expr /.  TensDot[tt1_,tt___][inds___] :> MatrixChain0[][tt1,tt][inds] /. MatrixChain0 -> MatrixChain1 /. MatrixChain1 -> MatrixChain2/. MatrixChain2 -> MatrixChain2a /. MatrixChain2a -> MatrixChain3 /. MatrixChain3 -> MatrixChain4;

SUNF /: SUNF[aa1___, cc_, aa2___]SUNF[bb1___,cc_,bb2___] := SUNF[aa2,aa1,bb2,bb1];

WriteParamList[ofile_String,lisi_List] := Block[{tmp},
          Do[WriteString[ofile,"     "<>ToString[lisi[[elem]], InputForm]<>",\n"],{elem,Length[lisi]-1}];
          WriteString[ofile,"     "<>ToString[Last[lisi], InputForm]<>"};"]]

(*No sign change in the FR because the symmetry is always -1 *)
FlippingRules={Rule[TensDot[dm1:_Ga|_SlashedP,dm2:_Ga|_SlashedP,Ppm:ProjP|ProjM][bb_,cc_],TensDot[dm2,dm1,Ppm][bb,cc]],
Rule[TensDot[dm:_Ga|_SlashedP,ProjP][bb_,cc_],TensDot[dm,ProjM][bb,cc]],Rule[TensDot[dm:_Ga|_SlashedP,ProjM][bb_,cc_],TensDot[dm,ProjP][bb,cc]]};
FlipRules={b___*a_[Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]->b*a[Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]],
b___*Ga[5,Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]->b*Ga[5,Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]],
b___*Ga[a_,Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]->-b*Ga[a,Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]],
b___*TensDot[a__,pj:ProjP|ProjM][Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]/;EvenQ[Length[List[a]]]->b*TensDot[Sequence@@Reverse[List[a]],pj][Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]],
b___*TensDot[a__,ProjP][Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]/;OddQ[Length[List[a]]]->-b*TensDot[Sequence@@Reverse[List[a]],ProjM][Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]],
b___*TensDot[a__,ProjM][Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]/;OddQ[Length[List[a]]]->-b*TensDot[Sequence@@Reverse[List[a]],ProjP][Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]],
b___*TensDot[a__,Ga[5]][Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]->b*TensDot[Sequence@@Reverse[List[a]],Ga[5]][Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]],
b___*TensDot[a__][Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]/;OddQ[Length[List[a]]]->-b*TensDot[Sequence@@Reverse[List[a]]][Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]],
b___*TensDot[a__][Index[Spin,Ext[ss1_]],Index[Spin,Ext[ss2_]]]/;EvenQ[Length[List[a]]]->b*TensDot[Sequence@@Reverse[List[a]]][Index[Spin,Ext[ss2]],Index[Spin,Ext[ss1]]]};


(* ::Subsection:: *)
(*LSymmetrize[particles,  structure]*)


(* ::Text:: *)
(*For all particles except fermions when there are at most 2 fermions in all the vertices*)


LSymmetrize[part_, struc_]:=Module[{prevpos,prevtype,newstruc,perm,permInt,permstruc,fermList,flip,nn,mm,ll,cc,SortSpin,fullstruc,Epstemp,kk,indfv,mom},

prevpos = 1;
prevtype = StringTake[part,{1}];
newstruc = struc/.Eps->Epstemp/.{SP[a_,b_]->SP[mom[a],mom[b]],SlashedP[a_]->SlashedP[mom[a]],FV[a_,b_]->FV[mom[a],b],FV[a_]->FV[mom[a]]};
Attributes[Epstemp]=Orderless;
fullstruc={};
SortSpin[x_]:=x/.Table[Cases[x,Index[Spin,Ext[xx_]],\[Infinity]][[kk]]->Sort[Cases[x,Index[Spin,Ext[xx_]],\[Infinity]]][[kk]],{kk,Length[Cases[x,Index[Spin,Ext[xx_]],\[Infinity]]]}];
Do[
	If[StringTake[part<>"Z",{kk}]!=prevtype,
		If[kk-prevpos>1,
			If[prevtype!="F",
				perm = Permutations[Range[prevpos,kk-1]];
                newstruc=newstruc/.{Ext[xx_,yy_]:>Ext[xx,"p"<>ToString[yy]]};
				Do[
					permstruc=ReplaceAll[newstruc,Join[Table[Rule[Ext[perm[[1,hh]]],Ext[perm[[ll,hh]]]],{hh,1,kk-prevpos}],
                                                       Table[Rule[mom[perm[[1,hh]]],mom[perm[[ll,hh]]]],{hh,1,kk-prevpos}]]];
					newstruc=Join[newstruc,Complement[permstruc,newstruc,SameTest->(MatchQ[#1,#2]&)]],
				{ll,2,Length[perm]}];
                 newstruc=newstruc/.{Ext[xx_,yy_]:>Ext[xx,ToExpression[StringTake[yy,-1]]]};
                 ,
				If[fermionCounter>2,Print["More than 2 fermions"];
					newstruc=Union[newstruc,SameTest->(MatchQ[SortSpin[#1],SortSpin[#2]]&)];
					For[ll=1,ll<=Length[newstruc],ll++,
						fermList = Cases[newstruc[[ll]],Index[Spin,Ext[bb_]]->bb,\[Infinity]];
						perm=Permutations[fermList];
						For[cc=1,cc<=Length[perm],cc++,
							permstruc=ReplaceAll[newstruc[[ll]],Join[Table[Rule[Ext[perm[[1,hh]]],Ext[perm[[cc,hh]]]],{hh,1,Length[fermList]}],
                                                       Table[Rule[mom[perm[[1,hh]]],mom[perm[[cc,hh]]]],{hh,1,Length[fermList]}]]];
							fullstruc=Join[fullstruc,Complement[{permstruc},fullstruc,SameTest->(MatchQ[#1,#2]&)]];
						];	
					];
					newstruc=fullstruc;
					Print["End More than 2 fermions"];,
					perm = Permutations[{prevpos,kk-1}];
					permstruc=ReplaceAll[newstruc,Join[Table[Rule[Ext[perm[[1,hh]]],Ext[perm[[2,hh]]]],{hh,1,2}],
                                                       Table[Rule[mom[perm[[1,hh]]],mom[perm[[2,hh]]]],{hh,1,kk-prevpos}]]];
					permstruc=Replace[permstruc,Table[Rule[Ext[perm[[1,hh]]],Ext[perm[[2,hh]]]],{hh,1,2}],\[Infinity]]/.FlippingRules;(*Does not exchange the spin indices because they are not written in the generic file*)
					newstruc=Join[newstruc,Complement[permstruc,newstruc,SameTest->(MatchQ[#1,#2]&)]];
				];
(*if fermionCounter>2*)
			](*end F*);
		];
		prevpos=kk;
		prevtype=StringTake[part<>"Z",{kk}];
	],
{kk, 2, StringLength[part]+1}];
{part,newstruc/.{Epstemp->Eps,mom[a_]->a}}
];


(* ::Section::Closed:: *)
(*Vertex reordering*)


VertexTypeSortRulesFA = {{xx___,"V","F",yy___} -> {xx,"F","V",yy},
                       {xx___,"S","F",yy___} -> {xx,"F","S",yy},
                       {xx___,"V","S",yy___} -> {xx,"S","V",yy}};

ReOrderFCforFA[ll__] := Module[{ReOrderFunc, temp, output,vt, AntiToEnd},
        ReOrderFunc[x___, {f1_?(FAPartFieldType[#] =!= "F" &), k__}, {f2_?(FAPartFieldType[#] === "F" &), l__}, y___] := ReOrderFunc[x, {f2, l}, {f1, k}, y];
        ReOrderFunc[x___, {f1_?(((FAPartFieldType[#] =!= "F") && (FAPartFieldType[#] =!= "S"))&), k__}, {f2_?((FAPartFieldType[#] === "S")&), l__}, y___] := ReOrderFunc[x, {f2, l}, {f1, k}, y];
        ReOrderFunc[x___, {f1_?(((FAPartFieldType[#] =!= "F") && (FAPartFieldType[#] =!= "S") && (FAPartFieldType[#] =!= "U"))&), k__}, {f2_?((FAPartFieldType[#] === "U")&), l__}, y___] := ReOrderFunc[x, {f2, l}, {f1, k}, y];
        AntiToEnd[x___, {f1_?FAAntiFieldQ, k__}, {f2_?(Not[FAAntiFieldQ[#]]&), l__}, y___] := AntiToEnd[x, {f2, l}, {f1, k}, y];
   (* These rules deal with charge conjugation. We are not allow to destroy the order obtained by FR! *)
         ReOrderFunc[{CC[F[ind1___]], k__}, {-F[ind2___], l__}, y___] := ReOrderFunc[{-F[ind2], l}, {CC[F[ind1]], k}, y];
         ReOrderFunc[{F[ind1___], k__}, {-CC[F[ind2___]], l__}, y___] := ReOrderFunc[{-CC[F[ind2]], l}, {F[ind1], k}, y];
         ReOrderFunc[{F[ind1___], k__}, {$Maj[-F[ind2___]], l__}, y___] := ReOrderFunc[{$Maj[F[ind2]], l}, {F[ind1], k}, y];
        temp = ReOrderFunc @@ (ll /. {CC[{aa_, bb_}] :> {CC[aa], bb}, $Maj[{aa_, bb_}] :> {$Maj[aa], bb}});
        temp = List @@ temp;
        vt = StringJoin @@ (FAPartFieldType /@ temp);
        output = Which[(vt === "FFV") || (vt === "FFS") || (vt === "UUV"),
              If[FAAntiFieldQ[temp[[2, 1]]], {temp[[2]], temp[[1]], temp[[3]]}, temp],
           (vt === "SSS"),
              temp = List @@ AntiToEnd @@ temp;
              If[(-temp[[2, 1]]) === temp[[3, 1]], {temp[[2]], temp[[1]], temp[[3]]}, temp],
           (vt === "VVV"),
              temp = List @@ AntiToEnd @@ temp;
              If[FAAntiFieldQ[temp[[3, 1]]], 
                        Which[(-temp[[2 ,1]]) === temp[[3, 1]], {temp[[1]], temp[[3]], temp[[2]]},
                              (-temp[[1, 1]]) === temp[[3, 1]], {temp[[2]], temp[[3]], temp[[1]]},
                              True, temp],
                        temp],        
           vt === "SVV",
              If[FAAntiFieldQ[temp[[3, 1]]], {temp[[1]], temp[[3]], temp[[2]]}, temp, temp],
           vt === "SSV",
              If[FAAntiFieldQ[temp[[2, 1]]], {temp[[2]], temp[[1]], temp[[3]]}, temp],
           (vt === "VVVV"),
              temp = Reverse[List @@ AntiToEnd @@ temp];
              If[temp[[1, 1]] =!= temp[[2, 1]],
                 If[FAAntiFieldQ[temp[[1, 1]]],
                    Which[(-temp[[3, 1]]) === temp[[1, 1]], {temp[[1]], temp[[3]], temp[[2]], temp[[4]]}, 
                          (-temp[[4, 1]]) === temp[[1, 1]], {temp[[1]], temp[[4]], temp[[3]], temp[[2]]},
                          True, temp],
                    temp],
                 temp],
           vt === "SSVV",
              temp = If[FAAntiFieldQ[temp[[1, 1]]], {temp[[2]], temp[[1]], temp[[3]], temp[[4]]}, temp];
              If[FAAntiFieldQ[temp[[3, 1]]], {temp[[1]], temp[[2]], temp[[4]], temp[[3]]}, temp],
           vt === "SUU",
              If[FAAntiFieldQ[temp[[3, 1]]], {temp[[1]], temp[[3]], temp[[2]]}, temp, temp],
           True,
              temp]];


(* ::Section::Closed:: *)
(*Particle renaming*)


FAPartFieldType[F[__]] := "F";
FAPartFieldType[V[__]] := "V";
FAPartFieldType[S[__]] := "S"; 
FAPartFieldType[U[__]] := "U";            
FAPartFieldType[T[__]] := "T";
FAPartFieldType[Times[-1, F[__]]] := "-F";
FAPartFieldType[Times[-1, V[__]]] := "V";
FAPartFieldType[Times[-1, S[__]]] := "S";
FAPartFieldType[Times[-1, U[__]]] := "U";
FAPartFieldType[Times[-1, T[__]]] := "T";
FAPartFieldType[{f_, k__}] := FAPartFieldType[f];

FAPartFieldType[CC[ff_]] := FAPartFieldType[ff];
FAPartFieldType[-CC[ff_]] := FAPartFieldType[ff];

FAPartFieldType[$Maj[ff_]] := FAPartFieldType[ff];
FAPartFieldType[-$Maj[ff_]] := FAPartFieldType[ff];

FAAntiFieldQ[f_[__]] := False;
FAAntiFieldQ[Times[-1, f_[__]]] := True;

PartNameFA[{ll__}] := MapIndexed[PartNameFA, {ll}];

PartNameFA[CC[ff_], {k_}] := CC[PartNameFA[ff, {k}]];

PartNameFA[$Maj[ff_], {k_}] := $Maj[PartNameFA[ff, {k}]];
              
PartNameFA[f_, {k_}] := Block[{exlist, output, nind, fclass},
      fclass = Which[MemberQ[MR$ClassNameList, f], f,
                     MemberQ[MR$ClassNameList, anti[f]], anti[f],
                     True, If[AntiFieldQ[f], anti[f], f] /. FA$MemberToClass /. FA$ClassToName];
      nind = NInd[fclass];
      exlist = Which[PartFieldType[fclass] === "F", 
                          If[nind == 1, {},
                             Table[ToExpression[StringJoin["e", ToString[k], "x", ToString[ll]]], {ll, 2, nind}]],
                        PartFieldType[fclass] === "V", 
                          If[nind == 1, {},
                             Table[ToExpression[StringJoin["e", ToString[k], "x", ToString[ll]]], {ll, 2, nind}]],                               
                        (PartFieldType[fclass] === "S") || (PartFieldType[fclass] === "U"), 
                          If[nind == 0, {},
                             Table[ToExpression[StringJoin["e", ToString[k], "x", ToString[ll]]], {ll, 1, nind}]],
                        PartFieldType[fclass] === "T", 
                          If[nind == 2, {},
                             Table[ToExpression[StringJoin["e", ToString[k], "x", ToString[ll]]], {ll, 3, nind}]]]; 
      If[MemberQ[MR$ClassNameList, f] || MemberQ[MR$ClassNameList, anti[f]], 
         output = If[exlist === {}, 
                     If[AntiFieldQ[f], {-MR$Class[anti[f]], k}, {MR$Class[f], k}], 
                     If[AntiFieldQ[f], {-MergeFAClassToBlank[MR$Class[anti[f]], exlist], k}, {MergeFAClassToBlank[MR$Class[f], exlist], k}]],
         (******************)
         output = If[exlist === {}, If[AntiFieldQ[f], {-MR$Class[fclass], k}, {MR$Class[fclass], k}],
                     If[AntiFieldQ[f], {-MergeFAClassToBlank[MR$Class[fclass], exlist], k}, {MergeFAClassToBlank[MR$Class[fclass], exlist], k}]]]];
  


(* ::Subsection:: *)
(*Particle ordering - New interface version*)


(* ::Text:: *)
(*This is the particle ordering according to the new interface version of 03.10.*)


FAParticleOrderingQBuild[x_,x_]:=True;
FAParticleOrderingQBuild["-F",_]:=True;
FAParticleOrderingQBuild["F",Except["-F",_]]:=True;
FAParticleOrderingQBuild["S",Except["-F"|"F",_]]:=True;
FAParticleOrderingQBuild["U",Except["-F"|"F"|"S",_]]:=True;
FAParticleOrderingQBuild["V",Except["-F"|"F"|"U"|"S",_]]:=True;
FAParticleOrderingQBuild["T",Except["-F"|"F"|"U"|"V"|"S",_]]:=True;


FAParticleOrderingQ = FAParticleOrderingQBuild[FAPartFieldType[#1[[1]]], FAPartFieldType[#2[[1]]]] === True &;


(* ::Section::Closed:: *)
(*CreateFAVertexList*)


(* ::Text:: *)
(*CreateFAVertexList transforms the output of FeynmanRules[ ] into the FA vertex format. The format is based ont he one used in the 1st version of the interface, and is*)
(**)
(*{  tag,    FA_names,   vertex,    FR_names }*)
(**)
(*with tag the tag of the vertex (e.g., "FFV"), FA_names the FA naming conventions (e.g., F[1,  ], etc), vertex is the analytic expression of the vertex as it comes from FeynmanRules, and FR_names is the list of FR particles names, as they come form FeynmanRules[], but without the number.*)


CreateFAVertexList[{parts_List, vertex_}] := Block[{faparts = parts[[All,1]] /. {ff_?((MajoranaFieldQ[#] && AntiFieldQ[#])&) :> $Maj[ff]}, porder,gam,tag,kk,FSignature, rules ,vert,vert2, 
frparts,nf,naf,spinL,faparts2,fm,ind,SPlist,ikk},

   (* Create the FA names *)
   faparts = PartNameFA[faparts];

porder=Ordering[faparts,All,FAParticleOrderingQ[#1,#2]&];
   (* We reorder the particles into FA ordering *)
   faparts=Sort[faparts, FAParticleOrderingQ[#1,#2]&];
   
   faparts = faparts /. {CC[F[ind___]] :> -F[ind], $Maj[-F[ind___]] :> F[ind]};

   (* No we create the tag (FFV, etc.)*)
   tag = StringJoin @@ (FAPartFieldType[#1]& @@@ faparts/.{"-F"->"F"});

   (* change the label of the external particles according to their order *)
   SPlist=Union[Cases[vertex,SP[a_Integer,b_Integer],\[Infinity]]];
   SPlist=Table[SPlist[[ll]]-> (SPlist[[ll]]/.Table[faparts[[kk,2]]->kk,{kk,1,Length[faparts]}]),{ll,1,Length[SPlist]}];
   vert2=Replace[vertex,Join[Table[Ext[faparts[[kk,2]],ikk___]->Ext[kk,ikk],{kk,1,Length[faparts]}],Table[FV[faparts[[kk,2]],ind__]->FV[kk,ind],{kk,1,Length[faparts]}],Table[SlashedP[faparts[[kk,2]]]->SlashedP[kk],{kk,1,Length[faparts]}],SPlist],\[Infinity],Heads->True];
   faparts[[All,2]]=Table[kk,{kk,1,Length[faparts]}];

   nf = Count[FAPartFieldType[#1]& @@@ faparts,"F"];
   naf = Count[FAPartFieldType[#1]& @@@ faparts,"-F"];
   (* Force the spin indices to be in the order 1 2 for two fermions vertices because those indices may not be transmitted to FA*)
   If[nf+naf==2,
		vert=ExpandAll[vert2];
        vert2=0;
        If[Head[vert]===Plus,
          For[kk=1,kk<=Length[vert],kk++,
           If[Signature[Cases[vert[[kk]],Index[Spin,Ext[ss_]]->ss,\[Infinity]]]===-1,
             vert2=vert2+Replace[vert[[kk]],FlipRules];,
             vert2=vert2+vert[[kk]];
             ];         
          ];,
          If[Signature[Cases[vert,Index[Spin,Ext[ss_]]->ss,\[Infinity]]]===-1, vert2=+Replace[vert,FlipRules];
,vert2=vert;]
        ];
   ];

   (* Return and exit *)
   Return[{tag, faparts,vert2,parts[[porder,1]]}]
];



(* ::Section:: *)
(*CouplRenaming*)


(* ::Subsubsection:: *)
(*FACommonCouplingFactor[vertex]*)


FACommonCouplingFactor[vertex_]:=Module[{a,f21List={},indices={},j,k,commonDen=1,vertexTmp,vertexTerms={},leftOver,commonTerm=1},
(*Get a list of the indices.  Make a replacement list from them.*)
Do[AppendTo[indices,vertex[[1,j]]/.{-f_[_,a_]->a,f_[_,a_]->a,f_[_]->1}],{j,1,Length[vertex[[1]]]}];
indices=DeleteCases[Flatten[indices],1];
Do[AppendTo[f21List,f_[___,indices[[j]],___]->1],{j,1,Length[indices]}];
(*Print[f21List];*)

(*Get a list of all the factors after removing anything that has an index.*)
If[Head[vertex[[2,1]]]===List,
Do[
vertexTmp=Collect[Expand[vertex[[2,j,1]]],{IndexDelta[__],SUNT[__],SUNF[__]},Simplify];
If[Head[vertexTmp]===Plus,
Do[
AppendTo[vertexTerms,vertexTmp[[k]]//.{IndexDelta[__]->1,SUNT[__]->1,SUNF[__]->1,Complex[0,a_]->a}];
,{k,1,Length[vertexTmp]}];
,
AppendTo[vertexTerms,vertexTmp//.{IndexDelta[__]->1,SUNT[__]->1,SUNF[__]->1,Complex[0,a_]->a}];
];
,{j,1,Length[vertex[[2]]]}];

,
vertexTmp=Collect[Expand[vertex[[2,1]]],{IndexDelta[__],SUNT[__],SUNF[__]},Simplify];
If[Head[vertexTmp]===Plus,
Do[
AppendTo[vertexTerms,vertexTmp[[k]]//.{IndexDelta[__]->1,SUNT[__]->1,SUNF[__]->1,Complex[0,a_]->a}];
,{k,1,Length[vertexTmp]}];
,
AppendTo[vertexTerms,vertexTmp//.{IndexDelta[__]->1,SUNT[__]->1,SUNF[__]->1,Complex[0,a_]->a}];
];
];
vertexTerms=DeleteCases[vertexTerms,0];
(*Print[vertexTerms];*)

(*Remove the common denominator.*)
commonDen=Denominator[Together[Sum[a[j]vertexTerms[[j]],{j,1,Length[vertexTerms]}]]];
vertexTerms=commonDen*vertexTerms;
(*Print[commonDen];
Print[vertexTerms];*)

(*Get the common term.*)
Which[Length[vertexTerms]>1,
leftOver=Simplify[vertexTerms[[1]]/vertexTerms[[2]]];
commonTerm=vertexTerms[[1]]/Numerator[leftOver];
(*Print[commonTerm];*)
Do[
leftOver=Simplify[commonTerm/vertexTerms[[j]]];
commonTerm=commonTerm/Numerator[leftOver];
(*Print[commonTerm];*)
,{j,3,Length[vertexTerms]}];
commonTerm=Simplify[commonTerm/commonDen];
,
Length[vertexTerms]==1,
commonTerm=vertexTerms[[1]]/commonDen;
,
Length[vertexTerms]==0,
commonTerm=1;
];

(*Return the common term.*)
commonTerm
];


(* ::Subsubsection:: *)
(*FAExtractCommonCoupling[vertex, nvertex]*)


FAExtractCommonCoupling[vertex_,n_]:=Module[{j,vertType,vertTemp,indices={},couplName,couplName1,commonTerm1=1,indices1={},couplName2,commonTerm2=1,indices2={},vert},
(*Print[vertex];*)
If[Length[vertex[[2]]]>1,
vertType=Sort[vertex[[1]]/.{F[__]->F,V[__]->V,S[__]->S,U[__]->U}/.{-F->F,-V->V,-S->S,-U->U}];
(*Obtain common term (or terms if a FFV vertex).*)
If[vertType==={F,F,V}||vertType==={F,F,S},
  vertTemp=vertex;
  vertTemp[[2]]=vertex[[2,1]];
  commonTerm1=FACommonCouplingFactor[vertTemp];
  vertTemp=vertex;
  vertTemp[[2]]=vertex[[2,2]];
  commonTerm2=FACommonCouplingFactor[vertTemp];
  ,
  commonTerm1=FACommonCouplingFactor[vertex];
];
(*Print[commonTerm1];*)

(*Determine if there are any indices in commonTerm.*)
Do[AppendTo[indices,vertex[[1,j]]/.{-f_[_,a_]->a,f_[_,a_]->a,f_[_]->1}],{j,1,Length[vertex[[1]]]}];
indices=DeleteCases[Flatten[indices],1];
Do[
  If[FreeQ[commonTerm1,indices[[j]]]===False,AppendTo[indices1,indices[[j]]]];
  If[FreeQ[commonTerm2,indices[[j]]]===False,AppendTo[indices2,indices[[j]]]];
,{j,1,Length[indices]}];

(*Create couplName.*)
If[commonTerm2=!=1&&commonTerm1=!=commonTerm2,
  couplName="gc"<>ToString[n]<>"L";
  If[Length[indices1]>0,couplName1=couplName<>"[";Do[couplName1=couplName1<>ToString[indices1[[j]]]<>"_,",{j,1,Length[indices1]}];couplName1=StringDrop[couplName1,-1]<>"]";,couplName1=couplName;];
  couplName1=ToExpression[couplName1];
  If[commonTerm1=!=1,AppendTo[M$FACouplings,couplName1->commonTerm1]];
  If[Length[indices1]>0,couplName1=couplName<>"[";Do[couplName1=couplName1<>ToString[indices1[[j]]]<>",",{j,1,Length[indices1]}];couplName1=StringDrop[couplName1,-1]<>"]";,couplName1=couplName;];
  couplName1=ToExpression[couplName1];

  couplName="gc"<>ToString[n]<>"R";
  If[Length[indices2]>0,couplName2=couplName<>"[";Do[couplName2=couplName2<>ToString[indices2[[j]]]<>"_,",{j,1,Length[indices2]}];couplName2=StringDrop[couplName2,-1]<>"]";,couplName2=couplName;];
  couplName2=ToExpression[couplName2];
  If[commonTerm2=!=1,AppendTo[M$FACouplings,couplName2->commonTerm2]];
  If[Length[indices2]>0,couplName2=couplName<>"[";Do[couplName2=couplName2<>ToString[indices2[[j]]]<>",",{j,1,Length[indices2]}];couplName2=StringDrop[couplName2,-1]<>"]";,couplName2=couplName;];
  couplName2=ToExpression[couplName2];
  ,
  couplName="gc"<>ToString[n];
  If[Length[indices1]>0,couplName1=couplName<>"[";Do[couplName1=couplName1<>ToString[indices1[[j]]]<>"_,",{j,1,Length[indices1]}];couplName1=StringDrop[couplName1,-1]<>"]";,couplName1=couplName;];
  couplName1=ToExpression[couplName1];
  If[commonTerm1=!=1,AppendTo[M$FACouplings,couplName1->commonTerm1]];
  If[Length[indices1]>0,couplName1=couplName<>"[";Do[couplName1=couplName1<>ToString[indices1[[j]]]<>",",{j,1,Length[indices1]}];couplName1=StringDrop[couplName1,-1]<>"]";,couplName1=couplName;];
  couplName1=ToExpression[couplName1]; 
];

(*Replace common term (s) with constant.*)
vert=vertex;

(********* DEBUT CORRECTION DAMIEN *********)
vert[[2,1,1]]=Collect[Expand[vert[[2,1,1]]],{IndexDelta[__],SUNT[__],SUNF[__]},Simplify];
vert[[2,2,1]]=Collect[Expand[vert[[2,2,1]]],{IndexDelta[__],SUNT[__],SUNF[__]},Simplify];
(********** FIN CORRECTION DAMIEN **********)

If[commonTerm2=!=1&&commonTerm1=!=commonTerm2,
  vert[[2,1,1]]=Simplify[couplName1/commonTerm1 vert[[2,1,1]]];
  vert[[2,2,1]]=Simplify[couplName2/commonTerm2 vert[[2,2,1]]];
  ,
  If[commonTerm2=!=1||commonTerm1=!=1,
    If[Head[vert[[2,1]]]===List,
      Do[vert[[2,j,1]]=Simplify[couplName1/commonTerm1 vert[[2,j,1]]],{j,1,Length[vert[[2]]]}];
      ,
      vert[[2,1]]=Simplify[couplName1/commonTerm1 vert[[2,1]]]
    ];
  ];
];
vert
,vertex]];


(* ::Subsubsection::Closed:: *)
(*Main call*)


FACouplingRenaming[favertices_] := Block[{vertListTmp = {}, vertN = 1},

     (* Perform Renaming *)
			Do[AppendTo[vertListTmp, FAExtractCommonCoupling[favertices[[j]],vertN]];
               vertN++,
               {j,1,Length[favertices]}];

     (* Return and exit *)
     Return[vertListTmp]

];


(* ::Section::Closed:: *)
(*Index handling*)


MakeFAExtIndex[Index[name_, Ext[k__]], fcl_,part_] := Block[{tempk, firstk, indlist, output, fcltemp = fcl,parttemp},
      fcltemp = fcltemp //. $Maj->Identity;
      fcltemp = fcltemp //. {x___, f_?AntiFieldQ, y___} :> {x, anti[f], y};
      fcltemp = fcltemp //. FA$MemberToClass;
      fcltemp = fcltemp //. FA$ClassToName;
      tempk = StringJoin @@ (ToString /@ {k});
      firstk = If[Length[{k}] > 1, First[[{k}]], k];
	  parttemp=Replace[part[[firstk,1]],-1->1,{1}];
      indlist = $IndList[fcltemp[[firstk]]] //. Index -> Identity;
      output = parttemp[[2,Flatten[Position[indlist, name]][[1]]-(Length[indlist]-Length[parttemp[[2]]])]]
]; 

MakeFAIntIndex[Index[name_, k__]] := Index[name, ToExpression[StringJoin @@ (ToString /@ {k})]];



(* ::Subsection:: *)
(*IndexSum*)


GetIndices[aa_+bb_]:=GetIndices[aa]+GetIndices[bb];
GetIndices[xx___,ff_[facs__],yy___]:=GetIndices[xx,ff,facs,yy]/;(ff=!=Plus)&&(ff=!=Index)&&(ff=!=NTIndex);
GetIndices[xx___,tt_?((FreeQ[#,Index]&&FreeQ[#,NTIndex])&),yy___]:=GetIndices[xx,yy];
GetIndices[xx___,Index[n__],yy___,Index[n__],zz___]:=GetIndices[xx,Index[n],yy,zz];
GetIndices[xx___, Index[_, _?NumericQ], yy___] := GetIndices[xx, yy];

IndexSum[a_*b_,{n_,ninc__}]:=a*IndexSum[b,{n,ninc}]/;FreeQ[a,n]

MakeIndexSum[lisi_List] := MakeIndexSum /@ lisi;

MakeIndexSum[expr_?(Head[#]=!=List &)]:=Block[{tmpexprlist,getindlist,IndexSum1},
   tmpexprlist=Expand[expr];
   tmpexprlist=If[Head[tmpexprlist]===Plus,List@@tmpexprlist,{tmpexprlist}];
   getindlist=GetIndices/@tmpexprlist;
   getindlist=getindlist/.GetIndices->List/.{}->1;
   getindlist=getindlist/.Index[name_,k_]:>IndexSum1[1,{k,First[MRIndexRange[Index[name]]],Last[MRIndexRange[Index[name]]]}];
   For[term=1,term<=Length[tmpexprlist],term++,
     If[Length[getindlist[[term]]]>0,getindlist[[term,1,1]]=tmpexprlist[[term]];
       For[ind=2,ind<=Length[getindlist[[term]]],ind++,
         getindlist[[term,ind,1]]=getindlist[[term,ind-1]];
       ];
       tmpexprlist[[term]]=Last[getindlist[[term]]];
     ];
   ];
   tmpexprlist=tmpexprlist/.IndexSum1->IndexSum;
   tmpexprlist=Plus@@tmpexprlist];






(* ::Section:: *)
(*Generic file writing functions*)


(* ::Subsection::Closed:: *)
(*WriteKinIndices[genfile,dirIndOpt]*)


WriteKinIndices[genfile_,dirInd_]:=(
WriteString[genfile, "(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)\n"];
WriteString[genfile, "(*                                                                               *)\n"];
WriteString[genfile, "(*         This file has been automatically generated by FeynRules.              *)\n"];
WriteString[genfile, "(*                                                                               *)\n"];
WriteString[genfile, "(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "\n"];
(*Kinematic indices*)
WriteString[genfile, "(*     Kinematic indices    *)\n"];
WriteString[genfile, "\n"];
If[dirInd,WriteString[genfile, "KinematicIndices[ F ] = {Dirac};\n"],WriteString[genfile, "KinematicIndices[ F ] = {};\n"]];
WriteString[genfile, "KinematicIndices[ V ] = {Lorentz};\n"];
WriteString[genfile, "KinematicIndices[ S ] = {};\n"];
WriteString[genfile, "KinematicIndices[ SV ] = {Lorentz};\n"];
WriteString[genfile, "KinematicIndices[ U ] = {};\n"];
If[Or@@(Not[StringFreeQ[#,"T"]]&/@structurelistFA[[All,1]]),WriteString[genfile, "KinematicIndices[ T ] = {Lorentz,Lorentz};\n"];];
WriteString[genfile, "\n"];

WriteString[genfile,"$FermionLines = "<>ToString[fermionCounter<=2]<>";\n"];
WriteString[genfile, "\n"];

(*If[dirInd>2,WriteString[genfile, "$FermionLines = False;\n"]];*)

);


(* ::Subsection::Closed:: *)
(*WriteSimpRules[genfile]*)


WriteSimpRules[genfile_]:=(
	  
(*Simplification rules from original lorentz.gen*)
WriteString[genfile, "(*     Simplification rules    *)\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "Attributes[ MetricTensor ] = Attributes[ ScalarProduct ] = {Orderless}\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "FourVector/: -FourVector[ mom_, mu_ ] := FourVector[Expand[-mom], mu]\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "FourVector[ 0, _ ] = 0\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "SpinorType[j_Integer, ___] := MajoranaSpinor /; SelfConjugate[F[j]]\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "SpinorType[_Integer, __] = DiracSpinor\n"];
WriteString[genfile, "\n"];
);


(* ::Subsection::Closed:: *)
(*WritePropagators[genfile,dicacIndOpt]*)


WritePropagators[genfile_,diracIndOpt_]:=(

(*Generic propagators*)
WriteString[genfile, "(*     Generic propagators    *)\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "M$GenericPropagators={\n"];
WriteString[genfile, "\n"];

WriteString[genfile, "(*general fermion propagator:*)\n"];
WriteString[genfile, "\n"];
If[diracIndOpt,
	WriteString[genfile, "AnalyticalPropagator[External][s1 F[j1,mom,{di1}]]\[Equal]\n"];
    WriteString[genfile, "DiracObject[SpinorType[j1][-mom,Mass[F[j1]]]][di1],\n"];
    WriteString[genfile, "\n"];
    WriteString[genfile, "AnalyticalPropagator[Internal][s1 F[j1,mom,{di1}->{di2}]]\[Equal]\n"];
    WriteString[genfile, "DiracObject[DiracSlash[-mom]+Mass[F[j1]]][di1,di2]*\n"];
    WriteString[genfile, "I PropagatorDenominator[mom,Mass[F[j1]]],\n"];,

	WriteString[genfile, "AnalyticalPropagator[External][s1 F[j1,mom]]\[Equal]\n"];
    WriteString[genfile, "NonCommutative[SpinorType[j1][-mom,Mass[F[j1]]]],\n"];
    WriteString[genfile, "\n"];
    WriteString[genfile, "AnalyticalPropagator[Internal][s1 F[j1,mom]]\[Equal]\n"];
    WriteString[genfile, "NonCommutative[DiracSlash[-mom]+Mass[F[j1]]]*\n"];
    WriteString[genfile, "I PropagatorDenominator[mom,Mass[F[j1]]],\n"];
];
WriteString[genfile, "\n"];

WriteString[genfile, "(*general vector boson propagator:*)\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "AnalyticalPropagator[External][s1 V[j1,mom,{li2}]]\[Equal]\n"];
WriteString[genfile, "PolarizationVector[V[j1],mom,li2],\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "AnalyticalPropagator[Internal][s1 V[j1,mom,{li1}\[Rule]{li2}]]\[Equal]\n"];
WriteString[genfile, "-I PropagatorDenominator[mom,Mass[V[j1]]]*\n"];
WriteString[genfile, "(MetricTensor[li1,li2]-(1-GaugeXi[V[j1]])*\n"];
WriteString[genfile, "FourVector[mom,li1] FourVector[mom,li2]*\n"];
WriteString[genfile, "PropagatorDenominator[mom,Sqrt[GaugeXi[V[j1]]] Mass[V[j1]]]),\n"];
WriteString[genfile, "\n"];

WriteString[genfile, "(*general mixing scalar-vector propagator:*)\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "AnalyticalPropagator[Internal][s1 SV[j1,mom,{li1}\[Rule]{li2}]]==\n"];
WriteString[genfile, "I Mass[SV[j1]] PropagatorDenominator[mom,Mass[SV[j1]]]*\n"];
WriteString[genfile, "FourVector[mom,If[s1\[Equal]1||s1\[Equal]-2,li1,li2]],\n"];
WriteString[genfile, "\n"];

WriteString[genfile, "(*general scalar propagator:*)\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "AnalyticalPropagator[External][s1 S[j1,mom]]\[Equal]1,\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "AnalyticalPropagator[Internal][s1 S[j1,mom]]\[Equal]\n"];
WriteString[genfile, "I PropagatorDenominator[mom,Sqrt[GaugeXi[S[j1]]] Mass[S[j1]]],\n"];
WriteString[genfile, "\n"];

If[Or@@(Not[StringFreeQ[#,"T"]]&/@structurelistFA[[All,1]]),
  WriteString[genfile, "(*tensor propagator:*)\n"];
  WriteString[genfile, "\n"];
  WriteString[genfile, "AnalyticalPropagator[External][ s1 T[j1, mom, {li1p1, li1p2}] ] == PolarizationTensor[T[j1], mom, li1p1, li1p2],\n"];
  WriteString[genfile, "\n"];
  WriteString[genfile, "AnalyticalPropagator[Internal][ s1 T[j1, mom, {li1p1, li1p2} -> {li2p1, li2p2}] ] ==\n"];
  WriteString[genfile, "(I*(((-((FourVector[mom, li1p1]*FourVector[mom, li2p2])/Mass[T[j1]]^2) + MetricTensor[li1p1, li2p2])*  
    (-((FourVector[mom, li1p2]*FourVector[mom, li2p1])/Mass[T[j1]]^2) + MetricTensor[li1p2, li2p1]))/2 +  
    ((-((FourVector[mom, li1p1]*FourVector[mom, li2p1])/Mass[T[j1]]^2) + MetricTensor[li1p1, li2p1])* 
     (-((FourVector[mom, li1p2]*FourVector[mom, li2p2])/Mass[T[j1]]^2) + MetricTensor[li1p2, li2p2]))/2 -  
   ((-((FourVector[mom, li1p1]*FourVector[mom, li1p2])/Mass[T[j1]]^2) + MetricTensor[li1p1, li1p2])* 
     (-((FourVector[mom, li2p1]*FourVector[mom, li2p2])/Mass[T[j1]]^2) + MetricTensor[li2p1, li2p2]))/3)) PropagatorDenominator[mom, Mass[T[j1]]],\n"];
  WriteString[genfile, "\n"];
];

WriteString[genfile, "(*general Fadeev-Popov ghost propagator:*)\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "AnalyticalPropagator[External][s1 U[j1,mom]]\[Equal]1,\n"];
WriteString[genfile, "\n"];
WriteString[genfile, "AnalyticalPropagator[Internal][s1 U[j1,mom]]\[Equal]\n"];
WriteString[genfile, "I*PropagatorDenominator[mom,Sqrt[GaugeXi[U[j1]]] Mass[U[j1]]]\n"];
WriteString[genfile, "}\n"];
WriteString[genfile, "\n"];
);


(* ::Subsection:: *)
(*WriteGenCouplings[genfile,diracIndOpt]*)


WriteGenCouplings[genfile_,diracIndOpt_]:=Module[{WriteStructure,temp},

WriteStructure[gen_,struc_]:=Module[{struc2,struc3,sumInd,SI},

	struc2=struc/.{Times[aa___,FV[bb_,xx_],cc___,Ga[xx_,dd___],ee___]->Times[aa,cc,SlashedP[bb,dd],ee],
	Times[aa___,FV[bb_,xx_],cc___,TensDot[ff___,Ga[xx_],gg___][hh___],ee___]->Times[aa,cc,TensDot[ff,SlashedP[bb],gg][hh],ee]};(*maybe obsolete now?*)

	struc2 = struc2/.FV[aa_,bb_Pattern]->FV[aa,  Index[Lorentz, bb]]//.{Times[aa___,Eps[bb___,dd_,cc___],ee___,FV[ff_,dd_],hh___]->Times[aa,Eps[bb,FV[ff],cc],ee,hh]};

	Switch[struc2,
		1,WriteString[gen,ToString[1]];,
		-1,WriteString[gen,ToString[-1]];,
		(*Dirac matrices and co, with or without spin indices, i.e. alone or from a TensDot for external and summed indices*)
		IndexDelta[Index[Spin,Ext[aa_]],Index[Spin,Ext[xx_]]],If[diracIndOpt,WriteString[gen,"DiracObject[ChiralityProjector[-1]][di"<>ToString[struc2[[1,2,1]]]<>",di"<>ToString[struc2[[2,2,1]]]<>"]+DiracObject[ChiralityProjector[+1]][di"<>ToString[struc2[[1,2,1]]]<>",di"<>ToString[struc2[[2,2,1]]]<>"]"];,WriteString[gen,"NonCommutative[ChiralityProjector[-1]]+NonCommutative[ChiralityProjector[+1]]"];];,
		Ga[Index[Lorentz,Ext[xx_]],__],If[diracIndOpt,WriteString[gen,"DiracObject[DiracMatrix[li"<>ToString[struc2[[1,2,1]]]<>"]][di"<>ToString[struc2[[2,2,1]]]<>",di"<>ToString[struc2[[3,2,1]]]<>"]"];,WriteString[gen,"NonCommutative[DiracMatrix[li"<>ToString[struc2[[1,2,1]]]<>"]]"];];,
		Ga[Index[Lorentz,Ext[xx_,yy_]],__],If[diracIndOpt,WriteString[gen,"DiracObject[DiracMatrix[li"<>ToString[struc2[[1,2,1]]]<>"p"<>ToString[struc2[[1,2,2]]]<>"]][di"<>ToString[struc2[[2,2,1]]]<>",di"<>ToString[struc2[[3,2,1]]]<>"]"];,WriteString[gen,"NonCommutative[DiracMatrix[li"<>ToString[struc2[[1,2,1]]]<>"p"<>ToString[struc2[[1,2,2]]]<>"]]"];];,
		Ga[Index[Lorentz,Ext[xx_]]],WriteString[gen,"DiracMatrix[li"<>ToString[struc2[[1,2,1]]]<>"]"];,
		Ga[Index[Lorentz,Ext[xx_,yy_]]],WriteString[gen,"DiracMatrix[li"<>ToString[struc2[[1,2,1]]]<>"p"<>ToString[struc2[[1,2,2]]]<>"]"];,
		Ga[_,__],If[diracIndOpt,WriteString[gen,"DiracObject[DiracMatrix["<>ToString[struc2[[1,1]]]<>"]][di"<>ToString[struc2[[2,2,1]]]<>",di"<>ToString[struc2[[3,2,1]]]<>"]"];,WriteString[gen,"NonCommutative[DiracMatrix["<>ToString[struc2[[1,1]]]<>"]]"];];,
		Ga[_],WriteString[gen,"DiracMatrix["<>ToString[struc2[[1,1]]]<>"]"];,
		ProjM[__],If[diracIndOpt,WriteString[gen,"DiracObject[ChiralityProjector[-1]][di"<>ToString[struc2[[1,2,1]]]<>",di"<>ToString[struc2[[2,2,1]]]<>"]"];,WriteString[gen,"NonCommutative[ChiralityProjector[-1]]"];];,
		ProjP[__],If[diracIndOpt,WriteString[gen,"DiracObject[ChiralityProjector[+1]][di"<>ToString[struc2[[1,2,1]]]<>",di"<>ToString[struc2[[2,2,1]]]<>"]"];,WriteString[gen,"NonCommutative[ChiralityProjector[+1]]"];];,
		ProjM,WriteString[gen,"ChiralityProjector[-1]"];,
		ProjP,WriteString[gen,"ChiralityProjector[+1]"];,
		SlashedP[xx_,__],If[diracIndOpt,WriteString[gen,"DiracObject[DiracSlash[mom"<>ToString[struc2[[1]]]<>" ]][di"<>ToString[struc2[[2,2,1]]]<>",di"<>ToString[struc2[[3,2,1]]]<>"]"];,WriteString[gen,"NonCommutative[DiracSlash[mom"<>ToString[struc2[[1]]]<>" ]]"];];,
		SlashedP[xx_],WriteString[gen,"DiracSlash[mom"<>ToString[struc2[[1]]]<>" ]"];,
		(*product of Dirac matrices*)
		_TensDot[__],If[diracIndOpt,
			WriteString[gen,"DiracObject["];
				Do[WriteStructure[gen,struc2[[0,ff]]];If[ff<Length[struc2[[0]]],WriteString[gen,", "]];,{ff,Length[struc2[[0]]]}];
			WriteString[gen," ][di"<>ToString[struc2[[1,2,1]]]<>",di"<>ToString[struc2[[2,2,1]]]<>"]"];,
			WriteString[gen,"NonCommutative["];
				Do[WriteStructure[gen,struc2[[0,ff]]];If[ff<Length[struc2[[0]]],WriteString[gen,", "]];,{ff,Length[struc2[[0]]]}];
			WriteString[gen," ]"];
			];,
		(*metrix, vector for external and summed indices, scalar product and Levi-Civita tensor*)
		ME[Index[Lorentz,Ext[xx__]],Index[Lorentz,Ext[yy__]]],WriteString[gen,"MetricTensor[li"<>ToString[struc2[[1,2,1]]]<>If[Length[struc2[[1,2]]]>1,"p"<>ToString[struc2[[1,2,2]]]," "]<>",li"<>ToString[struc2[[2,2,1]]]<>If[Length[struc2[[2,2]]]>1,"p"<>ToString[struc2[[2,2,2]]]," "]<>"]"];,
		FV[xx_,Index[Lorentz, Ext[yy_]]],WriteString[gen,"FourVector[mom"<>ToString[struc2[[1]]]<>", li"<>ToString[struc2[[2,2,1]]]<>"]"],
		FV[xx_,Index[Lorentz, Ext[yy_,yy2_]]],WriteString[gen,"FourVector[mom"<>ToString[struc2[[1]]]<>", li"<>ToString[struc2[[2,2,1]]]<>"p"<>ToString[struc2[[2,2,2]]]<>"]"],
		FV[xx_,yy_],WriteString[gen,"FourVector[mom"<>ToString[struc2[[1]]]<>", "<>ToString[struc2[[2,2,1]]]<>"]"],
		SP[__],WriteString[gen,"ScalarProduct[mom"<>ToString[struc2[[1]]]<>",mom"<>ToString[struc2[[2]]]<>"]"];,
		Eps[Index[Lorentz,Ext[a_]],Index[Lorentz,Ext[b_]],Index[Lorentz,Ext[c_]],Index[Lorentz,Ext[d_]]],WriteString[gen,"LeviCivita[li"<>ToString[struc2[[1,2,1]]]<>", li"<>ToString[struc2[[2,2,1]]]<>", li"<>ToString[struc2[[3,2,1]]]<>", li"<>ToString[struc2[[4,2,1]]]<>"]"],
		Eps[Index[Lorentz,Ext[a__]],Index[Lorentz,Ext[b__]],Index[Lorentz,Ext[c__]],Index[Lorentz,Ext[d__]]],WriteString[gen,"LeviCivita[li"<>ToString[struc2[[1,2,1]]]<>If[Length[struc2[[1,2]]]>1,"p"<>ToString[struc2[[1,2,2]]]," "]<>", li"<>ToString[struc2[[2,2,1]]]<>If[Length[struc2[[2,2]]]>1,"p"<>ToString[struc2[[2,2,2]]]," "]<>", li"<>ToString[struc2[[3,2,1]]]<>If[Length[struc2[[3,2]]]>1,"p"<>ToString[struc2[[3,2,2]]]," "]<>", li"<>ToString[struc2[[4,2,1]]]<>If[Length[struc2[[4,2]]]>1,"p"<>ToString[struc2[[4,2,2]]]," "]<>"]"],
		Eps[a___],WriteString[gen,"LeviCivita["];
			Do[
				If[MatchQ[struc2[[ff]],Index[Lorentz,Ext[b_]]],
				  WriteString[gen,"li"<>ToString[struc2[[ff,2,1]]]],
				  If[MatchQ[struc2[[ff]],_FV],WriteString[gen,"FourVector[mom"<>ToString[struc2[[ff,1]]]<>"]"],WriteString[gen,ToString[struc2[[ff,2,1]]]]];
				];
				If[ff<4,WriteString[gen,", "]];,{ff,4}];WriteString[gen,"]"];,
		(*Product of structure*)
		_Times,(*Summed indices?*)
		If[FreeQ[struc2,Pattern],Do[WriteStructure[gen,struc2[[ff]]],{ff,Length[struc2]}],
		  If[FreeQ[struc2,-1,{1}],
		    WriteString[gen,"IndexSum["];,
			WriteString[gen,"-IndexSum["];struc2=-struc2;
	      ];
		    Do[WriteStructure[gen,struc2[[ff]]],{ff,Length[struc2]}];
		    sumInd = Union[Cases[(struc2/.Pattern->SI),SI[xx_,yy_]->xx,\[Infinity],Heads->True]];
		    WriteString[gen,", "];
		    Do[WriteString[gen,"{"<>ToString[sumInd[[ff]]]<>",1,4},"];,{ff,Length[sumInd]-1}];
		    WriteString[gen,"{"<>ToString[Last[sumInd]]<>",1,4}]"];
		],
		(*If I forgot something*)
		_,Print["Warning : the following lorentz structure or part of the lorentz structure is not writen in the generic file"];
		Print[FullForm[struc]];
	 ];
];
      
(*Generic couplings*)
 WriteString[genfile, "(*     Generic couplings    *)\n"];
 WriteString[genfile, "\n"];
 WriteString[genfile, "M$GenericCouplings = {\n"];
 Do[WriteString[genfile,"\n\t (* "<>structurelistFA[[kk,1]]<>" *)\n\nAnalyticalCoupling["];
    Do[
		WriteString[genfile,"s"<>ToString[ll]<>" "<>StringTake[structurelistFA[[kk,1]],{ll}]<>"[j"<>ToString[ll]<>", mom"<>ToString[ll]];
		If[StringTake[structurelistFA[[kk,1]],{ll}]=="V",
			WriteString[genfile,", {li"<>ToString[ll]<>"}]"];,
			If[(diracIndOpt&&StringTake[structurelistFA[[kk,1]],{ll}]=="F"),
				WriteString[genfile,", {di"<>ToString[ll]<>"}]"];,
                  If[StringTake[structurelistFA[[kk,1]],{ll}]=="T",
                    WriteString[genfile,", {li"<>ToString[ll]<>"p1,li"<>ToString[ll]<>"p2}]"];,
				    WriteString[genfile,"]"];
                  ];
        
			];
		];
		If[ll<StringLength[structurelistFA[[kk,1]]],WriteString[genfile,", "];];,
		{ll,StringLength[structurelistFA[[kk,1]]]}];
	If[Not[StringFreeQ[structurelistFA[[kk,1]],"F"]]||Not[FreeQ[structurelistFA[[kk]],Eps]],
       WriteString[genfile," ] ==\nG[-1]["];,WriteString[genfile," ] ==\nG[+1]["];];
	Do[WriteString[genfile,"s"<>ToString[ll]<>" "<>StringTake[structurelistFA[[kk,1]],{ll}]<>"[j"<>ToString[ll]<>"]"];
		If[ll<StringLength[structurelistFA[[kk,1]]],WriteString[genfile,", "];];,
		{ll,StringLength[structurelistFA[[kk,1]]]}];
	WriteString[genfile,"].{"];
	temp = structurelistFA[[kk,2]];
    Do[WriteStructure[genfile,temp[[ll]]];If[ll<Length[temp],WriteString[genfile,", "]],{ll,Length[temp]}];
		If[kk<Length[structurelistFA],WriteString[genfile,"},\n"],WriteString[genfile,"}"]];,
    {kk,Length[structurelistFA]}];
WriteString[genfile, "\n"];
WriteString[genfile, "}\n"];
WriteString[genfile, "\n"];

];


(* ::Subsection::Closed:: *)
(*WriteFlippingRules[genfile,diracIndOpt]*)


WriteFlippingRules[genfile_,diracIndOpt_]:=(
  WriteString[genfile, "(* FlippingRules: the flipping rules determines how Dirac
   objects change when the order of fermion fields in the
   coupling is reversed. In other words, it defines how the 
   coupling C[F, -F, ...] is derived from C[-F, F, ...].*)

M$FlippingRules = {\n"];
  If[Not[diracIndOpt],
	WriteString[genfile, "NonCommutative[dm1:_DiracMatrix | _DiracSlash,dm2:_DiracMatrix | _DiracSlash, ChiralityProjector[+1]] ->\n NonCommutative[dm2,dm1, ChiralityProjector[+1]],\n"];
	WriteString[genfile, "NonCommutative[dm1:_DiracMatrix | _DiracSlash,dm2:_DiracMatrix | _DiracSlash, ChiralityProjector[-1]] ->\n NonCommutative[dm2,dm1, ChiralityProjector[-1]],\n"];
	WriteString[genfile, "NonCommutative[dm:_DiracMatrix | _DiracSlash, ChiralityProjector[+1]] ->\n-NonCommutative[dm, ChiralityProjector[-1]],\n"];
    WriteString[genfile, "NonCommutative[dm:_DiracMatrix | _DiracSlash, ChiralityProjector[-1]] ->\n-NonCommutative[dm, ChiralityProjector[+1]]"];
  ];
WriteString[genfile,"}\n"];
)


(* ::Subsection::Closed:: *)
(*WriteTruncation[genfile]*)


WriteTruncation[genfile_]:=(

WriteString[genfile, "\n	(* TruncationRules: rule for omitting the wave functions of
	   external Propagators defined in this file. *)

M$TruncationRules = {
  _PolarizationVector -> 1,
  _DiracSpinor -> 1,
  _MajoranaSpinor -> 1 
}"];
WriteString[genfile, "\n"];
);


(* ::Subsection:: *)
(*WriteLast[genfile]*)


WriteLast[genfile_]:=(
	  
WriteString[genfile, "	(* LastGenericRules: the very last rules that are applied to an
	   amplitude before it is returned by CreateFeynAmp. *)

M$LastGenericRules = {
  PolarizationVector[p_, _. mom:FourMomentum[Outgoing, _], li_] :>
    Conjugate[PolarizationVector][p, mom, li]
}\n"];
WriteString[genfile, "	(* cosmetics: *)

	(*  left spinor in chain + mom incoming -> \bar v
	    left spinor in chain + mom outgoing -> \bar u
	   right spinor in chain + mom incoming -> u
	   right spinor in chain + mom outgoing -> v *)
Format[
  FermionChain[
    NonCommutative[_[s1_. mom1_, mass1_]],
    r___,
    NonCommutative[_[s2_. mom2_, mass2_]]] ] :=
  Overscript[If[FreeQ[mom1, Incoming], \"u\", \"v\"], \"_\"][mom1, mass1] .
    r . If[FreeQ[mom2, Outgoing], \"u\", \"v\"][mom2, mass2]

Format[ DiracSlash ] = \"gs\"

Format[ DiracMatrix ] = \"ga\"

Format[ ChiralityProjector[1] ] = SequenceForm[\"om\", Subscript[\"+\"]]

Format[ ChiralityProjector[-1] ] = SequenceForm[\"om\", Subscript[\"-\"]]

Format[ GaugeXi[a_] ] := SequenceForm[\"xi\", Subscript[a]]

Format[ PolarizationVector ] = \"ep\"

Unprotect[Conjugate];
Format[ Conjugate[a_] ] = SequenceForm[a, Superscript[\"*\"]];
Protect[Conjugate]

Format[ MetricTensor ] = \"g\"

Format[ ScalarProduct[a__] ] := Dot[a]

Format[ FourVector[a_, b_] ] := a[b]
\n"];
);


(* ::Section::Closed:: *)
(*Write .Mod file*)


(* ::Subsection::Closed:: *)
(*WriteFAHeader*)


WriteFAHeader[outfile_] := Block[{golds},
      WriteString[outfile, "(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)\n"];
      WriteString[outfile, "(*                                                                             *)\n"];
      WriteString[outfile, "(*         This file has been automatically generated by FeynRules.            *)\n"];
      WriteString[outfile, "(*                                                                             *)\n"];
      WriteString[outfile, "(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)\n"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "FR$ModelInformation={\n"];
      WriteString[outfile, "  ModelName->"<>ToString[InputForm[M$ModelName]]];
      (WriteString[outfile, ",\n  "<>ToString[InputForm[#]]]&)/@M$Information;
      WriteString[outfile, "};\n"]; 
      WriteString[outfile, "\n"];
      WriteString[outfile, "FR$ClassesTranslation="<>ToString[InputForm[If[Length[NewFeynArtsClasses[]]>0,NewFeynArtsClasses[],{}]]]<>";"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "\n"];
      golds=If[Length[NewFeynArtsClasses[]]>0,
        DeleteCases[NewFeynArtsClasses[],_?(FreeQ[DeleteCases[Cases[M$ClassesDescription,_?(Not[FreeQ[#,Goldstone]]&)],Goldstone->x_,\[Infinity]],#[[1]]]&)][[All,2]],
        Cases[M$ClassesDescription,_?(Not[FreeQ[#,Goldstone]]&)][[All,1]]];
      WriteString[outfile, "FR$InteractionOrderPerturbativeExpansion="<>ToString[InputForm[FR$InteractionOrderPerturbativeExpansion]]<>";"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "FR$GoldstoneList="<>ToString[InputForm[golds]]<>";"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "\n"];
];


(* ::Subsection::Closed:: *)
(*WriteFAIndexDeclaration*)


WriteFAIndexDeclaration[outfile_] := Block[{},
      WriteString[outfile, "(*     Declared indices    *)\n"];
      WriteString[outfile, "\n"];
      Do[WriteString[outfile, "IndexRange[ ", ToString[MR$IndexList[[kk]]], " ] = ", 
                     If[Head[IndexRange[MR$IndexList[[kk]]]] === NoUnfold, "NoUnfold[ Range[ " <> ToString[Length[MRIndexRange[MR$IndexList[[kk]]]]] <> " ] ]\n",
                        "Range[ " <> ToString[Length[MRIndexRange[MR$IndexList[[kk]]]]] <> " ]\n"]];
         WriteString[outfile, "\n"],
         {kk, Length[MR$IndexList]}];
];


(* ::Subsection::Closed:: *)
(*WriteFAParticleDefinition*)


WriteFAParticleDefinition[outfile_] := Block[{},
      WriteString[outfile, "(*     Declared particles    *)\n"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "M$ClassesDescription = {\n"];
      Do[WriteString[outfile, ToString[FA$ClassesList[[kk]], InputForm], " == {\n"];
         Do[WriteString[outfile, "    ", ToString[FA$ClassesDescription[FA$ClassesList[[kk]]][[ll]], InputForm], ",\n"],
            {ll, Length[FA$ClassesDescription[FA$ClassesList[[kk]]]] - 1}];
         WriteString[outfile, "    ", ToString[Last[FA$ClassesDescription[FA$ClassesList[[kk]]]], InputForm], " },\n"];
         WriteString[outfile, "\n"],
         {kk, Length[FA$ClassesList] - 1}];
      WriteString[outfile, ToString[Last[FA$ClassesList], InputForm], " == {\n"];
      Do[WriteString[outfile, "    ", ToString[FA$ClassesDescription[Last[FA$ClassesList]][[ll]], InputForm], ",\n"],
            {ll, Length[FA$ClassesDescription[Last[FA$ClassesList]]] - 1}];
         WriteString[outfile, "    ", ToString[Last[FA$ClassesDescription[Last[FA$ClassesList]]], InputForm], " }\n"];
      WriteString[outfile, "}\n"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "\n"];
];


(* ::Subsection::Closed:: *)
(*WriteFAGaugeXi*)


WriteFAGaugeXi[outfile_, tempxi_] := Block[{},
      WriteString[outfile, "(*        Definitions       *)\n"];    
      WriteString[outfile, "\n"];  
      (* Write GaugeXi *)
      Do[WriteString[outfile, "GaugeXi[ ", ToString[tempxi[[kk, 1]]], " ] = ", ToString[tempxi[[kk, 2]]], ";\n"], {kk, Length[tempxi]}];
      WriteString[outfile, "\n"];
];


(* ::Subsection::Closed:: *)
(*WriteFATheMass*)


WriteFATheMass[outfile_] := Block[{tempclass, Massdone = {}},
     (* Write TheMass *)
      Do[tempclass = FA$ClassesList[[kk]];
         If[Not[UnphysicalQ[tempclass /. FA$ClassToName]],
           If[(Head[FA$MassMembers[tempclass]] === List) && (FA$Mass[tempclass] =!= 0),
              Do[WriteString[outfile, ToString[FA$Mass[tempclass]], "[ ", Sequence @@ (ToString /@ AddCommas[MakeFABlankList[FA$FlavPos[tempclass], ll, tempclass]]), " ] := ", ToString[FA$MassMembers[tempclass][[ll]]], ";\n"];
                 If[(NInd[tempclass /. FA$ClassToName] - LorIndNumber[tempclass /. FA$ClassToName])!=1, WriteString[outfile, ToString[FA$Mass[tempclass]], "[ ", ToString[ll], " ] := ", ToString[FA$MassMembers[tempclass][[ll]]], ";\n"]],
                 {ll, Length[FA$MassMembers[tempclass]]}],
              If[(FA$Mass[tempclass] =!= 0) && (Not[MemberQ[Massdone, FA$Mass[tempclass]]]), Massdone = Append[Massdone, FA$Mass[tempclass]]; WriteString[outfile, ToString[FA$Mass[tempclass]], "[ ___ ] := ", ToString[FA$Mass[tempclass]], ";\n"]]]],
         {kk, Length[FA$ClassesList] - 1}];
      If[Not[UnphysicalQ[Last[FA$ClassesList] /. FA$ClassToName]],
      If[(Head[FA$MassMembers[Last[FA$ClassesList]]] === List) && (FA$Mass[tempclass] =!= 0),
            Do[WriteString[outfile, ToString[FA$Mass[Last[FA$ClassesList]]], "[ ", Sequence @@ (ToString /@ AddCommas[MakeFABlankList[FA$FlavPos[Last[FA$ClassesList]], ll, Last[FA$ClassesList]]]), " ] := ", ToString[FA$MassMembers[Last[FA$ClassesList]][[ll]]], "\n"];
               If[(NInd[Last[FA$ClassesList] /. FA$ClassToName] - LorIndNumber[Last[FA$ClassesList] /. FA$ClassToName])!=1, WriteString[outfile, ToString[FA$Mass[Last[FA$ClassesList]]], "[ ", ToString[ll], " ] := ", ToString[FA$MassMembers[Last[FA$ClassesList]][[ll]]], "\n"]],
               {ll, Length[FA$MassMembers[Last[FA$ClassesList]]]}],
            If[(FA$Mass[Last[FA$ClassesList]] =!= 0) && (Not[MemberQ[Massdone, FA$Mass[Last[FA$ClassesList]]]]), Massdone = Append[Massdone, FA$Mass[Last[FA$ClassesList]]]; WriteString[outfile, ToString[FA$Mass[Last[FA$ClassesList]]], "[ ___ ] := ", ToString[FA$Mass[Last[FA$ClassesList]]], ";\n"]]]];
      WriteString[outfile, "\n"];
      WriteString[outfile, "\n"];
];


(* ::Subsection::Closed:: *)
(*WriteFATheLabel*)


WriteFATheLabel[outfile_] := Block[{tempclass},
     (* Write TheLabel *)
      Do[tempclass = FA$ClassesList[[kk]];
         If[Head[FA$PropLabelMembers[tempclass]] === List,
            Do[WriteString[outfile,"TheLabel[ ", ToString[MergeFAClassToBlank[tempclass, MakeFABlankList[FA$FlavPos[tempclass], ll, tempclass]]], " ] := \"", ToString[FA$PropLabelMembers[tempclass][[ll]]], "\";\n"];
               If[(NInd[tempclass /. FA$ClassToName] - LorIndNumber[tempclass /. FA$ClassToName])!=1, WriteString[outfile,"TheLabel[ ", ToString[MergeFAClassToBlank[tempclass, {ll}]], " ] := \"", ToString[FA$PropLabelMembers[tempclass][[ll]]], "\";\n"]],
               {ll, Length[FA$PropLabelMembers[tempclass]]}],
            If[Length[DeleteCases[$IndList[tempclass /. FA$ClassToName], Index[Spin] | Index[Lorentz]]] != 0,
               WriteString[outfile,"TheLabel[ ", StringInsert[ToString[tempclass], ", {__}", -2], " ] := TheLabel[", ToString[tempclass], "];\n"]]],
         {kk, Length[FA$ClassesList] - 1}];
      If[Head[FA$PropLabelMembers[Last[FA$ClassesList]]] === List,
            Do[WriteString[outfile,"TheLabel[ ", ToString[MergeFAClassToBlank[Last[FA$ClassesList], MakeFABlankList[FA$FlavPos[Last[FA$ClassesList]], ll, Last[FA$ClassesList]]]], " ] := \"", ToString[FA$PropLabelMembers[Last[FA$ClassesList]][[ll]]], "\"\n"];
               If[(NInd[Last[FA$ClassesList] /. FA$ClassToName] - LorIndNumber[Last[FA$ClassesList] /. FA$ClassToName])!=1, WriteString[outfile,"TheLabel[ ", ToString[MergeFAClassToBlank[Last[FA$ClassesList], {ll}]], " ] := \"", ToString[FA$PropLabelMembers[Last[FA$ClassesList]][[ll]]], "\"\n"]],
               {ll, Length[FA$PropLabelMembers[Last[FA$ClassesList]]]}],
            If[Length[DeleteCases[$IndList[Last[FA$ClassesList] /. FA$ClassToName], Index[Spin] | Index[Lorentz]]] != 0,
               WriteString[outfile,"TheLabel[ ", StringInsert[ToString[Last[FA$ClassesList]], ", {__}", -2], " ] := TheLabel[", ToString[Last[FA$ClassesList]], "];\n"]]];
      WriteString[outfile, "\n"];
      WriteString[outfile, "\n"];
];


(* ::Subsection::Closed:: *)
(*WriteFACouplings*)


WriteFACouplings[outfile_, vertices_] := Block[{},
      (*                        *)
      (*    Write couplings     *)
      (*                        *)
      WriteString[outfile, "(*      Couplings (calculated by FeynRules)      *)\n"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "M$CouplingMatrices = {\n"];
      WriteString[outfile, "\n"];


      Do[WriteString[outfile, "C[ "];
         Do[WriteString[outfile, ToString[vertices[[ll, 1, mm]], InputForm], " , "],
            {mm, Length[vertices[[ll, 1]]] - 1}];
         WriteString[outfile, ToString[Last[vertices[[ll, 1]]], InputForm]];
         WriteString[outfile, " ] == ", ToString[vertices[[ll, 2]], InputForm], ",\n"];
         WriteString[outfile, "\n"],
         {ll, Length[vertices] - 1}];

      WriteString[outfile, "C[ "];
      Do[WriteString[outfile, ToString[Last[vertices][[1, mm]], InputForm], " , "],
         {mm, Length[Last[vertices][[1]]] - 1}];
      WriteString[outfile, ToString[Last[Last[vertices][[1]]], InputForm]];
      WriteString[outfile, " ] == ", ToString[Last[vertices][[2]], InputForm], "\n"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "}"];

      WriteString[outfile, "\n"];
      WriteString[outfile, "\n"];

];


WriteFACouplingDefinitions[outfile_] := Block[{},

      WriteString[outfile, "(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)\n"];
      WriteString[outfile, "\n"];
      WriteString[outfile, "(* Parameter replacement lists (These lists were created by FeynRules) *)\n"];
      WriteString[outfile, "\n"];
  	WriteString[outfile, "(* FA Couplings *)\n"];
      WriteString[outfile, "\n"];
  	WriteString[outfile, "M$FACouplings = {\n"];
      If[M$FACouplings =!= {}, WriteParamList[outfile, M$FACouplings], WriteString[outfile,"};"]];
      WriteString[outfile, "\n"];
	  WriteString[outfile, "\n"];

];


(* ::Section:: *)
(*WriteFeynArtsOutput*)


(* ::Subsection:: *)
(*Options*)


Options[WriteFeynArtsOutput] = {FlavorExpand -> False, IndexExpand-> False, MaxParticles -> Automatic, MinParticles -> Automatic, 
          MaxCanonicalDimension -> Automatic, MinCanonicalDimension -> Automatic, SelectParticles -> Automatic, Output :> StringReplace[M$ModelName <> "_FA",{" "-> "_"}],CouplingRename->True,
          Exclude4Scalars -> False, GenericFile -> True, DiracIndices->Automatic, LoopOrder->MR$Null, ApplyMomCons->True};


(* ::Subsection:: *)
(*WriteFeynArtsOutput*)


WriteFeynArtsOutput[lags__, options__] := WriteFeynArtsOutput[{lags}, options] /; (And @@ ((Head[#] =!= Rule &) /@ {lags})) && (And @@ ((Head[#] === Rule &) /@ {options})) && (And @@ ((Head[#]=!=List&) /@ {lags}));


WriteFeynArtsOutput[lags__] := WriteFeynArtsOutput[{lags}] /; (And @@ ((Head[#] =!= Rule &) /@ {lags}))  && (And @@ ((Head[#]=!=List&) /@ {lags}));


WriteFeynArtsOutput[{lags__}, options___] := Block[{vertlength,verttype,tempclass, vertlistFA,genFileOpt,diracIndOpt,couplRenameOpt,j,vertN=1, 
													WFAflavexp, defname,outfile, genfile, temp, tempoutfile, tempoptions, tempxi, Rulli, 
													Massdone = {}, vertexlistFA, FlatDot, LoopOpt,CTlags,orderOpt,MyPat,mylags = {lags},CTcnt=0,tmp,tmppara,wfoit},
      Print[" - - - FeynRules interface to FeynArts - - -"];
      Print["      C. Degrande C. Duhr, 2013"];
      Print["      Counterterms: B. Fuks, 2012"];

      (* ***** Set Options ****** *)

      FA$FlavExpCheck = True;
      If[FlavorExpand/.{options},FA$FlavExpCheck = False;];

	  (*Rename?*)
	  couplRenameOpt=CouplingRename/.{options}/.Options[WriteFeynArtsOutput];
      genFileOpt=GenericFile/.{options}/.Options[WriteFeynArtsOutput];
      diracIndOpt=DiracIndices/.{options}/.Options[WriteFeynArtsOutput];

      (* BEGIN Benj: Generating counterterms *)
      orderOpt=LoopOrder/.{options}/.Options[WriteFeynArtsOutput];
      LoopOpt=If[orderOpt=!=MR$Null,True,False];
      If[LoopOpt,
        FR$Loop=True; 
        FR$LoopOrder=If[MatrixQ[orderOpt],Plus@@orderOpt[[All,2]],orderOpt[[2]]];
        CTlags=Block[{},CTcnt++;Print["Turning to Lagrangian "<>ToString[CTcnt]<>"..."]; (ExtractCounterterms[#,orderOpt]-#)]&/@mylags;
        CTlags=(Plus@@(OptimizeIndex/@(List@@#)))&/@CTlags;
        CTlags=CTlags/.(M$DeltaToParameters/.RuleDelayed[a_[orders__],b_]:>MyRule[b/.ids->MyPat[ids,__],b FR$LoopDum^Plus@orders]/.MyRule->Rule/.MyPat->Pattern);
       mylags=mylags + CTlags;
        FR$Loop=False;,
        FR$LoopOrder=0;];      
      (* END Benj *)


	  (* bookeeper for the lorentz structure *)
	  fermionCounter = 0;
	  If[diracIndOpt,fermionCounter = 3];
	  structurelistFA ={};


(*********************  Output Directory *******************************)

      tempoutfile = Output /. {options} /. Options[WriteFeynArtsOutput];

      If[Not[MemberQ[FileNames[], tempoutfile]],
         Print["Creating output directory: ", tempoutfile];
         CreateDirectory[tempoutfile];
      ];
      SetDirectory[tempoutfile];

      outfile = tempoutfile <> ".mod";

(*********************  FeynmanRule computation ***********************)


FR$FeynArtsInterface = True;

      (*Treating options list*)
      tempoptions = DeleteCases[DeleteCases[{options}, Name -> _], (Output -> _)];
      (* Getting started *)      
      defname = StringJoin["L", ToString[#]]& /@ Range[Length[mylags]];
      $lagrangianListtemp = {};
      WFAflavexp = FlavorExpand /. tempoptions /. Options[WriteFeynArtsOutput];

      (* Compute the Feynman rules *)

      If[LoopOpt, FR$Loop=True];
      Do[Print["Calculating Feynman rules for ", defname[[kmg]]];
         If[FA$FlavExpCheck, 
            FeynmanRules[mylags[[kmg]], Sequence @@ FilterRules[tempoptions,Options[FeynmanRules]], Name -> defname[[kmg]], ScreenOutput->False]],
         {kmg, Length[defname]}];
      If[LoopOpt, FR$Loop=False];


     (* Check if classes need to be redefined, if so do it *)
      If[Not[FA$FlavExpCheck],
         Print["Redefining classes in such a way that each particle is in a separate FeynArts class."];
         FARedefineClasses;
         Print["Restarting Feynman rule calculation, setting FlavorExpand -> True."];
         $lagrangianListtemp = {};

         If[LoopOpt, FR$Loop=True];
         Do[Print["Calculating Feynman rules for ", defname[[kmg]]];
         FeynmanRules[mylags[[kmg]], Sequence @@ DeleteCases[FilterRules[tempoptions,Options[FeynmanRules]],FlavorExpand->_], Name -> defname[[kmg]], ScreenOutput -> False, FlavorExpand -> True],         
{         kmg, Length[defname]}]];

         If[LoopOpt, FR$Loop=False];



FR$FeynArtsInterface = False;


      (* Merge vertices together *)
         vertexlistFA = MergeVertices @@ Table[Vertices[kmg],{kmg, defname}];
         If[LoopOpt,vertexlistFA = KillBil/@vertexlistFA];
         vertexlistFA = vertexlistFA/.T[Index[Gluon, gl_], Index[Colour, Ext[a_]], Index[Colour, Ext[c_]]]*T[Index[Gluon, gl_], Index[Colour, Ext[b_]], Index[Colour, Ext[d_]]]-> 
           1/2( IndexDelta[Index[Colour, Ext[a]],Index[Colour, Ext[d]]]IndexDelta[Index[Colour, Ext[b]],Index[Colour, Ext[c]]]-1/3  IndexDelta[Index[Colour, Ext[a]],Index[Colour, Ext[c]]]IndexDelta[Index[Colour, Ext[b]],Index[Colour, Ext[d]]]);
      
      (* Transform them into FA format *)
      vertexlistFA = CreateFAVertexList /@ vertexlistFA;
      vertexlistFA = DeleteCases[vertexlistFA,{a_,b_,0,c_},{1}];

(********************* Processing ***********************)

(*   The vertices are now computed, we now process them to 
     1) extract the generic coupling file
   or
     2) only allow for Lorentz.gen couplings
*)   

     If[genFileOpt,
        SetAttributes[FlatDot,Flat];

		IndiceCounter=0;
		FlatDot[aa___,SlashedP[bb_],cc___][r_,s_]:= Module[{ind},			
			IndiceCounter++; 
			ind=ToExpression["FVSInd"<>ToString[IndiceCounter]];
			FV[bb,Index[Lorentz,ind]](FlatDot[aa,Ga[Index[Lorentz,ind]],cc][r,s])];

		FlatDot[dd___,Ga[Index[Lorentz,aa__]],Ga[Index[Lorentz,bb__]],Ga[Index[Lorentz,cc__]],ee___][r_,s_]:=Module[{ind},
			IndiceCounter++; 
			ind=ToExpression["SummedInd"<>ToString[IndiceCounter]];
			ME[Index[Lorentz,aa],Index[Lorentz,bb]]FlatDot[dd,Ga[Index[Lorentz,cc]],ee][r,s]- 
			ME[Index[Lorentz,aa],Index[Lorentz,cc]]FlatDot[dd,Ga[Index[Lorentz,bb]],ee][r,s]+
			ME[Index[Lorentz,cc],Index[Lorentz,bb]]FlatDot[dd,Ga[Index[Lorentz,aa]],ee][r,s]+
			I Eps[Index[Lorentz,aa],Index[Lorentz,bb],Index[Lorentz,cc],Index[Lorentz,ind]]FlatDot[dd,Ga[Index[Lorentz,ind]],Ga[5],ee][r,s]];

        vertexlistFA = Replace[vertexlistFA,{TensDot[aa___,Sig[mu_,nu_],bb___]->I/2 TensDot[aa,Ga[mu].Ga[nu],bb]-I/2TensDot[aa,Ga[mu].Ga[nu],bb],
                               Sig[mu_,nu_,ss1_,ss2_]->I/2 TensDot[Ga[mu].Ga[nu]][ss1,ss2]-I/2TensDot[Ga[nu].Ga[mu]][ss1,ss2]},\[Infinity],Heads->True];
        vertexlistFA = Replace[vertexlistFA,TensDot[0][ss1_,ss2_]->0,\[Infinity],Heads->True];

		vertexlistFA=LorentzContract[vertexlistFA]/.{TensDot[aa__][Index[Spin,ss1_],Index[Spin,ss2_]]->(TensDot[aa,ProjP][Index[Spin,ss1],Index[Spin,ss2]]+
			CSPRL*TensDot[aa,ProjM][Index[Spin,ss1],Index[Spin,ss2]]),
			Ga[aa_,ss1_,ss2_]->(TensDot[Ga[aa],ProjP][ss1,ss2]+CSPRL*TensDot[Ga[aa],ProjM][ss1,ss2]),
			SlashedP[aa_,ss1_,ss2_]->(TensDot[SlashedP[aa],ProjP][ss1,ss2]+CSPRL*TensDot[SlashedP[aa],ProjM][ss1,ss2])}/.TensDot->FlatDot/.FlatDot->TensDot;
		vertexlistFA = vertexlistFA/.TensDot->FlatDot/.FlatDot->TensDot;
        vertexlistFA = OrderEps[LorentzContract[ExpandAll[vertexlistFA]]//.{Times[aa___,Eps[bb___,dd_,cc___],ee___,FV[ff_,dd_],hh___]->Times[aa,Eps[bb,FV[ff],cc],ee,hh]}];
  	  vertexlistFA = ExpandAll[vertexlistFA]/.FV[bbb_,cc_]TensDot[eee___,Ga[cc_],fff___][ss1_,ss2_]:>TensDot[eee,SlashedP[bbb],fff][ss1,ss2];
 
        FAIDStructure @@@ vertexlistFA;

		structurelistFA = LSymmetrize @@@ structurelistFA;

        If[FR$DoPara, 
        SetSharedFunction[FAStructure2];SetSharedVariable[structurelistFA];
        DistributeDefinitions[FR$LoopOrder];
        tmp=Table[tmppara=vertexlistFA[[wfoit]];ParallelSubmit[{wfoit,tmppara},FAStructure2 @@ tmppara],{wfoit,Length[vertexlistFA]}];
        vertexlistFA=WaitAll[tmp]/.CSPRL->1;
        (*Split the tree-level and counterterms and put them as two different entries*)
        tmp=Table[tmppara=vertexlistFA[[wfoit]];
          ParallelSubmit[{wfoit,tmppara},({#1,Join[Simplify[Coefficient[#2,FR$CT,0],TimeConstraint->0.01],Coefficient[#2,FR$CT,1],2]}&) @@ tmppara],
          {wfoit,Length[vertexlistFA]}];
        vertexlistFA=WaitAll[tmp];
        (*Remove the tree-level component for the vertices with two legs or less*)
        tmp=Table[tmppara=vertexlistFA[[wfoit]];
          ParallelSubmit[{wfoit,tmppara},(If[Length[#1]>2,{#1,#2},{#1,Transpose[{#2[[All,1]]*0,#2[[All,2]]}]}]&) @@ tmppara],
          {wfoit,Length[vertexlistFA]}];
        vertexlistFA=WaitAll[tmp];
        ,(*FR$DoPara=False*)
		vertexlistFA = (FAStructure2 @@@ vertexlistFA)/.CSPRL->1;

        (*Split the tree-level and counterterms and put them as two different entries*)
        vertexlistFA = ({#1,Join[Simplify[Coefficient[#2,FR$CT,0],TimeConstraint->0.01],Coefficient[#2,FR$CT,1],2]}&)@@@vertexlistFA;
        (*Remove the tree-level component for the vertices with two legs or less*)
        vertexlistFA = (If[Length[#1]>2,{#1,#2},{#1,Transpose[{#2[[All,1]]*0,#2[[All,2]]}]}]&)@@@vertexlistFA;
        ];(*end if FR$DoPara*)

        vertexlistFA = DeleteCases[vertexlistFA,{a_,_?(Union[#]==={{0,0}}&)}];

        vertexlistFA = DeleteCases[vertexlistFA, {_,_,_, 0 ,___}|{_,_,_,{0,0},___}];,
        (* else *)
Print["mytimecheck,before LGC"];

        If[FR$DoPara,      
          (*SetSharedVariable[vertexlistFA];*)
          DistributeDefinitions[LorentzGenCouplings];
          tmp=Table[tmppara=vertexlistFA[[wfoit]];ParallelSubmit[{wfoit,tmppara},LorentzGenCouplings @@ tmppara],{wfoit,Length[vertexlistFA]}];
          vertexlistFA=WaitAll[tmp];,
          vertexlistFA = LorentzGenCouplings @@@ vertexlistFA;];
Print["mytimecheck,after 1 LGC"];

        (*Split the tree-level and counterterms and put them as two different entries*)
        vertexlistFA = ({#1,Join[Simplify[Coefficient[#2,FR$CT,0],TimeConstraint->1],Coefficient[#2,FR$CT,1],2]}&)@@@vertexlistFA;
        (*Remove the tree-level component for the vertices with two legs or less*)
        vertexlistFA = (If[Length[#1]>2,{#1,#2},{#1,Transpose[{#2[[All,1]]*0,#2[[All,2]]}]}]&)@@@vertexlistFA;
        vertexlistFA = DeleteCases[vertexlistFA,{a_,_?(Union[#]==={{0,0}}&)}];
		];

Print["mytimecheck,after LGC"];
       (* Coupling renaming *)

	  If[couplRenameOpt === True,
         M$FACouplings={};
         vertexlistFA = FACouplingRenaming[vertexlistFA];
	    ];

      (* ******************** *)
      (* The GaugeXi function *)

      tempxi = DeleteCases[Rulli[#,GaugeXi[Name2Field[#/.(Reverse/@FA$MemberToClass)]]]&/@ FA$ClassesList,Rulli[x_,GaugeXi[x_]]];


(* **********   Writing the .mod file  ********** *)   

Print["Writing FeynArts model file into directory ", tempoutfile];  

      OpenWrite[outfile];
      WriteFAHeader[outfile];
      WriteFAIndexDeclaration[outfile];
      WriteFAParticleDefinition[outfile];
      WriteFAGaugeXi[outfile, tempxi];
      WriteFATheMass[outfile];
      WriteFATheLabel[outfile];
      WriteFACouplings[outfile, vertexlistFA];
      If[couplRenameOpt, WriteFACouplingDefinitions[outfile]];
      Close[outfile];



(* **********   Writing the parameter file file  ********** *)  

FR$FeynArtsInterface = True;  

      WriteParameters[NumericalOnly -> True, ScreenOutput -> False, Output -> StringTake[outfile,{1,StringLength[outfile]-4}]<>".pars"];

FR$FeynArtsInterface = False;
 
      (* here will come the writing of the .h files *)

(* **********   Writing the parameter file file  ********** *)
	  
  	If[genFileOpt,
   	  genfile = StringTake[outfile,{1,StringLength[outfile]-4}]<>".gen";
 
   	  If[diracIndOpt == Automatic,
            diracIndOpt = (fermionCounter>2)];

	     If[(diracIndOpt == False) && (fermionCounter>2),
            Message[FAInt::Dirac]];

	     Print["Writing FeynArts generic file on ", genfile, "."];
	  
	     OpenWrite[genfile];
         WriteKinIndices[genfile,diracIndOpt];
	     WriteSimpRules[genfile];
         WritePropagators[genfile,diracIndOpt];
	     WriteGenCouplings[genfile,diracIndOpt];
	     WriteFlippingRules[genfile,diracIndOpt];
         WriteTruncation[genfile];
	     WriteLast[genfile];
         Close[genfile];
	    ];

      (* Reset Directory, then exit *)
      ResetDirectory[];

];




(* ::Section:: *)
(*FARedefineClasses*)


(*MakeFAFlavorClass[flav_, cls_] := ToExpression[StringInsert[ToString[cls], ToString[flav], 3]];*)

MakeFAFlavorClass[flav_, cls_[nn_]] := cls[Which[cls === F, FA$FermionClassNumber++,
                                                 cls === V, FA$VectorClassNumber++,
                                                 cls === S, FA$ScalarClassNumber++,
                                                 cls === U, FA$GhostClassNumber++,
                                                 cls === T, FA$TensorClassNumber++]];

FARedefineClasses:=Block[{tmp,tmpmass,tmpind,tmppl,tmpelem,tmpsymb},
   (* Saving the old classes *)
   FA$ClassesListOld=FA$ClassesList;
   FA$MemberToClassOld=FA$MemberToClass;
   FA$ClassToNameOld=FA$ClassToName;
   FA$ClassesDescriptionOld=Table[FA$ClassesList[[cls]]->FA$ClassesDescription[FA$ClassesList[[cls]]],{cls,Length[FA$ClassesList]}];
   (* Create new classes *)
   FA$FermionClassNumber = 1;
   FA$VectorClassNumber = 1;
   FA$ScalarClassNumber = 1;
   FA$GhostClassNumber = 1;
   FA$TensorClassNumber = 1;
   tmp=({#1,MakeFAFlavorClass[#1,#2]}&)@@@FA$MemberToClass;
   Do[MR$Class[tmp[[cls,1]]] = tmp[[cls,2]], {cls, Length[tmp]}];
   FA$MemberToClass=Rule@@@tmp;
   FA$ClassToName=FA$MemberToClass/.Rule[xx_,yy_]:>Rule[yy,xx];
   FA$ClassesList=FA$MemberToClass/.Rule[xx_,yy_]:>yy;
   (* Create new classes descriptions *)
   Do[tmpelem=FA$ClassesList[[cls]];
      tmpsymb=tmpelem/.FA$ClassToName;
      tmp=tmpelem/.FA$ClassToName/.FA$MemberToClassOld/.FA$ClassesDescriptionOld;
      tmpmass=Mass[tmpsymb];
      FA$Mass[tmpelem]=tmpmass;
      tmp=Append[DeleteCases[tmp,Rule[Mass,_]], Rule[Mass, tmpmass]];
      tmpind=DeleteCases[$IndList[tmpsymb],Index[Lorentz]|Index[Spin]];
      tmp=Append[DeleteCases[tmp,Rule[Indices,_]], Rule[Indices, tmpind]];
      tmppl=ToString[tmpsymb];
      tmp=Append[DeleteCases[tmp,Rule[PropagatorLabel,_]], Rule[PropagatorLabel, tmppl]];
      FA$ClassesDescription[tmpelem]=tmp,
      {cls,Length[FA$ClassesList]}];
   FA$MassMembers = MassList[[2, All, 2]];
   Clear[FA$PropLabelMembers];
   FA$FlavorNumber[_]:=1;
   Print["See NewFeynArtsClasses[] for more information on the new classes"];
   NewFeynArtsClasses[] := FA$MemberToClass]


(* ::Section:: *)
(*FeynArts generic coupling *)


(* ::Subsection:: *)
(*FAIDStructure*)


FAIDStructure[vertextype_, FAPartContent_, vertex_, fc_] := Module[{temp,temp2,temp3,temp4,DelConst,Contraction},

	DelConst[x_]:=(Times@@Cases[x,_?(Not[FreeQ[#,Spin]&&FreeQ[#,Lorentz]&&FreeQ[#,SP]]&)]);

	Contraction[x_]:= Module[{listI,listP,listR},
		listI = Union[Cases[x, Index[a_,Except[Ext[__]],___], \[Infinity],Heads->True]];
		If[Length[listI]>26,Print["Warning : not all the kinematic indices are properly summed"]];
		listP = Pattern[#,Blank[]]&/@(ToExpression[(*"a"<>ToString[#]*)FromCharacterCode[#+96]]&/@Range[Min[Length[listI],26]]);
		listR = Table[Rule[listI[[kk]],listP[[kk]]],{kk,Length[listI]}];
		x/.listR
	];(*end module contraction*)

	If[FreeQ[structurelistFA,vertextype],
		temp = Expand[vertex];
		If[Head[temp]===Plus,temp=List@@temp;,temp=List[temp];];
		temp = DelConst/@temp;
		fermionCounter = Max[fermionCounter,StringCount[vertextype,"F"]];
		temp = Contraction/@temp;
		temp=Union[temp];
		structurelistFA=Union[structurelistFA,{{vertextype,temp}}];
	,temp3 = Position[structurelistFA,vertextype][[1,1]];
		temp = Expand[vertex];
		If[Head[temp]===Plus,temp=List@@temp;,temp=List[temp];];
		temp = DelConst/@temp;
		temp = Contraction/@temp;
		structurelistFA[[temp3,2]]=Join[structurelistFA[[temp3,2]],Complement[temp,structurelistFA[[temp3,2]],SameTest->(MatchQ[#1,#2]&)]];
	];(*end If*)
;
];(*end Module*)


(* ::Subsection:: *)
(*FAStructure2*)


FAStructure2[vertextype_, FAPartContent_, vertex_, fc_] := Module[{temp, ReturnConst, $HCHELAS, tempFApc, output,temp2},
	temp = vertex /. {T[aa_, ii_, jj_] -> SUNT[aa, ii, jj], T[aa_] -> SUNT[aa], f[aa_, bb_, cc_] -> SUNF[aa,bb,cc], ee -> EL, gs -> GS};
	temp = Expand[temp/.{SUNF[Aaa__,Bbb_,Ccc_]SUNT[Ccc_,Iii_,Jjj_]->-I(SUNT[Aaa,Bbb,Iii,Jjj]-SUNT[Bbb,Aaa,Iii,Jjj])}];
	temp = temp//.{SUNT[Aaa__,Index[Colour,Iii_],Index[Colour,Jjj_]]SUNT[Bbb__,Index[Colour,Jjj_],Index[Colour,Kkk_]]->SUNT[Aaa,Bbb,Index[Colour,Iii],Index[Colour,Kkk]]};
	temp = Expand[temp];

	ReturnConst[pat_] := Module[{epsind,perm,xx,kk,pos,remain,sign,epscoef,ll,newpat,newsign,res,aa,bb,cc,dd},
                           If[FreeQ[pat,Eps],
                             If[MatchQ[pat,Times[a___,-1,b___]],
							   If[Head[temp]===Plus, 
								 temp2=Total[Cases[temp,x_?(FreeQ[#,Spin]&&FreeQ[#,Lorentz]&&FreeQ[#,SP]&)*-pat->-x]]; 
                                  Join[{temp2/.FR$LoopDum->0},If[LoopOrder===0,{},Table[Coefficient[temp2,FR$LoopDum^ii],{ii,FR$LoopOrder}]]],
								  temp2=Total[Cases[{temp},x_?(FreeQ[#,Spin]&&FreeQ[#,Lorentz]&&FreeQ[#,SP]&)*-pat->-x]];
                                  Join[{temp2/.FR$LoopDum->0},If[LoopOrder===0,{},Table[Coefficient[temp2,FR$LoopDum^ii],{ii,FR$LoopOrder}]]]],
							   If[Head[temp]===Plus,
								 temp2=Total[Cases[temp,x_?(FreeQ[#,Spin]&&FreeQ[#,Lorentz]&&FreeQ[#,SP]&)*pat->x]];
                                 Join[{temp2/.FR$LoopDum->0},If[LoopOrder===0,{},Table[Coefficient[temp2,FR$LoopDum^ii],{ii,FR$LoopOrder}]]],
								 temp2=Total[Cases[{temp},x_?(FreeQ[#,Spin]&&FreeQ[#,Lorentz]&&FreeQ[#,SP]&)*pat->x]];
                                 Join[{temp2/.FR$LoopDum->0},If[LoopOrder===0,{},Table[Coefficient[temp2,FR$LoopDum^ii],{ii,FR$LoopOrder}]]]]
                               ],
                               If[Not[Head[temp]===Plus],temp={temp}];
                               epsind=Cases[pat,Eps[aa_,bb_,cc_,dd_]->{aa,bb,cc,dd},{0,\[Infinity]}];
                               epscoef=pat/.Eps[__]->1;
                               perm=Permutations/@epsind;
                               sign=Times@@Signature/@epsind;
                               res=0;
                               For[kk=1,kk<=24^Length[epsind],kk++,
                                  pos=Table[0,{Length[epsind]}];
                                  remain=kk-1;
                                  For[ll=1,ll<=Length[pos],ll++,pos[[ll]]=Quotient[remain,24^(Length[pos]-ll)]+1;remain=Mod[remain,24^(Length[pos]-ll)];];
                                  newpat=epscoef*Times@@Table[Eps[Sequence@@perm[[ll,pos[[ll]]]]],{ll,1,Length[pos]}];
                                  newsign=Times@@Table[Signature[perm[[ll,pos[[ll]]]]],{ll,1,Length[pos]}]*sign;
                                  res=res+Total[Cases[temp,yx_?(FreeQ[#,Spin]&&FreeQ[#,Lorentz]&&FreeQ[#,SP]&)*newpat->yx]]*newsign;
If[Length[Cases[temp,yx_?(FreeQ[#,Spin]&&FreeQ[#,Lorentz]&&FreeQ[#,SP]&)*newpat->yx]]>0&&kk>1,Print["test"];Print[pat];Print[ res];];
                               ];
                               {res}   
						   ]
                         ];

	temp = ReturnConst/@(structurelistFA[[Position[structurelistFA,vertextype][[1,1]],2]]);
	(*copy & paste from Claude code !!! *)
	temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
    temp = temp //. MR$Definitions;
    temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
    temp = temp //. $HCHELAS -> HC;
    temp = MakeMatrixChain[temp];
    temp = temp //. {ee -> EL, gs -> GS} //. ParamRules;
    temp = temp //. Index[name_, Ext[i__]] :> MakeFAExtIndex[Index[name, Ext[i]], fc,FAPartContent];
    temp = temp //. Index[name_, k__] :> MakeFAIntIndex[Index[name, k]];
    If[Not[FreeQ[temp, Index[_, _?(Not[NumericQ[#]]&)]]], temp = MakeIndexSum[temp]];
    temp = temp /. {Index[_, k_] -> k};
    Off[Simplify::time];
    temp = If[$VersionNumber>8,Factor[Expand[temp]],Simplify[Expand[temp],TimeConstraint->0.001]];
    On[Simplify::time];
    tempFApc = First /@ FAPartContent;
    output = {tempFApc, temp}
];


(* ::Subsection:: *)
(*LorentzGenCouplings*)


(* ::Text:: *)
(*LorentzGenCouplings   write a generic file corresponding to Lorentz.gen*)


LorentzGenCouplings[vertextype_, FAPartContent_, vertex_, fc_] := Block[{temp, temp1, temp2, temp3, output, $HCHELAS, tempFApc, done, deltflavind, GaAlgebraDone},   
  Print["mytimecheck, LGC1"];
  temp = vertex; //. IndexDelta[Index[Spin, s_], Index[Spin, r_]] -> 1;

      (* We have to rename the color matrix T into SUNT for FormCalc to run properly.*)
      temp = temp /. {T[aa_, ii_, jj_] -> SUNT[aa, ii, jj], T[aa_] -> SUNT[aa], f[aa_, bb_, cc_] -> SUNF[aa,bb,cc], ee -> EL, gs -> GS};
      temp = Expand[temp]//.{SUNT[Aaa__,Index[Colour,Iii_],Index[Colour,Jjj_]]SUNT[Bbb__,Index[Colour,Jjj_],Index[Colour,Kkk_]]->SUNT[Aaa,Bbb,Index[Colour,Iii],Index[Colour,Kkk]]};

(* 
   Here comes the filter, that will check whether the vertices are compliant with the Loretnz.gen structure.
   If so, the vertex is kept, if not it is discarded.
*)


      Which[(******* Fermion couplings: FF *********)
            (vertextype === "FF"), 
                (*Proj[n,1]=SlashedP[n].P-,Proj[n,2]=SlashedP[n].P+,Proj[1]=P-,Proj[2]=P+*)
                temp = temp/.{SlashedP[1,__]->Proj[1,1]-Proj[2,2],TensDot[SlashedP[1],Ga[5]][__]->-Proj[1,1]-Proj[2,2],
                              SlashedP[2,__]->-Proj[1,1]+Proj[2,2],TensDot[SlashedP[2],Ga[5]][__]->Proj[1,1]+Proj[2,2],
                              TensDot[SlashedP[1],ProjM][__]->Proj[1,1],TensDot[SlashedP[1],ProjP][__]->-Proj[2,2],
                              TensDot[SlashedP[2],ProjM][__]->-Proj[1,1],TensDot[SlashedP[2],ProjP][__]->Proj[2,2],
                              IndexDelta[__?(Not[FreeQ[#,Spin]]&)]->Proj[1]+Proj[2],Ga[5,__]->-Proj[1]+Proj[2],
                              ProjM[__]->Proj[1],ProjP[__]->Proj[2]};
                temp = {{Coefficient[temp, Proj[1,1]]}, {Coefficient[temp, Proj[2,2]]},{Coefficient[temp, Proj[1]]}, {Coefficient[temp, Proj[2]]}} //. GaAlgebra[___] -> 1,(******* Fermion couplings: FFV and FFS *********)
            (vertextype === "FFV") || (vertextype === "FFS"), 
                temp = temp //. IndexDelta[Index[Spin, s_], Index[Spin, r_]] -> 1;
                temp = temp //. TensDot[t1_?($TensClass[#] === MR$GammaMatrices &), t2___][r_,s_] -> GaAlgebra[t1, t2, r, s];
                temp = Expand[temp*GaAlgebra[]];
                temp = temp /. GaAlgebra[xx__, r_, s_] -> GaAlgebra[xx];
                temp = temp //. {GaAlgebra[] -> GaAlgebraDone[ProjP] + GaAlgebraDone[ProjM],
                         GaAlgebra[5] -> GaAlgebraDone[ProjP] - GaAlgebraDone[ProjM], 
                         GaAlgebra[nu_] :> GaAlgebraDone[nu ,ProjP] + GaAlgebraDone[nu, ProjM] /; (nu =!= 5) && (nu =!= ProjP) && (nu =!= ProjM), 
                         GaAlgebra[mu__, nu_] :> GaAlgebraDone[mu, nu, ProjP] + GaAlgebraDone[mu, nu, ProjM] /; (nu =!= 5) && (nu =!= ProjP) && (nu =!= ProjM),
                         GaAlgebra[mu__, 5] -> GaAlgebraDone[mu, ProjP] - GaAlgebraDone[mu, ProjM]};
                temp = temp //. GaAlgebraDone -> GaAlgebra;
                temp = temp //.{GaAlgebra[mu___,ProjP] -> GaAlgebra[mu]Proj[2],
                        GaAlgebra[mu___,ProjM] -> GaAlgebra[mu]Proj[1]};
                temp = Collect[temp, Proj[_], Simplify];
                temp = {{Coefficient[temp, Proj[1]]}, {Coefficient[temp, Proj[2]]}} //. GaAlgebra[___] -> 1,
             (******* Scalar couplings: S *********)
             (vertextype === "S"),
                Print[Style["Warning : tadpole vertices (S), this model should be used with lorentzTadpole.gen",Orange]];
                temp = {{temp}},
             (******* Scalar couplings: SS *********)
             (vertextype === "SS"),
                temp = temp/.{SP[2,2]->-SP[1,2],SP[1,1]->SP[1,2]};
                temp = {Coefficient[temp,SP[1,2]]*{1},Coefficient[temp,SP[1,2],0]*{1}},
             (******* Scalar couplings: SSS *********)
             (vertextype === "SSS"),
                temp = temp * {{1}},
             (******* Scalar couplings: SSSS *********)
             (vertextype === "SSSS"),
                temp = {{temp}},  
             (******* vector-scalar coupling: SV *******)
             (vertextype === "SV"),
                temp = temp /. {FV[a_,b_] -> FV[a]};
                temp = {Coefficient[temp,FV[1]] * {1},Coefficient[temp,FV[2]] * {1}},
             (******* vector-scalar coupling: SVV *******)
             (vertextype === "SVV"),
                temp = temp /. {ME[__] -> 1};
                temp = temp * {{1}},
             (******* vector-scalar coupling: SSVV *******)
             (vertextype === "SSVV"),
                temp = temp /. {ME[__] -> 1};
                temp = temp * {{1}},
             (******* 2 vector coupling: VV *******)
             (vertextype === "VV"),
                temp = temp/.{SP[2,2]->-SP[1,2],SP[1,1]->SP[1,2]};
                temp = {Coefficient[Coefficient[temp,SP[1,2]], ME[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]],
                        Coefficient[temp/.SP[__]->0, ME[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]],
                        Coefficient[temp, FV[FAPartContent[[1, 2]],Index[Lorentz, Ext[FAPartContent[[2, 2]]]]]FV[FAPartContent[[2, 2]],Index[Lorentz, Ext[FAPartContent[[1, 2]]]]]]};
                temp =  {{temp[[1]]},{temp[[2]]},{temp[[3]]}},            
             (******* 3 vector coupling: VVV *******)
             (vertextype === "VVV"),
                temp = Coefficient[temp, FV[FAPartContent[[2, 2]],Index[Lorentz, Ext[FAPartContent[[3, 2]]]]] ME[Index[Lorentz, Ext[FAPartContent[[1, 2]]]], Index[Lorentz, Ext[FAPartContent[[2, 2]]]]]];
                temp = temp * {{1}},                
             (******* 4 vector coupling: VVVV *******)
             (vertextype === "VVVV"),
                temp1 = Coefficient[temp, ME[Index[Lorentz, Ext[FAPartContent[[1, 2]]]], Index[Lorentz, Ext[FAPartContent[[2, 2]]]]]ME[Index[Lorentz, Ext[FAPartContent[[3, 2]]]], Index[Lorentz, Ext[FAPartContent[[4, 2]]]]]];
                temp2 = Coefficient[temp, ME[Index[Lorentz, Ext[FAPartContent[[1, 2]]]], Index[Lorentz, Ext[FAPartContent[[3, 2]]]]]ME[Index[Lorentz, Ext[FAPartContent[[2, 2]]]], Index[Lorentz, Ext[FAPartContent[[4, 2]]]]]];
                temp3 = Coefficient[temp, ME[Index[Lorentz, Ext[FAPartContent[[1, 2]]]], Index[Lorentz, Ext[FAPartContent[[4, 2]]]]]ME[Index[Lorentz, Ext[FAPartContent[[2, 2]]]], Index[Lorentz, Ext[FAPartContent[[3, 2]]]]]];
                temp = {{temp1}, {temp2}, {temp3}},
             (******* 2 scalar - 1 vector coupling: SSV *******)
             (vertextype === "SSV"),
                temp = Coefficient[Expand[temp], FV[FAPartContent[[1, 2]],Index[Lorentz, Ext[FAPartContent[[3,2]]]]]];  
                temp = temp * {{1}},
             (******* Gost couplings: UU *********)
             (vertextype === "UU"),
                temp = temp/.{SP[2,2]->-SP[1,2],SP[1,1]->SP[1,2]};
                temp = {Coefficient[temp,SP[1,2]]*{1},Coefficient[temp,SP[1,2],0]*{1}},
             (******* vector-scalar coupling: SUU *******)
             (vertextype === "SUU"),
                temp = temp * {{1}},
             (******* vector-scalar coupling: UUV *******)
             (vertextype === "UUV"),
                temp = Expand[temp /. FV[FAPartContent[[2, 2]],Index[Lorentz, Ext[FAPartContent[[3, 2]]]]] :> -FV[FAPartContent[[1, 2]],Index[Lorentz, Ext[FAPartContent[[3, 2]]]]] -FV[FAPartContent[[3, 2]],Index[Lorentz, Ext[FAPartContent[[3, 2]]]]]];
                temp = {{Coefficient[temp, FV[FAPartContent[[1, 2]],Index[Lorentz, Ext[FAPartContent[[3, 2]]]]], 1]}, {0}},
             (************** No Match *******************)
              True, 
                temp = 0;
                Print["Warning: The coupling ", vertextype, " is not supported by Lorentz.gen. Vertex is ignored."]];

(* 
  End filter. Go on with last 'make up'
*)
	  (*remove remaining lorentz structure from higher dimensional operators*)
	  If[Not[FreeQ[temp,Lorentz]||FreeQ[temp,Spin]||FreeQ[temp,SP]],
         Print["Warning : structure not contained in Lorentz.gen are removed"];
         If[ArrayDepth[temp]==1,
		   temp[[1]] = Total[Cases[ExpandAll[#],coef_/;(FreeQ[coef,Spin]&&FreeQ[coef,Lorentz]&&FreeQ[coef,SP]),If[Head[ExpandAll[#]]===Plus,{1},{0}]]]&@@temp[[1]];,
           temp[[All,1]] = Total[Cases[ExpandAll[#],coef_/;(FreeQ[coef,Spin]&&FreeQ[coef,Lorentz]&&FreeQ[coef,SP]),If[Head[ExpandAll[#]]===Plus,{1},{0}]]]&/@temp[[All,1]];
         ];
	  ];

      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;

      temp = MakeMatrixChain[temp];

      temp = temp //. {ee -> EL, gs -> GS} //. ParamRules;
      temp = temp //. Index[name_, Ext[i__]] :> MakeFAExtIndex[Index[name, Ext[i]], fc,FAPartContent];
      temp = temp //. Index[name_, k__] :> MakeFAIntIndex[Index[name, k]];

      If[Not[FreeQ[temp, Index[_, _?(Not[NumericQ[#]]&)]]], temp = MakeIndexSum[temp]];

      temp = temp /. {Index[_, k_] -> k};
      Off[Simplify::time];
      temp = If[$VersionNumber>8,Factor[Expand[temp]],Simplify[Expand[temp],TimeConstraint->0.001]];
      On[Simplify::time];

      tempFApc = First /@ FAPartContent;
Print["mytimecheck, LGC-1"];
      output = {tempFApc, temp}];


(* ::Section::Closed:: *)
(*Killing tree-level bilinears*)


KillBil[{parts_List,expr_}] := Block[{tree=expr /. FR$LoopDum ->0},

    If[Length[parts]!=2,
       Return[{parts,expr}]
       ];

    Return[{parts, Expand[expr - tree]}];
];
    

