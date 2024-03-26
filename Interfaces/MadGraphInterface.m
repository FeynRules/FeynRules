(* ::Package:: *)

(* ::Title:: *)
(*MadGraph interface *)


(* ::Text:: *)
(*C. Duhr*)
(*M. Herquet*)
(**)
(*12.05.2010, CD - Added RemoveGhostsAndGoldstones function, and added it to the MG interface.*)
(**)
(*28.08.09, PA - Starting to introduce spin2 particles.*)


(* ::Text:: *)
(*19.02.09, CD - Changed the writing of coupl.inc:*)
(*                            M$ModelName -> StringReplace[M$ModelName, {"-"->"_"," "->"_"}]*)
(*12. 02.09, CD - Changed ToFortranNumber[] to deal with the patological case where a parameter has a zero numerical value.*)


(* ::Section::Closed:: *)
(*RemoveGhostsAndGoldstones*)


(* ::Text:: *)
(*RemoveGhostsAndGoldstones[ vertexlist ] removes all vertices involving ghosts and goldstones from the vertexlist.*)


RemoveGhostsAndGoldstones[vertexlist_] := Block[{templist},

   templist = Select[vertexlist, FreeQ[#, _?((GhostFieldQ[#] === True) || (GoldstoneQ[#] === True)&)]&];

   If[Length[templist] != Length[vertexlist],
      Print["Removing ghost fields and Goldstone boson vertices: ", Length[vertexlist] - Length[templist], " vertices removed."]
     ];

   Return[templist]
];
   


(* ::Section:: *)
(*Useful functions*)


(********************** Collecting the couplings ********************)

CollectCouplingsMG[vert_List] := Block[{templist, tempvert = vert, couplist = {}, elem, namefunc, output},
     Do[elem = vert[[kk, 4]];
        If[(elem === {-G, -G}) || elem === {G, G} || elem === {I*G, I*G} || elem === {-I G, -I G}, tempvert = ReplacePart[tempvert, {"GG"}, {kk, 3}];
           If[elem === {I*G, I*G} || elem === {-I G, -I G}, tempvert = ReplacePart[tempvert, {-G,-G}, {kk, 4}]],
           If[Not[MemberQ[couplist, elem]], 
              couplist = Append[couplist, elem];
              namefunc[elem] = vert[[kk,3]],
              tempvert = ReplacePart[tempvert, namefunc[elem], {kk, 3}]]],
        {kk, Length[vert]}];
      output = tempvert];

MGOrdertemp[G] = MGOrdertemp[gs];
      


ExpandTensDotForMG[x_,y_][i_, j_] := Block[{ind1 = Last[$IndList[x]], ind2 = Extract[$IndList[y],-2], output},
          If[MRIndexRange[ind1] =!= MRIndexRange[ind2],
             Message[TensDot::ExpandTensDotForMG];Abort[],
             output = Sum[x[i, Insert[ind1, kk, 2]] y[Insert[ind2, kk, 2], j], {kk, Length[MRIndexRange[ind1]]}]];
          output];
        
ExpandTensDotForMG[x_,y_,zz__][i_, j_] := Block[{ind1 = Last[$IndList[x]], ind2 = Extract[$IndList[y],-2], output},
          If[MRIndexRange[ind1] =!= MRIndexRange[ind2],
             Message[TensDot::ExpandTensDotForMG];Abort[],
             output = Sum[x[i, Insert[ind1, kk, 2]] ExpandTensDotForMG[y, zz][Insert[ind2, kk, 2], j], {kk, Length[MRIndexRange[ind1]]}]];
          output];

(********************* DUMmies for MG *******************)

MakeDum0Sqrt[{a_, b_, {c_, "DUM0"}, d_, e_, f_}] := {a, b, {c, "DUM0"}, Sqrt[d], e, f};
MakeDum0Sqrt[{a_, b_, c_?(Not[MatchQ[#, {_,"DUM0"}]]&), d_, e_, f_}] := {a, b, c, d, e, f};

AdjustMGInteractionOrder[{aa_, bb_, cc_, dd_, ee_, ff_}] := Block[{tio = DeleteCases[ee, {H,_}|{n,_}|{a,_}|{"^dabc^",_}], vlist},
       tio = Plus @@ ((If[Head[#1] === Times, Length[#1] * #2, #2] &) @@@ tio);
       vlist = If[tio == Length[cc], {aa,bb,cc,dd,ee,ff}, {aa,bb,Join[cc, Table["DUM1", {tio - Length[cc]}]], dd, ee, ff}]];

PrintOutputMG[crealist_] := Module[{temp, temp2, output},
    temp = crealist;
    temp2 = temp //. {field_, k_?IntegerQ} :> field;
    temp = Reverse[StringJoin[ToString[#1],"_",ToString[#2]," "]& @@@ temp];
    temp2 = (PartFieldType /@ temp2) //. VertexTypeSortRules;
    temp2 = StringJoin @@ temp2;
    output = {temp2, temp}];
        


DecomposeEI[expr_] := Block[{MyRe, MyIm, tmpexpr},
    MyRe[pp_?((numQ[#] && Not[CnumQ[#]])&)]:=pp;
    MyIm[pp_?((numQ[#] && Not[CnumQ[#]])&)]:=0;
    MyRe[aa_+bb_]:=MyRe[aa]+MyRe[bb]; 
    MyIm[aa_+bb_]:=MyIm[aa]+MyIm[bb];
    MyRe[aa_*bb_]:=MyRe[aa]*MyRe[bb]-MyIm[aa]MyIm[bb];
    MyIm[aa_*bb_]:=MyRe[aa]MyIm[bb]+MyRe[bb]MyIm[aa];
    tmpexpr = expr /. Power[E, xx_?((numQ[#] && CnumQ[#])&)] :> Power[E, MyRe[xx]]*Power[E, I MyIm[xx]]/. {MyRe -> Re, MyIm -> Im};
    tmpexpr = tmpexpr /. {Power[E,aa_+bb_]:>Power[E,aa]*Power[E,bb]} /. {Power[E, xx_?((numQ[#] && CnumQ[#])&)] :>Power[E,MyRe[xx]]Cos[MyIm[xx]]+ I Power[E,MyRe[xx]]Sin[MyIm[xx]]};
    tmpexpr = tmpexpr /. {MyRe -> Re, MyIm -> Im}];

DecomposeEParam[param_] := Which[Length[param]==4, MapAt[NumericalValue,param,2],  
         Length[param]==6, MapAt[NumericalValue,param,4],
         True, param];

DecomposeIParam[param_] := MapAt[DecomposeEI,param,2];


UnCapitalize[str_]:=StringReplace[str,{"A"->"a", "B"->"b", "C"->"c", "D"->"d", "E"->"e", "F"->"f", "G"->"g", "H"->"h", "I"->"i", "J"->"j", "K"->"k", "L"->"l",
      "M"->"m", "N"->"n", "O"->"o", "P"->"p", "Q"->"q", "R"->"r", "S"->"s", "T"->"t", "U"->"u", "V"->"v", "W"->"w", "X"->"x", "Y"->"y", "Z"->"z"}];

ReplaceTilde[str_]:=If[StringTake[str,1]==="~","h"<>StringDrop[str,1],str];

CreateParticleNameReplacements[parts_]:=Block[{partNames={},tPartNames={},lowerPartNames={},replacements={},dialogString="",nonTrivialChanges=False},
   (*Extract particle names.*)
   Do[AppendTo[partNames,{parts[[j,2,k,1]],parts[[j,2,k,2]]}],{j,1,Length[parts]},{k,1,Length[parts[[j,2]]]}];
   (*Move ~ to the end of the names.*)
   Do[AppendTo[tPartNames,{ReplaceTilde[partNames[[j,1]]],ReplaceTilde[partNames[[j,2]]]}],{j,1,Length[partNames]}];
   (*Lower case names.*)
   Do[AppendTo[lowerPartNames,{UnCapitalize[tPartNames[[j,1]]],UnCapitalize[tPartNames[[j,2]]]}],{j,1,Length[tPartNames]}];
   (*If lower cased particle and antiparticle names are the same, add a tilde.*)
   dialogString="MG is case insensitive.  All particle names have been lower cased.\n";
   dialogString=dialogString<>"Additionally, a '~' can only be added at the end of a particle name.\n";
   dialogString=dialogString<>"\tAll initial '~'s have been replaced with 'h's.\n";
   dialogString=dialogString<>"There were still some particle name conflicts.\n";
   dialogString=dialogString<>"A '~' was added to the following antiparticles on the fly:\n";
   Do[
      If[lowerPartNames[[j,1]]===lowerPartNames[[j,2]]&&partNames[[j,1]]=!=partNames[[j,2]],
         lowerPartNames[[j,2]]=lowerPartNames[[j,2]]<>"~";
         dialogString=dialogString<>partNames[[j,2]]<>" \[Rule] "<>lowerPartNames[[j,2]]<>"\n";
         nonTrivialChanges=True;
      ];
   ,{j,1,Length[partNames]}];
   dialogString=dialogString<>"Please make sure that there are no conflicts in particle names.\n";
   dialogString=dialogString<>"\nIs it ok to make these renamings?";
   If[MG$DialogBox,
     If[nonTrivialChanges,If[!ChoiceDialog[dialogString,{"Ok"->True, "Abort"->False}],Abort[]]]];
   (*Create a replacement list.*)
   If[lowerPartNames=!=partNames,
      Do[
         Which[
            partNames[[j,1]]===partNames[[j,2]]&&partNames[[j,1]]=!=lowerPartNames[[j,1]]
         ,  AppendTo[replacements,partNames[[j,1]]->lowerPartNames[[j,1]]];
         ,  partNames[[j,1]]=!=partNames[[j,2]]&&partNames[[j,1]]=!=lowerPartNames[[j,1]]&&partNames[[j,2]]===lowerPartNames[[j,2]]
         ,  AppendTo[replacements,partNames[[j,1]]->lowerPartNames[[j,1]]];
         ,  partNames[[j,1]]=!=partNames[[j,2]]&&partNames[[j,1]]===lowerPartNames[[j,1]]&&partNames[[j,2]]=!=lowerPartNames[[j,2]]
         ,  AppendTo[replacements,partNames[[j,2]]->lowerPartNames[[j,2]]];
         ,  partNames[[j,1]]=!=partNames[[j,2]]&&partNames[[j,1]]=!=lowerPartNames[[j,1]]&&partNames[[j,2]]=!=lowerPartNames[[j,2]]
         ,  AppendTo[replacements,partNames[[j,1]]->lowerPartNames[[j,1]]];
            AppendTo[replacements,partNames[[j,2]]->lowerPartNames[[j,2]]];
         ];
      ,{j,1,Length[partNames]}];
   ];

replacements
];

MGName[part_]:=Module[{tmp},
If[Length[MGPartNameReplacements]===0,MGPartNameReplacements=CreateParticleNameReplacements[PartList]];
StringReplace[part,MGPartNameReplacements]
];


MGParticleName[ff_]:=MGName[PartNameMG[ff]];


FindListPermutation[list1_, list2_] :=Block[{rulelist},
   (* Finds the permutation that maps list1 into list2 *)
   rulelist = Table[Rule[list1[[kk]], kk],{kk,Length[list1]}];
   list2/.rulelist];
   


(* ::Section::Closed:: *)
(*Vertex reordering*)


VertexTypeSortRules = {{xx___,"V","F",yy___} -> {xx,"F","V",yy},
                       {xx___,"S","F",yy___} -> {xx,"F","S",yy},
                       {xx___,"T","F",yy___} -> {xx,"F","T",yy},
                       {xx___,"S","V",yy___} -> {xx,"V","S",yy},
                       {xx___,"T","V",yy___} -> {xx,"V","T",yy},
                       {xx___,"T","S",yy___} -> {xx,"S","T",yy},
                       {xx___,"V","U",yy___} -> {xx,"U","V",yy}};

GetCharge4Boson[wlist_]:=Block[{getQ,eecharge},
   getQ[ww_]:=If[Q[ww]===0,0,1];
   eecharge=Plus@@(getQ/@wlist);
   Which[
     (* If 4 charged bosons *)
     eecharge == 4,
     Which[(Q[wlist[[1]]]==1)&&(Q[wlist[[3]]]==1),wlist,
      (Q[wlist[[1]]]==1)&&(Q[wlist[[2]]]==1),{wlist[[1]],wlist[[3]],wlist[[2]],wlist[[4]]},
      (Q[wlist[[1]]]==1)&&(Q[wlist[[4]]]==1),{wlist[[1]],wlist[[2]],wlist[[4]],wlist[[3]]},
      (Q[wlist[[2]]]==1)&&(Q[wlist[[3]]]==1),{wlist[[2]],wlist[[1]],wlist[[3]],wlist[[4]]},
      (Q[wlist[[2]]]==1)&&(Q[wlist[[4]]]==1),{wlist[[2]],wlist[[1]],wlist[[4]],wlist[[3]]},
      (Q[wlist[[3]]]==1)&&(Q[wlist[[4]]]==1),{wlist[[3]],wlist[[1]],wlist[[2]],wlist[[4]]}],
    (* If 2 charged bosons *)
    eecharge == 2,
    Which[(Q[wlist[[1]]]==1)&&(Q[wlist[[3]]]==-1),wlist,
      (Q[wlist[[1]]]==1)&&(Q[wlist[[2]]]==-1),{wlist[[1]],wlist[[3]],wlist[[2]],wlist[[4]]},
      (Q[wlist[[1]]]==1)&&(Q[wlist[[4]]]==-1),{wlist[[1]],wlist[[2]],wlist[[4]],wlist[[3]]},
      (Q[wlist[[2]]]==1)&&(Q[wlist[[3]]]==-1),{wlist[[2]],wlist[[1]],wlist[[3]],wlist[[4]]},
      (Q[wlist[[2]]]==1)&&(Q[wlist[[4]]]==-1),{wlist[[2]],wlist[[1]],wlist[[4]],wlist[[3]]},
      (Q[wlist[[3]]]==1)&&(Q[wlist[[4]]]==-1),{wlist[[3]],wlist[[1]],wlist[[4]],wlist[[2]]},
      (Q[wlist[[1]]]==-1)&&(Q[wlist[[2]]]==1),{wlist[[2]],wlist[[3]],wlist[[1]],wlist[[4]]},
      (Q[wlist[[1]]]==-1)&&(Q[wlist[[3]]]==1),{wlist[[3]],wlist[[2]],wlist[[1]],wlist[[4]]},
      (Q[wlist[[1]]]==-1)&&(Q[wlist[[4]]]==1),{wlist[[4]],wlist[[2]],wlist[[1]],wlist[[3]]},
      (Q[wlist[[2]]]==-1)&&(Q[wlist[[3]]]==1),{wlist[[3]],wlist[[1]],wlist[[2]],wlist[[4]]},
      (Q[wlist[[2]]]==-1)&&(Q[wlist[[4]]]==1),{wlist[[4]],wlist[[1]],wlist[[2]],wlist[[3]]},
      (Q[wlist[[3]]]==-1)&&(Q[wlist[[4]]]==1),{wlist[[4]],wlist[[1]],wlist[[3]],wlist[[2]]}],
     eecharge==0,
       wlist]];
                       
ReOrderFCforMG[ll__] := Module[{ReOrderFunc, temp, output,vt, AntiToEnd, ffv},
        ReOrderFunc[x___, f1_?(Not[FermionQ[#] === True]&), f2_?(FermionQ[#] === True &), y___] := ReOrderFunc[x, f2, f1, y];
        ReOrderFunc[x___, f1_?(Not[FermionQ[#] === True] && Not[VectorFieldQ[#] === True]&), f2_?(VectorFieldQ[#] === True &), y___]:= ReOrderFunc[x, f2, f1, y];
        ReOrderFunc[x___, f1_?(Not[FermionQ[#] === True] && Not[VectorFieldQ[#] === True]&& Not[ScalarFieldQ[#] === True]&), f2_?(ScalarFieldQ[#] === True &), y___] := ReOrderFunc[x, f2, f1, y];
        AntiToEnd[x___, f1_?(AntiFieldQ[#] === True &), f2_?(Not[AntiFieldQ[#] === True] &), y___] := AntiToEnd[x, f2, f1, y];
        temp = ReOrderFunc @@ ll;
        temp = List @@ temp;
        temp = temp /. {ff_?((FieldQ[#] && Not[DiracFieldQ[#] === True] && Not[MajoranaFieldQ[#] === True])&) :> anti[ff]};
        vt = StringJoin @@ (PartFieldType /@ temp);
        output = Which[
           
           (vt === "FFV") || (vt === "FFS")|| (vt === "FFT"),
              ffv = If[AntiFieldQ[temp[[1]]], {temp[[2]], anti[temp[[1]]], temp[[3]]}, {temp[[1]], anti[temp[[2]]], temp[[3]]}];
              ffv = ffv //. CC -> anti,

(*Added by Prisila*)

           (vt === "FFVT"),
              ffv = If[AntiFieldQ[temp[[1]]], {temp[[2]], anti[temp[[1]]], temp[[3]], temp[[4]]}, {temp[[1]], anti[temp[[2]]], temp[[3]], temp[[4]]}];
              ffv = ffv //. CC -> anti,

(***)

           (vt === "VVV") || (vt === "SSS"),
                Which[(Q[temp[[1]]] == 0) && (Q[temp[[2]]] != 0), {temp[[2]], temp[[3]], temp[[1]]},
                      (Q[temp[[2]]] == 0) && (Q[temp[[1]]] != 0), {temp[[1]], temp[[3]], temp[[2]]},
                      True, temp],
                      
           vt  === "VVS",
              If[AntiFieldQ[temp[[3]]], If[AntiFieldQ[temp[[2]]], {temp[[2]], temp[[1]], temp[[3]]}, temp], If[AntiFieldQ[temp[[1]]], {temp[[2]], temp[[1]], temp[[3]]}, temp]],

           (vt === "VVVV"),
              temp = List @@ AntiToEnd @@ temp;
              temp = GetCharge4Boson[temp],

           (vt === "SSSS"),
              temp = List @@ AntiToEnd @@ temp,

           vt === "VVSS",
              temp = If[AntiFieldQ[temp[[1]]], {temp[[2]], temp[[1]], temp[[3]], temp[[4]]}, temp];
              If[AntiFieldQ[temp[[3]]], {temp[[1]], temp[[2]], temp[[4]], temp[[3]]}, temp],

           vt === "VSS",
              temp = Which[VectorFieldQ[temp[[2]]] === True, {temp[[2]], temp[[1]], temp [[3]]},
                           VectorFieldQ[temp[[3]]] === True, {temp[[3]], temp[[1]], temp [[2]]},
                           True, temp];
              temp = If[AntiFieldQ[temp[[2]]] === True, {temp[[1]], temp[[3]], temp[[2]]}, temp],

           True,
              temp]];
IsGluonOctet4PointQ[{g1_, g2_, g3_, g4_}] := And @@ ((MemberQ[$IndList[#], Index[Gluon]]&) /@ {g1, g2, g3, g4});
MakeMG4Interaction[{a_, b_?(# =!= "VVVV" &), c_, d_, e_}] := {a, b, c, {d}, e};
MakeMG4Interaction[{a_?(IsGluonOctet4PointQ[#] === True &), "VVVV", c_, d_, e_}] := {a, "VVVV", c, {d, "DUM1"}, e};
MakeMG4Interaction[{{v1_, v2_ , v3_, v4_}, "VVVV", c_, d_, e_}] := If[(v1 === v2) || ((v1 === anti[v2]) && (v1 === v3)) || ((v1 === anti[v3]) && (v1 === v4)) || (v1 === anti[v4]), {{v1, v2, v3, v4}, "VVVV", c, {d, "DUM0"}, e}, 
                                                                   If[v1 === anti[v3], {{v1, v2, v3, v4}, "VVVV", c, {d, "DUM1"}, e}, {{v1, v2, v3, v4}, "VVVV", c, {d, "DUM0"}, e}]]  ;


(* ::Section:: *)
(*Writing routines (M. Herquet)*)


(* ::Subsection::Closed:: *)
(*Fortran formatting*)


(************* Output files for MG *****************************************)
Unprotect[Power];
Format[Sqrt[x_],FortranForm]:=sqrt[N[x]];
Protect[Power];

Unprotect[Conjugate];
Format[Conjugate[x_],FortranForm]:=conjg[x];
Protect[Conjugate];

ToFortranDeclaration[xx_]:=
    StringJoin["double precision ",
      StringDrop[
        StringJoin[StringJoin[ToString[#,FortranForm],","]&/@ xx],-1]];

ToFortranDeclarationReal[xx_]:=
    StringJoin["double precision ",
      StringDrop[
        StringJoin[StringJoin[#,","]&/@ xx],-1]];

ToFortranDeclarationComplex[xx_]:=
    StringJoin["double complex ",
      StringDrop[
        StringJoin[StringJoin[If[StringQ[#], #, ToString[#, FortranForm]],","]&/@ xx],-1]];

ToFortranDeclarationDoubleComplex[xx_]:=
    StringJoin["double complex ",
      StringDrop[
        StringJoin[StringJoin[#,"(2),"]&/@ xx],-1]];

ToFortranCommonBlock[xx_,cb_]:=
    StringJoin["common/",cb,"/ ",
      StringDrop[
        StringJoin[StringJoin[ToString[#,FortranForm],","]&/@ xx],-1]];

ToFortranCommonBlockCoupl[xx_,cb_]:=
    StringJoin["common/",cb,"/ ",
      StringDrop[
        StringJoin[StringJoin[#,","]&/@ xx],-1]];

WriteToFortranFile[stream_, str_] :=
 If[StringMatchQ[str, "*\"*"], Block[{i, j, instr, char},
   (WriteString[stream, "      "];
    instr = False;
    j = 1;
    For[i = 1, i <= StringLength[str],
     If[Mod[j, 62] == 0,
      If[instr, (WriteString[stream, "\"\n     +//\""]; j += 3),
       WriteString[stream, "\n     +"]]];
       char = StringTake[str, {i}];
       If[StringMatchQ[char, "\""], instr = ! instr];
       WriteString[stream, char];
       i += 1; j += 1];
       WriteString[stream, "\n"])], 
       Block[{i},
        (WriteString[stream,"      " <> StringTake[str, {1, Min[StringLength[str], 61]}] <> "\n"];
         For[i = 62, i <= StringLength[str],
             WriteString[stream, "     +" <> StringTake[str, {i, Min[StringLength[str], i + 59]}] <>"\n"];
             i += 60])]];
        
ToFortranNumber[xxxx_] := Block[{outputo, tempo},
     If[N[xxxx] == 0., outputo = "0.00000000E+00",
   (* else *)
     tempo = SetPrecision[MantissaExponent[xxxx], 15];
     tempo = {10*tempo[[1]], Rationalize[tempo[[2]] - 1]};
     tempo = {StringTake[StringReplace[ToString[N[Rationalize[tempo[[1]]], 15]],{"`9." -> ""}], 15], ToString[PaddedForm[tempo[[2]], 2, NumberPadding -> {"0", "0"}, SignPadding -> True, NumberSigns -> {"-", "+"}]]};
     outputo = tempo[[1]] <> "E" <> tempo[[2]]];
   outputo];
     
ToFortranDefinition[xx_]:=
    ToString[xx[[1]]]<>" = "<>ToString[FortranForm[xx[[2]]]];

ToFortranCouplDefinition[xx_]:=
    xx[[1]]<>" = "<>ToString[FortranForm[xx[[2]]]];

ToFortranChiralDefinition[xx_]:=
List[xx[[1]]<>"(1) = "<>ToString[FortranForm[xx[[2]][[1]]]],xx[[1]]<>"(2) = "<>ToString[FortranForm[xx[[2]][[2]]]]];



(* ::Subsection::Closed:: *)
(*WriteInteractionsFiles*)


      KillDums[{a_, b_, c_, d_, e_, f_}] := {a, b, c[[1]], d, e, f};

WriteInteractionsFilesOne[L_,name_] := Module[{myfile, myfile2},
   (*   CouplOrderFormat[xx_]:=Module[{str,i},str="";tot=0;
             For[i=1,i<=#[[2]],str=str<>" "<>ToString[#[[1]]];i++]&/@ xx;
             For[i=1,i<Sum[xx[[All,2]]][[1]],str="DUM "<>str;i++];
             Return[str]];*)


      
      (* Write couplings "i".f *)
      
      myfile = "couplings" <> name <>".f";
      AppendTo[FR$CouplString, "couplings" <> name <>".o"];
      OpenWrite[myfile];
      WriteString[myfile, "      subroutine coup" <> name <>"(readlha)\n"];
      WriteString[myfile, "\n"];
      WriteString[myfile, "      implicit none\n"];
      WriteString[myfile, "      logical readlha\n"];
      WriteString[myfile, "\n"];
      WriteString[myfile, "      include 'input.inc'\n"];
      WriteString[myfile, "      include 'coupl.inc'\n"];
      WriteString[myfile, "      include 'intparam_definition.inc'\n"];
      WriteString[myfile, "\n"];
      WriteString[myfile, "\nc Interactions associated with "<>name<>"\n"];
      If[(CouplRun =!= {}) && (name =!= "1"),
         WriteString[myfile, "      if(readlha) then\n"]];
      WriteToFortranFile[myfile,ToFortranCouplDefinition[#]]&/@KillDoubles[Select[KillDums /@ L,#[[6]]=="R" &][[All,{3,4}]]];
      WriteString[myfile,"\n"];
      WriteToFortranFile[myfile,ToFortranCouplDefinition[#]]&/@KillDoubles[Select[KillDums /@ L,#[[6]]=="C" &][[All,{3,4}]]];
      WriteString[myfile,"\n"];
      WriteToFortranFile[myfile,#]&/@#&/@(ToFortranChiralDefinition[#]&/@KillDoubles[Select[KillDums /@ L,#[[6]]=="DC" &][[All,{3,4}]]]);
      If[(CouplRun =!= {}) && (name =!= "1"),
         WriteString[myfile, "      endif\n"]];
      WriteString[myfile, "\n"];
      WriteString[myfile, "      return\n"];
      WriteString[myfile, "      end\n"];
      Close[myfile];


      myfile = OpenAppend["couplings.f"];
      WriteString[myfile,"      CALL coup" <> name <> "(readlha)\n"];
      Close[myfile];
];





WriteInteractionsFiles[xx_]:=Module[{myfile, myfile2, lags},

      myfile=OpenWrite["interactions.dat"];
      WriteString[myfile, "#This file has been generated automatically by FeynRules " <> FR$VersionNumber <> "    Date: " <> FR$DateFormat[] <>"\n"];
      Close["interactions.dat"];

      myfile=OpenWrite["coupl_write.inc"];
      WriteString[myfile, "c This file has been generated automatically by FeynRules\n\n"];
      WriteString[myfile, "\nc Version: " <> FR$VersionNumber <>  "   Date: " <> FR$DateFormat[] <>"\n"];
      Close["coupl_write.inc"];

      myfile=OpenWrite["helas_couplings.inc"];
      WriteString[myfile, "c This file has been generated automatically by FeynRules\n\n"];
      WriteString[myfile, "\nc Version: " <> FR$VersionNumber <>  "   Date: " <> FR$DateFormat[] <>"\n"];
      Close["helas_couplings.inc"];

      myfile=OpenWrite["intparam_definition.inc"];
      WriteString[myfile, "c This file has been generated automatically by FeynRules\n"];
      WriteString[myfile, "\nc Version: " <> FR$VersionNumber <>  "   Date: " <> FR$DateFormat[] <>"\n\n"];
       WriteString[myfile, "c Internal parameters definition :\n"];
 

(*      If[MGIParamList =!= {},
         If[#[[1]] === G, WriteString[myfile,"      if(readlha) then\n"];WriteToFortranFile[myfile,ToFortranDefinition[#]]; WriteString[myfile,"      endif\n"], 
           WriteToFortranFile[myfile,ToFortranDefinition[#]]] &/@MGIParamList];*)
       

       If[MGIParamListRun =!= {},
          WriteString[myfile, "\n"]; 
          WriteString[myfile, "c Parameters that should not be recomputed event by event.\n"]; 
          WriteString[myfile,"      if(readlha) then\n"];
          WriteToFortranFile[myfile,ToFortranDefinition[#]] &/@MGIParamListNotRun;
          WriteString[myfile,"      endif\n"]];

       If[MGIParamListNotRun =!= {},
          WriteString[myfile, "c Parameters that should be recomputed at an event by even basis.\n"];
          WriteToFortranFile[myfile,ToFortranDefinition[#]] &/@MGIParamListRun];
          

       Close["intparam_definition.inc"];



     (* Write interactions.dat *)
     (* CD: Added the If statement in InteractionFormat[ ] to treat the VVVS correctly *)
      CouplOrderFormat[xxx_]:=Module[{str,i},str="";tot=0;
             For[i=1,i<=#[[2]],str=str<>" "<>ToString[#[[1]]];i++]&/@ xxx;
             Return[str]];
      InteractionFormat[xxx_]:=ToString[{xxx[[2]]}/.MGPartNameReplacements//TableForm]<>" "
      <>ToString[TableForm[{If[xxx[[1]] ==="VVVS", Reverse[xxx[[3]]], xxx[[3]]]}]]<>"  "<>CouplOrderFormat[xxx[[5]]]<>"\n";

      myfile=OpenAppend["interactions.dat"];
      WriteString[myfile, "\n# Interactions associated with "<>M$ModelName<>"\n"];
      WriteString[myfile,InteractionFormat[#]]&/@ VerticesMG[M$ModelName];
      Close["interactions.dat"];



     (* Writing the Coupl.inc *)
       lags = VerticesMG[M$ModelName];

          myfile=OpenAppend["coupl.inc"];
      WriteString[myfile, "\nc Interactions associated with " <> M$ModelName <> ":\n"];
      If[Length[Select[lags,#[[6]]=="R" &]]!=0,
      WriteToFortranFile[myfile,ToFortranDeclarationReal[KillDoubles[Select[lags,#[[6]]=="R" &][[All,3,1]]]]];
      WriteToFortranFile[myfile,ToFortranCommonBlockCoupl[KillDoubles[Select[lags,#[[6]]=="R" &][[All,3,1]]],"real_coupl_"<>StringReplace[M$ModelName,{"-"->"_"," "->"_"}]]<>"\n"]];
      If[Length[Select[lags,#[[6]]=="C" &]]!=0,
      WriteToFortranFile[myfile,ToFortranDeclarationComplex[KillDoubles[Select[lags,#[[6]]=="C" &][[All,3,1]]]]];
      WriteToFortranFile[myfile,ToFortranCommonBlockCoupl[KillDoubles[Select[lags,#[[6]]=="C" &][[All,3,1]]],"comp_coupl_"<>StringReplace[M$ModelName,{"-"->"_"," "->"_"}]]<>"\n"]];
      If[Length[Select[lags,#[[6]]=="DC" &]]!=0,
      WriteToFortranFile[myfile,ToFortranDeclarationDoubleComplex[KillDoubles[Select[lags,#[[6]]=="DC" &][[All,3,1]]]]];
      WriteToFortranFile[myfile,ToFortranCommonBlockCoupl[KillDoubles[Select[lags,#[[6]]=="DC" &][[All,3,1]]],"chiral_coupl_"<>StringReplace[M$ModelName,{"-"->"_"," "->"_"}]]<>"\n"]]; 
      Close["coupl.inc"];

      (* Writing coupl_write and helas_couplings *)

      myfile=OpenAppend["coupl_write.inc"];
      myfile2=OpenAppend["helas_couplings.inc"];
      WriteToFortranFile[myfile,"write(*,*)  ' Couplings of "<> M$ModelName <>"'"];
      WriteToFortranFile[myfile,"write(*,*)  ' ---------------------------------'"];
      WriteToFortranFile[myfile,"write(*,*)  ''"];
      WriteToFortranFile[myfile,"write(*,*)  ' Real couplings : '"];
      WriteToFortranFile[myfile,"write(*,1) '"<>#<>"=',"<>#]&/@KillDoubles[Select[KillDums /@ lags,#[[6]]=="R" &][[All,3]]];
      WriteToFortranFile[myfile2,"write(1,11) '"<>#<>"',"<>#]&/@KillDoubles[Select[KillDums /@ lags,#[[6]]=="R" &][[All,3]]];
      WriteToFortranFile[myfile,"write(*,*)  ''"];
      WriteToFortranFile[myfile,"write(*,*)  ' Complex couplings : '"];
      WriteToFortranFile[myfile,"write(*,2) '"<>#<>"=',"<>#]&/@KillDoubles[Select[KillDums /@ lags,#[[6]]=="C" &][[All,3]]];
      WriteToFortranFile[myfile2,"write(1,12) '"<>#<>"',"<>#]&/@KillDoubles[Select[KillDums /@ lags,#[[6]]=="C" &][[All,3]]];
      WriteToFortranFile[myfile,"write(*,*)  ''"];
      WriteToFortranFile[myfile,"write(*,*)  ' Left/Right couplings : '"];
      WriteToFortranFile[myfile,"write(*,4) '"<>#<>"(L)=',"<>#<>"(1),'"<>#<>"(R)=',"<>#<>"(2)"]&/@KillDoubles[Select[KillDums /@ lags,#[[6]]=="DC" &][[All,3]]];
      WriteToFortranFile[myfile2,"write(1,13) '"<>#<>"',"<>#<>"(1),"<>#<>"(2)"]&/@KillDoubles[Select[KillDums /@ lags,#[[6]]=="DC" &][[All,3]]];
      WriteToFortranFile[myfile,"write(*,*)  ' '"];
      WriteToFortranFile[myfile,"write(*,*)  ' '"];
      Close["coupl_write.inc"];
      Close["helas_couplings.inc"];


      (* Write couplings .f *)
      myfile=OpenWrite["couplings.f"];
      WriteString[myfile, "c This file has been generated automatically by FeynRules\n\n"];
      WriteString[myfile, "\nc Version: " <> FR$VersionNumber <>  "   Date: " <> FR$DateFormat[] <>"\n"];
      WriteString[myfile, "\n"];
      WriteString[myfile, "      subroutine coup(readlha)\n"];
      WriteString[myfile, "\n"];
      WriteString[myfile, "      implicit none\n"];
      WriteString[myfile, "      logical readlha\n"];
      WriteString[myfile, "\n"];
      Close["couplings.f"];

      FR$CouplString = {};


       WriteInteractionsFilesOne[VerticesMGCoupl[#],#]&/@xx;


      myfile=OpenAppend["couplings.f"];
      WriteString[myfile, "\n"];
      WriteString[myfile, "      return\n"];
      WriteString[myfile, "      end\n"];
      Close["couplings.f"];

      (* Build FR$CouplString and splice it into makefile *)

      Do[myfile = FR$FRTemplateFiles[[kk]];
         If[MemberQ[FileNames[], myfile], DeleteFile[myfile]];
         CopyFile[FR$FRTemplatePath <> "/" <> myfile, Directory[] <> "/" <>  myfile],
         {kk, Length[FR$FRTemplateFiles]}];
   
      AppendTo[FR$CouplString, "couplings.o"];

      myfile = OpenWrite["makeinc.inc"];
      WriteString[myfile, "MODEL="];
      Do[WriteString[myfile, FR$CouplString[[kk]] <> " "], {kk, Length[FR$CouplString]}];
      WriteString[myfile, "lha_read.o printout.o rw_para.o\n"];
      Close["makeinc.inc"];


    myfile = OpenAppend["intparam_definition.inc"];
    WriteString[myfile, "\n\nc Definition of the EW coupling used in the write out of aqed\n"]; 
    WriteString[myfile, "      gal(1) = ee\n"];
    WriteString[myfile, "      gal(2) = ee\n\n\n"];
    WriteString[myfile, "c Definition of DUM symbols\n"]; 
    WriteString[myfile, "      DUM0 = 0\n"];
    WriteString[myfile, "      DUM1 = 1\n\n\n"];
    (*WriteString[myfile, "       GG(1) = -G/n"];
    WriteString[myfile, "       GG(2) = -G/n"];*)
    Close["intparam_definition.inc"];
      ];


(* ::Subsection::Closed:: *)
(*WriteOutput*)


WriteOutput := Module[{GoodShape, GoodPrinting, myfile, tempparams, InvParamRules = Reverse /@ ParamRules},
myfile=OpenWrite["ident_card.dat"];
WriteString[myfile,
    "** This file has been generated automatically by FeynRules\n"];
WriteString[myfile,
    "*********************************************************\n"];
Close["ident_card.dat"];


DropInter[xx__]:=List[#[[1]],First[#[[2]]]]&/@Last[xx];
GoodShape[xx__]:=Map[Flatten,Prepend[#,First[xx]]&/@DropInter[xx],1];
GoodShape2[xx__]:=Map[Flatten,Prepend[{Abs[#[[1]]],#[[2]]},First[xx]]&/@Last[xx],1];
If[$VersionNumber >= 6, 
   GoodPrinting[xx__,myfile__]:=WriteString[myfile,StringJoin[ExportString[xx,"Table", "FieldSeparators"->" "], "\n"] ],
   GoodPrinting[xx__,myfile__]:=WriteString[myfile,StringJoin[ExportString[xx,"Table",ConversionOptions->{"ColumnAlignment"->Left}],"\n"] ]];
If[MGEParamList =!= {},
   myfile=OpenAppend["ident_card.dat"];
   GoodPrinting[#,myfile]&/@ GoodShape/@MGEParamList;
   Close["ident_card.dat"]];
If[MGMassList =!= {},
   myfile=OpenAppend["ident_card.dat"];
   GoodPrinting[GoodShape2[MGMassList],myfile];
   Close["ident_card.dat"]];
If[MGWidthList =!= {},
   myfile=OpenAppend["ident_card.dat"];
   GoodPrinting[GoodShape2[MGWidthList],myfile];
   Close["ident_card.dat"]];
myfile=OpenWrite["param_card.dat"];
WriteString[myfile, "#This file has been generated automatically by FeynRules\n"];
WriteString[myfile, "# Version: " <> FR$VersionNumber <>  "   Date: " <> FR$DateFormat[] <>"\n"];
WriteBlock[xx_,file_]:=Module[{i},
     WriteString[file,"Block "<>ToString[xx[[1]]]<>"\n"];
     WriteString[file,"  "<>StringJoin[ToString[#]<>" "&/@#[[1]]]
        <>" "<>ToFortranNumber[#[[2]][[-2]]]<>"  # "<>ToString[#[[2,1]]]<>"\n"]&/@ xx[[2]];
];
WriteBlockMass[xx_,file_]:=Module[{i},
     WriteString[file,"Block "<>ToString[xx[[1]]]<>"\n"];
     WriteString[file,"  "<>StringJoin[ToString[Abs[#]]<>" "&/@#[[1]]]
        <>" "<>ToFortranNumber[#[[3]]]<>"  # "<>ToString[#[[2]]]<>"\n"]&/@ xx[[2]];
];
WriteBlockDecay[xx_,file_]:=Module[{i},
     WriteString[file,"DECAY  "<>StringJoin[ToString[Abs[#]]<>" "&/@#[[1]]]
        <>" "<>ToFortranNumber[#[[3]]]<>"  # "<>ToString[#[[2]]]<>"\n"]&/@ xx[[2]];
];


If[MGEParamList =!= {},
   WriteBlock[#,myfile]&/@MGEParamList];
If[MGMassList =!= {},
   WriteBlockMass[MGMassList,myfile]];
If[MGWidthList =!= {},
   WriteBlockDecay[MGWidthList,myfile]];

WriteQNumbers["param_card.dat"];

Close["param_card.dat"];


myfile=OpenWrite["particles.dat"];
WriteString[myfile, "#This file has been generated automatically by FeynRules\n"];
WriteString[myfile, "\n# Version: " <> FR$VersionNumber <>  "   Date: " <> FR$DateFormat[] <>"\n"];
WriteString[myfile, "#This is a special data file which contains particles of the\n"];
WriteString[myfile, "# Model.The format for entering new particles is\n"];
WriteString[myfile,
    "#Particle codes taken from http://pdg.lbl.gov/2000/montecarlorpp.pdf\n\
"];
WriteString[myfile,
    "#Name anti_Name Spin Linetype Mass Width Color Label Model\n"];
WriteString[myfile,"#xxx xxxx SFV WSDC str str STO str PDG code\n\n"];
Do[
	MGPartList[[j,2,k,1]]=MGPartList[[j,2,k,1]]/.MGPartNameReplacements;
	MGPartList[[j,2,k,2]]=MGPartList[[j,2,k,2]]/.MGPartNameReplacements;
,{j,1,Length[MGPartList]},{k,1,Length[MGPartList[[j,2]]]}];
GoodPrinting[#[[2]],myfile]&/@ MGPartList;
Close["particles.dat"];


myfile=OpenWrite["input.inc"];
WriteString[myfile, "c This file has been generated automatically by FeynRules\n"];
WriteString[myfile, "c Version: " <> FR$VersionNumber <>  "   Date: " <> FR$DateFormat[] <>"\n\n"];
WriteString[myfile, "c External Parameters :\n"];
If[MGEParamList =!= {},
   WriteToFortranFile[myfile,
     ToFortranDeclaration[Flatten[#[[All,2]]&/@(DropInter/@MGEParamList)]]];
   WriteToFortranFile[myfile,
     ToFortranCommonBlock[Flatten[#[[All,2]]&/@(DropInter/@MGEParamList)],
       "external_params"]]];
WriteString[myfile,
    "\nc Internal Parameters :\n"];
If[MGIParamList =!= {},
   WriteToFortranFile[myfile,
     ToFortranDeclaration[Select[Flatten[Select[MGIParamList,Last[#]==False &][[All,1]]],!(#===G) &]]];
   WriteToFortranFile[myfile,
     ToFortranCommonBlock[Select[Flatten[Select[MGIParamList,Last[#]==False &][[All,1]]],!(#===G) &],
       "internal_params_R"]];
   If[Select[MGIParamList,Last[#]==True &] =!= {},
      WriteToFortranFile[myfile,
        ToFortranDeclarationComplex[Select[Flatten[Select[MGIParamList,Last[#]==True &][[All,1]]],!(#===G) &]]];
      WriteToFortranFile[myfile,
        ToFortranCommonBlock[Select[Flatten[Select[MGIParamList,Last[#]==True &][[All,1]]],!(#===G) &],
       "internal_params_C"]]]];
Close["input.inc"];


myfile=OpenWrite["coupl.inc"];
WriteString[myfile, "c This file has been generated automatically by FeynRules\n"];
WriteString[myfile, "c Version: " <> FR$VersionNumber <>  "   Date: " <> FR$DateFormat[] <>"\n\n"];
WriteString[myfile,"      double precision G\n"];
(*WriteString[myfile,"      double complex GG(2)\n"];
WriteString[myfile,"      common/strong/ GG,G\n\n"];*)
WriteString[myfile,"      common/strong/ G\n\n"];
WriteString[myfile,"      double complex gal(2)\n"];
WriteString[myfile,"      common/weak/ gal\n\n"];
WriteString[myfile,"      double precision DUM0\n"];
WriteString[myfile,"      common/FRDUM0/ DUM0\n\n"];
WriteString[myfile,"      double precision DUM1\n"];
WriteString[myfile,"      common/FRDUM1/ DUM1\n\n"];
WriteString[myfile,
    "\nc Masses : \n"];
WriteToFortranFile[myfile,
  ToFortranDeclaration[Flatten[MGMassList[[2]][[All,2]]]]];
WriteToFortranFile[myfile,
  ToFortranCommonBlock[Flatten[MGMassList[[2]][[All,2]]],"masses"]];
WriteString[myfile,
    "\nc Widths : \n"];
WriteToFortranFile[myfile,
  ToFortranDeclaration[Flatten[MGWidthList[[2]][[All,2]]]]];
WriteToFortranFile[myfile,
  ToFortranCommonBlock[Flatten[MGWidthList[[2]][[All,2]]],"widths"]];
Close["coupl.inc"];
myfile=OpenWrite["param_read.inc"];
DropInter2[xx__]:=List[#[[1]],First[#[[2]]],#[[2]][[-2]]]&/@Last[xx];
WriteParameterReading[param_,defval_]:="call LHA_get_real(npara,param,value,\""<>ToString[param,FortranForm]<>"\","<>ToString[param,FortranForm]<>",\""<>ToFortranNumber[defval]<>"\")";
If[MGEParamList =!= {},
   WriteToFortranFile[myfile,WriteParameterReading[#[[1]],#[[2]]]]&/@Flatten[#[[All,{2,3}]]&/@(DropInter2/@MGEParamList),1]];
If[MGMassList =!= {},
   WriteToFortranFile[myfile,WriteParameterReading[#[[1]],#[[2]]]]&/@MGMassList[[2]][[All,{2,3}]]];
If[MGWidthList =!= {},
   WriteToFortranFile[myfile,WriteParameterReading[#[[1]],#[[2]]]]&/@MGWidthList[[2]][[All,{2,3}]]];
Close["param_read.inc"];


myfile=OpenWrite["param_write.inc"];
WriteToFortranFile[myfile,"write(*,*)  ' External Params            '"];
WriteToFortranFile[myfile,"write(*,*)  ' ---------------------------'"];
If[MGEParamList =!= {},
   WriteToFortranFile[myfile,"write(*," <> If[CnumQ[# /. InvParamRules] === True, "2", "1"] <> ") '"<>ToString[#,FortranForm]<>"=',"<>ToString[#,FortranForm]]&/@Flatten[#[[All,2]]&/@(DropInter/@MGEParamList),1]];
WriteToFortranFile[myfile,"write(*,*)  ''"];
WriteToFortranFile[myfile,"write(*,*)  ' Internal Params            '"];
WriteToFortranFile[myfile,"write(*,*)  ' ---------------------------'"];
If[MGIParamList =!={},
WriteToFortranFile[myfile,"write(*," <> If[Last[#], "2", "1"] <> ") '"<>ToString[#[[1]],FortranForm]<>"=',"<>ToString[#[[1]],FortranForm]]&/@MGIParamList];
WriteToFortranFile[myfile,"write(*,*)  ''"];
WriteToFortranFile[myfile,"write(*,*)  ' Masses                     '"];
WriteToFortranFile[myfile,"write(*,*)  ' ---------------------------'"];
WriteToFortranFile[myfile,"write(*,3) '"<>ToString[#,FortranForm]<>"=',"<>ToString[#,FortranForm]]&/@MGMassList[[2]][[All,2]];
WriteToFortranFile[myfile,"write(*,*)  ''"];
WriteToFortranFile[myfile,"write(*,*)  ' Widths                     '"];
WriteToFortranFile[myfile,"write(*,*)  ' ---------------------------'"];
WriteToFortranFile[myfile,"write(*,3) '"<>ToString[#,FortranForm]<>"=',"<>ToString[#,FortranForm]]&/@MGWidthList[[2]][[All,2]];
WriteToFortranFile[myfile,"write(*,*)  ''"];
Close["param_write.inc"];
]; 



(* ::Section:: *)
(*WriteMGOutput*)


Options[WriteMGOutput] = {FlavorExpand -> Automatic, Output -> Automatic, DecomposeGluonVertex -> True, Exclude4Scalars -> False, Debug -> False, DialogBox -> On, 
      MaxVertices -> 50, IndexExpand -> {}, AuxiliaryGluonMass -> 10000, ConservedQuantumNumbers :> MR$QuantumNumbers, MaxParticles->4, Input -> {}};

WriteMGOutput[lags__, options__] := WriteMGOutput[{lags}, options] /; (And @@ ((Head[#] =!= Rule &) /@ {lags})) && (And @@ ((Head[#] === Rule &) /@ {options})) && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteMGOutput[lags__] := WriteMGOutput[{lags}] /; (And @@ ((Head[#] =!= Rule &) /@ {lags}))  && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteMGOutput[{lags__}, options___] := Block[{output, defname, FCListMG, vertlist, CreaListOutput, vertexlistpiece, vertexlistMG, fcl, cl, parttemplist, dirname, 
    olddir, GSList, GSMasses, GSDecays, IntMasses, IntWidth,
    tempMGdoublemasses, tempMGsinglemasses, pdgtomass, pdgtowidth, tempoutdir, neweparams = {}, tmplist = {}, goldsnghosts, MG$VertexCount =1,checkcappart,
    vertslistforMG = {}, dialogstring = "", MG$NoWarning = False, MG$FFVSverts, MG$SSSverts, MG$SSSSverts, MG$VSSverts, MG$VVVverts, MG$VVVVverts, MG$VVSverts, 
    MG$VVSSverts, MG$VVVSverts, MG$VVVVSverts, MG$FFTverts, MG$FFVTverts, MG$VVTverts, MG$VVVTverts, MG$SSTverts,MG$FFVSvertsN, MG$SSSvertsN, MG$SSSSvertsN, MG$VSSvertsN, MG$VVVvertsN, MG$VVVVvertsN, MG$VVSvertsN, 
    MG$VVSSvertsN, MG$VVVSvertsN, MG$VVVVSvertsN, MG$FFTvertsN, MG$FFVTvertsN, MG$VVTvertsN, MG$VVVTvertsN, MG$SSTvertsN, MG$Statistics, statverts, DCcoupl, Ccoupl, Rcoupl, CDCcoupl, ncoupl, fcl1, versionfile, indexp,
    vertslist5D, vertslistnot5D, conservedqns, input},

(*Debug mode *)

MG$Statistics = Debug /. {options} /. Options[WriteMGOutput];
MG$DialogBox = DialogBox /. {options} /. Options[WriteMGOutput];
MG$MaxParticles = MaxParticles/.{options}/.Options[WriteMGOutput];

(* Initialisation *)

Clear[MGEParamList, MGIParamList, MGPartList, MGMassList, MGWidthList, FRMGDefault];



    Print[" - - - FeynRules interface to MadGraph - - -"];
    Print["       C. Duhr, M. Herquet, 2009"];
    Print["       arXiv:0906.2474"];



    (* Setting the output directory *)
      tempoutdir = Output /. {options} /. Options[WriteMGOutput];
      If[tempoutdir =!= Automatic,
         dirname = If[Not[StringQ[tempoutdir]], tempoutdir = ToString[tempoutdir], tempoutdir],
         (*Default value *)
         dirname = StringReplace[StringJoin[M$ModelName, "_MG"],{" "-> "_"}]];
    If[Not[MemberQ[FileNames[], dirname]],    
       Print["Creating Directory ", dirname];
       CreateDirectory[dirname]];
       olddir = Directory[];
       If[$OperatingSystem === "Windows",
          SetDirectory[StringJoin[Directory[], "\\", dirname]],
          SetDirectory[StringJoin[Directory[], "/", dirname]]];

     versionfile=OpenWrite["ModelVersion.txt"];
     WriteString[versionfile, M$ModelName <> "_" <> StringReplace[StringJoin @@ MR$Date, {" " -> "", "-" -> "_", "." -> "_"}]];
     Close[versionfile];


    (* Getting started *)

    MG$AdditionalVerts = {};
    MG$AuxHeavyGluonMassLocal = AuxiliaryGluonMass /. {options} /. Options[WriteMGOutput];


    (*                                                          *)
    (*            Preparing the particle and parameter input     *)
    (*                                                          *)                                           
    Print["Checking particle list..."];
    (*To get a nices output, we first remove the ghosts and goldstones form the mass list. We do this by selecting the pdg's *)
    goldsnghosts = List[#]&/@Join[Select[Join@@PartList[[All,2]],#[[3]]===U&][[All,9]],Select[Join@@PartList[[All,2]],Last[#]=!=NoGS&][[All,9]]];
    MGMassList = Select[MassList[[2]],Not[MemberQ[goldsnghosts, #[[1]]]]&];
    MGMassList = {MASS, MGMassList};
    MGWidthList = Select[WidthList[[2]],Not[MemberQ[goldsnghosts, #[[1]]]]&];
    MGWidthList = {DECAY, MGWidthList};



    (* We have to rename all mass and width symbols that are the same*)
    tempMGdoublemasses = Select[MGMassList[[2]], (Count[MGMassList[[2,All,2]],#[[2]]] !=1)&];
    If[tempMGdoublemasses =!= {}, tmplist = KillDoubles[DeleteCases[tempMGdoublemasses, {_,_,Internal}][[All, 2]]]];
    If[tmplist =!= {}, neweparams = Table[{{dc}, {tmplist[[dc]], NumericalValue[tmplist[[dc]]], False, "Redefined mass"}}, {dc, Length[tmplist]}]];
    tempMGsinglemasses = Complement[MGMassList[[2]], tempMGdoublemasses];
    tempMGdoublemasses=Table[{tempMGdoublemasses[[kk,1]],Symbol[ToString[tempMGdoublemasses[[kk,2]]]<>"MG"<>ToString[kk]],If[tempMGdoublemasses[[kk,3]] === Internal, NumericalValue[tempMGdoublemasses[[kk,2]]], tempMGdoublemasses[[kk,3]]]},{kk,Length[tempMGdoublemasses]}];    
    MGMassList = {MASS, Join[tempMGsinglemasses,tempMGdoublemasses]};
    Do[pdgtomass[MGMassList[[2,kk,1]]] = MGMassList[[2,kk,2]], {kk, Length[MGMassList[[2]]]}];
    (* End renaming the masses *)


    Clear[tmplist];


    (* will now do the same for the width *)
    tempMGdoublemasses = Select[MGWidthList[[2]], (Count[MGWidthList[[2,All,2]],#[[2]]] !=1)&];
    If[tempMGdoublemasses =!= {}, tmplist = KillDoubles[DeleteCases[tempMGdoublemasses, {_,_,Internal}][[All, 2]]]];
    If[tmplist =!= {}, neweparams = Join[neweparams, Table[{{dc}, {tmplist[[dc]], NumericalValue[tmplist[[dc]]], False, "Redefined width"}}, {dc, Length[neweparams]+1, Length[neweparams]+Length[tmplist]}]]];
    tempMGsinglemasses = Complement[MGWidthList[[2]], tempMGdoublemasses];
    tempMGdoublemasses=Table[{tempMGdoublemasses[[kk,1]],Symbol[ToString[tempMGdoublemasses[[kk,2]]]<>"MG"<>ToString[kk]],tempMGdoublemasses[[kk,3]]},{kk,Length[tempMGdoublemasses]}];
    MGWidthList = {DECAY, Join[tempMGsinglemasses,tempMGdoublemasses]};
    Do[pdgtowidth[MGWidthList[[2,kk,1]]] = MGWidthList[[2,kk,2]], {kk, Length[MGWidthList[[2]]]}];
    (* End renaming the width *)

    (* Create MGPartList *)
    MGPartList = PartList;
    MGPartList = Cases[MGPartList, {{__}, {{__, NoGS}, ___}}];
    MGPartList = ({#1, ReplacePart[ReplacePart[#, pdgtomass[{#[[9]]}], 5], pdgtowidth[{#[[9]]}], 6]&/@ #2}&)@@@MGPartList;
    MGPartList = MGPartList //. {pdgtomass[_] -> ZERO, pdgtowidth[_] -> ZERO};
    GSList = Complement[PartList, MGPartList];
    GSMasses = If[GSList =!= {}, Select[(Sequence @@@ GSList[[All,2]])[[All,5]], Not[MatchQ[#, ZERO]]&], {}];
    GSDecays = If[GSList =!= {}, Select[(Sequence @@@ GSList[[All,2]])[[All,6]], Not[MatchQ[#, ZERO]]&], {}];
    If[Length[MGPartList] != Length[PartList], Print["Goldstone bosons will be ignored."]];
    MGPartList = RemoveSextetsFromPartList[MGPartList];
    MGPartList = ({#1, (Drop[#, -4]&) /@ #2} &)@@@ MGPartList;
    GSList = Cases[MGPartList, {{U[_], _}, {___}}];
    If[GSList =!= {}, GSMasses = Join[GSMasses, Select[(Sequence @@@ GSList[[All,2]])[[All,5]], Not[MatchQ[#, ZERO]]&]]];
    If[GSList =!= {}, GSDecays = Join[GSDecays, Select[(Sequence @@@ GSList[[All,2]])[[All,6]], Not[MatchQ[#, ZERO]]&]]];
    If[Not[FreeQ[MGPartList, U[_]]], MGPartList = DeleteCases[MGPartList, {{U[_], _}, {___}}]; Print["Ghost fields will be ignored."]];
    parttemplist = Flatten[MGPartList[[All,2]],1];

    MG$NoWarning = False;
    Do[If[Head[Last[parttemplist[[partkk]]]] === NoPDG, 
         If[Not[MG$NoWarning], dialogstring= dialogstring <> "- Some particles do not have assigned a PDG. Assigning automatic PDG codes.\n" ; MG$NoWarning = True]], {partkk, Length[parttemplist]}];
    MGPartList = MGPartList //. NoPDG -> Identity;

    MGPartNameReplacements = CreateParticleNameReplacements[MGPartList];
	MGMassList = MGMassList //. NoPDG -> Identity;
    IntMasses = (#2 &) @@@ Cases[MGMassList[[2]], {_, _, Internal}];
    MGMassList = MGMassList //. {a_, b_, Internal} :> {a, b, NumericalValue[b]};
    MGWidthList = MGWidthList //. NoPDG -> Identity;
    IntWidth = (#2 &) @@@ Cases[MGWidthList[[2]], {_, _, Internal}];
    MGWidthList = MGWidthList //. {a_, b_, Internal} :> {a, b, NumericalValue[b]};
    MG$NoWarning = False;
    Do[If[(MGMassList[[2, ml, 3]] === NoValue[1]) || (MGMassList[[2, ml, 3]] === NoValue), 
       If[Not[MG$NoWarning], dialogstring= dialogstring <> "- Some particles do not have assigned a value for the mass. Assigning default value 1.\n"; MG$NoWarning = True]], {ml, Length[MGMassList]}];
    MGMassList = MGMassList //. NoValue -> Identity;
    MG$NoWarning=False;
    Do[If[(MGWidthList[[2, ml, 3]] === NoValue[1]) || (MGWidthList[[2, ml, 3]] === NoValue), 
       If[Not[MG$NoWarning], dialogstring= dialogstring <> "- Some particles do not have assigned a value for the width. Assigning default value 1.\n"; MG$NoWarning = True]], {ml, Length[MGWidthList]}];
    MGWidthList = MGWidthList //. NoValue -> Identity;



    Print["Checking parameter list..."];
    MGEParamList = If[neweparams =!= {}, Append[EParamList, {FRMGDefault, neweparams}], EParamList];
  (*  MGEParamList = ({#1, (({#1, DecomposeEParam[#2]}&) @@@ #2)}&) @@@ MGEParamList;*)
    MGIParamList = DeleteCases[IParamList, {_?((MemberQ[IntMasses,#]||MemberQ[IntWidth,#])&),___}];
(*    MGIParamList = DecomposeIParam /@ MGIParamList;*)
    MGEParamList = ({#1, ({#1, Most[#2]} &) @@@ #2 } &) @@@ MGEParamList;
    MGIParamList = Most /@ MGIParamList;
    
    MG$NoWarning = False;
    Do[If[Head[MGEParamList[[kparam, 1]]] === NoBlockName, 
       If[Not[MG$NoWarning], dialogstring= dialogstring <> "- Some external parameters do not have assigned a BlockName. Assigning default block FRBlock.\n"; MG$NoWarning = True]],
       {kparam, Length[MGEParamList]}]; 
    MGEParamList = MGEParamList //. NoBlockName -> Identity;

  (*  Do[If[MatchQ[ParamList[[kparam]], {___,NoValue[_],___}],
       If[Not[MG$NoWarning], dialogstring= dialogstring <> "- Some parameters do not have assigned a value. Assigning default value 1.\n"; MG$NoWarning = True]],
       {kparam, Length[ParamList]}];*)
    MGEParamList = MGEParamList //. NoValue -> Identity;
    MGIParamList = MGIParamList //. NoValue -> Identity;
    MGIParamList = MGIParamList /. Pi -> N[Pi];

(* Create warning dialog box *)
If[MG$DialogBox === On,
   If[dialogstring =!= "", dialogstring = "Warning:\n\n"<> dialogstring <> "\nProceed anyway?";
      If[!ChoiceDialog[dialogstring,{"Ok"->True, "Abort"->False}],Abort[]]]];







    (*                                                          *)
    (*            Calculation of the vertices                   *)
    (*                                                          *)
    FR$DecomposeGluonVertex = DecomposeGluonVertex /. {options} /. Options[WriteMGOutput];
    FR$Exclude4Scalars = Exclude4Scalars /. {options} /. Options[WriteMGOutput];
    indexp = IndexExpand /. {options} /. Options[WriteMGOutput];
    defname = StringJoin["L", ToString[#]]& /@ Range[Length[{lags}]];
    $lagrangianListtemp = {};

(* 17.02,10, CD. Changed FlavorExpandOption from True to whatever *)

    MG$FlavExp = FlavorExpand /. {options} /. Options[WriteMGOutput] /. Automatic -> FR$AutoFlavorExpand /. {} -> False;


(* 07.01.11, CD, including Input option *)

    input = Input /. {options} /. Options[WriteMGOutput];
    If[input === {},
       Do[Print["\nCalculating Feynman rules for Lagrangian number ", kmg];
          vertexlistMG = {};
          vertexlistpiece ={};
          vertslistforMG = Append[vertslistforMG, FeynmanRules[List[lags][[kmg]], FlavorExpand -> MG$FlavExp, ScreenOutput -> False, 
                  Exclude4Scalars -> FR$Exclude4Scalars, IndexExpand -> indexp, ConservedQuantumNumbers -> False,MaxParticles->MG$MaxParticles]],
         {kmg, Length[defname]}];


       Print["Merging vertices..."];

       vertslistforMG = Join @@ vertslistforMG,
      (* else *)
       Print["Inputing vertices..."];
       vertslistforMG = input;
       vertexlistMG = {};
      ];

     

    (* Remove Sextets *)
    vertslistforMG = RemoveSextetsFromVertexList[vertslistforMG];

(*Print[vertslistforMG];*)



(************************************************)
(*   23.03.10                                  *)
(*   Started tackling speed issue.            *)
(************************************************)


MR$DefinitionsNoFields;
FR$AbbIndexSum={};
FR$AbbIndexSumCounter=1;
Clear[FRIndexSum];


vertslistforMG = FlavorExpansion[vertslistforMG, OptimizeFermionChains -> True];



(* The new optimization is insensitive to the order. Howeverm the VVVVS vertices need the scalar in the first entry *)
vertslist5D = Select[vertslistforMG, Length[#[[1]]] == 5&];
vertslistnot5D = Complement[vertslistforMG, vertslist5D];
vertslist5D = vertslist5D //. {fields1___, {v1_?(VectorFieldQ[#] === True&), kv1_}, {scal_?(ScalarFieldQ[#] === True&), kscal_}, fields2___} :> {fields1, {scal, kscal}, {v1, kv1}, fields2};
vertslist5D=RelabelExt /@ vertslist5D;
vertslistforMG = Join[vertslist5D, vertslistnot5D];


(* CD, 12.05.2010,
   We remove ghosts and goldstone fields form the list of vertices, since these are not supported by MG *)
vertslistforMG = RemoveGhostsAndGoldstones[vertslistforMG];


(* CD, 12.05.2010,
   Check for mass eigenstates *)
CheckMassEigenstates[vertslistforMG];

(* Check for QNumber conservation *)
conservedqns = (ConservedQuantumNumbers /. {options} /. Options[WriteMGOutput]);
If[(conservedqns =!= {}) && (conservedqns =!= False) ,
   ConserveQN[#, conservedqns]& /@ (((#1&)@@@#&)/@ vertslistforMG[[All,1]])];

MGIParamList = Join[MGIParamList, Most /@ FR$AbbIndexSumExpanded];

(*Print[vertslistforMG];*)
                  


    (*                                                          *)
    (*            Conversion to MG                              *)
    (*                                                          *)

      Print["\n    Converting vertices to MG format"];

       cl = vertslistforMG[[All, 1, All]];
       fcl = cl[[All, All, 1]];
       fcl1= fcl;
       vertlist = vertslistforMG[[All, 2]];

       (*    MG FCList   *)
       fcl = ReOrderFCforMG /@ fcl;


       FCListMG = (PartNameMG /@ List[##])& @@@ fcl;
       FCListMG = {Sequence[##], "MGVX"<>ToString[MG$VertexCount++]}& @@@ FCListMG;





       Do[CreaListOutput = PrintOutputMG[cl[[kvert]]];
          vertexlistpiece = Flatten[{CreaListOutput, vertlist[[kvert]]}];
          vertexlistMG = Append[vertexlistMG, List[fcl1[[kvert]], First[vertexlistpiece], Most[FCListMG[[kvert]]], Last[FCListMG[[kvert]]], Last[vertexlistpiece]]],
          {kvert, Length[fcl]}];
       vertexlistMG = MakeMG4Interaction /@ vertexlistMG;


   If[MG$Statistics, Print["      (* Starting Optimization        *)"]];

     FR$OptimizeParamsDefault = FR$OptimizeParams; 

     $OptimizedParamRules = $MakeOptimizedParamRules[];
     
     FR$OptimizeParams = DeleteCases[Union[FR$OptimizeParams, First/@ParamList],gs|G|ee|\[Alpha]S|\[Alpha]EW|\[Alpha]EWM1|aS|aEW];

      $OptimizedParams = {{Sqrt2, Sqrt[2],False}, {SqrtPi, Sqrt[Pi],False}};
      AppendTo[FR$OptimizeParams, Sqrt2];
      AppendTo[FR$OptimizeParams, SqrtPi];
  
     vertexlistMG = ({#1, #2,#3,#4, MakeOptimization[#5]}&) @@@ Expand[vertexlistMG];
 

     MGIParamList = Join[MGIParamList, $OptimizedParams];



(* CD, 15.09.10: Pi needs to be defined outisde MG *)
     MGIParamList = MGIParamList /. Pi -> FRPi;
     MGIParamList = Prepend[MGIParamList, {FRPi, N[Pi, 10], False}];
     vertexlistMG = vertexlistMG /. Pi -> FRPi;
 
     
     FR$OptimizeParams = FR$OptimizeParamsDefault;

   If[MG$Statistics, Print["      (* End Optimization        *)"]];

     

      Print["Getting color structures."];


      vertexlistMG =  CheckColorStructure[vertexlistMG];


     (* Initializing progress bars *)
       
       $helasprogress = 0;
       $partprogress = 0;
       $lengthvertexlistMG = Length[vertexlistMG];
       If[$VersionNumber >= 6, 
         Print["    Getting HELAS structures: ", Dynamic[$helasprogress], "/", $lengthvertexlistMG, " ."];
         Print[ProgressIndicator[Dynamic[progbarvar]]], 

          Print["    Getting HELAS structures."]];

$KEEP2=vertexlistMG;
    (* Seperating the Lorentz structures *)
      MG$FFVSverts = Cases[vertexlistMG, {_,"FFV",__}|{_,"FFS",__}];
      vertexlistMG = Complement[vertexlistMG, MG$FFVSverts];
      MG$SSSverts = Cases[vertexlistMG, {_,"SSS",__}];
      vertexlistMG = Complement[vertexlistMG, MG$SSSverts];
      MG$SSSSverts = Cases[vertexlistMG, {_,"SSSS",__}];  
      vertexlistMG = Complement[vertexlistMG, MG$SSSSverts]; 
      MG$VSSverts = Cases[vertexlistMG, {_,"VSS",__}];
      vertexlistMG = Complement[vertexlistMG, MG$VSSverts];
      MG$VVSverts = Cases[vertexlistMG, {_,"VVS",__}]; 
      vertexlistMG = Complement[vertexlistMG, MG$VVSverts];
      MG$VVSSverts = Cases[vertexlistMG, {_,"VVSS",__}]; 
      vertexlistMG = Complement[vertexlistMG, MG$VVSSverts];
      MG$VVVSverts = Cases[vertexlistMG, {_,"VVVS",__}];
      vertexlistMG = Complement[vertexlistMG, MG$VVVSverts];
      MG$VVVVSverts = Cases[vertexlistMG, {_,"VVVVS",__}];
      vertexlistMG = Complement[vertexlistMG, MG$VVVVSverts];
      MG$VVVverts = Cases[vertexlistMG, {_,"VVV",__}];
      vertexlistMG = Complement[vertexlistMG, MG$VVVverts];
      MG$VVVVverts = Cases[vertexlistMG, {_,"VVVV",__}];

(* New structures for Spin 2 *)
      MG$FFTverts = Cases[vertexlistMG, {_,"FFT",__}];
      vertexlistMG = Complement[vertexlistMG, MG$FFTverts];
      MG$FFVTverts = Cases[vertexlistMG, {_,"FFVT",__}];
      vertexlistMG = Complement[vertexlistMG, MG$FFVTverts];
      MG$VVTverts = Cases[vertexlistMG, {_,"VVT",__}];
      vertexlistMG = Complement[vertexlistMG, MG$VVTverts];
      MG$VVVTverts = Cases[vertexlistMG, {_,"VVVT",__}];
      vertexlistMG = Complement[vertexlistMG, MG$VVVTverts];
      MG$SSTverts = Cases[vertexlistMG, {_,"SST",__}];
      vertexlistMG = Complement[vertexlistMG, MG$SSTverts];



      MG$FFVSvertsN = Length[MG$FFVSverts];
      MG$SSSvertsN = Length[MG$SSSverts];
      MG$SSSSvertsN = Length[MG$SSSSverts];
      MG$VSSvertsN = Length[MG$VSSverts];
      MG$VVVvertsN = Length[MG$VVVverts];
      MG$VVVVvertsN = Length[MG$VVVVverts];
      MG$VVSvertsN = Length[MG$VVSverts];
      MG$VVSSvertsN = Length[MG$VVSSverts];
      MG$VVVSvertsN = Length[MG$VVVSverts];
      MG$VVVVSvertsN = Length[MG$VVVVSverts];

      MG$FFTvertsN = Length[MG$FFTverts];
      MG$FFVTvertsN = Length[MG$FFVTverts];
      MG$VVTvertsN = Length[MG$VVTverts];
      MG$VVVTvertsN = Length[MG$VVVTverts];
      MG$SSTvertsN = Length[MG$SSTverts];
      
   If[MG$Statistics, Print["      (* Statistics:             *)"];
                     Print["      (*    Before rejection     *)"];
                     Print["          FFV/FFS = ", MG$FFVSvertsN];
                     Print["          SSS       = ", MG$SSSvertsN];
                     Print["          SSSS       = ", MG$SSSSvertsN];
                     Print["          VSS       = ", MG$VSSvertsN];
                     Print["          VVV       = ", MG$VVVvertsN];
                     Print["          VVVV       = ", MG$VVVVvertsN];
                     Print["          VVS       = ", MG$VVSvertsN];
                     Print["          VVSS       = ", MG$VVSSvertsN];
                     Print["          VVVS       = ", MG$VVVSvertsN];
                     Print["          VVVVS       = ", MG$VVVVSvertsN];
                     Print["          FFT       = ", MG$FFTvertsN];
                     Print["          FFVT       = ", MG$FFVTvertsN];
                     Print["          VVT       = ", MG$VVTvertsN];
                     Print["          VVVT       = ", MG$VVVTvertsN];
                     Print["          SST       = ", MG$SSTvertsN];
                     Print["         "];
                     Print["      (*    Progress     *)"]];


(* FFV/ FFS structures *)
If[MG$FFVSvertsN !=0, $partprogressFFVS = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["     FFV/FFS: ", Dynamic[$partprogressFFVS], "/", MG$FFVSvertsN, " ."]];
   MG$FFVSverts = HELASFFVS @@@ MG$FFVSverts];

(* SSS structures *)
If[MG$SSSvertsN !=0, $partprogressSSS = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["        SSS: ", Dynamic[$partprogressSSS], "/", MG$SSSvertsN, " ."]];
   MG$SSSverts = HELASSSS @@@ MG$SSSverts];

(* SSSS structures *)
If[MG$SSSSvertsN !=0, $partprogressSSSS = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["       SSSS: ", Dynamic[$partprogressSSSS], "/", MG$SSSSvertsN, " ."]];
   MG$SSSSverts = HELASSSSS @@@ MG$SSSSverts];

(* VSS structures *)
If[MG$VSSvertsN !=0, $partprogressVSS = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["        VSS: ", Dynamic[$partprogressVSS], "/", MG$VSSvertsN, " ."]];
   MG$VSSverts = HELASVSS @@@ MG$VSSverts];

(* VVV structures *)
If[MG$VVVvertsN !=0, $partprogressVVV = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["        VVV: ", Dynamic[$partprogressVVV], "/", MG$VVVvertsN, " ."]];
   MG$VVVverts = HELASVVV @@@ MG$VVVverts];

(* VVVV structures *)
If[MG$VVVVvertsN !=0, $partprogressVVVV = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["       VVVV: ", Dynamic[$partprogressVVVV], "/", MG$VVVVvertsN, " ."]];
   MG$VVVVverts = HELASVVVV @@@ MG$VVVVverts];

(* VVS structures *)
If[MG$VVSvertsN !=0, $partprogressVVS = 0;
   MG$AddHEFTCouplings = {};
   MG$HEFTCounter = 1;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["        VVS: ", Dynamic[$partprogressVVS], "/", MG$VVSvertsN, " ."]];
   MG$VVSverts = HELASVVS @@@ MG$VVSverts;
   MG$VVSverts = Join[MG$VVSverts, MG$AddHEFTCouplings]];

(* VVSS structures *)
If[MG$VVSSvertsN !=0, $partprogressVVSS = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["       VVSS: ", Dynamic[$partprogressVVSS], "/", MG$VVSSvertsN, " ."]];
   MG$VVSSverts = HELASVVSS @@@ MG$VVSSverts];

(* VVVS structures *)
If[MG$VVVSvertsN !=0, $partprogressVVVS = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["       VVVS: ", Dynamic[$partprogressVVVS], "/", MG$VVVSvertsN, " ."]];
   MG$VVVSverts = HELASVVVS @@@ MG$VVVSverts];

(* VVVVS structures *)
If[MG$VVVVSvertsN !=0, $partprogressVVVVS = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["      VVVVS: ", Dynamic[$partprogressVVVVS], "/", MG$VVVVSvertsN, " ."]];
   MG$VVVVSverts = HELASVVVVS @@@ MG$VVVVSverts;];

(* FFT structures *)
If[MG$FFTvertsN !=0, $partprogressFFT = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["      VVVVS: ", Dynamic[$partprogressFFT], "/", MG$FFTvertsN, " ."]];
   MG$FFTverts = HELASFFT @@@ MG$FFTverts;];

(* FFVT structures *)
If[MG$FFVTvertsN !=0, $partprogressFFVT = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["      VVVVS: ", Dynamic[$partprogressFFVT], "/", MG$FFVTvertsN, " ."]];
   (*MG$FFVTverts = HELASFFVT @@@ MG$FFVTverts;*)];

(* VVT structures *)
If[MG$VVTvertsN !=0, $partprogressVVT = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["      VVVVS: ", Dynamic[$partprogressVVT], "/", MG$VVTvertsN, " ."]];
   (*MG$VVTverts = HELASVVT @@@ MG$VVTverts;*)];

(* VVVT structures *)
If[MG$VVVTvertsN !=0, $partprogressVVVT = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["      VVVVS: ", Dynamic[$partprogressVVVT], "/", MG$VVVTvertsN, " ."]];
  (* MG$VVVTverts = HELASVVVT @@@ MG$VVVTverts;*)];

(* SST structures *)
If[MG$SSTvertsN !=0, $partprogressSST = 0;
   If[MG$Statistics && ($VersionNumber >= 6),  Print["      VVVVS: ", Dynamic[$partprogressSST], "/", MG$SSTvertsN, " ."]];
   (*MG$SSTverts = HELASSST @@@ MG$SSTverts;*)];


(*(* Add additonal vertices, coming from octet VVSS *)


   MG$VVSverts = Join[MG$VVSverts, MG$AdditionalVerts];*)




   If[MG$Statistics,       MG$FFVSvertsN = Length[MG$FFVSverts];
               MG$SSSvertsN = Length[MG$SSSverts];
               MG$SSSSvertsN = Length[MG$SSSSverts];
               MG$VSSvertsN = Length[MG$VSSverts];
               MG$VVVvertsN = Length[MG$VVVverts];
               MG$VVVVvertsN = Length[MG$VVVVverts];
               MG$VVSvertsN = Length[MG$VVSverts];
               MG$VVSSvertsN = Length[MG$VVSSverts];
               MG$VVVSvertsN = Length[MG$VVVSverts];
               MG$VVVVSvertsN = Length[MG$VVVVSverts];
               MG$FFTvertsN = Length[MG$FFTverts];
               MG$FFVTvertsN = Length[MG$FFVTverts];
               MG$VVTvertsN = Length[MG$VVTverts];
               MG$VVVTvertsN = Length[MG$VVVTverts];
               MG$SSTvertsN = Length[MG$SSTverts];
      
                     Print["      (*    After rejection     *)"];
                     Print["          FFV/FFS = ", MG$FFVSvertsN];
                     Print["          SSS       = ", MG$SSSvertsN];
                     Print["          SSSS       = ", MG$SSSSvertsN];
                     Print["          VSS       = ", MG$VSSvertsN];
                     Print["          VVV       = ", MG$VVVvertsN];
                     Print["          VVVV       = ", MG$VVVVvertsN];
                     Print["          VVS       = ", MG$VVSvertsN];
                     Print["          VVSS       = ", MG$VVSSvertsN];
                     Print["          VVVS       = ", MG$VVVSvertsN];
                     Print["          VVVVS       = ", MG$VVVVSvertsN];
                     Print["          FFT       = ", MG$FFTvertsN];
                     Print["          FFVT       = ", MG$FFVTvertsN];
                     Print["          VVT       = ", MG$VVTvertsN];
                     Print["          VVVT       = ", MG$VVVTvertsN];
                     Print["          SST       = ", MG$SSTvertsN];
                     Print["         "];
                     Print["          Total = ", MG$FFVSvertsN+MG$SSSvertsN+MG$SSSSvertsN+MG$VSSvertsN+MG$VVVvertsN+
     MG$VVVVvertsN+MG$VVSvertsN+MG$VVSSvertsN+MG$VVVSvertsN+MG$VVVVSvertsN+MG$FFTvertsN+MG$FFVTvertsN+MG$VVTvertsN+MG$VVVTvertsN+MG$SSTvertsN];
                     Print["         "]];


(* Combing results *)


   If[MG$Statistics, statverts = Length[Join[MG$FFVSverts, MG$SSSverts, MG$SSSSverts, MG$VSSverts, MG$VVVverts, MG$VVVVverts, MG$VVSverts,MG$VVSSverts, MG$VVVSverts, MG$VVVVSverts, MG$FFTverts, MG$FFVTverts, MG$VVTverts, MG$VVVTverts, MG$SSTverts]]];

       DCcoupl = CollectCouplingsMG[Join[MG$FFVSverts, Cases[MG$VVSverts, {__, "DC"}], MG$FFVTverts]];
       Ccoupl = CollectCouplingsMG[Join[MG$SSSverts, MG$SSSSverts, Cases[MG$VVSverts, {__, "C"}], MG$VVSSverts, MG$VSSverts, MG$FFTverts, MG$VVTverts, MG$VVVTverts, MG$SSTverts]];
       Rcoupl = CollectCouplingsMG[Join[MG$VVVverts, MG$VVVVverts,MG$VVVSverts, MG$VVVVSverts]];

   CDCcoupl = Join[DCcoupl, Ccoupl];


   If[MG$Statistics, statverts = Join[CDCcoupl, Rcoupl]];
       CDCcoupl = DeleteCases[CDCcoupl, {_,_,_, MG$NoStruc ,___}|{_,_,_, 0 ,___}|{_,_,_,{0,0},___}];
       Rcoupl = DeleteCases[Rcoupl, {_,_,_, MG$NoStruc ,___}|{_,_,_, 0 ,___}|{_,_,_,{0,0},___}];

   If[MG$Statistics, Print["         Rejection:"];
              Print[TableForm[Complement[statverts,  statverts = Join[CDCcoupl, Rcoupl]]]]];

  (*     vertexlistMG = MakeDum0Sqrt /@ vertexlistMG;*)
   
       Rcoupl = AdjustMGInteractionOrder /@ Rcoupl;
       CDCcoupl = AdjustMGInteractionOrder /@ CDCcoupl;



  (* Breaking the coupl_defintion if they are too long. *)



       $BreakCoupl = MaxVertices/. {options} /. Options[WriteMGOutput];

       BreakList[lisi_List] := Block[{templist = lisi, newlist = {}},
                 While[Length[templist]>$BreakCoupl,
                       AppendTo[newlist,Take[templist,$BreakCoupl]];
                       templist= Drop[templist,$BreakCoupl]];
                       newlist=Append[newlist,templist]];



    (* Select those parameters depending on the strong coupling. Those should be recomputed on the fly for each event *)

      FR$RunningParameters = {aS, \[Alpha]S, G, gs};
      MGIParamListNotRun = {};
      MGIParamListRun = {};
      Do[If[(And @@ (FreeQ[MGIParamList[[pp, 2]], #]& /@ FR$RunningParameters) === False) && (MGIParamList[[pp, 1]] =!= G) && (MGIParamList[[pp, 1]] =!= gs) ,
            FR$RunningParameters=Union[FR$RunningParameters, {MGIParamList[[pp,1]] /. ParamRules, MGIParamList[[pp, 1]]}];
            AppendTo[MGIParamListRun, MGIParamList[[pp]]],
         (* else *)
            AppendTo[MGIParamListNotRun, MGIParamList[[pp]]]],
         {pp, Length[MGIParamList]}];





    (* We now do the same for the couplings *)
    CouplRun = {};
    CouplNotRun = {};


    Do[If[And @@ (FreeQ[Rcoupl[[pp, 4]], #]& /@ FR$RunningParameters) === False, 
            CouplRun = Append[CouplRun, Rcoupl[[pp]]],
         (* else *)
            AppendTo[CouplNotRun, Rcoupl[[pp]]]],
         {pp, Length[Rcoupl]}];

    Do[If[And @@ (FreeQ[CDCcoupl[[pp, 4]], #]& /@ FR$RunningParameters) === False,
            AppendTo[CouplRun, CDCcoupl[[pp]]],
         (* else *)
            AppendTo[CouplNotRun, CDCcoupl[[pp]]]],
         {pp, Length[CDCcoupl]}];
   

       VerticesMG[M$ModelName] = Join[Rcoupl, CDCcoupl];

       CouplRun = KillDoubles[ReplacePart[#, MR$Null, 2]& /@ CouplRun];
       VerticesMGCoupl["1"] = CouplRun;



      If[Length[CouplNotRun] <= $BreakCoupl, If[Length[CouplRun] != 0,
                   VerticesMGCoupl["2"] = CouplNotRun; ncoupl = 2, 
                   VerticesMGCoupl["1"] = CouplNotRun; ncoupl = 1],
          (* else *)

          CouplNotRun = KillDoubles[ReplacePart[#, MR$Null, 2]& /@ CouplNotRun];
          CouplNotRun = BreakList[CouplNotRun];

          If[Length[CouplRun] == 0,
             ncoupl = Length[CouplNotRun];
             Do[VerticesMGCoupl[ToString[kk]] = CouplNotRun[[kk]], {kk, 1, ncoupl-1}],   
             ncoupl = Length[CouplNotRun] + 1;
             Do[VerticesMGCoupl[ToString[kk]] = CouplNotRun[[kk-1]], {kk, 2, ncoupl}]]];


   If[MG$Statistics,  
                       Print["      (* End Statistics             *)"]];

       Print["    MG Output obtained for ", M$ModelName];


    WriteOutput;
    WriteInteractionsFiles[Table[ToString[kk], {kk, ncoupl}]];

    Print["MG / ME output written on ", dirname];
    SetDirectory[olddir];];


(* ::Section:: *)
(*HELAS Structure*)


(* ::Subsection::Closed:: *)
(*Color*)


MR$OrderDelta4[expr_] := expr //. {IndexDeltaNoExp[2,1] -> IndexDeltaNoExp[1,2], IndexDeltaNoExp[3,1] -> IndexDeltaNoExp[1,3], IndexDeltaNoExp[4,1] -> IndexDeltaNoExp[1,4],
                 IndexDeltaNoExp[3,2] -> IndexDeltaNoExp[2,3], IndexDeltaNoExp[4,2] -> IndexDeltaNoExp[2,4], IndexDeltaNoExp[4,3] -> IndexDeltaNoExp[3,4]};

ColourlessQ = (FreeQ[#, Index[Colour, _]] && FreeQ[#, Index[Gluon, _]])& ;



MyHELASComplex[0, a_?(# =!= 1 &)] := a* MyHELASComplex[0,1];
MyHELASRational[-a_, b_] := - MyHELASRational[a, b];
MyHELASRational[a_, -b_] := - MyHELASRational[a, b];
MyHELASRational[a_?((NumericQ[#] && (# < 0))&), b_] := - MyHELASRational[-a, b];
MyHELASRational[a_, b_?((NumericQ[#] && (# < 0))&)] := - MyHELASRational[a, -b];

CheckColorStructureDel[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{tempvert, delstruc, output},
    If[FreeQ[vertex, T] && FreeQ[vertex, f],
       tempvert= Factor[Expand[vertex]];
        output = Which[MatchQ[tempvert, _. * IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]]], 
                     {MRvertextype,vertextype, MGPartContent, vertexname,tempvert }/. IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]] :> 1,
             MatchQ[tempvert, _. * IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]]], 
                     {MRvertextype,vertextype, MGPartContent, vertexname,tempvert } /. IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]] :> 1,
             True, 
                    Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch],
        output = Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch]];

CheckColorStructureT[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{tempvert, delstruc, output},
    If[FreeQ[vertex, f[_,_,_]],
       tempvert= Factor[Expand[vertex] /. T[a_,i_, k_]T[b_,k_,j_] :> TensDot[T[a],T[b]][i,j]];
        output = Which[MatchQ[tempvert, _. * T[Index[Gluon,_], Index[Colour, _], Index[Colour, _]]], 
                     {MRvertextype,vertextype, MGPartContent, vertexname,tempvert }/. T[Index[Gluon,_], Index[Colour, _], Index[Colour, _]] :> 1,
                       MatchQ[tempvert, _.*(TensDot[T[a_], T[b_]][i_, j_]+TensDot[T[b_], T[a_]][i_, j_])]&& FreeQ[temp, f],
                     {MRvertextype,vertextype, MGPartContent, vertexname,tempvert } /.  {TensDot[ T[_], T[_]][_, _] -> 1/2},
             True, 
                    Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch],
        output = Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch]];

CheckColorStructureEps[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{tempvert, delstruc, output, EpsSign, mrvt},
    If[FreeQ[vertex, T|Gluon|IndexDelta[Index[Colour,_], Index[Colour, _]]],
       tempvert= Factor[Expand[vertex]];

        output = If[MatchQ[tempvert, _. * Eps[Index[Colour,_], Index[Colour, _], Index[Colour, _]]],
               mrvt = PartName /@ (MRvertextype  /. {ff_?((FieldQ[#] && Not[DiracFieldQ[#] === True] && Not[MajoranaFieldQ[#] === True])&) :> anti[ff]}
                                  /. {ff_?(((DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True)) && (AntiFieldQ[#] === True) &) :> anti[ff]}
                                  /. CC[ff_] :> anti[ff]);

                     (* Determine the of the relative sign of vertextype with respect to MGPartContent *)
                     EpsSign = Signature[FindListPermutation[MGPartContent, mrvt]];

                     {MRvertextype,vertextype, MGPartContent, vertexname, EpsSign * tempvert }/. Eps[Index[Colour,_], Index[Colour, _], Index[Colour, _]] :> 1,
                    Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch],
        output = Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch]];

CheckColorStructureF[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{tempvert, delstruc, output},
       tempvert = Expand[vertex] /. {f[aa1_, aa2_,bb_]f[aa3_,aa4_,bb_] :> f[aa1,aa2,aa3,aa4], 
           f[aa1_, aa2_,bb_]f[aa3_,bb_,aa4_] :> -f[aa1,aa2,aa3,aa4],
           f[aa1_, aa2_,bb_]f[bb_,aa3_,aa4_] :> f[aa1,aa2,aa3,aa4],
           f[aa1_, bb_,aa2_]f[aa3_,aa4_,bb_] :> -f[aa1,aa2,aa3,aa4],
           f[aa1_, bb_,aa2_]f[aa3_,bb_,aa4_] :> f[aa1,aa2,aa3,aa4],
           f[aa1_, bb_,aa2_]f[bb_,aa3_,aa4_] :> -f[aa1,aa2,aa3,aa4],
           f[bb_, aa1_,aa2_]f[aa3_,aa4_,bb_] :> f[aa1,aa2,aa3,aa4],
           f[bb_, aa1_,aa2_]f[aa3_,bb_,aa4_] :> -f[aa1,aa2,aa3,aa4],
           f[bb_, aa1_,aa2_]f[bb_,aa3_,aa4_] :> f[aa1,aa2,aa3,aa4]};

       If[FreeQ[tempvert, f[_,_,_,_]],
           (* three gluon structure *)
           tempvert= Factor[tempvert];
           output = Which[MatchQ[tempvert, _. * f[Index[Gluon,_], Index[Gluon, _], Index[Gluon, _]]], 
                     {MRvertextype,vertextype, MGPartContent, vertexname,tempvert },
             True, 
                    Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch],
  
           (* four gluon structure *)
           tempvert= Collect[tempvert, f[__]];
           output = Which[MatchQ[tempvert, g1_.*f[a1_, a3_,a2_,a4_]+g2_.*f[a1_, a2_,a3_,a4_]+g3_.*f[a1_, a4_,a2_,a3_]],
                             {MRvertextype,vertextype, MGPartContent, vertexname,tempvert }, 
                           (* SSVV Octets *)
                          MatchQ[tempvert, g_.*f[a1_, a2_,a3_,a4_]+g_.*f[a1_, a4_,a3_,a2_]], 
                             {MRvertextype,vertextype, MGPartContent, vertexname,Coefficient[tempvert, f[Index[Gluon, Ext[1]] ,Index[Gluon, Ext[3]], Index[Gluon, Ext[2]],Index[Gluon, Ext[4]]], 1] * MG$VVSSOctet }, 
                         True, 
                             Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch]]];

CheckColorStructureD[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{tempvert, delstruc, output, fpart, dpart},
       If[FreeQ[vertex, f[_,_,_]],
           (* Pure D structure structure *)
           tempvert= Factor[vertex];
           output = Which[MatchQ[tempvert, _. * dSUN[Index[Gluon,_], Index[Gluon, _], Index[Gluon, _]]], 
                     {MRvertextype,vertextype, MGPartContent, vertexname,tempvert * MG$DColStruc /. dSUN[__] :> 1 },
             True, 
                    Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch],
  
           (* F+D structure *)
           tempvert= Collect[vertex, f[__],Collect[#, dSUN[__]]&];
           output = Which[MatchQ[tempvert, _ * dSUN[Index[Gluon,_], Index[Gluon, _], Index[Gluon, _]] +  _?(FreeQ[#, dSUN[__]]&) * f[Index[Gluon,_], Index[Gluon, _], Index[Gluon, _]]],
                             dpart = Coefficient[tempvert /. dSUN[__] :> $fabcstruc, $fabcstruc];
                             fpart = tempvert /. dSUN[__] :> 0;
                             AppendTo[AddToFTerms, {MRvertextype,vertextype, MGPartContent, vertexname,fpart}];
                            {MRvertextype,vertextype, MGPartContent, vertexname,dpart * MG$DColStruc   },  
                         True, 
                             Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch];
output]];

        

CheckColorStructure[vertlist_] := Block[{nocolstruc, colstruc, delstruc, Tstruc, fstruc,dstruc, epsstruc},
      nocolstruc= Select[vertlist, (FreeQ[#,Colour] && FreeQ[#,Gluon]) & ];
      colstruc = Complement[vertlist, nocolstruc];
     epsstruc = Select[colstruc, Not[FreeQ[#, Eps[Index[Colour, _], Index[Colour, _], Index[Colour, _]]]]&];
      colstruc = Complement[colstruc, epsstruc];      
      dstruc = Select[colstruc, Not[FreeQ[#, dSUN]]&];
      colstruc = Complement[colstruc, dstruc];
     
   If[colstruc =!= {},
      fstruc = Select[colstruc , (FreeQ[#, T] && FreeQ[#,IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]]])&];
      colstruc = Complement[colstruc, fstruc]; 
      Tstruc = Select[colstruc, FreeQ[#,IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]]]&];
      delstruc = Complement[colstruc, Tstruc];

   If[MG$Statistics, Print["      (* Statistics:             *)"];
                     Print["      (*    Before rejection     *)"];
                     Print["          NoColor = ", Length[nocolstruc]];
                     Print["          \[Delta]       = ", Length[delstruc]];
                     Print["          T       = ", Length[Tstruc]];
                     Print["          f       = ", Length[fstruc]];
                     Print["          d       = ", Length[dstruc]];
                     Print["          \[Epsilon]       = ", Length[epsstruc]];
                     Print["         "]];
      
   AddToFTerms = {};


      (* Checking the color structures *)
       If[delstruc =!= {}, delstruc = CheckColorStructureDel @@@ delstruc];
       If[Tstruc =!= {}, Tstruc = CheckColorStructureT @@@ Tstruc];
       If[fstruc =!= {}, fstruc = CheckColorStructureF @@@ fstruc];
       If[dstruc =!= {}, dstruc = CheckColorStructureD @@@ dstruc];
       If[epsstruc =!= {}, epsstruc = CheckColorStructureEps @@@ epsstruc];

       If[AddToFTerms =!= {}, fstruc = Join[fstruc, AddToFTerms]];


      (* Reject dSUN structure *)
 (*     If[dstruc =!={}, Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; dstruc ={}];*)

   If[MG$Statistics, 
                     Print["      (*    After rejection     *)"];
                     Print["          NoColor = ", Length[nocolstruc]];
                     Print["          \[Delta]       = ", Length[delstruc]];
                     Print["          T       = ", Length[Tstruc]];
                     Print["          f       = ", Length[fstruc]];
                     Print["          d       = ", Length[dstruc]];
                     Print["          \[Epsilon]       = ", Length[epsstruc]];
                     Print["      (* End Statictics     *)"]];

       (* Combining the results *)
       colstruc = DeleteCases[Join[delstruc, Tstruc, fstruc, dstruc, epsstruc], $NoColMatch];
       Join[nocolstruc, colstruc], vertlist]];


(* ::Subsection::Closed:: *)
(*FFVS Structure*)


HELASFFVS[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, ord, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressFFVS++;
 
  progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};
      temp = temp /. f[__] -> 1;

           (* Extract L-R coupling *)
      temp = temp //. TensDot[t1_?($TensClass[#] === MR$GammaMatrices &), t2___][r_,s_] -> GaAlgebra[t1, t2, r, s];
      temp = Expand[temp*GaAlgebra[]];
      temp = temp /. GaAlgebra[xx__, r_, s_] -> GaAlgebra[xx];
      temp = temp //. {GaAlgebra[] -> 1,
                       IndexDelta[Index[Spin, s_], Index[Spin, r_]] -> GaAlgebraDone[ProjP] + GaAlgebraDone[ProjM],
                       GaAlgebra[5] -> GaAlgebraDone[ProjP] - GaAlgebraDone[ProjM], 
                       GaAlgebra[nu_] :> GaAlgebraDone[nu ,ProjP] + GaAlgebraDone[nu, ProjM] /; (nu =!= 5) && (nu =!= ProjP) && (nu =!= ProjM), 
                       GaAlgebra[mu__, nu_] :> GaAlgebraDone[mu, nu, ProjP] + GaAlgebraDone[mu, nu, ProjM] /; (nu =!= 5) && (nu =!= ProjP) && (nu =!= ProjM),
                       GaAlgebra[mu__, 5] -> GaAlgebraDone[mu, ProjP] - GaAlgebraDone[mu, ProjM]};
       temp = temp //. GaAlgebraDone -> GaAlgebra;  
           (* End Extract L-R coupling *)

       temp = Collect[temp, {GaAlgebra[___, ProjP], GaAlgebra[___, ProjM]}, Simplify];

           (* Check Lorentz structure *)
       If[MatchQ[temp, (_. *GaAlgebra[mu_, ProjP] + _.*GaAlgebra[mu_, ProjM]) | (_.*GaAlgebra[mu_, ProjP] )| ( _.*GaAlgebra[mu_, ProjM])| 
                       (_.*GaAlgebra[ProjP] + _.*GaAlgebra[ProjM]) | (_.*GaAlgebra[ProjP])| ( _.*GaAlgebra[ProjM])],
          temp = temp //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = temp //.{GaAlgebra[mu___,ProjP] -> GaAlgebra[mu]Proj[2],
                        GaAlgebra[mu___,ProjM] -> GaAlgebra[mu]Proj[1]};
                temp = Collect[temp, Proj[_], Simplify];
                temp = If[vertextype === "FFS", -1, 1] * {I * Coefficient[temp, Proj[1]], I * Coefficient[temp, Proj[2]]} //. GaAlgebra[___] -> 1 ,
          Message[MG::NoStructure];
            Print["No HELAS structure found for ", ToString[MGPartContent], ". Vertex ignored."];
                temp = MG$NoStruc ];
           (* End Check Lorentz structure *)

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      ord = If[Not[FreeQ[temp, MG$DColStruc]], temp = temp /. MG$DColStruc -> 1; Append[ord, {"^dabc^", 1}], ord];
      output = {vertextype, MGPartContent, vertexname, temp, ord,"DC"}];    

        


(* ::Subsection::Closed:: *)
(*SSS*)


HELASSSS[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, ord, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressSSS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex /. f[__] -> 1;

      temp = Expand[-I*temp];

     If[Not[FreeQ[temp,FV|SP]],
        temp = MG$NoStruc;
        Message[MG::NoStructure];
        Print["No HELAS structure found for ", ToString[MGPartContent], ". Vertex ignored."];
       ];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      ord = If[Not[FreeQ[temp, MG$DColStruc]], temp = temp /. MG$DColStruc -> 1; Append[ord, {"^dabc^", 1}], ord];
      output = {vertextype, MGPartContent, vertexname, temp, ord,"C"}];

      




(* ::Subsection::Closed:: *)
(*SSSS*)


HELASSSSS[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, ord, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressSSSS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex /. f[__] -> 1;

      temp = Expand[-I*temp];

     If[Not[FreeQ[temp,FV|SP]],
        temp = MG$NoStruc;
        Message[MG::NoStructure];
        Print["No HELAS structure found for ", ToString[MGPartContent], ". Vertex ignored."];
       ];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      output = {vertextype, MGPartContent, vertexname, temp, ord,"C"}];

      




(* ::Subsection:: *)
(*VVS *)


HELASVVS[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, ord, rcdc, output, epspart, tempheft},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};
      temp = temp /. f[__] -> 1;

     (* The higgs effective coupling *)
temp=Expand[temp];
If[Not[FreeQ[temp, Eps]], epspart = Select[Expand[temp], Not[FreeQ[#, Eps]]&]; 
                             temp = Expand[temp] /. Eps[_,_,_,_] :> 0; 
                             epspart = epspart //.Eps[mu1_,mu2_,a_,b_]FV[ii_,a_]FV[jj_,b_]:>Eps[mu1,mu2,MG$AA,MG$BB]FV[ii,MG$AA]FV[jj,MG$BB]//.Eps[mu1_,mu2_,MG$AA,MG$BB]FV[ii_,MG$AA]FV[jj_,MG$BB]:>-Eps[mu1,mu2,MG$AA,MG$BB]FV[jj,MG$AA]FV[ii,MG$BB]/;jj<ii;
                             epspart = epspart/. {Eps[__] -> 1, FV[__]->1, IndexDelta[__]->1};
                             epspart = Expand[-I*epspart] //. {MyHELASComplex -> Complex, MyHELASRational -> Rational},
    (* else*) epspart = 0];

(* 15.06, 2010: We could have vertices that involve both an SM coupling, as well as HEFT, e.g. HZZ *)
tempheft = 0;
If[ Not[FreeQ[temp, SP|FV]],
    tempheft = Select[Expand[temp], Not[FreeQ[#, SP|FV]]&];
    temp = temp - tempheft // Expand;
    tempheft = Collect[tempheft, {ME[__], SP[__], FV[__]}];
    If[MatchQ[tempheft, _. * ME[__] SP[ii_,jj_] + _. * FV[ii_,_] FV[jj_,_]],
       tempheft= tempheft /. IndexDelta[__]-> 1/. ME[__] -> MEAB;
       tempheft = Expand[I*Coefficient[tempheft, MEAB,1]] /. SP[__]->1;
       rcdc = "DC";  
       tempheft = tempheft //. {MyHELASComplex -> Complex, MyHELASRational -> Rational}
   ]
];

(* combine with the epssilon part *)

tempheft = {tempheft, epspart};

If[tempheft =!= {0,0},
   rcdc = "DC";
   tempheft = tempheft //. TensDot -> ExpandTensDotForMG;
   tempheft = Expand[tempheft];
   tempheft = tempheft //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
   tempheft = tempheft //. MR$Definitions;
   tempheft = tempheft //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
   tempheft = tempheft //. $HCHELAS -> HC;
   tempheft = DecomposeEI /@ tempheft; 
   tempheft = Expand[tempheft];
   tempheft = tempheft //. ParamRules;
   ord = If[tempheft[[1]] ===0,
            GetOrder[tempheft[[2]]], 
            GetOrder[tempheft[[1]]]
   ];
   ord = If[Not[FreeQ[tempheft, MG$DColStruc]], tempheft = tempheft /. MG$DColStruc -> 1; Append[ord, {"^dabc^", 1}], ord];
   AppendTo[MG$AddHEFTCouplings, {vertextype, MGPartContent, {"HEFT" <> ToString[MG$HEFTCounter++]}, tempheft, Append[ord, {H,1}], "DC"}]
];

                     
      (* End heft *)
      rcdc = "C";
      temp = Collect[temp, {ME[__]}, Simplify];
  
      If[(temp =!= 0) && Not[MatchQ[temp, _.*ME[a_,b_]]],
            Message[MG::NoStructure];
            Print["No HELAS structure found for ", ToString[MGPartContent], ". Vertex ignored."];
                temp = MG$NoStruc,
(*else*)  
                temp = Expand[-I*temp] //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = temp //. ME[__] -> 1];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      ord = If[Not[FreeQ[temp, MG$DColStruc]], temp = temp /. MG$DColStruc -> 1; Append[ord, {"^dabc^", 1}], ord];
      output = {vertextype, MGPartContent, vertexname, temp, ord,rcdc}];    

        


(* ::Subsection:: *)
(*VVSS*)


HELASVVSS[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, ord, output, heavygluon, heavygluonmass, heavygluonpdg, antiheavygluon, gluon, scal1, scal2},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVSS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};

(*We have to treat separately the case of octet scalars. They need be split up with a heavy gluon. 
  At this stage, only SS GG couplings are allowed, where the two gluons are identical (e.g. one gluon and one KK gluon is not supported)
*)


If[Not[FreeQ[temp, MG$VVSSOctet]],
            If[(Length[Union[Cases[MRvertextype, _?VectorFieldQ]]] != 1) || (Length[Union[Cases[MRvertextype, _?ScalarFieldQ]]] != 1),
               Print["SSVV couplings for scalar octets with different gluons or different scalars are not supported. Vertex ignored."];
               temp = MG$NoStruc,
            (*else *)
               gluon = Union[Cases[MRvertextype, _?VectorFieldQ]][[1]];
               scal1 = Cases[MRvertextype, _?ScalarFieldQ][[1]];
               heavygluon = Symbol["xg" <> ToString[MG$HeavyGluonCount]];
               heavygluonmass = Symbol["M" <> ToString[heavygluon]];
               heavygluonpdg = 9000000+(MG$HeavyGluonCount++);
               MGPartList = Append[MGPartList, {{V[heavygluon], heavygluon}, {{ToString[heavygluon], ToString[heavygluon], V, C, heavygluonmass, ZERO, O, ToString[heavygluon], heavygluonpdg}}}];
               MGMassList = {MGMassList[[1]], Append[MGMassList[[2]], {{heavygluonpdg}, heavygluonmass, MG$AuxHeavyGluonMassLocal}]};
               temp = temp]];

       

     (* The higgs effective coupling *)
temp=Expand[temp];


      temp = Collect[temp, {ME[__]}, Simplify];
  
      If[Not[MatchQ[temp, _.*ME[a_,b_]]],
            Message[MG::NoStructure];
            Print["No HELAS structure found for ", ToString[MGPartContent], ". Vertex ignored."];
                temp = MG$NoStruc,
(*else*)  
                temp = Expand[-I*temp] //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = temp //. ME[__] -> 1];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      output = Which[Not[FreeQ[temp, MG$VVSSOctet]], temp = temp /. MG$VVSSOctet -> 1;
                                    {"VVS", {ToString[gluon], ToString[heavygluon], ToString[scal1]} , vertexname, I * heavygluonmass * Sqrt[temp] , {{ord[[1,1]], 1}},"C"},
                     True, {vertextype, MGPartContent, vertexname, temp, ord,"C"}]];    

        


(* ::Subsection::Closed:: *)
(*VVV*)


HELASVVV[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, ord, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVV++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};

      temp = temp  //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = Coefficient[temp, FV[1,Index[Lorentz, Ext[3]]] ME[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]] /. f[__] :>1;

                Which[Re[(NumericalValue[temp] /. NoValue[1] -> 1)] == 0, temp = Expand[I * temp];,
                      Im[(NumericalValue[temp] /. NoValue[1] -> 1)] == 0, temp = temp,
                      True, Print["Warning: Complex VVV coupling encountered for ", ToString[MGPartContent], "."];
                            Print["         Keeping only the real part."];
                            temp = Re[temp]];


      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      ord = If[Not[FreeQ[temp, MG$DColStruc]], temp = temp /. MG$DColStruc -> 1; Append[ord, {"^dabc^", 1}], ord];
      output = {vertextype, MGPartContent, vertexname, temp, ord,"R"}];


(* ::Subsection::Closed:: *)
(*VVVS*)


HELASVVVS[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, temp1,ord, output, epspart, nonepspart, rcdc, nonepspart1},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVVS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};

(* Separation of the scalar and pseudo-scalar couplings *)

temp = Expand[temp];
rcdc = "DC";

epspart = Select[temp, Not[FreeQ[#, Eps]]&];
nonepspart = temp /. Eps[_,_,_,_] :> 0;

(* Treating the pseudo-scalar part *)
      epspart = Expand[epspart /. {Eps[m1_,m2_,m3_, a_] FV[ii_, a_] :> Eps[m1,m2,m3,MG$AA]FV[ii,MG$AA]}]; 
      epspart = epspart /. {Eps[__] ->1, f[__]->1, FV[__]->1};
      epspart = epspart  //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
      epspart = Expand[epspart/3];

(* Treating the scalar part *)
      nonepspart = nonepspart  //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
      nonepspart1 = nonepspart;
      nonepspart = Coefficient[nonepspart, FV[1,Index[Lorentz, Ext[3]]] ME[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]] /. f[__] :> 1 /. I -> 1;
      If[nonepspart === 0, nonepspart = Coefficient[nonepspart1, FV[2,Index[Lorentz, Ext[4]]] ME[Index[Lorentz, Ext[2]], Index[Lorentz, Ext[3]]]] /. f[__] :> 1 /. I -> 1];
      nonepspart = Abs[nonepspart] /. Abs -> Identity;

(* Combining the two parts *)
      temp = {-nonepspart, -epspart};

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      ord = If[Not[FreeQ[temp, MG$DColStruc]], temp = temp /. MG$DColStruc -> 1; Append[ord, {"^dabc^", 1}], ord];
      output = {vertextype, MGPartContent, Append[vertexname, "DUM1"], temp,ord, rcdc} /. {hh_ * QCD, 2} :> {hh, 1} ];


(* ::Subsection::Closed:: *)
(*VVVVS*)


HELASVVVVS[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, temp1,temp2,vecpart, scalpart,tens,tmpprtct,tmpvertname,
     tmpvecvertname,ord,output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVVVS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};


(* Put to zero the Eps part *)
If[Not[FreeQ[temp, Eps]], temp = temp /. Eps[_,_,_,_] :> 0];



(* Treat the scalar part *) 
      If[(Length[KillDoubles[MGPartContent]] == 2) && (temp =!= 0), 
                temp = temp //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = temp //. f[aa1_,aa2_,aa3_,aa4_] :> f[aa1, aa2, $DumInd]f[aa3, aa4, $DumInd];
                temp = Expand[temp] //. f[ind___] :> Eps[ind] //. IndexDelta[Index[Gluon,a1_], Index[Gluon,a2_]] :> delta[Index[Gluon,a1], Index[Gluon,a2]];
                temp = Expand[temp] //. delta -> IndexDeltaNoExp;
                temp = temp //. IndexDeltaNoExp[Index[_,Ext[i_]], Index[_, Ext[j_]]] -> IndexDeltaNoExp[i,j];
                temp = Simplify[MR$OrderDelta4[Expand[temp]]];
                If[Not[FreeQ[temp,IndexDeltaNoExp]],temp = Coefficient[temp, IndexDeltaNoExp[2,3]IndexDeltaNoExp[4,5]]];
                temp1 = Coefficient[temp, ME[Index[Lorentz, Ext[2]], Index[Lorentz, Ext[3]]]ME[Index[Lorentz, Ext[4]],Index[Lorentz, Ext[5]]]] /. I -> 1;
                temp2 = Coefficient[temp, ME[Index[Lorentz,Ext[2]], Index[Lorentz, Ext[4]]]ME[Index[Lorentz, Ext[3]],Index[Lorentz,Ext[5]]]] /. I -> 1;
                temp1 = Abs[temp1] /. Abs -> Identity;
                temp2 = Abs[temp2] /. Abs -> Identity;
                temp = If[temp1 === Expand[-2*temp2], temp2, temp1];

         vecpart = If[Length[KillDoubles[Rest[MGPartContent]]] == 1, MGPartContent[[2]], First[MGPartContent]];
         scalpart = Identity @@ DeleteCases[MGPartContent, vecpart];
         If[MemberQ[FR$DecomposedGluons, vecpart], tens = vecpart /. FR$DecomposedGluonsTensors,
            FR$DecomposedGluons = Append[FR$DecomposedGluons, vecpart]; 
            tens = ToExpression["T"<>ToString[MG$AuxTensCount++]];
            FR$DecomposedGluonsTensors = Append[FR$DecomposedGluonsTensors, MakeRule[MGPartContent[[1]], tens]/.MakeRule->Rule]];
         tmpprtct = {tens, tens, scalpart};
         tmpvertname = StringJoin @@ (ToString /@ tmpprtct);
         tmpvecvertname = ToString[vecpart] <> ToString[vecpart] <> ToString[tens];
      temp = Expand[temp / ToExpression[tmpvecvertname]^2]];


      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      output = {"STT", tmpprtct, vertexname, temp,Append[ord, {a, 1}], "R"} /. {hh_ * QCD, 2} :> {hh, 1}];





(* ::Subsection::Closed:: *)
(*VVVV*)


HELASVVVV[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, temp1,temp2,vecpart, scalpart,tens,tmpprtct,colless, 
     tmpvertname,tmpvecvertname,ord, output, MG$DecGlu,MG$WeakVVVV},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVVV++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};

colless = ColourlessQ[temp];

 temp = temp //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = temp //. f[aa1_,aa2_,aa3_,aa4_] :> f[aa1, aa2, $DumInd]f[aa3, aa4, $DumInd];
                temp = Expand[temp] //. f[ind___] :> Eps[ind] //. IndexDelta[Index[Gluon,a1_], Index[Gluon,a2_]] :> delta[Index[Gluon,a1], Index[Gluon,a2]];
                temp = Expand[temp] //. delta -> IndexDeltaNoExp;
                temp = temp //. IndexDeltaNoExp[Index[_,Ext[i_]], Index[_, Ext[j_]]] -> IndexDeltaNoExp[i,j];
                temp = Simplify[MR$OrderDelta4[Expand[temp]]];
                If[Not[FreeQ[temp,IndexDeltaNoExp]],temp = Coefficient[temp, IndexDeltaNoExp[1, 2]IndexDeltaNoExp[3,4]]];
 
                temp1 = Coefficient[temp, ME[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]ME[Index[Lorentz, Ext[3]],Index[Lorentz, Ext[4]]]];
                temp2 = Coefficient[temp, ME[Index[Lorentz,Ext[1]], Index[Lorentz, Ext[3]]]ME[Index[Lorentz, Ext[2]],Index[Lorentz,Ext[4]]]];

                temp1 = temp1 /. Complex[0,a_] :>a;
                temp2 = temp2 /. Complex[0,a_] :>a;

                temp = Which[Simplify[temp2/temp1] == 1, temp1,
                             Simplify[temp2/temp1] == -2, temp1,
                             Simplify[temp2/temp1] == -1/2, temp2,
                             True, Print["Warning: 4-vector coupling ", MGPartContent, " does not math HELAS structure! Coupling ignored"]; 0];

If[colless, temp = temp * MG$WeakVVVV];

If[FR$DecomposeGluonVertex && Not[colless] && (MGPartContent[[1]] === MGPartContent[[2]]) && (MGPartContent[[1]] === MGPartContent[[3]]) && (MGPartContent[[1]] === MGPartContent[[4]]),  
If[(NumericalValue[temp] /. NoValue[1] -> 1) <0, temp=Sqrt[-temp] * MG$DecGlu, temp = Sqrt[temp]*MG$DecGlu];
   If[MemberQ[FR$DecomposedGluons, MGPartContent[[1]]], tens = MGPartContent[[1]] /. FR$DecomposedGluonsTensors, 
      tens = ToExpression["T"<>ToString[MG$AuxTensCount++]]; FR$DecomposedGluons = Append[FR$DecomposedGluons, MGPartContent[[1]]]; 
              FR$DecomposedGluonsTensors = Append[FR$DecomposedGluonsTensors, MakeRule[MGPartContent[[1]], tens]/.MakeRule->Rule]];
   tmpprtct = {MGPartContent[[1]], MGPartContent[[1]],tens};
   tmpvertname = StringJoin @@ (ToString /@ tmpprtct);
   MGPartList = Append[MGPartList, {{T[tens], tens}, {{ToString[tens], ToString[tens], T, D, ZERO, ZERO, O, ToString[tens], 8000000+MG$AuxTensCount}}}]];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];

      If[Not[FreeQ[temp, MG$DecGlu]], 
         AppendTo[MGIParamList, {ToExpression[tmpvertname], temp, ord[[1,1]], ord[[1,2]], False} /. MG$DecGlu -> 1]];


      output = Which[Not[FreeQ[temp, MG$WeakVVVV]], {vertextype, MGPartContent, vertexname, temp,Append[ord, {n,1}], "R"} /. MG$WeakVVVV -> 1,
             Not[FreeQ[temp, MG$DecGlu]], {"VVT", tmpprtct, Most[vertexname], ToExpression[tmpvertname], Append[ord, {a, 1}], "R"} /. MG$DecGlu -> 1,
             True, {vertextype, MGPartContent, vertexname, temp, ord,"R"}]];


(* ::Subsection::Closed:: *)
(*VSS*)


HELASVSS[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, temprc,temp2,vecpos, scals,PartFV,ord, output, ascalpos, scalpos, temppl},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVSS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};

scals=Select[MRvertextype, ScalarFieldQ];
(*temprc = ((AntiFieldQ[scals[[1]]] === True) && (Not[AntiFieldQ[scals[[2]]]] === True)) || ((AntiFieldQ[scals[[2]]] === True) && (Not[AntiFieldQ[scals[[1]]]] === True));*)
vecpos = Expand[temp][[1]] //. _*FV[k_,mu_] :> FV[k,mu];
vecpos = vecpos /. FV[_, Index[_, Ext[k_]]] -> k;  

scalpos = Which[(ScalarFieldQ[MRvertextype[[1]]] === True) && (Not[AntiFieldQ[MRvertextype[[1]]]] === True), 1,
          (ScalarFieldQ[MRvertextype[[2]]] === True) && (Not[AntiFieldQ[MRvertextype[[2]]]] === True), 2,
          (ScalarFieldQ[MRvertextype[[3]]] === True) && (Not[AntiFieldQ[MRvertextype[[3]]]] === True), 3];

(*ascalpos = Which[
       (AntiFieldQ[MRvertextype[[2]]] === True) && (ScalarFieldQ[MRvertextype[[2]]] === True) && (Not[AntiFieldQ[MRvertextype[[1]]]] === True) && (ScalarFieldQ[MRvertextype[[1]]] === True), -1,
       (AntiFieldQ[MRvertextype[[3]]] === True) && (ScalarFieldQ[MRvertextype[[3]]] === True), -1,
       True, 1];*)

ascalpos = If[(PartNameMG[#]& /@ Delete[MRvertextype,vecpos]) =!= Rest[MGPartContent], 1, -1];

                 
          

temp = Collect[temp, FV[__], Factor];

Which[Not[FreeQ[temp, f]], temp = (-I)temp /. f[__] :> 1 ,
  (*  Message[MG::NoStructure]; Print["No HELAS structure found for ", ToString[MGPartContent], ". Vertex ignored."];
                temp = MG$NoStruc,*)
(*else*)
    MatchQ[temp,(g1_.*FV[p1_,mu_]+g2_.*FV[p2_,mu_])],  temp=temp ,
    True, Message[MG::NoStructure]; Print["No HELAS structure found for ", ToString[MGPartContent], ". Vertex ignored."];
                temp = MG$NoStruc];

If[temp =!= MG$NoStruc,
                temp = temp //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
(*                  If[vecpos == 1, temp =temp /.FV[2,_] :> PartFV, temp =temp /.FV[1,_] :> PartFV];*)
               (*  temp = Which[scalpos == 1, temp =temp /.FV[1,_] :> PartFV,
                              scalpos == 2, temp =temp /.FV[2,_] :> PartFV,
                              scalpos == 3, temp =temp /.FV[3,_] :> PartFV];*)

If[vecpos ==1 ,temp =temp /.FV[2,_] :> PartFV ; temppl = MRvertextype,
              temp =temp /.FV[1,_] :> PartFV; temppl = If[vecpos ==2, {MRvertextype[[2]], MRvertextype[[1]], MRvertextype[[3]]}, 
                        {MRvertextype[[3]], MRvertextype[[1]], MRvertextype[[2]]}]];


temppl = anti /@ temppl;

temppl = {temppl[[1]], temppl[[3]], temppl [[2]]};

temppl = PartNameMG /@ temppl;
                  temp = Expand[I * Coefficient[temp, PartFV, 1]]];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      ord = If[Not[FreeQ[temp, MG$DColStruc]], temp = temp /. MG$DColStruc -> 1; Append[ord, {"^dabc^", 1}], ord];
      output = {vertextype, temppl, vertexname, temp, ord,"C"}];


(* ::Subsection::Closed:: *)
(*FFT*)


HELASFFT[MRvertextype_,vertextype_, MGPartContent_, vertexname_, vertex_] := Block[{temp, ord, output, posgrav,fermind, fermpos,
                                                                                     pga12, pga21, MESla, MT, g1b, g2b, g3b, g4b},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressFFT++;
 
  progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};
      temp = temp /. f[__] -> 1;
      temp = temp /. IndexDelta[__] -> 1;
      temp = temp  //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};

      posgrav = Which[Spin2FieldQ[MRvertextype[[1]]]===True, 1,
                      Spin2FieldQ[MRvertextype[[2]]]===True, 2,
                      Spin2FieldQ[MRvertextype[[3]]]===True, 3,
                      True, Print["Graviton not found in ", MRvertextype]];

      temp = MomentumReplace[{MRvertextype,temp},posgrav][[2]];
      fermind = Complement[Range[3], {posgrav}];
      fermpos = If[FreeQ[temp,  FV[fermind[[1]],_] | SlashedP[fermind[[1]],_,_]],fermind[[2]],fermind[[1]]];
      temp = Expand[temp]/.{FV[fermpos, Index[Lorentz, Ext[posgrav,1]]]Ga[Index[Lorentz, Ext[posgrav,2]]]-> pga12,
                            FV[fermpos, Index[Lorentz, Ext[posgrav,2]]]Ga[Index[Lorentz, Ext[posgrav,1]]]-> pga21,
                            ME[Index[Lorentz, Ext[posgrav, 1]], Index[Lorentz, Ext[posgrav,2]]] SlashedP[fermpos,_,_]-> MESla(*,
                            ME[Index[Lorentz, Ext[posgrav, 1]], Index[Lorentz, Ext[posgrav,2]]]-> MT*)};
      g1b = Coefficient[temp, pga12, 1];
      g2b = Coefficient[temp, pga21, 1];
      g3b = Coefficient[temp, MESla, 1];
      (*g4b = Coefficient[temp, MT, 1];*)

      If[Expand[temp] - g1b pga12 - g2b pga21 - g3b MESla (*- g4b MT*) ==0, temp = g1b,  
          Message[MG::NoStructure];
            Print["No HELAS structure found for ", ToString[MGPartContent], ". Vertex ignored."];
                temp = MG$NoStruc ];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = DecomposeEI[temp]; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      ord = If[Not[FreeQ[temp, MG$DColStruc]], temp = temp /. MG$DColStruc -> 1; Append[ord, {"^dabc^", 1}], ord];
(* To be removed !!!!!*)
      ord = {{QED,1}} ;
      (*temp = 1;*)

      output = {vertextype, MGPartContent, vertexname, temp, ord,"C"}];    

        
