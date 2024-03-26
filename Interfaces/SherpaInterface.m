(* ::Package:: *)

(* ::Title:: *)
(*Sherpa interface*)


(* ::Text:: *)
(*C. Duhr*)
(*S. Schumann*)


(* ::Section::Closed:: *)
(*Useful functions*)


AddWhiteSpaces[lisi_List] := Drop[Flatten[Table[{lisi[[kkll]], " "}, {kkll, Length[lisi]}]], -1]

WriteMassSH[m_] := If[MantissaExponent[m][[2]] == 1, ToString[m], StringJoin[ToString[#1], "E", ToString[NumberForm[#2, NumberSigns -> {"-","+"}]]]& @@ ({N[#1*10], #2 -1}& @@ MantissaExponent[m])]; 



(* ::Section::Closed:: *)
(*Vertex reordering*)


ReOrderFieldTypeForSH[{xx___, {y_?(# =!= "F" &), k1_}, {"F", k2_}, zz___}] := ReOrderFieldTypeForSH[{xx, {"F", k2}, {y, k1}, zz}];
ReOrderFieldTypeForSH[{xx___, {y_?((# =!= "F") && (# =!= "V") &), k1_}, {"V", k2_}, zz___}] := ReOrderFieldTypeForSH[{xx, {"V", k2}, {y, k1}, zz}];
ReOrderFieldTypeForSH[{xx___, {y_?((# =!= "F") && (# =!= "V") && (# =!= "S") &), k1_}, {"S", k2_}, zz___}] := ReOrderFieldTypeForSH[{xx, {"S", k2}, {y, k1}, zz}];
ReOrderFieldTypeForSH[{xx___, {y_?((# =!= "F") && (# =!= "V") && (# =!= "S") && (# =!= "T") &), k1_}, {"T", k2_}, zz___}] := ReOrderFieldTypeForSH[{xx, {"T", k2}, {y, k1}, zz}];

ReOrderFieldTypeForSH[{{"F", k1_}, {"F", k2_}, {"V", k3_}}] := {"FFV","FFV"};
ReOrderFieldTypeForSH[{{"F", k1_}, {"F", k2_}, {"S", k3_}}] := {"FFS","FFS"};
ReOrderFieldTypeForSH[{{"V", k1_}, {"V", k2_}, {"V", k3_}}] := {"VVV","Gauge3"};
ReOrderFieldTypeForSH[{{"V", k1_}, {"V", k2_}, {"V", k3_}, {"V", k4_}}] := {"VVVV","Gauge4"};
ReOrderFieldTypeForSH[{{"V", k1_}, {"S", k2_}, {"S", k3_}}] := {"SSV","SSV"};
ReOrderFieldTypeForSH[{{"V", k1_}, {"V", k2_}, {"S", k3_}}] := {"VVS","VVS"};
ReOrderFieldTypeForSH[{{"S", k1_}, {"S", k2_}, {"S", k3_}}] := {"SSS","SSS"};
ReOrderFieldTypeForSH[{{"V", k1_}, {"V", k2_}, {"S", k3_}, {"S", k4_}}] := {"VVSS","VVSS"};
ReOrderFieldTypeForSH[{{"S", k1_}, {"S", k2_}, {"S", k3_}, {"S", k4_}}] := {"SSSS","SSSS"};
ReOrderFieldTypeForSH[{{"F", k1_}, {"F", k2_}, {"T", k3_}}] := {"FFT","FFT"};
ReOrderFieldTypeForSH[{{"V", k1_}, {"V", k2_}, {"T", k3_}}] := {"VVT","VVT"};
ReOrderFieldTypeForSH[{{"S", k1_}, {"S", k2_}, {"T", k3_}}] := {"SST","SST"};
ReOrderFieldTypeForSH[{{"F", k1_}, {"F", k2_}, {"V", k3_}, {"T", k4_}}] := {"FFVT","FFVT"};
ReOrderFieldTypeForSH[{{"V", k1_}, {"V", k2_}, {"V", k3_}, {"T", k4_}}] := {"VVVT","VVVT"};
ReOrderFieldTypeForSH[{{"S", k1_}, {"S", k2_}, {"S", k3_}, {"T", k4_}}] := {"SSST","SSST"};
ReOrderFieldTypeForSH[{{"S", k1_}, {"U", k2_}, {"U", k3_}}] := {"SUU","SUU"};
ReOrderFieldTypeForSH[{{"V", k1_}, {"U", k2_}, {"U", k3_}}] := {"VUU","VUU"};



ReOrderSHField1[{xx___, {y_?(FermionQ[#] =!= True &), k1_}, {t_?(FermionQ[#] === True &), k2_}, zz___}] := ReOrderSHField1[{xx, {t, k2}, {y, k1}, zz}];
ReOrderSHField1[{xx___, {y_?(((FermionQ[#] =!= True) && (VectorFieldQ[#] =!= True))&), k1_}, {t_?(VectorFieldQ[#] === True &), k2_}, zz___}] := ReOrderSHField1[{xx, {t, k2}, {y, k1}, zz}];
ReOrderSHField1[{xx___, {y_?(((FermionQ[#] =!= True) && (VectorFieldQ[#] =!= True) && (ScalarFieldQ[#] =!= True))&), k1_}, {t_?(ScalarFieldQ[#] === True &), k2_}, zz___}] := ReOrderSHField1[{xx, {t, k2}, {y, k1}, zz}];
ReOrderSHField1[{xx___, {y_?(((FermionQ[#] =!= True) && (VectorFieldQ[#] =!= True) && (ScalarFieldQ[#] =!= True) && (Spin2FieldQ[#] =!= True))&), k1_}, {t_?(Spin2FieldQ[#] === True &), k2_}, zz___}] := ReOrderSHField1[{xx, {t, k2}, {y, k1}, zz}];

ReOrderSHFieldFlipFermionFlow[{xx___, {y_?(FermionQ[#] === True &), k1_}, {t_?(FermionQ[#] === True &), k2_}, zz___}] := ReOrderSHFieldFlowDone[{xx, {t, k2}, {y, k1}, zz}];

ReOrderSHField2[{{f1_?(FermionQ[#] === True &), k1_}, {f2_?(FermionQ[#] === True &), k2_}, {v_?(VectorFieldQ[#] === True &), k3_}}] := {{f1, k1}, {v, k3}, {f2, k2}};
ReOrderSHField2[{{f1_?(FermionQ[#] === True &), k1_}, {f2_?(FermionQ[#] === True &), k2_}, {v_?(ScalarFieldQ[#] === True &), k3_}}] := {{f1, k1}, {v, k3}, {f2, k2}};
ReOrderSHField2[{{v1_?(VectorFieldQ[#] === True &), k1_}, {v2_?(VectorFieldQ[#] === True &), k2_}, {s_?(ScalarFieldQ[#] === True &), k3_}}] := {{v1, k1}, {s, k3}, {v2, k2}};
ReOrderSHField2[{{v_?(VectorFieldQ[#] === True &), k1_}, {s1_?(ScalarFieldQ[#] === True &), k2_}, {s2_?(ScalarFieldQ[#] === True &), k3_}}] := {{s1, k2}, {v, k1}, {s2, k3}};
ReOrderSHField2[{{v1_?(VectorFieldQ[#] === True &), k1_}, {v2_?(VectorFieldQ[#] === True &), k2_}, {s1_?(ScalarFieldQ[#] === True &), k3_}, {s2_?(ScalarFieldQ[#] === True &), k4_}}] := {{v1, k1}, {s1, k3}, {s2, k4}, {v2, k2}};


(* ::Section:: *)
(*WriteSHOutput*)


Options[WriteSHOutput] = {Output -> Automatic, Exclude4Scalars -> False, Debug -> False, DialogBox -> On, IndexExpand -> {}, MaxParticles->4, 
     ConservedQuantumNumbers :> MR$QuantumNumbers};

WriteSHOutput[lags__, options__] := WriteSHOutput[{lags}, options] /; (And @@ ((Head[#] =!= Rule &) /@ {lags})) && (And @@ ((Head[#] === Rule &) /@ {options})) && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteSHOutput[lags__] := WriteSHOutput[{lags}] /; (And @@ ((Head[#] =!= Rule &) /@ {lags}))  && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteSHOutput[{lags__}, options___] := Block[{output, defname, vertlist, vertexlistpiece, dirname, olddir, parttemplist, vertexlistSH, parttopdg, cl ,fcl, ftl, pdgl, verttag, 
              GSMasses, GSDecays, GSList, tempoutdir, goldsnghosts, SH$Statistics, SH$FFVSverts, SH$SSSverts, SH$SSSSverts, SH$VVVverts, SH$VVVVverts, SH$VVSSverts, 
              SH$VSSverts, SH$VVSverts, SH$FFVSvertsN, SH$SSSvertsN, SH$SSSSvertsN, SH$VVVvertsN, SH$VVVVvertsN, SH$VVSSvertsN, 
              SH$VSSvertsN, SH$VVSvertsN, SHFRoptions, indexp, conservedqns},
    Print[" - - - FeynRules interface to Sherpa - - -"];
    Print["       C. Duhr, S. Schumann, 2009"];
    Print["       arXiv:0906.2474"];
   (* Setting the output directory *)
      tempoutdir = Output /. {options} /. Options[WriteSHOutput];
      If[tempoutdir =!= Automatic,
         dirname = If[Not[StringQ[tempoutdir]], tempoutdir = ToString[tempoutdir], tempoutdir],
         (*Default value *)
         dirname = StringJoin[StringReplace[M$ModelName, {" " -> "_"}], "_SH"]];
    If[Not[MemberQ[FileNames[], dirname]],    
       Print["Creating Directory ", dirname];
       CreateDirectory[dirname]];
       olddir = Directory[];
       If[$OperatingSystem === "Windows",
          SetDirectory[StringJoin[Directory[], "\\", dirname]],
          SetDirectory[StringJoin[Directory[], "/", dirname]]];

(* Reading options *)

    optindexp = IndexExpand /. {options} /. Options[WriteSHOutput];
    SH$Statistics = Debug /. {options} /. Options[WriteSHOutput];
    optex4sc = Exclude4Scalars /. {options} /. Options[WriteSHOutput];
	SH$MaxParticles = MaxParticles /. {options} /. Options[WriteSHOutput];

    Print["Checking particle list..."];

    (*To get a nices output, we first remove the ghosts and goldstones form the mass list. We do this by selecting the pdg's *)
    SHPartList = PartList;
    goldsnghosts = List[#]&/@Join[Select[Join@@SHPartList[[All,2]],#[[3]]===U&][[All,9]],Select[Join@@SHPartList[[All,2]],Last[#]=!=NoGS&][[All,9]]];
    SHMassList = Select[MassList[[2]],Not[MemberQ[goldsnghosts, #[[1]]]]&];
    SHMassList = {MASS, SHMassList};
    SHWidthList = Select[WidthList[[2]],Not[MemberQ[goldsnghosts, #[[1]]]]&];
    SHWidthList = {DECAY, SHWidthList};
    SHPartList = Cases[SHPartList, {{__}, {{__, NoGS}, ___}}];
    GSList = Complement[PartList, SHPartList];
    GSMasses = If[GSList =!= {}, Select[(Sequence @@@ GSList[[All,2]])[[All,5]], Not[MatchQ[#, ZERO]]&], {}];
    GSDecays = If[GSList =!= {}, Select[(Sequence @@@ GSList[[All,2]])[[All,6]], Not[MatchQ[#, ZERO]]&], {}];
    If[Length[SHPartList] != Length[PartList], Print["Warning: Goldstone bosons will be ignored."]];
    SHPartList = RemoveSextetsFromPartList[SHPartList];
    (*SHPartList = ({#1, (Drop[#, -4]&) /@ #2} &)@@@ SHPartList;*)
    SHPartList = ({#1, (Delete[Delete[#, -1], -3]&) /@ #2} &) @@@ SHPartList;
    GSList = Cases[SHPartList, {{U[_], _}, {___}}];
    If[GSList =!= {}, GSMasses = Join[GSMasses, Select[(Sequence @@@ GSList[[All,2]])[[All,5]], Not[MatchQ[#, ZERO]]&]]];
    If[GSList =!= {}, GSDecays = Join[GSDecays, Select[(Sequence @@@ GSList[[All,2]])[[All,6]], Not[MatchQ[#, ZERO]]&]]];
    If[Not[FreeQ[SHPartList, U[_]]], SHPartList = DeleteCases[SHPartList, {{U[_], _}, {___}}]; Print["Warning: Ghost fields will be ignored."]];
    parttemplist = Flatten[SHPartList[[All,2]],1];
    Do[If[Head[Last[parttemplist[[partkk]]]] === NoPDG, Print["Warning: no PDG code found for ", parttemplist[[partkk, -2]], ". Assigned PDG code: ", Identity @@ Last[parttemplist[[partkk]]]]], {partkk, Length[parttemplist]}];
    SHPartList = SHPartList //. NoPDG -> Identity;
    SHPartList = Join @@ SHPartList[[All, 2]];
    parttopdg = {};
    Do[parttopdg = Prepend[parttopdg, Rule[SHPartList[[shpl, 1]], SHPartList[[shpl, 9]]]];
       If[SHPartList[[shpl, 1]] =!= SHPartList[[shpl, 2]], parttopdg = Prepend[parttopdg, Rule[SHPartList[[shpl, 2]], -SHPartList[[shpl, 9]]]]],
       {shpl, Length[SHPartList]}];

    SHMassList2 = SHMassList //. NoPDG -> Identity;
    SHMassList2 = SHMassList2 //. {a_, b_, Internal} :> {a, b, NumericalValue[b]};
    SHMassList2 = SHMassList2 //. {{a_?(#<0&)}, b_, val_} :> {{-a}, b, val};
    SHWidthList2 = SHWidthList //. NoPDG -> Identity;
    SHWidthList2 = SHWidthList2 //. {a_, b_, Internal} :> {a, b, NumericalValue[b]};
    SHWidthList2 = SHWidthList2 //. {{a_?(#<0&)}, b_, val_} :> {{-a}, b, val};
    SHMassList = SHMassList2[[2]];
    Do[If[(SHMassList[[ml, 3]] === NoValue[1]) || (SHMassList[[ml, 3]] === NoValue), Print["Warning: No mass value found for ", ToString[SHMassList[[ml, 2]]], ". Default value 1 assigned."]], {ml, Length[SHMassList]}];
    SHWidthList = SHWidthList2[[2]];
    Do[If[(SHWidthList[[ml, 3]] === NoValue[1]) || (SHWidthList[[ml, 3]] === NoValue), Print["Warning: No width value found for ", ToString[SHWidthList[[ml, 2]]], ". Default value 1 assigned."]], {ml, Length[SHWidthList]}];
    SHMassList = SHMassList //. {NoValue[1] -> 1, NoValue -> 1};
    SHMassList2 = SHMassList2 //. {NoValue[1] -> 1, NoValue -> 1};
    SHWidthList = SHWidthList //. {NoValue[1] -> 1, NoValue -> 1};
    SHWidthList2 = SHWidthList2 //. {NoValue[1] -> 1, NoValue -> 1};
    SHMassRules = Rule[#2, #3]& @@@ SHMassList;
    SHWidthRules = Rule[#2, #3]& @@@ SHWidthList;

    If[Not[QuantumNumberQ[Q] === True], Print["Warning: No electric charge (Q) defined. All electric charges will be put to zero."]];
    If[Not[QuantumNumberQ[Y] === True], Print["Warning: No hypercharge (Y) defined. All hypercharges will be put to zero."]];

    Print["Checking parameter list..."];
    SHEParamList2 = (({#1, ({#1, Most[#2]} &) @@@ #2 } &) @@@ EParamList) //. {NoBlockName -> Identity, NoValue[1] -> 1};
    Do[If[Head[EParamList[[kparam, 1]]] === NoBlockName, Print["Warning: no BlockName found for ", ToString[EParamList[[kparam, 2, All, 2, All]][[All, 1]]], ". Default BlockName FRBlock assigned."]],
       {kparam, Length[EParamList]}]; 
    SHEParamList = EParamList //. NoBlockName -> Identity;
    Do[If[Not[FreeQ[ParamList[[kparam]], NoValue]], Print["Warning: no value found for parameter ", ToString[ParamList[[kparam, 1]]], ". Default value 1 assigned."]],
       {kparam, Length[ParamList]}];
    SHEParamList = SHEParamList //. NoValue -> Identity;
    SHIParamList = IParamList //. NoValue -> Identity;
    SHEParamList = If[Length[#] == 4, {#[[1]], #[[2]], #[[4]]}, {#[[1]], #[[4]], #[[6]]}]& /@ (Join @@ SHEParamList[[All, 2, All, 2]]);
    SHIParamList = If[Length[#] == 4, {#[[1]], #[[2]], If[#[[3]], "C", "R"], Last[#]}, {#[[1]], #[[2]], If[#[[5]], "C", "R"], #[[6]]}]& /@ SHIParamList;
    defname = StringJoin["L", ToString[#]]& /@ Range[Length[{lags}]];


(*                             *)
(* Computation of the vertices *)
(*                             *)
    $lagrangianListtemp = {};
    FR$Exclude4Scalars = Exclude4Scalars /. {options} /. Options[WriteSHOutput];
    indexp = IndexExpand /. {options} /. Options[WriteSHOutput];
    vertexlistSH = {};
    Do[Print["Calculating Feynman rules for ", defname[[kmg]]];
       vertexlistpiece ={};
       AppendTo[vertexlistSH, FeynmanRules[List[lags][[kmg]],Name -> defname[[kmg]], FlavorExpand -> FR$AutoFlavorExpand, ConservedQuantumNumbers -> False, ScreenOutput->False, IndexExpand -> indexp, Exclude4Scalars -> FR$Exclude4Scalars, MaxParticles->SH$MaxParticles ]],
       {kmg, Length[defname]}];


     vertexlistSH = MergeVertices @@ vertexlistSH;

    (* Remove Sextets *)
    vertexlistSH = RemoveSextetsFromVertexList[vertexlistSH];

     vertexlistSH = FlavorExpansion[vertexlistSH];
     conservedqns = ConservedQuantumNumbers /. {options} /. Options[WriteSHOutput];
     If[(conservedqns =!= {}) && (conservedqns =!= False),
   	 ConserveQN[#,conservedqns]& /@ (((#1&)@@@#&)/@ vertexlistSH[[All,1]])
       ];

(*                             *)
(*  End :                      *)
(*  Computation of the vertices*)
(*                             *)



    (*                                                          *)
    (*            Conversion to SH                              *)
    (*                                                          *)


       Print["    Converting vertices to Sherpa format"];


       cl = vertexlistSH[[All,1,All]];
       cl = ReOrderSHField1 /@ cl;
    (* We reorder the fermions, because we need psi1bar.psi2 to correspond to psi2 decaying into psi*)
       cl = cl /. ReOrderSHField1 -> ReOrderSHFieldFlipFermionFlow;
       cl = cl /. {ReOrderSHFieldFlipFermionFlow -> Identity,ReOrderSHFieldFlowDone -> Identity};
       cl = ReOrderSHField2 /@ cl;
       cl = cl /. ReOrderSHField2 -> Identity;
       fcl = cl[[All, All, 1]];
       ftl = ({PartFieldType[#1], #2}& @@@ # &) /@ cl;
       verttag = ReOrderFieldTypeForSH /@ ftl;
       pdgl = anti /@ (fcl /. CC -> anti);     
       pdgl = (MapAt[anti, #, 1]&) /@ pdgl;
       pdgl = ((PartName /@ # &) /@ pdgl) //. parttopdg;
       vertlist = vertexlistSH[[All, 2]];
       vertexlistSH = Table[{fcl[[kvert]], ftl[[kvert]], verttag[[kvert, 1]], verttag[[kvert, 2]], pdgl[[kvert]], vertlist[[kvert]]}, {kvert, Length[fcl]}];



   If[SH$Statistics, Print["      (* Starting Optimization        *)"]];

     FR$OptimizeParamsDefault = FR$OptimizeParams; 

     $OptimizedParamRules = $MakeOptimizedParamRules[];
     
     FR$OptimizeParams = DeleteCases[Union[FR$OptimizeParams, First/@ParamList],gs|G|ee|\[Alpha]S|\[Alpha]EW|\[Alpha]EWM1|aS|aEW];

      $OptimizedParams = {{Sqrt2, Sqrt[2],False}, {SqrtPi, Sqrt[Pi],False}};
      AppendTo[FR$OptimizeParams, Sqrt2];
      AppendTo[FR$OptimizeParams, SqrtPi];


  
     vertexlistSH = ({#1, #2,#3,#4, #5, MakeOptimization[#6]}&) @@@ Expand[vertexlistSH];
 
     SHIParamList = Join[SHIParamList, If[Length[#] ==3, {#[[1]],#[[2]],If[#[[3]], "C", "R"],"Internal Definition"}, {#[[1]], #[[2]], If[#[[5]], "C", "R"], "Internal Definition"}]& /@ $OptimizedParams];
 
     
     FR$OptimizeParams = FR$OptimizeParamsDefault;

   If[SH$Statistics, Print["      (* End Optimization        *)"]];


     vertexlistSH = SHExchangeIndices @@@ vertexlistSH;



(*                *)
(*  Color         *)
(*                *)


      Print["Getting color structures."];
      vertexlistSH =  SHCheckColorStructure[vertexlistSH];

(*  End:          *)
(*  Color         *)
(*                *)

(*Print[vertexlistSH];*)

   (* Initializing progress bars *)
       
       $helasprogress = 0;
       $partprogress = 0;
       $lengthvertexlistSH = Length[vertexlistSH];
       If[$VersionNumber >= 6, 
         Print["    Getting Lorentz structures: ", Dynamic[$helasprogress], "/", $lengthvertexlistSH, " ."];
         Print[ProgressIndicator[Dynamic[progbarvar]]], 

          Print["    Getting Lorentz structures."]];


    (* Seperating the Lorentz structures *)
      SH$FFVSverts = Cases[vertexlistSH, {__,"FFV"|"FFS",__}];
      vertexlistSH = Complement[vertexlistSH, SH$FFVSverts];
      SH$SSSverts = Cases[vertexlistSH, {__,"SSS",__}];
      vertexlistSH = Complement[vertexlistSH, SH$SSSverts];
      SH$SSSSverts = Cases[vertexlistSH, {__,"SSSS",__}];  
      vertexlistSH = Complement[vertexlistSH, SH$SSSSverts]; 
      SH$VSSverts = Cases[vertexlistSH, {__,"SSV",__}];
      vertexlistSH = Complement[vertexlistSH, SH$VSSverts];
      SH$VVSverts = Cases[vertexlistSH, {__,"VVS",__}]; 
      vertexlistSH = Complement[vertexlistSH, SH$VVSverts];
      SH$VVSSverts = Cases[vertexlistSH, {__,"VVSS",__}]; 
      vertexlistSH = Complement[vertexlistSH, SH$VVSSverts];
      SH$VVVverts = Cases[vertexlistSH, {__,"VVV",__}];
      vertexlistSH = Complement[vertexlistSH, SH$VVVverts];
      SH$VVVVverts = Cases[vertexlistSH, {__,"VVVV",__}];


      SH$FFVSvertsN = Length[SH$FFVSverts];
      SH$SSSvertsN = Length[SH$SSSverts];
      SH$SSSSvertsN = Length[SH$SSSSverts];
      SH$VSSvertsN = Length[SH$VSSverts];
      SH$VVVvertsN = Length[SH$VVVverts];
      SH$VVVVvertsN = Length[SH$VVVVverts];
      SH$VVSvertsN = Length[SH$VVSverts];
      SH$VVSSvertsN = Length[SH$VVSSverts];
      
   If[SH$Statistics, Print["      (* Statistics:             *)"];
                     Print["      (*    Before rejection     *)"];
                     Print["          FFV/FFS = ", SH$FFVSvertsN];
                     Print["          SSS       = ", SH$SSSvertsN];
                     Print["          SSSS       = ", SH$SSSSvertsN];
                     Print["          VSS       = ", SH$VSSvertsN];
                     Print["          VVV       = ", SH$VVVvertsN];
                     Print["          VVVV       = ", SH$VVVVvertsN];
                     Print["          VVS       = ", SH$VVSvertsN];
                     Print["          VVSS       = ", SH$VVSSvertsN];
                     Print["         "];
                     Print["      (*    Progress     *)"]];



       Print["    Getting Sherpa structures."];


(* FFV/ FFS structures *)
If[SH$FFVSvertsN !=0, $partprogressFFVS = 0;
   If[SH$Statistics && ($VersionNumber >= 6),  Print["     FFV/FFS: ", Dynamic[$partprogressFFVS], "/", SH$FFVSvertsN, " ."]];
   SH$FFVSverts = SHFFVS @@@ SH$FFVSverts];

(* SSS structures *)
If[SH$SSSvertsN !=0, $partprogressSSS = 0;
   If[SH$Statistics && ($VersionNumber >= 6),  Print["        SSS: ", Dynamic[$partprogressSSS], "/", SH$SSSvertsN, " ."]];
   SH$SSSverts = SHSSS @@@ SH$SSSverts];

(* SSSS structures *)
If[SH$SSSSvertsN !=0, $partprogressSSSS = 0;
   If[SH$Statistics && ($VersionNumber >= 6),  Print["       SSSS: ", Dynamic[$partprogressSSSS], "/", SH$SSSSvertsN, " ."]];
   SH$SSSSverts = SHSSSS @@@ SH$SSSSverts];

(* VSS structures *)
If[SH$VSSvertsN !=0, $partprogressVSS = 0;
   If[SH$Statistics && ($VersionNumber >= 6),  Print["        VSS: ", Dynamic[$partprogressVSS], "/", SH$VSSvertsN, " ."]];
   SH$VSSverts = SHVSS @@@ SH$VSSverts];

(* VVV structures *)
If[SH$VVVvertsN !=0, $partprogressVVV = 0;
   If[SH$Statistics && ($VersionNumber >= 6),  Print["        VVV: ", Dynamic[$partprogressVVV], "/", SH$VVVvertsN, " ."]];
   SH$VVVverts = SHVVV @@@ SH$VVVverts];

(* VVVV structures *)
If[SH$VVVVvertsN !=0, $partprogressVVVV = 0;
   If[SH$Statistics && ($VersionNumber >= 6),  Print["       VVVV: ", Dynamic[$partprogressVVVV], "/", SH$VVVVvertsN, " ."]];
   SH$VVVVverts = SHVVVV @@@ SH$VVVVverts];

(* VVS structures *)
If[SH$VVSvertsN !=0, $partprogressVVS = 0;
   If[SH$Statistics && ($VersionNumber >= 6),  Print["        VVS: ", Dynamic[$partprogressVVS], "/", SH$VVSvertsN, " ."]];
   SH$VVSverts = SHVVS @@@ SH$VVSverts];

(* VVSS structures *)
If[SH$VVSSvertsN !=0, $partprogressVVSS = 0;
   If[SH$Statistics && ($VersionNumber >= 6),  Print["       VVSS: ", Dynamic[$partprogressVVSS], "/", SH$VVSSvertsN, " ."]];
   SH$VVSSverts = SHVVSS @@@ SH$VVSSverts];


   If[SH$Statistics,       SH$FFVSvertsN = Length[SH$FFVSverts];
               SH$SSSvertsN = Length[SH$SSSverts];
               SH$SSSSvertsN = Length[SH$SSSSverts];
               SH$VSSvertsN = Length[SH$VSSverts];
               SH$VVVvertsN = Length[SH$VVVverts];
               SH$VVVVvertsN = Length[SH$VVVVverts];
               SH$VVSvertsN = Length[SH$VVSverts];
               SH$VVSSvertsN = Length[SH$VVSSverts];
      
                     Print["      (*    After rejection     *)"];
                     Print["          FFV/FFS = ", SH$FFVSvertsN];
                     Print["          SSS       = ", SH$SSSvertsN];
                     Print["          SSSS       = ", SH$SSSSvertsN];
                     Print["          VSS       = ", SH$VSSvertsN];
                     Print["          VVV       = ", SH$VVVvertsN];
                     Print["          VVVV       = ", SH$VVVVvertsN];
                     Print["          VVS       = ", SH$VVSvertsN];
                     Print["          VVSS       = ", SH$VVSSvertsN];
                     Print["         "];
                     Print["          Total = ", SH$FFVSvertsN+SH$SSSvertsN+SH$SSSSvertsN+SH$VSSvertsN+SH$VVVvertsN+
     SH$VVVVvertsN+SH$VVSvertsN+SH$VVSSvertsN];
                     Print["         "]];

(* Combine results *)

       vertexlistSH = Join[SH$FFVSverts, SH$SSSverts, SH$SSSSverts, SH$VSSverts, SH$VVVverts, SH$VVVVverts, SH$VVSverts, SH$VVSSverts];



       vertexlistSH = DeleteCases[vertexlistSH, {_, MG$NoStruc ,___}|{_,0 ,___}|{_,{0,0},___}];
       Print["    Sherpa output obtained for ", "L"];

  WriteSHPart;
  WriteSHParam;
  WriteSHLesHouches;
  WriteSHInteractions[vertexlistSH, parttopdg];
  WriteMasterFile;
  Print["Sherpa output written in file ", dirname];
  SetDirectory[olddir];];



(* ::Section::Closed:: *)
(*Write Files*)


(* ::Subsection::Closed:: *)
(*WriteSHLesHouches*)


WriteSHLesHouches := Block[{myfile, DropInter, GoodShape, GoodShape2, GoodPrinting, WriteBlock, WriteBlockMass, WriteBlockDecay}, 
   myfile=OpenWrite["ident_card.dat"];
   WriteString[myfile, "!\n"];
   WriteString[myfile,"! This file has been generated automatically by FeynRules\n"];
   WriteString[myfile, "!\n"];
   Close["ident_card.dat"];
   (*DropInter[xx__]:=List[#[[1]],First[#[[2]]]]&/@Last[xx];*)
   DropInter[xx__]:=List[#[[1]],First[#[[2]]], If[Last[#[[2]]], "C", "R"]]&/@Last[xx];
   GoodShape[xx__]:=Map[Flatten,Prepend[#,First[xx]]&/@DropInter[xx],1];
   GoodShape2[xx__]:=Map[Flatten,Prepend[{Abs[#[[1]]],#[[2]],"R"},First[xx]]&/@Last[xx],1];
   If[$VersionNumber >= 6, 
      GoodPrinting[xx__,myfile__]:=WriteString[myfile,StringJoin[ExportString[xx,"Table", "FieldSeparators"->" "], "\n"] ],
      GoodPrinting[xx__,myfile__]:=WriteString[myfile,StringJoin[ExportString[xx,"Table",ConversionOptions->{"ColumnAlignment"->Left}],"\n"] ]];
   If[SHEParamList2 =!= {},
      myfile=OpenAppend["ident_card.dat"];
      GoodPrinting[#,myfile]&/@ GoodShape/@SHEParamList2;
      Close["ident_card.dat"]];
   If[SHMassList2 =!= {},
      myfile=OpenAppend["ident_card.dat"];
      GoodPrinting[GoodShape2[SHMassList2],myfile];
      Close["ident_card.dat"]];
   If[SHWidthList2 =!= {},
      myfile=OpenAppend["ident_card.dat"];
      GoodPrinting[GoodShape2[SHWidthList2],myfile];
      Close["ident_card.dat"]];
   myfile=OpenWrite["param_card.dat"];
   WriteString[myfile, "#This file has been generated automatically by FeynRules\n"];
   WriteBlock[xx_,file_] := Block[{i},
     WriteString[file,"Block "<>ToString[xx[[1]]]<>"\n"];
     WriteString[file,"  "<>StringJoin[ToString[#]<>" "&/@#[[1]]] <>" "<>ToFortranNumber[#[[2]][[-2]]]<>"  # "<>ToString[#[[2,1]]]<>"\n"]&/@ xx[[2]];];
   WriteBlockMass[xx_,file_]:=Module[{i},
     WriteString[file,"Block "<>ToString[xx[[1]]]<>"\n"];
     WriteString[file,"  "<>StringJoin[ToString[Abs[#]]<>" "&/@#[[1]]] <>" "<>ToFortranNumber[#[[3]]]<>"  # "<>ToString[#[[2]]]<>"\n"]&/@ xx[[2]];];
   WriteBlockDecay[xx_,file_]:=Module[{i}, WriteString[file,"DECAY  "<>StringJoin[ToString[Abs[#]]<>" "&/@#[[1]]] <>" "<>ToFortranNumber[#[[3]]]<>"  # "<>ToString[#[[2]]]<>"\n"]&/@ xx[[2]];];
   If[SHEParamList2 =!= {},
      WriteBlock[#,myfile]&/@SHEParamList2];
   If[SHMassList2 =!= {},
      WriteBlockMass[SHMassList2,myfile]];
   If[SHWidthList2 =!= {},
      WriteBlockDecay[SHWidthList2,myfile]];
   Close["param_card.dat"]];


(* ::Subsection::Closed:: *)
(*WriteSHPart*)


DefaultSherpaMasses[1] = 0.01;
DefaultSherpaMasses[2] = 0.005;
DefaultSherpaMasses[3] = 0.2;
DefaultSherpaMasses[4] = 1.42;
DefaultSherpaMasses[5] = 4.8;
DefaultSherpaMasses[6] = 175;

DefaultSherpaMasses[11] = 0.000511;
DefaultSherpaMasses[12] = 0.0;
DefaultSherpaMasses[13] = 0.105;
DefaultSherpaMasses[14] = 0.0;
DefaultSherpaMasses[15] = 1.777;
DefaultSherpaMasses[16] = 0.0;

DefaultSherpaMasses[21] = 0.0;
DefaultSherpaMasses[22] = 0.0;
DefaultSherpaMasses[23] = 91.188;
DefaultSherpaMasses[24] = 80.419;
DefaultSherpaMasses[25] = 120;

SherpaSMPartsQ[nn_] := MemberQ[{1,2,3,4,5,6,11,12,13,14,15,16,21,22,23,24,25}, nn];

WriteSHPart := Block[{file, SHElecCharge = (QuantumNumberQ[Q] === True), SHHypCharge = (QuantumNumberQ[Y] === True), PartAPartExchange, TempSHPartList},
   file = OpenWrite["Particle.dat"];
   WriteString[file, "# This file was generated automatically by FeynRules.\n"];
   WriteString[file, "\n"];
   WriteString[file, "kf       Mass         Width         3*e     Y     SU(3)     2*Spin     maj     on     stbl     m_on   IDName    TeXName\n"];

   (* We need to change all negative PDG codes to positive pdg codes in particles.dat *)

   PartAPartExchange[ll_List] := If[ll[[9]]<0, Join[{ll[[2]],ll[[1]]},Table[ll[[jj]],{jj,3,8}],
                                              {-ll[[9]],ll[[11]],ll[[10]]}],ll];
   
    
   TempSHPartList = PartAPartExchange /@ SHPartList;

   Do[WriteString[file, TempSHPartList[[pp, 9]]]; 
      If[StringLength[ToString[TempSHPartList[[pp, 9]]]] < 9, 
         WriteString[file, Sequence @@ Table[" ", {9 - StringLength[ToString[TempSHPartList[[pp, 9]]]]}]],
         WriteString[file, " "]];
      WriteString[file, If[(TempSHPartList[[pp, 5]] === ZERO) || ((NumericalValue[TempSHPartList[[pp, 5]]] /. NoValue[1] -> 1) == 0.), If[SherpaSMPartsQ[TempSHPartList[[pp, 9]]], DefaultSherpaMasses[TempSHPartList[[pp, 9]]], ".0"], WriteMassSH[NumericalValue[TempSHPartList[[pp, 5]]]/. NoValue -> Identity]]];
      WriteString[file, Sequence @@ Table[" ",{14 - StringLength[If[(TempSHPartList[[pp, 5]] === ZERO) || ((NumericalValue[TempSHPartList[[pp, 5]]] /. NoValue[1] -> 1) == 0.), If[SherpaSMPartsQ[TempSHPartList[[pp, 9]]], ToString[DefaultSherpaMasses[TempSHPartList[[pp, 9]]]], ".0"], WriteMassSH[NumericalValue[TempSHPartList[[pp, 5]]] /. NoValue -> Identity]]]}]];
      WriteString[file, If[(TempSHPartList[[pp, 6]] === ZERO) || ((NumericalValue[TempSHPartList[[pp, 6]]] /. NoValue[1] -> 1) == 0.), ".0", WriteMassSH[NumericalValue[TempSHPartList[[pp, 6]]] /. NoValue -> Identity]]];
      WriteString[file, Sequence @@ Table[" ",{14 - StringLength[If[(TempSHPartList[[pp, 6]] === ZERO) || ((NumericalValue[TempSHPartList[[pp, 6]]] /. NoValue[1] -> 1) == 0.), ".0", WriteMassSH[NumericalValue[TempSHPartList[[pp, 6]]] /. NoValue -> Identity]]]}]];
      WriteString[file, If[Not[SHElecCharge], "0      ", If[Q[TempSHPartList[[pp, 1]] /. FR$StringToSymbol] >=0, ToString[Evaluate[3*Q[TempSHPartList[[pp, 1]] /. FR$StringToSymbol]]] <> "      ", ToString[Evaluate[3*Q[TempSHPartList[[pp, 1]] /. FR$StringToSymbol]]] <> "     "]]]; 
      WriteString[file, If[Not[SHHypCharge], "0       ", If[Y[TempSHPartList[[pp, 1]] /. FR$StringToSymbol] >=0, ToString[Evaluate[Y[TempSHPartList[[pp, 1]] /. FR$StringToSymbol]]] <> "       ", ToString[Evaluate[Y[TempSHPartList[[pp, 1]] /. FR$StringToSymbol]]] <> "      "]]]; 
      WriteString[file, Which[TempSHPartList[[pp, 7]] === S, 0,
                              TempSHPartList[[pp, 7]] === T, 3,
                              TempSHPartList[[pp, 7]] === O, 8]];
      WriteString[file, "          "];
      WriteString[file, Which[TempSHPartList[[pp, 3]] === S, 0,
                              TempSHPartList[[pp, 3]] === F, 1,
                              TempSHPartList[[pp, 3]] === V, 2,
                              TempSHPartList[[pp, 3]] === T, 4]];
      WriteString[file, "       "];
      WriteString[file, Which[MajoranaFieldQ[TempSHPartList[[pp, 1]] /. FR$StringToSymbol], " 1",
                              SelfConjugateQ[TempSHPartList[[pp, 1]] /. FR$StringToSymbol], "-1", 
                              True, " 0"]];
      WriteString[file, "       1       1       ", If[(TempSHPartList[[pp, 5]] === ZERO) || ((NumericalValue[TempSHPartList[[pp, 5]]] /. NoValue[1] -> 1) == 0.), "0", "1"], "       "];
      WriteString[file,  TempSHPartList[[pp, 1]]];
      WriteString[file, StringDrop["          ",StringLength[TempSHPartList[[pp, 1]]]-1]];
      WriteString[file, TempSHPartList[[pp, 10]]];
   WriteString[file, "\n"];,{pp, Length[TempSHPartList]}];
   Close[file];];


(* ::Subsection::Closed:: *)
(*WriteSHLHA*)


WriteSHLHA[ptpdg_] := Block[{file, pdgtopart = (ptpdg /. Rule[a_, b_] :> Rule[b, a])},
   file = OpenWrite["LHInput.dat"];
   WriteString[file, "# This fils was generated automatically by FeynRules\n"];
   WriteString[file, "\n"];
   WriteString[file, "Block MASS  # Mass spectrum\n"];
   WriteString[file, "# PDG code      mass          particle\n"];
   Do[WriteString[file, "   ", ToString @@ SHMassList[[mlk, 1]], (StringJoin @@ Table[" ", {12 - StringLength[ToString[SHMassList[[mlk, 1]]]]}]), ToFortranNumber[SHMassList[[mlk, 3]]], "  # ", ToString @@ (SHMassList[[mlk, 1]] /. pdgtopart), "\n"],
      {mlk, Length[SHMassList]}];
   WriteString[file, "\n"];
   Do[WriteString[file, "DECAY ", ToString @@ SHWidthList[[mlk, 1]], (StringJoin @@ Table[" ", {12 - StringLength[ToString[SHWidthList[[mlk, 1]]]]}]), ToFortranNumber[SHWidthList[[mlk, 3]]], "  # ", ToString @@ (SHWidthList[[mlk, 1]] /. pdgtopart), "\n"],
      {mlk, Length[SHWidthList]}];
   Close[file];];


(* ::Subsection::Closed:: *)
(*WriteSHParam*)


WriteSHParam := Block[{file},
   file = OpenWrite["param_definition.dat"];
   (*WriteString[file, "! External Parameters \n"];
   WriteString[file, "!\n"];
   Do[WriteString[file, ToString[SHEParamList[[park, 1]]] <> " = " <> WriteMassSH[SHEParamList[[park, 2]]] <> "    ! " <> SHEParamList[[park, 3]] <> "\n"],
      {park, Length[SHEParamList]}];*)
   WriteString[file, "!\n"];
   WriteString[file, "! Internal Parameters \n"];
   WriteString[file, "!\n"];
    (*This could be freed*)
   (*Do[WriteString[file, ToString[SHIParamList[[park, 1]]] <> " = " <> ToFRSHString[SHIParamList[[park, 2]]] <> "    ! " <> SHIParamList[[park, 3]] <> "\n"],
      {park, Length[SHIParamList]}]; *)
   Do[WriteString[file, ToString[SHIParamList[[park, 1]]] <> " = " <> ToFRSHString[SHIParamList[[park, 2]]] <> " " <> SHIParamList[[park, 3]] <> " ! " <> SHIParamList[[park, 4]] <> "\n"],
      {park, Length[SHIParamList]}];
   WriteString[file, "!\n"];
   (*WriteString[file, "! MASS and DECAY parameters\n"];
   WriteString[file, "!\n"];
   WriteString[file, "SLHA_INPUT = LHInput.dat"];*)
   Close[file];];


(* ::Subsection::Closed:: *)
(*WriteSHInteractions*)


WriteSHInteractions[vertslist_, ptpdg_] := Block[{file, pdgtopart = (ptpdg /. Rule[a_, b_] :> Rule[b, a])},
   file = OpenWrite["Interactions.dat"];
   WriteString[file, "! This fils was generated automatically by FeynRules\n"];
   WriteString[file, "!\n"];
   WriteString[file, "!-------------------------------------------------------\n"];
   WriteString[file, "!-- Interactions ---------------------------------------\n"];
   WriteString[file, "!-------------------------------------------------------\n"];
      WriteString[file, "!\n"]; 
      WriteString[file, "!- Interactions associated with " <> M$ModelName <> "\n"];
      WriteString[file, "!\n"]; 
      WriteString[file, "\n"]; 
      Do[WriteString[file, "VERTEX " <>  StringJoin @@ AddWhiteSpaces[ToString /@ vertslist[[dnj, 1]]] <> "   # ", StringJoin @@ AddWhiteSpaces[vertslist[[dnj, 1]] /. pdgtopart], "\n"];
         If[Head[vertslist[[dnj, 2]]] === List, 
            WriteString[file, "    1 " <> ToString[ToFRSHString[vertslist[[dnj, 2, 2]]]] <> "  # right-handed coupling\n"];
            WriteString[file, "    2 " <> ToString[ToFRSHString[vertslist[[dnj, 2, 1]]]] <> "  # left-handed coupling\n"];
            WriteString[file, "    3 " <> ToString[vertslist[[dnj, 3]]] <> "  # colour structure\n"];
            WriteString[file, "    4 " <> ToString[vertslist[[dnj, 4]]] <> "  # Lorentz structure\n"],
            (* else *)
            WriteString[file, "    1 " <> ToString[ToFRSHString[vertslist[[dnj, 2]]]] <> "  # right-handed coupling\n"];
            WriteString[file, "    2 " <> ToString[ToFRSHString[vertslist[[dnj, 2]]]] <> "  # left-handed coupling\n"];
            WriteString[file, "    3 " <> ToString[vertslist[[dnj, 3]]] <> "  # colour structure\n"];
            WriteString[file, "    4 " <> ToString[vertslist[[dnj, 4]]] <> "  # Lorentz structure\n"]];
         WriteString[file, "\n"],
         {dnj, Length[vertslist]}];
    Close[file];];


(* ::Subsection::Closed:: *)
(*WriteMasterFile*)


WriteMasterFile := Block[{file},
     file = OpenWrite["feynrules.dat"];
     WriteString[file, "! This fils was generated automatically by FeynRules\n"];
     WriteString[file, "!\n"];
     WriteString[file, "PARAMDEFINITION = param_definition.dat\n"];
     WriteString[file, "IDENTFILE = ident_card.dat\n"];
     WriteString[file, "PARAMCARD = param_card.dat\n"];
     WriteString[file, "INTERACTIONS = Interactions.dat\n"];
     Close[file];];
   
   


(* ::Section:: *)
(*Sherpa vertex structures*)


(* ::Subsection::Closed:: *)
(*ExchangeIndices*)


SHExchangeIndices[parts_, FRnames_, struc1_, struc2_, pdgs_, vertex_] := Block[{changeext1, changeexta, changeextb, temp, Extdone},

(* We need to relabel the indices with respect to the permutation of the particles used for Sherpa *)

      changeext1 = (Extract[#, 2]&) /@ FRnames;
      changeexta = Table[Rule[Ext[changeext1[[i]]], Extdone[i]], {i, Length[changeext1]}];
      changeextb = Table[Rule[FV[changeext1[[i]], Pattern[mu, Blank[]]], FV[i, mu]], {i, Length[changeext1]}];

      temp = vertex //. changeexta /. changeextb //. Extdone -> Ext;
      {parts, FRnames, struc1, struc2, pdgs, temp}]


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

SHCheckColorStructureDel[parts_, frnames_, struc1_, struc2_, pdgs_, vertex_] := Block[{tempvert, delstruc, output, coltemp, ID},
    If[FreeQ[vertex, T] && FreeQ[vertex, f],
       tempvert= Factor[Expand[vertex]];
        output = Which[MatchQ[tempvert, _. * IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]]], 
                     coltemp = tempvert //. {_. * IndexDelta[Index[Colour, ii_], Index[Colour, jj_]] :> IndexDelta[Index[Colour, ii], Index[Colour, jj]], _. * IndexDelta[Index[Gluon, ii_], Index[Gluon, jj_]] :> IndexDelta[Index[Gluon, ii], Index[Gluon, jj]]};
                     coltemp = coltemp /. IndexDelta[ii_, jj_] IndexDelta[kk_, ll_] :> ID[ii,jj,kk,ll] /. IndexDelta[ii_, jj_] :> ID[ii,jj] /. Index[_, Ext[kk_]] :> kk ;
                     coltemp = coltemp /. ID[inds__] :> StringReplace[ToString[T[inds]], {"T" -> "D"," " -> ""}];
                     {parts,frnames, struc1, struc2,pdgs,tempvert, coltemp }/. IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]] :> 1,
             True, 
                    Print["Color structure for ", ToString[parts], " does not match. Vertex ignored."]; $NoColMatch],
        output = Print["Color structure for ", ToString[parts], " does not match. Vertex ignored."]; $NoColMatch]];

SHCheckColorStructureT[parts_, frnames_, struc1_, struc2_, pdgs_, vertex_] := Block[{tempvert, delstruc, output, coltemp},
    If[FreeQ[vertex, f[_,_,_]],
       tempvert= Factor[Expand[vertex]/. T[a_,i_, k_]T[b_,k_,j_] :> TensDot[T[a],T[b]][i,j]];
        output = Which[MatchQ[tempvert, _. * T[Index[Gluon,_], Index[Colour, _], Index[Colour, _]]], 
                     coltemp = tempvert //. {_*T[ind__] -> T[ind]} //. {T[xx___, Index[_, Ext[k_]], yy___] -> T[xx, k, yy]} //. {T[ind__] :> StringReplace[ToString[T[ind]], {" "->""}]}; 
                    { parts,frnames, struc1, struc2,pdgs,tempvert, coltemp }/. T[Index[Gluon,_], Index[Colour, _], Index[Colour, _]] :> 1,
                       MatchQ[tempvert, _.*(TensDot[T[a_], T[b_]][i_, j_]+TensDot[T[b_], T[a_]][i_, j_])]&& FreeQ[temp, f],
                     coltemp = Expand[tempvert] //. _.* TensDot[T[aa_], T[bb_]][ii_,jj_] :> TensDot[T[aa], T[bb]][ii,jj] /. TensDot[T[aa_], T[bb_]][ii_,jj_] + TensDot[T[bb_], T[aa_]][ii_,jj_] :> TensDot[T[aa], T[bb]][ii,jj] ;
                     coltemp = coltemp /. Index[_, Ext[k_]] :> k /.TensDot[T[aa_], T[bb_]][ii_,jj_] :> "T["<>ToString[aa]<>"," <>ToString[bb] <>"," <>ToString[ii] <>"," <>ToString[jj]<>"]";
                     {parts,frnames, struc1, struc2,pdgs,tempvert, coltemp } /.  {TensDot[ T[_], T[_]][_, _] -> 1/2},
             True, 
                    Print["Color structure for ", ToString[parts], " does not match. Vertex ignored."]; $NoColMatch],
        output = Print["Color structure for ", ToString[parts], " does not match. Vertex ignored."]; $NoColMatch]];

(*SHCheckColorStructureEps[parts_, frnames_, struc1_, struc2_, pdgs_, vertex_] := Block[{tempvert, delstruc, output, EpsSign, mrvt},
    If[FreeQ[vertex, T|Gluon|IndexDelta[Index[Colour,_], Index[Colour, _]]],
       tempvert= Factor[Expand[vertex]];

        output = If[MatchQ[tempvert, _. * Eps[Index[Colour,_], Index[Colour, _], Index[Colour, _]]],
               mrvt = PartName /@ (MRvertextype  /. {ff_?((FieldQ[#] && Not[DiracFieldQ[#] === True] && Not[MajoranaFieldQ[#] === True])&) :> anti[ff]}
                                  /. {ff_?(((DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True)) && (AntiFieldQ[#] === True) &) :> anti[ff]}
                                  /. CC[ff_] :> anti[ff]);

                     (* Determine the of the relative sign of vertextype with respect to MGPartContent *)
                     EpsSign = Signature[FindPermutation[MGPartContent, mrvt]];

                     {MRvertextype,vertextype, MGPartContent, vertexname, EpsSign * tempvert }/. Eps[Index[Colour,_], Index[Colour, _], Index[Colour, _]] :> 1,
                    Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch],
        output = Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; $NoColMatch]];*)

SHCheckColorStructureF[parts_, frnames_, struc1_, struc2_, pdgs_,  vertex_] := Block[{tempvert, delstruc, output, coltemp},
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
                     coltemp = tempvert //. {_*f[ind__] -> f[ind]} //. {f[xx___, Index[_, Ext[k_]], yy___] -> f[xx, k, yy]} /. {f[k1_, k2_, k3_] :> ("F[" <> ToString[k1] <> "," <> ToString[k2] <> "," <> ToString[k3] <> "]")};
                     {parts,frnames, struc1, struc2,pdgs,tempvert, coltemp },
             True, 
                    Print["Color structure for ", ToString[parts], " does not match. Vertex ignored."]; $NoColMatch],
  
           (* four gluon structure *)
           tempvert= Collect[tempvert, f[__]];
           output = Which[MatchQ[tempvert, g1_.*f[a1_, a3_,a2_,a4_]+g2_.*f[a1_, a2_,a3_,a4_]+g3_.*f[a1_, a4_,a2_,a3_]],
                             {parts,frnames, struc1, struc2,pdgs,tempvert, "F" }, 
                           (* SSVV Octets *)
                          MatchQ[tempvert, g_.*f[a1_, a2_,a3_,a4_]+g_.*f[a1_, a4_,a3_,a2_]],
                             tempvert = -Coefficient[tempvert, f[Index[Gluon, Ext[1]] ,Index[Gluon, Ext[3]], Index[Gluon, Ext[4]],Index[Gluon, Ext[2]]], 1];
                             {parts,frnames, struc1, struc2,pdgs,tempvert, "F" }, 
                         True, 
                             Print["Color structure for ", ToString[parts], " does not match. Vertex ignored."]; $NoColMatch]]];

(*SHCheckColorStructureD[parts_, frnames_, struc1_, struc2_, pdgs_, vertex_] := Block[{tempvert, delstruc, output, fpart, dpart},
       If[FreeQ[vertex, f[_,_,_]],
           (* Pure D structure structure *)
           tempvert= Factor[vertex];
           output = Which[MatchQ[tempvert, _. * dSUN[Index[Gluon,_], Index[Gluon, _], Index[Gluon, _]]], 
                     {parts,frnames, struc1, struc2,pdgs,tempvert * MG$DColStruc /. dSUN[__] :> 1 },
             True, 
                    Print["Color structure for ", ToString[parts], " does not match. Vertex ignored."]; $NoColMatch],
  
           (* F+D structure *)
           tempvert= Collect[vertex, f[__],Collect[#, dSUN[__]]&];
           output = Which[MatchQ[tempvert, _ * dSUN[Index[Gluon,_], Index[Gluon, _], Index[Gluon, _]] +  _?(FreeQ[#, dSUN[__]]&) * f[Index[Gluon,_], Index[Gluon, _], Index[Gluon, _]]],
                             dpart = Coefficient[tempvert /. dSUN[__] :> $fabcstruc, $fabcstruc];
                             fpart = tempvert /. dSUN[__] :> 0;
                             AppendTo[AddToFTerms, {parts,frnames, struc1, struc2,pdgs,vertexname,fpart}];
                            {parts,frnames, struc1, struc2,pdgs,dpart * MG$DColStruc   },  
                         True, 
                             Print["Color structure for ", ToString[parts], " does not match. Vertex ignored."]; $NoColMatch];
output]];*)

        

SHCheckColorStructure[vertlist_] := Block[{nocolstruc, colstruc, delstruc, Tstruc, fstruc,dstruc, epsstruc},


      nocolstruc= Select[vertlist, (FreeQ[#,Colour] && FreeQ[#,Gluon]) & ];
      colstruc = Complement[vertlist, nocolstruc];
     epsstruc = Select[colstruc, Not[FreeQ[#, Eps[Index[Colour, _], Index[Colour, _], Index[Colour, _]]]]&];
      colstruc = Complement[colstruc, epsstruc];      
      dstruc = Select[colstruc, Not[FreeQ[#, dSUN]]&];
      colstruc = Complement[colstruc, dstruc];

       (* Defining color structure "None" *)
       nocolstruc = Append[#, "None"] & /@ nocolstruc;

     
   If[colstruc =!= {},
      fstruc = Select[colstruc , (FreeQ[#, T] && FreeQ[#,IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]]])&];
      colstruc = Complement[colstruc, fstruc]; 
      Tstruc = Select[colstruc, FreeQ[#,IndexDelta[Index[Colour|Gluon, _], Index[Colour|Gluon, _]]]&];
      delstruc = Complement[colstruc, Tstruc];

   If[SH$Statistics, Print["      (* Statistics:             *)"];
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
       If[delstruc =!= {}, delstruc = SHCheckColorStructureDel @@@ delstruc];
       If[Tstruc =!= {}, Tstruc = SHCheckColorStructureT @@@ Tstruc];
       If[fstruc =!= {}, fstruc = SHCheckColorStructureF @@@ fstruc];
 (*      If[dstruc =!= {}, dstruc = SHCheckColorStructureD @@@ dstruc];
       If[epsstruc =!= {}, epsstruc = SHCheckColorStructureEps @@@ epsstruc];*)

       If[AddToFTerms =!= {}, fstruc = Join[fstruc, AddToFTerms]];


      (* Reject dSUN structure *)
 (*     If[dstruc =!={}, Print["Color structure for ", ToString[MGPartContent], " does not match. Vertex ignored."]; dstruc ={}];*)

   If[SH$Statistics, 
                     Print["      (*    After rejection     *)"];
                     Print["          NoColor = ", Length[nocolstruc]];
                     Print["          \[Delta]       = ", Length[delstruc]];
                     Print["          T       = ", Length[Tstruc]];
                     Print["          f       = ", Length[fstruc]];
                     Print["          d       = ", Length[dstruc]];
                     Print["          \[Epsilon]       = ", Length[epsstruc]];
                     Print["      (* End Statictics     *)"]];

       (* Combining the results *)
       colstruc = DeleteCases[Join[delstruc, Tstruc, fstruc(*, dstruc, epsstruc*)], $NoColMatch];


       Join[nocolstruc, colstruc], nocolstruc]];


(* ::Subsection::Closed:: *)
(*FFVS Structure*)


SHFFVS[parts_,FRnames_,struc1_,struc2_,pdgs_, vertex_, color_] := Block[{temp, ord, output},

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
                temp = {Coefficient[temp, Proj[1]], Coefficient[temp, Proj[2]]} //. GaAlgebra[___] -> 1 ,
          Message[SH::NoStructure];
            Print["No Lorentz structure found for ", ToString[parts], ". Vertex ignored."];
                temp = MG$NoStruc ];
           (* End Check Lorentz structure *)

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = Expand[temp];
      temp = temp //. ParamRules;
      output = {pdgs, temp, color, struc2 }];    

        


(* ::Subsection::Closed:: *)
(*SSS*)


SHSSS[parts_, FRnames_, struc1_, struc2_, pdgs_, vertex_, color_] := Block[{temp, ord, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressSSS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex /. f[__] -> 1;

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = Expand[temp];
      temp = temp //. ParamRules;
      output = {pdgs, temp, color, struc2}];

      




(* ::Subsection::Closed:: *)
(*SSSS*)


SHSSSS[parts_, FRnames_, struc1_, struc2_, pdgs_, vertex_, color_] := Block[{temp, ord, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressSSSS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex /. f[__] -> 1;


      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = Expand[temp];
      temp = temp //. ParamRules;
      ord = GetOrder[temp];
      output = {pdgs, temp, color, struc2}];

      




(* ::Subsection::Closed:: *)
(*VVS *)


SHVVS[parts_, FRnames_, struc1_, struc2_, pdgs_, vertex_, color_] := Block[{temp, ord, rcdc, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};
      temp = temp /. f[__] -> 1;

     (* The higgs effective coupling *)
temp=Expand[temp];
(*Which[Not[FreeQ[temp, Eps]], temp = temp //.Eps[mu1_,mu2_,a_,b_]FV[ii_,a_]FV[jj_,b_]:>Eps[mu1,mu2,MG$AA,MG$BB]FV[ii,MG$AA]FV[jj,MG$BB]//.Eps[mu1_,mu2_,MG$AA,MG$BB]FV[ii_,MG$AA]FV[jj_,MG$BB]:>-Eps[mu1,mu2,MG$AA,MG$BB]FV[jj,MG$AA]FV[ii,MG$BB]/;jj<ii;
                             temp = {0, temp}/. {Eps[__] -> 1, FV[__]->1, IndexDelta[__]->1};
                             rcdc = "DC";  
                             temp = Expand[I*temp] //. {MyHELASComplex -> Complex, MyHELASRational -> Rational},
      MatchQ[temp, _. * ME[__] SP[ii_,jj_] + _. * FV[ii_,_] FV[jj_,_]],
                             temp= temp /. IndexDelta[__]-> 1/. ME[__] -> MEAB;
                             temp = {Coefficient[temp, MEAB,1], 0} /. SP[__]->1;
                             rcdc = "DC";  
                             temp = Expand[-I*temp] //. {MyHELASComplex -> Complex, MyHELASRational -> Rational},
      (* End heft *)
      True,
      rcdc = "C";*)
      temp = Collect[temp, {ME[__]}, Simplify];
      If[Not[MatchQ[temp, _.*ME[i_,j_]]],
            Message[SH::NoStructure];
            Print["No Lorentz structure found for ", ToString[parts], ". Vertex ignored."];
                temp = MG$NoStruc,
(*else*)  
                temp = Expand[temp] //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = temp //. ME[__] -> 1];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = Expand[temp];
      temp = temp //. ParamRules;
      output = {pdgs, temp, color, struc2}];    

        


(* ::Subsection::Closed:: *)
(*VVSS*)


SHVVSS[parts_, FRnames_, struc1_, struc2_, pdgs_, vertex_, color_] := Block[{temp, ord, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVSS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};
      temp = temp /. f[__] -> 1;

     (* The higgs effective coupling *)
temp=Expand[temp];


      temp = Collect[temp, {ME[__]}, Simplify];
  
      If[Not[MatchQ[temp, _.*ME[i_,j_]]],
            Message[SH::NoStructure];
            Print["No Lorentz structure found for ", ToString[parts], ". Vertex ignored."];
                temp = MG$NoStruc,
(*else*)  
                temp = Expand[temp] //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = temp //. ME[__] -> 1];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC; 
      temp = Expand[temp];
      temp = temp //. ParamRules;
      output = {pdgs, temp, color, struc2}];    

        


(* ::Subsection::Closed:: *)
(*VVV*)


SHVVV[parts_, FRnames_, struc1_, struc2_, pdgs_, vertex_, color_] := Block[{temp, ord, output},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVV++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};

      temp = temp  //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};
                temp = Coefficient[temp, FV[1,Index[Lorentz, Ext[3]]] ME[Index[Lorentz, Ext[1]], Index[Lorentz, Ext[2]]]] /. f[__] :>1;

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = Expand[temp];
      temp = temp //. ParamRules;
      output = {pdgs, temp, color,"Gauge3"}];


(* ::Subsection::Closed:: *)
(*VVVV*)


SHVVVV[parts_, FRnames_, struc1_, struc2_, pdgs_, vertex_, color_] := Block[{temp, temp1,temp2,vecpart, scalpart,tens,tmpprtct,colless, 
     tmpvertname,tmpvecvertname,ord, output, MG$DecGlu,MG$WeakVVVV, temppdgs},

(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVVVV++;


progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};

colless = (color === "None");

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

(* Fix inspired by the 3-site model*)
(* particles need to be ordered according to  (2,-1, -1) *)

                Which[Simplify[temp2/temp1] == 1, temp = -temp1;(* Print["Done 1;"]; *)temppdgs = {pdgs[[1]], pdgs[[4]], pdgs[[3]], pdgs[[2]]}(*;Print[pdgs]; Print[temppdgs]*),
                             Simplify[temp2/temp1] == -2, temp = -temp1;(*Print["Done 2;"]; *)temppdgs = {pdgs[[1]], pdgs[[3]], pdgs[[2]], pdgs[[4]]}(*;Print[pdgs]; Print[temppdgs]*),
                             Simplify[temp2/temp1] == -1/2, temp = -temp2; temppdgs = pdgs,
                             True, Print["Warning: 4-vector coupling ", parts, " does not math Lorentz structure! Coupling ignored"]; temp = 0];



If[colless, temp = temp * MG$WeakVVVV];

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC; 
      temp = Expand[temp];
      temp = temp //. ParamRules;


      output = Which[Not[FreeQ[temp, MG$WeakVVVV]], {temppdgs, temp, color, "Gauge4"} /. MG$WeakVVVV -> 1,
             True, {temppdgs, temp, color, "Gluon4"}]];


(* ::Subsection::Closed:: *)
(*VSS*)


SHVSS[parts_, FRnames_, struc1_, struc2_, pdgs_, vertex_, color_] := Block[{temp, temprc,temp2,vecpos, scals,PartFV,ord, output, ascalpos, scalpos},


(* Updating the progress bar *)
 $helasprogress++;
 $partprogressVSS++;

progbarvar = $helasprogress/$lengthvertexlistMG;
(*End updating the progress bar *)

      temp = vertex //. {Complex[a_,b_] -> MyHELASComplex[a, b], Rational[a_,b_] -> MyHELASRational[a, b]};

(*scals=Select[MRvertextype, ScalarFieldQ];
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

ascalpos = If[(PartNameMG[#]& /@ Delete[MRvertextype,vecpos]) =!= Rest[MGPartContent], 1, -1];*)

                 
          

temp = Collect[temp, FV[__], Factor];



temp = temp /. f[__] :> 1;

    Which[MatchQ[temp,(g1_.*FV[p1_,mu_]+g2_.*FV[p2_,mu_])],  temp=Coefficient[temp, FV[1, Index[Lorentz, Ext[2]]]] ,
    True, Message[SH::NoStructure]; Print["No Lorentz structure found for ", ToString[parts], ". Vertex ignored."];
                temp = MG$NoStruc];

temp = temp /. {MyHELASComplex -> Complex, MyHELASRational -> Rational};

(*If[temp =!= MG$NoStruc,
                temp = temp //. {MyHELASComplex -> Complex, MyHELASRational -> Rational};


If[vecpos ==1 ,temp =temp /.FV[2,_] :> PartFV ; temppl = MRvertextype,
              temp =temp /.FV[1,_] :> PartFV; temppl = If[vecpos ==2, {MRvertextype[[2]], MRvertextype[[1]], MRvertextype[[3]]}, 
                        {MRvertextype[[3]], MRvertextype[[1]], MRvertextype[[2]]}]];


temppl = anti /@ temppl;

temppl = {temppl[[1]], temppl[[3]], temppl [[2]]};

temppl = PartNameMG /@ temppl;
                  temp = Expand[I * Coefficient[temp, PartFV, 1]]];*)

      temp = temp //. TensDot -> ExpandTensDotForMG;
      temp = Expand[temp];
      temp = temp //. {HC -> $HCHELAS} //. {$HCHELAS[t_?(TensQ)][ind___] -> $HCHELAS[t[ind]]}; 
      temp = temp //. MR$Definitions;
      temp = temp //.{$HCHELAS[t_?(TensQ)[i_,j_]] -> Conjugate[t[j,i]]};
      temp = temp //. $HCHELAS -> HC;
      temp = Expand[temp];
      temp = temp //. ParamRules;
      output = {pdgs, temp, color, struc2}];
