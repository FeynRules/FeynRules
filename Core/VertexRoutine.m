(* ::Package:: *)

(* ::Title:: *)
(*THE CORE*)


(* ::Section:: *)
(*FeynmanRules*)


(* ::Subsubsection:: *)
(*Options*)


Options[FeynmanRules] = {
  TeXOutput -> MR$Null, 
  ScreenOutput -> False, 
  Name -> MR$Null, 
  FlavorExpand -> FR$AutoFlavorExpand, 
  IndexExpand -> False,
  ConservedQuantumNumbers :> MR$QuantumNumbers, 
  FermionFlow -> False, 
  MaxParticles -> Automatic, 
  MinParticles -> Automatic, 
  MaxCanonicalDimension -> Automatic, 
  MinCanonicalDimension -> Automatic, 
  SelectParticles -> Automatic,
  Free -> Automatic, 
  Contains -> Automatic,
  Exclude4Scalars -> False,
  ApplyMomCons->True,
  NoDefinitions->False
};


(* ::Subsubsection:: *)
(*Main function, FeynmanRules (takes a list of Lagrangians as arguments)*)
(**)
(*CD: September 2013: Changed default of ScreenOutput to True.*)


FeynmanRules[lags__?(Head[#]=!=List&),OptionsPattern[]]:= FeynmanRules[{lags}, TeXOutput->OptionValue[TeXOutput], 
  ScreenOutput->OptionValue[ScreenOutput], Name->OptionValue[Name], FlavorExpand->OptionValue[FlavorExpand], ApplyMomCons->OptionValue[ApplyMomCons],
  IndexExpand->OptionValue[IndexExpand], FermionFlow->OptionValue[FermionFlow], MinParticles -> OptionValue[MinParticles],
  ConservedQuantumNumbers->OptionValue[ConservedQuantumNumbers], MaxParticles->OptionValue[MaxParticles], Free->OptionValue[Free],
  MaxCanonicalDimension -> OptionValue[MaxCanonicalDimension], MinCanonicalDimension -> OptionValue[MinCanonicalDimension], 
  SelectParticles->OptionValue[SelectParticles], Contains -> OptionValue[Contains], Exclude4Scalars->OptionValue[Exclude4Scalars],NoDefinitions->OptionValue[NoDefinitions]];


FeynmanRules[{lags__}, OptionsPattern[]] := Block[{FRname=OptionValue[Name],lag,FRoptions,flavopt=OptionValue[FlavorExpand]},
  (* Initialization *)
  lag=Plus@@( (FieldExpand[#/. Dot -> FR$Dot]/.FR$Dot -> Dot) &/@ {lags} );
  If[FRname=!=MR$Null,
    If[Not[StringQ[FRname]], FRname=ToString[FRname]];
    FR$Lagrangian[FRname] = lag;
    $lagrangianListtemp = If[ValueQ[$lagrangianListtemp], Append[$lagrangianListtemp, FRname], {FRname}]
  ];
  If[FRname=!=MR$Null, Print[Style["Starting Feynman rules calculation for "  <> FRname <> ".",Orange,Bold]], Print[Style["Starting Feynman rule calculation.",Orange,Bold]]];

  (* Calling the main function (parallelization moved inside GetVertices)*)
  If[flavopt==={},flavopt=FR$AutoFlavorExpand];
  FRoptions=DeleteCases[{Name->FRname, FlavorExpand->flavopt, ApplyMomCons->OptionValue[ApplyMomCons], IndexExpand->OptionValue[IndexExpand],
    FermionFlow->OptionValue[FermionFlow], MinParticles -> OptionValue[MinParticles], MaxParticles->OptionValue[MaxParticles],
    ConservedQuantumNumbers->OptionValue[ConservedQuantumNumbers], Free->OptionValue[Free], Contains -> OptionValue[Contains],
    MaxCanonicalDimension -> OptionValue[MaxCanonicalDimension], MinCanonicalDimension -> OptionValue[MinCanonicalDimension],
    SelectParticles->OptionValue[SelectParticles],  Exclude4Scalars->OptionValue[Exclude4Scalars],NoDefinitions->OptionValue[NoDefinitions]},Rule[_,MR$Null]];

  GetVertices[lag, Sequence@@FRoptions];
  (* Output *)
  If[Length[Vertices[lag]]==1, Print["1 vertex obtained."], Print[Length[Vertices[lag]], " vertices obtained."]];
  If[OptionValue[TeXOutput]=!=MR$Null, FRMakeTeXOut[OptionValue[TeXOutput], Vertices[lag]]];
  If[OptionValue[ScreenOutput],PrintScreenOutput[Vertices[lag]]];
  If[FR$FeynArtsInterface && FA$FlavExpCheck,
    If[Intersection[FR$CheckFlavExp, Flatten[Vertices[lag][[All, 1, All, 1]]]] =!= {}, Message[FA::FlavExp]]
  ];
  Return[Vertices[ToExpression["FR$VertNumb" <> ToString[FR$VertexNumber++]]]];
];


(* ::Section:: *)
(*Unitarity Constraints*)


ReplaceRepeatedL0[exp_,rules_]:=Block[{prevexp, newexp=exp},While[prevexp=!=newexp,prevexp=newexp;newexp=Replace[prevexp,rules,{0}]];newexp];


UnitarityConstraints[expr_] := Block[{tempexpr, nonmatrix,utime},

       If[FreeQ[expr,Index[a_?(Not[Head[IndexRange[Index[#]]]===NoUnfold]&&Not[#===Lorentz]&&Not[#===Spin]&),b_?(Not[NumericQ[#]]&)]],Return[expr]]; 
utime=SessionTime[];
       tempexpr=If[Head[expr]===Plus,If[FR$FExpand,Expand/@expr,(Expand[#,Indices]&)/@expr],If[FR$FExpand,Expand[expr],Expand[expr,Indices]]]; (*Indices added by Celine*)
(*Block[{tempexpr = Expand/@expr, nonmatrix}*)
(* Modifications by A. Alloul 12/11/2012:
   1) Modified the above 1 lines 
   2) Modifications bring a considerable speed gain: Applying Expand on the whole lagrangian is much slower
      than applying it on every term.
*)


      nonmatrix = If[Head[tempexpr]===Plus,
                       Select[tempexpr, FreeQ[#, _?((UnitaryQ[#] === True)&)| _?((OrthogonalQ[#] === True)&)]&],
                       If[FreeQ[tempexpr, _?((UnitaryQ[#] === True)&)| _?((OrthogonalQ[#] === True)&)],tempexpr,0]
                    ];
      tempexpr = Expand[tempexpr - nonmatrix];
If[Head[tempexpr]=!=Plus,tempexpr={tempexpr};];

      If[tempexpr ===0,Return[expr]];
     tempexpr = (ReplaceRepeatedL0[#, {xx_*tt_[Index[name_, ii_], Index[name_, jj_]]Conjugate[tt_[Index[name_, kk_], Index[name_, jj_]]] :> (xx/.kk -> ii)  /; Not[NumericQ[jj]]&&UnitaryQ[tt]&&FreeQ[xx,jj], 
                             xx_ *tt_[Index[name_, ii_], Index[name_, jj_]]Conjugate[tt_[Index[name_, ii_], Index[name_, kk_]]] :> (xx/.kk -> jj)  /; Not[NumericQ[ii]]&&UnitaryQ[tt]&&FreeQ[xx,ii]}]&)/@tempexpr;
   tempexpr =(ReplaceRepeatedL0[#,  {xx_*tt_?(OrthogonalQ)[ii_, Index[name_, jj_]]tt_[kk_, Index[name_, jj_]] :> IndexDelta[ii,kk] /; Not[NumericQ[jj]]&&FreeQ[xx,jj], 
                             xx_*tt_?(OrthogonalQ)[Index[name_, ii_], jj_]tt_[Index[name_, ii_], kk_] :> IndexDelta[jj,kk] /; Not[NumericQ[ii]]&&FreeQ[xx,ii]}]&)/@tempexpr;

If[SessionTime[]-utime>0.1,Print[InputForm[expr]];];
If[Head[tempexpr]=!=Plus,tempexpr=tempexpr[[1]];];
      Return[tempexpr + nonmatrix];
];




(* ::Section:: *)
(*ExpandIndices*)


Options[ExpandIndices] = Join[Options[FeynmanRules],{ SymmetryFactor -> True, StandAlone->True }];


ExpandIndices[lagrangian_, options___] := Block[{symfac = SymmetryFactor /. {options} /. Options[ExpandIndices],
   out, opts,MyCC,tmplag, sa = StandAlone/.{options}/.Options[ExpandIndices]},
   opts = If[FreeQ[{options}, SymmetryFactor],
             Append[{options}, SymmetryFactor -> symfac],
             {options}];

   tmplag=lagrangian;
   If[Not[MemberQ[opts,Rule[FlavorExpand,_]]], opts=Append[opts,FlavorExpand->True]];


   If[M$MixingsDescription=!={}, out=ExpandIndices2[tmplag];out=out//.Dot->FR$Dot//.FR$Dot->Dot;Return[FieldExpand/@out]];


   out=PrepareLag[lagrangian, Sequence @@ opts, StandAlone->sa ];

   out=out//.Dot->FR$Dot//.FR$Dot->Dot;
   Return[FieldExpand/@out];
];


(* ::Section:: *)
(*PrepareLag*)


(* ::Subsection:: *)
(*PrepareLagIndexExpansion*)


Options[PrepareLagIndexExpansion] = {StandAlone -> False};


PrepareLagIndexExpansion[lagrangian_, flavexp_, flavexplist_, options___] := Block[{

    templag = lagrangian,
    tempstandalone = StandAlone /. {options} /. Options[PrepareLagIndexExpansion],
    symfac = SymmetryFactor /. {options} /. SymmetryFactor -> True,
    nodef = NoDefinitions/.{options}
},

   (*                           *)
(* Begin vertex computation  *)
(*                           *)

(* Modifications by A. Alloul 12/11/2012:
   1) Modified the 3 lines below
   2) Modifications bring a considerable speed gain: Applying Expand or ApplyDefinitions on the whole lagrangian is much slower
      than applying it on every term.
*)
(*      If[Not[FreeQ[templag, _?RFermionFieldQ]], templag = Expand[templag]]; 
        templag = ApplyDefinitions[templag];
        templag = Expand[templag];
*)

     If[Not[FreeQ[templag, _?RFermionFieldQ]], templag = FieldExpand/@templag];      
     If[!nodef,templag = If[Head[templag]===Plus,ApplyDefinitions/@templag,ApplyDefinitions[templag]]];
     templag = If[Head[templag]===Plus,FieldExpand/@templag,FieldExpand[templag]];

     If[templag === 0,
        Return[templag]
       ];

(* Unitarity Constraints ,if remove by celine*)
    (* If[Not[tempstandalone],*)
        If[Head[templag]===Plus,templag = UnitarityConstraints/@templag;,templag = UnitarityConstraints[templag];];
       (* ];*)

(* End Unitarity constraints *)

      templag = TreatAllowSummation[templag];

      templag = templag //. f_?FieldQ[ind__?(FreeQ[{##},Index]&)] :> PutIndexName[f[ind]] //. done -> Identity;
      templag = PrePutIndices[templag];
(* Modifications by A. Alloul 12/11/2012:
   1) Modified the  line below
   2) Modifications bring a considerable speed gain: Applying  ApplyDefinitions on the whole lagrangian is much slower
      than applying it on every term.
*)
(*      templag = ApplyDefinitions[templag];*)
      If[!nodef,templag = If[Head[templag]===Plus,ApplyDefinitions/@templag,ApplyDefinitions[templag]]];

      templag = PutIndices[templag, flavexp, flavexplist,nodef];


     If[templag === 0,
        Return[templag]
       ];
      If[Not[tempstandalone],
         templag = UnitarityConstraints[templag];
         ];

      templag = templag //.f_?(NoTensQ[#] === True &)[Index[name_, ind_]] :> NoTensPutInd[f, {ind}];
      templag = templag /. {dd_?(SymTensQ[#] === True &)[ind__] :> SortSymTens[dd][ind], dd_?(AntiSymTensQ[#] === True &)[ind__] :> SortAntiSymTens[dd][ind],
                            ff_?(StrucConstQ[#] === True &) :> SortStrucConst[ff]};


      templag = templag //. Dot -> FR$Dot //. FR$Dot -> Dot;
      templag = TreatMajoranasAndCC[templag];

      templag = templag //. Dot -> FR$Dot //. FR$Dot -> Dot;

      templag = PrePutIndices[templag];

      templag = templag //. Dot -> FR$Dot //. FR$Dot -> Dot;

      templag = templag //. {HC -> $HCExtractVertex} //. {$HCExtractVertex[t_?(TensQ)][ind___] -> $HCExtractVertex[t[ind]]}; 

(* Modifications by A. Alloul 12/11/2012:
   1) Modified the line below
   2) Modifications bring a considerable speed gain: Applying  ApplyDefinitions on the whole lagrangian is much slower
      than applying it on every term.
*)
(*      templag =ApplyDefinitions[templag];*)
      If[!nodef,templag = If[Head[templag]===Plus,ApplyDefinitions/@templag,ApplyDefinitions[templag]]];


      templag = templag //.f_?(NoTensQ[#] === True &)[Index[name_, ind_]] :> NoTensPutInd[f, {ind}];
      templag = templag //. $HCExtractVertex -> HC;
      templag = templag //. {f_?(SymQ)[ind___] :> SymmetrizeFieldIndex[f[ind], $SymList[f]]};
      templag = templag //. {$done[f_] -> f};
      templag = templag //. {f_?(AntiSymQ)[ind___] :> AntiSymmetrizeFieldIndex[f[ind], $AntiSymList[f]]};
      templag = templag //. {$done[f_] -> f};

(* Modifications by A. Alloul 12/11/2012:
   1) Modified the line below
   2) Modifications bring a considerable speed gain: Applying  Expand on the whole lagrangian is much slower
      than applying it on every term.
*)
(*      templag = Expand[templag] //. {f_?FieldQ[] -> f};*)
      If[!nodef,templag = If[Head[templag]===Plus,ApplyDefinitions/@templag,ApplyDefinitions[templag]] //. {f_?FieldQ[] -> f}];

      If[tempstandalone,
         templag = templag //. NTIndex -> Index
        ];

      Return[templag];

];


(* ::Subsection:: *)
(*PrepareLag*)


Options[PrepareLag]= {FlavorExpand -> False, Name -> MR$Null, ConservedQuantumNumbers :> MR$QuantumNumbers, FermionFlow -> False, 
                      IndexExpand -> False, MaxParticles -> Automatic, MinParticles -> Automatic, SelectParticles -> Automatic,
                      MaxCanonicalDimension -> Automatic, MinCanonicalDimension -> Automatic, 
                      Exclude4Scalars -> False, StandAlone -> False, Free -> Automatic, Contains -> Automatic, SymmetryFactor -> True,NoDefinitions->False};    

PrepareLag[lagrangian_, options___] := Block[{ 
        templag = lagrangian, 
        (* setting options *)
        tempstandalone = StandAlone /. {options} /. Options[PrepareLag],
        symfac = SymmetryFactor /. {options} /. Options[PrepareLag],
        indexplist = IndexExpand /. {options} /. Options[PrepareLag],
        maxnum = MaxParticles /. {options} /. Options[PrepareLag],
        minnum = MinParticles /. {options} /. Options[PrepareLag],
        maxcanon = MaxCanonicalDimension /. {options} /. Options[PrepareLag],
        mincanon = MinCanonicalDimension /. {options} /. Options[PrepareLag],
        selpart = SelectParticles /. {options} /. Options[PrepareLag],
        excl4scal = Exclude4Scalars /. {options} /. Options[PrepareLag],
        tempfree = Free /. {options} /. Options[PrepareLag],
        tempcontains = Contains /. {options} /. Options[PrepareLag],
        nodef=NoDefinitions/.{options}/.Options[PrepareLag],
        selectionoptions
       }, 

      $FlavExp = FlavorExpand /. {options} /. Options[PrepareLag];
      Which[$FlavExp === True, $FlavExpList = MR$FlavorList,
            $FlavExp === False, $FlavExpList = {},
            True, $FlavExpList = Intersection[MR$FlavorList, Flatten[{$FlavExp}]];
                  $FlavExp = True];

      If[Not[indexplist === False], 
         indexplist = Flatten[{indexplist}];
         If[$FlavExp === False, 
            $FlavExp = True];
         $FlavExpList = KillDoubles[Join[indexplist, $FlavExpList]]];

      templag = If[Not[FreeQ[templag, PauliSigma]],
                   PerformPauliAlgebra[If[FR$FExpand,Expand[templag],Expand[templag,PauliSigma]]], (*PauliSigma added by Celine*)
                   templag];

(* Changes brought by Adam Alloul 12/11/20112: 
   1) The original version is below commented -> 2 lines
   2) The modification is justified by the fact that applying Expand to the whole templag is much slower than applying it
      on every term. For the standard model, Expand[templag] takes about 7s, Expand/@templag takes 0.17s 
*)
(*
      templag =Expand[templag];
      templag = Expand[templag  //. Dot -> FR$Dot //. FR$Dot -> Dot];
*)
      templag =If[Head[templag]===Plus,FieldExpand/@templag,FieldExpand[templag]];
      templag = templag  //. Dot -> FR$Dot //. FR$Dot -> Dot;
      templag = If[Head[templag]===Plus,FieldExpand/@templag,FieldExpand[templag]];
      templag = PrepareLagIndexExpansion[templag, $FlavExp, $FlavExpList, StandAlone -> tempstandalone, SymmetryFactor -> symfac,NoDefinitions->nodef];

     (* Applying the Lagrangian selection rules *)

      selectionoptions = {Exclude4Scalars -> excl4scal,
                          SelectParticles -> selpart,
                          Free            -> tempfree,
                          Contains        -> tempcontains,
                          MaxParticles    -> maxnum,
                          MinParticles    -> minnum,
                          MaxCanonicalDimension -> maxcanon,
                          MinCanonicalDimension -> mincanon
                         };

        templag = LagrangianTermSelectionRules[templag, Sequence @@ selectionoptions];

Return[templag];


];


(* ::Section:: *)
(*LagrangianTermSelectionRules*)


Options[LagrangianTermSelectionRules] := Options[PrepareLag];


LagrangianTermSelectionRules[lag_, OptionsPattern[]] := Block[{
      templag = FieldExpand[lag],
      tempfree = OptionValue[Free],
      tempcontains = OptionValue[Contains],
      maxnum = OptionValue[MaxParticles],
      minnum = OptionValue[MinParticles],
      maxcanon = OptionValue[MaxCanonicalDimension],
      mincanon = OptionValue[MinCanonicalDimension],
      excl4scal = OptionValue[Exclude4Scalars],
      selpart = OptionValue[SelectParticles]
    },
      (* Selecting field content *)
      If[selpart =!= Automatic,
         If[VectorQ[selpart], 
            selpart = {selpart}
           ];
         If[Not[FreeQ[selpart, _?RFermionFieldQ]], 
            selpart = KillDoubles[Join[selpart, selpart /. {maj_?RFermionFieldQ :> HC[maj]}]]
           ];
         selpart = KillDoubles[Join[selpart, selpart /. {ff_?CFermionFieldQ :> CC[anti[ff]]}]];
         selpart = Sort /@ selpart;
         If[maxnum === Automatic,
            maxnum = Max[Length /@ selpart]
           ];
         If[minnum === Automatic,
            minnum = Min[Length /@ selpart]
           ];
         ];
       If[selpart =!= Automatic,
          If[FR$Message[[6]],Print["Selecting specified field content. Warning! Only mass eigenstates should be selected!"]; FR$Message[[6]]=False];
          If[Head[templag]===Plus,
            templag = Select[templag, MemberQ[selpart, Sort[GetFieldContent[#]]]&],
            If[Not[MemberQ[selpart, Sort[GetFieldContent[templag]]]], templag = 0]
          ]
         ];

       If[templag === 0,
         Return[templag];
        ];

      (* Rejecting four scalar vertices *) 
      If[excl4scal === True,
         If[FR$Message[[1]],Print["Excluding quartic scalar couplings."];FR$Message[[1]]=False];
         templag = ApplyDefinitions[templag];
         If[Head[FieldExpand[templag]] === Plus, 
            templag = Select[templag, ((GetFieldContent[#] /. _?ScalarFieldQ :> S) =!= {S,S,S,S})&],
            (*else*)
            If[(GetFieldContent[templag]/._?ScalarFieldQ -> S) === {S,S,S,S}, 
               templag =0
              ];
            ];
         ];

       If[templag === 0,
         Return[templag];
        ];


      (* Free *)
      If[(tempfree =!= Automatic),
         tempfree = Flatten[{tempfree}];
         tempfree = KillDoubles[Join[tempfree, tempfree /. {maj_?RFermionFieldQ :> HC[maj]}]];
         tempfree = KillDoubles[Join[tempfree, tempfree /. {ff_?CFermionFieldQ :> CC[anti[ff]]}]];
         If[Head[templag] === Plus, 
            Do[templag = Select[templag, FreeQ[#, tempfree[[curelem]]]&], 
              {curelem, Length[tempfree]}],
            (* MW edit: just set templag to 0 if it contains a forbidden field *)
            Do[If[!FreeQ[templag, tempfree[[curelem]]], templag = 0], {curelem, Length[tempfree]}]]];

      If[templag === 0,
         Return[templag];
        ];

      (* Contains *) 
      If[(tempcontains =!= Automatic),
         tempcontains = Flatten[{tempcontains}];
         If[Head[templag] === Plus,
            (* MW edit: seems conditions in Which statements must evaluate to True or False, at least in Mathematica 6 *)
            Do[Which[CFermionFieldQ[tempcontains[[curelem]]]===True, templag = Select[templag, Not[FreeQ[#, tempcontains[[curelem]] | CC[anti[tempcontains[[curelem]]]]]]&],
                     RFermionFieldQ[tempcontains[[curelem]]]===True, templag = Select[templag, Not[FreeQ[#, tempcontains[[curelem]] | anti[tempcontains[[curelem]]]]]&],
                     True, templag = Select[templag, Not[FreeQ[#, tempcontains[[curelem]] | CC[anti[tempcontains[[curelem]]]]]]&]],
               {curelem, Length[tempcontains]}],
            Do[Which[CFermionFieldQ[tempcontains[[curelem]]]===True, templag = If[FreeQ[templag, tempcontains[[curelem]] | CC[anti[tempcontains[[curelem]]]]], 0, templag],
                     RFermionFieldQ[tempcontains[[curelem]]]===True, templag = If[FreeQ[templag, tempcontains[[curelem]] | anti[tempcontains[[curelem]]]], 0, templag],
                     True, templag = If[FreeQ[templag, tempcontains[[curelem]] | anti[tempcontains[[curelem]]]], 0, templag]],
               {curelem, Length[tempcontains]}]]];

      If[templag === 0,
         Return[templag];
        ];

      (* MaxParticles *) 
        If[maxnum =!= Automatic, 
           If[FR$Message[[2]],Print["Neglecting all terms with more than ", ToString[maxnum], " particles."];FR$Message[[2]]=False];
           If[Head[templag] === Plus,
              templag = Select[FieldExpand[templag], (Length[GetFieldContent[#]] <= maxnum)&],
              If[Length[GetFieldContent[templag]] > maxnum, 
                 templag = 0
                ];
              ]; 
           ];

      If[templag === 0,
         Return[templag];
        ];

      (* MinParticles *) 
      If[minnum =!= Automatic, 
          If[FR$Message[[3]],Print["Neglecting all terms with less than ", ToString[minnum], " particles."];FR$Message[[3]]=False];
          If[Head[FieldExpand[templag]] === Plus, 
             templag = Select[FieldExpand[templag], (Length[GetFieldContent[#]] >= minnum)&],
             If[Length[GetFieldContent[templag]] < minnum, 
                templag = 0;
                ];
             ];
          ];

      If[templag === 0,
         Return[templag];
        ];

      (* Selecting the maximal canonical dimension *)
      If[maxcanon =!= Automatic, 
         If[FR$Message[[4]],Print["Neglecting all terms with canonical dimension greater than ", ToString[maxcanon], "."];FR$Message[[4]]=False];
         If[Head[FieldExpand[templag]] === Plus, 
            templag = Select[FieldExpand[templag],(CanonicalDimension[#]<=maxcanon)&],
            If[Not[CanonicalDimension[templag] <= maxcanon], 
               templag = 0;
               ];
            ];
         ];

      If[templag === 0,
         Return[templag];
        ];

      (* Selecting the minimal canonical dimension *)
      If[mincanon =!= Automatic, 
         If[FR$Message[[5]],Print["Neglecting all terms with canonical dimension less than ", ToString[mincanon], "."];FR$Message[[5]]=False];
         If[Head[FieldExpand[templag]] === Plus, 
            templag = Select[FieldExpand[templag], (CanonicalDimension[#] >= mincanon)&],
            If[Not[CanonicalDimension[templag] >= mincanon], 
               templag = 0;
              ];
            ];
         ];

     Return[templag];

];





(* ::Section:: *)
(*Prepare Parallelization*)


PrepareParallelization[lagr_]:=Block[{tmplag,newtmplag,tot},
  tmplag=Listize[lagr];
  tot=Length[tmplag];
  If[Global`FR$Parallelize && tot>40 && $KernelCount>1, 
    FR$DoPara=True;
    newtmplag=Partition[tmplag,IntegerPart[tot/Min[FR$MaxKernels,tot]]];
    newtmplag[[-1]]=Union[newtmplag[[-1]],Complement[tmplag,Flatten[newtmplag,1]]];
    Return[newtmplag],
  Return[tmplag]
  ];
];


PrepareParallelizationList[vlist_]:=Block[{newvlist,tot=Length[vlist]},
  If[Global`FR$Parallelize && tot>40 && $KernelCount>1, 
    FR$DoPara=True;
    newvlist=Partition[vlist,IntegerPart[tot/Min[FR$MaxKernels,tot]]];
    newvlist[[-1]]=Union[newvlist[[-1]],Complement[vlist,Flatten[newvlist,1]]];
    Return[newvlist],
  Return[vlist]
  ];
];


Options[ParallelizeMe]={MyOptions->MR$Null,Counter->False};

ParallelizeMe[func_,arg_,OptionsPattern[]]:=Block[{myoptions,myarg,tmpres,inter},
(*First we want to create a list, if arg =!= list *)
  myarg=Which[ Head[arg]===Plus,List@@(FieldExpand/@arg),
               Head[arg]===Times,{arg},
               Head[arg]===List,arg];

(*If a counter is wished*)
  mycounter=0;
  SetSharedVariable[mycounter,FR$Message];
  If[OptionValue[Counter]===True, Print[Dynamic[mycounter]," / ",Length[arg]] ];

  tmpres=If[OptionValue[MyOptions]=!=MR$Null,
    (*If the option has some options*)
    myoptions=OptionValue[MyOptions];
    (*Distribute the definitions*)
    DistributeDefinitions[myoptions];SetSharedFunction[func];
    (*and parallelsubmit*)
    Table[inter=myarg[[ii]];ParallelSubmit[{ii,inter},mycounter++;$Output={};tmpres=func[inter,Sequence@@myoptions];$Output={OutputStream["stdout",1]}; tmpres],{ii,Length[myarg]}],
    (*Otherwise, distribute anyway*)
    SetSharedFunction[func];
    (*than parallelsubmit*)
    Table[inter=myarg[[ii]];ParallelSubmit[{ii,inter},mycounter++;$Output={};tmpres=func[inter];$Output={OutputStream["stdout",1]}; tmpres],{ii,Length[myarg]}]
  ];
(*Do the calculations and return the result in the format it was before*)
  tmpres=If[Head[arg]===Plus||Head[arg]===Times,Plus@@WaitAll[tmpres],WaitAll[tmpres]];
  Return[tmpres]];


(* ::Section:: *)
(*GetVertices*)


(* ::Subsection:: *)
(*Options*)


Options[GetVertices] = {
  FlavorExpand -> False, 
  Name -> MR$Null, 
  ConservedQuantumNumbers :> MR$QuantumNumbers,
  FermionFlow -> False,
  IndexExpand -> False,
  MaxCanonicalDimension -> Automatic,
  MinCanonicalDimension -> Automatic,
  MaxParticles -> Automatic, 
  MinParticles -> Automatic, 
  Exclude4Scalars -> False,
  Free -> Automatic, 
  Contains -> Automatic,
  SelectParticles -> Automatic,
  ApplyMomCons->True,
  NoDefinitions->False
};


Options[ProcessVertexList] = {
  Name -> MR$Null, 
  ApplyMomCons->True,
  ConservedQuantumNumbers :> MR$QuantumNumbers
};


Options[ProcessEmptyList] = { Name -> MR$Null};


(* ::Subsection:: *)
(*Core function*)


GetVertices[lagrangian_, OptionsPattern[]] := Block[{templag ,FRoptions,amc=OptionValue[ApplyMomCons], FCList,FCListtemp, CollectedIntLagList, checklist,vertexlist},
  (* Initialization *)
  Clear[StoreIntLor, StoreInt]; (*Avoid name clashes when renaming internal indices from different runs. *)
  FRoptions = {Name->OptionValue[Name], ApplyMomCons->OptionValue[ApplyMomCons], FlavorExpand->OptionValue[FlavorExpand], IndexExpand->OptionValue[IndexExpand], FermionFlow->OptionValue[FermionFlow],
    MinParticles -> OptionValue[MinParticles], MaxParticles->OptionValue[MaxParticles], ConservedQuantumNumbers->OptionValue[ConservedQuantumNumbers], Free->OptionValue[Free],
    Contains -> OptionValue[Contains], MaxCanonicalDimension -> OptionValue[MaxCanonicalDimension], MinCanonicalDimension -> OptionValue[MinCanonicalDimension],
    SelectParticles->OptionValue[SelectParticles],  Exclude4Scalars->OptionValue[Exclude4Scalars], StandAlone->False,NoDefinitions->OptionValue[NoDefinitions]};


  (*To Parallellize or not to Parallelize? *)
  FR$DoPara=If[Global`FR$Parallelize && Length[lagrangian]>40 && $KernelCount>1,True,False];

  (* Expansion of the Lagrangian *)
  Print["Expanding the Lagrangian..."];

  templag=If[FR$DoPara,
    Print["Expanding the indices over ", Global`FR$KernelNumber," cores"];
    ParallelizeMe[ExpandIndices,lagrangian,MyOptions->FRoptions],
    Plus@@(ExpandIndices[#,Sequence@@FRoptions]&/@If[Head[lagrangian]===Plus,(List@@lagrangian),{lagrangian}])
  ];

  If[templag == 0, Print["No vertices found."]; ProcessEmptyList[lagrangian,Name->OptionValue[Name]]; Return[]];

  (* Collecting the vertex structures -> remove non-interaction terms and extract the field content list (FCList) *)
  Print["Collecting the different structures that enter the vertex."];
  FCListtemp=Select[{GetFieldContent[#],#}&/@Listize[templag],Length[#[[1]]]>If[FR$Loop,0,2,2]&];
  CollectedIntLagList=FCListtemp[[All,2]];
  FCListtemp=FCListtemp[[All,1]];
  FCList=List@@@KillDoubles[FCListtemp];
  If[FR$FeynArtsInterface && FA$FlavExpCheck,
    checklist = MR$ClassesList /. FA$ClassToName;
    checklist = KillDoubles[Join[checklist, anti /@ checklist]];
    checklist = KillDoubles[Join[checklist, CC /@ checklist]];
    checklist = DeleteCases[checklist, _?UnphysicalQ];
    If[Complement[KillDoubles[Flatten[FCList]], checklist] =!= {}, FA$FlavExpCheck = False; 
      Print[Style["Warning: Class members in the Lagrangian! Not supported by FeynArts.",Orange,Bold]]; Return[]];
  ];
  CollectedIntLagList = (CollectedIntLagList[[#]]&/@#)&/@ ((Flatten[Position[FCListtemp,#]]&) /@ FCList);
  If[Length[FCList]===0, Print["No vertices found."]; ProcessEmptyList[lagrangian,Name->OptionValue[Name]]; Return[]];
  If[Length[CollectedIntLagList] != Length[FCList], Message[GetVert::FCList]];

  (* Computing the non-zero possible vertices *)
  FR$FeynmanRules=0;

  Print[Length[FCList], " possible non-zero vertices have been found -> starting the computation: ", Dynamic[FR$FeynmanRules], " / ", Length[FCList],"."];

  vertexlist = DeleteCases[If[FR$DoPara,
    SetSharedVariable[FR$FeynmanRules];ParallelizeMe[TreatVertex,CollectedIntLagList], 
    (TreatVertex[#]&/@CollectedIntLagList)],{{_,0}} ];

  vertexlist = DeleteCases[vertexlist, {_,0}];

  (* Output *)
  If[vertexlist=!={},
    ProcessVertexList[vertexlist,lagrangian, Name->OptionValue[Name], ConservedQuantumNumbers->OptionValue[ConservedQuantumNumbers],ApplyMomCons->amc],
    ProcessEmptyList[lagrangian, Name->OptionValue[Name]]
  ];

  Return[];
]; 


(* ::Subsection:: *)
(*TreatVertex*)


TreatVertex[vertx_]:=Block[{tempintterms,tempCreaList, CreaList,vtemp,$HCExtractVertex},
  (*Initialization *)
  MR$IntLorCount = 0; MR$IntCount = 0; Clear[StoreInt]; FR$FeynmanRules++;

  (* Pre-processing the Feynman rules *)
  tempintterms = ExpandTensorStructure /@ vertx;
  tempCreaList = ReOrderCL[List @@ If[Head[tempintterms[[1]]]===Power,(MakeCreaList[tempintterms[[1]]]),(MakeCreaList @@ (tempintterms[[1]]))]];
  CreaList = Reverse[Table[crea[tempCreaList[[k]], {}, k], {k, 1, Length[tempCreaList]}]];
  CreaList = RenameSpin2[RenameSpin3[RenameSpin4[CreaList]]];
  tempintterms = FieldExpand/@tempintterms;

  vtemp = Listize[Plus @@ (FromVertexTerm[#, CreaList, FR$FeynmanRules] &/@ tempintterms)];

  (* Finalizing the Feynman rule*)
  If[FR$FExpand,
    vtemp = Expand[MakeSlashedMatrix[LorentzContract[PerformGaAlgebra[#]]]]&/@vtemp; 
    vtemp = Expand[# /. HC -> $HCExtractVertex /. $HCExtractVertex[t_?(TensQ)][ind___] -> $HCExtractVertex[t[ind]] //. MR$Definitions/. Dot -> FR$Dot /. FR$Dot -> Dot]&/@vtemp; 
    vtemp = Expand[# /. delta -> IndexDelta /. {NTIndex -> Index, NTI -> Index}]&/@vtemp; 
    vtemp = Expand[# /.$HCExtractVertex[t_?(TensQ)[xx___, i_, j_]] -> Conjugate[t[xx, j, i]]/. $HCExtractVertex -> HC]&/@vtemp; 
    ,
    vtemp = MakeSlashedMatrix[LorentzContract[PerformGaAlgebra[#]]]&/@vtemp;
    vtemp = (# /. HC -> $HCExtractVertex /. $HCExtractVertex[t_?(TensQ)][ind___] -> $HCExtractVertex[t[ind]] //. MR$Definitions/. Dot -> FR$Dot /. FR$Dot -> Dot)&/@vtemp;
    vtemp = (# /. delta -> IndexDelta /. {NTIndex -> Index, NTI -> Index})&/@vtemp;
    vtemp = (# /.$HCExtractVertex[t_?(TensQ)[xx___, i_, j_]] -> Conjugate[t[xx, j, i]]/. $HCExtractVertex -> HC)&/@vtemp;
  ];

(*Print[InputForm[vtemp]];*)
  vtemp = If[$VersionNumber>8,Factor[Plus@@vtemp],Simplify[Plus@@vtemp,TimeConstraint->0.01]];(* by celine*)

  (* Returning the vertex *)
  Return[{MakeCreaListoutput[CreaList], vtemp}];
];


(* ::Subsection:: *)
(*ProcessVertexList*)


ProcessEmptyList[lagrangian_,OptionsPattern[]]:=Block[{},
  Vertices[lagrangian] = {};
  Vertices[ToExpression["FR$VertNumb" <> ToString[FR$VertexNumber]]] = {};
];


ProcessVertexList[vlist_,lagrangian_,OptionsPattern[]]:=Block[{vertexlist,tag,vertparticles,conserveqn,amc2=OptionValue[ApplyMomCons],tempvertlistname},
  (* Initialization *)

  tag = ToExpression["FR$VertNumb" <> ToString[FR$VertexNumber]];

  (* Particles in vertices are sorted, and fermions are relabelled (lambar -> lam, CC[psi] -> psibar + identical fermion issue (ticket #84 *)
  vertexlist = MergeAllVertices[vlist]; 
  vertexlist = PutRightSignForIdenticalFermions /@ vertexlist;

  (* Simplify Colour-Eps[i,j,k], and remove possible zeroes *)
  vertexlist = vertexlist /. {Eps[Index[Colour, i_], Index[Colour, j_], Index[Colour, k_]] :> OrderEps[Eps[Index[Colour, i], Index[Colour, j], Index[Colour, k]]]};

  (* Optimization of the index naming scheme *)
  vertexlist = DeleteCases[(List[#[[1]],OptimizeIndex[#[[2]]]] &/@vertexlist), {_, 0}];

  (* Saving the output *)
  If[vertexlist==={}, Vertices[lagrangian] = {}; Vertices[tag] = {}; Return[]];

  Vertices[lagrangian] = If[amc2,ApplyMomentumConservation[vertexlist],vertexlist,ApplyMomentumConservation[vertexlist]];

  Vertices[tag] = vertexlist;

  (* Check for Quantum Number Conservation *)
  vertparticles = ((#1&)@@@#&)/@ vertexlist[[All,1]];
  conserveqn = OptionValue[ConservedQuantumNumbers];
  If[(conserveqn =!= {}) && (conserveqn =!= False), Do[ConserveQN[vertparticles[[yy]], conserveqn], {yy, Length[vertparticles]}]];

  (* Output *) 
  tempvertlistname = OptionValue[Name];
  If[tempvertlistname =!= MR$Null, Vertices[tempvertlistname] = vertexlist];  
];


(* ::Section::Closed:: *)
(*MergeVertexLists*)


MergeVertices[verts__] := Block[{tmp, tmpfc1, tmpfc2, res = {}, tmpres, tmppos},
    tmp = Join[verts];
    tmpfc1 = tmp[[All, 1]];
    tmpfc2 = KillDoubles[tmpfc1];
    tmp = tmp[[All,2]];
    Do[tmppos = Flatten[Position[tmpfc1, tmpfc2[[kmvl]]]];
       tmpres = Plus @@ (tmp[[#]]& /@ tmppos);
       res = Append[res, {tmpfc2[[kmvl]], tmpres}];
       tmppos = List /@ tmppos;
       tmp = Delete[tmp, tmppos];
       tmpfc1 = Delete[tmpfc1, tmppos],
       {kmvl, Length[tmpfc2]}];
    res];
       
       


(* ::Section::Closed:: *)
(*RemoveZeroVertices*)


RemoveZeroVertices[frs_]:=Module[{frs2={}},
Do[If[(NumericalValue[frs[[j,2]]]/.{0.0->0,Complex[0.0,0.0]->0})=!=0,AppendTo[frs2,frs[[j]]]],{j,1,Length[frs]}];
frs2
];
