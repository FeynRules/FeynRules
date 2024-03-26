(* ::Package:: *)

(* ::Title:: *)
(*Class declarations*)


(* ::Text:: *)
(*This file contains the routines that read the model file, and declare all the corresponding classes*)


(* ::Section:: *)
(*Some usefull functions*)


MRIndexRange[ind_] := If[Head[IndexRange[ind]] === NoUnfold, Identity @@ IndexRange[ind], IndexRange[ind]];

$FAToMRRules = If[Not[ValueQ[$FAToMRRules]], {}, $FAToMRRules];
$FlavorExpandRules = If[Not[ValueQ[$FlavorExpandRules]], {}, $FlavorExpandRules];
MR$GaugeGroupList = If[Not[ValueQ[MR$GaugeGroupList]], {}, MR$GaugeGroupList];
MR$FlavorList = If[Not[ValueQ[MR$FlavorList]], {}, MR$FlavorList];
MR$ParameterList = If[Not[ValueQ[MR$ParameterList]], {}, MR$ParameterList];
MR$Definitions = If[Not[ValueQ[MR$Definitions]], {}, MR$Definitions];
M$MixingsDescription = If[Not[ValueQ[M$MixingsDescription]], {}, M$MixingsDescription];
M$vevs = If[Not[ValueQ[M$vevs]], {}, M$vevs];
FR$ReprMap = If[Not[ValueQ[FR$ReprMap]],{},FR$ReprMap];

MR$IndexList = If[Not[ValueQ[MR$IndexList]], {}, MR$IndexList];
MR$ClassNameList = If[Not[ValueQ[MR$ClassNameList]], {}, MR$ClassNameList];
FR$CheckFlavExp = If[Not[ValueQ[FR$CheckFlavExp]], {}, FR$CheckFlavExp];

Options[AddParticlesToClass] = {ClassName -> Hold[$defaultClassName], FlavorIndex -> MR$NoFlavorIndex, Indices -> {}, SymmetricIndices -> {}, 
                                AntiSymmetricIndices -> {}, Symmetric -> False, AntiSymmetric -> False};

FlavoredQ[_] := False;
FlavoredQ[del[field_, _]] := FlavoredQ[field];
FlavoredQ[field_?(FieldQ)[___]] := FlavoredQ[field];
FlavoredQ[field_?(AntiFieldQ)] := FlavoredQ[anti[field]];

(*GenerationIndices[ind__] := $GenList = {ind};*)

SelfConjugateQ[x_?AntiFieldQ] := SelfConjugateQ[anti[x]];
SelfConjugateQ[CC[x_]] := SelfConjugateQ[x];

MakeZERO[{_, ZERO}] := ZERO;
MakeZERO[{_, 0}] := ZERO;
MakeZERO[{ZERO, _}] := ZERO;

AddDefinitions[deflist_List] := Block[{tempdeflist, tempdeflist1, tempdeflist2, tempdeflist3, tempdeflist4},
    If[deflist =!= {},
(* CD, 12.05.2010: Needed to add Hold to this function, to protect module from evaluation *)
       tempdeflist = deflist  //. $TensIndRules  /. Module->MyModule /. Hold->MyHold /. ReleaseHold :> RHold /. RHold[MyHold[MyModule[xx__]]] :>  HoldModule[xx]; 
(*End fix *)
       tempdeflist1 = tempdeflist //. {Rule[x_, _] -> x, RuleDelayed[x_, _] -> x};
       tempdeflist2 = tempdeflist //. {Rule[_, y_] -> y, RuleDelayed[_, y_] -> y};
       tempdeflist1 = PrePutIndices[tempdeflist1];
       tempdeflist2 = PrePutIndices[tempdeflist2];
       tempdeflist = KillDoubles[Join[tempdeflist, MyRuleDelayed @@@ Transpose[{tempdeflist1, tempdeflist2}]] //. MyRuleDelayed -> RuleDelayed];
(* CD, continue fix *)
       tempdeflist = tempdeflist /. {HoldModule[list_, expr_] :> ReleaseHold[Hold[Module[list,expr]]], MyModule->Module, MyHold -> Hold, RHold -> ReleaseHold};
(* end fix *)
       MR$Definitions = KillDoubles[Join[deflist, tempdeflist, $FlavorExpandRules]]]]; 
       
CheckParameters[x_?(Not[FreeQ[#, Equal]]&)] := x;
CheckParameters[x_?(FreeQ[#, Equal]&)] := (x == {});

KillMassValue[{x_, _}] := x;
KillMassValue[x_?(Head[#] =!= List &)] := x;

PrintList[ll_List] := Do[Print[ll[[llkk]]], {llkk, Length[ll]}];

GhToGB[field_?(AntiFieldQ[#] === True &)] := HCanti[GhToGB[HCanti[field]]];
GBToGh[field_?(AntiFieldQ[#] === True &)] := HCanti[GBToGh[HCanti[field]]];
    





(* ::Subsection:: *)
(*DeclareNewDefinition*)


(* ::Text:: *)
(*DeclareNewDefinition[ rule, outlist ] adds a new definition to outlist, and returns the new list. If outlist is not a list, then Null is returned.*)


DeclareDef::usage = "The second argument must be a list.";


DeclareNewDefinition[rule_, outputlist_]:=Block[{tempdef = rule,tempdeflhs,tempdefrhs,MyModule,modulelist,tempspinindex,indexname, newoutputlist, MyRuleDelayed,sumspin},

   (* Check if outputlist already exists and if it is a list. If not, print a warning and exit, returning Null *)

   If[Not[ValueQ[outputlist]] | (Head[outputlist] =!=List),
      Message[DeclareDef::usage];Return[]];

   (* Add the new definition to outputlist *)
   newoutputlist = Join[outputlist, tempdef];

   (* If there are no fields involved, we are done*)
   If[FreeQ[tempdef,_?FieldQ],
      Return[newoutputlist]
     ];

   (* Otherwise, we have to add the rules for the hermitian conjugate of the field, etc. *)

   tempdeflhs = tempdef //. {Rule[xx_,yy_] :> xx, RuleDelayed[xx_,yy_] :> xx};
   tempdefrhs = tempdef //. {Rule[xx_,yy_] :> Hold[yy], RuleDelayed[xx_,yy_] :> Hold[yy]};
   tempdefrhs = ReleaseHold[tempdefrhs /. Module -> MyModule];

   Do[
       If[Head[tempdefrhs[[kkfr]]] === MyModule, 
          modulelist[kkfr] = tempdefrhs[[kkfr, 1]]; tempdefrhs[[kkfr]] = tempdefrhs[[kkfr, 2]],
          modulelist[kkfr] = {}],
      {kkfr, Length[tempdefrhs]}];

   tempdeflhs = HCanti /@ tempdeflhs;
   tempdefrhs = HCanti /@ tempdefrhs;
   tempdeflhs = tempdeflhs //. Module->MyModule //. {HCanti[MyModule[{inds__},expr_]]:>MyModule[{inds},HCanti[expr]]} //. MyModule->Module;
   tempdefrhs = tempdefrhs //. Module->MyModule //. {HCanti[MyModule[{inds__},expr_]]:>MyModule[{inds},HCanti[expr]]};

   If[Not[FreeQ[tempdeflhs, Ga[0,___]]],       
      tempdeflhs = tempdeflhs /. {Ga[0,r_,s_] field_[s_, xx___] :> field[r, xx], Ga[0,s_,r_] field_[s_, xx___] :> field[r, xx]};
      tempspinindex = tempdeflhs[[1,1,1]];
      indexname = Symbol["newname"];
      tempdefrhs = tempdefrhs //. tempspinindex -> indexname;
      Do[ tempdefrhs[[kk]] = If[AntiFieldQ[Head[tempdeflhs[[kk]]]], 
                               Expand[tempdefrhs[[kk]]Ga[0, indexname, tempspinindex]],
                               Expand[tempdefrhs[[kk]]Ga[0, tempspinindex, indexname]]],
         {kk, Length[tempdefrhs]}]
     ];

    Do[If[modulelist[kkfr] =!= {},
          sumspin = Cases[modulelist[kkfr],_?(FreeQ[tempdefrhs[[kkfr]],#]&)];
          If[Length[sumspin]>0,sumspin=sumspin[[1]],sumspin={}]; 
 	     tempdefrhs[[kkfr]]= MyModule[modulelist[kkfr], If[Not[Length[sumspin]==={}],Replace[tempdefrhs[[kkfr]],(ma:ProjP|ProjM|_TensorDot|_Ga)[a_, sp1_]*b_[a_, ff___]*c___:>c*ma[sumspin,sp1]*b[sumspin,ff]],tempdefrhs[[kkfr]]]]], 
      {kkfr, Length[tempdefrhs]}];

    tempdefrhs = tempdefrhs /. MyModule[ll_, exi_] :> Hold[Module[ll, exi]];
    If[(tempdeflhs =!= tempdef //. {Rule[xx_,yy_] :> Hold[xx], RuleDelayed[xx_,yy_] :> Hold[xx]}) ||(tempdefrhs =!= tempdef //. {Rule[xx_,yy_] :> Hold[yy], RuleDelayed[xx_,yy_] :> Hold[yy]}),

(* add all this to newoutputlist *)
       newoutputlist = Join[newoutputlist,Table[MyRuleDelayed[tempdeflhs[[mm]], tempdefrhs[[mm]]], {mm, Length[tempdef]}]] //. MyRuleDelayed[xx_,yy_] :> RuleDelayed[xx, ReleaseHold[yy]]
       ];

(* Finally return and exit *)
Return[newoutputlist]

];


(* ::Section:: *)
(*Reading the model file*)


(* ::Subsection::Closed:: *)
(*MakeNumQ*)


MakeNumQ[eparams_,iparams_]:=Block[{

  es ={#[[1]],#[[-3]],#[[-2]]}&/@(#[[2]]&/@Join@@EParamList[[All,2]]),
  is={#[[1]],#[[-2]]}&/@IParamList,
  tester=False
  
  },

  (* Test if all external parameters are real *)
  If[Im[#[[2]]]!=0,tester=True]& /@ es;
  If[tester,Message[LoadModel::ExtParams]];

  (* Create numQ and CnumQ *)
  es = Delete[#,{2}]&/@es;
  is = Join[es,is];

  Set[numQ[#],True]&/@(#[[1]]&/@is);
  Set[CnumQ[#1],#2]&@@@is;

];



(* ::Subsection:: *)
(*ReadAuthors*)


Options[ReadAuthors] = {Authors -> MR$Null, Institutions -> MR$Null, Emails -> MR$Null, Date -> MR$Null, References -> MR$Null, Version -> 0, URLs -> MR$Null};

ReadAuthors[authorlist_] := Block[{temp,dt},
      MR$Authors = Flatten[{Authors /. authorlist /. Options[ReadAuthors]}];
      MR$Institutions = Flatten[{Institutions /. authorlist /. Options[ReadAuthors]}];
      MR$Emails = Flatten[{Emails /. authorlist /. Options[ReadAuthors]}];
      MR$References = Flatten[{References /. authorlist /. Options[ReadAuthors]}];
      MR$URLs = Flatten[{URLs /. authorlist /. Options[ReadAuthors]}];
      MR$Date = Flatten[{Date /. authorlist /. Options[ReadAuthors]}];
	  If[MR$Date==={MR$Null},dt=Date[];MR$Date={ToString[dt[[1]]]<>"."<>ToString[dt[[2]]]<>"."<>ToString[dt[[3]]]};];
      MR$Version = Flatten[{Version /. authorlist /. Options[ReadAuthors]}];
      If[MR$Authors =!= {MR$Null}, 
         Print["This model implementation was created by"];
         PrintList[MR$Authors];
         Print["Model Version: ", ToString @@ MR$Version];
		 If[MR$References =!= {MR$Null},
			Print["Please cite"];
		    PrintList[MR$References];
		 ];
         If[MR$URLs =!= {MR$Null},
            PrintList[MR$URLs]];
         Print["For more information, type ModelInformation[]."];
		 Print[""]]];

ModelInformation[] := Block[{tmp},
      Print["Model name: ", M$ModelName];
      Print["Model version: ", ToString @@ MR$Version];
      If[(MR$Date =!= {MR$Null}) && ValueQ[MR$Date], Print["Date: ", Sequence @@ MR$Date]];
      If[(MR$Authors =!= {MR$Null}) && ValueQ[MR$Authors], Print["Authors: "]; PrintList[StringJoin["   ", #]& /@ MR$Authors]];
      If[(MR$Institutions =!= {MR$Null}) && ValueQ[MR$Institutions], Print["Institutions: "]; PrintList[StringJoin["   ", #]& /@ MR$Institutions]];
      If[(MR$Emails =!= {MR$Null}) && ValueQ[MR$Emails], Print["Emails: "]; PrintList[StringJoin["   ", #]& /@ MR$Emails]];
      If[(MR$References =!= {MR$Null}) && ValueQ[MR$References], Print["References: "]; PrintList[StringJoin["   ", #]& /@ MR$References]];
      If[(MR$URLs =!= {MR$Null}) && ValueQ[MR$URLs], Print["URL's: "]; PrintList[StringJoin["   ", #]& /@ MR$URLs]]];

      
         
      


(* ::Subsection:: *)
(*LoadModel (one argument)*)


Options[LoadModel] = {Output -> MR$Default, ClassName -> MR$Null, ClassMembers -> {}, FlavorIndex -> MR$Null, QuantumNumbers -> {},
                      Mass -> MR$Null, Width -> MR$Null, PropagatorType -> MR$Null, PropagatorLabel -> MR$Null, 
                      ParticleName -> MR$Null, AntiParticleName -> MR$Null, GaugeIndex -> S, PDG -> NoPDG, Abelian -> MR$Null,
                      GaugeBoson -> MR$Null, CouplingConstant ->MR$Null, Charge -> MR$Null, Representations -> MR$Null,
                      StructureConstant -> MR$Null, AdjointIndex -> MR$Null, Indices -> MR$Null, TensorClass -> MR$Null, Unphysical -> False,
                      Definitions -> MR$Null, ParameterType -> External, BlockName -> MR$Null, ParameterName -> MR$Null, InteractionOrder -> MR$Null,
                      Value -> NoValue[1], Definition -> MR$Null, OrderBlock -> MR$Null, ComplexParameter -> False,
                      Unitary -> False, Orthogonal -> False, Hermitian -> False, SymmetricIndices -> {}, AntiSymmetricIndices -> {},
                      Symmetric -> MR$Null, AntiSymmetric -> MR$Null, TeX -> MR$Null, AllowSummation -> False, MajoranaPhase -> 0,
                      SymmetricTensor -> MR$Null, FullName -> MR$Null, Description -> MR$Null, TeXParticleName -> MR$Null, TeXAntiParticleName -> MR$Null, 
                      Goldstone -> NoGS, Ghost -> NoGhost, SelfConjugate -> MR$Null, Report -> False, Restriction -> None, (* MW edit *) Charges -> {},
                      VeVs -> 0, GaugeXi -> MR$Null, WeylComponents -> {}, Chirality -> Automatic, Dynkin -> MR$Null, GUTNormalization->1, Casimir->MR$Null};

LoadModel[modfile_String] := LoadModel[modfile, Report -> False];


LoadModel[] := LoadModel["MergedModel"];

LoadModel[modfile_String, rule_Rule] := Block[{templist, MRoutput, tempq, masslist, widthlist, tempwidthlist, $report, temprestric,curDir},

	(*Parallelize - NC*)
	If[Global`FR$Parallelize&&Length[Kernels[]]>0&&modfile =!= "MergedModel",(*Print["1) N_kernels=",Length[Kernels[]]];*)
		curDir=Directory[];
		DistributeDefinitions[curDir];
		ParallelEvaluate[
			Begin["Global`"];(*NC: The subkernels have System` as their default context which causes shadowing of the model defined parameters. *)
			$Output={};
			SetDirectory[curDir];
			LoadModel[modfile, rule];
			$Output={OutputStream["stdout",1]};
		];
	];
	(*End Parallelize - NC*)

    (* Reinitialisation *)
    If[MR$ModelLoaded,
       Clear[EParamList, IParamList, PartList, MassList, WidthList];
       Clear[MR$ClassesDescription, MR$GaugeGroups, MR$Parameters,M$MixingsDescription, M$Superfields];
       Clear[MR$ClassesList, MR$GaugeGroupList, MR$ParameterList];
       Clear[MR$ClassesRules, MR$GaugeGroupRules, MR$ParameterRules];
       Clear[MR$FieldList, MR$QuantumNumbers];
       MR$ClassNameList = {}];
    
    MR$Definitions = MR$DefinitionsDefault;
   
   FR$OptimizeParams = {};
    
    (*Reading *)
    If[modfile =!= "MergedModel", 
       Get[modfile]];

   
   (* We check whether there are new gauge representations to be declared *)
   If[FR$NewGaugeRepresentations =!= {},
      UpdateGaugeRepresentations[FR$NewGaugeRepresentations]
      ];

   (* We add the Grassmann theta definitions to be declared along with the other spinor fields *)
   M$ClassesDescription=Join[M$ClassesDescription,M$GrassmannTheta];

    (* Initialisation *)
    $report = Report /. {rule} /. Report -> False;
    MR$currentmodel = modfile;
    If[Not[ValueQ[M$ModelName]], M$ModelName = StringReplace[MR$currentmodel, {"/"->"","\\"->""}];
                                 If[StringTake[M$ModelName, -3] === ".fr",  M$ModelName = StringDrop[M$ModelName, -3]]];
    If[MR$ModelLoaded === True, Message[LoadModel::Loaded], MR$ModelLoaded = True];
    If[ValueQ[M$Information], ReadAuthors[M$Information]];
    MR$FieldList = {};

    (* Loading in the particles classes *)
    If[Not[ValueQ[M$ClassesDescription]], Message[LoadModel::NoClasses]; Abort[],
      MR$ClassesDescription = M$ClassesDescription /. {(xx_ == yy_) -> (xx -> yy)};
      MR$ClassesList = M$ClassesDescription /. {(x_ == y_) -> x};
      templist = M$ClassesDescription /. {(x_ == y_) -> y};
      Do[MR$ClassesRules[MR$ClassesList[[kk]]] = templist[[kk]],
        {kk, Length[M$ClassesDescription]}];

    (* Loading in the gauge groups *)
      If[ValueQ[M$GaugeGroups] && (Length[M$GaugeGroups] != 0), 
         MR$GaugeGroups = M$GaugeGroups /. {(x_ == y_) -> (x -> y)};
         MR$GaugeGroupList = M$GaugeGroups /. {(x_ == y_) -> x};
         templist = M$GaugeGroups /. {(x_ == y_) -> y};
         Do[MR$GaugeGroupRules[MR$GaugeGroupList[[kk]]] = templist[[kk]], {kk, Length[M$GaugeGroups]}]];

    (* Loading in the parameters *)
      If[ValueQ[M$Parameters] && (Length[M$Parameters] != 0), 
         M$Parameters = CheckParameters /@ M$Parameters;
         MR$Parameters = M$Parameters /. {(x_ == y_) -> (x -> y)};
         MR$ParameterList = M$Parameters /. {(x_ == y_) -> x};
         templist = M$Parameters /. {(x_ == y_) -> y};
(* BF Small addition here *)
         Do[
           MR$ParameterRules[MR$ParameterList[[kk]]] = templist[[kk]];
           If[(ParameterName/.templist[[kk]])=!=MR$ParameterList[[kk]], MR$ParameterRules[(ParameterName/.templist[[kk]])] = templist[[kk]]],
         {kk, Length[M$Parameters]}]];
(* End BF *)
    (* Go through indices, to unfold them *)
    UnfoldIndices[];

   M$ClassesDescription = Cases[M$ClassesDescription, Equal[_,_List]];
   (*M$MixingDescriptions = DeleteCases[TestM$MixingDescription /@ M$MixingDescriptions, MR$Null];*)

    (* Declaration corresponding to the output *)
       FR$Declaration[$report];
       DeclareParametersMG;
       DeclareParticlesMG;
       masslist = MassList[[2]] //. NoPDG -> Identity //. {{n_}, m1_, m2_} -> {n, m1, m2};
       Do[If[masslist[[ml, 3]] =!= Internal, NumericalValue[masslist[[ml, 2]]] = NumericalValue[masslist[[ml, 3]]]];
          PDGToMass[masslist[[ml, 1]]] = masslist[[ml, 2]], {ml, Length[masslist]}];
       widthlist = Rest /@ WidthList[[2]];
       Do[If[widthlist[[ml, 2]] =!= Internal,  NumericalValue[widthlist[[ml, 1]]] = NumericalValue[widthlist[[ml, 2]]]];
          tempwidthlist = WidthList[[2]] //. NoPDG -> Identity //. {{n_}, m1_, m2_} -> {n, m1, m2};
          PDGToWidth[tempwidthlist[[ml,1]]] = tempwidthlist[[ml,2]], {ml, Length[widthlist]}];
       AddDefinitions[MR$Definitions];
       Do[MR$QuantumNumbers[[xx]][_] := 0, {xx, Length[MR$QuantumNumbers]}];
       Q[_] := 0;
       $OptimizedParamRules = $MakeOptimizedParamRules[];
       Print["\nModel ", M$ModelName, " loaded."]];
       M$IndParam=DeleteCases[If[(Indices/.MR$ParameterRules[#])=!=Indices,#,{}]&/@MR$ParameterList,{}];
       temprestric = Restriction/.{rule}/.Options[LoadModel];
       If[temprestric =!= None, 
          LoadRestriction[temprestric]];

    (* BF:: Declare superfields *)
    If[M$Superfields =!= {},
       SuperfieldDeclaration[M$Superfields]; 
       GaugeGroupTest[]; 
      ];
  
    (* BF:: Update the PDG ids *)
    Module[{pdglist, newpdgs,pdglist2,newIDs={},counter=9000001,myRule},
      (* PDG lists *)
      pdglist=DeleteCases[Flatten[(PartList[[All,2]]),1][[All,-5]],NoPDG[_]]; pdglist2=pdglist;
      newpdgs=Cases[Flatten[PartList],NoPDG[_]]/.NoPDG[a_]->a;
      newpdgs=DeleteCases[If[MemberQ[pdglist,#],#,pdglist2=Append[pdglist2,#];{}]&/@newpdgs,{}];

     (* Get the required number of new PDGs IDs *)
     Do[While[MemberQ[pdglist2,counter],counter=counter+1];newIDs=Append[newIDs,counter];counter=counter+1,{kkk,1,Length[newpdgs]}];

     (* Update the lists *)
     PartList=PartList/.(Inner[myRule,newpdgs,newIDs,List]/.myRule[a_,b_]->myRule[NoPDG[a],NoPDG[b]]/.myRule->Rule);
     MassList=MassList/.(Inner[myRule,newpdgs,newIDs,List]/.myRule[a_,b_]->myRule[NoPDG[a],NoPDG[b]]/.myRule->Rule);
     WidthList=WidthList/.(Inner[myRule,newpdgs,newIDs,List]/.myRule[a_,b_]->myRule[NoPDG[a],NoPDG[b]]/.myRule->Rule);
    ];



   (* BF: NEW mixing initialization *)
   InitializeMixings[];
   (*END BF *)

    (* update numQ and CnumQ *)
    MakeNumQ[EParamList, IParamList];

    (* Initialize Interaction order hierarchy *)
    InitializeInteractionOrders[ M$InteractionOrderHierarchy, M$InteractionOrderLimit ];

    (* Declare the form factors *)
    If[FR$FormFactors =!= {},
       DeclareFormFactors[M$FormFactors];
      ];


    (* Build the FAToFR particle replacement list *)
    BuildFeynArtsToFeynRulesParticles[];

    (*Update*)
     Update[DC]; 
     Update[CC]; 
     Update[HC]; 
     Update[HCanti]; 
     Update[];
     BuildPDGs[];

];




(* ::Subsection:: *)
(*LoadModel (multiple arguments) *)


LoadModel[modfile1_String, modfiles___, modfile2_String] := LoadModel[modfile1, modfiles, modfile2, Report -> False]

LoadModel[modfile1_String, modelfiles__, rule_Rule] := Block[{frfiles = {modfile1, modelfiles}, 
   loadparams, loadgroups, loadparts, loadindices, loadmixings, loadhier, loadexplimit, loadsuperfields, loadformfactors,curDir},
   
	(*Parallelize - NC*)
	If[Global`FR$Parallelize&&Length[Kernels[]]>0,(*Print["2) N_kernels=",Length[Kernels[]]];*)
		curDir=Directory[];
		DistributeDefinitions[curDir];
		ParallelEvaluate[
			Begin["Global`"];(*NC: The subkernels have System` as their default context which causes shadowing of the model defined parameters. *)
			$Output={};
			SetDirectory[curDir];
			LoadModel[modfile1,modelfiles, rule];
			$Output={OutputStream["stdout",1]};
		];
	];
	(*End Parallelize - NC*)

   Print["Merging model-files..."];

   Do[Clear[M$Parameters,M$GaugeGroups,M$ClassesDescription, M$InteractionOrderHierarchy, M$InteractionOrderLimit, M$Superfields, M$MixingsDescription];

      Get[frfiles[[kk]]];

      loadparts[kk]       = If[ValueQ[M$ClassesDescription],M$ClassesDescription,{}];
      loadparams[kk]      = If[ValueQ[M$Parameters],M$Parameters,{}];
      loadgroups[kk]      = If[ValueQ[M$GaugeGroups],M$GaugeGroups,{}];
      loadmixings[kk]     = If[ValueQ[M$MixingsDescription], M$MixingsDescription, {}];
      loadhier[kk]        = If[ValueQ[M$InteractionOrderHierarchy], M$InteractionOrderHierarchy, {}];
      loadexplimit[kk]    = If[ValueQ[M$InteractionOrderLimit], M$InteractionOrderLimit, {}];
      loadsuperfields[kk] = If[ValueQ[M$Superfields], M$Superfields, {}];
      loadformfactors[kk] = If[ValueQ[M$FormFactors], M$FormFactors, {}],
      {kk,Length[frfiles]}];

   M$Parameters                 = Join @@ Table[loadparams[kk],{kk,Length[frfiles]}];
   M$GaugeGroups                = Join @@ Table[loadgroups[kk],{kk,Length[frfiles]}];
   M$ClassesDescription         = Join @@ Table[loadparts[kk],{kk,Length[frfiles]}];
   M$MixingsDescription         = Join @@ Table[loadmixings[kk],{kk,Length[frfiles]}];
   M$InteractionOrderHierarchy  = Join @@ Table[loadhier[kk],{kk,Length[frfiles]}];
   M$InteractionOrderLimit      = Join @@ Table[loadexplimit[kk],{kk,Length[frfiles]}];
   M$Superfields                = Join @@ Table[loadsuperfields[kk],{kk,Length[frfiles]}];
   M$FormFactors                = Join @@ Table[loadformfactors[kk],{kk,Length[frfiles]}];

   (* Make sure only the last one is kept in each case *)
   M$InteractionOrderHierarchy = OverwriteInteractionOrders[M$InteractionOrderHierarchy, Type -> "Hierachy"];
   M$InteractionOrderLimit     = OverwriteInteractionOrders[M$InteractionOrderLimit    , Type -> "Hierachy"];

   

   Clear[loadparts,loadgroups,loadparams,loadformfactors];
   loadparams = KillDoubles[M$Parameters/.Equal[x_,_]->x];
   If[Length[loadparams] != Length[M$Parameters], Message[MergeModels::Params]];

   loadgroups = KillDoubles[M$GaugeGroups/.Equal[x_,_]->x];
   If[Length[loadgroups] != Length[M$GaugeGroups], Message[MergeModels::Gauge]];

   loadparts = KillDoubles[M$ClassesDescription/.Equal[x_,_]->x];
   If[Length[loadparts] != Length[M$ClassesDescription], Message[MergeModels::Particles]];

   loadformfactors = KillDoubles[M$FormFactors/.Equal[x_,_]->x];
   If[Length[loadformfactors] != Length[M$FormFactors], Message[MergeModels::FormFactors]];

   LoadModel["MergedModel",rule];

   loadindices = KillDoubles[MR$IndexList];
   If[Length[loadindices] != Length[MR$IndexList], Message[MergeModels::Index]];

];


(* ::Subsection:: *)
(*Restrictions and Definitions*)


DeconvoluteEParamBlock[{lhablockname_, block_}] := Prepend[#, lhablockname]& /@ block;

ReconvoluteEParamBlock[block_] := {block[[1,1]], Rest /@ block};


CreateParamListEntryFromDeconvolutedEParamListEntry[{lhablock_, lhanumber_, entry_List}] := Block[{entr = entry},

    If[Length[entr] == 6,
       entr = Insert[Delete[entr, {4}], entr[[4]], 2],
       If[Length[entr] == 8,
         entr = Insert[Delete[entr, {6}], entr[[6]], 2],
         If[Length[entr]>8,Print["More than 2 interaction order? not handle"];]
         ];
      ];

    Return[Insert[entr, lhablock, 3]];

];

    


ResetNumericalValues[] := Block[{
   params = Delete[#, {2}]& /@ ParamList[[All, 1;;3]],
   params1, params2, params3,
   InvParamRules = (Reverse /@ ParamRules) /. Index[_, k_] :> k
   },

   (* params1: Fully dressed *)
   params1 = MapAt[# /. InvParamRules //.PRIVATE`$TensIndRules/.{f_?(NoTensQ[#] === True &)[ind__?(FreeQ[##, NTIndex]&)] :> NoTensPutInd[f, {ind}]} /. NTIndex -> Index&,  #, 1]& /@ params;
   (* params2: Index tag remoxed *)
   params2 = MapAt[# /. Index[_, k_] :> k&, #, 1]& /@ params1;
   (* MC form *)
   params3 = MapAt[# //. ParamRules&, #, 1]& /@ params1;

   (* Reset the values *)
   SetDelayed[NumericalValue[#1], NumericalValue[#2]]& @@@ Join[params1, params2, params3];

];
   
    


ConvoluteEIToParamList[elist_, ilist_] := Block[{eparams = elist, iparams = ilist},

    iparams = Insert[#, Int, 2]& /@ iparams;

    eparams = Delete[#, {2}]& /@ eparams;
    eparams = MapAt[If[Length[#]===6,Insert[Delete[#,{4}],#[[4]],2],If[Length[#]===8,Insert[Delete[#,{6}],#[[6]],2],If[Length[#]>8,Print["More than two interactionorder, not handled"]];#]]&,#,2]& /@ eparams;
    eparams = Insert[#, Ext, 2]& /@ (Insert[#2, #1,3]& @@@ eparams);

    Return[Join[eparams, iparams]];

];


CleanParamLists[] := Block[{
   ipl = IParamList,
   epl = Join @@ (DeconvoluteEParamBlock /@ EParamList),
   eplsave,
   pl = ParamList,
   InvParamRules = (Reverse /@ ParamRules) /. Index[_, k_] :> k
   },

   eplsave = epl;


   ipl = MapAt[# //. MR$Restrictions /. InvParamRules //.PRIVATE`$TensIndRules/.{f_?(PRIVATE`NoTensQ[#] === True &)[ind__?(FreeQ[##, NTIndex]&)] :> PRIVATE`NoTensPutInd[f, {ind}]} /. NTIndex -> Index //. MR$Restrictions/. Index[PRIVATE`DUMMY, a_] :> a  //. ParamRules &, #, 1]& /@ IParamList; 
   IParamList = Intersection[IParamList, ipl];
   IParamList = MapAt[# //. MR$Restrictions /. InvParamRules //.PRIVATE`$TensIndRules/.{f_?(PRIVATE`NoTensQ[#] === True &)[ind__?(FreeQ[##, NTIndex]&)] :> PRIVATE`NoTensPutInd[f, {ind}]} /. NTIndex -> Index //. MR$Restrictions/. Index[PRIVATE`DUMMY, a_] :> a  //. ParamRules &, #, 2]& /@ IParamList; 
   IParamList = IParamList /. ParamRules;

   epl = MapAt[MapAt[# //. MR$Restrictions /. InvParamRules //.PRIVATE`$TensIndRules/.{f_?(PRIVATE`NoTensQ[#] === True &)[ind__?(FreeQ[##, NTIndex]&)] :> PRIVATE`NoTensPutInd[f, {ind}]} /. NTIndex -> Index //. MR$Restrictions/. Index[PRIVATE`DUMMY, a_] :> a  //. ParamRules &, #, 1]&, #, 3]& /@ epl;
   epl = Intersection[eplsave, epl];
   epl = epl /. ParamRules;

   IParamList = ParameterWaterfall[IParamList];

   ParamList = ConvoluteEIToParamList[epl, IParamList];

   EParamList = ReconvoluteEParamBlock /@ GatherByFirstElement[epl];



];
   


MR$Restrictions = {};


Options[AddDefinition]={Output->True};

AddDefinition[Rule[x_,y_]|RuleDelayed[x_,y_],options___]:=Block[{temprule1,temprule2,temprule3,temprule4,
  tempipl=IParamList,
  tempepl= Join @@ (DeconvoluteEParamBlock /@ EParamList),
  tempepllist,
  temppl=ParamList,
  tempml=MassList[[2]],
  tempwl=WidthList[[2]], 
  tempx = x,
  tempx2, tempx3,
  position, newparam, index, InvParamRules2, row, 
  SetMassAccordingtoPDG},
  SetMassAccordingtoPDG[a_] := (Mass[PDGtoSymb[a]] = y);


  FR$restrictionCounter++;
  InvParamRules = (Reverse/@ParamRules);
  InvParamRules2 = InvParamRules /. Index[_, k_] :> k;

  (* Update MR$Definitions with new definitions *)
  temprule1=RuleDelayed[x,y] /. InvParamRules /.InvParamRules2 //.$TensIndRules//.{f_?(NoTensQ[#] === True &)[ind__?(FreeQ[##, NTIndex|Index]&)] :> NoTensPutInd[f, {ind}],
       f_?(NoTensQ[#] === True &)[Index[name_, k_]] :> NoTensPutInd[f, {k}]};

  temprule2=temprule1/.NTIndex->Index;
  temprule3 = temprule2 /. Index[_,k_Integer]:>k;
  temprule4 = temprule2 /. ParamRules;
  MR$Restrictions=Join[MR$Restrictions,Union[{RuleDelayed[x,y],temprule1,temprule2,temprule3,temprule4}]];

  (* Update internal parameters *)
  tempipl=tempipl/.InvParamRules;
  (*tempipl=DeleteCases[tempipl,_?(((#[[1]]//.temprule2)=!=#[[1]])||((#[[1]]//.RuleDelayed[x,y])=!=#[[1]])&)];*)
  tempipl = MapAt[# //. RuleDelayed[x,y] //. temprule4 &, #, 2]& /@ tempipl;
  IParamList=tempipl/.ParamRules;

  (* Update the exernal params *)
  (* We start by determining the one that needs to be replaced *)
  tempepllist = tempepl[[All, 3, 1]];
  position = Flatten[Position[tempepllist, tempx|tempx2]];
  If[Length[position] =!= 0,
     If[Length[position] =!= 1,
        Print["Something went wrong..."],
        (*else*)
        position = position[[1]];
        newparam = tempepl[[position, 3]];
        index = Length[newparam] - 2;
        newparam = Append[tempepl[[position, 1;;2]], ReplacePart[newparam, index -> y]];
        (* Throw away zeroes *)
        tempepl = ReplacePart[tempepl, position -> newparam];
        ];
      ];

  ParamList= Join[Insert[#,Ext,2]& /@ (CreateParamListEntryFromDeconvolutedEParamListEntry /@ tempepl), Insert[#,Int,2]& /@ IParamList];
  EParamList = ReconvoluteEParamBlock /@ (GatherByFirstElement[tempepl]);

  row = Select[Join[tempml, tempwl], Not[FreeQ[#, x]]&];
  If[row =!= {},
     row = Flatten[First /@ row];
     SetMassAccordingtoPDG /@ row;
    ];
     


  If[y===0,
     tempml=DeleteCases[tempml,_?(Not[FreeQ[#,x]]&)];
     tempwl=DeleteCases[tempwl,_?(Not[FreeQ[#,x]]&)];
     MassList={MassList[[1]],tempml};
     WidthList={WidthList[[1]],tempwl};
     PartList=PartList/.RuleDelayed[x,ZERO],
    (*else*)
     If[NumericQ[y],
       tempml=If[Not[FreeQ[#, x]], ReplacePart[#,3->y],#]& /@ tempml;
       tempwl=If[Not[FreeQ[#, x]], ReplacePart[#,3->y],#]& /@ tempwl;
       MassList={MassList[[1]],tempml};
       WidthList={WidthList[[1]],tempwl}]
     ];

     NumericalValue[x] := y;

  If[Output /. {options} /.Options[AddDefinition],
  Print["Definition and parameter lists updated"]]]


LoadRestriction[filename_String] := Block[{curDir},

    LoadRestriction[filename,1];

(*	(*Parallelize - NC*)
	If[Global`FR$Parallelize&&Length[Kernels[]]>0,
		ParallelEvaluate[
			Begin["Global`"];(*NC: The subkernels have System` as their default context which causes shadowing of the model defined parameters. *)
			$Output={};
			ResetNumericalValues[];
		    CleanParamLists[];
		    MR$Definitions = Join[MR$Definitions, MR$Restrictions];
			$Output={OutputStream["stdout",1]};
		];
	];
	(*End Parallelize - NC*)*)
  
    ResetNumericalValues[];
    CleanParamLists[];

    MR$Definitions = Join[MR$Definitions, MR$Restrictions];
    

];

LoadRestriction[filelist__, filename_String] := Block[{tmp,curDir},

	LoadRestriction[#, 0]& /@ {filelist};
    LoadRestriction[filename];

];


LoadRestriction[filename_String, nprint_Integer]:=Block[{curDir, split, ParalEval},

	(*Parallelize - NC*)
(*	If[Global`FR$Parallelize&&Length[Kernels[]]>0,
		curDir=Directory[];
		DistributeDefinitions[curDir];
		ParallelEvaluate[
			Begin["Global`"];(*NC: The subkernels have System` as their default context which causes shadowing of the model defined parameters. *)
			$Output={};
			SetDirectory[curDir];
			LoadRestriction[filename,nprint];
			$Output={OutputStream["stdout",1]};
		];
	];
	(*End Parallelize - NC*)*)

   If[filename =!= "", Get[filename]];
   (* Set up the counter *)

   FR$restrictionCounter = 0;

   If[Global`FR$Parallelize,
      UpdateFRDistributedVariables[]; SetSharedVariable[MR$Definitions];
     ];
   Print["Loading restrictions from ", filename, " : ", Dynamic[FR$restrictionCounter]," / ",Length[M$Restrictions]];
   AddDefinition[#,Output->False]&/@M$Restrictions;
   If[Global`FR$Parallelize,
      UpdateFRDistributedVariables[]; SetSharedVariable[MR$Definitions];
     ];



   If[nprint == 1, Print["Restrictions loaded."]]];


LoadRestriction[] := LoadRestriction[""];


(* ::Section::Closed:: *)
(*Unfold[]*)


(* ::Text:: *)
(*IndexUnfold[Index[name]] adds name to list of indices that should automatically be expanded over (FR$AutoFlavorExpand), and then returns Index[name]*)


IndexUnfold[Index[name_]] := (AppendTo[FR$AutoFlavorExpand, name]);


(* ::Text:: *)
(*UnfoldIndices[] goes through all indices, and if the IndexRange of this index has an Unfold tag, it applies IndexUnfold to the Index, and and then sets the new IndexRange, where the tag was removed.*)


UnfoldIndices[] := UnfoldASingleIndex /@ Union[Flatten[#[[2]]& /@ FilterRules[Flatten[#[[2]]& /@ Join[M$ClassesDescription, M$Parameters]], Indices]]];


(* ::Text:: *)
(*UnfoldASingleIndex[Index[name]] tests the Index[name] and if the IndexRange of this index has an Unfold tag, it applies IndexUnfold to the Index, and and then sets the new IndexRange, where the tag was removed.*)


UnfoldASingleIndex[index_Index] := If[Head[IndexRange[index]] === Unfold, 
                                      IndexUnfold[index]; 
                                      IndexRange[index] = Identity @@ IndexRange[index]
                                     ];


(* ::Section::Closed:: *)
(*DeclareQuantumNumbers*)


DeclareQuantumNumbers[tempclassname_, tempclassmembers_,tempQNrules_] := Block[{tempQN, tempQNvalue},
    tempQN = tempQNrules //. {Rule[x_, y_] -> x};
    tempQNvalue = tempQNrules //. {Rule[x_, y_] -> y};
    MR$QuantumNumbers = KillDoubles[Join[MR$QuantumNumbers, tempQN]];
    If[tempQN =!= {},
             Do[tempQN[[xx]][tempclassname] = tempQNvalue[[xx]];
                tempQN[[xx]][anti[tempclassname]] = -tempQNvalue[[xx]];
                tempQN[[xx]][tempclassname[___]] = tempQNvalue[[xx]];
                tempQN[[xx]][anti[tempclassname][___]] = -tempQNvalue[[xx]];
                Do[tempQN[[xx]][tempclassmembers[[yy]]] = tempQNvalue[[xx]];
                   tempQN[[xx]][anti[tempclassmembers[[yy]]]] = -tempQNvalue[[xx]];
                   tempQN[[xx]][tempclassmembers[[yy]][___]] = tempQNvalue[[xx]];
                   tempQN[[xx]][anti[tempclassmembers[[yy]]][___]] = -tempQNvalue[[xx]], 
                   {yy, Length[tempclassmembers]}];
                QuantumNumberQ[tempQN[[xx]]] = True,
                {xx, Length[tempQN]}]]
];


(* ::Section:: *)
(*Class declarations (spin 3/2 added)*)


FR$TensCount=1; 

 FR$Declaration[repo_] := Block[{currentclass, tempclassname, tempclassmembers, tempflavind, tempoptions, tempmass, tempmassFA, tempmass2, tempmassvalue, tempwidth,
                                 tempwidth2, tempproptype, tempproplabel, tempMGpartname, tempMGantipartname, tempMGgauge, temppdg, nclassmembers,
                                 tempMGoptions, fieldtype, tempMGparamtype, tempMGblockname, tempMGparamname, tempMGintorder, 
                                 tempMGvalue, tempMGorderblock, tempMGcomplexparam, tempdim1, temparray, currentrules, currentelem,
                                 tempcomplexparam, tempTeX, tempcomp, tempnotens, tempdef, tempdef2, tempdef3, tempdeflhs, tempdefrhs, MyRuleDelayed, tempunphys, tempQNs,
                                 tempQNrules, tempQNvalue, tempcc, tempsym, tempantisym, tempsymsp2, tempantisymsp2, tempind, trinv1, casi,
                                 tempabelian1, tempboson1, tempSF,tempbosonSF,tempcoup1, tempcharge1, tempreps1, tempadj1, tempstruc1, tempDterm1, tempfullname,
                                 tempind1, tempherm, tempunitary, tempdescription, tempopt, tempTeXpartname, tempTeXantipartname, tempaddprop, tempGoldstone,
                                 tempghost, tempselfconjugate, MyModule,(* MW edit *) tempcharges, tempvevs, tempspinindex, indexname, tempweylcomp, modulelist, tempdef20},
  
If[Not[repo], Print["   - Loading particle classes."]];

    Do[currentclass = MR$ClassesList[[kk]];

       tempclassname = ClassName /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       TeXCN[tempclassname] = TeXClassName /. MR$ClassesRules[currentclass] /. TeXClassName -> ToString[tempclassname];
       (* 25. 03. 2010: Added ClassMemberQ *)
       ClassMemberQ[tempclassname] = False;
       (* End Added ClassMemberQ *)

(*                   *)
(* Unphysical        *)
(*                   *)

       tempunphys = Unphysical /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       (* At least ina first stage, Weyl fermions are always considered unphysical in the sense that there is no MC for them *)
       If[(currentclass === W[_]) || (currentclass === RW[_]), 
          tempunphys = True
         ];
       UnphysicalQ[tempclassname] = tempunphys;


       tempselfconjugate = SelfConjugate /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       If[tempselfconjugate === MR$Null, Message[LoadModel::SelfConjugate], SelfConjugateQ[tempclassname] = tempselfconjugate];


       If[tempclassname =!= MR$Null, MR$ClassNameList = Append[MR$ClassNameList, tempclassname];

       If[Not[tempunphys] && (Head[currentclass] =!= W) && (Head[currentclass] =!= RW), 
             FA$ClassesDescription[currentclass] = DeleteCases[MR$ClassesRules[currentclass], Rule[_?(Not[MemberQ[FA$FAOptions, #]]&), _]];
             FA$ClassesList = Append[FA$ClassesList, currentclass]];
          MR$Class[tempclassname] = currentclass;
          FA$ClassToName = Append[FA$ClassToName, currentclass -> tempclassname]];
       If[FreeQ[FA$ClassesDescription[currentclass], PropagatorLabel], FA$ClassesDescription[currentclass] = Append[FA$ClassesDescription[currentclass], Rule[PropagatorLabel, tempclassname]]];
       If[FreeQ[FA$ClassesDescription[currentclass], PropagatorType], 
          tempaddprop = currentclass /. {S[_] -> ScalarDash, F[_] -> Straight, V[_] -> Sine};
          FA$ClassesDescription[currentclass] = Append[FA$ClassesDescription[currentclass], Rule[PropagatorType, tempaddprop]]];
       If[FreeQ[FA$ClassesDescription[currentclass], PropagatorArrow],
          If[(SelfConjugate /. MR$ClassesRules[currentclass]) === False, 
             FA$ClassesDescription[currentclass] = Append[FA$ClassesDescription[currentclass], Rule[PropagatorArrow, Forward]],
             FA$ClassesDescription[currentclass] = Append[FA$ClassesDescription[currentclass], Rule[PropagatorArrow, None]]]];


(*                   *)
(* Class members     *)
(*                   *)
       tempclassmembers = ClassMembers /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       (* 25. 03. 2010: Added ClassMemberQ *)
       If[tempclassmembers =!= {},
          Set[ClassMemberQ[#], True]& /@ tempclassmembers
         ];
       (* End Added ClassMemberQ *)
       FR$CheckFlavExp = Join[FR$CheckFlavExp, tempclassmembers];
       If[(tempclassmembers === {}) && (tempclassname =!= MR$Null), tempclassmembers = {tempclassname}];
       Do[SelfConjugateQ[tempclassmembers[[clm]]] = tempselfconjugate;
          UnphysicalQ[tempclassmembers[[clm]]] = tempunphys, {clm, Length[tempclassmembers]}];
       If[Not[tempunphys] && (Head[currentclass] =!= W) && (Head[currentclass] =!= RW),
          FA$MemberToClass = Join[FA$MemberToClass, (Rule[#, currentclass]&) /@ tempclassmembers];
         ];
       If[tempclassname =!= MR$Null, MR$ClassMembers[tempclassname] = tempclassmembers];
       tempflavind = FlavorIndex /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       If[tempflavind =!= MR$Null,
          FA$FlavorNumber[currentclass] = Length[MRIndexRange[Index[tempflavind]]], 
          FA$FlavorNumber[currentclass] = 1];



(*                   *)
(* Chirality         *)
(*                   *)   
 
      If[MatchQ[currentclass, F[_]|W[_]|R[_]|RW[_]],
         Chirality[tempclassname] = Chirality /. MR$ClassesRules[currentclass] /. Options[LoadModel];
         If[MatchQ[currentclass, W[_]|RW[_]], Chirality[tempclassname] = Chirality[tempclassname] /. Automatic -> Left]];
       


(*                   *)
(* MajoranaPhase     *)
(*                   *)
       MajoranaPhase[tempclassname] = MajoranaPhase /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       Do[MajoranaPhase[tempclassmembers[[cm]]] = MajoranaPhase[tempclassname], {cm, Length[tempclassmembers]}];


(*                   *)
(* Mass              *)
(*                   *)
       tempmass = Mass /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       Which[NumericQ[tempmass] || (tempmass === Internal),
                 tempmass = If[Length[tempclassmembers] == 1, {ToExpression[StringJoin["M", ToString[tempclassname]]], tempmass},
                               Prepend[Table[{ToExpression[StringJoin["M", ToString[tempclassname], ToString[cm]]], tempmass}, {cm, Length[tempclassmembers]}], {ToExpression[StringJoin["M", ToString[tempclassname]]], tempmass}]],
             (Head[tempmass] === Symbol) && (tempmass =!= ZERO) && (tempmass =!= MR$Null), 
                 numQ[tempmass] = True;
                 CnumQ[tempmass] = False,
             VectorQ[tempmass] && NumericQ[tempmass[[2]]] && (Length[tempmass] == 2),
                 numQ[tempmass[[1]]] = True;
                 CnumQ[tempmass[[1]]] = False,
             MatchQ[tempmass, {_, Internal}],
                 numQ[tempmass[[1]]] = True;
                 CnumQ[tempmass[[1]]] = False,
             Head[tempmass] === List,
                 tempmass2 = KillMassValue /@ tempmass;
                 Which[Length[tempmass2] == Length[tempclassmembers],
                           Do[numQ[tempmass[[rr]]] = True;
                              CnumQ[tempmass[[rr]]] = False,
                              {rr, Length[tempclassmembers]}],
                       Length[tempmass2] == (Length[tempclassmembers] + 1),
(*Modif, CD, 26.02.2010: We want the masses to have values at this point*)
                           tempmassvalue = If[Head[#] === List, #[[2]], #] & /@ Rest[tempmass];
                           numQ[tempmass2[[1]]] = True;
                           CnumQ[tempmass2[[1]]] = False;
                           numQ[tempmass2[[1]][__]] := True;
                           CnumQ[tempmass2[[1]][__]] := False;
                           NoTensQ[tempmass2[[1]]] = True;
                           If[tempflavind =!= MR$Null, $IndList[tempmass2[[1]]] = {Index[tempflavind]}];
                           Do[numQ[tempmass2[[rr]]] = True;
                              CnumQ[tempmass2[[rr]]] = False;
                              If[tempflavind =!= MR$Null,
                                 tempmass2[[1]][NTIndex[tempflavind, rr-1]] = tempmass2[[rr]];
                                 NumericalValue[tempmass2[[1]][Index[tempflavind, rr-1]]] = tempmassvalue[[rr-1]];
                                 NumericalValue[tempmass2[[1]][rr-1]] = tempmassvalue[[rr-1]];
                                 tempmass2[[1]][rr-1] = tempmass2[[rr]];
                                 tempmass2[[1]][Index[tempflavind, rr-1]] = tempmass2[[rr]];],
                              {rr, 2, Length[tempclassmembers] + 1}],
                       True,
                           Message[LoadModel::Mass];Abort[]],
               tempmass === MR$Null,
                   tempmass = If[Length[tempclassmembers] == 1, {ToExpression[StringJoin["M", ToString[tempclassname]]], NoValue[1]},
                               Prepend[Table[{ToExpression[StringJoin["M", ToString[tempclassname], ToString[cm]]], NoValue[1]}, {cm, Length[tempclassmembers]}], {ToExpression[StringJoin["M", ToString[tempclassname]]], NoValue[1]}]]];
       tempmassFA = tempmass;
       tempmassFA = tempmassFA //. {_, 0} -> 0//. {{x_, _?NumericQ} -> x, {x_, Internal} -> x, {x_, NoValue[1]} -> x};
       If[tempmassFA =!= MR$Null,
          Which[tempmassFA === 0,
                      FA$Mass[currentclass] = 0;
                      FA$ClassesDescription[currentclass] = Append[DeleteCases[FA$ClassesDescription[currentclass], Rule[Mass, _]], Rule[Mass, FA$Mass[currentclass]]],
                tempmassFA === ZERO,
                      FA$Mass[currentclass] = 0;
                      FA$ClassesDescription[currentclass] = Append[DeleteCases[FA$ClassesDescription[currentclass], Rule[Mass, _]], Rule[Mass, FA$Mass[currentclass]]],
                Head[tempmassFA] === Symbol,
                      FA$Mass[currentclass] = tempmassFA;
                      FA$ClassesDescription[currentclass] = Append[DeleteCases[FA$ClassesDescription[currentclass], Rule[Mass, _]], Rule[Mass, FA$Mass[currentclass]]],
                NumericQ[tempmassFA],
                      FA$Mass[currentclass] = ToExpression[StringJoin["Mass", ToString[tempclassname]]];
                      FA$ClassesDescription[currentclass] = Append[DeleteCases[FA$ClassesDescription[currentclass], Rule[Mass, _]], Rule[Mass, FA$Mass[currentclass]]],
                (Head[tempmassFA] === List) && (Length[tempmassFA] == 1),
                      FA$Mass[currentclass] = tempmassFA[[1]];
                      FA$ClassesDescription[currentclass] = Append[DeleteCases[FA$ClassesDescription[currentclass], Rule[Mass, _]], Rule[Mass, FA$Mass[currentclass]]],
                (Length[tempmassFA] == FA$FlavorNumber[currentclass]) && (FA$FlavorNumber[currentclass] != 1),
                      FA$Mass[currentclass] = ToExpression[StringJoin["Mass", ToString[tempclassname]]];
                      FA$ClassesDescription[currentclass] = Append[DeleteCases[FA$ClassesDescription[currentclass], Rule[Mass, _]], Rule[Mass, FA$Mass[currentclass]]],
                (Length[tempmassFA] == (FA$FlavorNumber[currentclass] + 1)) && (FA$FlavorNumber[currentclass] != 1), 
                      FA$Mass[currentclass] = tempmassFA[[1]];
                      FA$ClassesDescription[currentclass] = Append[DeleteCases[FA$ClassesDescription[currentclass], Rule[Mass, _]], Rule[Mass, FA$Mass[currentclass]]];
                      FA$MassMembers[currentclass] = Rest[tempmassFA],
                True,
                      Message[LoadModel::FAMass];
                      FA$ClassesDescription[currentclass] = DeleteCases[FA$ClassesDescription[currentclass], Rule[Mass, _]]]];
       If[(Length[tempmass] == (Length[tempclassmembers] + 1)) && Not[Length[tempclassmembers] == 1], tempmass = Rest[tempmass]];

(*                   *)
(* Width             *)
(*                   *)
       tempwidth = Width /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       Which[NumericQ[tempwidth] || (tempwidth === Internal),
                 tempwidth = If[Length[tempclassmembers] == 1, {ToExpression[StringJoin["W", ToString[tempclassname]]], tempwidth},
                               Prepend[Table[{ToExpression[StringJoin["W", ToString[tempclassname], ToString[cm]]], tempwidth}, {cm, Length[tempclassmembers]}], {ToExpression[StringJoin["W", ToString[tempclassname]]], tempwidth}]],
            (Head[tempwidth] === Symbol) && (tempwidth =!= ZERO) && (tempwidth =!= MR$Null), 
                 numQ[tempwidth] = True;
                 CnumQ[tempwidth] = False,
             VectorQ[tempwidth] && (NumericQ[tempwidth[[2]]] || (tempwidth[[2]] === Internal)) && (Length[tempwidth] == 2),
                 numQ[tempwidth[[1]]] = True;
                 CnumQ[tempwidth[[1]]] = False,
             Head[tempwidth] === List,
                 tempwidth2 = KillMassValue /@ tempwidth;
                 Which[Length[tempwidth2] == Length[tempclassmembers],
                           Do[numQ[tempwidth[[rr]]] = True;
                              CnumQ[tempwidth[[rr]]] = False,
                              {rr, Length[tempclassmembers]}],
                       Length[tempwidth2] == (Length[tempclassmembers] + 1),
                           numQ[tempwidth2[[1]]] = True;
                           CnumQ[tempwidth2[[1]]] = False;
                           numQ[tempwidth2[[1]][__]] := True;
                           CnumQ[tempwidth2[[1]][__]] := False;
                           NoTensQ[tempwidth2[[1]]] = True;
                           If[tempflavind =!= MR$Null, $IndList[tempwidth2[[1]]] = {Index[tempflavind]}];
                           Do[numQ[tempwidth2[[rr]]] = True;
                              CnumQ[tempwidth2[[rr]]] = False;
                              If[tempflavind =!= MR$Null,
                                 tempwidth2[[1]][NTIndex[tempflavind, rr-1]] = tempwidth2[[rr]]],
                              {rr, 2, Length[tempclassmembers] + 1}],
                       True,
                           Message[LoadModel::Width];Abort[]],
              tempwidth === MR$Null,
                   tempwidth = If[Length[tempclassmembers] == 1, {ToExpression[StringJoin["W", ToString[tempclassname]]], NoValue[1]},
                               Prepend[Table[{ToExpression[StringJoin["W", ToString[tempclassname], ToString[cm]]], NoValue[1]}, {cm, Length[tempclassmembers]}], {ToExpression[StringJoin["W", ToString[tempclassname]]], NoValue[1]}]]];
       If[Length[tempwidth] == (Length[tempclassmembers] + 1) && Not[Length[tempclassmembers] == 1], tempwidth = Rest[tempwidth]];


(*                   *)
(* Symmetries        *)
(*                   *)
       tempsym = SymmetricIndices /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       tempsym = Flatten[{tempsym}];
       tempantisym = AntiSymmetricIndices /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       tempantisym = Flatten[{tempantisym}];
       tempsymsp2 = Symmetric /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       tempantisymsp2 = AntiSymmetric /. MR$ClassesRules[currentclass] /. Options[LoadModel];


(*                   *)
(* Indices           *)
(*                   *)
       tempind = Indices /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       If[tempind =!= MR$Null, MR$IndexList = KillDoubles[Join[MR$IndexList, tempind]]];



(*                   *)
(* Declare           *)
(*                   *)
       tempoptions = DeleteCases[{ClassName -> tempclassname, FlavorIndex -> tempflavind, SymmetricIndices -> tempsym, 
               AntiSymmetricIndices -> tempantisym, Symmetric -> tempsymsp2, AntiSymmetric -> tempantisymsp2}, _ -> MR$Null];
       tempoptions = DeleteCases[{ClassName -> tempclassname, FlavorIndex -> tempflavind}, _ -> MR$Null];
       If[tempclassmembers =!= {}, 
          AddParticlesToClass[tempclassmembers, currentclass, Sequence @@ tempoptions];
          If[repo, If[tempclassname =!= MR$Null,
                          Print["Class ", currentclass, " = ", tempclassname, " loaded. (",tempclassmembers, " )"],
                          Print["Class ", currentclass, " loaded. (",tempclassmembers, " )"]]]];
       tempdef = Definitions /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       tempdef = Flatten[{tempdef}];
       If[tempdef =!= {MR$Null}, 
           MR$Definitions = DeclareNewDefinition[tempdef, MR$Definitions];
         ]; 



(*                   *)
(*QuantumNumbers     *)
(*                   *)
       If[tempclassname =!= MR$Null,
          tempQNrules = QuantumNumbers /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          If[(SelfConjugate /. MR$ClassesRules[currentclass]) && tempQNrules =!= {}, Message[LoadModel::QN]];
             DeclareQuantumNumbers[tempclassname, tempclassmembers, tempQNrules]
          ];






(* V1 .5.18, CD: Adding VeVs option, in the perspective to have automatic generation of gauge fixing and ghost Lagrangians. *)

       tempvevs = VeVs /. MR$ClassesRules[currentclass] /. Options[LoadModel];

       If[(Head[currentclass] =!= S) && (tempvevs =!= 0), Message[vevs::noscalar]];
          VeVs[currentclass] = tempvevs;
          VeVs[tempclassname] = tempvevs;
               




(* MW edit: declare charges, i.e. eigenvalues for DiagProd *)
       If[tempclassname =!= MR$Null,
         tempcharges = U1Charges /. MR$ClassesRules[currentclass] /. Options[LoadModel];
         DeclareU1Charges[tempclassname, tempcharges];
       ];
(*Ghosts *)
          tempghost = Ghost /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          If[tempghost =!= NoGhost, 
                GhToGB[tempclassname] = tempghost;
                GBToGh[tempghost] = tempclassname];
        (*                                                               *)
        (*                   FA Declarations                             *)
        (*                                                               *)
        If[Not[tempunphys], 
             FA$ClassesDescription[currentclass] = FA$ClassesDescription[currentclass] /. Rule[QuantumNumbers, {xx___}] :> Rule[QuantumNumbers, (Times @@ # &) /@ {xx}];
             FA$ClassesDescription[currentclass] = FA$ClassesDescription[currentclass] /. Rule[PropagatorLabel, {xx_, ___}] :> Rule[PropagatorLabel, xx];
             FA$ClassesDescription[currentclass] = FA$ClassesDescription[currentclass] //. {Rule[PropagatorType, S] -> Rule[PropagatorType, Straight],
                    Rule[PropagatorType, W] -> Rule[PropagatorType, Sine],
                    Rule[PropagatorType, C] -> Rule[PropagatorType, Cycles],
                    Rule[PropagatorType, D] -> Rule[PropagatorType, ScalarDash]};
             If[FreeQ[FA$ClassesDescription[currentclass], Indices], FA$ClassesDescription[currentclass] = Append[FA$ClassesDescription[currentclass], Rule[Indices, {}]]]];
        If[tempflavind =!= MR$Null,
           tempcc = currentclass /. f_[_] -> f;
           Which[(tempcc === F) || (tempcc === V) || (tempcc === W) || (tempcc === RW) || (tempcc === R), 
                       FA$FlavPos[currentclass] = (ToExpression @@ Flatten[Position[$IndList[tempclassname], Index[tempflavind]]]) - 1,
                 tempcc === S, 
                       FA$FlavPos[currentclass] = (ToExpression @@ Flatten[Position[$IndList[tempclassname], Index[tempflavind]]]),
                 tempcc === T, 
                       FA$FlavPos[currentclass] = (ToExpression @@ Flatten[Position[$IndList[tempclassname], Index[tempflavind]]]) - 2]];
       tempproplabel = PropagatorLabel /. MR$ClassesRules[currentclass] /. Options[LoadModel];
       tempproplabel = Flatten[{tempproplabel}];
       tempproplabel = Which[tempproplabel === {MR$Null} && (Length[tempclassmembers] == 1), {ToString[tempclassname]}, 
                             tempproplabel === {MR$Null} && (Length[tempclassmembers] != 1), Join[{ToString[tempclassname]}, ToString /@ tempclassmembers],
                             (Length[tempproplabel] == 1) && (Length[tempclassmembers] != 1), Join[tempproplabel, ToString /@ tempclassmembers],
                             (Length[tempproplabel] == Length[tempclassmembers]) && (Length[tempproplabel] != 1), Prepend[tempproplabel, ToString[tempclassname]],
                             Length[tempproplabel] == (Length[tempclassmembers] + 1), tempproplabel,
                             True, tempproplabel];
       If[VectorQ[tempproplabel] && (Length[tempproplabel] > 1),
          FA$PropLabelMembers[currentclass] = Rest[tempproplabel]];
       (*                                                                           *)
       (* From here on we have MG declarations. This does not have any effect on MR *)
       (*                                                                           *)
       If[Not[tempunphys],
          nclassmembers = Length[tempclassmembers];
          tempmass = tempmass //. {xx___, 0, yy___} -> {xx, ZERO, yy};
          If[Length[tempclassmembers] == 1, tempmass = tempmass //. {_, ZERO} -> ZERO];
          tempwidth = tempwidth //. {xx___, 0, yy___} -> {xx, ZERO, yy};
          tempproptype = PropagatorType /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          If[Length[tempproplabel] == (Length[tempclassmembers] + 1), tempproplabel = Rest[tempproplabel]];
          tempMGpartname = ParticleName /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          tempMGpartname = Flatten[{tempMGpartname}];
          tempMGantipartname = AntiParticleName /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          tempMGantipartname = Flatten[{tempMGantipartname}];
          tempTeXpartname = TeXParticleName /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          tempTeXpartname = Flatten[{tempTeXpartname}];
          tempTeXantipartname = TeXAntiParticleName /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          tempTeXantipartname = Flatten[{tempTeXantipartname}];
          tempMGgauge = GaugeIndex /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          Which[MemberQ[$IndList[tempclassname], Index[Colour]], tempMGgauge = Colour,
                MemberQ[$IndList[tempclassname], Index[Gluon]], tempMGgauge = Gluon,
                MemberQ[$IndList[tempclassname], Index[Sextet]], tempMGgauge = Sextet];
          temppdg = PDG /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          temppdg = Flatten[{temppdg}];


          (*                                                          *)
          (* This has to be changed if new particle classes are added *)
          (*                                                          *)
          fieldtype = currentclass /. {f_[_] -> f};
          fieldtype = Which[(fieldtype === S),
                                  If[SelfConjugate /. MR$ClassesRules[currentclass], RS, CS],
                            (fieldtype === V), 
                                  If[SelfConjugate /. MR$ClassesRules[currentclass], RV, CV],
                            (fieldtype === F),
                                  If[SelfConjugate /. MR$ClassesRules[currentclass], M, F],
							(fieldtype === R),
								 If[SelfConjugate /. MR$ClassesRules[currentclass], RR, CR],
                            True, fieldtype];


          If[(temppdg === {NoPDG}), temppdg = Table[NoPDG[++$PDGNb], {Length[tempclassmembers]}]];


          tempMGgauge = Which[tempMGgauge === S, S,
                              Length[MRIndexRange[Index[tempMGgauge]]] == 3, T,
                              Length[MRIndexRange[Index[tempMGgauge]]] == 6, S6,
                              Length[MRIndexRange[Index[tempMGgauge]]] == 8, O,
                              True, Message[LoadModel::Gauge];Abort[]];


          tempMGantipartname = Which[tempMGantipartname === MR$Null, StringJoin[ToString[#], "~"]& /@ tempclassmembers,
                                     Length[tempMGantipartname] === 1, Table[Sequence @@ tempMGantipartname ,{nclassmembers}],
                                     Length[tempMGantipartname] === nclassmembers, tempMGantipartname,
                                     True, Message[LoadModel::PartName];Abort[]];         
          tempMGpartname = Which[tempMGpartname === {MR$Null}, ToString /@ tempclassmembers,
                                 Length[tempMGpartname] === 1, Table[Sequence @@ tempMGpartname ,{nclassmembers}],
                                 Length[tempMGpartname] === nclassmembers, tempMGpartname,
                                 True, Message[LoadModel::PartName];Abort[]];

          If[tempTeXpartname === {MR$Null}, tempTeXpartname = tempMGpartname];
          If[tempTeXantipartname === {MR$Null}, tempTeXantipartname = tempMGantipartname];

          tempproptype = Which[tempproptype === Straight, S,
                               tempproptype === ScalarDash, D,
                               tempproptype === Sine, W,
                               tempproptype === Cycles, C,
                               True, tempproptype];



          tempwidth = Which[tempwidth === MR$Null, Table[MR$Null, {nclassmembers}],
                            tempwidth === 0, Table[ZERO, {nclassmembers}],
                            tempwidth === ZERO, Table[ZERO, {nclassmembers}],
                            Head[tempwidth] === Symbol, Table[{tempwidth, NoValue[1]}, {mk, nclassmembers}],
                            VectorQ[tempwidth] && (Length[tempwidth] == 2) && (Head[tempwidth[[1]]] === Symbol) && (NumericQ[tempwidth[[2]]] || tempwidth[[2]] === Internal || tempwidth[[2]] === NoValue[1] || tempwidth[[2]] === ZERO), Table[tempwidth, {mk, nclassmembers}],
                            MatrixQ[tempwidth], tempwidth,
                            Head[tempwidth] === List, Table[Flatten[{tempwidth[[ll]]}], {ll, nclassmembers}] /. {x_} -> {x,NoValue[1]},
                            True, Message[LoadModel::Mass];Abort[]];
          tempwidth = MakeZERO /@ tempwidth;
          tempwidth = tempwidth /. MakeZERO -> Identity;


          tempmass = Which[NumericQ[tempmass] || (tempmass === Internal), Table[tempmass, {Length[tempclassmembers]}],
                           tempmass === MR$Null, Table[MR$Null, {nclassmembers}],
                           tempmass === 0, Table[ZERO, {nclassmembers}],
                           tempmass === ZERO, Table[ZERO, {nclassmembers}],
                           Head[tempmass] === Symbol, Table[{tempmass, NoValue[1]}, {mk, nclassmembers}],
                           VectorQ[tempmass] && (Length[tempmass] == 2) && (Head[tempmass[[1]]] === Symbol) && (NumericQ[tempmass[[2]]] || tempmass[[2]] === Internal || tempmass[[2]] === NoValue[1]), Table[tempmass, {mk, nclassmembers}],
                           MatrixQ[tempmass], tempmass,
                           Head[tempmass] === List, Table[Flatten[{tempmass[[ll]]}], {ll, nclassmembers}] /. {x_} -> {x, NoValue[1]},
                           True, Message[LoadModel::Mass];Abort[]];
          tempmass = MakeZERO /@ tempmass;
          tempmass = tempmass /. MakeZERO -> Identity;


          tempfullname = FullName /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          tempfullname = Flatten[{tempfullname}];
          tempfullname = Which[
                tempfullname === {MR$Null}, ToString /@ tempclassmembers,
                Length[tempfullname] != Length[tempclassmembers], Table[ToString[tempfullname[[1]]], {Length[tempclassmembers]}],
                True, tempfullname];

          tempGoldstone = Goldstone /. MR$ClassesRules[currentclass] /. Options[LoadModel];
          If[(tempGoldstone =!= NoGS) && (fieldtype =!= RS) && (fieldtype =!= CS), Message[Goldstone::Scalar]];


          tempMGoptions = Table[DeleteCases[{Mass -> tempmass[[ll]], Width -> tempwidth[[ll]], LineType -> tempproptype, LineName -> tempproplabel[[ll]], PartName -> tempMGpartname[[ll]], AntiPartName -> tempMGantipartname[[ll]], MRClassName -> {currentclass, tempclassname}, FullName -> tempfullname[[ll]], TeXParticleName -> tempTeXpartname[[ll]], TeXAntiParticleName -> tempTeXantipartname[[ll]], Goldstone -> tempGoldstone}, _ -> MR$Null], {ll, nclassmembers}];
      Do[Particle[tempclassmembers[[ll]], fieldtype, tempMGgauge, temppdg[[ll]], Sequence @@ tempMGoptions[[ll]]], {ll, nclassmembers}]];

(*                           *)
(* Chirality of antifermions *)
(*                           *)

      If[MatchQ[currentclass, W[_]|RW[_]],
         Chirality[anti[tempclassname]] = Chirality[tempclassname] /. {Left -> Right, Right -> Left}];



(*                   *)
(* WeylComponents    *)
(*                   *)
       If[MatchQ[currentclass, F[_]|R[_]],
         tempweylcomp = WeylComponents /. MR$ClassesRules[currentclass] /. Options[LoadModel];
         tempweylcomp = Flatten[{tempweylcomp}];
         If[Length[tempweylcomp] == 1, 
            AppendTo[tempweylcomp, anti[tempweylcomp[[1]]]];
            If[Not[tempselfconjugate], Message[Weyl::Chirality]]];
         WeylComponents[tempclassname] = tempweylcomp;
         (To4Components[#]=tempclassname)&/@tempweylcomp;
         (To4Components[anti[#]]=tempclassname)&/@tempweylcomp;
         If[tempweylcomp =!= {}, AppendTo[FR$MakeWeylToDirac, tempclassname]]],

      {kk, Length[MR$ClassesList]}];

	(*NC : Feb 4, 2015: Added dispatch here because it was causing problems in Math10 in AddParticlesToClass where it used to be.*)
	$FAToMRRules = Dispatch[$FAToMRRules];
	(*NC : End*)

      DeclareWeylFermions[];

If[repo,  Print["(* * * * * * * * * * * * * * * * *)"]];




    (*                                                                           *)
    (*                              Gauge Group declaration                      *)
    (*                                                                           *)
    If[ValueQ[M$GaugeGroups] && (Length[M$GaugeGroups] != 0),
       
      If[Not[repo], Print["   - Loading gauge group classes."]];

       Do[currentclass = MR$GaugeGroupList[[kk]];
          tempabelian1 = Abelian /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
(* BF: Superfield case: if no 'Gaugeboson' but 'Superfield' -> take GaugeBoson from the superfield .*)
          tempboson1 = GaugeBoson /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
          tempSF = Superfield /.MR$GaugeGroupRules[currentclass]/.Superfield->MR$Null;
          If[tempboson1===MR$Null && tempSF=!=MR$Null, 
             MR$GaugeGroupRules[currentclass]=Append[MR$GaugeGroupRules[currentclass],Rule[GaugeBoson,SF2Boson[tempSF]]];
             M$GaugeGroups[[Position[M$GaugeGroups,Equal[currentclass,__]][[1,1]]]][[2]]=
               Append[M$GaugeGroups[[Position[M$GaugeGroups,Equal[currentclass,__]][[1,1]]]][[2]],Rule[GaugeBoson,SF2Boson[tempSF]]];
             tempboson1=GaugeBoson/.Cases[M$Superfields,Equal[VSF[_],List[___,ClassName->tempSF,___]]][[1,2]]
          ];
(* END BF *)
(* V1 .5.18, CD: Adding VeVs and GaugeXi option, in the perspective to have automatic generation of gauge fixing and ghost Lagrangians. *)

          tempvevs = VeVs /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
          VeVs[currentclass] = tempvevs;
          VeVs[tempboson1] = tempvevs;

          tempxi = GaugeXi /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
          If[tempxi =!= MR$Null, GaugeXi[currentclass] = tempxi; GaugeXi[tempboson1] = tempxi];
          

          tempcoup1 = CouplingConstant /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
          Which[tempabelian1 === True, 
                   tempcharge1 = Charge /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
                   If[Length[DeleteCases[{tempboson1, tempcoup1, tempcharge1}, MR$Null]] != 3,
                      Message[Gauge::Abelian];Abort[]];
                   AbelianGaugeGroup[currentclass, tempboson1, tempcharge1, tempcoup1, repo],
                tempabelian1 === False,
                   tempreps1 = Representations /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
                   tempstruc1 = StructureConstant /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
                   tempDterm1 = SymmetricTensor /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
                   tempadj1 = AdjointIndex /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
                   trinv1 = Dynkin /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
                   casi = Casimir /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
                   If[Length[DeleteCases[{tempboson1, tempcoup1, tempstruc1}, MR$Null]] != 3,
                      Message[Gauge::NonAbelian];Abort[]];
                   If[tempadj1 =!= MR$Null, 
                      NonAbelianGaugeGroup[currentclass, tempboson1, tempreps1, tempcoup1, tempstruc1, repo, AdjointIndex -> tempadj1, SymmetricTensor -> tempDterm1, Dynkin -> trinv1, Casimir->casi],
                      NonAbelianGaugeGroup[currentclass, tempboson1, tempreps1, tempcoup1, tempstruc1, repo,  SymmetricTensor -> tempDterm1, Dynkin ->trinv1, Casimir-> casi]],
                True, 
                   Message[Gauge::Invalid];Abort[]];
         tempdef = Definitions /. MR$GaugeGroupRules[currentclass] /. Options[LoadModel];
         tempdef = Flatten[{tempdef}];
         If[tempdef =!= {MR$Null}, MR$Definitions = Join[MR$Definitions, tempdef]],         
         {kk, Length[MR$GaugeGroupList]}]];
    (*                                                                           *)
    (*                              Parameter declaration                        *)
    (*                                                                           *)     
      If[ValueQ[M$Parameters] && (Length[M$Parameters] != 0),
         If[Not[repo], Print["   - Loading parameter classes."]];
         Do[currentclass = MR$ParameterList[[kk]];
            If[repo, Print["Loading parameter ", currentclass]];
            tempind1 = Indices /. MR$ParameterRules[currentclass] /. Options[LoadModel];
            tempind1 = Flatten[{tempind1}];

            If[(tempind1 === {MR$Null}) || (tempind1 ==={}),
         (*                                                   *)
         (*                Non tensor Parameters              *)
         (*                                                   *)
               numQ[currentclass] = True;
               CnumQ[currentclass] = ComplexParameter /. MR$ParameterRules[currentclass] /. Options[LoadModel];
               AppendTo[FR$OptimizeParams, currentclass];
               tempTeX = TeX /. MR$ParameterRules[currentclass] /. Options[LoadModel];
               If[Not[tempTeX === MR$Null], TeXFormat[currentclass, tempTeX]];
               tempdef = Definitions /. MR$ParameterRules[currentclass] /. Options[LoadModel];
               tempdef = Flatten[{tempdef}];
               If[tempdef =!= {MR$Null}, 
                  MR$Definitions = Join[MR$Definitions, tempdef]];
(* CD, 26.03.2012: Need to make sure the value is defined, even if the parameter is removed by a Definition*)
             tempMGvalue = Value /. MR$ParameterRules[currentclass];
             tempMGparamname = ParameterName /. MR$ParameterRules[currentclass] /. Options[LoadModel]; 

             tempMGvalue = If[tempdef =!= {MR$Null},
                              currentclass /. tempdef,
                              tempMGvalue] /. Options[LoadModel];

(* CD, 26.02.2010: Added Expandion over flover of possible contracted indices *)
             If[Not[FreeQ[tempMGvalue, Index]], tempMGvalue = PerformIndexSum[Expand[tempMGvalue]]];
             NumericalValue[currentclass] = NumericalValue[tempMGvalue];
             NumericalValue[tempMGparamname] = NumericalValue[tempMGvalue];
             (*                                                          *)
             (*                These are the MG declarations             *)
             (*                                                          *) 
             tempMGparamtype = ParameterType /. MR$ParameterRules[currentclass] /. Options[LoadModel];
             tempMGblockname = BlockName /. MR$ParameterRules[currentclass] /. Options[LoadModel];
             If[tempMGblockname == MR$Null, tempMGblockname = NoBlockName[FRBlock]];
 
             tempdescription = Description /. MR$ParameterRules[currentclass] /. Options[LoadModel];
             If[(Head[tempdescription] =!= String) && (tempdescription =!= MR$Null), tempdescription = ToString[tempdescription]];
             Which[tempdescription =!= MR$Null, tempdescription = tempdescription,
                   (tempdescription === MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = ToString[tempMGparamname],
                   (tempdescription === MR$Null) && (tempMGparamname === MR$Null), tempdescription = ToString[currentclass]];
             tempMGintorder = InteractionOrder /. MR$ParameterRules[currentclass] /. Options[LoadModel];
             tempMGorderblock = OrderBlock /. MR$ParameterRules[currentclass] /. Options[LoadModel];
             tempMGorderblock = Flatten[{tempMGorderblock}];
             If[tempMGorderblock =!= {MR$Null}, 
                If[Head[OrderBlock[tempMGblockname]] === List, OrderBlock[tempMGblockname] = Append[OrderBlock[tempMGblockname], {currentclass, tempMGorderblock}], OrderBlock[tempMGblockname] = {{currentclass, tempMGorderblock}}]];
             Which[(tempMGparamtype === External),
                         tempMGoptions = DeleteCases[{ParamName -> tempMGparamname, InteractionOrder -> tempMGintorder, Value -> tempMGvalue, MR$Complex -> CnumQ[currentclass], Description -> tempdescription}, _ -> MR$Null];
                         ExtParameter[currentclass, tempMGblockname, Sequence @@ tempMGoptions],
                   (tempMGparamtype === Internal),
                         tempMGoptions = DeleteCases[{ParamName -> tempMGparamname, InteractionOrder -> tempMGintorder, MR$Complex -> CnumQ[currentclass], Description -> tempdescription}, _ -> MR$Null];
                         IntParameter[currentclass, tempMGvalue, Sequence @@ tempMGoptions]],
         (*                                                   *)
         (*                    Tensor Parameters              *)
         (*                                                   *)
          (* No Tensor Parameters *)
          tempnotens = AllowSummation /. MR$ParameterRules[currentclass] /. Options[LoadModel];
          If[tempnotens,
             numQ[currentclass[__]] := True;
             If[ComplexParameter /. MR$ParameterRules[currentclass] /. Options[LoadModel],
                CnumQ[currentclass[__]] := True, CnumQ[currentclass[__]] :=  False];
             tempTeX = TeX /. MR$ParameterRules[currentclass] /. Options[LoadModel];
             If[Not[tempTeX === MR$Null], MakeTeXIndex[currentclass, tempTeX], MakeTeXIndex[currentclass, currentclass]];
             NoTensQ[currentclass] = True;
             If[tempind =!= MR$Null, MR$IndexList = KillDoubles[Join[MR$IndexList, tempind]]];
             $IndList[currentclass] = tempind1;
             tempdef = Definitions /. MR$ParameterRules[currentclass] /. Options[LoadModel];
             tempdef = Flatten[{tempdef}];
             If[tempdef =!= {MR$Null}, 
                tempdef2 = tempdef //.{f_?(NoTensQ[#] === True &)[ind__?(FreeQ[##, NTIndex]&)] :> NoTensPutInd[f, {ind}]};
                tempdef3 = tempdef2 //. {NTIndex -> Index};
                MR$Definitions = Join[MR$Definitions, tempdef, tempdef2, tempdef3]];
            (*                                                          *)
             (*                These are the MG declarations             *)
             (*                                                          *)
             tempdim1 = Length /@ (MRIndexRange /@ tempind1);
             temparray = Flatten[Array[currentclass, tempdim1]];
             FR$TensCount++;
             Do[currentelem = temparray[[jj]];
               AppendTo[FR$OptimizeParams, currentelem];
                If[(currentelem //. MR$Definitions) === currentelem,
                   currentrules = ParameterType /. MR$ParameterRules[currentclass] /. ParameterType -> {};
                   tempMGparamtype = ParameterType /. MR$ParameterRules[currentclass] /. ParameterType -> Internal;
                   Which[(tempMGparamtype === External),
                               tempMGblockname = BlockName /. MR$ParameterRules[currentclass] /. Options[LoadModel];
                               If[tempMGblockname == MR$Null, tempMGblockname = NoBlockName[ToExpression["FRBlock"<>ToString[FR$TensCount]]]];
                               currentrules = ParameterName /. MR$ParameterRules[currentclass] /. {ParameterName :> currentclass};
                               tempMGparamname = currentelem /. {currentelem :> ToExpression[StringJoin[ToString[currentrules], Sequence @@ (ToString /@ currentelem)]]};
                               tempdescription = Description /. MR$ParameterRules[currentclass] /. Options[LoadModel];
                               If[(Head[tempdescription] =!= String) && (tempdescription =!= MR$Null), tempdescription = ToString[tempdescription]];
                               Which[(tempdescription === MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = ToString[tempMGparamname],
                                     (tempdescription === MR$Null) && (tempMGparamname === MR$Null), tempdescription = ToString[currentclass],
                                     (tempdescription =!= MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = StringJoin[tempdescription, " ( ", ToString[tempMGparamname], ")"],
                                     (tempdescription =!= MR$Null) && (tempMGparamname === MR$Null), tempdescription = tempdescription];
                               currentrules = Value /. MR$ParameterRules[currentclass];
                               tempdef20 = If[tempdef === {MR$Null}, {}, tempdef];
                               currentrules = If[(currentelem /. tempdef20) =!= currentelem,
                                                 {currentelem ->( currentelem /. tempdef20)},
                                                 currentrules]/. Value -> MR$Null;
                               tempMGvalue = If[currentrules =!= MR$Null, currentelem /. currentrules, currentelem /. {currentelem -> NoValue[1]}];
                               If[tempMGvalue === currentelem, tempMGvalue = NoValue[1]];
                               NumericalValue[currentelem] = NumericalValue[tempMGvalue];
                               NumericalValue[tempMGparamname] = NumericalValue[tempMGvalue];
                               currentrules = InteractionOrder /. MR$ParameterRules[currentclass] /. {InteractionOrder -> {}};
                               currentrules = If[(Head[currentrules] === List) && (Length[currentrules] == 2),
                                                   {currentelem -> currentrules},
                                                   currentrules];
                               tempMGintorder = currentelem /. currentrules /. {currentelem -> MR$Null};
                               currentrules = OrderBlock /. MR$ParameterRules[currentclass] /. OrderBlock -> {};
                               tempMGorderblock = currentelem /. currentrules /. {currentelem :> List @@ currentelem};
                               tempMGorderblock = {currentelem, tempMGorderblock} /. {currentelem -> tempMGparamname};
                               If[Head[OrderBlock[tempMGblockname]] === List,
                                  OrderBlock[tempMGblockname] = Append[OrderBlock[tempMGblockname], tempMGorderblock],
                                  OrderBlock[tempMGblockname] = {tempMGorderblock}];
                               currentrules = ComplexParameter /. MR$ParameterRules[currentclass] /. ComplexParameter -> {};
                               currentrules = Which[currentrules === False, {currentelem -> False},
                                                    currentrules === True, currentrules = {currentelem -> True},
                                                    True, currentrules];
                               tempMGcomplexparam = currentelem /. currentrules /. {currentelem -> True};
                               tempopt = DeleteCases[{ParamName -> tempMGparamname, InteractionOrder -> tempMGintorder, Value -> tempMGvalue, MR$Complex -> tempMGcomplexparam, Description -> tempdescription}, _ -> MR$Null];
                               currentelem = currentelem //.{f_?(NoTensQ[#] === True &)[ind__?(FreeQ[##, NTIndex]&)] :> NoTensPutInd[f, {ind}]} //. {NTIndex -> Index};
                               ExtParameter[currentelem, tempMGblockname, Sequence @@ tempopt],
                          (tempMGparamtype === Internal),
                               currentrules = Value /. MR$ParameterRules[currentclass];
                               tempdef20 = If[tempdef === {MR$Null}, {}, tempdef];
                               currentrules = If[(currentelem /. tempdef20) =!= currentelem,
                                                 {currentelem ->( currentelem /. tempdef20)},
                                                 currentrules]/. Value -> MR$Null;
                               tempMGvalue = If[currentrules =!= MR$Null, currentelem /. currentrules, currentelem /. {currentelem -> NoValue[1]}];
                               If[tempMGvalue === currentelem, tempMGvalue = NoValue[1]];
(* CD, 26.02.2010: Added Expandion over flover of possible contracted indices *)
                               If[Not[FreeQ[tempMGvalue, Index]], tempMGvalue = PerformIndexSum[Expand[tempMGvalue]]];
                               currentrules = ParameterName /. MR$ParameterRules[currentclass] /. {ParameterName -> {}};
                               tempMGparamname = currentelem /. currentrules /. {currentelem :> ToExpression[StringJoin[ToString[currentclass], Sequence @@ (ToString /@ currentelem)]]};
                               NumericalValue[currentelem] = NumericalValue[tempMGvalue];
                               NumericalValue[tempMGparamname] = NumericalValue[tempMGvalue];
                               tempdescription = Description /. MR$ParameterRules[currentclass] /. Options[LoadModel];
                               If[(Head[tempdescription] =!= String) && (tempdescription =!= MR$Null), tempdescription = ToString[tempdescription]];
                               Which[(tempdescription === MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = ToString[tempMGparamname],
                                     (tempdescription === MR$Null) && (tempMGparamname === MR$Null), tempdescription = ToString[currentclass],
                                     (tempdescription =!= MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = StringJoin[tempdescription, " ( ", ToString[tempMGparamname], " )"],
                                     (tempdescription =!= MR$Null) && (tempMGparamname === MR$Null), tempdescription = tempdescription];
                               currentrules = InteractionOrder /. MR$ParameterRules[currentclass] /. {InteractionOrder -> {}};
                               currentrules = If[(Head[currentrules] === List) && (Length[currentrules] == 2),
                                                   {currentelem -> currentrules},
                                                   currentrules];
                               tempMGintorder = currentelem /. currentrules /. {currentelem -> MR$Null};
                               currentrules = ComplexParameter /. MR$ParameterRules[currentclass] /. {ComplexParameter -> {}};
                               currentrules = Which[currentrules === False, {currentelem -> False},
                                                    currentrules === True, currentrules = {currentelem -> True},
                                                    True, currentrules];
                               tempMGcomplexparam = currentelem /. currentrules /. {currentelem -> True};
                               tempopt = DeleteCases[{ParamName -> tempMGparamname, InteractionOrder -> tempMGintorder, MR$Complex -> tempMGcomplexparam, Description -> tempdescription}, _ -> MR$Null];
                               currentelem = currentelem //.{f_?(NoTensQ[#] === True &)[ind__?(FreeQ[##, NTIndex]&)] :> NoTensPutInd[f, {ind}]} //. {NTIndex -> Index};
                               IntParameter[currentelem, tempMGvalue, Sequence @@ tempopt]]],
               {jj, Length[temparray]}],
             (* Tensors *)
            tempclassname = TensorClass /. MR$ParameterRules[currentclass] /. Options[LoadModel];
            tempunitary = Unitary /. MR$ParameterRules[currentclass] /. Options[LoadModel];
            tempherm = Hermitian /. MR$ParameterRules[currentclass] /. Options[LoadModel];
            tempcomp = ComplexParameter /. MR$ParameterRules[currentclass] /. {ComplexParameter -> True};
            tempTeX = TeX /. MR$ParameterRules[currentclass] /. Options[LoadModel];
            If[tempunitary && tempherm, Message[Tensor::UnitHerm]];
            If[tempclassname =!= MR$Null,
               DeclareTensor[currentclass, tempind1, TensorClass -> tempclassname, Unitary -> tempunitary, Hermitian -> tempherm, ComplexParameter -> tempcomp, TeX -> tempTeX],
               DeclareTensor[currentclass, tempind1, Unitary -> tempunitary, Hermitian -> tempherm, ComplexParameter -> tempcomp, TeX -> tempTeX ]];
            tempdef = Definitions /. MR$ParameterRules[currentclass] /. Options[LoadModel];
            tempdef = Flatten[{tempdef}];
            If[tempdef =!= {MR$Null}, MR$Definitions = Join[MR$Definitions, tempdef]];
             (*                                                          *)
             (*                These are the MG declarations             *)
             (*                                                          *)
             tempdim1 = Length /@ (MRIndexRange /@ tempind1);
             temparray = Flatten[Array[currentclass, tempdim1]];
             FR$TensCount++;
             Do[currentelem = temparray[[jj]]; 
               AppendTo[FR$OptimizeParams, currentelem];
                If[(currentelem //. MR$Definitions) === currentelem,
                  (* currentrules = ParameterType /. MR$ParameterRules[currentclass] /. ParameterType -> {};
                   tempMGparamtype = currentelem /. currentrules /. {currentelem -> Internal};*)
                   tempMGparamtype = ParameterType /. MR$ParameterRules[currentclass] /. ParameterType -> Internal;
                   Which[(tempMGparamtype === External),
                               tempMGblockname = BlockName /. MR$ParameterRules[currentclass] /. Options[LoadModel];
                               If[tempMGblockname == MR$Null, tempMGblockname = NoBlockName[ToExpression["FRBlock"<>ToString[FR$TensCount]]]];
                               currentrules = ParameterName /. MR$ParameterRules[currentclass] /. {ParameterName :> currentclass};
                               tempMGparamname = currentelem /. {currentelem :> ToExpression[StringDrop[StringJoin[ToString[currentrules], Sequence @@ (StringJoin[ToString[#],"x"] &/@ currentelem)],-1]]};
                               tempdescription = Description /. MR$ParameterRules[currentclass] /. Options[LoadModel];
                               If[(Head[tempdescription] =!= String) && (tempdescription =!= MR$Null), tempdescription = ToString[tempdescription]];
                               Which[(tempdescription === MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = ToString[tempMGparamname],
                                     (tempdescription === MR$Null) && (tempMGparamname === MR$Null), tempdescription = ToString[currentclass],
                                     (tempdescription =!= MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = StringJoin[tempdescription, " ( ", ToString[tempMGparamname], " )"],
                                     (tempdescription =!= MR$Null) && (tempMGparamname === MR$Null), tempdescription = tempdescription];
                               tempdef20 = If[tempdef === {MR$Null}, {}, tempdef];
                               currentrules = If[(currentelem /. tempdef20) =!= currentelem,
                                                 {currentelem ->( currentelem /. tempdef20)},
                                                 Value /. MR$ParameterRules[currentclass]]/. Value -> MR$Null;
                               tempMGvalue = If[currentrules =!= MR$Null, currentelem /. currentrules, currentelem /. {currentelem -> NoValue[1]}];
                               If[tempMGvalue === currentelem, tempMGvalue = NoValue[1]];
(* CD, 26.02.2010: Added Expandion over flover of possible contracted indices *)
                               If[Not[FreeQ[tempMGvalue, Index]], Print["here"]; tempMGvalue = PerformIndexSum[Expand[tempMGvalue]]];
                               NumericalValue[currentelem] = NumericalValue[tempMGvalue];
                               NumericalValue[tempMGparamname] = NumericalValue[tempMGvalue];
                               currentrules = InteractionOrder /. MR$ParameterRules[currentclass] /. {InteractionOrder -> {}};
                               currentrules = If[(Head[currentrules] === List) && (Length[currentrules] == 2),
                                                   {currentelem -> currentrules},
                                                   currentrules];
                               tempMGintorder = currentelem /. currentrules /. {currentelem -> MR$Null};
                               currentrules = OrderBlock /. MR$ParameterRules[currentclass] /. OrderBlock -> {};
                               tempMGorderblock = currentelem /. currentrules /. {currentelem :> List @@ currentelem};
                               tempMGorderblock = {currentelem, tempMGorderblock} /. {currentelem -> tempMGparamname};
                               If[Head[OrderBlock[tempMGblockname]] === List,
                                  OrderBlock[tempMGblockname] = Append[OrderBlock[tempMGblockname], tempMGorderblock],
                                  OrderBlock[tempMGblockname] = {tempMGorderblock}];
                               currentrules = ComplexParameter /. MR$ParameterRules[currentclass] /. ComplexParameter -> {};
                               currentrules = Which[currentrules === False, {currentelem -> False},
                                                    currentrules === True, currentrules = {currentelem -> True},
                                                    True, currentrules];
                               Which[UnitaryQ[currentclass] === True, tempMGcomplexparam = True,
                                     OrthogonalQ[currentclass] === True, tempMGcomplexparam = False,
                                     True, tempMGcomplexparam = currentelem /. currentrules /. {currentelem -> True}];
                               tempopt = DeleteCases[{ParamName -> tempMGparamname, InteractionOrder -> tempMGintorder, Value -> tempMGvalue, MR$Complex -> tempMGcomplexparam, Description -> tempdescription}, _ -> MR$Null];
                               ExtParameter[currentelem, tempMGblockname, Sequence @@ tempopt],
                          (tempMGparamtype === Internal),
                               currentrules = Value /. MR$ParameterRules[currentclass];
                               tempdef20 = If[tempdef === {MR$Null}, {}, tempdef];
                               currentrules = If[(currentelem /. tempdef20) =!= currentelem,
                                                 {currentelem ->( currentelem /. tempdef20)},
                                                 currentrules]/. Value -> MR$Null;
                               tempMGvalue = If[currentrules =!= MR$Null, currentelem /. currentrules, currentelem /. {currentelem -> NoValue[1]}];
                               If[tempMGvalue === currentelem, tempMGvalue = NoValue[1]];
(* CD, 26.02.2010: Added Expandion over flover of possible contracted indices *)
                               If[Not[FreeQ[tempMGvalue, Index]], tempMGvalue = PerformIndexSum[Expand[tempMGvalue]]];
                               currentrules = ParameterName /. MR$ParameterRules[currentclass] /. {ParameterName -> {}};
                               tempMGparamname = currentelem /. currentrules /. {currentelem :> ToExpression[StringDrop[StringJoin[ToString[currentclass], Sequence @@ (StringJoin[ToString[#],"x"] & /@ currentelem)],-1]]};
                               NumericalValue[currentelem] = NumericalValue[tempMGvalue];
                               NumericalValue[tempMGparamname] = NumericalValue[tempMGvalue];
                               tempdescription = Description /. MR$ParameterRules[currentclass] /. Options[LoadModel];
                               If[(Head[tempdescription] =!= String) && (tempdescription =!= MR$Null), tempdescription = ToString[tempdescription]];
                               Which[(tempdescription === MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = ToString[tempMGparamname],
                                     (tempdescription === MR$Null) && (tempMGparamname === MR$Null), tempdescription = ToString[currentclass],
                                     (tempdescription =!= MR$Null) && (tempMGparamname =!= MR$Null), tempdescription = StringJoin[tempdescription, " ( ", ToString[tempMGparamname], " )"],
                                     (tempdescription =!= MR$Null) && (tempMGparamname === MR$Null), tempdescription = tempdescription];
                               currentrules = InteractionOrder /. MR$ParameterRules[currentclass] /. {InteractionOrder -> {}};
                               currentrules = If[(Head[currentrules] === List) && (Length[currentrules] == 2),
                                                   {currentelem -> currentrules},
                                                   currentrules];
                               tempMGintorder = currentelem /. currentrules /. {currentelem -> MR$Null};
                               currentrules = ComplexParameter /. MR$ParameterRules[currentclass] /. {ComplexParameter -> {}};
                               currentrules = Which[currentrules === False, {currentelem -> False},
                                                    currentrules === True, currentrules = {currentelem -> True},
                                                    True, currentrules];
                               Which[UnitaryQ[currentclass] === True, tempMGcomplexparam = True,
                                     OrthogonalQ[currentclass] === True, tempMGcomplexparam = False,
                                     True, tempMGcomplexparam = currentelem /. currentrules /. {currentelem -> False}];
                               tempopt = DeleteCases[{ParamName -> tempMGparamname, InteractionOrder -> tempMGintorder, MR$Complex -> tempMGcomplexparam, Description -> tempdescription}, _ -> MR$Null];
                               IntParameter[currentelem, tempMGvalue, Sequence @@ tempopt]]],
               {jj, Length[temparray]}]]],
            {kk, Length[MR$Parameters]}]];   
             ];


(* ::Section:: *)
(*Particle declaration (spin 3/2 added)*)


(* ::Subsection::Closed:: *)
(*Useful stuff*)


fieldtypeMG[RS] = RScalarField;
fieldtypeMG[CS] = CScalarField;
fieldtypeMG[F] = DiracField;
fieldtypeMG[M] = MajoranaField;
fieldtypeMG[RV] = RVectorField;
fieldtypeMG[CV] = CVectorField;
fieldtypeMG[RR] = RSpin32Field;
fieldtypeMG[CR] = CSpin32Field;
fieldtypeMG[U] = GhostField;

MGField[RS] = S;
MGField[CS] = S;
MGField[F] = F;
MGField[M] = F;
MGField[RV] = V;
MGField[CV] = V;
MGField[RR] = R;
MGField[CR] = R;
MGField[T] = T;
MGField[U] = U;

PartFieldType[_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True])&)] := "S";
PartFieldType[_?DiracFieldQ] := "F";
PartFieldType[_?MajoranaFieldQ] := "F";
PartFieldType[_?VectorFieldQ] := "V";
PartFieldType[_?RSpin32FieldQ] := "R";
PartFieldType[_?CSpin32FieldQ] := "R";
PartFieldType[_?Spin2FieldQ] := "T";
PartFieldType[_?GhostFieldQ] := "U";

$DefaultLineType[RS] = D;
$DefaultLineType[CS] = D;
$DefaultLineType[F] = S;
$DefaultLineType[M] = S;
$DefaultLineType[RV] = W;
$DefaultLineType[CV] = W;
$DefaultLineType[RR] = S;
$DefaultLineType[CR] = S;
$DefaultLineType[T] = W;
$DefaultLineType[U] = D;

$DefaultAntiPartName[name_, type_] := (ToString[name] <> "~") /; MemberQ[{CS, F, CV, CR, U},type];
$DefaultAntiPartName[name_, type_] := Hold[ToString[name]] /; MemberQ[{RS, M, RV, RR, T},type];


(* ::Subsection::Closed:: *)
(*Declaration*)


ParticlesMG[pp__] := Block[{pplist = List[pp], MRList, temppartlist, partclasslist},
        NPartMG = Length[pplist];   
        temppartlist = Table[Join[{pplist[[kk,3]],pplist[[kk,4]],MGField[pplist[[kk,2]]], pplist[[kk,5]], pplist[[kk,6, 1]], pplist[[kk,7, 1]]},
                     Table[pplist[[kk,ii]],{ii,8,15}]],{kk, 1, NPartMG}];
        partclasslist = KillDoubles[Table[temppartlist[[kk, 14]], {kk, NPartMG}]];
        PartList = {};
        Do[PartList = Append[PartList, {partclasslist[[kk]], Most /@ Select[temppartlist, Extract[#, 14] === partclasslist[[kk]] &]}], {kk, Length[partclasslist]}];           
        MassList = {MASS,DeleteCases[Table[{{pplist[[kk,10]]}, Sequence @@ pplist[[kk,6]]}, {kk, 1, NPartMG}],
             {{__}, ZERO, 0}]};
        WidthList = {DECAY,DeleteCases[Table[{{pplist[[kk,10]]},Sequence @@ pplist[[kk,7]]}, {kk, 1, NPartMG}],
             {{__}, ZERO, 0}]};];
                           
    
Options[Particle] = {PartName -> Hold[ToString[$defaultname]], AntiPartName -> Hold[$DefaultAntiPartName[$defaultname,$defaulttype]], 
       LineType -> Hold[$DefaultLineType[$defaulttype]], MassName -> Hold[ToExpression["M" <> ToString[$defaultname]]], 
       MassValue -> 1, WidthName -> Hold[ToExpression["W" <> ToString[$defaultname]]], WidthValue -> 1, 
       LineName -> Hold[ToString[$defaultname]], TeXParticleName -> Hold[ToString[$defaultname]], TeXAntiParticleName -> Hold[$DefaultAntiPartName[$defaultname,$defaulttype]], 
       MRClassName -> MR$NoClass};

Particle[name_, type_, STO_, pdg_, options___] := Module[{optionlist, output, antname, tm, tw, tgs, temp},
    optionlist = Which[(Mass /. {options}) === ZERO, Join[{options}, {MassName -> ZERO, MassValue -> 0}],
                       Length[Mass /. {options}] === 2, Join[{options}, {MassName -> (Mass /. {options})[[1]], MassValue -> (Mass /. {options})[[2]]}],
                       True, {options}];
    optionlist = Which[(Width /. {options}) === ZERO, Join[optionlist, {WidthName -> ZERO, WidthValue -> 0}],
                       Length[Width /. {options}] === 2, Join[optionlist, {WidthName -> (Width /. {options})[[1]], WidthValue -> (Width /. {options})[[2]]}],
                       True, optionlist]; 
    temp = ReleaseHold[{name, type, PartName, AntiPartName, LineType, {MassName, MassValue}, {WidthName, WidthValue}, STO, 
        LineName, pdg, FullName, TeXParticleName, TeXAntiParticleName, Goldstone, MRClassName} /. optionlist /. Options[Particle] /. {$defaultname -> name, $defaulttype -> type}];
    If[SelfConjugateQ[name], temp = ReplacePart[ReplacePart[temp, temp[[3]], 4], temp[[12]], 13]];
    If[type === F || type === R,
           PartNameMG[name] = temp[[3]];PartNameMG[anti[name]] = temp[[4]], 
           PartNameMG[name] = temp[[3]];PartNameMG[anti[name]] = temp[[4]]];
    PartName[name] = temp[[3]];
    PartName[anti[name]] = temp[[4]];
    PartSymbol[temp[[3]]] = name;
    PartSymbol[temp[[4]]] = anti[name];
    tgs = (Goldstone /. {options});
    If[tgs === NoGS, 
        PartNameCH[name] = temp[[3]];
        PartNameCH[anti[name]] = temp[[4]],
(* CD, 12.05.2010: Added tag for Goldstones *)
        GoldstoneQ[name] = True;
        GoldstoneQ[anti[name]] = True;
(* CD, end modif *)
        PartNameCH[name] := PartNameCH[tgs] <> ".f";
        PartNameCH[anti[name]] := PartNameCH[anti[tgs]] <> ".f"];
    If[GhostFieldQ[name],
       PartNameCH[name] := PartNameCH[GhToGB[name]] <> ".c";
       PartNameCH[anti[name]] := If[SelfConjugateQ[GhToGB[name]], PartNameCH[GhToGB[name]] <> ".C", PartNameCH[anti[GhToGB[name]]] <> ".C"]];
    tm = (Mass /. {options});
    If[Length[tm] == 2, MassToPart[tm[[1]]] = name; Mass[name] = tm[[1]]];
    If[(Length[tm] == 1) && (tm =!= ZERO), MassToPart[tm] = name; Mass[name] = tm];
    If[(tm === ZERO), Mass[name] = 0];
    tw = (Width /. {options});
    If[Length[tw] == 2, WidthToPart[tw[[1]]] = name; Width[name] = tw[[1]]];
    If[(Length[tw] == 1) && (tw =!= ZERO), WidthToPart[tw] = name; Width[name] = tw];
    If[(tw === ZERO), Width[name] = 0];
    FR$StringToSymbol = If[ValueQ[FR$StringToSymbol], Append[FR$StringToSymbol, Rule[temp[[3]], name]], {Rule[temp[[3]], name]}];
    antname = anti[name];
    If[antname =!= name, antname = anti[name]; FR$StringToSymbol = Append[FR$StringToSymbol, Rule[temp[[4]], antname]]];
    $ParticleListtemp = If[ValueQ[$ParticleListtemp], Append[$ParticleListtemp, temp], {temp}];
    output = temp];
                  
DeclareParticlesMG := Module[{nlist},
     $ParticleListtemp = KillDoubles[$ParticleListtemp];
     nlist = Length[$ParticleListtemp];
     ParticlesMG @@ $ParticleListtemp];     


(* ::Subsection::Closed:: *)
(*AddParticlesToClass*)


AddParticlesToClass[part_List, class_[n_], options___] := Block[{nind, classname, flav, addflav = True},
      classname = ReleaseHold[ClassName /. {options} /. Options[AddParticlesToClass] /. $defaultClassName -> part[[1]]];
      $ClassMembers[class[n]] = part;
      $ClassMembers[classname] = part;
      If[Intersection[MR$FieldList, part] =!= {}, Message[LoadModel::Part]];
      If[MemberQ[MR$FieldList, classname], Message[LoadModel::Part]];
      MR$FieldList = Join[MR$FieldList, part, anti /@ part, {classname, anti[classname]}];
      flav = FlavorIndex /. {options} /.Options[AddParticlesToClass];
      If[(flav =!= MR$NoFlavorIndex) && (Length[part] != 1), 
         FlavoredQ[classname] = True;
         If[Length[part] != Length[MRIndexRange[Index[flav]]], 
                                       Message[PartClass::FlavRange];
                                       addflav = False],
         If[Length[part] != 1, Message[PartClass::NoGenMatch];Abort[]]];
      MR$FlavorList = If[Not[MemberQ[MR$FlavorList, flav]] && Not[flav === MR$NoFlavorIndex], Append[MR$FlavorList, flav], MR$FlavorList];
        (*                                  *)
        (* Declaration of the fields for MR *)
        (*                                  *) 
      $IndList[classname] = Indices /. MR$ClassesRules[class[n]] /. Options[AddParticlesToClass];  
      $SymList[classname] = SymmetricIndices /. MR$ClassesRules[class[n]] /. Options[AddParticlesToClass];  
      $AntiSymList[classname] = AntiSymmetricIndices /. MR$ClassesRules[class[n]] /. Options[AddParticlesToClass];
      SymQ[classname] = If[$SymList[classname] =!= {}, True, False];
      AntiSymQ[classname] = If[$AntiSymList[classname] =!= {}, True, False];
      nind = Length[Indices /. MR$ClassesRules[class[n]]];
      Which[class === S,
        If[SelfConjugate /. MR$ClassesRules[class[n]],
           (*RScalarField[class[n], nind];*)
           MR$FieldType[classname] = "Real Scalar Field";
           RScalarField[classname, nind];
           If[FlavoredQ[classname], (RScalarField[#,nind-1]&) /@ part, (RScalarField[#,nind]&) /@ part],
           (*CScalarField[class[n], nind];*)
           CScalarField[classname, nind]; 
           MR$FieldType[classname] = "Complex Scalar Field";    
           If[FlavoredQ[classname], (CScalarField[#,nind-1]&) /@ part, (CScalarField[#,nind]&) /@ part]],
        class === F,
        $IndList[classname] = Prepend[$IndList[classname], Index[Spin]];
        If[SelfConjugate /. MR$ClassesRules[class[n]],
           (*MajoranaField[class[n], nind + 1];*)
           MR$FieldType[classname] = "Majorana Field";
           MajoranaField[classname, nind + 1];
           If[FlavoredQ[classname], (MajoranaField[#,nind]&) /@ part, (MajoranaField[#,nind + 1]&) /@ part],
           (*DiracField[class[n], nind + 1];*)
           MR$FieldType[classname] = "Dirac Field";
           DiracField[classname, nind + 1];
           If[FlavoredQ[classname], (DiracField[#,nind]&) /@ part, (DiracField[#,nind + 1]&) /@ part]],
        class === W,
        $IndList[classname] = Prepend[$IndList[classname], If[Chirality[classname] === Left, Index[Spin1], Index[Spin2]]];     
        MR$FieldType[classname] = "Weyl Field";
        WeylField[classname, nind + 1];
        If[FlavoredQ[classname], (WeylField[#,nind]&) /@ part, (WeylField[#,nind + 1]&) /@ part],
        class === V,
        $IndList[classname] = Prepend[$IndList[classname], Index[Lorentz]];
        If[SelfConjugate /. MR$ClassesRules[class[n]],
           (*RVectorField[class[n], nind + 1];*)
           MR$FieldType[classname] = "Real Vectorfield";
           RVectorField[classname, nind + 1];
           If[FlavoredQ[classname], (RVectorField[#,nind]&) /@ part, (RVectorField[#,nind + 1]&) /@ part],
           (*CVectorField[class[n], nind + 1];*)
           MR$FieldType[classname] = "Complex Vectorfield";
           CVectorField[classname, nind + 1];
           If[FlavoredQ[classname], (CVectorField[#,nind]&) /@ part, (CVectorField[#,nind + 1]&) /@ part]],
        class === R,
        $IndList[classname] = Prepend[Prepend[$IndList[classname], Index[Lorentz]], Index[Spin]];
        If[SelfConjugate /. MR$ClassesRules[class[n]],
           (*MajoranaField[class[n], nind + 1];*)
           MR$FieldType[classname] = "Real Spin 3/2 Field";
           RSpin32Field[classname, nind + 1];
           If[FlavoredQ[classname], (RSpin32Field[#,nind]&) /@ part, (RSpin32Field[#,nind + 1]&) /@ part],
           (*DiracField[class[n], nind + 1];*)
           MR$FieldType[classname] = "Complex Spin 3/2 Field";
           CSpin32Field[classname, nind + 1];
           If[FlavoredQ[classname], (CSpin32Field[#,nind]&) /@ part, (CSpin32Field[#,nind + 1]&) /@ part]],
        class === RW,
        $IndList[classname] = Prepend[Prepend[$IndList[classname], Index[Lorentz]], If[Chirality[classname] === Left, Index[Spin1], Index[Spin2]]];
           MR$FieldType[classname] = "Weyl Spin 3/2 Field";
           WSpin32Field[classname, nind + 1];
           If[FlavoredQ[classname], (WSpin32Field[#,nind]&) /@ part, (WSpin32Field[#,nind + 1]&) /@ part],
        class === T,
        If[Symmetric /. MR$ClassesRules[class[n]] /. Options[AddParticlesToClass], 
           SymQ[classname] = True;
           $SymList[classname] = Prepend[$SymList[classname], Index[Lorentz]]];
        If[AntiSymmetric /. MR$ClassesRules[class[n]] /. Options[AddParticlesToClass], 
           AntiSymQ[classname] = True;
           $AntiSymList[classname] = Prepend[$AntiSymList[classname], Index[Lorentz]]];
        $IndList[classname] = Prepend[Prepend[$IndList[classname], Index[Lorentz]], Index[Lorentz]];
        If[SelfConjugate /. MR$ClassesRules[class[n]],
       (*Spin2Field[class[n], nind + 2];*)
           MR$FieldType[classname] = "Spin 2 Field";
           Spin2Field[classname, nind + 2];
           If[FlavoredQ[classname], (Spin2Field[#,nind + 1]&) /@ part, (Spin2Field[#,nind + 2]&) /@ part],
           (* NC Added for complex tensor class. *)
		   MR$FieldType[classname] = "Complex Spin 2 Field";
           CSpin2Field[classname, nind + 2];
           If[FlavoredQ[classname], (CSpin2Field[#,nind + 1]&) /@ part, (CSpin2Field[#,nind + 2]&) /@ part]],
        class === U,
        If[SelfConjugate /. MR$ClassesRules[class[n]],
           (*RScalarField[class[n], nind];*)
           Message[Ghost::SelfConjugate],
           (*CScalarField[class[n], nind];*)
           GhostField[classname, nind]; 
           MR$FieldType[classname] = "Ghost Field";    
           If[FlavoredQ[classname], (GhostField[#,nind-1]&) /@ part, (GhostField[#,nind]&) /@ part]]
      ];
      MR$FieldList = KillDoubles[MR$FieldList];

(*Index declarations *)


      $IndList[anti[classname]] = $IndList[classname] /. {Spin1 -> Spin2, Spin2 -> Spin1};


      FlavoredQ[anti[classname]] = FlavoredQ[classname];

      If[$SymList[classname] =!= {}, $SymList[classname] = KillDoubles[$SymList[classname]]];
      If[$AntiSymList[classname] =!= {}, $AntiSymList[classname] = KillDoubles[$AntiSymList[classname]]];

      $SymList[anti[classname]] = $SymList[classname] /. {Spin1 -> Spin2, Spin2 -> Spin1};
      $AntiSymList[anti[classname]] = $AntiSymList[classname] /. {Spin1 -> Spin2, Spin2 -> Spin1};

      SymQ[anti[classname]] = SymQ[classname];
      AntiSymQ[anti[classname]] = AntiSymQ[classname];    
  
      Do[$IndList[part[[kk]]] = If[FlavoredQ[classname], DeleteCases[$IndList[classname], Index[flav]], $IndList[classname]];
         $IndList[anti[part[[kk]]]] = $IndList[part[[kk]]] /. {Spin1 -> Spin2, Spin2 -> Spin1};
         $SymList[part[[kk]]] = If[FlavoredQ[classname], DeleteCases[$SymList[classname], Index[flav]], $SymList[classname]];
         $SymList[anti[part[[kk]]]] = $SymList[part[[kk]]]/. {Spin1 -> Spin2, Spin2 -> Spin1};
         SymQ[part[[kk]]] = SymQ[classname];
         SymQ[anti[part[[kk]]]] = SymQ[classname];
         $AntiSymList[part[[kk]]] = If[FlavoredQ[classname], DeleteCases[$AntiSymList[classname], Index[flav]], $AntiSymList[classname]];
         $AntiSymList[anti[part[[kk]]]] = $AntiSymList[part[[kk]]]/. {Spin1 -> Spin2, Spin2 -> Spin1};
         AntiSymQ[part[[kk]]] = AntiSymQ[classname];
         AntiSymQ[anti[part[[kk]]]] = AntiSymQ[classname],
         {kk, Length[part]}];    
        (*                                           *)
        (* Relation between the FA and MR notations  *)
        (*                                           *)
	(*NC : Feb 4, 2015: Removed Dispatch from the next line because it was causing trouble in Math 10.  Moved it to after the loop finishes in FR$Declarations..*)
      $FAToMRRules = Join[$FAToMRRules, {temp -> classname, class[n, {ind___}] -> classname[ind]}]/. temp -> class[n];
	(*NC End*)
      If[FlavoredQ[classname] && addflav, 
         templist1 = Table[classname[ind1___, Index[flav, kk] ,ind2___], {kk, Length[MRIndexRange[Index[flav]]]}];
         templist2 = $ClassMembers[class[n]] //. {g1___, field_?(FreeQ[#, ind1]&), g2___} -> {g1, field[ind1, ind2], g2};
         templist3 = Table[anti[classname][ind1___, Index[flav, kk] ,ind2___], {kk, Length[MRIndexRange[Index[flav]]]}];
         templist4 = anti /@ ($ClassMembers[class[n]]) //. {g1___, field_?(FreeQ[#, ind1]&), g2___} -> {g1, field[ind1, ind2], g2};
         If[Length[templist1] != Length[templist2],
            Message[PartClass::NoGenMatch];Abort[],
            $FlavorExpandRules = Join[$FlavorExpandRules, Table[templist1[[kk]] -> templist2[[kk]], {kk ,Length[templist1]}], Table[templist3[[kk]] -> templist4[[kk]], {kk ,Length[templist3]}]]]];
];
 


(* ::Section:: *)
(*Parameter declaration*)


(* ::Subsection::Closed:: *)
(*Useful stuff*)


EParamQ[{xx_, ei_, yy__}] := (ei === Ext);
IParamQ[{xx_, ei_, yy__}] := (ei === Int);

SortEParamListMG[EPLMG_, BNL_]:= Module[{temp = {},EPLMGtemp},
    EPLMGtemp = EPLMG;
    Do[temp = Append[temp, Select[EPLMGtemp,(#[[2]] === BNL[[kk]])&]];
       EPLMGtemp = DeleteCases[EPLMGtemp,{_,BNL[[kk]],___}], {kk, Length[BNL]-1}];
    temp = Append[temp, EPLMGtemp]];
    
FactorBlockName[xx__] := Prepend[{(Delete[#,2]&) /@ xx}, xx[[1,2]]];
    
SortOrderBlock[OB_,elemfunc_,WriteRules_] := Module[{elem2list, tempfunc, nOB = Length[OB]},
    tempOB = OB //. WriteRules;
    elem2list = Sort[(Last /@ tempOB)];
    Do[tempfunc[tempOB[[kk, 2]]] = tempOB[[kk, 1]], {kk, nOB}];
    output = Table[{elem2list[[kk]],elemfunc[tempfunc[elem2list[[kk]]]]}, {kk, 1, nOB}]];
   
AddOrderBlock[{BN_, xx_},elemfunc_,WriteRules_] := {BN, Reverse /@ MapIndexed[List,xx]} /; Not[ValueQ[OrderBlock[BN]]];
AddOrderBlock[{BN_, xx_},elemfunc_,WriteRules_] := {BN, SortOrderBlock[OrderBlock[BN],elemfunc,WriteRules]} /; ValueQ[OrderBlock[BN]]; 
     
MGOrder[x_] := NoOrder /; Not[ValueQ[MGOrdertemp[x]]];
MGOrder[x_] := MGOrdertemp[x][[1]]^MGOrdertemp[x][[2]] /; ValueQ[MGOrdertemp[x]];

MGOrderQ[NoOrder] = True;

ParamRules = {};


(* ::Subsection::Closed:: *)
(*GetOrder*)


HomoMorph /: HomoMorph[a_+b_] := HomoMorph[a];
HomoMorph /: HomoMorph[a_-b_] := HomoMorph[a];
HomoMorph /: HomoMorph[a_*b_] := HomoMorph[a]*HomoMorph[b];
HomoMorph /: HomoMorph[a_/b_] := HomoMorph[a]*HomoMorph[b]^(-1);
HomoMorph /: HomoMorph[1/b_] := HomoMorph[b]^(-1);
HomoMorph /: HomoMorph[a_^n_] := HomoMorph[a]^n;
HomoMorph /: HomoMorph[Sqrt[a_]] := HomoMorph[a]^(1/2);
HomoMorph /: HomoMorph[Conjugate[a_]] := HomoMorph[a];

GetOrder[expr_] := Module[{output, temp, OrderContainer},
    temp = HomoMorph[Expand[expr]];
    temp = temp //. HomoMorph -> MGOrder ;
    temp = temp//.Power[Power[exp_,n2_],n1_]:>Power[exp,n1*n2];
    temp = temp //. NoOrder -> 1;
    temp = temp //.a_*x_^n_. :> x^n /; MGOrderQ[x] && Not[MGOrderQ[a]];
    If[Head[temp] === Plus, temp = temp //. Plus[xx___,a_^n_.,yy___] :> Plus[xx,yy] /; Not[MGOrderQ[a]]];
    temp = temp * OrderContainer[] //. a_ * OrderContainer[xx___] -> OrderContainer[a,xx];
    temp = temp //. OrderContainer[xx___,a_/b_,yy___] -> OrderContainer[xx,a,b^(-1),yy];
    temp = temp /. x_^n_.:> {x,n} /; MGOrderQ[x];
    output = List @@ temp];
    
GetOrder[{x1_,x2___}] := GetOrder[x1] /; (x1 =!= 0);
GetOrder[{0,x2_,___}] := GetOrder[x2];


(* ::Subsection:: *)
(*ExtParameter*)


Options[ExtParameter] = {Value -> 1, InteractionOrder -> {}, ParamName -> Hold[$defaultparamname], MR$Complex -> False};

ExtParameter[gMath_, block_, options___] := Module[{output, temp},
    temp = ReleaseHold[Flatten[{gMath, ParamName, Ext, Value, block, InteractionOrder, MR$Complex, Description} /. {options} /. Options[ExtParameter] /. $defaultparamname -> gMath]];
    NameToParameter[temp[[2]]] = temp[[1]]; 
    ParamIntOrder[temp[[1]]] = InteractionOrder /. {options} /. Options[ExtParameter];
    If[VectorQ[ParamIntOrder[temp[[1]]]],
       ParamIntOrder[temp[[1]]] = {ParamIntOrder[temp[[1]]]}
      ];
    ParamIntOrder[temp[[2]]] = ParamIntOrder[temp[[1]]]; 
    Which[Length[temp] == 8,
              MGOrdertemp[temp[[2]]] = {temp[[6]],1};
              MGOrderQ[temp[[6]]] = True;
              elemfunc[temp[[2]]] = {temp[[2]],temp[[6]],1,temp[[4]],temp[[7]], temp[[8]]},
          Length[temp] == 9,
              MGOrdertemp[temp[[2]]] = {temp[[6]], temp[[7]]};
              MGOrderQ[temp[[6]]] = True;
              elemfunc[temp[[2]]] = {temp[[2]],temp[[6]],temp[[7]],temp[[4]],temp[[8]], temp[[9]]},
          Length[temp] == 7,
              elemfunc[temp[[2]]] = {temp[[2]],temp[[4]],temp[[6]], temp[[7]]},
          EvenQ[Length[temp]-7]&&Length[temp]-7>0,
              MGOrdertemp[temp[[2]]] = temp[[6;;Length[temp-2]]];
              For[kk=6,kk<Length[temp]-2,kk=kk+2,MGOrderQ[temp[[kk]]] = True;];
              elemfunc[temp[[2]]] = {temp[[2]],Sequence@@temp[[6;;Length[temp]-2]],temp[[4]],temp[[-2]], temp[[-1]]}
    ];
    If[Not[gMath === temp[[2]]], ParamRules = If[ValueQ[ParamRules], Append[ParamRules, gMath -> temp[[2]]], {gMath -> temp[[2]]}]];
    $ParamListtemp = If[ValueQ[$ParamListtemp], Append[$ParamListtemp, temp], {temp}];
    output = temp];


(* ::Subsection::Closed:: *)
(*IntParameter*)


Options[IntParameter] = {InteractionOrder -> {}, ParamName -> Hold[$defaultparamname], MR$Complex -> False};

IntParameter[gMath_, def_, options___] := Module[{output, temp},
    temp = ReleaseHold[Flatten[{gMath, ParamName, Int, def, InteractionOrder, MR$Complex, Description} /. {options} /. Options[IntParameter] /. $defaultparamname -> gMath]];
    NameToParameter[temp[[2]]] = temp[[1]];
    ParamIntOrder[temp[[1]]] = InteractionOrder /. {options} /. Options[IntParameter];
    If[VectorQ[ParamIntOrder[temp[[1]]]],
       ParamIntOrder[temp[[1]]] = {ParamIntOrder[temp[[1]]]}
      ];
    ParamIntOrder[temp[[2]]] = ParamIntOrder[temp[[1]]]; 
    Which[Length[temp] == 7,
              MGOrdertemp[temp[[2]]] = {temp[[5]],1};
              MGOrderQ[temp[[5]]] = True;
              elemfunc[temp[[2]]] = {temp[[2]],temp[[5]],1, temp[[6]], temp[[7]]},
          Length[temp] == 8,
              MGOrdertemp[temp[[2]]] = {temp[[5]], temp[[6]]};
              MGOrderQ[temp[[5]]] = True;
              elemfunc[temp[[2]]] = {temp[[2]],temp[[5]],temp[[6]],temp[[7]], temp[[8]]},
          Length[temp] == 6,
              elemfunc[temp[[2]]] = {temp[[2]], temp[[5]], temp[[6]]}];
    If[Not[gMath === temp[[2]]], ParamRules = If[ValueQ[ParamRules], Append[ParamRules, gMath -> temp[[2]]], {gMath -> temp[[2]]}]];
    $ParamListtemp = If[ValueQ[$ParamListtemp], Append[$ParamListtemp, temp], {temp}];
    output = temp];


(* ::Subsection::Closed:: *)
(*Declaration*)


ParametersMG[cc__] := Module[{cclist, cclist2, BlockNameList,EPLtemp, temp},
    ParamListMG = List[cc];
    NParamMG = Length[ParamListMG];
    cclist = First /@ ParamListMG;
    cc2list = Extract[#,2]& /@ ParamListMG;
    ParamListMG = Rest /@ ParamListMG;
    ParamListMG = ParamListMG //. ParamRules;
    Do[temp = ParamListMG[[kk]];
       If[temp[[2]] === Ext,
          If[Length[temp] == 7,
                   ParamListMG = ReplacePart[ParamListMG, Append[temp,1],kk]],
          If[Length[temp] == 6,
                   ParamListMG = ReplacePart[ParamListMG, Append[temp,1],kk]]],
       {kk, NParamMG}];
    EParamList = Select[ParamListMG, EParamQ];
    NEParam = Length[EParamList];
    IParamList = Select[ParamListMG, IParamQ];
    NIParam = Length[IParamList];    
    If[(NEParam + NIParam) != NParamMG, Message[ParametersMG::EI]; 
      Abort[]];
    ParamList = Join[EParamList, IParamList];
    If[EParamList =!= {}, 
       EParamList = (Insert[Delete[#,3], #[[3]], -3]&) /@ EParamList;
       EParamList = (Delete[#,2]&) /@ EParamList;
       BlockNameList = (Extract[#,2]& /@ EParamList) //. {xx___,a_,yy___,a_,zz___} -> {xx,a,yy,zz};
       BlockNameList = Sort[BlockNameList];
       EParamList = SortEParamListMG[EParamList, BlockNameList];
       EParamList = FactorBlockName /@ EParamList;
       EParamList = (AddOrderBlock[#,elemfunc,ParamRules]&) /@ EParamList];
    If[IParamList =!= {},
       IParamList = (Delete[#,2]&) /@ IParamList];];
    
    
DeclareParametersMG := Module[{nlist, ParamRules2},
     ParamRules2 = ParamRules //. $TensIndRules;
     ParamRules = KillDoubles[Join[ParamRules, ParamRules2]];
     $ParamListtemp = KillDoubles[$ParamListtemp];
     nlist = Length[$ParamListtemp];
     ParametersMG @@ $ParamListtemp]; 


(* ::Subsection::Closed:: *)
(*Tensor parameters*)


Options[DeclareTensor] = {TensorClass -> $defaulttensclass, Unitary -> False, Orthogonal -> False, Hermitian -> False, ComplexParameter -> True, TeX -> MR$Null};

$TensClassList = {MR$GammaMatrices, MR$LeviCivita};

$TensIndRules = If[Not[ValueQ[$TensIndRules]], {}, $TensIndRules];

DeclareTensor[t_, ind_List, options___] := Block[{tc, tcom, tex},
      tc = TensorClass /. {options} /. Options[DeclareTensor] /. $defaulttensclass -> t;
      UnitaryQ[t] = Unitary /. {options} /. Options[DeclareTensor];
      OrthogonalQ[t] = Orthogonal /. {options} /. Options[DeclareTensor];
      HermitianQ[t] = Hermitian /. {options} /. Options[DeclareTensor];
      tex = TeX /. {options} /. Options[DeclareTensor];
      If[(GaugeMatrixQ[t] =!= True) && (StrucConstQ[t] =!= True), If[tex =!= MR$Null, MakeTeXIndex[tc, tex], MakeTeXIndex[tc, tc]]
         (*Format[t[indind__], TraditionalForm] := Subscript[t, indind];
         Format[t[indind__], StandardForm] := Subscript[t, indind]*)];
      If[OrthogonalQ[t] && UnitaryQ[t], Message[Tensor::UnitOrth]];
      If[HermitianQ[t] && UnitaryQ[t], Message[Tensor::UnitHerm]];
      If[HermitianQ[t] && OrthogonalQ[t], Message[Tensor::HermOrth]];
      $TensClass[t] = tc;
      $TensClass[t[___]] := $TensClass[t];
      If[Not[MemberQ[$TensClassList, tc]], $TensClassList = Append[$TensClassList, tc]];
      numQ[t[ii__]] := True /; (Length[{ii}] == Length[ind]);
      tcom = ComplexParameter /. {options} /. Options[DeclareTensor];
      CompTensQ[t] = tcom;
      If[OrthogonalQ[t], tcom = False];
  (* MW edit: real unitary tensors are orthogonal *)
     If[UnitaryQ[t] && !CompTensQ[t], OrthogonalQ[t] = True];
      If[tcom, CnumQ[t[ii__]] := True /; (Length[{ii}] == Length[ind]), CnumQ[t[ii__]] := False /; (Length[{ii}] == Length[ind])];
      TensQ[t] = True;
      TensQ[t[___]] := True;
      t[x___][y___] := t[x, y];
      $IndList[t] = ind;
      $TensIndRules = Append[$TensIndRules, t[a___, ii_?(FreeQ[#, Index] &), b___] :> t[a, Index[(Identity @@ Part[$IndList[t], Length[{a}] + 1]), ii], b]];
  
   (* MW edit: define rules for unitary, orthogonal and hermitian tensors *)
   (* OK, Orthogonal and Unitary tensors are now sorted out in *)
   (* VertexRoutine.m, so these should really go. However, see comments in *)
   (* VertexRoutine.m *)
    (* If[UnitaryQ[t],
       t /: Conjugate[t[Index[name_, i_],j_]] t[Index[name_, i_],k_] * aa_:= IndexDelta[j,k] * aa /; FreeQ[aa,i];
       t /: Conjugate[t[j_,Index[name_, i_]]] t[k_,Index[name_, i_]] * aa_ := IndexDelta[j,k] * aa /; FreeQ[aa,i];
     ];
     If[OrthogonalQ[t],
       t /: t[Index[name_, i_],j_] t[Index[name_, i_],k_] * aa_ := IndexDelta[j,k] * aa /; FreeQ[aa,i];
       t /: t[j_,Index[name_, i_]] t[k_,Index[name_, i_]] * aa_ := IndexDelta[j,k] * aa /; FreeQ[aa,i];
     ];*)
(*     If[HermitianQ[t],
       t /: Conjugate[t[i_,j_]] := t[j,i];
       If[UnitaryQ[t],
         (*t[i_,Index[name_, j_]] t[Index[name_, j_],k_] * aa_ := IndexDelta[i,k] * aa /; FreeQ[aa,j];*)
          $TensorSimplifyRules = Append[$TensorSimplifyRules, RuleDelayed[t[i_,Index[name_, j_]] t[Index[name_, j_],k_] * aa_ , IndexDelta[j,k] * aa /; FreeQ[aa,i]]]
       ];
     ];
     If[UnitaryQ[t],
        $TensorSimplifyRules = Join[$TensorSimplifyRules, 
              {RuleDelayed[Conjugate[t[Index[name_, i_?(Not[NumericQ[#]]&)],j_]] t[Index[name_, i_],k_] * aa_, IndexDelta[j,k] * aa /; FreeQ[aa,i]], 
               RuleDelayed[Conjugate[t[j_,Index[name_, i_?(Not[NumericQ[#]]&)]]] t[k_,Index[name_, i_]] * aa_, IndexDelta[j,k] * aa /; FreeQ[aa,i]]}]];
     If[OrthogonalQ[t],
        $TensorSimplifyRules = Join[$TensorSimplifyRules, 
              {RuleDelayed[t[Index[name_, i_?(Not[NumericQ[#]]&)],j_] t[Index[name_, i_],k_] * aa_, IndexDelta[j,k] * aa /; FreeQ[aa,i]], 
               RuleDelayed[t[j_,Index[name_, i_?(Not[NumericQ[#]]&)]] t[k_,Index[name_, i_]] * aa_, IndexDelta[j,k] * aa /; FreeQ[aa,i]]}]];*)
        

];


(* ::Section:: *)
(*Gauge group declaration*)


(* ::Subsection:: *)
(*Abelian gauge groups*)


AbelianGaugeGroup[name_, boson_, charge_, coupconst_, report_] := Block[{output},
      numQ[charge] = True;
      numQ[charge[___]] := True;
      AbelianQ[name] = True;
      GroupToBoson[name] = boson;
      GroupToCharge[name] = charge;
      GroupToCoup[name] = coupconst;
      GroupToNorm[name] = GUTNormalization/.MR$GaugeGroupRules[name]/.GdUTNormalization->1;

     (* MW edit: define covariant derivative *)
    (* DC[name][(field_?FieldQ)[ii___], mu_] :=
       del[field, mu] - I FR$DSign coupconst boson[mu] DiagProd[charge,field][ii];
     DC[name][field_?FieldQ, mu_] :=
       del[field, mu] - I FR$DSign coupconst boson[mu] DiagProd[charge,field];
     DC[name,chrg_][(field_?FieldQ)[ii___], mu_] :=
       del[field, mu] - I FR$DSign coupconst boson[mu] DiagProd[chrg,field][ii];
     DC[name,chrg_][field_?FieldQ, mu_] :=
       del[field, mu] - I FR$DSign coupconst boson[mu] DiagProd[chrg,field];*)

     DCint[name][(field_?FieldQ)[ii___], mu_] := - I FR$DSign coupconst boson[mu] charge[field] field[ii];
     DCint[name][field_?FieldQ, mu_] := - I FR$DSign coupconst boson[mu] charge[field] field;
      
      If[report, Print["Abelian gauge group ", ToString[name], " declared."];
           Print["Charge: ", ToString[charge]];
           Print["Gauge boson: ", ToString[boson]];
           Print["Field Strength tensor: FS[ ", boson, ",\[Mu], \[Nu]]"]];

      If[Not[MemberQ[MR$ParameterList, coupconst]], 
         Message[Gauge::Coupling];
         numQ[coupconst] = True; 
         MR$ParameterList = Append[MR$ParameterList, coupconst]];
      
      If[report, Print["(* * * * * * * * * * * * * * * * *)"]];
      ];


(* ::Subsection::Closed:: *)
(*Non abelian gauge groups*)


FormatGaugeMatrix[repmat_List, name_, adjind_] := Block[{temptensclass}, 
         temptensclass = ToExpression[StringJoin[ToString[name], "$", ToString[repmat[[2]]]]];
         GaugeMatrixQ[repmat[[1]]] = True;
         DeclareTensor[repmat[[1]], {adjind, repmat[[2]], repmat[[2]]}, TensorClass -> temptensclass];
         Format[repmat[[1]][aind_], StandardForm] := Power[repmat[[1]], aind];
         Format[repmat[[1]][aind_, ind__], StandardForm] := Power[Subscript[repmat[[1]], ind], aind];
         Format[repmat[[1]][aind_], TraditionalForm] := Power[repmat[[1]], aind];
         Format[repmat[[1]][aind_, ind__], TraditionalForm] := Power[Subscript[repmat[[1]], ind], aind];
];


Options[NonAbelianGaugeGroup] = {AdjointIndex -> MR$Null, SymmetricTensor -> MR$Null, Dynkin -> MR$Null, Casimir ->MR$Null};

NonAbelianGaugeGroup[name_, boson_, reps_, coupconst_, strucconst_, report_, options___] :=  Block[{tempstrucconst, tempadjind, temptensclass, NAtempDterm, 
    dstring, oldrules, AdjSymbol, newDef, newreps, newcasi, newdynk,casi,dynk},

      tempadjind = AdjointIndex /. {options} /. Options[NonAbelianGaugeGroup];
      casi = Casimir/.{options} /. Options[NonAbelianGaugeGroup];
      dynk = Dynkin/.{options} /. Options[NonAbelianGaugeGroup];


      If[(tempadjind =!= MR$Null) && (Not[MemberQ[$IndList[boson], Index[tempadjind]]]),
          Message[Gauge::WrongAdjInd];Abort[],
          If[Length[DeleteCases[$IndList[boson], Index[Lorentz]]] == 1, 
             tempadjind = Identity @@ Extract[DeleteCases[$IndList[boson], Index[Lorentz]],1],
             Message[Gauge::WrongAdjInd];Abort[]]];
      AbelianQ[name] = False;
      GroupToBoson[name] = boson;
      GroupToReps[name] = reps;
      GroupToStrucConst[name] = strucconst;
      GroupToAdj[name] = tempadjind;
     (* BF: group invariants *)
      GroupToDynkin[name] = Dynkin/.{options} /. Options[NonAbelianGaugeGroup];
      GroupToCasimirs[name] = casi;
      GroupToCoup[name] = coupconst;
      temptensclass = ToExpression[StringJoin[ToString[name], "$", ToString[tempadjind]]];
      NAtempDterm = SymmetricTensor /. {options} /. Options[NonAbelianGaugeGroup];
      If[NAtempDterm =!= MR$Null,
         DTermFormat[name] = NAtempDterm;
         DeclareTensor[NAtempDterm, {tempadjind, tempadjind, tempadjind}, TensorClass -> temptensclass, ComplexParameter -> False];
         Format[NAtempDterm[ind___], StandardForm] := Subscript[DTermFormat[name], ind];
         Format[NAtempDterm[ind___], TraditionalForm] := Subscript[DTermFormat[name], ind];
         SymTensQ[NAtempDterm] = True];
      If[strucconst =!= Eps, 
         StrucConstQ[strucconst] = True;
         StrucConstQ[strucconst[___]] := True;
         DeclareTensor[strucconst, {tempadjind, tempadjind, tempadjind}, TensorClass -> temptensclass, ComplexParameter -> False];
         Format[strucconst[ind___], StandardForm] := Subscript[strucconst, ind];
         Format[strucconst[ind___], TraditionalForm] := Subscript[strucconst, ind]];
   
   (* BF: Replacement lists by double lists for Casimirs, Dynkin and Representations *)
      newreps=If[VectorQ[reps],List[reps],reps];
      newcasi=If[VectorQ[casi],List[casi],casi];
      newdynk=If[VectorQ[dynk],List[dynk],dynk];
      If[newreps===MR$Null,newreps={}];
      If[newcasi===MR$Null,newcasi={}];
      If[newdynk===MR$Null,newdynk={}];

   (* BF: Adjoint representation definition *)
      If[Not[MemberQ[newreps[[All,2]],tempadjind]], 
        oldrules=DeleteCases[MR$GaugeGroupRules[name],Rule[Definitions,_]|RuleDelayed[Definitions,_]|Rule[Representations,_]|RuleDelayed[Representations,_]];
        AdjSymbol=Symbol["F"<>ToString[name]];
        DeclareTensor[AdjSymbol, {tempadjind, tempadjind, tempadjind}, ComplexParameter->True];
        AdjointRep[name]=AdjSymbol;
        GaugeMatrixQ[AdjSymbol] = True;
        newreps=Append[newreps,List[AdjSymbol,tempadjind]];
        GroupToReps[name]=newreps;
        newDef=Prepend[Definitions/.MR$GaugeGroupRules[name]/.Definitions->{},Rule[AdjSymbol[a_,b_,c_],-I strucconst[a,b,c]]]; 
        MR$GaugeGroupRules[name]=Join[oldrules,{myRuleDelayed[Definitions,newDef], Rule[Representations,newreps]}]/.myRuleDelayed->RuleDelayed;
        M$GaugeGroups=M$GaugeGroups/.Equal[name,_]:>Equal[name,MR$GaugeGroupRules[name]];
        MR$GaugeGroups=MR$GaugeGroups/.Rule[name,_]:>Rule[name,MR$GaugeGroupRules[name]] ];

   (* BF: Casimir and Dynkin for the adjoint*)
      If[Not[MemberQ[newcasi[[All,2]],tempadjind]], 
        newcasi=Append[newcasi,Rule[tempadjind,Sqrt[Length[IndexRange[Index[tempadjind/.Index[aa__]->aa]]/.{NoUnfold[bb_]->bb,Unfold[bb_]->bb}]+1]]]];
      If[Not[MemberQ[newdynk[[All,2]],tempadjind]], newdynk=Append[newdynk,Rule[tempadjind,1/2]]];
      
   (* BF: Casimir for the fundamentals *)
      Module[{funds,NN},
        NN=Sqrt[Length[IndexRange[Index[tempadjind/.Index[aa__]->aa]]/.{NoUnfold[bb_]->bb,Unfold[bb_]->bb}]+1];
        funds=(Cases[{#,Length[IndexRange[Index[#/.Index[aa__]->aa]]/.{NoUnfold[bb_]->bb,Unfold[bb_]->bb}]}&/@newreps[[All,2]],
          {_,NN}])[[All,1]];
        funds=DeleteCases[funds,aaa_?(MemberQ[newcasi[[All,2]],#]&)];
        newcasi=Join[newcasi,(Rule[#,(NN^2-1)/(2 NN)]&/@funds)];
        funds=DeleteCases[newreps[[All,2]],aaa_?(MemberQ[newdynk[[All,2]],#]&)];
        newdynk=Join[newdynk,(Rule[#,1/2]&/@funds)];     
      ];
      GroupToCasimirs[name] = newcasi;
(*Mod 03.22.2013 AA: Dynkin index is now calculated from the representations. The function just below *)
      (*GroupToDynkins[name] = newdynk;*)

      FormatGaugeMatrix[#, name, tempadjind]& /@ newreps;
      FS[boson, mu_, nu_, a_] := FS[boson, mu, nu, a, strucconst, coupconst]
];


(* ::Text:: *)
(*Computing the dynkin index for every group*)


GroupToDynkins[gr_]:=Block[{reps,fund,adj,ind,gen,indd,dyn,bla},
  If[!AbelianQ[gr], 
  (*the dynkin index is given by S(R)IndexDelta[a,b] = Tr(T^A T^B) *)
  (*intialization*) 
    (*list of all representations*) 
    reps=GroupToReps[gr];
    (*The non-adjoint reprsentation*)
    fund=DeleteCases[reps,List[_,GroupToAdj[gr]]];
    (*The adjoint representation*)
    adj=Cases[reps,List[_,GroupToAdj[gr]]][[1]];

  (*Computing Dynkings for the adjoint*)
    (*First the indices associated to the adjoint. bla is a dummy index*)
    ind={bla,adj[[2]],adj[[2]]};
    (*Then create something like generator[bla, Index[type,ind1], Index[type,ind2] ]*)
    gen=ApplyDefinitions[GroupToStrucConst[gr]][Sequence@@Inner[Index,ind,Unique/@ind,List]];
    (*Isolate the indices ind1 and ind2 to expand them*)
    indd=gen/.a_[ind1_,ind2__]:>List@ind2/.Index[_,a_]->a;
    (*Expand indices ind1 and ind2 while setting bla to 1. This will create the matrix representing the first generator of the adj*)
    gen=MyTable[gen/.a_[Index[_,_],ind1_,ind2_]:>a[1,ind1,ind2]/.Index[_,a_]->a,Sequence@@Inner[List,indd,IndexDim/@Delete[ind,1],List]]/.MyTable->Table;
    (*Compute the dynkin index according to the formula above and return a rule like Rep -> dynkin*)
    dyn=Rule[adj[[2]],Tr[gen . ConjugateTranspose[gen]]];

  (*Dynkin for other representations*)
    dyn=Join[{dyn},(ind={bla,#[[2]],#[[2]]};
    gen= #[[1]][Sequence@@Inner[Index,ind,Unique/@ind,List]];
    indd=gen/.a_[ind1_,ind2__]:>List@ind2/.Index[_,a_]->a;
    gen=ApplyDefinitions/@(MyTable[gen/.a_[Index[_,_],ind1_,ind2_]:>a[1,ind1,ind2]/.Index[_,a_]->a,Sequence@@Inner[List,indd,IndexDim/@Delete[ind,1],List]]/.MyTable->Table);
   Rule[#[[2]],Tr[gen . ConjugateTranspose[gen]]])&/@fund];

  (*Special case for SU(3)*)
   (*For the moment keep it commented 
    Global`Colourb=Colour;*)
   dyn=dyn/.{Rule[Colour,a__]->Rule[Colour,1/2],Rule[Gluon,a__]->Rule[Gluon,3/2]};
 (* Print results*)
 Return[dyn],
 Print["No Dynkin for abelian groups"];]];


(* MW edit: routine for declaring U (1) charges *)

DeclareU1Charges[name_, chargelist_List] := Block[
 {rules},

 chrglist = If[MatchQ[chargelist,{__List}] || chargelist === {},
   chargelist, {chargelist}];

 (name /: DiagProd[#[[1]], name] := #[[2]] name)& /@ chrglist;
 (name /: DiagProd[#[[1]], f_name] := #[[2]] f)& /@ chrglist;
];


(* ::Subsection::Closed:: *)
(*AddGaugeRepresentation*)


(* ::Text:: *)
(*AddGaugeRepresentation[rules, list] adds rules to list, if list exists. If not list is first created. *)


AddGaugeRepresentation[rules_List, list_:FR$NewGaugeRepresentations] := If[ValueQ[list], list = Join[list, rules], list = rules];

AddGaugeRepresentation[rule_Rule, list___] := AddGaugeRepresentation[{rule}, list];
AddGaugeRepresentation[rule_RuleDelayed, list___] := AddGaugeRepresentation[{rule}, list];          


(* ::Subsection::Closed:: *)
(*UpdateGaugeRepresentations*)


(* ::Text:: *)
(*UpdateGaugeRepresentations[ rule ] addes the representations in rule to M$GaugeGroups*)


UpdateGaugeRepresentations[rule_Rule] := Block[{

   gaugegroups = First /@ M$GaugeGroups,
   workgroup = rule[[1]],
   reps = rule[[2]],
   oldreps,
   oldgroup
   },

   If[MemberQ[gaugegroups, workgroup],
      oldgroup = Cases[M$GaugeGroups, Equal[workgroup, _]][[1,2]];
      M$GaugeGroups = DeleteCases[M$GaugeGroups, Equal[workgroup, _]];
      If[Not[FreeQ[oldgroup, Representations]],
         oldreps = Representations /. oldgroup;
         If[VectorQ[oldreps], oldreps = {oldreps}];,
         oldreps = {}
        ];
      oldgroup = DeleteCases[oldgroup, Rule[Representations,_]|RuleDelayed[Representations,_]];
      If[VectorQ[reps], reps = {reps}];
      reps = Join[reps, oldreps];
      oldgroup = Append[oldgroup, Representations -> reps];
      AppendTo[M$GaugeGroups, workgroup == oldgroup];
     ];
];


      
UpdateGaugeRepresentations[rule_RuleDelayed] := UpdateGaugeRepresentations[rule];

UpdateGaugeRepresentations[list_List] := UpdateGaugeRepresentations /@ Union[list];
      

   


(* ::Section::Closed:: *)
(*Coupling Orders*)


(* ::Text:: *)
(*CD, 12.04.11*)
(**)
(*The model file can contain a two variables*)
(**)
(*M$InteractionOrderHierarchy = { { QCD, 1},*)
(*                                                        {QED, 2},*)
(*                                                        ...*)
(*                                                        }*)
(*                                                        *)
(*M$InteractionOrderLimit = { { QCD, 3},*)
(*                                                { QED, 4},*)
(*                                                ...*)
(*                                                }*)
(*                                                *)
(* note that this is optional. If an order does not appear in the lists, then the default is*)
(* * 1, for the hierarchy*)
(* * Infinity, for the limit*)
(*                                                          *)


(* ::Subsection::Closed:: *)
(*ReadOutOrdersFromParamList*)


(* ::Text:: *)
(*ReadOutOrdersFromParamList[ eparamlist, iparamlist ] goes through the eparamlist and iparamlist,*)
(*and returns the list of all order defined in these lists*)


ReadOutOrdersFromParamList[eparamlist_, iparamlist_] := Block[{

    eorders = eparamlist,
    iorders = iparamlist

    },

    eorders = #[[2]]& /@ (Join @@ (#[[2]]& /@ eorders));
    eorders = Select[eorders, Length[#]>4&];
    eorders = Union[DeleteCases[Flatten[Drop[Rest[#],-3]&/@ eorders],_Integer]];

    iorders = Union[DeleteCases[Join @@ (Drop[Drop[#,2],-2]&/@iorders),_Integer]];

    Return[Union[eorders, iorders]];

];


(* ::Subsection::Closed:: *)
(*InitializeInteractionOrders*)


(* ::Text:: *)
(*InitializeInteractionOrders[ hierarchy, expansion ] takes the content of the matrices hierarchy and expansion (which correspond to the lists M$InteractionOrderHierarchy and M$InteractionOrderLimit), and saturates them with default values. then, it stores the results in FR$InteractionOrderLimit and FR$InteractionOrderHierarchy*)


InitializeInteractionOrders[hierarchy_, expansion_] := Block[{

    hier = hierarchy,
    exp = expansion,
    hierorders, exporders,
    allorders

    },

    

    (* Read out thye symbols for the orders *)
    hierorders = First /@ hier;
    exporders  = First /@ exp;

    (* Check if nothing is doubly defined *)
    If[Length[Union[hierorders]] != Length[hierorders],
       Message[LoadModel::IntOrderHierarchy]; 
       hierorders = Union[hierorders];
       hier = hierorders /. (Rule @@@ hier);
      ];
    If[Length[Union[exporders]] != Length[exporders],
       Message[LoadModel::IntOrderLimit];
       exporders = Union[exporders];
       exp = exporders /. (Rule @@@ exp)
      ];

    (* Read out all orders *) 
    allorders = ReadOutOrdersFromParamList[EParamList, IParamList];

    (* and select those that were not yet defined *)
    hierorders = Complement[allorders, hierorders];
    exporders  = Complement[allorders, exporders];

    (* and make them default *)
    hierorders = {#, 1}& /@ hierorders;
    exporders  = {#, Infinity}& /@ exporders;

    (* Save the list *)
    FR$InteractionOrderHierarchy = Join[hier, hierorders];
    FR$InteractionOrderLimit     = Join[exp,  exporders];
    FR$InteractionOrderPerturbativeExpansion = {#, 0}& /@ (First /@ FR$InteractionOrderHierarchy);
];
    

     
    


(* ::Subsection::Closed:: *)
(*OverwriteInteractionOrders*)


(* ::Text:: *)
(*We want that if a model consists of various files (e.g., LoadModel["SM.fr", "Extension.fr"] with conflicting settings for the hierarchy and the limit, only the setting in the last .fr file is kept. This function does this for both the limits and the hierarchy.*)
(*A warning is issued, and the warning that is issued corresponds to the type (limit or hierarchy), which is set through the options Type:*)
(**)
(*Type -> "Hierarchy"*)
(*Type -> "Limits"*)
(**)
(*The default is Type -> "Hierarchy"*)


Options[OverwriteInteractionOrders] = {Type -> "Hierarchy"};


OverwriteInteractionOrders[list_List, OptionsPattern[]] := Block[{

  allorders = Union[First /@ list], (* all orders that are defined *)
  newsetting
  
  },

  (* If list is empty, nothing to be done *)
  If[list === {},
     Return[list]
    ];

  (*
     Gather according to the orders, without destroying the relative order. This
     is important in order to assure that we take the last one
  *)
  newsetting = DeleteCases[Table[Union[DeleteCases[list,Except[{ord,_}]]], {ord,allorders}], {}];
  
  (* Keep the last one in each case, and throw message *)
  If[Length[#] > 1, Message[IntOrder::Overwrite, OptionValue[Type], #[[1,1]]]]& /@ newsetting;
  newsetting = Last /@ newsetting;

  Return[newsetting];

];
