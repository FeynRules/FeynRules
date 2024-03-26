(* ::Package:: *)

(* ::Title:: *)
(*UFO : Main file*)


(* ::Section:: *)
(*Setup*)


(* ::Subsection::Closed:: *)
(*Version*)


UFO$Version = "1.1";


(* ::Subsection:: *)
(*Python files coming with FR (definition of classes, etc)*)


PY$InputFiles = {"object_library.py", 
                 "__init__.py",
                 "function_library.py",
                 "write_param_card.py",
                 "propagators.py"
                };


(* ::Subsection::Closed:: *)
(*Supported particle types*)


(* ::Text:: *)
(*These are the supported classes (for the moment this corresponds to unitary gauge).*)


GenInt$Classes = {U, S, F, V, R, T};


(* ::Text:: *)
(*These are the spin types supported (as 2s+1)*)


GenInt$Spins = {-1, 1, 2, 3, 4, 5};


(* ::Text:: *)
(*These are the supported color structures (as the dim of the irrep)*)


GenInt$Colors = {1, 3, 6, 8};


(* ::Text:: *)
(*Should Goldstone boson be written out?*)


GenInt$Goldstones = True;


(* ::Subsection::Closed:: *)
(*Compulsory PDG codes*)


GenInt$CompulsoryPDG = {{"SM leptons", 11, 13, 15},
                        {"SM neutrinos", 12, 14, 16},
                        {"SM quarks", 1, 2, 3, 4, 5, 6},
                        {"SM gauge bosons", 21, 22, 23, 24}
                       };


(* ::Subsection:: *)
(*UFO system variables*)


PY$IntIndexCounter = 1;


(* ::Section:: *)
(*LoadPackage*)


<< "PYIntLog.m";
<< "PYIntParameters.m";
<< "PYIntParticles.m";
<< "PYIntVertices.m";
<< "PYCouplingOrders.m";
<< "PYDecays.m";
<< "PYFormFactors.m";
<< "PYNLO.m";
(*<< "PYRestrictions.m";*)




(* ::Section::Closed:: *)
(*Messages*)


GenInt::PartName = "Warning: several particles have the same name.";

GenInt::AntiPartName = "Warning: several antiparticles have the same name.";

GenInt::Masses = "Warning: several masses have the same name.";

GenInt::Widths = "Warning: several widths have the same name.";

GenInt::PDG = "Warning: several particles have the same PDG code.";

GenInt::Lags = "The Lagrangians in slot `1` are zero, and will be ignored.";


IntOrder::Sum = "Different terms in `1` have different interaction orders.";


(* ::Section:: *)
(*PYCheckLagrangian*)


(* ::Text:: *)
(*PYCheckLagrangian[ lagrangians ] checks whether the lagrangians have the correct form for the interface.*)
(**)
(** Zeros are removed. A warning is printed to the screen, and to the log file.*)


PYCheckLagrangians[lagrangians___] := Block[{lags = Expand[lagrangians], zeros},

    (* Write to the logfile *)
    AppendTo[GenInt$LogFile, "# Checking the Lagrangians"];

    (* Remove the zeros *)
    lags = DeleteCases[lags, 0];
    If[Length[lags] != Length[lagrangians],
       zeros = Flatten[Position[lags, 0]];
       Message[GenInt::Lags, zeros];
       AppendTo[GenInt$LogFile, "   * The Lagrangians in slot `1` are zero, and will be ignored."]
      ];

    (* Return and exit *)
    (* If nothing has changed, write this to the logfile *)
    If[Length[lags] == Length[lagrangians],
       AppendTo[GenInt$LogFile, "   * All Lagrangians are ok."]
      ];

    WriteToLogFile[GenInt$LogFileName];
   
    Return[lags];

];
       


(* ::Section:: *)
(*PYCheckQuantumNumbers*)


(* ::Text:: *)
(*PYCheckQunatumNumbers[ list ] checks whether <list> contains the symbol for the electric charge (Q).*)
(*If not, a message a printed on the screen, and a line is added to the log file.*)


PYCheckQuantumNumbers[list_List] := Block[{containsQ},

    (* Write to the logfile *)
    AppendTo[GenInt$LogFile, "# Checking the Quantum numbers"];

    containsQ = MemberQ[MR$QuantumNumbers, Q];

    If[Not[containsQ],
       Print[Style["Warning: no electric charge defined. Putting all electric charges to zero.", Red]];
       AppendTo[GenInt$LogFile, "   >>> Electric charge not defined for the model. Assuming zero electric charge for all particles."],
       (*else*)
       AppendTo[GenInt$LogFile, "   * Electric charge defined."];
    ];

    WriteToLogFile[GenInt$LogFileName];

];
       


(* ::Section:: *)
(*PYSplitVertices*)


(* ::Text:: *)
(*PYSplitVertices[ vertices ] takes a list of vertices and separates them into color Lorentz and coupling structures.*)


(* ::Section:: *)
(*WriteUFO*)


(* ::Subsection:: *)
(*UpdateInitPY*)


UpdateInitPY[] := Block[{
   authors = Authors /. M$Information /. Authors -> {},
   date = Date  /. M$Information /. Date -> "",
   version = Version  /. M$Information /. Version -> "",
   init = OpenAppend["__init__.py"]
   },

   authors = Flatten[{authors}];

   WriteString[init, "\n\n"];
   WriteString[init, "gauge = [0", If[Global`FeynmanGauge === True, ", 1]\n","]\n"]];
   WriteString[init, "\n\n"];

   If[authors =!= {},
      authors = StringJoin @@ Riffle[authors, ", "];
      WriteString[init, "__author__ = \"", authors, "\"\n"];
     ];
   If[date =!= "",
      WriteString[init, "__date__ = \"", date, "\"\n"];
     ];
   If[version =!= "",
      WriteString[init, "__version__= \"", version, "\"\n"];
     ];

   Close[init];
];
    
    
   
    

   


(* ::Subsection:: *)
(*Options*)


Options[WriteUFO] := Join[Options[FeynmanRules], 
    {Input -> {}, 
     Debug -> False, 
     DialogBox -> On, 
     Output :> StringReplace[M$ModelName <> "_UFO", {" " -> "_"}],
     NegativeInteractionOrder -> Automatic,
     Optimization -> False,
     RemoveGhosts -> False,
     AddDecays    ->  True,
     SimplifyDecays -> False,
     R2Vertices   -> {},
     UVCounterterms -> {},
     CTParameters -> {} /. {Rule[FlavorExpand, _] :> Rule[FlavorExpand, Automatic]},
     Restrictions -> {}
}
];


WriteUFO[lagrangians_List, options___] := WriteUFO[Sequence @@ lagrangians, options];


WriteUFO[lagrangians___, OptionsPattern[]] := Block[{lags = {lagrangians}, vertices, opts, message, decays = {},
   (* internal and external parameters *)
   eparams = EParamList, iparams = IParamList, masslist = MassList, widthlist = WidthList,
   plist,
   (* Particles *)
    partlist = PartList,
    simplifydecays = OptionValue[SimplifyDecays],
   (* form factors*)
    formfactors,
   (* NLO *)
   PY$R2Vertices = OptionValue[R2Vertices], 
   PY$UVVertices = OptionValue[UVCounterterms],
   PY$CTparameters = OptionValue[CTParameters],
   IsUFOAtNLO = False, NLOcoupls, 
   PY$OldDirectory = Directory[]
   },

   Print[" --- Universal FeynRules Output (UFO) v " <> UFO$Version <> " ---"];

(* * * * * * * * * * * * * * * * * * * * * * * *)
(* Preparation                                *)
(* * * * * * * * * * * * * * * * * * * * * * * *)



   (* Set Global variables *)
    $Debug = OptionValue[Debug];
    GenInt$Dialog = OptionValue[DialogBox];
    GenInt$LogFile = {};
    GenInt$DialogString = {};
    PY$NegativeInteractionOrder = OptionValue[NegativeInteractionOrder];
    PY$NegativeInteractionOrderFound = False;

    (*Simplify If*)
    PY$UVVertices=PY$UVVertices/.((Rule[#,#/.MR$Definitions]&)/@Union[Cases[PY$UVVertices,_If,\[Infinity]]]);

    (*Simplify using the definitions to avoid 0/0 later*)
    (*Off[Simplify::time];
    PY$R2Vertices=Simplify[PY$R2Vertices//.MR$Definitions/.FR$RmDblExt,TimeConstraint->1];
    On[Simplify::time];*)


   (* Create the output directory *)
   If[Not[MemberQ[FileNames[], OptionValue[Output]]],
      CreateDirectory[OptionValue[Output]]
     ];
   SetDirectory[OptionValue[Output]];

   DeleteFileIfExists /@ PY$InputFiles;
   CopyFile[Global`$FeynRulesPath <> "/Interfaces/UFO/Model/" <> #, #]& /@ PY$InputFiles; 
   UpdateInitPY[];

   (* Initialize the log file *)
   GenInt$LogFileName = GenIntInitializeLogFile[OptionValue[Output] <> ".log"];


(*  Complete __init __.py *)
 (*  WritePYModelInformation[];*)


(* * * * * * * * * * * * * * * * * * * * * * * *)
(* Check input                                *)
(* * * * * * * * * * * * * * * * * * * * * * * *)
   
   (* Check that the electric charge Q is defined for the model *)
   PYCheckQuantumNumbers[MR$QuantumNumbers];


   (* Check the Lagrangians *)
   If[lags =!= {},
      lags = PYCheckLagrangians[lags];
     ];

   (* Check the particles *)
   partlist = CheckPythonParticles[partlist];


   (* Check the paramaters *)
   plist = CheckPythonParameters[eparams, iparams, masslist, widthlist];


   (* Create the dialog box *)
   (* First, check if dialog boxes are enabled and if there are any messages *)
   If[GenInt$Dialog && (Length[GenInt$DialogString] =!= 0),
      PrependTo[GenInt$DialogString, "Warning:\n"];
      AppendTo[GenInt$DialogString, "Proceed anyway?"];
      CreateDialogBox[GenInt$DialogString]
      ];


(* * * * * * * * * * * * * * * * * * * * * * * *)
(* Computation of the Feynman  rules          *)
(* * * * * * * * * * * * * * * * * * * * * * * *)


   (* We now compute the FeynmanRules, If they are not input by hand *)
   AppendTo[GenInt$LogFile, ""];
   AppendTo[GenInt$LogFile, "# Vertices"];
   If[lags =!= {},

      AppendTo[GenInt$LogFile, "   * Calling FeynmanRules for " <> ToString[Length[lags]] <> " Lagrangians."];

      (* Construct options for Feynman Rules 
         ConservedQuantumNumbers is put to False.
      *)


      opts = Table[Rule[Options[WriteUFO][[iii,1]], OptionValue[Options[WriteUFO][[iii,1]]]], {iii, Length[Options[WriteUFO]]}];

      opts = FilterRules[opts, First /@ Options[FeynmanRules]];

      opts = FilterRules[opts, Except[ConservedQuantumNumbers|ScreenOutput]];
      opts = Join[opts, {ConservedQuantumNumbers -> False, ScreenOutput -> False}];
      (* Set FlavorExpand from Automatic to FR$AutoFlavorExpand, and from {} to False *)
      opts = opts /. {Rule[FlavorExpand, Automatic] -> Rule[FlavorExpand, FR$AutoFlavorExpand]} /. {Rule[FlavorExpand, {}] -> Rule[FlavorExpand, False]};

      vertices = Join @@ (FeynmanRules[#, Sequence @@ opts]& /@ lags);

     ];

   


   (* Add the manually input vertices *)
   If[OptionValue[Input] =!= {},
      If[Not[ValueQ[vertices]], vertices = {}];
      AppendTo[GenInt$LogFile, "   * Appending manually appended vertices."];
      vertices = Join[vertices, OptionValue[Input]];
     ];


   (* Add the R2 and UV vertices, if present *)
    If[PY$R2Vertices =!= {},
       IsUFOAtNLO = True;
       PY$R2Vertices = SwitchVertexParticlesFAToFR /@ PY$R2Vertices;
       PY$R2Vertices = SwitchFromFAToFRVertexConventions /@ PY$R2Vertices;
       PY$R2Vertices = AddPYR2UVTag[#, "R2"]& /@ PY$R2Vertices;
       vertices = Join[vertices, PY$R2Vertices];
      ];



    If[PY$UVVertices =!= {},
       (*To avoid that IPL and IF are inside a conjugate*)
       PY$UVVertices = PY$UVVertices//.{Conjugate[aa_Plus]:>Conjugate/@aa,Conjugate[aa_Times]:>Conjugate/@aa}/.Conjugate[xx_IPL]:>xx/.Conjugate[If[xx_,a_,c_]]:>
         If[xx,Conjugate[a],Conjugate[c]];

       IsUFOAtNLO = True;
       PY$UVVertices = SwitchVertexParticlesFAToFR /@ PY$UVVertices;
       PY$UVVertices = SwitchFromFAToFRVertexConventions /@ PY$UVVertices;
       PY$UVVertices = AddPYR2UVTag[#, "UV"]& /@ PY$UVVertices;
       (* We need to rename the logs, in order to avoid divergences in the flavor expansion *)
       PY$UVVertices = PY$UVVertices /. Log -> RenormLog (*/. FR$MU -> 1*);
       vertices = Join[vertices, PY$UVVertices];
      ];

     If[PY$CTparameters =!= {},
       (*To avoid that IPL and IF are inside a conjugate*)
       PY$CTparameters = PY$CTparameters//.{Conjugate[aa_Plus]:>Conjugate/@aa,Conjugate[aa_Times]:>Conjugate/@aa}/.Conjugate[xx_IPL]:>xx/.Conjugate[If[xx_,a_,c_]]:>
         If[xx,Conjugate[a],Conjugate[c]];

       IsUFOAtNLO = True;
       PY$CTparameters = SwitchFromFAToFRVertexConventions /@ (PY$CTparameters/.Rule->List);
       PY$CTparameters = PY$CTparameters /. Log -> RenormLog (*/. FR$MU -> 1*);
      ];

If[$Debug,
  Print[FreeQ[vertices,Log]];
  Print[FreeQ[vertices,RenormLog]];
];

   (* Apply simplification rules *)
   vertices = DeleteCases[VertexSimplify @@@ vertices, {_, 0}];

   (*vertices = MergeSortedVertices[vertices];*)
   vertices = MergeAllVertices[vertices];
 
  (* Update the Logfile with the number of vertices before flavor expansion*)
   AppendTo[GenInt$LogFile, "   * Number of classes vertices: " <> ToString[Length[vertices]]];
  (* Start FlavorExpansion *)

  vertices = FlavorExpansion[vertices];
  AppendTo[GenInt$LogFile, "   * Number of flavored vertices: "<> ToString[Length[vertices]]];

  (* Save vertices for later *)
  AddToRunTable[vertices];
  message = "Saved vertices in InterfaceRun[ " <> ToString[Length[FR$InterfaceRuns]] <> " ].";
  Print["   - " <> message];
  AppendTo[GenInt$LogFile, "   * " <> message];

If[$Debug,
  Print[FreeQ[vertices,Log]];
  Print[FreeQ[vertices,RenormLog]];
];

  (* Check if vertices only contain mass eigenstates *)
  CheckMassEigenstates[vertices];


  (* Remove ghosts and goldstones, if required *)
  If[OptionValue[RemoveGhosts],
     AppendTo[GenInt$LogFile, "   * Removing all ghosts and Goldstone bosons form the output."];
     vertices = RemoveGhostsAndGoldstones[vertices];
     partlist = DeleteCases[partlist, {_,_,-1,__}|{__, Except[NoGS]}];
    ];

  (* And we update the internal params *)

  If[FR$AdditonalIParamsFromAbbreviations =!= {},
     plist = Join[plist, ChangeIParameterConventions[FR$AdditonalIParamsFromAbbreviations]];
     vertices = vertices /. FR$InteractionOrderEmergencyReplacement;
    ];

  If[FR$AbbIndexSumExpanded =!= {},
     plist = Join[plist, ChangeIParameterConventions[FR$AbbIndexSumExpanded]];
    ];

  (* We now check for QNumber conservation *)

  If[(OptionValue[ConservedQuantumNumbers] =!= {}) && (OptionValue[ConservedQuantumNumbers] =!= False),
     AppendTo[GenInt$LogFile, "   * Checked QNumber conservation."];
     QNConservationToLogFile[CheckQuantumNumberConservation[vertices, OptionValue[ConservedQuantumNumbers]]];
     ];

   WriteToLogFile[GenInt$LogFileName];


(* * * * * * * * * * * * * * * * * * * * * * * *)
(* Multi-fermions                             *)
(* * * * * * * * * * * * * * * * * * * * * * * *)

   (*vertices = BreakIntoFermionFlows[vertices]; commented by celine*)



(* * * * * * * * * * * * * * * * * * * * * * * *)
(* Compute Decays                             *)
(* * * * * * * * * * * * * * * * * * * * * * * *)

(*Print[OptionValue[AddDecays]];*)
   If[OptionValue[AddDecays],
      decays = GetUFODecays[vertices //. {PYR2UVTag[_] :> 0}, SimplifyDecays -> simplifydecays];
      ];




(* * * * * * * * * * * * * * * * * * * * * * * *)
(* Split into {Color, Lorentz, coupl}         *)
(* * * * * * * * * * * * * * * * * * * * * * * *)
If[$Debug,
  Print["before MSBAR"];
  Print[FreeQ[vertices,Log]];
  Print[FreeQ[vertices,RenormLog]];
];
   (* Before splitting, we remove the finite renormalisation for massless particles introduced by FA *)
   (*vertices = ProjectMasslessOntoMSBar[vertices]; commented by celine*)

If[$Debug,
  Print["after MSBAR"];
  Print[FreeQ[vertices,Log]];
  Print[FreeQ[vertices,RenormLog]]
];


   Print["Preparing Python output."];
   (* First, reorder the particles *)
   vertices = RelabelExt[{PYReorderParticles[#1],#2}]& @@@ vertices;
   vertices = RelabelExt[PYOrderFermions[#1,#2]]& @@@ vertices;
   (* Simplify Colour-Eps[i,j,k], and remove possible zeroes *)
   vertices = DeleteCases[vertices /. {Eps[Index[Colour, i_], Index[Colour, j_], Index[Colour, k_]] :> OrderEps[Eps[Index[Colour, i], Index[Colour, j], Index[Colour, k]]]}, {_, 0}];
   (* Then, we make all particles outgoing *)
   vertices = MakeOutgoingParticles /@ vertices;

   formfactors = If[FR$FormFactors =!= {},
                    MatchFormFactorsToVertexList[vertices, Ingoing -> False],
                    {}
                   ];

   (* We now separate into Lorentz, color and coupling structures , anmd create PY$LorentzObjects and PY$CouplObjects*)
   Print["    - Splitting vertices into building blocks."];
   vertices = PYSplitVertices[vertices];
(*Print["after split"];
Print[InputForm[vertices]];*)

   (* Color optimization *)
   (*vertices = OptimizeColors @@@ vertices;*)

   (* We introduce anti-colour objects (EpsilonBar, K6B) *)
   vertices = RenameAntiColourStructures @@@ vertices;

   (* Optimize by removing zero couplings *)
   If[OptionValue[Optimization],
      vertices = OptimizeUFOVertices[vertices, PY$CouplObjects, PY$LorentzObjects];
     ]; 

   (* Add the form factors to the lorentz objects *)

   PY$LorentzObjects = AddFormFactorsToLorentzObject /@ PY$LorentzObjects;

(* * * * * * * * * * * * * * * * * * * * * * * *)
(* NLO module                                 *)
(* * * * * * * * * * * * * * * * * * * * * * * *)

(*
   This part of the code is only called if either R2 terms 
   or UV counterterms are given as input.
*)
If[$Debug,
  Print[FreeQ[PY$CouplObjects,Log]];
  Print[FreeQ[PY$CouplObjects,RenormLog]]
];
  If[IsUFOAtNLO,
     NLOcoupls = Transpose[CollectCouplingsByTag[PY$CouplObjects]];

 
     {PY$R2Vertices,  PY$R2CouplObjects} = BuildPYR2UVVertices[vertices, NLOcoupls, "R2"];
     {PY$UVVertices,  PY$UVCouplObjects} = BuildPYR2UVVertices[vertices, NLOcoupls, "UV"];
     {vertices,       PY$CouplObjects}   = BuildPYR2UVVertices[vertices, NLOcoupls, "1"];

    {PY$R2Vertices, PY$R2CouplObjects} = ProcessNLOVertices[PY$R2Vertices, PY$R2CouplObjects, PY$UVVertices, PY$UVCouplObjects];
    (* Note that the variables now contains both R2 and UV, distinguished by a tag *)
    ];

$KEEP = PY$R2Vertices;
$KEEPUV = PY$UVVertices;


If[$Debug,     
  Print[FreeQ[PY$R2CouplObjects,Log]];
  Print[FreeQ[PY$R2CouplObjects,RenormLog]]
];   






(* * * * * * * * * * * * * * * * * * * * * * * *)
(* WriteOut                                   *)
(* * * * * * * * * * * * * * * * * * * * * * * *)

   Print["    - Writing files."];

 (*   (* Create restrictions.py *)
   If[ValueQ[NLOCT$assumptions] && (NLOCT$assumptions =!= {}),
      WritePYAssumptions[NLOCT$assumptions]
      ];


   If[OptionValue[Restrictions] =!= {},
      WritePYRestrictions[PY$OldDirectory,OptionValue[Restrictions]];
     ];*)

   (* Create particles.py *)
   WritePYParticles[partlist];

   (* Create parameters.py *)
   WritePYParameters[plist, IsUFOAtNLO];

   (* Create vertices.py *)
   WritePYVertices[vertices];

   (* Create lorentz.py *)
   WritePYLorentz[PY$LorentzObjects];

   (* Create vertices.py *)
   WritePYCouplings[PY$CouplObjects];

   (* Create coupling_orders.py*)
   WritePYCouplingOrders[];

   (* Create decays.py*)
   If[OptionValue[AddDecays],
      WriteUFODecays[decays];   
      ];


   (* Create form_factors.py*)
   If[FR$FormFactors =!= {},
      WritePYFormFactors[formfactors]
      ];


   (* Create CT_couplings.py *)
   If[PY$R2CouplObjects =!= {},
     If[$Debug,
       Print["write"];
       Print[FreeQ[PY$R2CouplObjects,Log]];
       Print[FreeQ[PY$R2CouplObjects,RenormLog]]
     ];
     WritePYCTCouplings[PY$R2CouplObjects]
   ];

   If[PY$R2Vertices =!= {},
      WritePYCTVertices[PY$R2Vertices]
     ];

   (* Exit *)
   ResetDirectory[];

   Print["Done!"];


];
 

