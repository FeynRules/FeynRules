(* ::Package:: *)

(* ::Title:: *)
(*particles.py*)


(* ::Section:: *)
(*Test Particles*)


(* ::Text:: *)
(*These tests check that all the entries in the modified PartList have the form required.*)
(*  If not, a warning a printed.*)


(* ::Subsection:: *)
(*Test functions*)


IsGenIntParticleName = StringQ;


IsGenIntSpin[ss_] := MemberQ[GenInt$Spins, ss];


IsGenIntColor[cc_] := MemberQ[GenInt$Colors, cc];


IsGenIntLineType[ll_] := MemberQ[{S, W, C, D, GhostDash}, ll]


IsGenIntMassOrWidth[mw_] := (Head[mw] === Symbol);


IsGenIntPropagatorLabel[ll_] := StringQ[ll];


IsGenIntPDG[pdg_] := (IntegerQ[pdg] || Head[pdg] === NoPDG);


IsGenIntDescription[des_] := StringQ[des];


IsGenIntTeXName[tex_] := StringQ[tex];


IsGenIntGoldStone[gold_] := Not[gold === NoGS];


(* ::Subsection:: *)
(*GenIntRemoveParticleClasses*)


(* ::Text:: *)
(*GenIntRemoveParticleClasses[partlist] removes the class assignments from partlist.*)


GenIntRemoveParticleClasses[partlist_] := Join @@ partlist[[All,2]];


(* ::Subsection:: *)
(*GentIntChangeParticleConventions*)


(* ::Text:: *)
(*GenIntChangeParticleConventions[ partlist ] changes the entries so that thay match the GenInt conventions:*)
(** Spin {S,V,F,T}    ->  {0,2,3,4,5}*)
(** Color {S, T, O}   ->  {1,3,8}*)


GenIntChangeSpinConvention[ss_] := Which[ss === U, -1,
                                         ss === S, 1,
                                         ss === F, 2,
                                         ss === V, 3,
                                         ss === R, 4,
                                         ss === T, 5,
                                         True, ss ];


GenIntChangeColorConvention[cc_] := Which[cc === S,  1,
                                          cc === T,  3,
                                          cc === S6, 6,
                                          cc === O,  8,
                                          True, cc];


GenIntChangeParticleConventions[partlistentry_] := Join[partlistentry[[1;;2]], {GenIntChangeSpinConvention[partlistentry[[3]]]}, partlistentry[[4;;6]], {GenIntChangeColorConvention[partlistentry[[7]]]}, partlistentry[[8;;13]]];


(* ::Subsection:: *)
(*GenIntRemoveParticles*)


(* ::Text:: *)
(*GenIntRemoveParticles[ partlist ] removes all particles from partlist that do not meet the criteria fixed in *)
(** GenInt$Classes*)
(** GenInt$Spins*)
(** GenInt$Colors*)
(** GenInt$GoldStones*)


GenIntRemoveParticles[partlist_] := Block[{temppartlist = partlist, removed, newpartlist},
   
   (* Remove particles that are not in GenInt$Classes nor GenInt$Spins*)

   newpartlist = Select[temppartlist, MemberQ[Join[GenInt$Classes, GenInt$Spins], #[[3]]]&];
   
   removed = Complement[temppartlist, newpartlist];
   If[Length[removed] =!= 0,
      AppendTo[GenInt$LogFile, "   * Particle " <> #1 <> " removed. Not in GenInt$Classes."]& @@@ removed
     ];

   temppartlist = newpartlist;


   (* Remove particles that are not in GenInt$Colors *)
   newpartlist = Select[temppartlist, MemberQ[GenInt$Colors, #[[7]]]&];
   
   removed = Complement[temppartlist, newpartlist];
   If[Length[removed] =!= 0,
      AppendTo[GenInt$LogFile, "   * Particle " <> #1 <> " removed. Not in GenInt$Colors."]& @@@ removed
     ];

   temppartlist = newpartlist;


(*   (* Remove GoldStones (if relevant) *)
   If[Not[GenInt$Goldstones],
      newpartlist = Cases[temppartlist, {___, NoGS}];
   
      removed = Complement[temppartlist, newpartlist];
      If[Length[removed] =!= 0,
         AppendTo[GenInt$LogFile, "   * Particle " <> #1 <> " removed. Goldstone boson."]& @@@ removed
         ]
      ];

   temppartlist = newpartlist; *)


    (* If no particles have been removed, write this to the log file *)
    If[Length[temppartlist] == Length[partlist],
       AppendTo[GenInt$LogFile, "   * No particles removed. All particles correspond to GenInt setup."]
      ];

    (* Return and exit *)
    FRDebug["GenIntRemoveParticles", temppartlist];
    Return[temppartlist]
];


(* ::Subsection:: *)
(*GenIntGlobalParticleTests*)


(* ::Text:: *)
(*GenIntGlobalParticleTests[ partlist ] tests wether all particles*)
(** have a different ParticleName.*)
(** have a different AntiParticleName.*)
(** have a different Mass name.*)
(** have a different Width name.*)


GenIntGlobalParticleTests[partlist_] := Block[{temp},

    (* Test ParticleName *)
    temp = partlist[[All, 1]];
    If[DoubleEntriesQ[temp, GenInt$ParticlesLog, "   >>> Some particles have the same name."], Message[GenInt:PartName]];

    (* Test AntiParticleName *)
    temp = partlist[[All, 2]];
    If[DoubleEntriesQ[temp, GenInt$ParticlesLog, "   >>> Some antiparticles have the same name."], Message[GenInt:AntiPartName]];

    (* Test Masses *)
    temp = DeleteCases[partlist[[All, 5]], ZERO];
    If[DoubleEntriesQ[temp, GenInt$ParticlesLog, "   * Some masses have the same name."], Message[GenInt:Masses]];

    (* Test Widths *)
    temp = DeleteCases[partlist[[All, 6]], ZERO];
    If[DoubleEntriesQ[temp, GenInt$ParticlesLog, "   * Some widths have the same name."], Message[GenInt:Widths]];

    (* Test PDG *)
    temp = DeleteCases[partlist[[All, 9]], ZERO];
    If[DoubleEntriesQ[temp, GenInt$ParticlesLog, "   >>> Some masses have the same name."], Message[GenInt:PDG]]

];


(* ::Subsection::Closed:: *)
(*GenIntIndividualParticleTests*)


(* ::Text:: *)
(*GenIntIndividualParticleTests[ partlist ] applies all the test functions to partlist. *)
(*If a test fails, a warning is printed to the logfile.*)


GenIntIndividualParticleTests[partlist_]:= Block[{},
   
   (* Test ParticleName *)
   TestQ[IsGenIntParticleName, #, "   >>> Some particle names are not strings!"]& /@ partlist[[All, 1]];

   (* Test AntiParticleName *)
   TestQ[IsGenIntParticleName, #, "   >>> Some antiparticle names are not strings!"]& /@ partlist[[All, 2]];

   (* Test PDG *)    
   TestQ[IsGenIntPDG, #, "   >>> Some antiparticle names are not strings!"]& /@ partlist[[All, 9]];

];
   


(* ::Subsection:: *)
(*ParticleToSpin*)


(* ::Text:: *)
(*ParticleToSpin[ particle ] return the spin (2s+1) of the particle.*)
(*If the particle type is not known, 0 is returned.*)


ParticleToSpin[particle_] := Which[(GhostFieldQ[particle] === True), -1,
       (ScalarFieldQ[particle] === True), 1,
       (DiracFieldQ[MakeIdenticalFermions[particle]] === True) || (MajoranaFieldQ[MakeIdenticalFermions[particle]] === True) || (WeylFieldQ[particle] === True), 2,
       VectorFieldQ[particle] === True, 3,
       Spin32FieldQ[particle] === True, 4,
       Spin2FieldQ[particle] === True, 5,
       True,  0];


(* ::Subsection:: *)
(*PYConvertLineType*)


(* ::Text:: *)
(*PYConvertLineType[  ] converts the PropagatorStyle options form the Fr convention to the PY onvention, and returns the line with which particles should be drawn in a Feynman diagram. *)
(**)
(**  'dotted'*)
(**  'dashed'*)
(**  'wavy'*)
(**  'curly'*)
(**  'straight'*)


PYConvertLineType[linetype_] := Which[(linetype === GhostDash), PYString["dotted"],
       (linetype === D), PYString["dashed"],
       (linetype === S), PYString["straight"],
       (linetype === W), PYString["wavy"],
       (linetype === C), PYString["curly"],
       True,  linetype];


(* ::Section:: *)
(*CheckPythonParticles*)


CheckPythonParticles[partlist_] := Block[{plist = partlist},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# Particle definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];
   WriteToLogFile[GenInt$LogFileName];

   (* Transform the original PartList to remove the classes entry *)
   plist = GenIntRemoveParticleClasses[plist];

   (* Change the naming conventions for the spins and the color *)
   plist = GenIntChangeParticleConventions /@ plist;

   (* Remove all particles that are not defined in the setup in GenInt_main *)
   plist = GenIntRemoveParticles[plist];

   (* Perform the tests *)
   GenIntGlobalParticleTests[plist];
   GenIntIndividualParticleTests[plist];

   (* Assign the pdg codes *)
   plist = AssignPDGCodes[plist];

   (* Write results to the LogFile *)
   If[Length[GenInt$LogFile] == 0, 
      AppendTo[GenInt$LogFile, "   * All particles are ok."]
      ];
   AppendTo[GenInt$LogFile, ""];
   (* We now check whether all required pdg codes are present. *)
   CheckPDGCodes[plist];


   (* Finally, masses and widths without any numerical value (i.e., NoValue[1]) are set to ZERO *)
   plist = PutUndefinedMassesToZERO[plist];

   (* Write to Logfile *)
   WriteToLogFile[GenInt$LogFileName];
   

   (* Return  and exit *)
   Return[plist];

];



(* ::Subsection:: *)
(*PutUndefinedMassesToZERO*)


(* ::Text:: *)
(*PutUndefinedMassesToZERO[ <particle list >] goes through MassList and WidthList, and looks for NoValue[1] entries. *)
(*The corresponding symbols are read out, and replace in the <particle list> by ZERO.*)


PutUndefinedMassesToZERO[particles_] := Block[{MyAspergeMasses={},

   zeromasses = Join[MassList[[2]], WidthList[[2]]]

   },

   (* Define the Asperge masses *)
   If[M$MixingsDescription=!={},
     MyAspergeMasses=DeleteCases[PDGToMass/@CalculatePDGFLR/@Flatten[(MassBasis/.#)&/@M$MixingsDescription[[All,2]]],PDGToMass[_]];
    ];


   (* Read out the mass and width symbols which have a NoValue[1] *)
   zeromasses = #[[2]]& /@ Cases[zeromasses, {a1_,a2_?(Not[MemberQ[MyAspergeMasses,#]]&),NoValue[1]}];
 
   (* Make a replacement list out of it *)
   zeromasses = Rule[#,ZERO]& /@ zeromasses;

   (* and apply it to the particle list *)
   Return[particles /. zeromasses];

];  


(* ::Section:: *)
(*Assign PDG codes*)


(* ::Subsection::Closed:: *)
(*AssignPDGCodes*)


(* ::Text:: *)
(*AssignPDGCodes[partlist] removes the tag NoPDG from partlist, and returns the new partlist.*)
(*If NoPDG is found, then the assigned PDG codes are written into the log file, and a dialog box opens in the notebook (if dialog boxes are enabled).*)


AssignPDGCodes[partlist_] := Block[{nopdgs},
    
   (* Select particles without pdg code *)
   nopdgs = Cases[partlist, {___, NoPDG[_], ___}];
   
   (* If there are particles without pdg code, then add a sentence to the GenInt Dialog and to the logfile *)
   AppendTo[GenInt$LogFile, ""];
   If[nopdgs =!= {}, 
      AppendTo[GenInt$DialogString, "- Some particles do not have assigned a PDG. Assigning automatic PDG codes.\n"];
      AppendTo[GenInt$LogFile, "# Automatically assigned PDG numbers"];
      AppendTo[GenInt$LogFile, "   * Assigned PDG number " <> ToString[#[[9, 1]]] <> " to particle " <> ToString[#[[1]]]]& /@ nopdgs
      ];

   (* Remove NoPDG tag *)
    nopdgs = partlist /. NoPDG -> Identity;

   (* Return and exit *)
    FRDebug["AssignPDGcodes", nopdgs];
    Return[nopdgs]

];


(* ::Subsection::Closed:: *)
(*CheckPDGCodes*)


(* ::Text:: *)
(*CheckPDGCodes[ partlist ] checks wether all compulsory pdg codes given in the setup are present in the model.*)
(*If not, a warning is added to the logfile.*)


CheckPDGCodes[partlist_] := Block[{pdgs = partlist[[All, 9]], miss},
    
   AppendTo[GenInt$LogFile, ""];
   AppendTo[GenInt$LogFile, "# Compulsory PDG codes:"];

   Do[miss = Complement[Rest[GenInt$CompulsoryPDG[[kk]]], pdgs];
      If[Length[miss] == 0, 
         AppendTo[GenInt$LogFile, "   * Class " <> ToString[GenInt$CompulsoryPDG[[kk,1]]] <> " complete."],
         (*else*)
         AppendTo[GenInt$LogFile, "   * Class " <> ToString[GenInt$CompulsoryPDG[[kk,1]]] <> " incomplete. " <> PYListString[miss] <> " missing."]
         ],
      {kk, Length[GenInt$CompulsoryPDG]}
      ];
];


(* ::Section:: *)
(*Create Particle Object*)


(* ::Subsection:: *)
(*CreateQNumberEntry*)


(* ::Text:: *)
(*CreateQNumberEntry[ partdef ] creates a dictionary with the QuantumNumbers for the specified particle.*)
(*The output is a matrix, each line being {"string", integer}.*)
(*Note that the electric charge is renamed form Q to "carge", and is always be put in first place.*)
(*We also make sure that the value of the Qnumber is printed in InputForm, to make sure that e.g. 2/3 is printed as "2/3".*)


CreateQNumberEntry[partdef_] := Block[{Qnums,
    mr$QNums = Union[Append[MR$QuantumNumbers, Q]] (* Make sure Q is always there by default *)
    },

    (* Create the list, and rename "Q" to "charge" *)
    Qnums = (({ToString[#], ToString[#[PartSymbol[partdef]], InputForm]}&) /@ mr$QNums)/. "Q"->"charge";
    (* Put "charge" in first place *) 
    Qnums = Prepend[DeleteCases[Qnums, {"charge", _}], Cases[Qnums, {"charge", _}][[1]]];
    
    (* Return and exit *)
    Return[Qnums];

];

    
   
   


(* ::Subsection:: *)
(*CreateParticleObjectEntry*)


(* ::Text:: *)
(*CreateParticleObjectEntry[ list ] takes a definition list for a single particle and prepends to each entry the PY dic name. *)
(*The output is a matrix of python strings and/or integers.*)
(**)
(*CD, 07.01.11: removing line from object attributes*)


CreateParticleObjectEntry[partdef_] := Block[{dicout},
   
   dicout = {{"pdg_code",     ToString[partdef[[9]]]},
             {"name",         PYString[partdef[[1]]]},
             {"antiname",     PYString[partdef[[2]]]},
             {"spin",         ToString[partdef[[3]]]},
             {"color",        ToString[partdef[[7]]]},
             {"mass",         "Param." <> ToString[partdef[[5]]]},
             {"width",        "Param." <> ToString[partdef[[6]]]},
             {"texname",      PYString[PYTeXString[partdef[[11]]]]},
             {"antitexname",  PYString[PYTeXString[partdef[[12]]]]}
            };

   (* If it is a Goldstone boson, we have to add a tag *)
    If[GoldstoneQ[PartSymbol[partdef[[1]]]],
       AppendTo[dicout, {"goldstone", "True"}]
      ];

    (* Add the Quantum numbers *)
    dicout = Join[dicout, CreateQNumberEntry[partdef[[1]]]];

    (* Return and exit *)
    FRDebug["CreateParticleDicEntry", dicout];
    Return[dicout]

];
             


(* ::Section:: *)
(*WritePYParticles*)


(* ::Subsection:: *)
(*WriteParticleObject*)


(* ::Text:: *)
(*WriteParticleObject[ list ] transforms list into the particle object definition*)
(**)
(*e__minus__ = Particle(pdg_code=3,*)
(*                            name='name',*)
(*                            antiname='antiname',*)
(*                            spin=3,*)
(*                            color='color',*)
(*                            mass='mass',*)
(*                            width='width',*)
(*                            texname='texname',*)
(*                            antitexname='antitexname',*)
(*                            line='line', *)
(*                            charge='charge').*)
(*                            *)
(*and adds a newline at the end.*)
(*list must be a matrix as provided by ParticleObjectEntry.*)


WriteParticleObject[file_, list_List] := Block[{
     classname = CreateObjectParticleName[StringReplace[list[[2,2]],"'"->""]],
     anticlassname = CreateObjectParticleName[StringReplace[list[[3,2]],"'"->""]]},

     WritePYObject[file, classname, "Particle", list];
     WriteString[file, "\n"];
     
     (* If we have an antiparticle, than we also need to declare the antiparticle *)
     If[classname =!= anticlassname,
        WriteString[file, anticlassname, " = ", classname, ".anti()\n"];
        WriteString[file, "\n"];
       ];
];

      


(* ::Subsection:: *)
(*WritePYParticles*)


(* ::Text:: *)
(*WritePYParticles is the main file that writes particles.py.*)


WritePYParticles[partlist_] := Block[{partfile, plist = partlist, outfile},

   (* Create particle python dictionary *)
   plist = CreateParticleObjectEntry /@ plist;

   (* Write particles.py*)
   DeleteFileIfExists["particles.py"];
   outfile = OpenWrite["particles.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from __future__ import division\n"];
   WriteString[outfile, "from object_library import all_particles, Particle\n"];
   WriteString[outfile, "import parameters as Param\n\n"];
   WriteString[outfile, "import propagators as Prop\n\n"];

   WriteParticleObject[outfile, #]& /@ plist;

   Close[outfile];
   TestQ[FileExistsQ, "particles.py", "   * particles.py written.", "   * particles.py not written"];
   
   (* Write the Log file *)
   WriteToLogFile[GenInt$LogFileName];

];

   

   
