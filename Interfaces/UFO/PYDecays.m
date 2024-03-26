(* ::Package:: *)

(* ::Title:: *)
(*UFO decays*)


(* ::Section:: *)
(*GetUFODecays*)


(* ::Subsection:: *)
(*Helper functions*)


GetUFODecayChannel[{{x_, p1_, p2_}, m2_}] := {x, {p1, p2}, m2};


ExtractDecayingParticle[list_List] := Block[{
    particle = list[[1,1]],
    output
    },
    output = Rest /@ list;

    Return[Prepend[output, particle]];
];



(* ::Subsection:: *)
(*Main*)


(* ::Text:: *)
(*GetUFODecays[ vertices ] computes all the decays in the vertex list vertices and returns them in the format*)
(**)
(*{*)
(*....,*)
(*{X , { {p1, p2},  M2}, ...},*)
(* ....*)
(* }*)
(* *)
(* where X is ingoing and p1 and p2 are outgoing.*)


Options[GetUFODecays] = {SimplifyDecays -> False};


GetUFODecays[vertices_List, OptionsPattern[]] := Block[{
    verts=vertices,
    decays,
    simplify = OptionValue[SimplifyDecays]
    },

    decays = Timing[CalculateM2Decays[verts]]; 
    Print["Squared matrix elent compute in ", decays[[1]], " seconds."]; decays=decays[[2]];
   
    decays = Timing[ComputeDecays[decays, Simplify -> simplify, ProgressIndicator -> False]];
    Print["Decay widths computed in ", decays[[1]], " seconds."]; decays=decays[[2]];

    (* Combine into channels, and remove antiparticles that are in the initial state *)
    decays = GatherByFirstElement[GetUFODecayChannel /@ decays];
    decays = DeleteCases[ExtractDecayingParticle /@ decays, {_,0}];

    Return[decays];
];
    
    


(* ::Section:: *)
(*CreatePYDecayChannel*)


(* ::Text:: *)
(*CreatePYDecayChannel[{p1, p2, ...}, M2]  returns the string*)
(**)
(*(p1, p2, ...):M2*)
(**)
(*in Python form*)


CreatePYDecayChannel[finalstate_List, m2_] := Block[{
    final = ("P." <> CreateObjectParticleName[PartNameMG[#]])& /@finalstate,
    matrixelement = PythonForm[m2]
    },
    final = "(" <> Riffle[final, ","]<>")";

    Return[PYDicEntry[final, PYString[matrixelement]]]
];
     


(* ::Section:: *)
(*CreateDecayObjectEntry*)


CreateDecayObjectEntry[incoming_, channels__] := Block[{
   inpart = CreateObjectParticleName[PartNameMG[incoming]],
   chans = CreatePYDecayChannel @@@ {channels},
   attributes, 
   first, last, bulk, allchannels,
   object 
   },

   object = "Decay_" <> CreateObjectParticleName[PartNameMG[incoming]];
   inpart = "P." <> inpart;

   If[Length[chans] == 1,
      allchannels = {{"partial_widths", PY$DicFirstLast @@ chans}},
      (* else *)
      first = {PY$DicFirst[chans[[1]]]};
      last  = {PY$DicLast[chans[[-1]]]};
      bulk  = PY$Dic /@ Most[Rest[chans]];
      allchannels = {"partial_widths", #}& /@ Join[first, bulk, last];
      ];

   attributes = {{"name",     PYString[object]},
                 {"particle", inpart},
                 Sequence @@ allchannels};

   Return[{object, attributes}];

];

   

   


(* ::Section:: *)
(*WriteDecayObject*)


WriteDecayObject[file_, {object_, atts_List}] := Block[{
     },

     WritePYObject[file, object, "Decay", atts];
     WriteString[file, "\n"];

];
   


(* ::Section:: *)
(*WriteUFODecays*)


WriteUFODecays[vertlist_] := Block[{outfile, verts},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# Decay definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write decays.py *)
   DeleteFileIfExists["decays.py"];
   outfile = OpenWrite["decays.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_decays, Decay\n"];
   WriteString[outfile, "import particles as P\n"];
   WriteString[outfile, "\n\n"];

   verts = CreateDecayObjectEntry @@@ vertlist;
   WriteDecayObject[outfile, #]& /@ verts;
   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[vertlist] == 1, "1 decay", ToString[Length[vertlist]] <> " decays"] <> " written."];
   TestQ[FileExistsQ, "decay.py", "   * decay.py written.", "   * decay.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 
