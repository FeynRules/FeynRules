(* ::Package:: *)

(* ::Title:: *)
(*UFO@NLO*)


(* ::Section:: *)
(**)


GetIntOrder[IPL[__]] := 0;
GetIntOrder[PYR2UVTag[__]] := 0;


(* ::Section:: *)
(*Tagging*)


(* ::Text:: *)
(*AddPYR2UVTag[ {particles, expr}, "tag"] returns {particles, PYR2UVTag["tag"]}*)


AddPYR2UVTag[{particles_, vert_}, tag_String] := {particles, Expand[PYR2UVTag[tag]*vert]};


TagCollector[a_, sum_Plus] := TagCollector[a, #]& /@ sum;
TagCollector[a_, b_ PYR2UVTag[c_]] := TagCollector[a PYR2UVTag[c], b];


PullOutTagAndSum[lim_List] := {lim[[1,1]], Plus @@ (Last/@ lim)};


CollectByTag[expi_] := Block[{
   DUM$NULL,
   tempexpi = Expand[expi + DUM$NULL*(1+PYR2UVTag["R2"]+PYR2UVTag["UV"])]
   },

   tempexpi = List @@ TagCollector[1,tempexpi];
   tempexpi = GatherByFirstElement[tempexpi];

   tempexpi = PullOutTagAndSum /@ tempexpi;

   tempexpi = tempexpi /. DUM$NULL -> 0;

   Return[tempexpi];

];
   


CollectCouplingsByTag[CouplingObject[name_, coupl_]] := Block[{
    LOCoupl,
    R2Coupl,
    UVCoupl,
    coupli = Expand[coupl]
    },

    coupli = CollectByTag[coupli];
    LOCoupl = Cases[coupli, {1, _}][[1,2]];
    R2Coupl = Cases[coupli, {PYR2UVTag["R2"], _}][[1,2]];
    UVCoupl = Cases[coupli, {PYR2UVTag["UV"], _}][[1,2]];

    Return[{CouplingObject[name, LOCoupl], R2CouplingObject[name, R2Coupl], UVCouplingObject[name, UVCoupl]}];
]; 


CollectCouplingsByTag[list_List] := CollectCouplingsByTag /@ list;


(* ::Section:: *)
(*BuildPYR2Vertices*)


CleanZeroPYVertices[{parti_List, rest__}] := Block[{
    restlist = {rest}
    },

    restlist = DeleteCases[CleanZeroPYIndividualVertices /@ restlist, 0];
    If[restlist === {},
       Return[0],
       Return[Prepend[restlist, parti]]
      ];
];

    


CleanZeroPYIndividualVertices[{verts__, last_}] := Block[{ 
    cc = DeleteCases[last, 0]
    },
    
    If[cc === {},
       Return[0],
       Return[{verts, cc}]
       ];
];


BuildPYR2UVVertices[vertexlist_List, couplings_List, tag_:"1"] := Block[{
     coupls, zerocouplrules,
     cleanvertices
     },

    (* First we read out the couplings *)
    (* If the tag is "R2", we need the second line of the coupling matrix 
       If the tag is "UV", the 3rd, if "1" the first *)
    coupls = Which[tag === "1",  couplings[[1]],
                   tag === "R2", couplings[[2]],
                   tag === "UV", couplings[[3]]
                  ];
 
    (* Take those that are zero *)
    zerocouplrules = Cases[coupls, _[_,0]];
    coupls = Complement[coupls, zerocouplrules];
    zerocouplrules = Rule @@@ zerocouplrules;

    (* Clean the vertices from zero couplings *)
    cleanvertices = DeleteCases[CleanZeroPYVertices/@(vertexlist //. zerocouplrules), 0];

    Return[{cleanvertices, coupls}];
];

    
             
     


(* ::Section:: *)
(*Loop particles*)


SetAttributes[PYLoopParticles, Orderless];


CreatePYLoopParticles[loopparts_List, repllist_:FeynArtsToFeynRulesParticles] := CreatePYLoopParticles[loopparts, repllist] = Block[{
   loopy = loopparts
   },

   loopy = loopy //. {{a_. S[mm_Integer,_],nn_Integer}:>{a S[mm],nn},
                            {a_. F[mm_Integer,_],nn_Integer}:>{a F[mm],nn},
                            {a_. V[mm_Integer,_],nn_Integer}:>{a V[mm],nn},
                            {a_. R[mm_Integer,_],nn_Integer}:>{a R[mm],nn},
                            {a_. T[mm_Integer,_],nn_Integer}:>{a T[mm],nn},
                            {a_. U[mm_Integer,_],nn_Integer}:>{a U[mm],nn}
                           };

   loopy = loopy //. repllist;

   (* We do not need antiparticle distinctions. 
      Indeed, if a certain particle can not appear in a loop, 
      its antiparticle cannot either
    *)
   loopy = If[AntiFieldQ[#] === True, anti[#], #]& /@ loopy;
   (* Remove double entries, and sort into canonical order *)
   loopy = Union[loopy];
   loopy = PYLoopParticles @@ loopy;


   Return[loopy];
];
   


(* ::Subsection:: *)
(*GatherByLoopParticles*)


SumByLoopParticles[ListOfCoupls_List] := Block[{
   first = ListOfCoupls[[1,1]],
   summands = ListOfCoupls[[All,2]]
   },
 
   summands = Plus @@ summands;
   Return[{first, summands}];
];


LoopParticlesCollector[lops_, sum_Plus] := LoopParticlesCollector[lops, #]& /@ sum;
LoopParticlesCollector[lops_, nlocoupl_*lops1_PYLoopParticles] := LoopParticlesCollector[lops*lops1, nlocoupl];


InventNewNameForR2UVCouplingObject[listi_List, r2uvtag_String, counter_Integer] := Block[{
    fulllist = Last /@ listi,
    newname
    },


    newname = r2uvtag <> fulllist[[1,1]] <> "_" <> ToString[counter];
    fulllist = Rule[#, newname]& /@ fulllist;

    Return[fulllist];
];

    


CreateNewNamesForR2UVCouplingObject[listcoupl_List, r2uvtag_String] := Block[{
   allobjs = {#[[3]], List@@#[[1;;2]]}& /@ listcoupl
   },

   allobjs = GatherByFirstElement[allobjs];


  
   allobjs = Flatten[Table[InventNewNameForR2UVCouplingObject[allobjs[[iii]], r2uvtag, iii], {iii, Length[allobjs]}]];

   Return[allobjs];
];
   
   

    


GatherByLoopParticles[couplingobject_] := Block[{
   head = Head[couplingobject],
   name = couplingobject[[1]],
   couplingconst = Expand[couplingobject[[2]]],
   r2uvtype
   },

   r2uvtype = If[head === R2CouplingObject, "R2", "UV"];

   (* split according to the loop particles *)
   couplingconst = LoopParticlesCollector[1, couplingconst];
   couplingconst = If[Head[couplingconst] === Plus,
                      List @@ couplingconst,
                      {couplingconst}
                      ];
   couplingconst = GatherByFirstElement[couplingconst];
   couplingconst = SumByLoopParticles /@ couplingconst;

   couplingconst = head[name, ##]& @@@ couplingconst;   

   Return[couplingconst];
];

   


RenameNLOCoupling[elem_, repl_] := Block[{
   head = Head[elem],
   oldname = elem[[1]],
   lops = elem[[2]],
   analytic = elem[[3]],
   newname, MyRule
   },

   newname = Cases[MyRule @@@ repl, MyRule[{oldname, lops},_]][[1,2]];
  
   Return[head[oldname, lops, newname, analytic]];

];


(* ::Section:: *)
(*AddLoopParticlesToVertex*)


AddLoopParticlesToVertex[{particles_List, rest__}, coupllist_List, tag_String] := Block[{
    restlist = {rest}
    },

    restlist = Flatten[AddLoopParticlesToVertexTerm[#, coupllist]& /@ restlist] /. dumbo -> List;
    restlist= Join @@ restlist;
  
    (* We need to collect couplings for which color, Lorentz and loop particles are identical,
       because they only differ by an interaction order *)
    restlist = GatherByFirstElement[{#[[1;;3]], #[[4]]}& /@ restlist];
    restlist = Append[#[[1,1]], Last/@#]& /@ restlist;

    Return[Join[{particles, tag}, restlist]];

   ];


AddLoopParticlesToVertexTerm[{colorstruc_, lorentzstruc_, couplingstruclist_List}, coupllist_List] := Block[{
   allcoups
   },
   allcoups = Reverse[Most[Rest[#]]]& /@ Flatten[Table[Select[coupllist, Not[FreeQ[#, elem]]&], {elem, couplingstruclist}]];
   allcoups = dumbo[colorstruc, lorentzstruc, Sort[Last /@ #], #[[1,1]]]& /@ GatherByFirstElement[allcoups];
   allcoups = dumbo @@ allcoups; 
   

   Return[allcoups];

];


(* ::Section:: *)
(*ProcessNLOVertices*)


ProcessNLOVertices[R2verts_List, R2coupls_List, UVverts_List, UVcoupls_List] := Block[{
   R2V = R2verts,
   R2C = R2coupls, 
   UVV = UVverts,
   UVC = UVcoupls,
   optimizedrenamingrulesR2, optimizedrenamingrulesUV
   },


   (* Rename the internal loop particles in the couplings *)
   R2C = R2C //. IPL -> CreatePYLoopParticles;
   UVC = UVC //. IPL -> CreatePYLoopParticles;



   (* Collect couplings according to loop particles *)
   R2C = Flatten[GatherByLoopParticles/@ R2C];
   UVC = Flatten[GatherByLoopParticles /@ UVC];


   (* Build the replacement list *)
   PYNLO$R2CouplingRename = CreateNewNamesForR2UVCouplingObject[R2C, "R2"];
   PYNLO$UVCouplingRename = CreateNewNamesForR2UVCouplingObject[UVC, "UV"]; 

   (* Optimize renaming *)
   

   (* Rename *)
   R2C = RenameNLOCoupling[#, PYNLO$R2CouplingRename]& /@ R2C;
   UVC = RenameNLOCoupling[#, PYNLO$UVCouplingRename]& /@ UVC;



   (* Modify the vertices to pull out the loop particles
      then merge the two lists, after adding the tag *)
   R2V = AddLoopParticlesToVertex[#, R2C, "R2"]& /@ R2V;
   UVV = AddLoopParticlesToVertex[#, UVC, "UV"]& /@ UVV;


   R2V = Join[R2V, UVV];

   (* Finally, drop the superfluous entries for the coupling objects *)
   R2C = Drop[#, 2]& /@ R2C;
   UVC = Drop[#, 2]& /@ UVC;
   R2C = KillDoubles[Join[R2C, UVC]];

   

   Return[{R2V, R2C}];
];
   
   


(* ::Section:: *)
(*WritePYCTParameters*)


(* ::Text:: *)
(*WritePYCTParameters[ list ] writes all the couplings in list to CT_parameters.py*)


WritePYCTParamters[list_] := Block[{outfile},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# CTCoupling definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write couplings.py *)
   DeleteFileIfExists["CT_parameters.py"];
   outfile = OpenWrite["CT_parameters.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_CTparameters, CTParameter\n"];
   WriteString[outfile, "\nfrom function_library import ", Sequence @@ Riffle[PY$NewCMathFunctions, ", "], "\n\n"];
   WriteString[outfile, "\n\n"];


   WriteParameterObject[outfile, #]& /@ (CreateCTParamterObjectEntry /@ list);
   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[list] == 1, "1 CTparameter", ToString[Length[list]] <> " CTparameters"] <> " written."];
   TestQ[FileExistsQ, "CT_parameters.py", "   * CT_parameters.py written.", "   * CT_parameters.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 


(* ::Section:: *)
(*WritePYCTCouplings*)


(* ::Text:: *)
(*WritePYCTCouplings[ list ] writes all the vertices in list to CT_couplings.py*)


WritePYCTCouplings[list_] := Block[{outfile},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# CTCoupling definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write couplings.py *)
   DeleteFileIfExists["CT_couplings.py"];
   outfile = OpenWrite["CT_couplings.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_couplings, Coupling\n"];
   WriteString[outfile, "\nfrom function_library import ", Sequence @@ Riffle[PY$NewCMathFunctions, ", "], "\n\n"];
   WriteString[outfile, "\n\n"];

(*Print["In WPYCTCoupl"];
  Print[InputForm[list]];Print[InputForm[CreateCouplingObjectEntry /@ list]];*)

   WriteCouplingObject[outfile, #]& /@ (CreateCouplingObjectEntry /@ list);
   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[list] == 1, "1 CTcoupling", ToString[Length[list]] <> " CTcouplings"] <> " written."];
   TestQ[FileExistsQ, "CT_couplings.py", "   * CT_couplings.py written.", "   * CT_couplings.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 


(* ::Section:: *)
(*WritePYCTVertices*)


(* ::Subsection:: *)
(*Python form for loop particles*)


SetAttributes[PythonFormatLoopParticles, Listable];

PythonFormatLoopParticles[symb_Symbol] := StringJoin["P." <> CreateObjectParticleName[PartNameMG[symb]]];


PYFormLoopParticles[listi_List] := StringReplace[PYList[PythonFormatLoopParticles[listi //. PYLoopParticles -> List]],{"{"->"[","}"->"]"}];


(* ::Subsection:: *)
(*CreateCTVertexObjectEntry*)


(* ::Text:: *)
(*CreateVertexObjectEntry[ list] takes a definition for a single vertex and transforms into the format required by for the Python vertex objects.*)


CreateCTVertexObjectEntry[list_, {counter_Integer}] := Block[{
     particles = list[[1, All, 1]],
     type = list[[2]],
     colors, loops,
     lorentz = Union[#[[2]]& /@ list[[3;;]]],
     coupl = list[[3;;]],
     lorentzreverse, colorreverse, loopsreverse,
     pyvertex
    },

   (* Remove the CC statements, and the bar for Majoranas, 
      then introduce the particle names, and finally 
      check if we need new Particle object names.
    *)

    particles = MakeIdenticalFermions[particles];
    particles = CreateObjectParticleName[PartNameMG[#]]& /@ particles;


    (* Now, create the color structure strings *)
    coupl = {PythonForm[#1], #2, PYFormLoopParticles[#3], #4}& @@@ coupl;
    colors = Union[First /@ coupl];
    loops = Union[#[[3]]& /@ coupl];
    
    (* We now build replacement rules to identify back the Lorentz and color structures,
       and how they are convolved.
     *)
    lorentzreverse = Rule @@@ MapIndexed[Join, List /@ lorentz];
    colorreverse = Rule @@@ MapIndexed[Join, List /@ colors];
    loopsreverse = Rule @@@ MapIndexed[Join, List /@ loops];

    (* And we then apply these reverse replacements to coupl *)
    coupl = coupl /. colorreverse /. lorentzreverse /. loopsreverse;

    (* Finally, since a list in Python starts with 0, and not one, we 
       have to rescale everything by one unit 
     *)
     coupl = {#1-1, #2-1, #3-1, #4}& @@@ coupl;

     (* Add the tags P., L., and C. to the particle, lorentz and coupling objects *)
     particles = ("P." <> # &) /@ particles;
     lorentz = ("L." <> # &) /@ lorentz;
     coupl = {#1, #2, #3, "C." <> #& /@ #4}& @@@ coupl;


     (* We now construct the output object *)
     pyvertex = {{"name",           PYString["V_" <> ToString[counter]]},
                 {"type",           PYString[type]},
                 {"particles",      PYList[particles]},
                 {"color",          PYList[PYString /@ colors]},
                 {"lorentz",        PYList[lorentz]},
                 {"loop_particles", PYList[loops]},
                 {"couplings",      PYShortDictionary[CreateCouplingEntry /@ coupl]}
                 };

     (* Return and exit *)
     Return[pyvertex];
                 
];

    


(* ::Section:: *)
(*WriteCTVertexObject*)


(* ::Text:: *)
(*WriteVertexCTObject[file, vertex, {i}]  write the vertex, which is an output of CreateCTVertexObjectEntry,*)
(*to file. The vertex object is given the name V_i.*)


WriteCTVertexObject[file_, vertex_List] := Block[{
     name = StringReplace[vertex[[1,2]], {"'" -> ""}]
     },

     WritePYObject[file, name, "CTVertex", vertex];
     WriteString[file, "\n"];

];
   


(* ::Section:: *)
(*WritePYCTVertices*)


(* ::Text:: *)
(*WritePYCTVertices[ list ] writes all the vertices in list to vertices.py*)


WritePYCTVertices[vertlist_] := Block[{outfile},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# CTVertex definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write vertices.py *)
   DeleteFileIfExists["CT_vertices.py"];
   outfile = OpenWrite["CT_vertices.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_vertices, all_CTvertices, Vertex, CTVertex\n"];
   WriteString[outfile, "import particles as P\n"];
   WriteString[outfile, "import CT_couplings as C\n"];
   WriteString[outfile, "import lorentz as L\n"];
   WriteString[outfile, "\n\n"];

   WriteCTVertexObject[outfile, #1]& /@ MapIndexed[CreateCTVertexObjectEntry, vertlist];
   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[vertlist] == 1, "1 CTvertex", ToString[Length[vertlist]] <> " CTvertices"] <> " written."];
   TestQ[FileExistsQ, "CT_vertices.py", "   * CT_vertices.py written.", "   * CT_vertices.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 


(* ::Section:: *)
(*Massless particles*)


(* ::Text:: *)
(*This routine goes through the list of UV counterterms *after* flavor expansion, and looks at the two-point counterterms.*)
(*If the two particles are identical (or conjugate), then we project onto MSbar.*)
(**)
(*Note that we still need to figure out what happens with mixing terms that involve a massive and a massless particle!*)


ProjectMasslessOntoMSBar[uvlist_List] := ProjectMasslessCountertermOntoMSBar /@ uvlist;


ProjectMasslessCountertermOntoMSBar[uvcounterterm_] := Block[{
   particles = uvcounterterm[[1]],
   counterterm = uvcounterterm[[2]],
   uvterm, rest,
   masses
   },

   (* We only look at two-point UV functions *)
   If[(Length[uvcounterterm[[1]]] > 2) || FreeQ[counterterm, PYR2UVTag["UV"]],
      Return[uvcounterterm]
     ];

   (* we only look at massless particles *)
   masses = Mass[#[[1]]]& /@ particles;
   If[masses =!= {0,0},
      Return[uvcounterterm]
     ];

   counterterm = Expand[counterterm];
   uvterm = Expand[Coefficient[counterterm, PYR2UVTag["UV"],1]*PYR2UVTag["UV"]];
   rest = Expand[counterterm - uvterm];
   uvterm = Expand[Coefficient[uvterm, FR$Eps, -1]/FR$Eps];
   counterterm = uvterm+rest;
   
   Return[{particles, counterterm}];

];

   


(* ::Section:: *)
(*WritePYAssumptions*)


CreateAssumptionObject[string_String, {i_}]:= AssumptionObject["Assumption"<>ToString[i], "Assumption",
     {{"name", PYString["Assumption"<>ToString[i]]},
      {"condition", PYString[string]},
      {"validity", "'nlo'"}}
     ];

   


WriteOutAssumptionObject[name_String,ass_] := Block[{
    file = OpenAppend[name]
    },

    WritePYObject[name,Sequence @@ ass];
    WriteString[file,"\n\n"];
    Close[file];
];


WritePYAssumptions[list_List] := Block[{asslist = ToString /@ list,
    file},

    asslist = MapIndexed[CreateAssumptionObject, asslist];
    
    DeleteFileIfExists["assumptions.py"];
    file = OpenWrite["assumptions.py"];
    WritePYFRHeader["assumptions.py"];
    WriteString[file, "from object_library import all_assumptions, Assumption\n\n\n"];
    Close[file];

    WriteOutAssumptionObject["assumptions.py", #]& /@ asslist;
];
  
    


   
