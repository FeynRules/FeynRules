(* ::Package:: *)

(* ::Title:: *)
(*parameters.py*)


(* ::Section:: *)
(*Test Parameters*)


(* ::Subsection:: *)
(*ReOrderFunctions*)


(* ::Subsubsection::Closed:: *)
(*ReorderEParamEntry*)


(* ::Text:: *)
(*Reorders an external parameter from*)
(**)
(*{LHABlock, {LHANumber}, Name, Value, Type, Description}*)
(**)
(*or*)
(**)
(*{LHABlock, {LHANumber}, Name, InteractionOrder, InteractionOrderValue, Value, Type, Description}*)
(**)
(*to *)
(**)
(*{ FR name,  MC name, Real/Complex, Value, Description, texname, LHA Block, {LHA Block Entry}, {InteractionOrder}}*)


(* For no InteractionOrder *)

ReorderEParamEntry[{lhablock_, lhanumber_, name_, value_, type_, descrip_}] := {name, name /. ParamRules, type, value, descrip, ToString[name, TeXForm], lhablock, lhanumber, {}};

(* For no InteractionOrder *)

MakeInteractionOrderMatrix[ios__]:= Table[{{ios}[[2k-1]],{ios}[[2k]]},{k,Length[{ios}]/2}];
    

ReorderEParamEntry[{lhablock_, lhanumber_, name_, ios__,  value_, type_, descrip_}] := {name, name /. ParamRules, type, value, descrip, ToString[name, TeXForm], lhablock, lhanumber, MakeInteractionOrderMatrix[ios]};


(* ::Subsubsection:: *)
(*ReorderMassOrWidthEntry*)


(* ::Text:: *)
(*Reorders a mass or width from*)
(**)
(*{MASS/WIDTH, {PDG}, Name, Value}*)
(**)
(*to *)
(**)
(*{ FR name,  MC name, Real, Value,  "Mass of particle PDG", LHA Block, {LHA Block Entry}, {InteractionOrder}}*)
(**)
(*Note that the type is set to real. This might be corrected later when checking the parameters.*)


ReorderMassOrWidthEntry[{lhablock_, lhanumber_, name_, value_}] := {name, name /. ParamRules, False, value, ToString[lhablock] <> " of particle " <> ToString[lhanumber[[1]]], ToString[name, TeXForm], lhablock, lhanumber, {}};


(* ::Subsubsection:: *)
(*ReorderIParamEntry*)


(* ::Text:: *)
(*Reorders an internal parameter from*)
(**)
(*{Name, Value, Type, Description}*)
(**)
(*or*)
(**)
(*{ Name, Value, InteractionOrder, InteractionOrderValue, Type, Description}*)
(**)
(*to *)
(**)
(*{ FR name,  MC name, Real/Complex, Value, Description,  {InteractionOrder}}*)


(* For no InteractionOrder *)

ReorderIParamEntry[{name_, value_, type_, descrip_}] := {name, name /. ParamRules, type, value, descrip, ToString[name, TeXForm], {}};

(* For no InteractionOrder *)

ReorderIParamEntry[{name_, value_, ios__, type_, descrip_}] := {name, name /. ParamRules, type, value, descrip, ToString[name, TeXForm], MakeInteractionOrderMatrix[ios]};


(* ::Subsection:: *)
(*ChangeEParameterConventions*)


(* ::Text:: *)
(*ChangeEParameterConventions[ EParamList ] transforms EParamList:*)
(** It adds all external masses and widths to the list*)
(** EParamList is ordered according LHA blocks.*)
(** The new EParamList has the structure:*)
(*{External,  FR name,  MC name, Real/Complex, Value, Description, texname, LHA Block, {LHA Block Entry}, {InteractionOrder}}*)
(*{InteractionOrder may be an empty list if this parameter does not have an interaction order.*)


ChangeEParameterConventions[eparamlist_, masslist_, widthlist_] := Block[{newlist = eparamlist, neparams = Length[eparamlist],
    newmasslist = masslist[[2]], newwidthlist = widthlist[[2]],
    goldsnghosts = List[#]& /@ Join[Select[Join @@ PartList[[All,2]], #[[3]] === U &][[All,9]],Select[Join @@ PartList[[All,2]], Last[#] =!= NoGS &][[All,9]]]},
  
   (* We bring newlist to the matrix form 
      {LHABlock, {LHANumber}, Name, Value, Type, Description}
      or for parameters with interaction order defined,
      {LHABlock, {LHANumber}, Name, InteractionOrder, InteractionOrderValue, Value, Type, Description} 
    *)

    newlist = MapAt[Sequence @@ # &, #, 3]& /@ (Join @@ Table[Prepend[#, newlist[[kk, 1]]]& /@ newlist[[kk,2]], {kk, neparams}]);

    (* We now reorder this matrix according to the new convention *)
    (* We start with the parameters without interaction order *)

    newlist = ReorderEParamEntry /@ newlist;

    (* We now add the masses *)
    (* We start by removing the mass entries for goldstones and ghosts *)
    newmasslist = Select[newmasslist,Not[MemberQ[goldsnghosts, #[[1]]]]&]; 
    (** ADDED BY BENJ: removing extra mass parameters when several particles have the same masses **)
    newmasslist = Delete[newmasslist,List/@Flatten[Drop[Position[newmasslist[[All,2]],#],-1]&/@ DeleteCases[Tally[newmasslist[[All,2]]],{_,1}][[All,1]]]];
    (** END BENJ **)
(*    (* If the same symbol is used for more than one mass, only keep the first *)
Print[newmasslist];
    newmasslist = RemoveDuplicateMassesOrWidthsForUFO[newmasslist];
Print[newmasslist];*)
    (* We start by reordering them into the same order as for the eparams *)
    (* To start we write each external mass in the for {MASS, {PDG}, FR name, MC name, Value *)
    newmasslist = DeleteCases[Prepend[#, MASS]& /@ newmasslist, {__, Internal}];
    (* Then we reorder it into the required form *)
    newmasslist = ReorderMassOrWidthEntry /@ newmasslist;
    (* Finally, remove the NoPDG[ ] tags *)
    newmasslist = newmasslist /. NoPDG -> Identity;

    (* We now do the same for the width *)
    newwidthlist = Select[newwidthlist,Not[MemberQ[goldsnghosts, #[[1]]]]&]; 
(*    newwidthlist = RemoveDuplicateMassesOrWidthsForUFO[newwidthlist]*)
    newwidthlist = DeleteCases[Prepend[#, DECAY]& /@ newwidthlist, {__, Internal}];
    (* Then we reorder it into the required form *)
    newwidthlist = ReorderMassOrWidthEntry /@ newwidthlist;
    (* Finally, remove the NoPDG[ ] tags *)
    newwidthlist = newwidthlist /. NoPDG -> Identity;

    (* We now join everything together *)
     newlist = Join[newlist, newmasslist, newwidthlist];

    (* Prepend External *)
    newlist = Prepend[#, External]& /@ newlist;
   

    (* Return and exit *)
    FRDebug["ChangeEParameterConventions", newlist];
    Return[newlist]

    ];


(* ::Subsection:: *)
(*ChangeIParameterConventions*)


(* ::Text:: *)
(*ChangeIParameterConventions[ IParamList ] transforms IParamList into*)
(*{Internal,  FR name,  MC name, Real/Complex, Value, Description, {InteractionOrder}}*)
(*{InteractionOrder may be an empty list if this parameter does not have an interaction order.*)


ChangeIParameterConventions[iparamlist_] := Block[{newlist = iparamlist, neparams = Length[iparamlist]},
  
    (* We now reorder this matrix according to the new convention *)
    (* We start with the parameters without interaction order *)

    newlist = ReorderIParamEntry /@ newlist;

    (* Prepend Internal *)
    newlist = Prepend[#, Internal]& /@ newlist;

    (* Return and exit *)
    FRDebug["ChangeIParameterConventions", newlist];
    Return[newlist]

    ];


(* ::Subsection::Closed:: *)
(*PYCheckParameters*)


(* ::Text:: *)
(*CheckEParameters[ eparams ] checks that *)
(** all external parameters have a numerical value. If not, the value 1 is assigned.*)
(** the type of the numerical value (R/C) corresponds to the type given. If not, the type is changed accordingly.*)
(** Checks if a LHA block is assigned. If not, FRBlock is assigned *)


PYCheckParameters[params_] := Block[{numbers, isnumber, makedialog = False, iscomplex, newparams = params, blocks, eparams},
  
    (* Check if everything has a value *)
     numbers = {#, NumericalValue[#]}& /@ newparams[[All, 3]];
     isnumber =  {#1, NumericQ[#2]}& @@@ numbers;
     If[Not[#[[2]]], makedialog = True; AppendTo[GenInt$LogFile, "   * Parameter " <> ToString[#[[1]]] <> " has no value. Default value 1 assigned."]]& /@ isnumber;
     If[makedialog, 
        AppendTo[GenInt$Dialog, "- Some parameters do not have assigned a value. Assigning default value 1.\n"]
        ];
     makedialog = False;
 
    (* Check if every eparam has an LHA block assigned *)
    eparams = Cases[params, {External, __}];
    blocks = Transpose[List[eparams[[All, 3]], eparams[[All, 7]]]];
    If[MatchQ[#, {_, NoBlockName[_]}], makedialog = True; AppendTo[GenInt$LogFile, "   * External parameter " <> ToString[#[[1]]] <> " has no BlockName assigned. Default block FRBlock assigned."]]& /@ blocks;
    If[makedialog, 
        AppendTo[GenInt$Dialog, "- Some external parameters do not have assigned a BlockName. Assigning default block FRBlock.\n"]
        ];
     
    (* Check if complex numbers are defined as complex *)
     iscomplex = {#1, MatchQ[#2, _Complex]}& @@@ numbers;
     Do[If[iscomplex[[kk, 2]],
           If[Not[newparams[[kk, 4]]],
              AppendTo[GenInt$LogFile, "   * Parameter " <> ToString[iscomplex[[kk, 1]]] <> " is complex, but declared as real. Changing type to complex."];
              newparams[[kk]] = ReplacePart[newparams[[kk]], 4 -> True]
              ]
           ],
         {kk, Length[iscomplex]}
         ];

     (* Remove NoValue and NoBlockName tags *)
     newparams = newparams /. {NoValue|NoBlockName :> Identity};

     (* Return and exit *)
     FRDebug["CheckParameters", newparams];
     Return[newparams]

];  


(* ::Subsection:: *)
(*DuplicateMassesOrWidthsForUFO*)


RemoveDuplicateMassesOrWidthsForUFO[masslist_List] := Block[{mlist = masslist},
    mlist = GatherBy[mlist, #[[2]]&];
    mlist = First /@ mlist;
    Return[mlist];
];

    


(* ::Section:: *)
(*CheckPythonParameters*)


CheckPythonParameters[eparams_, iparams_, masslist_, widthlist_] := Block[{
   eplist = eparams, iplist = iparams, mlist = masslist, wlist = widthlist,
   plist
   },


   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# Parameter definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];
   WriteToLogFile[GenInt$LogFileName];
   GenInt$LogFile = {};
   GenInt$Dialog = {};

   (* Waterfall for internal params *)
   iplist = ParameterWaterfall[iplist];

   (* Change conventions *)
   eplist = ChangeEParameterConventions[eplist, mlist, wlist];
   iplist = ChangeIParameterConventions[iplist];



   plist = Join[eplist, iplist];

   (* Check parameters*)
   plist = PYCheckParameters[plist];


   (* Write results to the LogFile *)
   If[Length[GenInt$LogFile] == 0, 
      AppendTo[GenInt$LogFile, "   * All parameters are ok."]
      ];
   AppendTo[GenInt$LogFile, ""];

   (* Write the Log file *)
   WriteToLogFile[GenInt$LogFileName];

   (* Return and exit *)
   Return[plist];

];


(* ::Section:: *)
(*Waterfall *)


(* ::Text:: *)
(*Here we give the routine that orders the parameters for the output. The order is*)
(**)
(*1) all external parameters, in arbitrary order.*)
(*2) internal parameters, such that the one at position i does not depend on those at position j > i.*)


(* ::Subsection::Closed:: *)
(*ParameterWaterfall*)


(* ::Text:: *)
(*ParameterWaterfall[eparams,  iparams ] reorders the params, and returns the combined list.*)


ParameterWaterfall[iparams_] := Block[{testfunc,
    newparams = {}, oldparams = iparams},

   (* We select always those that are not dependent on the rest *)
   While[oldparams =!= {},
         newparams = Join[newparams, Select[oldparams, And @@ Table[FreeQ[#[[2]], oldparams[[kk, 1]]], {kk, Length[oldparams]}]&]];
         oldparams = Complement[oldparams, newparams];
         ];

   (* Return and exit *)
   Return[newparams];

];


(* ::Section:: *)
(*Create the parameter object*)


(* ::Subsection::Closed:: *)
(*ToPYForm*)


(* ::Text:: *)
(*ToPYForm[ paramlist ] transforms the Value of all parameters such tht *)
(** internal parameters are in PythonForm*)
(** external parameters correspond to the agreed format.*)


ParamToPYForm[params_] := Block[{eparams, iparams, newparams},
   
   eparams = Cases[params, {External, __}];
   iparams = Cases[params, {Internal, __}];

   eparams = MapAt[ToString[#, CForm]&, #, 5]& /@ eparams;
   iparams = MapAt[PYString[PythonForm[#]]&, #, 5]& /@ iparams;

   newparams = Join[eparams, iparams];

   (* Return and Exit *)
   FRDebug["ParamToPYForm", newparams];
   Return[newparams]

];
   


(* ::Subsection:: *)
(*CreateParameterObjectEntry*)


(* ::Text:: *)
(*CreateEParameterObjectEntry[ list ] takes a definition list for a single eparam and prepends to each entry the PY dic name. *)
(*The output is a matrix of python strings and/or integers.*)


CreateParameterObjectEntry[paramdef_] := Block[{dicout},
   
   dicout = {{"name",         PYString[MakeString[paramdef[[3]]]]},
             {"nature",       paramdef[[1]]} /. {External -> "'external'", Internal -> "'internal'"},
             {"type",         If[paramdef[[4]], "'complex'", "'real'"]},
             {"value",        paramdef[[5]]},
             {"texname",      PYString[PYTeXString[paramdef[[7]]]]}};

    (* Add the LHA information for external parameters *)
    If[paramdef[[1]] === External,
       dicout = Join[dicout, {{"lhablock", PYString[MakeString[paramdef[[8]]]]}, {"lhacode", PYList[paramdef[[9]]]}}];
       ];

    (* Return and exit *)
    FRDebug["CreateParticleObjectEntry", dicout];
    Return[dicout]

];
             


(* ::Subsection:: *)
(*CreateCTParameterObjectEntry*)


(* ::Text:: *)
(*CreateCTParameterObjectEntry[ list ] takes a definition list for a single eparam and prepends to each entry the PY dic name. *)
(*The output is a matrix of python strings and/or integers.*)


CreateCTParameterObjectEntry[paramdef_] := Block[{dicout},
   
   dicout = {{"name",         PYString[MakeString[paramdef[[1]]]]},
             {"type",         "'complex'"},
             {"value",        paramdef[[2]]},
             {"texname",      PYString[PYTeXString[paramdef[[1]]]]}};

    (* Return and exit *)
    FRDebug["CreateCTParticleObjectEntry", dicout];
    Return[dicout]

];
             


(* ::Subsection:: *)
(*WriteParameterObject*)


(* ::Text:: *)
(*WriteParameterObject[ list ] transforms list into the particle object definition*)
(**)
(*aS = Particle(name = 'name',*)
(*                                 nature = 'nature'*)
(*                                 type = 'type'*)
(*                                 value = 'value'*)
(*                                 texname = 'texname'*)
(*                                 lhablock = 'lhablock'*)
(*                                 lhacode = 'lhacode').*)
(*                            *)
(*and adds a newline at the end.*)
(*list must be a matrix as provided by ParticleObjectEntry.*)


WriteParameterObject[file_, list_List] := Block[{
     classname = CreateObjectParticleName[list[[1,2]]]},

     WritePYObject[file, classname, "Parameter", list];
     WriteString[file, "\n"];
];

      


(* ::Section:: *)
(*WritePYParameters*)


(* ::Text:: *)
(*WritePYParameters is the main file that writes parameters.py.*)


WritePYParameters[pars_, isNLO_:False] := Block[{paramfile, plist = pars, outfile,
   zeroobject = {{"name",   "'ZERO'"},
                 {"nature", "'internal'"},
                 {"type",   "'real'"},
                 {"value",  "'0.0'"},
                 {"texname","'0'"}},
   MURobject  = {{"name",     "'MU_R'"},
                 {"nature",   "'external'"},
                 {"type",     "'real'"},
                 {"value",    "91.188"},
                 {"texname",  "'\\\\text{\\\\mu_r}'"},
                 {"lhablock", "'LOOP'"},
                 {"lhacode",  "[1]"}}
   },

   
   (* Create parameter python dictionary *)
   plist = ParamToPYForm[plist];
   plist = CreateParameterObjectEntry /@ plist;

   (* Write parameters.py*)
   DeleteFileIfExists["parameters.py"];
   outfile = OpenWrite["parameters.py"];
   WritePYFRHeader[outfile];
   WriteString[outfile, "\nfrom object_library import all_parameters, Parameter\n\n"];
   WriteString[outfile, "\nfrom function_library import ", Sequence @@ Riffle[PY$NewCMathFunctions, ", "], "\n\n"];

   WritePYCommentLine[outfile, "This is a default parameter object representing 0."];
   WritePYObject[outfile, "ZERO", "Parameter", zeroobject];
   WriteString[outfile, "\n"];
   If[isNLO,
      WritePYCommentLine[outfile, "This is a default parameter object representing the renormalization scale (MU_R)."];
      WritePYObject[outfile, "MU_R", "Parameter", MURobject];
      WriteString[outfile, "\n"];
     ];
   WritePYCommentLine[outfile, "User-defined parameters."];


   WriteParameterObject[outfile, #]& /@ plist;
   Close[outfile];
   TestQ[FileExistsQ, "parameters.py", "   * parameters.py written.", "   * parameters.py not written"];
   
   (* Write the Log file *)
   WriteToLogFile[GenInt$LogFileName];

];

   

   
