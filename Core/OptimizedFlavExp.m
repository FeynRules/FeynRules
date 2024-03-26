(* ::Package:: *)

(* ::Title:: *)
(*Optimized FlavorExpansion*)


(* ::Section:: *)
(*Messages*)


BuildFlavorExpLookUp::oneind = "More than one flavor index for a particle class.";
OptFlavExp::FR$AbbIndexSum = "FR$AbbIndexSum is not empty. Risk of conflict between definitions.";


(* ::Section::Closed:: *)
(*MR$DefinitionsNoFields*)


MR$DefinitionsNoFields := (* MR$DefinitionsNoFields= *)Dispatch[Select[MR$Definitions,FreeQ[#,_?FieldQ]&]];


(* ::Section::Closed:: *)
(*FindClassFlavor*)


(* ::Text:: *)
(*FindClassFlavor[ class ] returns the name of the flavor index of the class < class >. *)
(*If the class is not flavored, MR$Null is returned.*)


FindClassFlavor[class_] := Block[{flavind},
 
  flavind = If[Not[PRIVATE`FlavoredQ[class]], 
               MR$Null,
               (*else*)
               Intersection[($IndList[class] /. Index -> Identity), MR$FlavorList][[1]]
               ];

  (* Return and exit *)
   FRDebug["FindClassFlavor[ " <> ToString[class] <> " ]:", MR$Null];
   Return[flavind]
];
  


(* ::Subsection::Closed:: *)
(*Class - Flavor Mapping*)


(* ::Text:: *)
(*We create an extension of $ClassMembers, which also includes antifields and cc-fields.*)


ClassMemberList[field_] := PRIVATE`$ClassMembers[field] /; (Head[field] =!= CC) && Not[AntiFieldQ[field] === True];

ClassMemberList[field_] := anti[ClassMemberList[anti[field]]] /; (Head[field] =!= CC) && AntiFieldQ[field] === True;

ClassMemberList[CC[field_]] := CC /@ ClassMemberList[field];


(* ::Text:: *)
(*We also create an extension of FlavoredQ to include CC-fields*)


PRIVATE`FlavoredQ[CC[field_]] := PRIVATE`FlavoredQ[field];


(* ::Section::Closed:: *)
(*FR$FlavorExpLookUp (added spin 3/2)*)


(* ::Text:: *)
(*In here we build a look up table (replacement rule list) that connects the classes fields to the class members*)
(**)
(*This variable is initialized to an empty List*)


If[Not[ValueQ[FR$FlavorExpLookUp]],
   FR$FlavorExpLookUp = {}
];


(* ::Subsection::Closed:: *)
(*BuildFlavorExpLookUpForOneParticle*)


(* ::Text:: *)
(*BuildFlavorExpLookUpForOneParticle[ class ] return a list { class[1]  ->  member1, ... } for the class class.*)


BuildFlavorExpLookUpForOneParticle[class_] := Block[{flavind, flavindlist, MakeRule},
 
   (* We have to determine the flavor index for this particle *)
   flavind = {FindClassFlavor[class]};

   (* There should only be one flavor index. If not, abort, otherwise convert list to symbol *)
   If[Length[flavind] != 1,
      Message[BuildFlavorExpLookUp::oneind]; Abort[],
      (*else*)
      flavind = flavind[[1]]
    ];
   

   (* Create the list {class[1] -> member1, class[2] -> member2, ... } *)
   flavindlist = (MakeRule[class[#], ClassMemberList[class][[#]]]& /@ (IndexRange[Index[flavind]] /. NoUnfold -> Identity)) /. MakeRule -> Rule;

   (* Return and exit *)
   FRDebug[" FlavExpLookUp for " <> ToString[class], flavindlist];
   Return[flavindlist]

];

   


(* ::Subsection::Closed:: *)
(*BuildFlavorExpLookUp*)


(* ::Text:: *)
(*BuildFlavorExpLookUp[] builds FR$FlavorExpLookUp.*)


BuildFlavorExpLookUp[] := Block[{flavclasses, flavlist},

   (* Build the list of flavored classes *)
   flavclasses = Select[MR$ClassesList/.PRIVATE`FA$ClassToName, PRIVATE`FlavoredQ];
  
   (* We also need the antifields in this list *)
   flavclasses = Union[flavclasses, anti[flavclasses]];

   (* and for Dirac fermions also the CC fields *)
   flavclasses = Union[flavclasses, CC /@ Select[flavclasses, DiracFieldQ[#] === True&]];
   
   (* and for Complex spin 3/2 fermions also the CC fields *)
   flavclasses = Union[flavclasses, CC /@ Select[flavclasses, CSpin32FieldQ[#] === True&]];
   
   (* Create the list *)
    flavlist = Join @@ (BuildFlavorExpLookUpForOneParticle /@ flavclasses);

   (* Return and exit *)
   FRDebug["BuildFlavorExpLookUp : ", flavlist];
   Return[flavlist]

];

   

   


(* ::Section::Closed:: *)
(*FlavorBooleans*)


(* ::Subsection:: *)
(*FlavoredParticleQ*)


(* ::Text:: *)
(*FlavoredParticleQ[ { particle, i }] return FlavoredQ[ particle ]*)


FlavoredParticleQ[{part_Symbol, _}] := PRIVATE`FlavoredQ[part];


(* ::Text:: *)
(*FlavoredParticleQ[ { partList1, partList2, ...} ]   returns True if at least one of the plists is flavored*)


FlavoredParticleQ[ {plist1_List, plists__} ] := Or @@ (FlavoredParticleQ /@ {plist1, plists});


(* ::Section::Closed:: *)
(*ExtractNonFlavoredVertices*)


(* ::Text:: *)
(*ExtractNonFlavoredVertices[ vertices ] extract all vertices from < vertices > that do not depend on any flavored field.*)


ExtractNonFlavoredVertices[ vertices_List ] := Select[vertices, Not[FlavoredParticleQ[#[[1]]]]&];


(* ::Section:: *)
(*AddIndexTagToTensors*)


(* ::Text:: *)
(*AdIndexTagToTensors[expr] adds the tag Index[ ] to each index of a tensor. If the tag already exists, ignore it.*)


AddIndexTagToTensors[expr_] := Block[{workingexpr},

    (* Treat Allow summation tensors *)
    workingexpr = expr /. nt_?(PRIVATE`NoTensQ[#] === True&)[Except[Index[__], ind_]] :> nt[Append[$IndList[nt][[1]], ind]];

    (* Deal with the rest *)
    workingexpr = workingexpr //. PRIVATE`$TensIndRules;

    (* Return and exit *)
    Return[workingexpr]

];


(* ::Section:: *)
(*FlavorExpandVertex*)


(* ::Subsection::Closed:: *)
(*AddFlavorToClass*)


(* ::Text:: *)
(*AddFlavorToClass[ {part, i} ] returns {part[Index[ Flavor, Ext[i] ] ], i}*)


AddFlavorToClass[{part_, i_}] := Block[{flav = FindClassFlavor[part]},
   
    If[flav =!= MR$Null, 
      Return[{part[Index[FindClassFlavor[part], Ext[i]]], i}],
      (*else*)
      Return[{part,i}]
      ]

];


(* ::Text:: *)
(**)


(* ::Subsection:: *)
(*FlavorExpandVertex*)


(* ::Text:: *)
(*FlavorExpandVertex[ vertex ] takes a single vertex object and returns a list with the vertex expanded over flavors.*)


FlavorExpandVertex[vertex_] := Block[{prtls, flavinds, newvertices, flavorindexrules, MyRule, modifiedvertex},

(*$FlavExpCount++;*)

   (*Read out the particles, and add the flavor to each *)
   prtls = AddFlavorToClass /@ vertex[[1]];

   (*Read out flavor indices to be expanded *)
   flavinds = Cases[prtls[[All, 1]], _[Index[ind__]]];
   flavinds = flavinds[[All,1]];

   (* Index[ Flavor, Ext[i]] ->  Index[ Index[ Flavor, Ext[i]]]
      This is necessary because when we sum, then Index[Flavor, Ext[i]] gets replaced by a number *)
   
   flavorindexrules = Table[MyRule[flavinds[[k]], Index[flavinds[[k,1]], flavinds[[k]]]], {k, Length[flavinds]}] /. MyRule -> Rule;
   modifiedvertex = vertex[[2]] /. flavorindexrules;


   (* Index[Flavor, Ext[i]]   ->  {Index[Flavor, Ext[i]], {1,2,3,4,....}}
      These lists act as inputs to create tables to expand the vertices *)
   flavinds = {#, IndexRange[Drop[#, -1]]}& /@ flavinds;


   (* Now Expand!
      I.e., we create a table with the vertex, and the flavinds as indices in the table *)
   newvertices = Flatten[Table @@ List[VertexObject[prtls, modifiedvertex], Sequence @@ flavinds]];



   (* Finally, apply the definitions, and then turn to ParamRules *)
   newvertices = newvertices //. FR$FlavorExpLookUp;
   newvertices = Expand[newvertices  //. MR$DefinitionsNoFields //. NTIndex -> Index ];


   (* Last but not least, we remove vertices that have evaluated to zero *)
    newvertices = DeleteCases[newvertices, VertexObject[_, 0]];

   (* Return and exit *)
   FRDebug["Vertex " <> ToString[vertex[[1, All, 1]]], newvertices];
   Return[newvertices];

]; 


(* ::Text:: *)
(**)


(* ::Subsection:: *)
(*FlavorExpandVertices*)


(* ::Text:: *)
(*FlavorExpandVertices [ vertices ] expands <vertices>, written in terms of classes, over flavors.*)


FlavorExpandVertices[vertices_List] := Block[{outvertices, nonflavored, flavored},
    FR$Count1++; 
   (* We first build the look up table for the flavors *)
   FR$FlavorExpLookUp=BuildFlavorExpLookUp[];

   (* We extract the non Flavored vertices, because they are already ok*)
   nonflavored = ExtractNonFlavoredVertices[{vertices}];

   (* If all vertices are non flavored, then return and exit *)
   If[Length[nonflavored] == 1,
      Return[{vertices}]];

   (* This gives us the flavored ones *)
   flavored = Complement[{vertices}, nonflavored];

   
   (* Those are no expanded over flavors*)
   flavored = Flatten[FlavorExpandVertex /@ flavored];

   (* Remove the VertexObject[] *)
   flavored = flavored /. VertexObject -> List;

   (*Combine the two*)
    outvertices = Join[nonflavored, flavored];

   (*Return and exit *)
    Return[outvertices]

];
   


(* ::Section:: *)
(*IndexSummation*)


(* ::Subsection::Closed:: *)
(*IndexSumCounter*)


(* ::Text:: *)
(*IndexSumCounter[Index[ name,  symbol,  i ]] take the entries of an internal index, renames it to Index[ name,  Int[ <some number>]].*)
(*The result is stored in a a replacement list   IndexSumCounterList    =   {Index[name, symbol, i]  ->  Index[ name,  Int[ <some number>]], ...} to reuse the assignment for other indices with the same name.*)


IndexSumCounter[Index[name_, symbol__]] := Block[{workingindex = Index[name, symbol] /. IndexSumCounterList},


   (* Test if {name, symbol i} is already known. If yes, return and exit *)
   If[workingindex =!= Index[name, symbol],
      Return[workingindex]
     ];

   (* Otherwise create it! *)
   workingindex = Index[name, Int[$IndexSumCounter[name]++]];

   (* There could be hidden indices we want to sum over, i.e. things that are not wrapped into Index.*)
   (* To catch those, we save them into a replacement rule list, and replace them later *)
   If[Length[{symbol}] == 1, 
      AppendTo[$IndexSumReplacements, symbol -> workingindex]
     ];

   (* and then add it to the memory *)
   AppendTo[IndexSumCounterList, Rule @@ {Index[name, symbol], Index[name, workingindex]}];

   (* Return and exit *)
   Return[workingindex]

];


(* ::Subsection::Closed:: *)
(*ConvertSumToList*)


(* ::Text:: *)
(*Converts a sum to a list. If there is only a single term, {expr} is retruned.*)


ConvertSumToList[expr_] := If[Head[expr] === Plus, List @@ expr, {expr}];


(* ::Subsection:: *)
(*PerformIndexSumInTerm*)


(* ::Text:: *)
(*PerformIndexSumInTerm[ expr ] expand the contrated indices on the term expr.*)


PerformIndexSumInTerm[expr_] := Block[{workingexpr, prefac},
 
    (* Initialise the counters ad the memory list*)
    Set[$IndexSumCounter[#], 1]& /@ (PRIVATE`MR$IndexList /. Index -> Identity);
    IndexSumCounterList = {};
    $IndexSumReplacements = {};
(*Print[IndexSumCounterList];*)

    (* Count the indices *)
    workingexpr = expr /. Index[Except[Lorentz|Spin|Colour|Gluon|Sextet, name_], symb__]:> IndexSumCounter[Index[name,symb]] /; Not[MatchQ[{symb}, {Ext[_]}|{_?NumericQ}]];
(*Print[workingexpr];*)

    (* Find the hidden indices *)
    workingexpr = workingexpr //. $IndexSumReplacements;

    (* If {}, then nothing to be done, and return and exit *)
    If[IndexSumCounterList === {},
       Return[expr]
       ];
  
    (* Rename the indices *)
    workingexpr = workingexpr /. IndexSumCounterList;
    IndexSumCounterList = {#, IndexRange[Index[#[[1]]]] /. NoUnfold -> Identity}& /@ (IndexSumCounterList[[All, 2]] /. Index[_, Index[inds__]] :> Index[inds]);

    (* Go into the recursion to build the sums *)
(*Print["***********"];
Print[workingexpr];*)
    workingexpr = List@@ workingexpr;
    prefac = Select[workingexpr, FreeQ[#, Int]&];
    workingexpr = Fold[IndexSumExpand, Times @@ Complement[workingexpr, prefac], IndexSumCounterList];
    prefac = Times @@ prefac;
    workingexpr = prefac * workingexpr /. IndexSumExpand -> IndexSumCombine; 
    workingexpr= workingexpr /. IndexSumCombine[ex_, inds__] :> FRIndexSum[ex, Sequence @@ Sort[{inds}]];
    (* We now introduce the abbreviations *)
    workingexpr = workingexpr /. FRIndexSum -> AbbreviateIndexSum;



    (* Return and exit *)
    Return[workingexpr]

];

    


(* ::Subsection::Closed:: *)
(*CombineSums*)


(* ::Text:: *)
(*CombineSums[ ...] transforms a 2xn matrix {{a, s}, {b,s}, ...} into the vector {a+b+..., s}*)


CombineSums[matrix_List] := Head[matrix[[1]]][Plus @@ matrix[[All, 1]], matrix[[1,2]]];
 
   


(* ::Subsection:: *)
(*GatherByFirstElement[ list ] *)


(* ::Text:: *)
(*This function is equivalent to GatherBy[ list, #[[1]]& ], but since GatherBy is not avaibale for Mathematica V6 and below, we have to define it ourselves. *)
(*If however V > 6, then use GatherBy in its native way.*)


GatherByFirstElement[ list_ ] := GatherBy[ list, #[[1]]&] /; $VersionNumber >= 7;


GatherByFirstElement[ list_ ] := Block[{firsts = Union[list[[All, 1]]]},

    Return[Table[Select[list, #[[1]] === firsts[[$kk]]&], {$kk, Length[firsts]}]]

] /; $VersionNumber < 7;

   


(* ::Subsection:: *)
(*GatherBySecondElement[ list ] *)


(* ::Text:: *)
(*This function is equivalent to GatherBy[ list, #[[2]]& ], but since GatherBy is not avaibale for Mathematica V6 and below, we have to define it ourselves. *)
(*If however V > 6, then use GatherBy in its native way.*)


GatherBySecondElement[ list_ ] := GatherBy[ list, #[[2]]&] /; $VersionNumber >= 7;


GatherBySecondElement[ list_ ] := Block[{seconds = Union[list[[All, 2]]]},

    Return[Table[Select[list, #[[2]] === seconds[[$kk]]&], {$kk, Length[seconds]}]]

] /; $VersionNumber < 7;

   


(* ::Subsection:: *)
(*GatherByFirstTwoElements[ list ] *)


(* ::Text:: *)
(*This function is equivalent to GatherBy[ list, Take[#, 2]& ], but since GatherBy is not avaibale for Mathematica V6 and below, we have to define it ourselves. *)
(*If however V > 6, then use GatherBy in its native way.*)


GatherByFirstTwoElements[ list_ ] := GatherBy[ list, Take[#, 2]&] /; $VersionNumber >= 7;


GatherByFirstTwoElements[ list_ ] := Block[{seconds = Union[Take[#, 2]& /@ list]},

    Return[Table[Select[list, #[[2]] === seconds[[$kk]]&], {$kk, Length[seconds]}]]

] /; $VersionNumber < 7;

   


(* ::Subsection::Closed:: *)
(*MappedGatherByFirstTwoElements*)


(* ::Text:: *)
(*MappedGatherByFirstTwoElements[ f,  list ] applies GatherByFirstTwoElements to list, and returns a list where f is applied over the set of equivalence classes.*)


MappedGatherByFirstTwoElements[func_, list_] := func @@@ GatherByFirstTwoElements[list];



(* ::Subsection:: *)
(*IndexSum*)


(* ::Text:: *)
(*Here we give the building blocks used to build IndexSum. The building blocks are*)
(**)
(*   *  IndexSumExpand:   IndexSumExpand[ c * b_i,  i]   =  c * IndexSumExpand[b_i, i].*)
(*   * IndexSumCombine: IndexSumCombine[a_i  IndexSumCombine[ b_ij, j], i] = IndexSumCombine[a_i  b_ij, i, j]*)
(*   * AbbreviateIndexSum : Generate an AutoName for the sum, return this autoname, and keep the relation between the AutoName and the sum in memory, via the following global variable, reset for each interface call, *)


FR$AbbIndexSum = {};


(* ::Text:: *)
(*to define the sum later as an internal param.*)
(*                                           !!!!  This routine is recursive, and does not forget its past!!!!!   f[x_] := f[x] = ....*)
(*                                           *)
(*                                           The counter for the AutoName is initialized by the following global variable, which is reset by the interface at each call.*)


FR$AbbIndexSumCounter = 1;


(* ::Text:: *)
(*                                           This routine also calls FindExternalIndices.*)
(*    *find all indices of the type Index[_, Ext[_]], and returns an ordered list containing them. This is done via a helper function GetExtInd*)
(*    *)
(*    * FlavorExpandISUM expand an abbreviation ISUM over externl indices*)


(* ::Subsubsection:: *)
(*IndexSumExpand*)


IndexSumExpand[aa_ * b_., {Index[g_, Int[i_]], inds_}] := aa * IndexSumExpand[b, {Index[g, Int[i]], inds}] /; FreeQ[aa, Int[i]] || FreeQ[aa, g];


(* ::Subsubsection:: *)
(*IndexSumCombine*)


IndexSumCombine[aa_. IndexSumCombine[b_, inds1__], inds2__] := IndexSumCombine[aa * b, inds1, inds2];


(* ::Subsubsection::Closed:: *)
(*GetExtInd*)


GetExtInd[name_, Ext[i_]] := Block[{},
   
    AppendTo[OptFlav$GetExtInd, Index[name, Ext[i]]];

    (* Return and exit *)
    Return[Index[name, Ext[i]]]

];


(* ::Subsubsection:: *)
(*AbbreviateIndexSum*)


AbbreviateIndexSum[summand_, inds__] := FRIndexSum[summand, inds] = Block[{autoname = Symbol["I" <> ToString[FR$AbbIndexSumCounter++]], abb},

    (* Initialize the helper function *)
    OptFlav$GetExtInd = {};

    (* Find the external inds the sum depends on *)
    summand /. Index[name_, Ext[i_]] :> GetExtInd[name, Ext[i]];

    (* Reorder the externl indices we found, and delete double entries, and create the new abbreviation*)
    abb = autoname @@ Sort[Union[OptFlav$GetExtInd]];

    (* Add the new abbreviation to the list of new symbols *)
    AppendTo[FR$AbbIndexSum, {abb, IndexSum[summand, inds]}];

    (* Return the abbreviation, and exit (no return command, because this one gives weird thing in the recursive call *)
    abb

];

    

    
    


(* ::Subsubsection:: *)
(*FlavorExpandISUM*)


FlavorExpandISUM[{isum_, expr_}] := Block[{indexplist},
   (* we build the list of indices used to build the table *)
   indexplist = NestList[IndexRange[Take[#, 1]]&, #, 1]& /@ isum;

   (* Return and exit *)
   Return[Table @@ List[ISUMObject[isum, expr], Sequence @@ indexplist]];

];


(* ::Subsubsection:: *)
(*BuildParamRulesForISUMs*)


Options[BuildParamRulesForISUMs] = {Style -> ""};


FR$AbbreviationsCharactorCode = 96;


BuildParamRulesForISUMs[list_, OptionsPattern[]] := Block[{isums = list[[All,1]], MakeSymb, MyRule, prefix},

   (* This function creates the symbol *)
    prefix = OptionValue[Style];
    MakeSymb[is_[iis__]] := Symbol[prefix <> ToString[is] <> FromCharacterCode[FR$AbbreviationsCharactorCode] <> StringJoin @@ (ToString /@ {iis})];

    MakeSymb[is_[]] := Symbol[prefix <> ToString[is] <> FromCharacterCode[FR$AbbreviationsCharactorCode] <> "x"];

   (* Create the symbol replacements *)
   isums = (MyRule[#, MakeSymb[#]]& /@ isums) /. MyRule -> Rule;


   (* Return and exit *)
   Return[Dispatch[isums]]

];
    


(* ::Subsection:: *)
(*PerformIndexSum*)


(* ::Text:: *)
(*PerformIndexSum[expr] sums expr over internal contracted indices (except for Loretnz, Spin, Colour, Gauge).*)


PerformIndexSum[expr_] := Block[{listexpr = ConvertSumToList[Expand[expr]], out, nosums},

    (* CD, 05.04.11: We need to open TensDot's first *)
   listexpr = listexpr /. {TensDot[x_, y___][Except[Index[Spin|Spin1|Spin2|Lorentz|Colour|Gauge|Sextet,__], i_], 
         j_] :> PRIVATE`PYTensDotOpen[x,y][i,j]};
    

(*$IndSumCount++;*)
    (* make sure everything has an index tag *)
    listexpr = AddIndexTagToTensors[listexpr]; 

(*Print[$DebugCounter++];*)
    (* Identify the sums per term *)
    listexpr = Plus @@ (PerformIndexSumInTerm /@ listexpr);
(*Print["After PerformIndexSumInTerm"];
Print[listexpr];*)


    out = AddIndexTagToTensors[Expand[listexpr]] //. MR$DefinitionsNoFields //. ParamRules;


    (* Return and exit *)
    FRDebug["PerformIndexSum: ", out];
    Return[out];

];

    



(* ::Section:: *)
(*PerformIndexSumInVertex[vertex]*)


(* ::Text:: *)
(*PerformIndexSumInVertex[vertex] returns a vertex object where the vertex was expanded over  internal contracted indices*)


PerformIndexSumInVertex[vertex_] := {vertex[[1]], PerformIndexSum[vertex[[2]]]};


(* ::Section:: *)
(*RemoveIndexWrapper*)


(* ::Text:: *)
(*RemoveIndexWrapper[expr ] removes all Index[ _, i], where i is numeric, and returns simply i.*)


RemoveIndexWrapper[expr_] := Expand[expr/.Index[_,i_?NumericQ]:>i];


(* ::Subsection:: *)
(*CreateIParamsFromISUMs*)


(* ::Text:: *)
(*CheckInteractionOrderForAbbreviation[] checks the interaction orders of an abbreviation. If all the terms have the same orders, nothing is done.*)
(*Otherwise, it is split into pieces with the same orders, and new symbols are created that contain the pieces. These new symbols are stored in the IParam-like list*)
(*FR$AdditonalIParamsFromAbbreviations. Furthermore, a replacement list is created that allows to replace the abbreviation by the new symbols later *)


CheckInteractionOrderForAbbreviation[name_, value_] := Block[{

   val = Expand[value],
   symbols

   },
   (*Make a list of the expression *)
   val = If[Head[val] === List,
            List @@ val,  
            {val}
           ];
   
   (* Get the orders of each term, and group the terms by order *)
   val = GatherByFirstElement[{PRIVATE`GetIntOrder[#], #}& /@ val];
   val = Plus @@@ val;

   (* If only one term, there is only one order -> Nothing to be done *)
   If[Length[val] == 1,
      Return[val[[1]]];
     ];

   (* If more than one order*)
   (* Create the new symbols *)
   symbols = Table[Unique["J"], {Length[val]}];
   (* We keep in memory the replacement for the abbreviation to the new symbols, in order to do this later *)
   AppendTo[FR$InteractionOrderEmergencyReplacement, name -> Plus @@ symbols];

   (*We also keep in memory the definitions of the new parameters *)
   FR$AdditonalIParamsFromAbbreviations = Join[FR$AdditonalIParamsFromAbbreviations, Inner[CreateIParamsEntry, symbols, val, List]];

   Return[value];

];   


(* ::Text:: *)
(*CreateIParamsFromISUMs[ list ] takes an FR$AbbIndexSumExpanded entry and creates an IParamList entry from it.*)
(*It also updates MGOrdertemp to include possible interaction order *)


CreateIParamsFromISUMs[ISUMObject[name_, value_]] := CreateIParamsEntry[name, value];

CreateIParamsEntry[name_, value_] := Block[{

    order  = GetOrder[value], 
    order2 = PRIVATE`GetIntOrder[value],
    iparamentry
    
    },
    
    (* Update MGOrdertemp *)
    If[order =!= {}, MGOrdertemp[name] = order[[1]];
       ParamIntOrder[name] = PRIVATE`CreateInteractionOrderList[order2];
      ];

    (* Create the IParamList entry *)
    iparamentry = If[order === {} ,
       {name, value, True, "FeynRules Abbreviation"},
       Flatten[{name, value,  Sequence @@ Flatten[PRIVATE`CreateInteractionOrderList[order2]], True, "FeynRules Abbreviation"}]
      ];

    (* Setting the numQ and CnumQ tags *)
    numQ[name] = True;
    CnumQ[name] = True;

    (* Setting the numerical value *)
    NumericalValue[name] = NumericalValue[value];
 
    (* Return and exit *)
    Return[iparamentry]

];


(* ::Section:: *)
(*MasterModule*)


Options[CreateISUMAbbr] = {Style -> ""};


Options[PrepareBuildParamRulesForISUMs] = {Style -> ""};

PrepareBuildParamRulesForISUMs[expr_,OptionsPattern[]]:=BuildParamRulesForISUMs[{expr}, Style -> OptionValue[Style]];


CreateISUMAbbr[abbrlist_, OptionsPattern[]] := Block[{style = OptionValue[Style], zeroentries = {}},


  (* FR$AbbIndexSum is now known, so we can expand it, and use it to build the new ParamRules *)
  FR$AbbIndexSumExpanded = abbrlist /. IndexSum -> Sum;
 
  (* Benj: Parallaelization *) 
    FR$DoPara=If[Global`FR$Parallelize=!=False && Length[FR$AbbIndexSumExpanded]>40 && $KernelCount>1,True,False];
    FR$AbbIndexSumExpanded = Flatten[If[FR$DoPara,
      ParallelizeMe[FlavorExpandISUM,FR$AbbIndexSumExpanded],
      (* no parallelization *)
      FlavorExpandISUM /@ FR$AbbIndexSumExpanded
    ]];

    FR$DoPara=If[Global`FR$Parallelize=!=False && Length[FR$AbbIndexSumExpanded]>40 && $KernelCount>1,True,False];
    If[FR$DoPara,
      FR$AbbIndexSumExpanded = ParallelizeMe[AddIndexTagToTensors,FR$AbbIndexSumExpanded];
      FR$AbbIndexSumExpanded = FR$AbbIndexSumExpanded//. Dispatch[MR$DefinitionsNoFields];
      FR$AbbIndexSumExpanded = ParallelizeMe[Expand,FR$AbbIndexSumExpanded],
      (* no parallelization *)
      FR$AbbIndexSumExpanded = AddIndexTagToTensors /@ FR$AbbIndexSumExpanded;
      FR$AbbIndexSumExpanded = Expand[# //. Dispatch[MR$DefinitionsNoFields]]& /@ FR$AbbIndexSumExpanded;
    ];

    zeroentries = Cases[FR$AbbIndexSumExpanded, ISUMObject[_, 0]] /. ISUMObject -> Rule;
    FR$AbbIndexSumExpanded = Complement[FR$AbbIndexSumExpanded, zeroentries /. Rule -> ISUMObject];

    (* Now we go to ParamRules notation for the abbreviations *)
    FR$AbbIndexSumParamRules = BuildParamRulesForISUMs[FR$AbbIndexSumExpanded, Style -> style];

    If[FR$DoPara,
      FR$AbbIndexSumExpanded = ParallelizeMe[Expand,FR$AbbIndexSumExpanded],
      FR$AbbIndexSumExpanded = Expand[FR$AbbIndexSumExpanded]
    ];
    FR$AbbIndexSumExpanded = FR$AbbIndexSumExpanded //. ParamRules //. FR$AbbIndexSumParamRules;

    If[ValueQ[Abbreviations[]],
       Abbreviations[] = Union[Join[Abbreviations[], Rule[#1,#2]& @@@ (Take[#, 2]& /@ FR$AbbIndexSumExpanded)]],
       Abbreviations[] = Rule[#1,#2]& @@@ (Take[#, 2]& /@ FR$AbbIndexSumExpanded);
      ];

    (* Return and exit *)
    Return[zeroentries];
];


(* ::Text:: *)
(*This is the master module which calls all other modules in the correct order*)


PartitionVertexList[list_List, n_Integer] := Block[{length=Length[list],
     quotient, partlist, prelength
     },

     If[n==1,
        Return[{list}]
       ];

     If[n>length,
        Return[List /@ list];
        ];
   
     quotient = Ceiling[length/(n)];
     partlist = Partition[list, quotient];
     prelength = Length[Join @@ partlist];
     If[prelength < length,
        partlist = Append[partlist, Drop[list, prelength]];
       ];

     If[Length[Join@@partlist] =!= length,
        Print[" !!!! fatal Error !!!!"];
       ];

     Return[partlist];
];


Options[PerformVertexFlavorExpansion] = {SaveVertices -> False};


PerformVertexFlavorExpansion[vertexlist_, OptionsPattern[]] := Block[{vertices = Expand[vertexlist], zeroentries = {}},
   
    (* Check if FR$AbbIndexSum is empty. If not, print a warning *)
    If[FR$AbbIndexSum =!= {}, Message[OptFlavExp::FR$AbbIndexSum]];

    (* We introduce the abbreviations for the the internal index sums and store them in FR$AbbIndexSum *)
    (* Benj: this does not need to be parallelized; faster like this *)
    vertices = PerformIndexSumInVertex /@ vertices;
    FR$DoPara=If[Global`FR$Parallelize=!=False && Length[vertices]>40 && $KernelCount>1,True,False];

    (* Benj: This is slow \[Rule] parallelization inside the function *)
    zeroentries = CreateISUMAbbr[FR$AbbIndexSum];

    (* 4.4.12, CD: We store the vertices,so that we can reuse them in the decay module *)
    If[OptionValue[SaveVertices],
       FR$AbbreviatedVertices = vertices;
       FR$ZeroAbbreviations = zeroentries;
    ];

    (* We now go back to the vertices and expand them over externl indices *)
      FR$DoPara=If[Global`FR$Parallelize=!=False && Length[vertices]>40 && $KernelCount>1,True,False];
      FR$Count1=0;
      vertices=If[FR$DoPara,
        Print["Flavor expansion of the vertices distributed over ", Global`FR$KernelNumber," cores: ", Dynamic[FR$Count1], " / ",
          Length[vertices]];
        SetSharedVariable[FR$Count1];
        ParallelizeMe[FlavorExpandVertices,vertices],
        Print["Flavor expansion of the vertices: ", Dynamic[FR$Count1], " / ",
         Length[vertices]];
        FlavorExpandVertices/@vertices
    ];
    vertices = DeleteCases[Flatten[vertices,1] /. zeroentries, {_, 0}]; 

    
   (* Branching: whether ParamRules should be applied or not *)
   vertices = RemoveIndexWrapper[vertices];
   vertices = vertices //. MR$DefinitionsNoFields //. zeroentries;
   FR$DoPara=If[Global`FR$Parallelize=!=False && Length[vertices]>40 && $KernelCount>1,True,False];  
   vertices=If[FR$DoPara,ParallelizeMe[Expand,vertices],Expand/@vertices];
   vertices = DeleteCases[vertices, {_, 0}] ;
   vertices=If[FR$DoPara,DistributeDefinitions[$IndList,PRIVATE`NoTensQ,ParamRules,FR$AbbIndexSumParamRules];ParallelizeMe[PleaseApplyParamRules,vertices],PleaseApplyParamRules/@vertices];
    
    (* Create the IParamList entries *)
    FR$AbbIndexSumExpanded = CreateIParamsFromISUMs /@ FR$AbbIndexSumExpanded;

    (* Return and exit *)
    Return[vertices];
];

    

    
    



PleaseApplyParamRules[verts_]:= verts/. nt_?(PRIVATE`NoTensQ[#] === True&)[Except[Index[__], ind_]] :> nt[Append[$IndList[nt][[1]], ind]] //. ParamRules //. FR$AbbIndexSumParamRules;


(* ::Section::Closed:: *)
(*ClassMemberQ*)


(* ::Text:: *)
(*ClassMemberQ[ field ] returns True if field is a class member, and False otherwise.*)


ClassMemberQ[field_?(AntiFieldQ)] := ClassMemberQ[anti[field]];
ClassMemberQ[CC[field_]] := ClassMemberQ[field];

ClassMemberQ[xxx_] := False /; Not[FieldQ[xxx]];


(* ::Section::Closed:: *)
(*MergeSortedVertices*)


(* ::Text:: *)
(*MergeSortedVertices[ <vertices >] merges the vertices in the list <vertices>.*)
(*The vertices must be sorted already according to SortVertexFields.*)
(*The routine gather the vertices by first elements, then maps MergeSortedVertex over it.*)


MergeSortedVertices[list_List] := MergeSortedVertex /@ GatherByFirstElement[list];


(* ::Subsection::Closed:: *)
(*MergeSortedVertex*)


(* ::Text:: *)
(*MergeSortedVertex[{  vertexobj1, vertexobj2, ....} ] merges the vertex object, assuming that they have the same head.*)


MergeSortedVertex[list_List] := {list[[1,1]], Plus @@ list[[All, 2]]};


(* ::Section:: *)
(*SymmetryFactors*)


(* ::Subsection::Closed:: *)
(*RelabelExt*)


(* ::Text:: *)
(*RelabelExt[{parts, vertex}] relabels the Ext[] tags in vertex according to the particle ordering in parts.*)


RelabelExt[{parts1_, vertex_}] := Block[{newparts = parts1, extrpls, newvertex},    
    
    (* We build the replacement list for Ext[] *)
    newparts = MapIndexed[Join[#1,#2]&, newparts];
    extrpls = Rule[Ext[#2, opt___], Ext[#3, opt]]& @@@ newparts;
    newparts = {#1,#3}& @@@ newparts;

    (* Then relabel Ext[] in vertex *)
    newvertex = vertex /. {FV[Except[Ext[_], k_], mu_]:> FV[Ext[k], mu], 
                           SP[Except[Ext[_], k1_], Except[Ext[_], k2_]]:> SP[Ext[k1], Ext[k2]], 
                           SlashedP[Except[Ext[_], k_], inds___] :> SlashedP[Ext[k], inds]};
    newvertex = newvertex /. extrpls;
    newvertex = newvertex /. {FV[Ext[k_], mu_] :> FV[k, mu],
                              SP[Ext[k1_], Ext[k2_]] :> SP[k1,k2],
                              SlashedP[Ext[k_], inds___] :> SlashedP[k, inds]};

    (* Return and exit *)
    Return[{newparts, newvertex}];
];


(* ::Subsection:: *)
(*SortVertexFields*)


(* ::Text:: *)
(*SortVertexFields[ {parts, vertex} ] sorts the fields in <parts> into canonical order, and applies MakeIdenticalFermions.*)
(*It also relabels the external indices back into canonical order.*)
(**)
(*E.g.*)
(**)
(*{{b,1}, {c,2}, {a,3}}   ->  {{a,3}, {b,1}, {c,2}} ->  {{a,1}, {b,2}, {c,3}}*)


SortVertexFields[{parts_, vertex_}] := Block[{newparts, newvertex, FieldOrder},
	(* Sort, but only interchange fields which are not both ghosts. *)
	FieldOrder[{aa_,b_}]:=Which[
		GhostFieldQ[aa[[1]]]===True&&GhostFieldQ[b[[1]]]===True,True,
		1==1,OrderedQ[{aa,b}]
	];
	newparts = Sort[parts,FieldOrder[{#1,#2}]&];

    (* If nothing has changed after sorting, done! *)
    If[newparts === parts, 
       Return[{parts, vertex}]
       ];

   (* Otherwise we build the replacement list for Ext[] *)
   newvertex = RelabelExt[{newparts, vertex}];


    (* Return and exit *)
    Return[newvertex];

];
    
    


(* ::Subsection::Closed:: *)
(*MakeIdenticalFermions*)


(* ::Text:: *)
(*MakeIdenticalFermions[field] transforms *)
(** a Majorana "antifield" into the Majorana itself.*)
(** a charge conjugated fermion into its antifield.*)


MakeIdenticalFermions[field_] := field /. {xx_?((RFermionFieldQ[#] === True) && (AntiFieldQ[#] === True)&) :> anti[xx],        
                                           CC[xx_] :> anti[xx]
                                          };


(* ::Subsection::Closed:: *)
(*IdenticalParticlesQ*)


(* ::Text:: *)
(*IdenticalParticlesQ[ field1, field2 ] returns True if the field1 and field2 are the same particle, otherwise False.*)
(*Same paticle means one of the following:*)
(** field1 ===  field2*)
(** field1 is a Majorana and anti[field1] === field2*)
(** CC[field1]   is the same as anti[field2]. and vice-versa*)


IdenticalParticlesQ = MakeIdenticalFermions[#1] === MakeIdenticalFermions[#2]&;


(* ::Subsection:: *)
(*ClassesSymmetryFactor*)


(* ::Text:: *)
(*ClassSymmetryFactor[n1, n2, n3, ...][]  returns e.g.,*)
(**)
(*ClassSymmetryFactor[1,2,3]  =  (1!) x (1!) x (1!)*)
(*ClassSymmetryFactor[1,1,3]  =  (2!) x (1!) *)
(*ClassSymmetryFactor[1,1,1]  =  (3!)*)
(**)
(*SymbolicClassesSymmetryFactor does the same, but also evaluates on symbolic arguments.*)


ClassesSymmetryFactor[inds1___, Index[_, i_?NumericQ], inds2___][] := ClassesSymmetryFactor[inds1, i, inds2][];


ClassesSymmetryFactor[inds___][] := (Times @@ (Factorial[#2]& @@@ Tally[{inds}])) /; Union[NumericQ /@ {inds}] === {True};


SymbolicClassesSymmetryFactor[inds___][] := (Times @@ (Factorial[#2]& @@@ Tally[{inds}]))


(* ::Subsection:: *)
(*RemoveClassesSymmetryFactors*)


(* ::Text:: *)
(*RemoveClassesSymmetryFactors[{ parts, vertex }] divides vertex by the symmetry factor corresponding to the classes in *)


RemoveClassesSymmetryFactors[{parts_List, vertex_}] := Block[{idclasses = MakeIdenticalFermions /@ Cases[parts, {_?PRIVATE`FlavoredQ, _}],
    internalsymmetryfactor},

    (* If idclasses is empty, there is nothing to be done -> Exit *)
    If[idclasses === {}, 
       Return[{parts, vertex}]
      ];

    (* Count identical particles*)
    idclasses = GatherByFirstElement[idclasses];
    internalsymmetryfactor = Times @@ (Factorial[Length[#]]& /@ ((First /@ #)& /@ idclasses));

    (* Build the symmetry factors out of this *)
     idclasses = (Index[FindClassFlavor[#1], Ext[#2]]& @@@ # &) /@ idclasses;
     idclasses = ClassesSymmetryFactor[Sequence @@ #][]& /@ idclasses;
(*     idclasses = (#/Length[#[[0]]] &) /@ idclasses;*)
     idclasses = Times @@ idclasses;

    (* Rescale vertex, then return and exit *)
    Return[{parts, vertex * idclasses/internalsymmetryfactor}]
];  


(* ::Section:: *)
(*OptimizeFermionChains*)


(* ::Text:: *)
(*OptimizeFermionChains[{parts, vertex} ] looks for vertices with _exactly_ one fermion chain, and then rewrites it in the convention followed by most ME generators (the"decay" convention, see FR manual).*)
(*It only does it for vertices with exactly one fermion chain, because the convention becomes meaningless for n-femrion operators, n>2.*)


OptimizeFermionChains[{parts_, vertex_}] := Block[{ferm2, newvertex = vertex, psi1, psi2, nfbar1, nf2,
     FermQ = (Spin32FieldQ[#]===True) ||(DiracFieldQ[#]===True) ||( MajoranaFieldQ[#]===True)&,
     Dir1Q = ((CSpin32FieldQ[#[[1]]] === True)||(DiracFieldQ[#[[1]]] === True)) && (Not[AntiFieldQ[#[[1]]]] === True)&,
     Dirm1Q = ((CSpin32FieldQ[#[[1]]] === True)||(DiracFieldQ[#[[1]]] === True)) && (AntiFieldQ[#[[1]]] === True)&,
     MajQ = ((RSpin32FieldQ[#[[1]]] === True)||(MajoranaFieldQ[#[[1]]] === True))&},
	(*Neil -- I am not so sure about simply placing RSpin32FieldQ and CSpin32FieldQ in the expressions above. It might require more work.*)
 
    (* If more than one fermion chain, exit *)
    If[Count[parts,{_?(FermQ),_}] != 2,
       Return[{parts, vertex}]
      ];

    
     (* Otherwise reorder the fermions into the last position *)
     ferm2 = parts //. {fields1___, {psi_?FermQ, ipsi_}, {phi_?(Not[FermQ[#]]&), iphi_}, field2___} :> {fields1, {phi, iphi}, {psi, ipsi}, field2};

     (* We now distinguish cases to get the conventional particle ordering *)
     psi1 = ferm2[[-2]];
     psi2 = ferm2[[-1]];
     ferm2 = Drop[ferm2, -2]; 


     Which[(* psi1 = fermion, psi2 = antifermion: This vertex must be psi2.psi1, or CC[psi1bar].CC[psi2bar]. the second therm is taken care of later by the fermion flow algortihm *)
           Dir1Q[psi1] && Dirm1Q[psi2], newvertex = RelabelExt[{Join[ferm2, {psi2,psi1}], newvertex}],
           (* psi1 = fermion, psi2 = fermion: this vertex corresponds to CC[psi1bar].psi2, or CC[psi2bar].psi1, but this is taken care of later by the fermion flow *)
           Dir1Q[psi1] && Dir1Q[psi2], newvertex = RelabelExt[{Join[ferm2, {{CC[anti[psi1[[1]]]],psi1[[2]]},psi2}], newvertex}],
           (* psi1 = antifermion, psi2 = fermion: this vertex corresponds to psi1.psi2, or CC[psi2bar].CC[psi1bar] *)
           Dirm1Q[psi1] && Dir1Q[psi2], newvertex = RelabelExt[{Join[ferm2, {psi1,psi2}], newvertex}],
           (* psi1 = antifermion, psi2 = antifermion: this vertex corresponds to psi1.CC[psi2bar], or psi2.CC[psi1bar] *)
           Dirm1Q[psi1] && Dirm1Q[psi2], newvertex = RelabelExt[{Join[ferm2, {psi1,{CC[anti[psi2][[1]]], psi2[[2]]}}], newvertex}],
           (********)
           (* psi1 = antifermion, psi2 = Majorana: this vertex corresponds to psi1.psi2, or psi2bar.CC[psi1bar] *)
           Dirm1Q[psi1] && MajQ[psi2], newvertex = RelabelExt[{Join[ferm2, {psi1,psi2}], newvertex}],
           (* psi1 = fermion, psi2 = Majorana: this vertex corresponds to CC[psi1bar].psi2, or psi2bar.psi1 *)
           Dir1Q[psi1] && MajQ[psi2], newvertex = RelabelExt[{Join[ferm2, {{anti[psi2[[1]]], psi2[[2]]},psi1}], newvertex}],
           (********)
           (* psi1 = Majorana, psi2 = antifermion: this vertex corresponds to psi2.psi1, or psi1bar.CC[psi2] *)
           MajQ[psi1] && Dirm1Q[psi2], newvertex = RelabelExt[{Join[ferm2, {psi2,psi1}], newvertex}],
           (* psi1 = Majorana, psi2 = fermion: this vertex corresponds to psi1.psi2, or psi2bar.psi1bar *)
           MajQ[psi1] && Dir1Q[psi2], newvertex = RelabelExt[{Join[ferm2, {anti[psi1],psi2}], newvertex}],
           (********)
           (* psi1 = Majorana, psi2 = Majorana: this vertex corresponds to psi1.psi2 *)
           MajQ[psi1] && MajQ[psi2], newvertex = RelabelExt[{Join[ferm2, {anti[psi1],psi2}], newvertex}],
           True, Print["ERROR", {psi1,psi2}]; newvertex = {Join[ferm2, {anti[psi1],psi2}], newvertex}
      ];

     (* All the vertices now have the form  {.....,   psi1bar, psi2 }, i.e., the particles are in the order required by the convention. *)
     (* So all left to do is to take care that all Gamma matrices are in the correct order. *)
     nf2 = Length[newvertex[[1]]];
     nfbar1 = nf2 - 1; 
     newvertex[[2]] = Expand[newvertex[[2]]] /. {PP_[inds___, Index[Spin, Ext[nf2]], Index[Spin, Ext[nfbar1]]] :> PRIVATE`CGa[PP[inds, Index[Spin, Ext[nfbar1]], Index[Spin, Ext[nf2]]]]};
     (* Return and Exit *)
     Return[newvertex];

];
     
     


(* ::Section::Closed:: *)
(*OptimizeGhostChains*)


(* ::Text:: *)
(*Same as OptimizeFermionChains, but for ghost fields*)


OptimizeGhostChains[{parts_, vertex_}] := Block[{ferm2, newvertex,
     GFieldQ = GhostFieldQ[#] === True &,
     GhostQ = (GhostFieldQ[#]===True) &&(AntiFieldQ[#]=!=True)&,
     AGhostQ = (GhostFieldQ[#]===True) &&(AntiFieldQ[#]===True)&},
 
    (* If more than one ghost chain, exit *)
    If[Count[parts,{_?(GFieldQ),_}] != 2,
       Return[{parts, vertex}]
      ];

    
     (* Otherwise reorder the ghosts into the last position *)
     ferm2 = parts //. {fields1___, {psi_?GFieldQ, ipsi_}, {phi_?(Not[GFieldQ[#]]&), iphi_}, field2___} :> {fields1, {phi, iphi}, {psi, ipsi}, field2};


     (* We also want the antighost before the ghost *)
     ferm2 = ferm2 /. {fields1___, {psi_?GhostQ, ipsi_}, {phi_?AGhostQ, iphi_}} :> {fields1, {phi, iphi}, {psi, ipsi}};

      (* Relabel Ext[] *)
      newvertex = RelabelExt[{ferm2, vertex}];
    
     (* Return and Exit *)
     Return[newvertex];

];
     
     





(* ::Section::Closed:: *)
(*JoinRuleLists*)


(* ::Text:: *)
(*JoinRuleLists[ list1, list2, ...]   checks if the lists are DispatchTables. If so, transform them into normal lists, and then join, and retransform back into Dispatch.*)


JoinRuleLists[lists__] := Block[{newlists},

    newlists = If[Head[#] === Dispatch, #[[1]], #]& /@ {lists};

    newlists = Dispatch[Join @@ newlists];

    Return[newlists];

];


(* ::Section:: *)
(*IndexExpandParameter[param]*)


Options[IndexExpandParameter] = {DeclareAbbreviations -> True};


FR$IndexExpandParameterAbbr = {};
FR$IndexExpandParameterParamRules = {};
FR$IndexExpandZeroValues = {};


IndexExpandParameter[param_, OptionsPattern[]] := Block[{par = param, 
   newabbr, oldabbr,
   newiparams, zeroentries = {}},

   par = PerformIndexSum[par];

   newabbr = Complement[FR$AbbIndexSum, FR$IndexExpandParameterAbbr];
   FR$IndexExpandParameterAbbr = Join[FR$IndexExpandParameterAbbr, newabbr];


   (* If no new abbreviation was created, we are done *)
   If[newabbr === {},
      IParamList = IParamList;
      ParamList = ParamList;
      par = par //. FR$IndexExpandZeroValues //. FR$IndexExpandParameterParamRules;
      Return[par];
     ];


   (* If there is a new abbreviation, then we need to declare them *)
   Print["Abbreviations created. They can be called from Abbreviations[]."];
   zeroentries = CreateISUMAbbr[newabbr, Style -> "E"];
    FR$IndexExpandZeroValues = Join[FR$IndexExpandZeroValues, zeroentries];
    FR$IndexExpandParameterParamRules = JoinRuleLists[FR$IndexExpandParameterParamRules, FR$AbbIndexSumParamRules];

   par = par //. FR$IndexExpandZeroValues //. FR$IndexExpandParameterParamRules;
   If[OptionValue[DeclareAbbreviations],
      newiparams = CreateIParamsFromISUMs/@FR$AbbIndexSumExpanded;
      IParamList = Join[IParamList, newiparams];
      ParamList = Join[ParamList, Insert[#, Int, 2]& /@ newiparams];
      Print["Abbreviations have been declared."];
     ];

    (* Reinitialize *)
    FR$AbbIndexSumExpanded = {};
    FR$AbbIndexSumParamRules = {};

    (* Exit *)
    Return[par];

];
    

   
   

   
