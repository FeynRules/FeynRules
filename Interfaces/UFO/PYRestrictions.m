(* ::Package:: *)

(* ::Title:: *)
(*PY Restrictions*)


PYLoadRestrictionFile[file_String] := Block[{M$Restrictions},

    Get[file];
    If[Not[ValueQ[M$Restrictions]] || M$Restrictions === {},
       Message[PYRest::NoRestric]; 
       Return[{file,{}}];
      ];

    Return[{file, M$Restrictions}];
];


WriteOneSetOfRestrictions[filename_String,list_List] := Block[{intparams, eparams, $Dum,
   invparamrules = Reverse /@ ParamRules,
   restrictions = list[[2]], file = list[[1]],
   checkint, IsInternal, restlist, ConvertToPY, 
   stringname = StringReplace[list[[1]], {".rst" -> ""}], objname, out
   },

   ConvertToPY[{a_,b_}] := {"Param." <> CreateObjectParticleName[ToString[a]], PYString[PythonForm[b]]};

   intparams = Join[First /@ IParamList, 
                  Union[Cases[MassList[[2]],{__,Internal}][[All,2]]], 
                  Union[Cases[WidthList[[2]],{__,Internal}][[All,2]]]
                 ];
   eparams = Join[Flatten[$Dum @@@ #& /@ (Last /@ EParamList)] //. $Dum[_,b_]:>First[b], 
                  Union[DeleteCases[MassList[[2]],{__,Internal}][[All,2]]], 
                  Union[DeleteCases[WidthList[[2]],{__,Internal}][[All,2]]]
                 ];

   intparams = Join[intparams, intparams //. invparamrules];
   eparams = Join[eparams, eparams //. invparamrules];

   (* Check if there are rules that apply for internals *)
   checkint = DeleteCases[{#,#//.restrictions}& /@ intparams,{a_,a_}];
   IsInternal = (Length[checkint] =!= 0);
    
   (* Get Rules table *)
   restlist = DeleteCases[{#,#//.restrictions}& /@ eparams,{a_,a_}];
   restlist = Union[restlist //. ParamRules];

   restlist = ConvertToPY /@ restlist;
   restlist = PYTuple /@ restlist;
   
   objname = CreateObjectParticleName[stringname];
   
   out = OpenAppend[filename];
   WriteString[out, objname, " = Restriction(name = ", PYString[stringname], ",\n"];
   WriteString[out, Sequence @@ Table[" ", {StringLength[objname] + 15}], "restriction = "];
   Close[out];
   WritePYLongTuple[filename, restlist, StringLength[objname] + 31];
   out = OpenAppend[filename];
   WriteString[out, "\n",StringJoin @@ Table[" ",{23}], ")\n\n"];
   Close[out];

   Return[IsInternal];
];
   
   


WritePYRestrictions[directory_String, list_List] := Block[{allfiles, out},

   (* Get the inputs *)
   SetDirectory[directory];
   allfiles = DeleteCases[PYLoadRestrictionFile /@ Union[list], {_,{}}];
   ResetDirectory[];

   DeleteFileIfExists["restrictions.py"];
   out = OpenWrite["restrictions.py"];
   WritePYFRHeader[out];
   WriteString[out, "from object_library import all_restrictions, Restriction\n"];
   WriteString[out, "import parameters as Param\n\n"];
   Close[out];
   allfiles = And @@ (WriteOneSetOfRestrictions["restrictions.py",#]& /@ allfiles);
   If[allfiles,
      Message[UFORestrict::NoInternal]
     ];
];
 
