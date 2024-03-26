(* ::Package:: *)

(* ::Title:: *)
(*Generic Interface: Python definitions*)


(* ::Section:: *)
(*PY output format*)


(* ::Subsection:: *)
(*WritePYCommentLine*)


(* ::Text:: *)
(*WritePYCommentLine[ "file" ,  "string" ] write the string*)
(**)
(*"# string \n" *)
(**)
(*to the file "file" ("file" needs to be open).*)


WritePYCommentLine[file_, string_String] := WriteString[file, "# " <> string <> "\n"];


(* ::Subsection:: *)
(*WritePYFRHeader*)


(* ::Text:: *)
(*WritePYFRHeader[ "file" ] adds the following header to the file "file" (must be open already).*)
(**)
(*# This file was automatically created by FeynRules <FR$VersionNumber>*)
(*# Mathematica version: <$Version>*)
(*# Date: <DateString[]>*)


WritePYFRHeader[file_] := Block[{},

   WritePYCommentLine[file, "This file was automatically created by FeynRules "<> FR$VersionNumber];
   WritePYCommentLine[file, "Mathematica version: "<> $Version];
   WritePYCommentLine[file, "Date: "<> DateString[]];
   WriteString[file, "\n\n"];

];



(* ::Subsection:: *)
(*PYDicEntry*)


(* ::Text:: *)
(*PYDicEntry["name", "entry"] creates the Python dictionary entry   "name:entry"'*)


PYDicEntry[name_String,entry_String] := name <> ":" <> entry;


(* ::Subsection:: *)
(*PYShortDictionary*)


(* ::Text:: *)
(*PYShortDictionary[ matrix ] creates from matrix  a one-line python dictionary*)
(**)
(*"{m11:m12, m21:m22, ...}"*)


PYShortDictionary[matrix_List] := Block[{dic},

   (* Create the PY dic entries *)
   dic = PYDicEntry @@@ matrix;

   (* Add commas, and concateante the resulting strings *)
   dic = StringJoin @@ Riffle[dic, ","];

   (* Write out *)
   Return["{" <> dic <> "}"];

];


(* ::Subsection:: *)
(*WritePYDictionary*)


(* ::Text:: *)
(*WritePYDictionary["file", "name", list] writes out a python dictionary*)
(**)
(*name = {*)
(*   list1:...*)
(*   list2:...*)
(*   *)
(*}*)
(**)
(*to the file "file" (must be open already).*)
(*list must be a matrix of strings, each element a python dictionary entry, {name, entry}*)


WritePYDictionary[file_, name_String, list_List] := Block[{dic},

   (* Create PY Dic entries out of the list *)
   dic = PYDicEntry @@@ list;

   (* Write to file *)
   WriteString[file, name <> " = {\n"];
   WriteString[file, "   " <> # <> ",\n"]& /@ Most[dic];
   WriteString[file, "   " <> Last[dic] <> "\n"];
   WriteString[file, "}" <> "\n"];
   WriteString[file, "\n\n"];

];


(* ::Subsection:: *)
(*WritePYTuple*)


(* ::Text:: *)
(*WritePYLongTuple[file, list] writes out a python tuple*)
(**)
(*( list1, list2, ...)*)


WritePYTuple[file_String, list_List] := WriteString[file, PYTuple[list]];


(* ::Subsection:: *)
(*WritePYLongTuple*)


(* ::Text:: *)
(*WritePYLongTuple[file, list, offset] writes out a python tuple*)
(**)
(*( list1,*)
(*  list2*)
(*)*)
(**)
(*off set is the number of white space at the beginning of the new line.*)


WritePYLongTuple[filename_String, list_List, offset_:20] := Block[{entries = list,
   whitespace = StringJoin @@ Table[" ", {offset}],
   file = OpenAppend[filename]
   },

   entries = Append[Table[entries[[i]] <> ",\n", {i,Length[entries]-1}], entries[[-1]]];
   WriteString[file, "( ", entries[[1]]];
   WriteString[file, whitespace, #] & /@ Rest[Most[entries]];
   WriteString[file, whitespace, entries[[-1]], ")"];
   Close[file];

];


(* ::Subsection:: *)
(*PYString*)


(* ::Text:: *)
(*PYString["string"] return a Python string, "'string'"*)


PYString[string_String] := "'" <> string <> "'";


(* ::Subsection:: *)
(*PYTeXString*)


PYTeXString[string_String] := StringReplace[string, {"\\" -> "\\\\", "'" -> "\\'"}];


(* ::Subsection:: *)
(*PYListString*)


(* ::Text:: *)
(*PYListString[ list ] transforms the list {a, b, c, ...} into the string "a, b, c, ..." *)


PYListString[list_List] := If[Length[list] == 1,
                              MakeString @@ list,
                              StringJoin @@ Append[Most[(MakeString[#] <> ", ")& /@ list], MakeString[Last[list]]]
                              ];


(* ::Subsection:: *)
(*PYTuple*)


PYTuple[list_List] := StringJoin["( ", Sequence @@ Riffle[list, ", "], ")"];


(* ::Subsection:: *)
(*MakeString*)


(* ::Text:: *)
(*MakeString[ expr ] tests is expr is a string, and if not, transforms it into a string.*)


MakeString[expr_] := If[StringQ[expr], expr, ToString[expr]];


(* ::Subsection:: *)
(*PYList*)


(* ::Text:: *)
(*PYList[ list ] turns a list into a Python list. the output is a string.*)


PYList[{}] = "[]";


PYList[list_List] := StringJoin["[ ", PYListString[list], " ]"];


(* ::Section:: *)
(*ClassCreation*)


(* ::Subsection:: *)
(*ClassAttributeCreation*)


(* ::Text:: *)
(*ClassAttributeCreation[ "name" ,   "entry", offset ] creates the class attribute.*)
(*offset is an integer, that specifies the indentation.*)
(**)
(*(offset number of blank spaces )self.name = entry*)


ClassAttributeCreation[name_String, entry_String, offset_Integer] := If[offset == 0, "", StringJoin @@ Table[" ", {offset}]] <> name <> " = " <> entry;

ClassAttributeCreation[name_String, entry_PY$DicFirst, offset_Integer] := If[offset == 0, "", StringJoin @@ Table[" ", {offset}]] <> name <> " = {" <> entry[[1]];
ClassAttributeCreation[name_String, entry_PY$DicFirstLast, offset_Integer] := If[offset == 0, "", StringJoin @@ Table[" ", {offset}]] <> name <> " = {" <> entry[[1]] <> "}";
ClassAttributeCreation[name_String, entry_PY$Dic, offset_Integer]      := If[offset == 0, "", StringJoin @@ Table[" ", {offset + StringLength[name] + 4}]] <> entry[[1]];
ClassAttributeCreation[name_String, entry_PY$DicLast, offset_Integer]      := If[offset == 0, "", StringJoin @@ Table[" ", {offset + StringLength[name] + 4}]] <> entry[[1]] <> "}";


(* ::Subsection:: *)
(*WritePYObject*)


(* ::Text:: *)
(*WritePYObject["file", "class_name", "class",  list]  writes *)
(**)
(**)
(*name  =  class(list11 = list12,*)
(*                        list21 = list22,*)
(*                        list31 = list32,*)
(*                        ...)*)
(**)
(*to the file "file" (must be open already).*)
(*list must be a matrix of strings, each element a python dictionary entry, {name, entry}*)


WritePYObject[file_, name_String, class_String, list_List] := Block[{
    firstline = name <> " = " <> class <> "(",
    offset, bulk, lastline},

    (* We need to treat the first and the last line separately:
       1st line: needed to determine the off set.
       last line: no comma, but close parenthesis 
     *)

     offset = StringLength[firstline];
     firstline = firstline <> ClassAttributeCreation[list[[1,1]], list[[1,2]], 0] <> ",\n";
     bulk = Flatten[(ClassAttributeCreation[#1, #2, offset] <> ",\n")& @@@ Rest[Most[list]]];
     lastline = ClassAttributeCreation[Last[list][[1]], Last[list][[2]], offset] <> ")\n";
     
     (* WriteOut *)
     WriteString[file, firstline];
     WriteString[file, #] & /@ bulk;
     WriteString[file, lastline];

];


(* ::Section:: *)
(*CreateObjectParticleName*)


(* ::Text:: *)
(*CreateObjectParticleName[ "name" ] return a string, corresponding to our convention for particle object names. If the first element is one of these, then P is added.*)
(**)
(*+     ->   __plus__*)
(*-     ->   __minus__*)
(*@     ->   __at__*)
(*!     ->   __exclam__*)
(*?     ->   __quest__*)
(**     ->   __star__*)
(**)
(**)


PY$SpecialCharactersReplace = {"+" ->  "__plus__", 
                   "-" ->  "__minus__",
                   "@" ->  "__at__",
                   "!" ->  "__exclam__",
                   "?" ->  "__quest__",
                   "*" ->  "__star__",
                   "~" ->  "__tilde__"};


PY$SpecialCharacters = (#[[1]]&) /@ PY$SpecialCharactersReplace;


CreateObjectParticleName[name_String] := Block[{newname, 
   firstcarac = StringTake[name, 1]
   },

   (* first, cure the 1st element *)
   
   newname = If[MemberQ[PY$SpecialCharacters, firstcarac],
               "P" <> (firstcarac /. PY$SpecialCharactersReplace) <> StringDrop[name, 1],
               (* else *)
               name];

   (* now, replace the rest, as well as the 'name' -> name *)
   newname = StringReplace[newname, Append[PY$SpecialCharactersReplace, "'" -> ""]];

   (* Return and exit *)
   Return[newname];

];
      


(* ::Section:: *)
(*Testing*)


(* ::Subsection:: *)
(*DoubleEntriesQ*)


(* ::Text:: *)
(*DoubleEntriesQ[list, logfile, message] tests wether there are double entries in list. If so, a message is written to logfile and True is returned. *)


DoubleEntriesQ[list_List, message_String] := Block[{bool},

    (* The test *)
     bool = (Length[list] == Length[Union[list]]);

     If[Not[bool],
        AppendTo[GenInt$LogFile, message]
        ];

     Return[bool]
 
   ];
      


(* ::Subsection:: *)
(*TestQ*)


(* ::Text:: *)
(*TestQ[ test,  x, logfile, message ] performs the boolean test test[x] and returns the result. If it fails,  message is added to logfile.*)
(*If test[x] does not evaluate to True/False, then False is returned.*)
(**)
(*TestQ[ test,  x, logfile, message1, message ] performs the boolean test test[x] and returns the result. If it fails,  message2 is added to logfile, otherwise message1.*)
(*If test[x] does not evaluate to True/False, then False is returned.*)
(**)
(**)


TestQ[test_, x_, message_] := Block[{bool},
   
    (* The test *)
    bool = (test[x] === True);

    (* Write to logfile *)
    If[Not[bool],
       AppendTo[GenInt$LogFile, message];
      ];

     (* Return and exit *)
     Return[bool]
];

TestQ[test_, x_, message1_, message2_] := Block[{bool},
   
    (* The test *)
    bool = (test[x] === True);

    (* Write to logfile *)
    If[Not[bool],
       AppendTo[GenInt$LogFile, message2],
       AppendTo[GenInt$LogFile, message1]
      ];

     (* Return and exit *)
     Return[bool]
];


(* ::Section:: *)
(*CreateDialogBox*)


CreateDialogBox[list_List] := Block[{templist},

      (* Concatenate the elements in the list, and insert linebreaks at the end of each line, except the last one *)
      templist = StringJoin @@ Append[(# <> "\n")& /@ Most[list], Last[list]];

      (* Create the dialog box *)
      If[!ChoiceDialog[templist,{"Ok"->True, "Abort"->False}],
         ResetDirectory[]; Abort[]
        ]

];


(* ::Section:: *)
(*Creating and deleting files*)


(* ::Subsection:: *)
(*FileExistsQ*)


(* ::Text:: *)
(*FileExistsQ must be defined for version lower than V7 *)


If[$VersionNumber < 7,
   FileExistsQ[file_String] := Length[FileNames[file]] =!= 0
  ];


(* ::Subsection:: *)
(*DeleteFileIfExists*)


(* ::Text:: *)
(*DeleteFileIfExists[ "file" ] checks whether "file" exists, and if yes, deletes the file.*)


DeleteFileIfExists[name_String] := If[FileExistsQ[name], DeleteFile[name]];


(* ::Subsection:: *)
(*SetOutputDirectory*)


(* ::Text:: *)
(*SetOutputDirectory[ "dir" ] checks if the directory "dir" exists in the current working diretory:*)
(*- if yes, then it goes into this directory.*)
(*- if no, then it creates "dir" in the current working directory, and then goes into this directory.*)


SetOutputDirectory[dir_String] := Block[{},

    (* Check if directory exists. If not, create it. *)
    If[FileNames[dir] === {},
       Print["Creating directory..."];
       CreateDirectory[dir]
       ];
 
     (* Go into the directory *)
     SetDirectory[dir];

     (* Return and exit *)
     FRDebug["SetOutputDirectory", Directory[]];
     Return[Directory[]]

];

   


(* ::Section:: *)
(*Python Format*)


PY$InputForm = {Complex[aa_,b_] :> aa + b * PYComplexI, 
                Power[x_, 1/2] :> FRSqrt[x],
                Power[x_, -1/2] :> 1/FRSqrt[x],
                E^aa_.:>FRExp[aa],
                Less[aa_,b_]:>PythonForm[aa]<>"<"<>PythonForm[b],
                LessEqual[aa_,b_]:>PythonForm[aa]<>"<="<>PythonForm[b],
                Greater[aa_,b_]:>PythonForm[aa]<>">"<>PythonForm[b],
                GreaterEqual[aa_,b_]:>PythonForm[aa]<>">="<>PythonForm[b],
                Equal[aa_,b_]:>PythonForm[aa]<>"=="<>PythonForm[b],
                Unequal[aa_,b_]:>PythonForm[aa]<>"<="<>PythonForm[b],
                If[aa_,b_,c_]:>"( "<> PythonForm[b]<>" if "<>PythonForm[aa]<>" else "<>PythonForm[c]<>" )"};
PY$OutputForm = {"PYTuple" -> "", "ADDSLASH" -> "\\'","\""->"",".and."->" and ",".or."->" or "};

PY$IndexForm = {Index[_, Ext[k_]] :> k,
                Index[_, Ext[k_, l_]] :> (1000 l + k),
                Index[Spin|Lorentz|Colour|Gluon|Sextet, Int[k_]] :> -k
                };

(*PY$IndexForm = {Index[_, Ext[k_]] :> k,
                Index[_, Ext[k_, l_]] :> (1000 l + k),
                Index[Spin, Int[k_]] :> Symbol["ADDSLASHs" <> ToString[k] <> "ADDSLASH"],
                Index[Lorentz, Int[k_]] :> Symbol["ADDSLASHm" <> ToString[k] <> "ADDSLASH"],
                Index[Colour, Int[k_]] :> Symbol["ADDSLASHi" <> ToString[k] <> "ADDSLASH"],
                Index[Gluon, Int[k_]] :> Symbol["ADDSLASHa" <> ToString[k] <> "ADDSLASH"],
                Index[Sextet, Int[k_]] :> Symbol["ADDSLASHk" <> ToString[k] <> "ADDSLASH"]};*)


(* ::Subsection:: *)
(*SetPythonForm*)


(* ::Text:: *)
(*SetPythonForm[x, y] sets the Python form of x and y, where x is a symbol and y is a string.*)
(** x -> PY$x is added to the list PY$InputForm.*)
(** "PY$x" -> y is added to PY$OutputForm*)


SetPythonForm[x_Symbol, y_String] := Block[{xy, xystring},
   
    xystring = "FR$" <> ToString[x] <> "$FR";
    xy = Symbol[xystring];

    AppendTo[PY$InputForm, x -> xy];
    AppendTo[PY$OutputForm, xystring -> y]

];


(* ::Subsection:: *)
(*PythonForm*)


(* ::Text:: *)
(*PythonForm[ expr ] returns the Python form of an algebraic expression, taking into account the definitions made by SetPythonForm.*)


PythonForm[expr_] := Block[{string},
    
    (* Reset the counter for internl indices*)
    PY$IntIndexCoutner = 1; 
  
    string = StringReplace[ToString[expr /. {L$FV[k_, mu_] :> L$FV[mu, k]} //. L$Ga[5, inds___] :> L$Ga5[inds] //. PY$IndexForm /. ParamRules //. PY$InputForm, FortranForm], PY$OutputForm];

    (* Reset the counter for internl indices*)
    PY$IntIndexCoutner = 1; 

    Return[string];

];


(* ::Subsection:: *)
(*cmath functions*)


SetPythonForm[E, "cmath.e"];
SetPythonForm[Pi, "cmath.pi"];


SetPythonForm[Exp, "cmath.exp"];
SetPythonForm[FRExp, "cmath.exp"];
SetPythonForm[Log, "cmath.log"];

SetPythonForm[FRSqrt, "cmath.sqrt"];

SetPythonForm[ArcCos, "cmath.acos"];
SetPythonForm[ArcSin, "cmath.asin"];
SetPythonForm[ArcTan, "cmath.atan"];

SetPythonForm[Cos, "cmath.cos"];
SetPythonForm[Sin, "cmath.sin"];
SetPythonForm[Tan, "cmath.tan"];

SetPythonForm[ArcCosh, "cmath.acosh"];
SetPythonForm[ArcSinh, "cmath.asinh"];
SetPythonForm[ArcTanh, "cmath.atanh"];

SetPythonForm[Cosh, "cmath.cosh"];
SetPythonForm[Sinh, "cmath.sinh"];
SetPythonForm[Tanh, "cmath.tanh"];

SetPythonForm[Abs, "abs"];


(* ::Subsection:: *)
(*Non cmath functions*)


PY$NewCMathFunctions = {"complexconjugate", "re", "im", "csc", "sec", "acsc", "asec", "cot"};


SetPythonForm[PYComplexI, "complex(0,1)"];

SetPythonForm[Conjugate, "complexconjugate"];
SetPythonForm[Im, "im"];
SetPythonForm[Re, "re"];
SetPythonForm[Csc,"csc"];

SetPythonForm[Sec, "sec"];
SetPythonForm[ArcSec, "asec"];

SetPythonForm[Sech, "sech"];
SetPythonForm[ArcSech, "asech"];

SetPythonForm[Cot, "cot"];

SetPythonForm[HeavisideTheta, "HeavTheta"];




(* ::Subsection:: *)
(*Color objects*)


SetPythonForm[C$T, "T"];
SetPythonForm[C$f, "f"];
SetPythonForm[C$IndexDelta, "Identity"];
SetPythonForm[C$dSUN, "d"];
SetPythonForm[C$Eps, "Epsilon"];
SetPythonForm[C$EpsBar, "EpsilonBar"];
SetPythonForm[C$T6, "T6"];
SetPythonForm[C$K6, "K6"];
SetPythonForm[C$K6bar, "K6Bar"];


(* ::Subsection:: *)
(*Lorentz objects*)


SetPythonForm[L$Ga, "Gamma"];
SetPythonForm[L$Ga5, "Gamma5"];
SetPythonForm[L$ProjP, "ProjP"];
SetPythonForm[L$ProjM, "ProjM"];
SetPythonForm[L$Sig, "Sigma"];
SetPythonForm[L$IndexDelta, "Identity"];

SetPythonForm[L$ME, "Metric"];
SetPythonForm[L$Eps, "Epsilon"];
SetPythonForm[L$FV, "P"];




(* ::Subsection:: *)
(*NLO*)


SetPythonForm[FR$MU, "MU_R"];
SetPythonForm[FR$Cond,"cond"];
SetPythonForm[RenormLog,"reglog"];


(* ::Section:: *)
(*WritePYModelInformation*)


WritePYModelInformation[] := Block[{

    authors = Authors /. M$Information,
    version = Version /. M$Information,
    emails   = Emails  /. M$Information,

    file = OpenAppend["__init__.py"]

    },


    If[VectorQ[authors],
       authors = StringJoin @@ Riffle[authors, ", "];
      ];
    If[VectorQ[emails],
       emails  = StringJoin @@ Riffle[emails,  ", "];
      ];

    WriteString[file, "\n"];
    WriteString[file, "\n"];
    WriteString[file, "__author__ = \"", authors, "\"\n"];
    WriteString[file, "__version__ = \"", version, "\"\n"];
    WriteString[file, "__email__ = \"", emails, "\"\n"];

    Close[file];

];
    
    


(* ::Section::Closed:: *)
(*Debugging*)


FRDebug[message_String, var_] := If[$Debug, Print["*** ", message]; Print["    ", var]];


(* ::Section:: *)
(*QNConservationToLogFile*)


(* ::Text:: *)
(*QNConservationToLogFile[ qnconservation ], where qnconservation is the output of CheckQuantumNumberConservation, writes the results of the quantum number conservation to the logfile.*)


QNConservationToLogFile[qncons_List] := Block[{

     qnumbers = Union[#[[2]]& /@ qncons],
     working

     },

     Do[(*Read out all vertices corresponding to a given qnumber *)
        working = Cases[qncons, {_, qnumbers[[$ii]],_}];
        (* Remove those vertices that are conserved *)
        working = DeleteCases[working, {__, True}]; 
        (* If the list is now empty, this quantum number entirely conserved *)
        If[working === {},
           AppendTo[GenInt$LogFile, "      - Quantum number " <> ToString[qnumbers[[$ii]], InputForm] <> " conserved in all vertices."],
           (* else *)
           working = StringJoin["          ", #]& /@ (ToString[#, InputForm]& /@ (MakeIdenticalFermions[First /@ working]));
           AppendTo[GenInt$LogFile, "      - Quantum number " <> ToString[qnumbers[[$ii]], InputForm] <> " NOT conserved in the following vertices"];
           AppendTo[GenInt$LogFile, #]& /@ working;
          ],
        {$ii, Length[qnumbers]}];
];

