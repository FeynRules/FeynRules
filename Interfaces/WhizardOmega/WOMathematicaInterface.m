(* ::Package:: *)

(*

   $Id: WOMathematicaInterface.m 3666 2012-01-14 17:03:09Z cnspeckn $

   This is a mathematica model file generator for WHIZARD / O'Mega; its task
   is to translate an abstraction of the model data into a model file package.
   Other codes can tap into this library and use it to implement interfaces to WO.
   As the code is a near 1-to-1 copy of the FR interface atm, this "abstraction"
   currently are the FR data structures, but I plan to refine this as I find time.

   Christian Speckner, 2009 - 2011
*)

(*
TODO
   - Sanitize the handling of kinds in FORTRAN real constants.
   - Remove "open coupling" and qualify all identifiers from this module instead
   - Add verbosity, more sensible messages (don't complain about every ghost vertex etc.) *partly done*
   - Make sure that all functions commonly appearing in parameters and couplings get treated correctly.
   - Allow vertices which are sums of known structures.
   - Win paths to different drives like "c:\foo" are broken atm.
*)

(*
   "API" version to ensure that the top level code gets what it deserves
*)
WO`APIversion = 3;

(*
The backend can query the revision
*)
WO`BackendRevision = "$Id: WOMathematicaInterface.m 3666 2012-01-14 17:03:09Z cnspeckn $";

(*
   Option handling: all options are defined within the WO namespace - some magic must
   be played to create copies in other namespaces if desired.
*)

Options[WO`WriteOutput] = {
   WO`WOVertexList -> {},
   WO`WOModelName -> "unknown",
   WO`WOMaxNcf -> 4,
   WO`WOGauge -> WO`WOUnitarity,
   WO`WOGaugeParameter -> "Rxi",
   WO`WOWhizardVersion -> "3.0.0",
   WO`WOVerbose -> False,
   WO`WOAutoGauge -> True,
   WO`WOMaxCouplingsPerFile -> 500,
   WO`WORunParameters -> {aS, G},
   WO`WOOutputDir -> Null,
   WO`WOOplotter -> False,
   WO`WOFast -> True,
   WO`WOMassList -> {},
   WO`WOWidthList -> {},
   WO`WOEParamList -> {},
   WO`WOIParamList -> {},
   WO`WOPartList -> {},
   WO`WOProgress -> 10,
   WO`WOExtraComment -> ""
};

Options[WO`WriteExtParams] = {
   WO`WOWhizardVersion -> "3.0.0",
   WO`WOEParamList -> {},
   WO`WOModelName -> "unknown",
   WO`WOMassList -> {},
   WO`WOWidthList -> {}
};

Protect @@ (Options[WP`WriteOutput] /. Rule[a_, b_] :> a);
Protect[WO`WORxi, WO`WOFeynman, WO`WOUnitarity];

WO`WriteOutput::usage = "Write O'Mega / WHIZARD model files.";
WO`WriteWOExtParams::usage = "Write external parameters to a file suitable for inclusion by WHIZARD.";
WO`WOVertexList::usage = "Directly pass a vertex list to WriteWOOutput; overrides any Lagrians";
WO`WOModelName::usage = "override model name; will be lowercased.";
WO`WOMaxNcf::usage = (""
   <> "The maximum number of color flows provided for in "
   <> "O'mega. Irrelevant for WHIZARD 2+. Default: 4");
WO`WOGauge::usage = (""
   <> "Choose a gauge, choices: WOUnitarity (default), WOFeynman, WORxi");
WO`WOUnitarity::usage = WO`WOFeynman::usage = "see WOGauge";
WO`WORxi::usage = (""
   <> "see WOGauge; if you specify Rxi gauge, then you MUST implement a parameter that "
   <> "represents the gauge parameter \\xi; you can pass it to WriteWOOutput via "
   <> "WOGaugeParameter (default: \"Rxi\").");
WO`WOGaugeParameter::usage = "see WORxi, WOGauge";
WO`WOWhizardVersion::usage = (""
   <> "Version of WHIZARD / O'Mega to generate code for; possible values are:\n"
   <> "   \"1.92\" : <= 1.92\n"
   <> "   \"1.93\" : >= 1.93\n"
   <> "   \"1.96\" : >= 1.96\n"
   <> "   \"2.0\"  : 2.0 - 2.0.2\n"
   <> "   \"2.0.3\": 2.0.3 - 2.2.2\n"
   <> "   \"2.2.3\": 2.2.3\n"
   <> "   \"2.3.0\": 2.3.0\n"
   <> "   \"2.4.0\": 2.4.0\n"
   <> "   \"3.0.0\": 3.0.0 (default)");
WO`WOVerbose::usage = (""
   <> "Verbose output. At the moment, this enables more detailed information "
   <> "on skipped vertices. Default: False");
WO`WOAutoGauge::usage = (""
   <> "Automagically assign goldstone boson masses and add gauge parameter if necessary. Default: True");
WO`WOMaxCouplingsPerFile::usage = (""
   <> "Maximum number of couplings that get calculated in one FORTRAN module. Default: 500");
WO`WORunParameters::usage = (""
   <> "A list of all derived parameters which should be evolved together with the strong coupling. "
   <> "Default: {aS, G}");
WO`WOOplotter::usage = (""
   <> "Generate model files for oplotter. Default: false");
WO`WOFast::usage = (""
   <> "Avoid time-consuming checks when translating the vertices, at the danger of missing "
   <> "some structures. Retry with this option enabled if you encounter unidentified vertices. Default: True");
WO`WOOutputDir::usage = "Set the output directory. Default: derifed from model name";
WO`WOExtraComment::usage = "Additional comment prepended to the generated files. Default: empty";
WO`WOProgress::usage = "Granularity for the progress meter. Default: 10"

(* Create a directory if nonexistent *)
WO`CautiousMd[dir_] := If[Length[FileNames[
      StringReplace[dir, RegularExpression[WO`fileSlashRE <> "\\s*$"] -> ""]]] == 0,
   If[CreateDirectory[dir] == $Failed, Throw["FATAL: unable to create directory " <> dir, WO`EAbort]]];

(* Global setup.
   For clarities sake, all global variables should be defined here, even if they are
   overridden later. *)
WO`GlobalSetup := Module[{},
   WO`cpldeflist = {};
   WO`maxarity = 3;
   WO`havemajoranas = False;
   WO`taglist = {};
   WO`masslist = {};
   WO`widthlist = {};
   WO`PartList = {};
   WO`IParamList = {};
   WO`EParamList = {};
   WO`ncfmax = 4;
   WO`maxiter = 1000;
   WO`runningcouplings = {};
   WO`appendAlphas = False;
   WO`gauge = WO`WOUnitarity;
   WO`gsym = "Rxi";
   WO`whizv = "3.0.0";
   WO`verbose = False;
   WO`autogauge = False;
   WO`MaxCouplingsPerFile = 500;
   WO`RunParameters = {};
   WO`fast = True;
   WO`vlist = {};
   WO`fileheader = "";
   WO`progress = 10;
   WO`WOForm = False;
   Clear[WO`hash];
   If[$OperatingSystem === "Windows",
      WO`fileSlash = "\\";
      WO`fileSlashRE = "\\\\";
      WO`absolutePathPattern = "^?:\\.*"
   ,
      WO`fileSlash = "/";
      WO`fileSlashRE ="/"; 
      WO`absolutePathPattern = "^/.*"
   ];
   WO`f2sin = Join[
      Rule[RegularExpression["(^|\\W)" <> #[[1]] <> "(\\s*)\\("], "$1" <> #[[2]] <> "$2("]& /@
      {   {"Sqrt", "sqrt"}, {"Abs", "abs"}, {"WOxexp", "exp"}, {"Log", "log"}, {"Sin", "sin"},
         {"Cos", "cos"}, {"Tan", "tan"}, {"ACOS", "acos"}, {"ASIN", "asin"}, {"ATAN", "atan"},
         {"Sinh", "sinh"}, {"Cosh", "cosh"}, {"Tanh", "tanh"}, {"WOxsin", "sin"}, {"WOxcos", "cos"},
         {"WOxsinh", "sinh"}, {"WOxcosh", "cosh"}} /. Rule -> RuleDelayed
   ,
      Rule[RegularExpression["(^|\\W)" <> #[[1]] <> "(\\W|$)"], "$1" <> #[[2]] <> "$2"]& /@
      {   {"Pi", "pi"} } /. Rule -> RuleDelayed
   ];
   WO`StringRules =
      Rule[RegularExpression[#[[1]]], #[[2]]]& /@ {
         (* + is replaced by "plus" *)
         {"^\\+$", "plus"}, {"^\\+", "plus_"}, {"\\+$", "_plus"}, {"\\+", "_plus_"},
         (* - is replaced by "minus" *)
         {"^\\-$", "minus"}, {"^\\-", "minus_"}, {"\\-$", "_minus"}, {"\\-", "_minus_"},
         (* ~ is replaced by "bar" *)
         {"^~$", "bar"}, {"^~", "bar_"}, {"~$", "_bar"}, {"~", "_bar_"},
         (* leading digits are moved to the end *)
         {"^(\\d+)(_*)(.*)$", "$3$2$1"},
         (* All other non-word characters, including whitespaces, are replaced by "_" *)
         {"[^\\w\\+\\-~]", "_"},
         (* Multiple underscores are concatenated. *)
         {"_{2}", "_"},
         (* Leading underscores are moved to the end. *)
         {"^(_+)(.*)$", "$2$1"}} /. Rule -> RuleDelayed;
   Print["WOMathematicaInterface.m running, revision: $Id: WOMathematicaInterface.m 3666 2012-01-14 17:03:09Z cnspeckn $"];
];

(* Conditionally redefine FortranForm *)
Unprotect[Re];
Format[Re, FortranForm] /; WO`WOForm := REAL;
Protect[Re];
Unprotect[Im];
Format[Im, FortranForm] /; WO`WOForm := AIMAG;
Protect[Im];
Unprotect[ArcTan];
Format[ArcTan,FortranForm] /; WO`WOForm := ATAN;
Protect[ArcTan];
Unprotect[ArcSin];
Format[ArcSin,FortranForm] /; WO`WOForm := ASIN;
Protect[ArcSin];
Unprotect[ArcCos];
Format[ArcCos,FortranForm] /; WO`WOForm := ACOS;
Protect[ArcCos];
Unprotect[ArcCot];
Format[ArcCot,FortranForm] /; WO`WOForm := ACOT;
Protect[ArcCot];
Format[FortranSec[x_], FortranForm] /; WO`WOForm := 1/FortranCos[x];
Format[FortranCsc[x_], FortranForm] /; WO`WOForm := 1/FortranSin[x];
Format[FortranSin[x_], FortranForm] /; WO`WOForm := Sin[x];
Format[FortranCos[x_], FortranForm] /; WO`WOForm := Cos[x];
Unprotect[Csc];
Format[Csc[x_], FortranForm] /; WO`WOForm := FortranCsc[x];
Protect[Csc];
Unprotect[Sec];
Format[Sec[x_], FortranForm] /; WO`WOForm := FortranSec[x];
Protect[Sec];
Unprotect[Conjugate];
Format[Conjugate, FortranForm] /; WO`WOForm := conjg;
Protect[Conjugate];
Unprotect[Sqrt, Power];
Format[Sqrt[x_Integer], FortranForm] /; WO`WOForm := Format["Sqrt (" <> ToString[N[x]] <> ")", OutputForm];
Protect[Sqrt, Power];


WO`WriteOutput[options___]:=Module[{dirName, modelname, onames, frpars, Addpar,
   reclimit, itlimit, gsymfixed, opts, omegadir, whizdir, opldir, oplotter, WOAddECharge},
   (* Global initializations. *)
   WO`GlobalSetup[];

   (* Insert default options *)
   opts = Join[{options}, Select[Options[WO`WriteOutput], FreeQ[#[[1]]& /@ {options}, #[[1]]]&]];

   (* Check for unknown options. *)
   onames = Select[#[[1]]& /@ opts, FreeQ[#[[1]]& /@ Options[WO`WriteOutput], #]&];
   If[Length[onames] > 0,
      Print["ERROR: WO`WriteOutput: unknown options: " <> WO`Concat[ToString /@ onames, " , "]];
      Return[Null];
   ];

   (* Handle options; model short name *)
   modelname = WO`SanitizeString[ToLowerCase[WO`WOModelName /. opts]];
   Print["Short model name is \"" <> modelname <> "\""];
  
   (* The diverse lists *)
   WO`masslist = WO`WOMassList /. opts;
   WO`widthlist = WO`WOWidthList /. opts;
   WO`EParamList = WO`WOEParamList /. opts;
   WO`IParamList = WO`WOIParamList /. opts;
   WO`PartList = WO`WOPartList /. opts;

   (* CD, 31.03.2015: Following F. Staub's email, we add the electric charge to the particle list.
      We define a new local variable WOAddECharge in this module *)
   WOAddECharge[{class_, plist_}]  := {class, Append[#, Q[class[[2]]]]& /@ plist};
   WO`PartList = WOAddECharge /@ WO`PartList;
   (* CD: End change *)
   

   (* Maximum number of color flows *)
   WO`ncfmax = WO`WOMaxNcf /. opts;
   If[Not[IntegerQ[WO`ncfmax]] || WO`ncfmax < 2,
      Print["ERROR: WOMaxNcf must be a integer >= 2!"];
      Return[Null];
   ];
   
   (* Gauge *)
   WO`gauge = WO`WOGauge /. opts;
   If[Not[MatchQ[WO`gauge, WO`WOUnitarity|WO`WOFeynman|WO`WORxi]],
      Print["ERROR: unknown gauge requested, take a look at ?WOGauge."];
      Return[Null];
   ];
   Print["Gauge: " <> Switch[WO`gauge, WO`WOUnitarity, "Unitarity", WO`WOFeynman,
      "Feynman", WO`WORxi, "Rxi", _, "unknown - BUG!"]];
   
   (* Gauge Symbol *)
   WO`gsym = Symbol[ToString[WO`WOGaugeParameter /. opts]];
   If[WO`gauge === WO`WORxi, Print["Gauge symbol: \"" <> ToString[WO`gsym] <> "\""]];
   
   (* WHIZARD version *)
   WO`whizv = ToString[WO`WOWhizardVersion /. opts];
   Print["Generating code for WHIZARD / O'Mega version " <> WO`whizv];
   
   (* Verbosity *)
   WO`verbose = WO`WOVerbose /. opts;
   If[WO`verbose =!= True && WO`verbose =!= False,
      Print["ERROR: WOVerbose must be either True or False."];
      Return[Null];
   ];
   If[WO`verbose, Print["Verbose output enabled."]];

   (* Vertex processing progress meter *)
   WO`progress = WO`WOProgress /. opts;
   
   (* Automagic goldstone masses *)
   WO`autogauge = WO`WOAutoGauge /. opts;
   If[WO`autogauge =!= True && WO`autogauge =!= False,
      Print["ERROR: WOAutoGauge must be either True or False"];
      Return[Null];
   ];
   If[Not[FreeQ[{WO`WOFeynman, WO`WORxi}, WO`gauge]] && WO`autogauge,
      Print["Automagically assigning Goldstone boson masses..."];
   ];
   If[WO`gauge === WO`WORxi && WO`autogauge === True,
      Print["Adding gauge symbol to parameter list..."];
      gsymfixed = WO`RegisterEParam[WO`gsym, 1, "Rxi gauge parameter"];
      If [ToString[gsymfixed] =!= ToString[WO`gsym],
         Print[""
            <> "WARNING: parameter name \"" <> ToString[WO`gsym] <> "\" already taken; "
            <> "substituting \"" <> ToString[gsymfixed] <> "\"!"
         ];
         WO`gsym = gsymfixed;
      ];
   ];

   (* oplotter ? *)
   oplotter = WO`WOOplotter /. opts;
   If[oplotter =!= True && oplotter =!= False,
      Print["ERROR: WOOplotter must be either True or False"];
      Return[Null];
   ];
   
   (* Maximum number of couplings per file. *)
   WO`MaxCouplingsPerFile = WO`WOMaxCouplingsPerFile /. opts;
   If[Not[IntegerQ[WO`MaxCouplingsPerFile]] || WO`MaxCouplingsPerFile <= 0,
      Print["ERROR: WOMaxCouplingsPerFile must be an positive integer!"];
      Return[Null];
   ];
   Print["Maximum number of couplings per FORTRAN module: " <> ToString[WO`MaxCouplingsPerFile]];
   
   (* Which parameters should we run if \alpha_s is evolved? *)
   WO`RunParameters = WO`WORunParameters /. opts;

   (* Be pendantic? *)
   WO`fast = WO`WOFast /. opts;
   If[WO`fast =!= True && WO`fast =!= False,
      Print["ERROR: WOFast must be either True or False"];
      Return[Null];
   ];
   If[WO`fast, Print["Extensive lorentz structure checks disabled."]];

   (* Check if the version is OK *)
   Catch[WO`whizvn[], WO`EAbort, (Print["ERROR: invalid WHIZARD version"]; Return[])&];

   (* -> Now it is time for version-dependent checks and status messages *)
   If[WO`whizv19x[], Print["Maximum number of color flows: " <> ToString[WO`ncfmax]]];
   Switch[{oplotter, WO`whizv19x[]},
      {True, False},
         Print["WARNING: oplotter output is not supported with WOWhizardVersion->\"2.0\", disabling..."];
         oplotter = False,
      {False, True},
         Print["Oplotter output disabled."],
      {True, _},
         Print["Oplotter output enabled."]
   ];

   (* Check if the model name is OK. *)
   If[Not[StringMatchQ[ToLowerCase[modelname], RegularExpression["^fr.*"]]] && WO`whizv19x[],
      Print[""
         <> "WARNING: Short Model name doesn't start with \"fr\". O'Mega won't pick "
         <> "up this model automatically, you will have to add it to the build system "
         <> "yourself!"]
   ];
   
   (* This will be prepended as a comment to every file. *)
   WO`fileheader = ""
      <> "--------------------------------------------------------------------------------\n"
      <> "Autogenerated by the happy scrappy WHIZARD model file generator on " <> Module[{d, s},
         d = Date[];
         s = ToString[Round[d[[6]]]];
         d = ToString /@ d; 
         d[[2]] <> "/" <> d[[3]] <> "/" <> d[[1]] <> " , " <> d[[4]] <> ":" <> d[[5]] <> ":" <> s] <> "\n"
      <> "WHIZARD interface svn revision info: $Id: WOMathematicaInterface.m 3666 2012-01-14 17:03:09Z cnspeckn $\n"
      <> "Code generated for WHIZARD / O'Mega version " <> WO`whizv <> "\n"
      <> "Model name: " <> modelname <> "\n"
      <> "Gauge: " <> WO`GaugeName[] <> "\n"
      <> "Maximum number of couplings per FORTRAN module: " <> ToString[WO`MaxCouplingsPerFile] <> "\n"
      <> "--------------------------------------------------------------------------------" <> "\n"
      <> ToString[WO`WOExtraComment /. opts] <> "\n"
      <> "--------------------------------------------------------------------------------";

   (*Create directory if not present.*)
   dirName=WO`WOOutputDir /. opts /. Null :> (
      ToFileName[{Directory[], StringReplace[modelname, " " ->"-"] <> "-WO"}]);
   If[!StringMatchQ[dirName, RegularExpression[WO`absolutePathPattern]],
      dirName = ToFileName[{Directory[], dirName}]];

   (* Save and adjust iteration and recursion limits *)
   $IterationLimit = Max[itlimit = $IterationLimit, 100000];
   $RecursionLimit = Max[reclimit = $RecursionLimit, 100000];

   (* Activate our FortranForm rules *)
   WO`WOForm = True;

   Catch[
      WO`CautiousMd[dirName];
      WO`VersionCheck[dirName];

      (* Create vertex list. *)
      WO`vlist = Evaluate[ReleaseHold[WO`WOVertexList /. opts]];
      If[Head[WO`vlist] =!= List,
         Print[WO`vlist];
         Print["ERROR: vertex list invalid!"];
         Return[];
      ];
      If[Length[WO`vlist] == 0,
         Print["Vertex list empty; nothing to, aborting..."];
         Return[];
      ];

      WO`CautiousMd[omegadir = ToFileName[{dirName, "omega"}]];
      WO`CautiousMd[whizdir = ToFileName[{dirName, "whizard"}]];
      If[oplotter,
         WO`CautiousMd[opldir = ToFileName[{dirName, "oplotter"}]];
      ];
      WO`omeganame = WO`oplname = WO`whizname = ToLowerCase[modelname];

      Print["Writing files to ",dirName,"\n"];

      WO`WriteOmega[omegadir]; 
      If[oplotter, WO`WriteOpl[opldir]];
      WO`WriteWhiz[whizdir];
      WO`CopyAux[WO`InterfacePath, dirName];

      Print["Done!"];
      If[WO`whizv19x[], Print[""
         <> "Most likely your next step should to use the \"inject\" script which has been copied "
         <> "to the output directory to inject the model files into WHIZARD / O'Mega..."
      ]];
      If[WO`whizv2x[], Print [""
         <> "You should now use the configure script which has been copied to the output "
         <> "directory to configure the build system (use the variable WO_CONFIG and consult "
         <> "\"configure --help\" if WHIZARD is not installed in the system search path). Afterwards, "
         <> "the model files can be compiled and installed via \"make install\"."
      ]];

   , WO`EAbort, Function[{val, tag},
      If[Head[val] === String, Print[val]];
      Print["Aborting..."];
   ]];

   (* Restore Limits *)
   $IterationLimit = itlimit;
   $RecursionLimit = reclimit;
   WO`WOForm = False;
];

(* Get the name of a gauge. *)
WO`GaugeName[g_] := Switch[g, WO`WOUnitarity, "Unitarity", WO`WOFeynman, "Feynman", WO`WORxi, "Rxi",
   _, Throw[WO`EAbort, "BUG: WO`GaugeName: illegal gauge " <> ToString[g]]];
WO`GaugeName[] := WO`GaugeName[WO`gauge];

(* Version query helpers *)
WO`whizvn[v_] := Switch[v, "1.93", 193, "1.95", 195, "1.96", 196, "2.0", 200, "2.0.3", 203,"2.2.3",223,"2.3.0",230,"2.4.0",240,"3.0.0",300,_,
   Throw["BUG: invalid version in WO`whizvn, please report", WO`EAbort]];
WO`whizvn[] := WO`whizvn[WO`whizv];
WO`whizv30[] := WO`whizvn[] >= WO`whizvn["3.0.0"];
WO`whizv23[] := WO`whizvn[] >= WO`whizvn["2.3.0"];
WO`whizv2x[] := WO`whizvn[] >= WO`whizvn["2.0"];
WO`whizv19x[] := !WO`whizv2x[];


(* Those should be overwritten when called from FR. *)
WO`tmp = Length[Names["WO`HC"]];
If[WO`tmp < 1,
   WO`HC[p_] := Symbol[ToString[WO`hash["HC", ToString[p]]]];
];

(* Conditional Simplify and FullSimplify *)
WO`Simplify[x_, o___] := If[WO`fast, x, Simplify[x, o]];
WO`FullSimplify[x_, o___] := If[WO`fast, x, FullSimplify[x, o]];

(* Return a list of all symbols that have already been defined. *)
WO`SymbolsList[] := WO`WeedOutList[Join[
   #[[2, 1]]& /@ Flatten[#[[2]]& /@ WO`EParamList, 1],
   #[[1]]& /@ WO`IParamList,
   #[[2]]& /@ WO`masslist,
   #[[2]]& /@ WO`widthlist]];

(* Append a new parameter to WO`EParamList *)
WO`RegisterEParam[par_, val_, descr_] := Module[{EPLCopy, params, blocks, i, bname, bdata, index, pname},
   EPLCopy = WO`EParamList;
   (* All already defined parameters. *)
   params = ToUpperCase /@ (ToString /@ WO`SymbolsList[]);
   (* All already defined block names. *)
   blocks = ToString /@ (#[[1]]& /@ EPLCopy);
   If[Not[StringMatchQ[ToString[EPLCopy[[-1, 1]]], RegularExpression["^WOAUTOGEN(_\\d+)?"]]],
      (* If no WOAUTOGEN block exists, create one. As out block always will be the last one,
         we have to provide disambiguations in case another WOAUTOGENx block has been defined
         further up in the list (though admittedly unlikely), *)
      bname = "WOAUTOGEN";
      i = 0;
      While[Not[FreeQ[blocks, bname]],
         bname = "WOAUTOGEN" <> ToString[i];
         i++;
      ];
      bdata = {};
      index = 1;
   ,
      (* If the block exists, copy name and data and delete it. *)
      bname = ToString[EPLCopy[[-1, 1]]];
      bdata = EPLCopy[[-1, 2]];
      index = bdata[[-1, 1, 1]] + 1;
      EPLCopy = Take[EPLCopy, {1, -2}];
   ];
   pname = ToString[par];
   i = 0;
   (* Disambiguate the parameter name if necessary. *)
   While[Not[FreeQ[params, ToUpperCase[pname]]],
      pname = ToString[par] <> ToString[i];
      i++;
   ];
   (* Put together the block with the new parameter appended and reappend it. *)
   AppendTo[bdata, {{index}, {Symbol[pname], val, False, descr}}];
   AppendTo[EPLCopy, {Symbol[bname], bdata}];
   WO`EParamList = EPLCopy;
   (* Return the parameter name to allow the calling code to check whether it has been changed. *)
   Return[Symbol[pname]];
];

(* Append a parameter to WO`IParamList. *)
WO`RegisterIParam[par_, expr_, descr_, da_] := Module[{params, i, pname},
   params = ToUpperCase /@ (ToString /@ WO`SymbolsList[]);
   pname = ToString[par];
   If[da,
      i = 0;
      While[Not[FreeQ[params, ToUpperCase[pname]]],
         pname = ToString[par] <> ToString[i];
         i++;
      ];
   ];
   AppendTo[WO`IParamList, {Symbol[pname], expr, False, descr}];
   Return[Symbol[pname]];
];
WO`RegisterIParam[par_, expr_, descr_] := WO`RegisterIParam[par, expr, descr, True];
   
(* Append a mass to WO`masslist. *)
WO`RegisterMass[mass_, val_, pdg_, da_] := Module[{masses, i, mname},
   masses = ToUpperCase /@ (ToString /@ WO`SymbolsList[]);
   mname = ToString[mass];
   If[da,
      i = 0;
      While[Not[FreeQ[masses, ToUpperCase[mname]]],
         mname = ToString[mass] <> ToString[i];
         i++;
      ];
   ];
   AppendTo[WO`masslist,{{pdg}, Symbol[mname], val}];
   Return[Symbol[mname]];
];
WO`RegisterMass[mass_, val_, pdg_] := WO`RegisterMass[mass, val, pdg, True];

(* Sanitize a string, putting it into a form suitable for Caml and FORTRAN *)
WO`SanitizeString[s_] := WO`StringReplaceAll[s, WO`StringRules];

(* Convert a string into a comment. *)
WO`CommentMaker[s_, beg_, line_, end_] :=
   StringReplace[s, {RegularExpression["^"] -> beg, RegularExpression["\n"] -> ("\n" <> line),
      RegularExpression["$"] -> end}];

(* Replace patterns until the result doesn't change anymore. *)
WO`StringReplaceAll[s_, l_List] := FixedPoint[StringReplace[#, l]&, s];
WO`StringReplaceAll[s_, l_] := WO`StringReplaceAll[s, {l}];

(* Split a FORTRAN statement into lines of approximately 80 characters, indenting by 3 blanks and adding "&" *)
WO`FortranSplit[s_] := WO`FortranSplit[s, 80, 3];
WO`FortranSplit[str_, min_, ni_] := Module[{spacer, helper},
   spacer = StringJoin @@ Table[" ", {i, 1, ni}];

   helper[s_, l_, {}] := s;
   helper[s_, l_, {head_, tail___}] := If[
      (l + StringLength[head] <= min) || s == "",
         helper[s <> head, l + StringLength[head], {tail}]
      ,
         helper[s <> " &\n" <> spacer <> head, 0, {tail}]
   ];
   
   helper["", 0, StringSplit[str, RegularExpression["([^\\w\\.]+)"] :> "$1"]]
];


(* Split a string into lines of approximately n characters, prepending p and appending a *)
WO`StringSplit[s_, n_, p_, a_] := StringJoin @@
   StringSplit[s, {
      RegularExpression["(.{" <> ToString[n] <> "})\\s+(\\S)"] :> ("$1" <> a <> "\n" <> p <> "$2")
   ,
      "\n" -> "\n" <> p
   }];

(* Remove double occurences of list elements, based on a predicate function. *)
WO`WeedOutList[{hd_, tl__}, pred_] :=
   If[pred[hd, {tl}], Prepend[WO`WeedOutList[{tl}, pred], hd], WO`WeedOutList[{tl}, pred]];
WO`WeedOutList[{hd_}, pred_] := {hd};
WO`WeedOutList[{}, _] := {};
WO`WeedOutList[l_] := WO`WeedOutList[l, FreeQ[#2, #1]&];

(* Render a FORTRAN double precision complex number. *)
WO`FortranComplex[n_] := "(" <> ToString[Re[n]] <> "_double, " <> ToString[Im[n]] <> "_double)";

(* Extends a string using spaces. *)
WO`ExtendString[s_, n_] := If[StringLength[s] < n,
   s <> StringJoin @@ Table[" ", {i, 1, n - StringLength[s]}], s];

(* Concatenate two strings smartly, inserting a linebreak if the resulting     *
 * l line would exceed 80 characters, and optionally indenting the new line.   *
 * Lines don't get broken if this would intruduce a blank line.                *)
WO`SmartAppend[source_, target_, n_, cont_] := Module[{spacer, lastline, firstline},
   If[n > 1, spacer = StringJoin @@ Table[" ", {i, 1, n}], spacer = ""];
   lastline = StringCases[target, RegularExpression["[^\n]*$"]][[1]];
   firstline = StringCases[source, RegularExpression["^[^\n]*"]][[1]];
   If[StringLength[lastline <> firstline] <= 80 || StringMatchQ[lastline, RegularExpression["^\\s*$"]],
      target <> source, target <> cont <> "\n" <> spacer <> source]
];
WO`SmartAppend[source_, target_, n_] := WO`SmartAppend[source, target, n, ""];
WO`SmartAppend[source_, target_] := WO`SmartAppend[source, target, 0];


(* Indent the lines of a text by n blanks *)
WO`Indent[s_, n_] := Module[{spacer},
   spacer = StringJoin @@ Table[" ", {i, 1, n}];
   StringReplace[s, {RegularExpression["(?m)^(.+)$"] :> spacer <> "$1",
      RegularExpression["\n(.+)"] :> ("\n" <> spacer <> "$1")}]
] /; n > 1;

(* Uppercase the first letter *)
WO`FirstUpper[s_] := StringReplace[s, RegularExpression["^(.)"] :> ToUpperCase["$1"]];

(* Lowercase the first letter *)
WO`FirstLower[s_] := StringReplace[s, RegularExpression["^(.)"] :> ToLowerCase["$1"]];

(* Smart concatenation of a string list to a target introducing spacers *)
WO`SmartConcatRev[List[hd_, tl__], spacer_, i_, cont_] := WO`SmartAppend[spacer <> hd,
   WO`SmartConcatRev[List[tl], spacer, i, cont], i, cont];
WO`SmartConcatRev[List[hd_], spacer_, i_, cont_] := hd;
WO`SmartConcatRev[l_, s_, i_] := WO`SmartConcatRev[l, s, i, ""];
WO`SmartConcatRev[l_, s_] := WO`SmartConcatRev[l, s, 0];
WO`SmartConcat[l_,o___] := WO`SmartConcatRev[Reverse[l], o];
WO`SmartConcat[{}, ___] := "";

(* Not so smart concatenation, not taking care of breaking lines *)
WO`Concat[List[hd_, tl__], spacer_] := hd <> spacer <> WO`Concat[{tl}, spacer];
WO`Concat[{hd_}, _] := hd;
WO`Concat[{}, _] := "";

(* Transform a expression into sindarin via a piece of cheatery using FORTRAN form and
   regular expressions. *)
WO`SindarinForm[e_] := Module[{},
   If[Not[FreeQ[e, Re] && FreeQ[e, Im] && FreeQ[e, Complex]],
      Print[""
         <> "WARNING: complex calculus is not yet implemented in sindarin. Continuing operation, "
         <> "but the output is most likely dysfunctional."
      ]];
   WO`StringReplaceAll[ToString[FortranForm[e /.
      {   Power[E, x_] :> WO`exp[x], Sec[x_] :> 1/WO`cos[x], Csc[x_] :> 1/WO`sin[x],
         Sech[x_] :> 1/WO`cosh[x], Csch[x_] :> 1/WO`sinh[x] }
   ]], WO`f2sin]
];

(* Split a sindarin expression into multiple lines. *)
WO`SindarinSplit[s_] := WO`FortranSplit[s, 80, 3];
WO`SindarinSplit[str_, min_, ni_] := Module[{spacer, helper},
   spacer = StringJoin @@ Table[" ", {i, 1, ni}];

   helper[s_, l_, {}] := s;
   helper[s_, l_, {head_, tail___}] := If[
      (l + StringLength[head] <= min) || s == "",
         helper[s <> head, l + StringLength[head], {tail}]
      ,
         helper[s <> " \n" <> spacer <> head, 0, {tail}]
   ];
   
   helper["", 0, StringSplit[str, RegularExpression["([^\\w\\.]+)"] :> "$1"]]
];

(* Check if a version stamp exists; if it does, read it and check if matches. *)
WO`VersionCheck[dir_] := Module[{handle, v},
   If[Length[FileNames[{"WhizardVersion"}, dir]] != 1, 
      handle = OpenWrite[ToFileName[dir, "WhizardVersion"]];
      WriteString[handle, WO`whizv];
      Close[handle];
   ,
      handle = OpenRead[ToFileName[dir, "WhizardVersion"]];
      v = StringReplace[Read[handle, String], RegularExpression["\\s+"] -> ""];
      Close[handle];
      If[v != WO`whizv,
         Throw["ERROR: output directory already contains files generated for a differen WHIZARD / O'Mega version",
            WO`EAbort]
      ];
   ];
];

(* Copy the auxiliary files. *)
WO`CopyAux[srcdir_, destdir_] := Module[{CopyHelper},
   If[WO`whizv19x[],
      CopyHelper[stem_] := Module[{srcfiles, filenames, destfiles},
         srcfiles = Select[FileNames[{"*"}, ToFileName[{srcdir, stem}]],
            Not[StringMatchQ[#, "*.svn*"]]&];
         filenames =
            StringReplace[#, RegularExpression[
               "^.*" <> WO`fileSlashRE <> "([^" <> WO`fileSlashRE <> "]+)$"] :> "$1"]& /@ srcfiles;
         destfiles = Select[FileNames[{"*"}, destdir], (!FreeQ[filenames,
            StringReplace[#, RegularExpression[
               "^.*" <> WO`fileSlashRE <> "([^" <> WO`fileSlashRE <> "]+)$"] :> "$1"]])&];
         Catch[
            If[(Print["Deleting " <> # <> " ..."]; DeleteFile[#]) === $Failed,
               Throw[Null, WO`EFileSystem]]& /@ destfiles;
            If[(Print["Copying " <> # <> " ..."];
                  CopyFile[ToFileName[{srcdir, stem}, #], ToFileName[destdir, #]]) === $Failed,
               Throw[Null, WO`EFileSystem]]& /@ filenames;
         , WO`EFileSystem,
            Throw["ERROR copying auxiliary files...", WO`EAbort]&
         ];
      ];
      CopyHelper /@ {"all_19x", WO`whizv};
   ];
   If[WO`whizv2x[],
      CopyHelper[{filea_, fileb_}] := Module[{src, dest, sdir, sfile, ddir, dfile},
(* CD: changed this to include WO 2.3.0 *)
         (* src=If[WO`whizvn[] > WO`whizvn["2.0.3"],
            ToFileName[{srcdir, "2.2.3"}, filea],
            ToFileName[{srcdir, "2.0"}, filea]
         ];*)
         src=Which[WO`whizvn[] >= WO`whizvn["3.0.0"], ToFileName[{srcdir, "3.0.0"}, filea],
		           WO`whizvn[] >= WO`whizvn["2.3.0"], ToFileName[{srcdir, "2.3.0"}, filea],
                   WO`whizvn[] > WO`whizvn["2.0.3"], ToFileName[{srcdir, "2.2.3"}, filea],
                   True, ToFileName[{srcdir, "2.0"}, filea]
         ];
(* End CD change *)
         dest = ToFileName[destdir, fileb];
         StringReplace[src, RegularExpression[
            "^(.*" <> WO`fileSlashRE <> ")([^" <> WO`fileSlashRE <> "]+)$"] :>
               (sdir = "$1"; sfile = "$2"; "")];
         StringReplace[dest, RegularExpression[
            "^(.*" <> WO`fileSlashRE <> ")([^" <> WO`fileSlashRE <> "]+)$"] :>
            (ddir = "$1"; dfile = "$2"; "")];
         Catch[
            If[Length[FileNames[{dfile}, ddir]] != 0,
               If[(Print["Deleting " <> dest <> " ..."]; DeleteFile[dest]) === $Failed,
                  Throw[Null, WO`EFileSystem]]
            ];
            If[(Print["Copying " <> sfile <> " ..."]; CopyFile[src, dest]) === $Failed,
               Throw[Null, WO`EFileSystem]];
         ,
            WO`EFileSystem, Throw["ERROR copying auxiliary files...", WO`EAbort]&
         ];
      ];
      CopyHelper /@ {
         {"configure.ac", "configure.ac"},
         {"configure", "configure"},
         {"install-sh", "install-sh"},
         {"Makefile.in", "Makefile.in"},
         {"Makefile.omega.in", ToFileName["omega", "Makefile.in"]},
         {"Makefile.whizard.in", ToFileName["whizard", "Makefile.in"]},
         {"INSTALL", "INSTALL"}
      };
   ];
];

(* Write the O'Mega model files *)
WO`WriteOmega[dir_] := Module[{prefix, drvname, cdrvname},
   prefix = ToFileName[dir, WO`omeganame];
   If [WO`whizv2x[],
      Print["Writing O'Mega module signature to " <> prefix <> "_mdl.mli ..."];
      WO`WriteOmegaSig[prefix <> "_mdl.mli"];
      Print["Writing O'Mega module to " <> prefix <> "_mdl.ml ..."];
      WO`WriteOmegaStruct[prefix <> "_mdl.ml"];
      drvname = ToFileName[dir, "omega_" <> WO`omeganame <> ".ml"];
      Print["Writing O'Mega binary driver to " <> drvname <> " ..."];
      WO`WriteOmegaBinary[drvname, "", WO`ncfmax];
   ,
      Print["Writing O'Mega module signature to " <> prefix <> ".mli ..."];
      WO`WriteOmegaSig[prefix <> ".mli"];
      Print["Writing O'Mega module to " <> prefix <> ".ml ..."];
      WO`WriteOmegaStruct[prefix <> ".ml"];
      drvname = ToFileName[dir, "f90_" <> WO`omeganame <> ".ml"];
      cdrvname = ToFileName[dir, "f90_" <> WO`omeganame <> "_Col.ml"];
      Print["Writing O'Mega binary drivers to " <> drvname <> " and " <> cdrvname <> " ..."];
      WO`WriteOmegaBinary[drvname, cdrvname, WO`ncfmax];
   ];
];

(* Write the oplotter part. *)
WO`WriteOpl[dir_] := Module[{gluefile, prefix},
   gluefile = ToFileName[dir, "opl_" <> WO`oplname <> ".f90"];
   Print["Writing oplotter FORTRAN glue to " <> gluefile " ..."];
   WO`WriteOplGlue[WO`opldir <> gluefile];
   prefix = ToFileName[dir, WO`oplname <> ".f90"];
   Print["Writing oplotter model definitions to " <> prefix <> ".mdl and " <> prefix <> ".grb ..."];
   WO`WriteOplMdl[prefix <> ".mdl", prefix <> ".grb"];
];

(* Write the WHIZARD part. *)
WO`WriteWhiz[dir_] := Module[{whizmdl, whizglue},
   whizmdl = ToFileName[dir, WO`whizname <> ".mdl"];
   Print["Writing WHIZARD model file to " <> whizmdl <> " ..."];
   WO`WriteWhizMdl[whizmdl];
   whizglue = ToFileName[dir, "parameters." <> WO`whizname <> If[WO`whizv19x[], ".omega", ""]];
   Print["Writing WHIZARD FORTRAN glue..."];
   WO`WriteWhizGlue[whizglue];
];

(* Write the O'Mega model signature *)
WO`WriteOmegaSig[file_] := Module[{handle, contents},
   handle = OpenWrite[file];
   contents = ""
      <> WO`CommentMaker[WO`fileheader, "(* ", "   ", " *)"] <> "\n "
      <> "\n "
      <> "type gauge = Unitarity | Feynman | Rxi \n "
      <> "\n "
      <> "module type Frules_options = \n "
      <> "sig \n "
      <> "   val gauge: gauge \n "
      <> "   val color: bool \n "
      <> "end \n "
      <> "\n "
      <> "module Implementation: functor (Opts: Frules_options) -> Model.T\n "
      <> If[WO`whizvn[] >= WO`whizvn["2.0.3"], "   with module Ch = Charges.Null\n ", ""];
   WriteString[handle, contents];
   Close[handle];
];

(* During the initial particle list parsing, a number of hashes is filled for later use. These are: *
 * "constr" O'Mega constructor / "lrep" lorentz representation / "oname" O'Mega name /              *
 * "colrep" SU(3) representation / "pdg" pdg number / "mass" mass / "revpdg" pdg -> tag,            *
 * "whizname" WHIZARD name, "conj" conjugation, "cpl" coupling <-> symbol                           *
 * "oids" omega identifier register, "goldstone" goldstone flag, "HC": WO`HC helper                 *)

(* Write the O'Mega module structure *)
WO`WriteOmegaStruct[file_] := Module[{handle, contents, preamble, flavor, color, nc, caveats, pdg, lorentz,
      gauge, propagator, width, conjugate, fermion, colsymm, constant, maxdegree, vertices, fusions,
      flavors, extflavor, goldstone, parameters, flavortostring, flavorofstring, flavorsym, gaugesym,
      masssym, widthsym, texsym, constsym, options, rcs, ParsePList, sanscolorstubs, charges},

   (* Parse the Particle List *)
   ParsePList[] := Module[{classtypes, ltypes, ftype, trampolines, mlists, clists, ltdict,
      taglists, pdglists, cconjugators, lconjugators, creplists, lreplists, fsymlists,
      nonecolored, gslists, msymlists, wsymlists, tsymlists, ParsePart, ParseClass, OmegaConstructor,
      LTHelper, gsmasses},

      (* Remap PDGs which have been assigne multiple times. But remapping these induces other issues, so *
       * we just warn the user.                                                                          *)
      FixPDG[pdg_, tag_] := Module[{mtag},
         If[FreeQ[mtag = WO`hash["revpdg", pdg], WO`hash],
            Print[""
               <> "WARNING: PDG " <> ToString[pdg] <> " has been assigned both to \""
               <> ToString[tag] <> "\" and \"" <> ToString[mtag] <> "\". This will NOT work with "
               <> "WHIZARD, you'll have to fix your model before plugging it into WHIZARD. "
               <> "O'Mega will and oplotter might work though..."];
         ];
      ];

      ltdict[x_] := Switch[x, 1, "scalar", 2, "fermion", 3, "vector", 4, "tensor",
         "S", 1, "F", 2, "V", 3, "T", 4];
      classtypes = "";
      ltypes = {"", "", "", ""};
      trampolines = "";
      mlists = "";
      clists = {"", "", "", ""};
      cconjugators = "";
      lconjugators = {"", "", "", ""};
      nonecolored = {True, True, True, True};
      (* These nested lists warrant some explanation: each sublist is a list of pattern  *
       * matching associations, each of the type "A -> B", with B possible extending over *
       * multiple lines. They get glued together to form the query functions required by  *
       * O'Mega.                                                                          *)
      taglists = pdglists = creplists = lreplists = gslists = msymlists = wsymlists = fsymlists = tsymlists = 
         {{}, {}, {}, {}};
      gsmasses = {};

      (* Generates a mass identifier for a goldstone boson and adds it to the gsmasses. *)
      RegisterGoldstone[name_, gboson_, pdg_] := Module[{msym},
         msym = "m" <> StringReplace[ToString[name], RegularExpression["[^a-zA-Z\\d]"] -> ""];
         msym = WO`RegisterMass[msym, Null, pdg];
         AppendTo[gsmasses, {name, msym, gboson}];
         Return[msym];
      ];

      (* Generate the actual expression for a goldstone boson mass. To be mapped on gsmasses. *)
      ProcessGoldstone[{name_, msym_, gboson_}] :=
         WO`RegisterIParam[msym,
            If[WO`gauge === WO`WORxi,Sqrt[WO`gsym] * WO`hash["mass", gboson], WO`hash["mass", gboson]],
            ToString[name] <> " mass (autogenerated by the interface)", False];

      (* Create a valid constructor. *)
      OmegaConstructor[s_] := Module[{i, c},
         c = WO`FirstUpper[WO`SanitizeString[s]];
         i = 1;
         If[WO`hash["oids", ToLowerCase[c]] === True,
            While[WO`hash["oids", ToLowerCase[c] <> "_" <> ToString[i]] === True, i++];
            c = c <> "_" <> ToString[i];
         ];
         WO`hash["oids", ToLowerCase[c]] = True;
         c
       ];

      (* Parse class description *)
      ParseClass[{cdesc_, plist_}] := Module[{cname, LTHelper, classtype, mlist, cconjugator, taglist, pdglist,
         creplist, lreplist, allcharged, majoranas, spinors, cspinors, vectors, hvectors, tensors, htensors, allcolored,
         fsu3s, afsu3s, asu3s, gslist, msymlist, wsymlist, tsymlist, allmassive, allnzw, allgoldstones, fsymlist},
         classtype = "";
         mlist = "";
         cconjugator = "";
         allcharged = allcolored = allgoldstones = allmassive = allnzw = True;
         majoranas = spinors = cspinors = vectors = hvectors = tensors = htensors = {};
         fsu3s = afsu3s = asu3s = {};
         (* See above; lists of mattern matching associations that get glued together with *
          * the class constructors to form new pattern matching associations               *)
         taglist = pdglist = creplist = lreplist = gslist = msymlist = wsymlist = fsymlist = tsymlist = {};

         (* Parse particle description *)
         ParsePart[{name_, aname_, spin_, prop_, msym_, wsym_, crep_, plabel_, pdg_, descr_, tex_, atex_, gs_,echarge_}] :=
         Module[{sname, saname, RegisterParticle},

            (* Register a particle *)
            RegisterParticle[pname_, ptag_, ppdg_, anti_, sconj_] := Module[{realmsym},
               (* This can be dynamically changed, so initialize it here *)
               realmsym = msym;
               (* Add particle to global list. *)
               AppendTo[WO`taglist, ptag];
               (* Append constructor for particle to class type *)
               classtype = WO`SmartAppend[" | " <> WO`FirstUpper[pname], classtype];
               (* Append constructor to class member list *)
               mlist = WO`SmartAppend[" " <> WO`FirstUpper[pname] <> ";", mlist];
               (* add a hash from the particle tag to the full ocaml type *)
               WO`hash["constr", ptag] = "FR" <> ToString[spin] <> " (" <> WO`FirstUpper[cname] <> " " <> WO`FirstUpper[pname] <> ")";
               (* Add translation from flavor to tag *)
               AppendTo[taglist, WO`FirstUpper[pname] <> " -> \"" <> ptag <> "\""];
               (* The TeX symbol *)
               AppendTo[tsymlist, WO`FirstUpper[pname] <> " -> \"" <> StringReplace[If[anti, atex, tex], "\\" -> "\\\\"] <> "\""];
               (* PDG number *)
               FixPDG[ppdg, ptag];
               AppendTo[pdglist, WO`FirstUpper[pname] <> " -> " <> ToString[ppdg]];
               WO`hash["pdg", ptag] = ppdg;
               WO`hash["revpdg", ppdg] = ptag;
               (* Color representation *)
               Switch[crep,
                  S, (allcolored = False; WO`hash["colrep", ptag] = "S"),
                  T, If[anti, AppendTo[afsu3s, WO`FirstUpper[pname]]; WO`hash["colrep", ptag] = "f",
                        AppendTo[fsu3s , WO`FirstUpper[pname]]; WO`hash["colrep", ptag] = "F"],
                  O, AppendTo[asu3s, WO`FirstUpper[pname]]; WO`hash["colrep", ptag] = "O";
               ];
               (* If a lorentz type contains no colored particles, the pattern matching may be *
                * shortened                                                                    *)
               nonecolored[[ltdict[ToString[spin]]]] = nonecolored[[ltdict[ToString[spin]]]] && (crep == S);
               (* Lorentz representation *)
               Switch[spin,
                  V,   Switch[msym,
                     ZERO, AppendTo[vectors, WO`FirstUpper[pname]], 
                        _, AppendTo[hvectors, WO`FirstUpper[pname]]],
                  T,   Switch[msym,
                     ZERO, AppendTo[tensors, WO`FirstUpper[pname]], 
                        _, AppendTo[htensors, WO`FirstUpper[pname]]],
                  F, Switch[{sconj, anti},
                     {True, _}, (AppendTo[majoranas, WO`FirstUpper[pname]]; WO`havemajoranas=True),
                     {False, False}, AppendTo[spinors, WO`FirstUpper[pname]],
                     {False, True}, AppendTo[cspinors, WO`FirstUpper[pname]]]
               ];
               (* Goldstone boson? As we most likely don't know the gauge boson constructor at ths point, *
                * we have to treat this special, sigh, again. If we are using Feynman or Rxi gauge and are*
                * expected to assign the goldstone masses automatically, we must invoke the handler.      *)
               If[spin == S && ToString[gs] != "NoGS",
                  AppendTo[gslist, {WO`FirstUpper[pname] <> " -> Some ", ToString[PartName[If[anti, WO`HC[gs], gs]]]}];
                  If[WO`autogauge && Not[FreeQ[{WO`WOFeynman, WO`WORxi}, WO`gauge]],
                     realmsym = RegisterGoldstone[ptag, ToString[PartName[If[anti, WO`HC[gs], gs]]], ppdg]
                  ];
                  WO`hash["goldstone", ToString[ptag]] = True;
               ,
                  allgoldstones = False;
                  WO`hash["goldstone", ToString[ptag]] = False;
               ];
               (* Mass *)
               Switch[realmsym,
                  ZERO, allmassive = False,
                  _?NumericQ, AppendTo[msymlist, WO`FirstUpper[pname] <> " -> " <> "\""
                     <> ToString[realmsym] <> "\""],
                  _, AppendTo[msymlist, WO`FirstUpper[pname] <> " -> \"" <> ToLowerCase[ToString[realmsym]] <> "\""]
               ];
               WO`hash["mass", ptag] = realmsym;
               (* Width *)
               Switch[wsym,
                  ZERO, allnzw = False,
                  _?NumericQ, AppendTo[wsymlist, WO`FirstUpper[pname] <> " -> " <> "\""
                     <> ToString[wsym] <> "\""],
                  _, AppendTo[wsymlist, WO`FirstUpper[pname] <> " -> \"" <> ToLowerCase[ToString[wsym]] <> "\""]
               ];
               (* Add a Lorentz type code to the hash *)
               WO`hash["lrep", ptag] = Switch[spin,
                  S, "S", V, "V", T, "T",
                  F, Switch[{sconj, anti}, {True, _}, "M", {False, False}, "F", _, "f"]
               ];
               (* Add a hash from the particle tag to the O'Mega name *)
               WO`hash["oname", ptag] = pname;
               (* Almost forgot this one: flavor symbol *)
               AppendTo[fsymlist, WO`FirstUpper[pname] <> " -> \"" <> ToLowerCase[pname] <> "\""];
            ];

            (* Construct sane names *)
            sname = OmegaConstructor[name];
            saname = OmegaConstructor[aname];
            WO`hash["conj", name] = aname;
            WO`hash["conj", aname] = name;
            RegisterParticle[sname, name, pdg, False, name == aname];
            allcharged = allcharged && (name != aname);
            If[name != aname,
               RegisterParticle[saname, aname, -pdg, True, False];
               cconjugator = WO`SmartAppend[" | " <> WO`FirstUpper[sname] <> " -> "
                  <> WO`FirstUpper[saname], cconjugator];
               cconjugator = WO`SmartAppend[" | " <> WO`FirstUpper[saname] <> " -> "
                  <> WO`FirstUpper[sname], cconjugator];
            ];
         ];

         (* A helper digesting the lorentz type *)
         LTHelper[idx_] := Module[{trampoline, MakePMatcher},

            (* Step up one level in a pattern matcher. Take care to treat a several patterns *
             * matching to one value as multiple matching pairs (use match x with)           *)
            MakePMatcher[Hold[lists_], list_] := Module[{text},
               Switch[Length[list] + If[And @@ (StringFreeQ[#, "|"]& /@ Append[list, "a"]), 0, 1] ,
                  0, Null,
                  1, (
                     text = list[[1]];
                     AppendTo[lists[[idx]], WO`FirstUpper[cname] <> " " <> text];),
                  _Integer, (
                     text = " | " <> WO`SmartConcat[list, " | "];
                     AppendTo[lists[[idx]], WO`FirstUpper[cname] <> " x -> (match x with \n"
                        <> WO`Indent[text, 2] <> ")"] ;)
               ];
            ];

            (* Add the class constructor to the lorentz type *)
            ltypes[[idx]] = WO`SmartAppend[" | " <> WO`FirstUpper[cname]  <> " of " <> WO`FirstLower[cname], ltypes[[idx]]];
            (* Add the trampoline to class -> lorentz type *)
            trampoline = WO`FirstLower[ltdict[idx]] <> "_of_" <> WO`FirstLower[cname];
            trampolines = trampolines <> "let " <> trampoline <> " c = " <> WO`FirstUpper[cname] <> " c\n";
            (* Add the class members to the lorentz level particle list*)
            clists[[idx]] = WO`SmartAppend[" @ (List.map " <> trampoline <> " " <> WO`FirstLower[cname] <> "_members)", clists[[idx]]];
            (* Add the class conjugator to the lorentz conjugator *)
            lconjugators[[idx]] = lconjugators[[idx]] <> "| " <> WO`FirstUpper[cname] <> " x -> " <> WO`FirstUpper[cname]
               <> " (conjugate_" <> WO`FirstLower[cname] <> " x)\n";
            (* Construct class level of the flavor -> tag translation *)
            MakePMatcher[Hold[taglists], taglist];
            (* Dito, TeX symbol *)
            MakePMatcher[Hold[tsymlists], tsymlist];
            (* Dito, PDG *)
            MakePMatcher[Hold[pdglists], pdglist];
            (* Color Representation *)
            If[Not[allcolored], AppendTo[creplist, "_ -> Color.Singlet"]];
            MakePMatcher[Hold[creplists], creplist];
            (* Lorentz Representation *)
            MakePMatcher[Hold[lreplists], lreplist];
            (* Goldstone Bosons *)
            (* TODO: defunct atm *)
            (*
            gslist = (#[[1]] <> "(" <> WO`hash["constr", #[[2]]] <> ", Const 1)")& /@ gslist;
            If[idx == 1 && Not[allgoldstones], AppendTo[gslist, "_ -> None"]];
            MakePMatcher[Hold[gslists], gslist];
            *)
            (* Mass *)
            If[Not[allmassive], AppendTo[msymlist, "_ -> \"fr_zero\""]];
            MakePMatcher[Hold[msymlists], msymlist];
            (* Width *)
            If[Not[allnzw], AppendTo[wsymlist, "_ -> \"fr_zero\""]];
            MakePMatcher[Hold[wsymlists], wsymlist];
            (* Flavor *)
            MakePMatcher[Hold[fsymlists], fsymlist];
         ];

         (* Construct the class name... *)
         cname = StringReplace[ToString[cdesc[[1]]], {"[" -> "", "]" -> ""}] <> "_" <> OmegaConstructor[ToString[cdesc[[2]]]];
         (* Don't want no ghosts *)
         If[StringMatchQ[cname, RegularExpression["^[SFVT].*"]],
            (* ... loop over members ... *)
            ParsePart /@ plist;
            (* Put to gether the lorentz rep patterns *)
            (If[Length[#[[1]]] != 0, AppendTo[lreplist, WO`SmartConcat[#1[[1]], " | "] <> " -> " <> #[[2]]]])&
               /@ {{majoranas, "Majorana"}, {spinors, "Spinor"}, {cspinors, "ConjSpinor"},
                   {vectors, "Vector"}, {hvectors, "Massive_Vector"}, {tensors, "Tensor_1"}, {htensors, "Tensor_2"}};
            (* Dito, color rep patterns *)
            (If[Length[#[[1]]] != 0, AppendTo[creplist, WO`SmartConcat[#1[[1]], " | "] <> " -> " <> #[[2]]]])&
               /@ {{fsu3s, "Color.SUN 3"}, {afsu3s, "Color.SUN (-3)"}, {asu3s, "Color.AdjSUN 3"}};
            (* ...Add a constructor for the class to the proper lorentz type... *)
            LTHelper[ltdict[StringTake[cname, {1}]]];
            (* Create the class member list ...*)
            mlists = mlists <> "let " <> WO`FirstLower[cname] <> "_members =\n" <>
               WO`Indent[StringReplace[mlist, RegularExpression["^ "] -> "["] <> "]", 3] <> "\n";
            (* Create new class type... *)
            classtypes = classtypes <> "type " <> WO`FirstLower[cname] <> " =\n" <> WO`Indent[classtype, 2] <> "\n";
            (* Add the class conjugator *)
            cconjugators = cconjugators <> "let conjugate_" <> WO`FirstLower[cname] <> " = function \n";
            If [cconjugator != "", cconjugators = cconjugators <> WO`Indent[cconjugator, 2] <> "\n"];
            If [Not[allcharged], cconjugators = cconjugators <> "   | x -> x\n";];
         ,
            (* Ghosts still get hashed w.r.t. their lorentz rep. *)
            (WO`hash["lrep", #[[1]]] = WO`hash["lrep", #[[2]]] = "U")& /@ plist;
         ];
      ];

      (* A helper for digesting a lorentz type *)
      LTHelper[idx_] := Module[{constructor},

         (* Step up one level in a pattern matcher *)
         MakePMatcher[Hold[matcher_], list_] := Module[{text},
            Switch[Length[list],
               0, Null,
               1, (
                  text = list[[1]];
                  matcher = matcher <> WO`Indent["| " <> constructor <> " " <> text <> "\n", 3];),
               _Integer, (
                  text = "| " <> WO`SmartConcat[list, "\n| "];
                  matcher = matcher <> WO`Indent[""
                     <> "| " <> constructor <> " x -> (match x with \n"
                     <> WO`Indent[text, 3] <> ")\n" ,3];)
            ];
         ];

         If [ltypes[[idx]] != "",
            (* Construct the lorentz constructor *)
            constructor = "FR" <> ToUpperCase[StringTake[ltdict[idx], {1}]];
            (* Assemble the lorentz type definition *)
            ltypes[[idx]] = "type " <> WO`FirstLower[ltdict[idx]] <> " =\n" <> WO`Indent[ltypes[[idx]], 2] <> "\n";
            (* Append the constructor to the flavor type *)
            ftype = WO`SmartAppend[" | " <> constructor <> " of " <> WO`FirstLower[ltdict[idx]], ftype, 2];
            (* Add a trampoline lorentz -> flavor *)
            trampolines = trampolines <> "let flavor_of_" <> WO`FirstLower[ltdict[idx]]
               <> " lt = " <> constructor <> " lt\n";
            (* Assemble the particle list *)
            clists[[idx]] = "let all_" <> WO`FirstLower[ltdict[idx]] <> "s = []\n" <> WO`Indent[clists[[idx]], 2] <> "\n";
            (* Add the all particles of this lorentz rep to the flavor list *)
            flavors = WO`SmartAppend[" @ (List.map flavor_of_" <> WO`FirstLower[ltdict[idx]]
               <> " all_" <> WO`FirstLower[ltdict[idx]] <> "s)", flavors, 2];
            (* Assemble the conjugation operation for the lorentz type *)
            lconjugators[[idx]] = "let conjugate_" <> WO`FirstLower[ltdict[idx]] <> " = function \n"
               <> WO`Indent[lconjugators[[idx]], 3];
            (* Add to the conjugation function *)
            conjugate = conjugate <> "   | " <> constructor
               <> " x -> " <> constructor <> " (conjugate_"
               <> WO`FirstLower[ltdict[idx]] <> " x)\n";
            (* Construct lorentz level of the flavor -> tag translation *)
            MakePMatcher[Hold[flavortostring], taglists[[idx]]];
            (* Dito, TeX symbol *)
            MakePMatcher[Hold[texsym], tsymlists[[idx]]];
            (* Dito, PDG *)
            MakePMatcher[Hold[pdg], pdglists[[idx]]];
            (* Color Rep *)
            If[nonecolored[[idx]], creplists[[idx]] = {"_ -> Color.Singlet"}];
            MakePMatcher[Hold[color], creplists[[idx]]];
            (* Lorentz Rep *)
            MakePMatcher[Hold[lorentz], lreplists[[idx]]];
            (* Goldstone mapper *)
            (* TODO: defunct atm *)
            (*
            MakePMatcher[Hold[goldstone], gslists[[idx]]];
            *)
            (* Mass *)
            MakePMatcher[Hold[masssym], msymlists[[idx]]];
            (* Width *)
            MakePMatcher[Hold[widthsym], wsymlists[[idx]]];
            (* Flavor *)
            MakePMatcher[Hold[flavorsym], fsymlists[[idx]]];
         ];
      ];

      (* We need this hack if the default WO`HC implementation is active. *)
      ((WO`hash["HC", ToString[#[[1]]]] = #[[2]]; WO`hash["HC", ToString[#[[2]]]] = #[[1]])&
         /@ #[[2]])& /@ WO`PartList;
      ParseClass /@ WO`PartList;
      ProcessGoldstone /@ gsmasses;
      ftype = "type flavor =\n  ";
      flavors = "let flavors () = []\n  ";
      flavortostring = "let flavor_to_string = function\n";
      pdg = "let pdg = function\n";
      conjugate = "let conjugate = function\n";
      color = "let color = function\n";
      nc = "let nc () = 3\n";
      caveats = "let caveats () = []\n  ";
      lorentz = "let lorentz = function\n";
      goldstone = "let goldstone = function\n";
      masssym = "let mass_symbol = function\n";
      widthsym = "let width_symbol = function\n";
      flavorsym = "let flavor_symbol = function\n";
      texsym = "let flavor_to_TeX = function\n";
      LTHelper /@ {1, 2, 3, 4};
      goldstone = goldstone <> "   | _ -> None\n";
      If[ltypes[[1]] != "", lorentz = lorentz <> "   | FRS _ -> Scalar\n"];
      flavors = flavors <> "\n";
      ftype = ftype <> "\n";
      flavor = ""
         <> "(* Classes with their members *)\n \n "
         <> classtypes
         <> "\n(* Lorentz types with the corresponding classes *)\n \n "
         <> (StringJoin /@ ltypes)
         <> "\n(* The actual flavor type *)\n \n "
         <> ftype
         <> "\n(* Trampoline functions *)\n \n "
         <> trampolines
         <> "\n(* Particle lists *)\n \n "
         <> mlists <> (StringJoin /@ clists);
      conjugate = ""
         <> "(* The conjugation operation on a single class *)\n \n "
         <> cconjugators
         <> "\n(* The conjugation operation on a lorentz type *)\n \n "
         <> StringJoin @@ lconjugators
         <> "\n(* Conjugation for the masses *)\n \n "
         <> conjugate;
      propagator = ""
         <> "let propagator = \n "
         <> "   let msg = \" " <> WO`omeganame <> ".Implementation: invalid lorentz rep in propagator \" in function \n "
         <> If[ltypes[[1]] != "", "   | FRS _ -> Prop_Scalar \n ", ""]
         <> If[ltypes[[2]] != "", ""
            <>"   | FRF x -> (match lorentz (FRF x) with \n "
            <> "      | Spinor -> Prop_Spinor \n "
            <> "      | ConjSpinor -> Prop_ConjSpinor \n "
            <> "      | Majorana -> Prop_Majorana \n "
            <> "      | _ -> invalid_arg msg)\n ",
            ""]
         <> If[ltypes[[3]] != "", ""
            <>"   | FRV x -> (match Opts.gauge with \n "
            <> "      | Unitarity -> (match lorentz (FRV x) with \n "
            <> "         | Vector -> Prop_Feynman \n "
            <> "         | Massive_Vector -> Prop_Unitarity \n "
            <> "         | _ -> invalid_arg msg)\n "
            <> "      | Feynman -> (match lorentz (FRV x) with \n "
            <> "         | Vector -> Prop_Feynman \n "
            <> "         | Massive_Vector -> Prop_Rxi Xi \n "
            <> "         | _ -> invalid_arg msg)\n "
            <> "      | Rxi -> (match lorentz (FRV x) with \n "
            <> "         | Vector -> Prop_Gauge Xi \n "
            <> "         | Massive_Vector -> Prop_Rxi Xi \n "
            <> "         | _ -> invalid_arg msg))\n ",
            ""]
         <> If[ltypes[[4]] != "","   | FRT _ -> Prop_Tensor _ 2\n ", ""];
   ];

   (* Digest the vertices *)
   DigestVertices[] := Module[{vlist, sort, tagger, v3deflist={}, v4deflist={}, vndeflist={},
      v3deflist$c={}, v4deflist$c={}, cplhash, cplrevhash, DigestVertex, RegisterCoupling,
      vndeflist$c={}, ID, VlistMaker, VlistSmartMaker, StripColor, nproc=0, nskipped=0, nghost=0},
   
      (* Register a coupling constant and return the caml constructor *)
      RegisterCoupling[plist_, cpl_] := Module[{cname, suffix=1, mycpl},
         (* Sanity check *)
         If[Not[FreeQ[cpl, Index]],
            Throw["WARNING: a coupling still has a index structure, something is badly wrong...", WO`ESkip]];
         (* If applicable, apply any vertex rules *)
         mycpl = If[Head[WO`VertexRules] === List, cpl /. WO`VertexRules, cpl];
         (* Have we already encountered this coupling? *)
         If[MatchQ[WO`hash["cpl", mycpl], _String],
            (* -> Do nothing, return the constructor *)
            WO`hash["cpl", mycpl]
         ,
            (* otherwise, suggest a constructor... *)
            cname = "G_" <> ToLowerCase[WO`Concat[WO`hash["oname", PartName[#[[1]] ]]& /@ plist, "_"]];
            (* ...and refine it if necessary with an index *)
            If[cplrevhash[cname] === True,
               While[cplrevhash[cname <> "_" <> ToString[suffix]] === True, suffix++];
               cname = cname <> "_" <> ToString[suffix];
            ];
            (* Append to coupling list, register in hashes *)
            AppendTo[WO`cpldeflist, {cname, mycpl}];
            cplrevhash[cname] = True;
            (* If the coupling contains aS or G, it will be evolved *)
            If[WO`whizv2x[] && (Or @@ (Not[FreeQ[mycpl, #]]& /@ WO`RunParameters)),
               AppendTo[WO`runningcouplings, Length[WO`cpldeflist]]];
            WO`hash["cpl", mycpl] = cname
         ]
      ];

      (* This function strips the color structure from a colored vertex. It works by calculating *
       * all color flows and then throwing away all but one flow ("keyflow").                    *
       * Tricky and propably still wrong in some cases :)                                        *)
      StripColor[cpl_, plist_] := Module[{expc, MakeReallyLong, CalcCflow, Cflow,
            FlowArrow, GetFlowList, taglist, tag, k, ConnectedQ, flowlist, keyflow},
         SetAttributes[Cflow, Orderless];

         (* Replace structure constants and apply completeness relation *)
         MakeReallyLong[xpr_] := Module[{fundi=0, adji=0, expterm, tmp1, tmp2, tmp3, i=0, GetCRep},
            (* Get the color representation of a particle. *)
            GetCRep[i_] := Module[{part},
               part = Flatten[Select[plist, (#[[2]] == i)&]][[1]];
               WO`hash["colrep",PartName[part]]
            ];

            (* Start by expanding the expression *)
            expterm = Expand[xpr];
            (* Replace Index[Gluon,Gluon$1] with Index[Gluon,Int,1] and similar. *)
            expterm = expterm //. {Index[a_, b_] :> If[Head[b] =!= Ext, 
                           Index[a, Int, ToExpression[StringReplace[ToString[b], __ ~~ "$" -> ""]]],
                           Index[a, b]]};
            (* Determine the highest internal fundamental and adjoint indices. *)
            expterm /. {Index[Gluon, _, i_] :> (adji = Max[adji, i]),
               Index[Colour, _, i_] :> (fundi = Max[fundi, i])};
            fundi++; adji++;
            (* Expand TensDots. *)
            expterm = expterm //. {
               TensDot[T[a_], o__][i_, j_] :>
                  T[a, i, tmp1 = Index[Colour, z, fundi++]] * TensDot[o][tmp1, j],
               TensDot[T[a_]][i_, j_] :> T[a, i, j]
            };
            (* Expand the structure constants to traces over the generators.              *
             * TODO: Find out whether the execution order in mathematica is well defined  *
             * in this case.                                                              *)
            expterm = Expand[expterm /. {f[a_, b_, c_] :>
            -I * 2 * (T[a, Index[Colour, z, tmp1=fundi++], Index[Colour, z, tmp2=fundi++]] *
               T[b, Index[Colour, z, tmp2], Index[Colour, z, tmp3=fundi++]] *
               T[c, Index[Colour, z, tmp3], Index[Colour, z, tmp1]] -
            T[b, Index[Colour, z, tmp1=fundi++], Index[Colour, z, tmp2=fundi++]] *
               T[a, Index[Colour, z, tmp2], Index[Colour, z, tmp3=fundi++]] *
               T[c, Index[Colour, z, tmp3], Index[Colour, z, tmp1]])}];
            (* Replace IndexDeltas of gluons via normalization condition. *)
            expterm = expterm /. IndexDelta[Index[Gluon, a_], Index[Gluon, b_]] :>
               2 * T[Index[Gluon, a], Index[Colour, z, tmp1 = fundi++], Index[Colour, z, tmp2 = fundi++]] *
               T[Index[Gluon, b], Index[Colour, z, tmp2], Index[Colour, z, tmp1]];
            (* Replace IndexDeltas with FlowArrows, restoring the correct direction. This is necessary *
               because IndexDelta is orderless.                                                        *)
            expterm = expterm /.
               IndexDelta[Index[Colour, Ext[i_]], Index[Colour, Ext[j_]]] :>
                  If[GetCRep[i] === "f",
                     FlowArrow[Index[Colour, Ext[i]], Index[Colour, Ext[j]]],
                     FlowArrow[Index[Colour, Ext[j]], Index[Colour, Ext[i]]]
                  ];
            (* Apply completeness relation to generators. TODO: this loop may be infinite in pathological cases. *)
            While[Not[FreeQ[expterm, Index[Gluon, _, _]]],
               expterm = Expand[expterm /.
                  Times[p___, T[Index[Gluon, x1_, i_], a_, b_], T[Index[Gluon, x2_, j_], c_, d_]]/; (i===j && x1===x2)
                     :> Times[p, 1/2, FlowArrow[a, d] * FlowArrow[c, b] -
                     1/3 * FlowArrow[a, b] * FlowArrow[c, d]]];
               If[i++ > WO`maxiter,
                  Print[""
                     <> "WARNING: I might be stuck in a loop in StripColor (BUG!), you might consider "
                     <> "to abort the calculation...."];
                  i = 0;
               ];
            ];
            expterm
         ];
  
         (* Extract the flow from a monomial *)
         CalcCflow[term_] := Module[{flow, Ic},

            flow = {};
            (* Converts an index into something simpler *)
            ConvertIndex[i_] := i/.{Index[_, Ext[j_]] :> j, Index[Colour, x_, j_] :> Ic[x, j]};
            (* Extract the colour flow pairs and append them to flow *)
            term /. {
               FlowArrow[a_, b_] :>
                  (AppendTo[flow, {ConvertIndex[a], ConvertIndex[b]}]; Null),
               T[a_, b_, c_] :> (AppendTo[flow, {ConvertIndex[b], ConvertIndex[a]}];
                  AppendTo[flow, {ConvertIndex[a], ConvertIndex[c]}];)};
            (* Contract indices until only the physical flow is left. Using an orderless symbol *
             * would be more elegant but puts mathematica (at least V5) into an endless loop... *
             * (at least for me).                                                               *)
            flow = flow
               //.
                  {{p1___, {a_, Ic[t1_, x_]}, p2___, {Ic[t2_, y_], b_}, p3___}/;(x==y && t1===t2) :> {p1, {a, b}, p2, p3},
                  {p1___, {Ic[t1_, x_], b_}, p2___, {a_, Ic[t2_, y_]}, p3___}/;(x==y && t1===t2) :> {p1, {a, b}, p2, p3}};
            If[Not[FreeQ[flow, Ic[___]] && FreeQ[flow, Index[Gluon, _, _]]],
               Throw["WARNING: Unable to calculate color flow for vertex, skipping this one...\n" 
                  <> "         This is almost certainly a BUG...", WO`ESkip];
            ];
            Cflow @@ flow
         ];

         (* Extract the different flows and their coefficents. *)
         GetFlowList[expr_] := Module[{key},
            If[TrueQ[expr == 0],
               {}
            ,
               key = Null;
               expr /. Cflow[a___] :> (key = Cflow[a]);
               If[key === Null, Throw[Futile[
                  "BUG: colored vertex contains terms without associated flow, skipping..."], WO`ESkip]];
               Append[GetFlowList[expr /. key -> 0], {key, (expr /. key -> 1) /. Cflow[___] -> 0 }]
            ]
         ];

         ConnectedQ[flow_] := Module[{s, ReplaceNCheck},
            ReplaceNCheck[x_, list_] := Module[{rules, nx, nlist},
               rules = Select[list, MatchQ[#, {x, _}]&];
               If[Length[rules] != 1, Return[False]];
               nx = rules[[1, 2]];
               nlist = Complement[list, rules];
               If[nx == s, Return[Length[nlist] == 0]];
               ReplaceNCheck[nx, nlist]
            ];

            s = flow[[1, 1]];
            ReplaceNCheck[s, List @@ flow]
         ];

         GetKeyflow[tag_, k1_, k2_, k3_, k4_] := Module[{flows, keyflow},
            flows = {
                  {"fFS", {{k1, k2}}}, {"fFO", {{k1, k3}, {k3, k2}}}, {"OOS", {{k1, k2}, {k2, k1}}},
                  {"OOO", {{k1, k3}, {k3, k2}, {k2, k1}}}, {"SSS", {}},
                  {"fFSS", {{k1, k2}}}, {"fFOS", {{k1, k3}, {k3, k2}}}, {"fFOO", {{k1, k3}, {k3, k4}, {k4, k2}}},
                  {"OOSS", {{k1, k2}, {k2, k1}}}, {"OOOS", {{k1, k3}, {k3, k2}, {k2, k1}}},
                  {"OOOO", {{k1, k4}, {k4, k3}, {k3, k2}, {k2, k1}}}, {"SSSS", {}}};
            keyflow = Null;
            If[tag == #[[1]], keyflow = #[[2]]]& /@ flows;
            If[keyflow === Null, Throw[Futile[
               "BUG in GetKeyFlow --- please report. Skipping vertex..."], WO`ESkip]];
            Cflow @@ keyflow
         ];

         (* If the vertex is colorless, skip the lengthy algebra. *)
         If[FreeQ[cpl, Index[Gluon, ___]] && FreeQ[cpl, Index[Colour, ___]], Return[cpl]];
         (* Enforce color constraints, part I *)
         taglist = Sort[{WO`hash["colrep", PartName[#[[1]]]], PartName[#[[1]]], #[[2]]}& /@ plist,
            OrderedQ[{#1[[1]], #2[[1]]}]&];
         tag = StringJoin @@ (#[[1]]& /@ taglist);
         k = Append[#[[3]]& /@ taglist, Null];
         Switch[tag,
            "fff"|"FFF"|"fffS"|"FFFS"|"ffFF",
               Throw[Futile["WARNING: unsupported color structure, skipping this vertex..."], WO`ESkip],
            "fFOO", If[taglist[[3, 2]] != taglist[[4, 2]], Throw[Futile[
               "WARNING: Color: 3388 is only supported for identical octets, skipping this vertex..."],
                  WO`ESkip]],
            "OOS"|"OOSS", If[WO`whizvn[] < WO`whizvn["1.96"], Throw[Futile[
               "WARNING: Color: 8811 and 881 are only supported for WHIZARD 1.96 and higher, skipping this vertex..."],
                  WO`ESkip]],
            "OOO"|"OOOS", If[
               !(taglist[[1 ,2]] == taglist[[2, 2]] || taglist[[1, 2]] == taglist[[3, 2]]
                  || taglist[[2, 2]] == taglist[[3, 2]]),
               Throw[Futile["Warning: Color: 888 and 8881 are only supported if at least two of the octets "
                  <> "are identical; skipping this vertex..."], WO`ESkip]]
         ];
         (* Start by expanding and applying the completeness relation. *)
         expc = MakeReallyLong[cpl];
         (* Replace all coulour structures by the corresponding flows. *)
         expc = If[Head[expc] === Plus, ((CalcCflow[#] * #)& /@ expc), CalcCflow[expc] * expc] /.
            {T[Index[Gluon, ___], ___] -> 1, FlowArrow[Index[Gluon, ___], ___] -> 1,
               FlowArrow[Index[Colour, ___], ___] -> 1};
         (* Build the flow list *)
         flowlist = GetFlowList[expc];
         (* Enforce color constraints, part II --- revenge of the octets *)
         If[tag == "OOOO", If[!Catch[
            Module[{key, keys, perms, Permute, PermuteExt, fact1, facts, factsref},
               (* Apply the permutations that generate the different flows *)
               Permute[x_, {}] := x;
               Permute[x_, {{a_, b_}, more___}] := Permute[x /. {a -> b, b -> a}, {more}];
   
               PermuteExt[x_, {}] := x;
               PermuteExt[x_, {{a_, b_}, more___}] := PermuteExt[x /. {Ext[a] -> Ext[b], Ext[b] -> Ext[a]}, {more}];

               (* Check whether all flows are there and whether they are connected *)
               If[Length[flowlist] != 6, Throw[False]];
               If[! (And @@ (ConnectedQ[#[[1]]]& /@ flowlist)), Throw[False]];
               (* The permutations *)
               perms = {{}, {{3, 4}}, {{2, 3}}, {{2, 3}, {2, 4}}, {{2, 4}}, {{2, 4}, {2, 3}}};
               key = Cflow[{1, 2}, {2, 3}, {3, 4}, {4, 1}];
               (* The flows. *)
               keys = Permute[key, #]& /@ perms;
               (* The coefficients *)
               factsref = Flatten[Function[x, #[[2]]& /@ Select[flowlist, (#[[1]] === x)&]] /@ keys, 1];
               (* Reference coefficients *)
               fact1 = (#[[2]]& /@ Select[flowlist, (#[[1]] === key)&])[[1]];
               facts = PermuteExt[fact1, #]& /@ perms;
               (* Equality? *)
               Throw[And @@ MapThread[TrueQ[WO`Simplify[#1 - #2, Trig -> False] == 0]&, {factsref, facts}]];
            ]], Throw["WARNING: Color: OOOO only supported if all six flows are present "
                  <> "and properly related by permutations, skipping...", WO`ESkip]];
         ];
         (* Weed out the flows. *)
         keyflow = GetKeyflow[tag, k[[1]], k[[2]], k[[3]], k[[4]]];
         expc = #[[2]]& /@ Select[flowlist, (#[[1]] === keyflow)&];
         If[Length[expc] != 1, Throw[Futile[
                  "WARNING: None of the expected color flows matches, skipping this vertex...\n"
               <> "         This is possibly a BUG..."], WO`ESkip];
         ];
         expc[[1]]
      ];


      (* Digest a single vertex *);
      DigestVertex[vspec_] := Module[{tag, partlist, coupling, arity, Digest3ary, Digets4ary,
         Digestnary, CheckLorentz, LInd, SInd, Met, CheckLorentzBackend, RegisterVertex, ID, Futile},

         (* This tries to decompose a lorentz sructure in a given set of structures ("bricks"). *
          * These are assumed to decompose into disjunct sums of atoms, as is the structure. If *
          * successfull, a list if the resulting coefficients is returned.                      *)
         CheckLorentzBackend[lego_List, struct_] := Module[{Div, Onestep, coeffs={}, Iterate},
            (* Divide by a lorentz structure, return the factor if no more indices remain, 0 otherwise. *)
            Div[num_, den_] := Module[{quot},
               quot = WO`Simplify[num / den, Trig -> False];
               If[FreeQ[quot, Index] && FreeQ[quot, SP[_, _]], quot, 0]
            ];

            (* Take one brick, try to decompose and return the remainder. *)
            Onestep[brick_, mstruct_] := Module[{ebrick, estruct, key, coeff},
               ebrick = Expand[brick];
               (* Take the first monomial as key. *)
               key = If[MatchQ[ebrick, _Plus], ebrick[[1]], ebrick];
               estruct = Expand[mstruct];
               (* Divide target monomials by key, build list of coefficients. *)
               estruct = If[MatchQ[estruct, _Plus], List @@ estruct, {estruct}];
               coeff = Plus @@ (Div[#, key]& /@ estruct);
               (* Push total coefficient to the coefficient list *)
               AppendTo[coeffs, coeff];
               (* Return remainder. *)
               Simplify[mstruct - coeff * brick, Trig -> False]
            ];
            (* Iterate this procedure over all bricks on the respective remainders. *)
            Iterate[{hd_, tl__}, mstruct_] := Iterate[{tl}, Onestep[hd, mstruct]];
            Iterate[{hd_}, mstruct_] := Onestep[hd, mstruct];
            (* If we were successfull, the remainder must be zero. *)
            If[TrueQ [Iterate[lego, struct] == 0], coeffs, Null]
         ];
         CheckLorentzBackend[brick_, struct_] := Module[{cpl},
            cpl = CheckLorentzBackend[{brick}, struct];
            Switch[cpl, Null, Null, _, cpl[[1]]]
         ];

         (* Multiplies the coupling with the overall -i and also accounts for the modified  *
          * normalization of color octets in the color flow basis.                          *)
         CheckLorentz[brick_, struct_, fact_] := Module[{cpl},
            cpl = CheckLorentzBackend[brick, struct];
            If [cpl =!= Null, Throw[
               Simplify[(-I)
                  * 1 / Sqrt[2^(Plus @@ (Switch[WO`hash["colrep", PartName[#[[1]]]], "O", 1, _, 0]& /@ partlist))]
                  * fact * cpl, Trig -> False
               ]
            ]];
         ];
         CheckLorentz[brick_, fact_] := CheckLorentz[brick, coupling, fact];
         CheckLorentz[brick_] := CheckLorentz[brick, 1];

         (* Some helpers to ease writing the patterns *)
         LInd[x__] := Index[Lorentz, Ext[x]];
         SInd[x_] := Index[Spin, Ext[x]];
         Met[List[x__], List[y__]] := ME[Index[Lorentz, Ext[x]], Index[Lorentz, Ext[y]]];
         Met[x_, y_] := ME[Index[Lorentz, Ext[x]], Index[Lorentz, Ext[y]]];

         (* Register a vertex. If given a list of lorentz structures and couplings, we interpret *
          * these as a list of vertices.                                                         *)
         RegisterVertex[Hold[list_], perm_List, struct_List, cpl_List] :=
            MapThread[RegisterVertex[Hold[list], #1, #2, #3]&, {perm, struct, cpl}];
         RegisterVertex[Hold[list_], struct_List, cpl_List] :=
            MapThread[RegisterVertex[Hold[list], #1, #2]&, {struct, cpl}];
         (* 3 arguments -> trivial permutation *)
         RegisterVertex[Hold[list_], struct_, cpl_] :=
            RegisterVertex[Hold[list], {}, struct, cpl];
         (* 4 arguments -> permute the particles, {} means trivial permutation *)
         RegisterVertex[Hold[list_], perm_List, struct_, cpl_] :=
            Module[{plist},
               plist = If[perm === {}, partlist, (partlist[[#]]& /@ perm)];
               AppendTo[list, {WO`Concat[WO`hash["constr", PartName[#[[1]] ]]& /@ plist, ", "], struct,
                  RegisterCoupling[partlist, cpl]}];
            ];

         Digest3ary := Module[{i1, i2, i3, cpl, struct, PermuteParticles, perm = {}, futile = False},

            (* This function MUST be called prior to checking the lorentz structures. It performs  *
             * a final permutation of the particles to place them in the proper order for O'Mega   *
             * to work. It then strips the color structure. Remaining IndexDeltas are replaced by  *
             * ID as the Einstein convention awareness of IndexDelts causes nasty issues with the  *
             * Lorentz matcher.                                                                    *)
            PermuteParticles[j1_, j2_, j3_] := Module[{},
               (* Permute... *)
               partlist = {partlist[[j1]], partlist[[j2]], partlist[[j3]]};
               (* ... and save new indices. *)
               i1 = partlist[[1, 2]]; i2 = partlist[[2, 2]]; i3 = partlist[[3, 2]];
               coupling = StripColor[coupling, partlist];
               coupling = coupling /. {IndexDelta[x_, y_] -> ID[x, y]};
               If[Not[FreeQ[coupling, Gluon] && FreeQ[coupling, Colour]],
                  Throw[Futile[
                     "WARNING: I have failed to strip the color structure from a vertex, it will be skipped..."], WO`ESkip]];
            ];

            (* We proceed like this: first identify the spins that are meeting; then, for each spin,   *
             * we go through all the known lorentz structures. If none matches, we print a warning and *
             * skip the vertex. This would be the point to insert a hook for generating new lorentz    *
             * structures on the fly...                                                                *)
            cpl = Switch[tag,
               (* The case of trilinear couplings between two fermions and a boson can be *
                * streamlined. The clashing-arrows-code is not active at the moment as    *
                * O'Mega only knows a subset of the required interactions.                *)
               "fFV"|"fMV"|"FMV"|"fFS"|"fMS"|"FMS"|"MMS"|"MMV",
                  Module[{descr, ferm1, ferm2, Genstruct},
                     Genstruct = (""
                        <> "Coupling." <> descr <> " (1, Coupling." <> ferm1 <> ", Coupling."
                        <> # <> ", Coupling." <> ferm2 <> ")")&;
                     (* Put the particles into the correct ordering required by O'Mega. *)
                     If[MatchQ[StringTake[tag, 2], "fF"|"fG"|"fM"|"MM"],
                        PermuteParticles[1, 3, 2]
                     ,
                        PermuteParticles[2, 3, 1]
                     ];
                     (* Adjust the coupling type and fermion descriptors. *)
                     {descr, ferm1, ferm2} = Switch[StringTake[tag, 2],
                        "fF", {"FBF", "Psibar", "Psi"},
                        "fG", {"BBB", "Psibar", "Psibar"},
                        "Fg", {"PBP", "Psi", "Psi"},
                        "fM", {"FBF", "Psibar", "Chi"},
                        "FM", {"FBF", "Chibar", "Psi"},
                        "MM", {"FBF", "Chibar", "Chi"},
                        _, Throw[WO`EAbort, "Internal..."]
                     ];
                     Catch[
                        (* Distinguish the cases of scalar vs. vector couplings here. *)
                        If[StringTake[tag, {3, 3}] == "V",
                           struct = Genstruct["V"];
                           CheckLorentz[Ga[LInd[i2], SInd[i1], SInd[i3]]];
                           struct = Genstruct["VL"];
                           (* The factor 1/2 is due to the different definitions of P_+- in O'Mega and FeynRules. *)
                           CheckLorentz[TensDot[Ga[LInd[i2]], ProjM][SInd[i1], SInd[i3]], 1/2];
                           struct = Genstruct["VR"];
                           CheckLorentz[TensDot[Ga[LInd[i2]], ProjP][SInd[i1], SInd[i3]], 1/2];
                           struct = Genstruct["VLR"];
                           CheckLorentz[{TensDot[Ga[LInd[i2]], ProjM][SInd[i1], SInd[i3]],
                              TensDot[Ga[LInd[i2]], ProjP][SInd[i1], SInd[i3]]}, 1/2];
                           struct = Genstruct["A"];
                           CheckLorentz[- TensDot[Ga[LInd[i2]], Ga[5]][SInd[i1], SInd[i3]]];
                           struct = Genstruct["VA"];
                           CheckLorentz[{Ga[LInd[i2], SInd[i1], SInd[i2]],
                              - TensDot[Ga[Lind[i2]], Ga[5]][Sind[i2], SInd[i2]]}];
                        ,
                           struct = "Coupling.FBF (1, Coupling.Psibar, Coupling.S, Coupling.Psi)";
                           CheckLorentz[ID[SInd[i1], SInd[i3]]];
                           (* Wondering about 1/2? See above. *)
                           struct = "Coupling.FBF (1, Coupling.Psibar, Coupling.SL, Coupling.Psi)";
                           CheckLorentz[ProjM[SInd[i1], SInd[i3]], 1/2];
                           struct = "Coupling.FBF (1, Coupling.Psibar, Coupling.SR, Coupling.Psi)";
                           CheckLorentz[ProjP[SInd[i1], SInd[i3]], 1/2];
                           struct = "Coupling.FBF (1, Coupling.Psibar, Coupling.SLR, Coupling.Psi)";
                           CheckLorentz[{ProjM[SInd[i1], SInd[i3]], ProjP[SInd[i1], SInd[i3]]}, 1/2];
                           struct = "Coupling.FBF (1, Coupling.Psibar, P, Coupling.Psi)";
                           CheckLorentz[Ga[5, SInd[i1], SInd[i3]]];
                        ];
                        (* If nothing has matched we print a warning. *)
                        Throw["WARNING: " <> tag <> " type vertex with unknown lorentz structure, skipping...", WO`ESkip];
                     ]
                  ],
               "fGS", Catch[
                  PermuteParticles[1, 3, 2];
                  struct = "Coupling.BBB (1, Coupling.Psibar, Coupling.SL, Coupling.Psibar)";
                  CheckLorentz[ProjM[SInd[i1], SInd[i3]], 1/2];
                  struct = "Coupling.BBB (1, Coupling.Psibar, Coupling.SR, Coupling.Psibar)";
                  CheckLorentz[ProjP[SInd[i1], SInd[i3]], 1/2];
                  struct = "Coupling.BBB (1, Coupling.Psibar, Coupling.SLR, Coupling.Psibar)";
                  CheckLorentz[{ProjM[SInd[i1], SInd[i3]], ProjP[SInd[i1], SInd[i3]]}, 1/2];
                  Module[{c},
                     c = Catch[CheckLorentz[{ID[SInd[i1], SInd[i3]], -Ga[5, SInd[i1], SInd[i3]]}]];
                     If[c =!= Null, Throw[{c[[1]] + c[[2]], c[[1]] - c[[2]]}]];
                  ];
                  Throw["WARNING: " <> tag <> " type vertex with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "FgS", Catch[
                  PermuteParticles[2, 3, 1];
                  struct = "Coupling.PBP (1, Coupling.Psi, Coupling.SL, Coupling.Psi)";
                  CheckLorentz[ProjM[SInd[i1], SInd[i3]], 1/2];
                  struct = "Coupling.PBP (1, Coupling.Psi, Coupling.SR, Coupling.Psi)";
                  CheckLorentz[ProjP[SInd[i1], SInd[i3]], 1/2];
                  struct = "Coupling.PBP (1, Coupling.Psi, Coupling.SLR, Coupling.Psi)";
                  CheckLorentz[{ProjM[SInd[i1], SInd[i3]], ProjP[SInd[i1], SInd[i3]]}, 1/2];
                  Module[{c},
                     c = Catch[CheckLorentz[{ID[SInd[i1], SInd[i3]], -Ga[5, SInd[i1], SInd[i3]]}]];
                     If[c =!= Null, Throw[{c[[1]] + c[[2]], c[[1]] - c[[2]]}]];
                  ];
                  Throw["WARNING: " <> tag <> " type vertex with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "VVV", Catch[
                  PermuteParticles[1, 2, 3];
                  struct = "Gauge_Gauge_Gauge 1";
                  (* Another O'Mega special: the vertex is multiplied by I internally, so we must *
                   * compensate for it.                                                           *)
                  CheckLorentz[
                     - Met[i1, i2] * (2 * FV[i2, LInd[i3]] + FV[i3, LInd[i3]])
                     + Met[i1, i3] * (2 * FV[i3, LInd[i2]] + FV[i2, LInd[i2]])
                     + Met[i2, i3] * (FV[i2, LInd[i1]] - FV[i3, LInd[i1]]),
                     coupling /. {FV[i1, j_] :> - FV[i2, j] - FV[i3, j]}, -I];
                  (* Anomalous dimension 4 TVCs are a bit more tricky: we have to decompose them *
                   * in a unique basis and translate the result into the O'Mega basis of         *
                   * permuted longitudinal / transverse aTGC operators.                          *
                   *                                                                             *
                   * Limitation: this does not work if identical fields meet at the   *
                   * vertex due to the asymmetric nature of the couplings in O'Mega - the only   *
                   * way to get the necessary permutations is permuting the particles at the     *
                   * vertex, but if those are identical... you get the idea :)                   *
						 *                                                                             *
                   * Another tricky point are color flows. As we are permuting the particles     *
                   * after stripping color, we have to take care not to change the flow.         *
                   * While I can't see a reliable way of doing this in the n-point case, the     *
                   * only potentially problematic color structure in the three-point case is OOO.*
                   * There, we have a cyclic and a anticyclic flow, and if we only perform       *
                   * cyclic permutations, the flow is unchanged -> take care to only do cyclic   *
                   * permutations.                                                               *)
                  Module[{a}, If [(a = Catch[Module[{dec, all},
                     If[partlist[[1, 1]] === partlist[[2, 1]] || partlist[[1, 1]] == partlist[[3, 1]] ||
                           partlist[[2, 1]] === partlist[[3, 1]],
                        futile = True;
                        Print[""
                           <> "LIMITATION: O'Mega only handles anomalous triple vector couplings "
                           <> "if the fields are mutually different."];
                        Throw[Null]];
                     dec = Catch[CheckLorentz[{
                           FV[i1, LInd[i1]] * Met[i2, i3], FV[i1, LInd[i2]] * Met[i1, i3],
                           FV[i1, LInd[i3]] * Met[i1, i2], FV[i2, LInd[i1]] * Met[i2, i3],
                           FV[i2, LInd[i2]] * Met[i1, i3], FV[i2, LInd[i3]] * Met[i1, i2]
                        },
                           coupling /. {FV[i3, j_] :> - FV[i1, j] - FV[i2, j]}
                        , -I
                     ]];
                     If[dec === Null, Throw[Null]];
                     all = Select[Simplify[Join[
                        {#[[1]], "Dim4_Vector_Vector_Vector_L 1", #[[2]]}& /@ {
                           {{1, 2, 3}, dec[[1]] - dec[[4]] / 2}, {{2, 3, 1}, dec[[5]] - dec[[2]] / 2},
                           {{3, 1, 2}, - (dec[[3]] + dec[[6]]) / 2}},
                        {#[[1]], "Dim4_Vector_Vector_Vector_T 1", #[[2]]}& /@ {
                           {{2, 3, 1}, dec[[2]] / 2}, {{3, 1, 2}, (dec[[6]] - dec[[3]]) / 2},
                           {{1, 2, 3}, - dec[[4]] / 2}}
                        ], Trig -> False], !TrueQ[#[[3]] == 0]&]; 
                     perm = #[[1]]& /@ all;
                     struct = #[[2]]& /@ all;
                     Throw[#[[3]]& /@ all];
                  ]]) =!= Null,
                     If[Length[a] > 1,
                        Print[partlist];
                        Print[""
                              <> "WARNING: splitting vertex into " <> ToString[Length[a]] <> " pieces, "
                              <> "the diagram count reported by O'Mega will not be accurate anymore."];
                     ];
                     Throw[a]]];
                  Throw[If[futile, Futile[#], #]&("WARNING: VVV type vertex with unknown lorentz structure, skipping..."), WO`ESkip];
               ],
               "SVV", Catch[
                  PermuteParticles[1, 2, 3];
                  struct = "Scalar_Vector_Vector 1";
                  CheckLorentz[Met[i2, i3]];
                  struct = "Dim5_Scalar_Gauge2 1";
                  CheckLorentz[FV[i2, LInd[i3]] * FV[i3, LInd[i2]] - SP[i2, i3] * Met[i2, i3],
                     coupling /. {FV[i1, i_] :> - FV[i2, i] - FV[i3, i]}, 1];
                  Throw["WARNING: SVV type vertex with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "SSS", Catch[
                  PermuteParticles[1, 2, 3];
                  struct = "Scalar_Scalar_Scalar 1";
                  CheckLorentz[1];
                  Throw["WARNING: SSS type vertex with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "SSV", Catch[
                  PermuteParticles[3, 1, 2];
                  struct = "Coupling.Vector_Scalar_Scalar 1";
                  CheckLorentz[FV[i3, LInd[i1]] - FV[i2, LInd[i1]],
                     coupling /. {FV[i1, j_] :> - FV[i2, j] - FV[i3, j]}, 1];
                  Throw["WARNING: SSV type vertex with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "SST", Catch[
                  PermuteParticles[3, 1, 2];
                  struct = "Coupling.Graviton_Scalar_Scalar 1";
                  Module[{c},
                     c = Catch[CheckLorentz[
                        {
                           - Met[{i1, 1}, {i1, 2}]
                        ,
                           - Met[{i1, 1}, {i1, 2}] * SP[i2, i3] +
                           FV[i2, LInd[i1, 1]] * FV[i3, LInd[i1, 2]] +
                           FV[i2, LInd[i1, 2]] * FV[i3, LInd[i1, 1]]
                        }, coupling /. {FV[i1, j_] :> - FV[i2, j] - FV[i3, j]}, 1]
                     ];
                     If[(c = Catch[
                           If[c === Null, Throw[Null]];
                           If[WO`hash["mass", PartName[partlist[[2, 1]]]] =!= WO`hash["mass", PartName[partlist[[3, 1]]]],
                              Throw[Null]];
                           If[c[[2]] == 0, Throw[Null]];
                           If[Simplify[
                                 (c[[1]] / c[[2]] - WO`hash["mass", PartName[partlist[[2, 1]]]]^2) 
                                 //. (Rule[#[[1]], #[[2]]]& /@ WO`IParamList) /. ZERO -> Null
                              ] == 0, Throw[c[[2]]]
                           ];
                        ]) =!= Null, Throw[c]
                     ];
                  ];
                  Throw["WARNING: SST type vertix with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "TVV", Catch[
                  PermuteParticles[1, 2, 3];
                  struct = "Coupling.Graviton_Vector_Vector 1";
                  Module[{c, csym, dsym},
                     csym[m_, n_, r_, s_] :=
                        ME[m, r] * ME[n, s] + ME[m, s] * ME[n, r] - ME[m, n] * ME[r, s];
                     dsym[m_, n_, r_, s_, p1_, p2_] := ME[m, n] * FV[p1, s] * FV[p2, r]
                        - (Plus @@ ((ME[#[[1]], s] * FV[p1, #[[2]]] * FV[p2, r] + ME[#[[1]], r] * FV[p1, s] * FV[p2, #[[2]]]
                        - ME[r, s] * FV[p1, #[[1]]] * FV[p2, #[[2]]])& /@ {{m, n}, {n, m}} ));
                     c = Catch[CheckLorentz[
                        {
                           - csym[LInd[i1, 1], LInd[i1, 2], LInd[i2], LInd[i3]]
                        ,
                           - SP[i2, i3] * csym[LInd[i1, 1], LInd[i1, 2], LInd[i2], LInd[i3]]
                           - dsym[LInd[i1, 1], LInd[i1, 2], LInd[i2], LInd[i3], i2, i3]
                        }, coupling /. {FV[i1, j_] :> - FV[i2, j] - FV[i3, j]}, 1];
                     ];
                     If[(c = Catch[
                           If[c === Null, Throw[Null]];
                           If[WO`hash["mass", PartName[partlist[[2, 1]]]] =!= WO`hash["mass", PartName[partlist[[3, 1]]]],
                              Throw[Null]];
                           If[c[[2]] == 0, Throw[Null]];
                           If[Simplify[
                                 (c[[1]] / c[[2]] - WO`hash["mass", PartName[partlist[[2, 1]]]]^2) 
                                 //. (Rule[#[[1]], #[[2]]]& /@ WO`IParamList) /. ZERO -> 0
                              ]== 0, Throw[c[[2]]]];
                        ]) =!= Null, Throw[c]
                     ];
                  ];
                  Throw["WARNING: VVT type vertix with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "fFT", Catch[
                  PermuteParticles[1, 3, 2];
                  struct = "Coupling.Graviton_Spinor_Spinor 1";
                  Module[{c},
                     c = Catch[CheckLorentz[
                        {
                           - Met[{i2, 1}, {i2, 2}] * ID[SInd[i1], SInd[i3]]/ 2
                        ,
                           Ga[LInd[i2, 1], SInd[i1], SInd[i3]] * (FV[i3, LInd[i2, 2]] - FV[i1, LInd[i2, 2]]) / 8 +
                           Ga[LInd[i2, 2], SInd[i1], SInd[i3]] * (FV[i3, LInd[i2, 1]] - FV[i1, LInd[i2, 1]]) / 8 +
                           Met[{i2, 1}, {i2, 2}] * (SlashedP[i1, SInd[i1], SInd[i3]] - SlashedP[i3, SInd[i1], SInd[i3]]) / 4
                        }, coupling /. {FV[i2, j_] :> - FV[i1, j] - FV[i3, j]}, 1];
                     ];
                     If[(c = Catch[
                           If[c === Null, Throw[Null]];
                           If[WO`hash["mass", PartName[partlist[[1, 1]]]] =!= WO`hash["mass", PartName[partlist[[3, 1]]]],
                              Throw[Null]];
                           If[c[[2]] == 0, Throw[Null]];
                           If[Simplify[
                                 (c[[1]] / c[[2]] + WO`hash["mass", PartName[partlist[[1, 1]]]]) 
                                 //. (Rule[#[[1]], #[[2]]]& /@ WO`IParamList) /. ZERO -> 0
                              ]== 0, Throw[c[[2]]]];
                        ]) =!= Null, Throw[c]
                     ];

                  ];
                  Throw["WARNING: fFT type vertix with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               _, Throw[Futile["WARNING: unidentified vertex of arity 3 (spin structure: " <> tag <> "), skipping..."], WO`ESkip];
            ];
            (* If we have identified our friend, we append it to the definition list. *)
            If [perm === {} && MatchQ[struct, _List], perm = {}& /@ struct];
            If[cpl =!= Null, RegisterVertex[
               If[FreeQ[WO`hash["colrep", PartName[#[[1]]]]& /@ partlist, "O"], Hold[v3deflist], Hold[v3deflist$c]], perm, struct, cpl]];
         ];


       Digest4ary := Module[{i1, i2, i3, i4, cpl, struct, PermuteParticles},

            (* See above, MUST be called under all circumstances. *)
            PermuteParticles[j1_, j2_, j3_, j4_] := Module[{},
               partlist = {partlist[[j1]], partlist[[j2]], partlist[[j3]], partlist[[j4]]};
               i1 = partlist[[1, 2]]; i2 = partlist[[2, 2]]; i3 = partlist[[3, 2]]; i4 = partlist[[4, 2]];
               coupling = StripColor[coupling, partlist];
               coupling = coupling /. {IndexDelta[x_, y_] -> ID[x, y]};
               If[Not[FreeQ[coupling, Gluon] && FreeQ[coupling, Colour]],
                  Throw["WARNING: I have failed to strip the color structure from a vertex, it will be skipped...", WO`ESkip]];
            ];

            cpl = Switch[tag,
               "SSSS", Catch[
                  PermuteParticles[1, 2, 3, 4];
                  struct = "Scalar4 1";
                  CheckLorentz[1];
                  Throw["WARNING: SSSS type vertex with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "SSVV", Catch[
                  PermuteParticles[1, 2, 3, 4];
                  struct = "Scalar2_Vector2 1";
                  CheckLorentz[Met[i3, i4]];
                  Throw["WARNING: SSVV type vertex with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               "VVVV", Catch[
                     PermuteParticles[1, 2, 3, 4];
                     Catch[Module[{i, cpl, nums, dens, fact},
                     (* Try to decompose the vertex structure in terms of inequivalent contractions. *)
                     cpl = Catch[CheckLorentz[{Met[i1, i2] * Met[i3, i4], Met[i1, i3] * Met[i2, i4],
                        Met[i1, i4] * Met[i2, i3]}]];
                     If[cpl === Null, Throw[Null]];
                     (* If it worked, check if the decomposition can be split in a prefactor and *
                      * a list of integers.                                                      *)
                     If[!TrueQ[cpl[[1]] == 0],
                        fact = Simplify[cpl / cpl[[1]], Trig -> False];
                        nums = Numerator[fact];
                        dens = Denominator[fact];
                     ];
                     If[And @@ (IntegerQ /@ Join [nums, dens]) && !TrueQ[cpl[[1]] == 0],
                        (* We can? -> Assemble a single coupling. *)
                        cpl = cpl[[1]] / (LCM @@ dens);
                        fact = fact * (LCM @@ dens);
                        struct = "Vector4 [" <> WO`Concat[{
                           If[fact[[1]] != 0, ToString[fact[[1]]] <> ", Coupling.C_12_34", ""],
                           If[fact[[2]] != 0, ToString[fact[[2]]] <> ", Coupling.C_13_42", ""],
                           If[fact[[3]] != 0, ToString[fact[[3]]] <> ", Coupling.C_14_23", ""]}, "; "] <> "]";
                        Throw[cpl];, 
                        (* We cannot? -> Generate several couplings then. *)
                        struct = ("Vector4 [" <> # <> "]")& /@
                           {"1, Coupling.C_12_34", "1, Coupling.C_13_42", "1, Coupling.C_14_23"};
                        (* Drop all pieces that are zero. *)
                        Module[{tmp},
                           tmp = Select[MapThread[{#1, #2}&, {struct, cpl}], !TrueQ[#[[2]] == 0]&];
                           struct = #[[1]]& /@ tmp;
                           cpl = #[[2]]& /@ tmp;
                        ];
                        If[Length[cpl] > 1,
                           Print[partlist];
                           Print[""
                              <> "WARNING: splitting vertex into " <> ToString[Length[cpl]] <> " pieces, "
                              <> "the diagram count reported by O'Mega will not be accurate anymore."];
                        ];
                        Throw[cpl];
                     ];
                  ], Null];
                  Throw["WARNING: VVVV type vertex with unknown lorentz structure, skipping...", WO`ESkip];
               ],
               _, Throw[Futile["WARNING: unidentified vertex of arity 4 (spin structure: " <> tag <> "), skipping..."], WO`ESkip];
            ];
            (* If we have identified our friend, we append it to the definition list and record its arity. *)
            If[cpl =!= Null,
               RegisterVertex[
                  If[FreeQ[WO`hash["colrep", PartName[#[[1]]]]& /@ partlist, "O"], Hold[v4deflist], Hold[v4deflist$c]], struct, cpl];
               WO`maxarity = Max[arity, WO`maxarity];
            ];
         ];

         Digestnary := Module[{},
            Throw[Futile["Vertices of arity > 4 are not implemented yet, skipping vertex...."], WO`ESkip];
         ];
         
         SetAttributes[ID, Orderless];
         tag  = vspec[[1]];
         (* We map charge conjugation to hermitian conjugation. *)
         partlist = vspec[[2]] /. CC -> WO`HC;
         coupling = vspec[[3]];
         arity = Length[partlist];
         Catch[
            If[Or @@ (((GhostFieldQ[#[[1]]] === True) ||
                  (WO`gauge === WO`WOUnitarity && WO`hash["goldstone", PartName[#[[1]]]]))&/@ partlist),
               nghost++; nskipped++ ,
               Switch[arity, 3, Digest3ary[], 4, Digest4ary[], _, Digestnary[]]
            ],
         (* Retry with the simplified structure if WOFast is off*)
         WO`ESkip, Function[{val, tag},
            Catch[
               Switch[{WO`fast, val},
                  {True, Futile[_]}, Throw[val, tag],
                  {True, _},
                     Print[""
                        <> "Vertex could not be identified (see below), but setting WOFast -> False "
                        <> "might help..."];
                     Throw[val, tag],
                  {False, Futile[_]}, Throw[val, tag],
                  {False, _},
                     partlist = vspec[[2]] /. CC -> WO`HC;
                     coupling = FullSimplify[vspec[[3]], ExcludedForms->{x_/;(FreeQ[x, Index] && FreeQ[x, Dot])}];
                     Switch[arity, 3, Digest3ary[], 4, Digest4ary[], _, Digestnary[]]
               ],
            WO`ESkip, Function[{val, tag},
               val /. {s_String :> Print[s], Futile[s_String]:> Print[s]};
               Print["Skipped vertex: " <> WO`Concat[ToString[PartName[#[[1]]]]& /@ partlist, " , "]];
               Print[partlist];
               If[WO`verbose,
                  Print["Verbose vertex structure:"];
                  Print[coupling];
               ];
               nskipped++;
            ]];
         ]];
         nproc++;
         If[WO`progress > 0 && Mod[nproc, WO`progress] == 0,
            Print["   " <> ToString[nproc] <> " of " <> ToString[Length[WO`vlist]] <> " vertices processed..."]
         ];
      ];

      (* Put the particle lists into a canonical order w.r.t. their lorentz representations. *
       * This works by assigning a tag to every representation:                              *
       * Scalar : S - Fermion : F - A-Fermion : f - Majorana : M - Vector : V - Tensor : T   *
       * The particles are then ordered lexicographically w.r.t. these tags; the tags are    *
       * joined for each vertex to give a sort of "fingerprint" of the lorentz structure.    *
       * Charge-conjugated fermions are tagged with G and g to allow the identification of   *
       * clashing-arrow type vertices.                                                       *)
          Module[{prepend, sort, tag, remove},
         prepend = If[FreeQ[#[[1]], CC],
            Prepend[#, WO`hash["lrep", PartName[ #[[1]] ]]]
         ,
            (* We _need_ the majorana modules for clashing arrow type stuff... *)
            WO`havemajoranas = True;
            Prepend[#, Switch[WO`hash["lrep", PartName[#[[1]] /. CC -> WO`HC]],
               (* As we have mapped CC to HC, we have an extra conjugation. *)
               "f", "G", "F", "g",
               _, Throw[WO`EAbort, "Charge conjugation appearing for a field other than a dirac fermion!"]
            ]]
         ]&;
         sort = Sort[#, Function[{x, y}, OrderedQ[{x[[1]], y[[1]]}]]]&;
         tagger = StringJoin[Function[{x}, x[[1]]] /@ #]&;
         remove = Take[#, {2, 3}]&;
         vlist = {sort[prepend /@ #[[1]]], #[[2]]}& /@ WO`vlist;
         vlist = {tagger[#[[1]]], remove /@ #[[1]], #[[2]]}& /@ vlist;
      ];
      (* Process the vertices. *)
      DigestVertex /@ vlist;
      Print["processed a total of " <> ToString[nproc] <> " vertices, kept "
         <> ToString[nproc - nskipped] <> " of them and threw away " <> ToString[nskipped]
         <> ", " <> ToString[nghost] <> " of which contained ghosts"
         <> If[WO`gauge === WO`WOUnitarity, " or goldstone bosons", ""] <> "."];
      (* Create constant type and translator. *)
      constant = ""
         <> "type constant =\n"
         <> WO`Indent[" | " <> WO`SmartConcat[#[[1]]& /@ WO`cpldeflist, " | "], 2]
         <> "\n";
      Module[{c},
         c = (#[[1]] <> " -> \"" <> WO`FirstUpper[ToLowerCase[#[[1]]]] <> "\"")& /@ WO`cpldeflist;
         constsym = ""
            <> "let constant_symbol = function\n"
            <> WO`Indent[" | " <> WO`SmartConcat[c, " | "], 2]
            <> "\n";
      ];

      (* Generate the actual vertex list. *)
      VlistMaker[Hold[list_]] := WO`Concat[
         ("(" <> #[[1]] <> "),\n " <> #[[2]] <> ",\n " <> #[[3]])& /@ list, "\n;\n"];
      VlistSmartMaker[Hold[list_], lname_] :=
         If[Length[list] > 0,
            "let " <> lname <> " = [\n" <> WO`Indent[VlistMaker[Hold[list]], 3] <> "\n]\n",
            "let " <> lname <> " = []\n"
         ];

      vertices = ""
         <> VlistSmartMaker[Hold[v3deflist], "vertices_3"]
         <> "\n"
         <> VlistSmartMaker[Hold[v3deflist$c], "vertices_3_qcd"]
         <> "\n"
         <> VlistSmartMaker[Hold[v4deflist], "vertices_4"]
         <> "\n"
         <> VlistSmartMaker[Hold[v4deflist$c], "vertices_4_qcd"]
         <> "\n"
         <> VlistSmartMaker[Hold[vndeflist], "vertices_n"]
         <> "\n"
         <> VlistSmartMaker[Hold[vndeflist$c], "vertices_n_qcd"]
         <> "\n"
         <> "let vertices () = (\n"
         <> "   vertices_3 @ (match Opts.color with true -> vertices_3_qcd | false -> []),\n"
         <> "   vertices_4 @ (match Opts.color with true -> vertices_4_qcd | false -> []),\n" 
         <> "   vertices_n @ (match Opts.color with true -> vertices_n_qcd | false -> [])\n" 
         <> ")\n"
   ];

   handle = OpenWrite[file];
   (* Parse the particle list. *)
   ParsePList[];
   (* Generate the vertex definitions *)
   DigestVertices[];
   (* The preamble *)
   preamble = ""
      <> WO`CommentMaker[WO`fileheader, "(* ", "   ", " *)"] <> "\n "
      <> "\n "
      <> "type gauge = Unitarity | Feynman | Rxi \n "
      <> "\n "
      <> "module type Frules_options =\n "
      <> "sig \n "
      <> "   val gauge: gauge \n "
      <> "   val color: bool \n "
      <> "end \n "
      <> "\n "
      <> "module Implementation (Opts: Frules_options) =\n "
      <> "struct \n "
      <> "\n "
      <> "   open Coupling \n ";
   (* Stuff independent of the actual model *)
   gauge = "type gauge = Xi\n";
   width = ""
      <> "let width x = match width_symbol x with \n"
      <> "   | \"fr_zero\" -> Vanishing\n"
      <> "   | _ -> Timelike\n";
   fermion = ""
      <> "let fermion x = (match lorentz x with\n"
      <> "   | Spinor -> 1 | ConjSpinor -> -1 | Majorana -> 2 | _ -> 0)\n";
   colsymm = "let colsymm _ = (0, false), (0, false)\n";
   maxdegree = "let max_degree () = " <> ToString[WO`maxarity] <> "\n";
   fusions = ""
      <> "module F = " <> If[WO`whizv2x[], "Modeltools", "Models"] <> ".Fusions (struct\n"
      <> "   type f = flavor\n"
      <> "   type c = constant\n"
      <> "   let compare = compare\n"
      <> "   let conjugate = conjugate\n"
      <> "end)\n"
      <> "\n"
      <> "let table = F.of_vertices (vertices ())\n"
      <> "let fuse2 = F.fuse2 table\n"
      <> "let fuse3 = F.fuse3 table\n"
      <> "let fuse = F.fuse table\n";
   extflavors = "let external_flavors () = [\"Autogenerated flavors\", flavors ()]\n";
   parameters = "let parameters () = {input = []; derived = []; derived_arrays = []}\n";
   flavorofstring = ""
      <> "let flavor_of_string x =\n"
      <> "   let dict = List.map (fun x -> (x, flavor_to_string x)) (flavors ())\n"
      <> "   in try\n"
      <> "      fst (List.find (fun (_, y) -> (x = y)) dict)\n"
      <> "   with\n"
      <> "      Not_found -> invalid_arg \"" <> WO`omeganame <> ".Implementation: flavor_of_string: invalid particle?\"\n";
   gaugesym = ""
      <> "let gauge_symbol = function\n"
      <> "   Xi -> (match Opts.gauge with\n"
      <> "      | Unitarity -> invalid_arg\n"
      <> "         \"" <> WO`omeganame <> ".Implementation: requesting gauge symbol in unitarity gauge!\"\n"
      <> "      | Feynman -> \"one\"\n"
      <> "      | Rxi -> \"" <> ToString[WO`gsym] <> "\")\n";
   options = "let options = Options.empty\n";
   rcs = ""
      <> "let rcs = RCS.parse \"" <> WO`omeganame <> "\" [\"automatically generated model\"] {\n"
      <> "   RCS.revision = \"Revision: Noop\";\n"
      <> "   RCS.date = \"Date: Noop\";\n"
      <> "   RCS.author = \"Author: Mathematica automaton\";\n"
      <> "   RCS.source = \"Source: Noop\"\n"
      <> "}\n";
   goldstone = "let goldstone _ = None\n";
   sanscolorstubs = ""
      <> "type flavor_sans_color = flavor\n"
      <> "let flavor_sans_color x = x\n"
      <> "let conjugate_sans_color = conjugate\n"
      <> "let flavor_sans_color_of_string = flavor_of_string\n"
      <> "let flavor_sans_color_to_string = flavor_to_string\n"
      <> "let flavor_sans_color_to_TeX = flavor_to_TeX\n"
      <> "let flavor_sans_color_symbol = flavor_symbol\n";
   charges = ""
      <> "module Ch = Charges.Null\n"
      <> "let charges _ = ()\n";
   finalwords = "end\n";
   (* Assemble the module. *)
   WriteString[handle, "" 
      <> preamble <> "\n" <> WO`Indent [ ""
         <> flavor <> "\n" <> conjugate <> "\n" <> color <> "\n" <> pdg <> "\n" <> lorentz <> "\n" <> gauge <> "\n"
         <> propagator <> "\n" <> widthsym <> "\n" <> width <> "\n" <> fermion <> "\n"
         <> colsymm <> "\n" <> flavors <> "\n" <> extflavors <> "\n" <> goldstone <> "\n"
         <> flavortostring <> "\n" <> If[FreeQ[{"1.92"}, WO`whizv], texsym <> "\n", ""]
         <> flavorofstring <> "\n" <> flavorsym <> "\n" <> gaugesym <> "\n"
         <> masssym <> "\n"
         <> "(* Coupling constants and parameters *)\n \n "
         <> constant <> "\n " <> parameters <> "\n " <> constsym <> "\n "
         <> "(* Vertices and fusions *)\n \n "
         <> maxdegree <> "\n " <> vertices <> "\n " <> fusions <> "\n "
         <> If[WO`whizv30[], nc <> "\n", ""]
         <> If[WO`whizv30[], caveats <> "\n", ""]
         <> If[WO`whizvn[] >= WO`whizvn["2.0.3"], ""
               <> "(* Charge (stubbed) *)\n \n "
               <> charges
               <> "\n "
            , ""]
         <> "(* Misc. infrastructure *)\n \n "
         <> options <> "\n" <> If[MemberQ[{"1.92", "1.93", "1.96", "2.0", "2.0.3", "2.2.3", "2.3.0", "2.4.0"}, WO`whizv], rcs <> "\n", ""]
         <> If[WO`whizv2x[], sanscolorstubs <> "\n ", ""]
      , 3] <> finalwords
   ];
   Close[handle];
];

(* Write the O'Mega binary driver *)
WO`WriteOmegaBinary[file_, cfile_, ncf_] := Module[{handle, contents, fusions, modname, gauge,
   header, target, colorinvanilla},
   (* The fusions module. *)
   fusions = Switch[WO`maxarity, _?(#<=3&), "Binary", _?(#<=4&), "Mixed23", _?(#>4&), "Nary"]
      <> If[WO`havemajoranas, "_Majorana", ""];
   (* The module name. *)
   modname = WO`FirstUpper[WO`omeganame] <> If[WO`whizv2x[], "_mdl", ""];
   (* The gauge constructor. *)
   gauge = modname <> "." <> WO`GaugeName[];
   colorinvanilla = If[WO`whizv2x[], "true", "false"];
   target = "Targets.Fortran" <> If[WO`havemajoranas, "_Majorana", ""];
   header = WO`CommentMaker[WO`fileheader, "(* ", "   ", " *)"] <> "\n ";
   contents = ""
      <> header
      <> "\n "
      <> "module O =  Omega.Make (Fusion." <> fusions <> ") (" <> target <> ")\n "
      <> "   (" <> modname <> ".Implementation (struct let gauge = " <> gauge <> " let color = " <> colorinvanilla <> " end))\n "
      <> "\n "
      <> "let _ = O.main ()\n ";
   handle = OpenWrite[file];
   WriteString[handle, contents];
   Close[handle];
   contents = ""
      <> header
      <> "\n "
      <> "module O =  Omega.Make (Fusion." <> fusions <> ") (" <> target <> ")\n "
      <> "   (Colorize.It (struct let max_num = " <> ToString[ncf] <> " end)\n "
      <> "      (" <> modname <> ".Implementation\n "
      <> "         (struct let gauge = " <> gauge <> " let color = true end)))\n "
      <> "\n "
      <> "let _ = O.main ()\n ";
   If[WO`whizv19x[],
      handle = OpenWrite[cfile];
      WriteString[handle, contents];
      Close[handle];
   ];
];

(* Write the oplotter glue library. *)
WO`WriteOplGlue[file_] := Module[{header, footer, contents, pardefs, namelist, DoParams,
   DoTranslators, translators, contains, initparams},

   (* Generate the parameter definitions and the namelist. *)
   DoParams[] := Module[{realparams, cmplxparams, realvars, cmplxvars, realchecklist, isreal, rendercpl},
      (* Real parameters are all numerical masses, widths and externals which are flagged as real. *)
      realparams = Join [
         {#[[2]], #[[3]]}& /@ Select[Join[WO`masslist, WO`widthlist], (NumericQ[#[[3]]])&],
         ({#[[1]], #[[2]]} & /@ Select[
            {#[[2, 1]], #[[2, -3]], #[[2, -2]]} & /@ Flatten[#[[2]] & /@ WO`EParamList, 1],
         Not[#[[3]]] &])];
      (* There are some double occurances of widths and masses I don't understand (TODO: Neil?), *
       * so weed them out here. The same applies below.                                          *)
      realparams = WO`WeedOutList[realparams, FreeQ[#2, #1[[1]]]&];
      If[realparams != {},
         pardefs = WO`FortranSplit["real(kind=double), public :: " <> WO`Concat[
            (ToString[#[[1]]] <> "=" <> ToString[#[[2]]] <> "_double")& /@ realparams, ", "]
            <> "\n"];
      ];
      (* Complex parameters are all externals flagged as complex (obviously). *)
      cmplxparams =
         ({#[[1]], #[[2]]} & /@ Select[
            {#[[2, 1]], #[[2, -3]], #[[2, -2]]} & /@ Flatten[#[[2]] & /@ WO`EParamList, 1],
         #[[3]]&]);
      cmplxparams = WO`WeedOutList[cmplxparams, FreeQ[#2, #1[[1]]]&];
      If[cmplxparams != {},
         pardefs = pardefs <> WO`FortranSplit["\ncomplex(kind=double), public :: " <> WO`Concat[
            (ToString[#[[1]]] <> "=" <> WO`FortranComplex[#[[2]]])& /@ cmplxparams, ", "]
            <> "\n"];
      ];
      (* Real variables are all nonumerical masses, widths and internals flagged as real. Actually, *
       * we have to separate internals and masses/widths to make the latter private and the others  *
       * public                                                                                     *)
      realvars =    WO`WeedOutList[
         #[[2]]& /@ Select[Join[WO`masslist, WO`widthlist], (Not[NumericQ[#[[3]]]])&]];
      If[realvars != {},
         pardefs = pardefs <>
            WO`FortranSplit["\nreal(kind=double), public :: " <> WO`Concat[ToString /@ realvars, ", "]
            <> "\n"];
      ];
      (* Take also care to include nothing that has already been declared. *)
      realvars =    Select[WO`WeedOutList[
         #[[1]]& /@ Select[{#[[1]], #[[-2]]}& /@ WO`IParamList, Not[#[[2]]]&]], FreeQ[realvars, #]&];
      If[realvars != {},
         pardefs = pardefs <>
            WO`FortranSplit["\nreal(kind=double) :: " <> WO`Concat[ToString /@ realvars, ", "]
            <> "\n"];
      ];

      (* Internals flagged as complex. *)
      cmplxvars =    WO`WeedOutList[
         #[[1]]& /@ Select[{#[[1]], #[[-2]]}& /@ WO`IParamList, #[[2]]&]];
      If[cmplxvars != {},
         pardefs = pardefs <>
            WO`FortranSplit["\ncomplex(kind=double) :: " <> WO`Concat[ToString /@ cmplxvars, ", "]
            <> "\n"];
      ];
      (* The namelist presented to oplotter. *)
      namelist = WO`FortranSplit[""
         <> "namelist /parameters/ "
         <> WO`Concat[ToString[#[[1]]]& /@ Join[realparams, cmplxparams], ", "]
         <> "\n"];
      (* A list of functions, each checking if an expression is free of respective complex quantities. *)
      realchecklist =
         Function[{par}, Function[{exp}, FreeQ[exp, par]]] /@ (Join[#[[1]]& /@ cmplxparams, cmplxvars]);
      AppendTo[realchecklist, FreeQ[#, Complex]&];
      (* A function is constructed from this list checking if an expression should be considered as real. *)
      isreal = Function[{exp}, And @@ (Function[{checker}, checker[exp]] /@ realchecklist)];
      (* As some of the couplings are lists, we have to be smart and render to arrays where necessary. *)
      rendercpl = If[Head[#[[2]]] === List, ToString[#[[1]]] <> "(" <> ToString[Length[#[[2]]]] <> ")",
         ToString[#[[1]]]]&;
      (* In the hope that this allows better compiler optimization, the couplings are divided into real and *
       * complex ones. But as FORTRAN isn't capable of automatically casting complex to double, this is too *
       * smart, so I remove it again...                                                                     *)
      pardefs = pardefs <>
         WO`FortranSplit["\ncomplex(kind=double), public :: " <> WO`Concat[rendercpl /@ WO`cpldeflist, ", "]];
   ];

   DoTranslators[] := Module[{pdglist, tospin, tomass, toname, MakeSelect, lr},

         (* Render a select case ... end select structure *)
         MakeSelect[l_, d_] := Module[{},
         "select case (pdg)\n" <> WO`Indent[WO`Concat[
            ("case(" <> ToString[#[[1]]] <> ");   res = " <> ToString[#[[2]]])& /@ l, "\n"], 3]
         <> "\n   case default;   " <> d <> "\nend select\n"];

         translators = ""
            <> "function pdg_to_mass (pdg) result (res)\n"
            <> "integer, intent(in) :: pdg\n"
            <> "real(kind=double) :: res\n"
            <> WO`Indent[MakeSelect[{WO`hash["pdg", #], WO`hash["mass", #]}&
               /@ Select[WO`taglist, WO`hash["mass", #] =!= ZERO&], "res = 0._double"], 3]
            <> "end function pdg_to_mass\n"
            <> "\n"
            <> "function pdg_to_ident (pdg) result (res)\n"
            <> "integer, intent(in) :: pdg\n"
            <> "character(len=256) :: res\n"
            <> WO`Indent[MakeSelect[{WO`hash["pdg", #], "\"" <> ToString[#] <> "\""}& /@ WO`taglist,
               "call panic (\"FATAL: out of range in pdg_to_mass\")"], 3]
            <> "end function pdg_to_ident\n"
            <> "\n"
            <> "function pdg_to_spin (pdg) result(res)\n"
            <> "integer, intent(in) :: pdg\n"
            <> "integer :: res\n"
            <> WO`Indent[MakeSelect[{WO`hash["pdg", #],
                  Switch[lr = WO`hash["lrep", #], "S", "0", "f"|"F"|"M", "1", "V", "2", _,
                     Print["WARNING: oplotter can't deal with lrep " <> lr <> " , will treat as scalar..."];
                  "0"]}& /@ WO`taglist, "call panic (\"FATAL: out of range in pdg_to_spin\")"], 3]
            <> "end function pdg_to_spin\n";
   ];

   DoCouplings[] := Module[{i, RenderCoupling, ProcessCoupling, pardefs},

      (* Renders a name / expression pair to a string *)
      RenderCoupling[name_, cpl_] :=
         WO`FortranSplit[name <> " = " <> ToString[FortranForm[cpl]]];

      (* Renders a coupling, taking care of the possibility of an array *)
      ProcessCoupling[cpl_] :=
         If[Head[cpl[[2]]] === List,
            WO`Concat[
               RenderCoupling[ToString[cpl[[1]]] <> "(" <> ToString[#[[1]]] <> ")", #[[2]]]& /@
                  MapThread[{#1, #2}&, {Range[1, Length[cpl[[2]]]], cpl[[2]]}]
               , "\n"],
            RenderCoupling[ToString[cpl[[1]]], cpl[[2]]]
         ];
      
      (* Internal parameter definitions. *)
      pardefs = {#[[1]], #[[2]]}& /@ WO`IParamList;
      initparams = "subroutine init_parameters\n";
      (* The imperative approach grantees the correct order of execution. *)
      For[i = 1, i <= Length[pardefs], i++,
         initparams = initparams <> WO`Indent[ProcessCoupling[pardefs[[i]]], 3] <> "\n"];
      (* The order of the couplings is irrelevant, so we use the functional approach... :) *)
      initparams = initparams <> WO`Indent[WO`Concat[ProcessCoupling /@ WO`cpldeflist, "\n"], 3] <>
         "\nend subroutine init_parameters\n";
      initparams = StringReplace[initparams, RegularExpression["(\\d+\\.\\d*)"] -> "$1_double"];
   ];

   header = ""
      <> WO`CommentMaker[WO`fileheader, "! ", "! ", ""] <> "\n"
      <> "\n"
      <> "module opl_" <> WO`oplname <> "\n"
      <> "implicit none\n"
      <> "save\n"
      <> "private\n"
      <> "\n"
      <> "public :: parameters, init_parameters, pdg_to_mass, pdg_to_spin, pdg_to_ident\n"
      <> "\n"
      <> "integer, parameter :: double=selected_real_kind (precision(1.) + 1, range(1.) + 1)\n"
      <> "real(kind=double), parameter, public :: fr_zero=0._double, one=1._double\n";
   footer = ""
      <> "end module opl_" <> WO`oplname <> "\n";
   contains = ""
      <> "contains\n"
      <> "\n"
      <> "subroutine panic (msg)\n"
      <> "character(len=*), intent(in) :: msg\n"
      <> "   print *, msg\n"
      <> "   call exit (1)\n"
      <> "end subroutine panic\n";
   DoParams[];
   DoTranslators[];
   DoCouplings[];
   contents = header <> "\n" <> namelist <> "\n" <> pardefs <> "\n" <> contains <> "\n"
      <> initparams <> "\n" <> translators <> "\n" <> footer;
   handle = OpenWrite [file];
   WriteString[handle, contents];
   Close[handle];
];

WO`WriteOplMdl[mdlfile_, grbfile_] := Module[{handle, contents},
   contents = ""
      <> WO`CommentMaker[WO`fileheader, "# ", "# ", ""] <> "\n\n"
      <>"models=\"$models " <> WO`oplname <> "\"\n"
      <> "\n"
      <> WO`oplname <> "_tag=\"" <> WO`omeganame <> "\"\n"
      <> "\n"
      <> WO`oplname <> "_name=\""
         <> StringReplace[M$ModelName, {"\"" -> "_", "'" -> "_"}] <> " (Mathematica interface)\"\n"
      <> "\n"
      <> WO`oplname <> "_mod=\"opl_" <> WO`oplname <> "\"\n"
      <> "\n"
      <> WO`oplname <> "_settings=\"OMEGA_DIR     : directory containing the omega95 library and includes; mandadatory\"\n"
      <> WO`oplname <> "_setup() {\n"
      <> "\tLDFLAGS=\"$LDFLAGS -L$OMEGA_DIR -lomega95\"\n"
      <> "\tsrcfiles=\"$models_dir/opl_" <> WO`oplname <> ".f90 $srcfiles\"\n"
      <> "\tobjfiles=\"opl_" <> WO`oplname <> ".o $objfiles\"\n"
      <> "\tF95FLAGS=\"$F95FLAGS -I$OMEGA_DIR\"\n"
      <> "\tif grep -e \"use omega_parameters\" $amp; then\n"
      <> "\t\techo\n"
      <> "\t\techo \"patching $amp to include the correct modules...\"\n"
      <> "\t\trun \"sed -i -e 's/use omega_parameters/use opl_" <> WO`oplname <> "/' $amp\"\n"
      <> "\tfi\n"
      <> "}\n";
   handle = OpenWrite[mdlfile];
   WriteString[handle, contents];
   Close[handle];
   handle = OpenWrite[grbfile];
   WriteString[handle, WO`CommentMaker[WO`fileheader, "# ", "# ", ""] <> "\n\n" <>
      "opl_" <> WO`oplname <> ".mod opl_" <> WO`oplname <> ".o\n"];
   Close[handle];
];


(* Write the WHIZARD model file. *)
WO`WriteWhizMdl[file_] := Module[{handle, content, header, params, DoParams, reprules={}, delimit,
      RegisterCvar, parts, vertices, DoParticles, DoVertices},

   (* A delimiter for prettyprinting. *)
   delimit = StringJoin @@ Table["#", {i, 1, 70}] <> "\n";

   (* Register a complex veriable in the replacement rules list (for 1.9x), noop for 2.0. *)
   RegisterCvar[var_] := If[WO`whizv19x[], AppendTo[reprules, RuleDelayed[
      RegularExpression["(\\W|^)" <> ToString[var] <> "(\\W|$)"],
      "$1(" <> ToString[var] <> "_r + " <> ToString[var] <> "_i * (0., 1.))$2"]]];

   (* Digest the parameters. Complex quantities are split into real and imaginary part as WHIZARD only *
    * handles real parameters.                                                                         *)
   DoParams[] = Module[{epars, eparsc, ipars, i, tmp, cpars, cdecrules, formatNumber},
      (* Format a number *)
      formatNumber = ToString[FortranForm[SetPrecision[N[#], 10]]]&;
      (* External parameters from the list. *)
      epars = {#[[2, 1]], N[#[[2, -3]]], #[[2, -2]], #[[2, -1]]}& /@ Flatten[#[[2]]& /@ WO`EParamList, 1];
      (* Check whether we have to append alphas *)
      If[WO`whizv2x[],
         If[FreeQ[ToUpperCase[ToString[#]]& /@ WOSymbolsList, "ALPHAS"],
            WO`appendAlphas  = ! FreeQ[#[[1]]& /@ epars, aS]
         ,
            WO`appendAlphas = False;
            Print[""
               <> "WARNING: A symbol called \"alphas\" has already been defined. "
               <> "As WHIZARD expect this to contain the value of \\alpha_s at the Z "
               <> "pole, running the strong coupling will not work as expected."
            ];
         ]
      ];

      (* Append the numerical masses and widths. *)
      AppendTo[epars,
         {#[[2]], N[#[[3]]], False,
            If[FreeQ[tmp = WO`hash["revpdg", #[[1, 1]]], WO`hash], ToString[tmp] <> " mass", "unknown mass, unphysical?"]}]& /@
         Select[WO`masslist, NumericQ[#[[3]]]&];
      AppendTo[epars,
         {#[[2]], N[#[[3]]], False,
            If[FreeQ[tmp = WO`hash["revpdg", #[[1, 1]]], WO`hash], ToString[tmp] <> " width", "unknown width, unphysical?"]}]& /@
         Select[WO`widthlist, NumericQ[#[[3]]]&];
      (* Remove duplicates. *)
      epars = WO`WeedOutList[epars, FreeQ[#2, #1[[1]]]&];
      (* Generate code and glue together. *)
      params = ""
         <> If[WO`whizv2x[], "# NEVER CHANGE THE ORDER OF THE PARAMETERS!!!\n\n", ""]
         <> delimit
         <> "# External Parameters\n"
         <> delimit
         <> "\n";
      (* For >= 2.0, the order must be consistent between the model file and the glue code -> use a loop. *)
      For[i = 1, i <= Length[epars], i++,
         If [epars[[i, 3]],
            params = params
               <> WO`ExtendString[WO`ExtendString["parameter " <> ToString[epars[[i, 1]]] <> "_r", 20]
                  <> If[WO`whizv2x[], " = ", " "] <> formatNumber[Re[epars[[i, 2]]]], 35]
               <> " # " <> WO`StringSplit[
                  epars[[i, 4]] <> " (real part)", 40, (StringJoin @@ Table[" ", {n, 1, 36}]) <> "# ", ""]
               <> "\n"
               <> WO`ExtendString[WO`ExtendString["parameter " <> ToString[epars[[i, 1]]] <> "_i", 20]
                  <> If[WO`whizv2x[], " = ", " "] <> formatNumber[Im[epars[[i, 2]]]], 35]
               <> " # " <> WO`StringSplit[
                  epars[[i, 4]] <> " (imaginary part)", 40, (StringJoin @@ Table[" ", {n, 1, 36}]) <> "# ", ""]
               <> "\n\n"
         ,
            params = params
               <> WO`ExtendString[WO`ExtendString["parameter " <> ToString[epars[[i, 1]]], 20]
                  <> If[WO`whizv2x[], " = ", " "] <> formatNumber[epars[[i, 2]]], 35]
               <> " # " <> WO`StringSplit[
                  epars[[i, 4]] <> If[WO`whizv2x[] && Not[FreeQ[{aS}, epars[[i, 1]]]],
                     "\n(will be reset if \\alpha_S is evolved)", ""]
                  , 40, (StringJoin @@ Table[" ", {n, 1, 36}]) <> "# ", ""]
               <> "\n\n";
         ]
      ];
      (* Generate replacement rules for the complex parameters. *)
      eparsc = Select[epars, #[[3]]&];
      RegisterCvar[#[[1]]]& /@ eparsc;
      (* The following hacks are specific to WHIZARD v1.9x *)
      If[WO`whizv19x[],
         (* Hack: WHIZARDs perl scripts won't accept the assignment of a number to a derived parameter, so *
          * rewrite this case as 1. * x                                                                    *)
         AppendTo[reprules, RegularExpression["^\\s*(-?\\d+.?\\d*)\\s*$"] :> "1. * ($1)"];
         (* Another Hack: aimag expects a complex argument, but most intrinsic functions are defined as *
          * real. This hack forces FORTRAN to typecast.                                                 *)
         AppendTo[reprules, RegularExpression["AIMAG\\s*\\(\\s*-"] :> "aimag((-1., 0.) * "];
         AppendTo[reprules, RegularExpression["AIMAG\\s*\\("] :> "aimag((1., 0.) * "];
      ];
      params = params
         <> delimit
         <> "# Internal Parameters\n"
         <> delimit
         <> "\n";
      (* Digest the internal parameter. We use a loop to gurantee the correct order of execution. *)
      ipars = {#[[1]], #[[2]], #[[-2]], #[[-1]]}& /@ WO`IParamList;
      (* All complex parameters *)
      cpars = Join[#[[1]]& /@ eparsc, #[[1]]& /@ Select[ipars, #[[3]]&]];
      cdecrules = Flatten[({Rule[#, WO`real[#] + WO`I * WO`imag[#]], Rule[Conjugate[#], WO`real[#] - WO`I* WO`imag[#]]}&
         /@ cpars), 1] /. Rule -> RuleDelayed;
      For[i = 1, i <= Length[ipars], i++,
         If[WO`whizv2x[],
            If[ipars[[i, 3]],
               (* Preliminary handling of complex derived parameters in W2. Only works with polynomials. *)
               Catch[Module[{eepr, clist, ipart, rpart},
                  eepr = Expand[ipars[[i, 2]] /. Join[cdecrules, {Complex[r_, i_] :> r + i * WO`I}]];
                  clist = CoefficientList[eepr, WO`I];
                  clist = Table[{i - 1, clist[[i]]}, {i, 1, Length[clist]}];
                  rpart = Plus @@ (If[EvenQ[#[[1]]], I^#[[1]] * #[[2]], 0]& /@ clist);
                  ipart = Plus @@ (If[OddQ[#[[1]]], I^(#[[1]] - 1) * #[[2]], 0]& /@ clist);
                  If[Not[FreeQ[{rpart, ipart}, WO`I]],
                     Print[""
                        <> "Unable to extract real and imaginary parts from derived complex parameter "
                        <> "\"" <> ToString[ipars[[i, 1]]] <> "\". The interface currently support a "
                        <> "polynomial dependence on complex quantities. If you are sure that your "
                        <> "derived parameters meet this requirement, it is time to file a BUG report..."
                     ];
                     Throw[Null];
                  ];
                  params = params <> StringReplace[
                     WO`StringSplit["# " <> ipars[[i, 4]] <>
                        If[Not[FreeQ[WO`RunParameters, ipars[[i, 1]]]],
                           "\n(will be reset if \\alpha_S is evolved)", ""]
                        , 68, "# ", ""] <> "\n"
                     <> WO`ExtendString["derived " <> ToString[ipars[[i, 1]]] <> "_r", 15] <> " = "
                        <> WO`SindarinSplit[WO`SindarinForm[rpart], 55, 18] <> "\n"
                     <> WO`ExtendString["derived " <> ToString[ipars[[i, 1]]] <> "_i", 15] <> " = "
                        <> WO`SindarinSplit[WO`SindarinForm[ipart], 55, 18] <> "\n\n",
                     {RegularExpression["WOxreal\\((.*?)\\)"] :> "$1_r",
                        RegularExpression["WOximag\\((.*?)\\)"] :> "$1_i"}];
               ]];
            ,
               If[Not[And @@ (FreeQ[ipars[[i, 2]], #]& /@ cpars)],
                  Print[""
                     <> "WARNING: the real derived parameter \"" <> ToString[ipars[[i, 1]]] <> "\" depends "
                     <> "on a complex quantity. This is currently not supported with WHIZARD 2 and will be "
                     <> "skipped - prepare for broken output..."
                  ]
               ,
                  params = params
                     <> WO`StringSplit["# " <> ipars[[i, 4]] <>
                        If[Not[FreeQ[WO`RunParameters, ipars[[i, 1]]]],
                           "\n(will be reset if \\alpha_S is evolved)", ""]
                        , 68, "# ", ""] <> "\n"
                     <> WO`ExtendString["derived " <> ToString[ipars[[i, 1]]], 15] <> " = "
                     <> WO`SindarinSplit[WO`SindarinForm[ipars[[i, 2]]], 55, 18] <> "\n\n"
                  ];
               ];
         ,
            If[ipars[[i, 3]],
               params = params <> WO`StringSplit["# " <> ipars[[i, 4]], 68, "# ", ""] <> "\n"
                  <> WO`ExtendString["derived " <> ToString[ipars[[i, 1]]] <> "_r", 15] <> " " <> WO`FortranSplit[
                     "\"" <> WO`StringReplaceAll[ToString[FortranForm[Re[ipars[[i, 2]]]]], reprules] <> "\"",
                     55, 17] <> "\n"
                  <> WO`ExtendString["derived " <> ToString[ipars[[i, 1]]] <> "_i", 15] <> " " <> WO`FortranSplit[
                     "\" " <> WO`StringReplaceAll[ToString[FortranForm[Im[ipars[[i, 2]]]]], reprules] <> "\"",
                     55, 17] <> "\n\n";
                  RegisterCvar[ipars[[i, 1]]];
               ,
               params = params <> WO`StringSplit["# " <> ipars[[i, 4]], 68, "# ", ""] <> "\n"
                  <> WO`ExtendString["derived " <> ToString[ipars[[i, 1]]], 15] <> " " <> WO`FortranSplit
                     ["\"" <> WO`StringReplaceAll[ToString[FortranForm[ipars[[i, 2]]]], reprules] <> "\"",
                     55, 17] <> "\n\n";
            ];
         ];
      ];
      (* Append alphas if requested *)
      If[WO`appendAlphas,
         params = params
            <> "# Strong coupling at the Z pole (required by WHIZARD)\n"
            <> "derived alphas = aS\n\n"
      ];
      (* W2: We need this to ensure the correct ordering in the glue interface. *)
      WO`paramlist = {#[[1]], #[[3]]}& /@ Join[epars, ipars];
   ];

   (* Build the particle list. *)
   DoParticles[] = Module[{plist, AppendParticle, WhizPname, AppendClass},

      (* Construct a sane name. *)
      WhizPname[s_] := Module[{i, c},
         c = "wp_" <> ToLowerCase[WO`SanitizeString[s]];
         i = 1;
         If[WO`hash["wpns", c] === True,
            While[WO`hash["wpns", c <> "_" <> ToString[i]] === True, i++];
            c = c <> "_" <> ToString[i];
         ];
         WO`hash["wpns", c] = True;
         c
       ];

      (* Build the definition block for one particle. TODO: the "parton" property is derived from color, *
       * singlets are no partons, all other particles are partons. This is hackish, but I see no other   *
       * way to do this.                                                                                 *)
      AppendParticle[{name_, aname_, spin_, prop_, msym_, wsym_, crep_, plabel_, pdg_, descr_, tex_, atex_, gs_,echarge_}] := Module[
         {thename, thetex, theaname, theatex},
         (* Make sure that we record the particle with positive PDG. *)
         If[pdg > 0,
            thename = name; thetex = tex; theaname = aname; theatex = atex
         , 
            thename = aname; thetex = atex; theaname = name; theatex = tex
         ];
         If[MatchQ[spin, U], Return[Null]];
         If[WO`hash["goldstone", ToString[thename]] && WO`gauge === WO`WOUnitarity, Return[Null]];
         WO`hash["whizname", name] = WO`hash["whizname", aname] = WhizPname[ToString[thename]];
         parts = parts <> "# particle: " <> ToString[descr] <> "\n"
            <> "particle " <> WO`ExtendString[WO`hash["whizname", thename], 10] <> " "
            <> WO`ExtendString[ToString[Abs[pdg]], 8] <> If[MatchQ[crep, T|O], " parton", ""] <> "\n"
           <> WO`Indent[""
               <> "spin " <> WO`ExtendString[Switch[spin, S, "0", F, "1/2", V, "1", T, "2"], 3]
               <> " charge " <>ToString[InputForm[echarge]]<>" "
            <> Switch[crep, T, " color 3", O, " color 8", _, ""] <> "\n"
               <> If[WO`whizv2x[], ""
                  <> "name \"" <> ToString[thename] <> "\""
                  <> "\ntex_name \"" <> ToString[thetex] <> "\"\n"
                  <> If[name =!= aname, ""
                     <> "anti \"" <> ToString[theaname] <> "\""
                     <> "\ntex_anti \"" <> ToString[theatex] <> "\"\n", ""]
               , ""
                  <> "name " <> WO`ExtendString["\"" <> ToString[thename] <> "\"", 7]
                  <> If[thetex != thename, " tex:\"" <> ToString[thetex] <> "\"", ""] <> "\n"
                  <> If[name =!= aname, ""
                     <> "anti omega:" <> WO`ExtendString["\"" <> ToString[theaname] <> "\"", 7]
                     <> If[theatex != theaname, " tex:\"" <> ToString[atex] <> "\"", ""] <> "\n",
                  ""]
               ]
            <> If[msym =!= ZERO, "mass " <> ToString[WO`hash["mass", thename]] <> "\n", ""]
            <> If[wsym =!= ZERO, "width " <> ToString[wsym] <> "\n", ""]
            , 3] <> "\n";
      ];

      (* Adds a comment with the class name and then appends all particles in this class. *)
      AppendClass[{{cdef_, cname_}, partlist_}] := Module[{},
         (* Filter out ghosts and unphysical particles. *)
         If[Select[partlist, (#[[3]] =!= U)&] == {}, Return[Null]];
         parts = parts <> "# class: " <> ToString[cname] <> "\n" <> delimit <> "\n";
         AppendParticle /@ partlist;
      ];

      parts = ""
         <> delimit
         <> "# Particle definitions\n"
         <> delimit
         <> "\n";
      AppendClass /@ WO`PartList;
      If[WO`whizv2x[],
         parts = parts <> ""
            <> "# Hadrons\n"
            <> delimit
            <> "\n"
            <> "particle PROTON 2212\n"
            <> "  spin 1/2  charge 1\n"
            <> "  name p \"p+\"\n"
            <> "  anti pbar \"p-\"\n"
            <> "\n"
            <> "particle HADRON_REMNANT 90\n"
	    <> "  name hr\n"
	    <> "  tex_name \"had_r\"\n"
            <> "\n"
            <> "particle HADRON_REMNANT_SINGLET 91\n"
	    <> "  name hr1\n"
	    <> "  tex_name \"had_r^{(1)}\"\n"
            <> "\n"
            <> "particle HADRON_REMNANT_TRIPLET 92\n"
	    <> "  name hr3\n"
	    <> "  tex_name \"had_r^{(3)}\"\n"
            <> "  color 3\n"
            <> "\n"
            <> "particle HADRON_REMNANT_OCTET 93\n"
	    <> "  name hr8\n"
	    <> "  tex_name \"had_r^{(8)}\"\n"
            <> "  color 8"
            <> "\n\n";
      ];
   ];

   (* Construct the vertex list. *)
   DoVertices[] := Module[{nmasses, reprules, massrules, verts, Weight, i},

      (* Get the numeric mass of particle(s) . *)
      Weight[plist_List] := Plus @@ (Weight /@ plist);
      Weight[part_] := Module[{m},
         If[NumericQ[m = (part /. massrules)], m,
            If[WO`hash["mass", part] =!= ZERO,
               Print["WARNING: mass of particle " <> ToString[part] <> " could not be detemined, assuming 0..."]];
            0
         ]
      ];

      (* Convert Parameters and numeric masses / widths into a replacement list. *)
      reprules = WO`WeedOutList[Join[{},
         Rule[#[[1]], #[[-3]]]& /@
            (#[[2]]& /@ Flatten[#[[2]]& /@ WO`EParamList, 1]),
         RuleDelayed[#[[1]], Evaluate[#[[2]]]]& /@ WO`IParamList,
         Rule[#[[2]], #[[3]]]& /@ Select[Join[WO`masslist, WO`widthlist], NumericQ[#[[3]]]&]
      ]];
      (* Try to calculate all masses by applying the replacement list. *)
      massrules =
         Select[{WO`hash["revpdg", #[[1, 1]]], #[[2]]}& /@ Simplify[WO`masslist //. reprules],
            NumericQ[#[[2]]]&];
      (* Make sure that the conjugated particles are also part of the game. *)
      massrules = Flatten[{#, {WO`hash["conj", ToString[#[[1]]]], #[[2]]}}& /@ massrules, 1];
      (* Weed out duplicates and create replacement list. *)
      massrules = WO`WeedOutList[(Rule @@ #)& /@ massrules];
      (* Get the vertex list, take care to replace CC with HC *)
      verts = Select[(PartName[#[[1]] /. CC -> WO`HC]& /@ #[[1]])& /@ WO`vlist, (Length[#] == 3)&];
      (* Drop all particles not known to WHIZARD. *)
      verts = Select[verts, (And @@ ((Head[WO`hash["whizname", #]] === String)& /@ #))&];
      (* Sort the list w.r.t. the total mass at the vertex. *)
      verts = Sort[verts, (Weight[#1] < Weight[#2])&];
      (* Write out the list; the For[] loop ensures the correct ordering. *)
      vertices = ""
         <> delimit
         <> "# vertex list (arity 3 only)\n"
         <> delimit
         <> "\n";
      For[i=1, i <= Length[verts], i++,
         vertices = vertices <> "vertex " <>
            WO`Concat[
               If[WO`whizv2x[], ("\"" <> # <> "\"")& /@ verts[[i]], verts[[i]]],
            " "] <> "\n"
      ];
   ];

   header = WO`CommentMaker[WO`fileheader, "# ", "# ", ""] <> "\n\n";
   DoParams[];
   DoParticles[];
   DoVertices[];
   content = header <> "\n" <> 
      If[WO`whizv2x[],
         "model \"" <> WO`whizname <> "\"\n\n"
      ,
         ""
      ]
      <> params <> parts <> vertices;
   handle = OpenWrite[file];
   WriteString[handle, content];
   Close[handle];
];

WO`WriteWhizGlue[filestem_] := Module[{handle, content, pubdefs, header, footer, DoPubdefs, DoImport,
   global, local, master, couplings={}, i, VOpen, kind, preamble, namepre, import, run, DoRun,
   RenderCoupling},

   (* Verbose open a file. *)
   VOpen[file_] := (Print["Writing \"" <> file <> "\"..."]; OpenWrite[file]);

   (* These depends on the WHIZARD version. *)
   kind = If[WO`whizv2x[], "default", "omega_prec"];
   preamble = If[WO`whizv2x[],
      "use kinds\nuse constants, only: pi\n",
      "use omega_kinds !NODEP!\nuse omega_constants !NODEP!\nuse parameters\n"];
   namepre = If[WO`whizv2x[], "parameters_" <> WO`whizname, "omega_parameters_whizard"];

   (* Create the public definitions. *)
   DoPubdefs[] := Module[{reals, cmplxs},
      (* Reals are widths and masses. *)
      reals = WO`WeedOutList[ToString[#[[2]]]& /@ Join[WO`masslist, WO`widthlist]];
      (* If we are in Rxi gauge, we need to add the Rxi symbol. *)
      If[WO`gauge === WO`WORxi, AppendTo[reals, ToString[WO`gsym]]];
      (* Couplings are complex. *)
      cmplxs = (ToString[#[[1]]] <> If[Head[#[[2]]] === List, "(" <> ToString[Length[#[[2]]]] <> ")", ""])&
         /@ WO`cpldeflist;
      pubdefs = "real(kind="<> kind <> "), parameter :: fr_zero=0." <> If[WO`whizv19x[], ", one=1.\n", "\n"] <>
         If[reals != {}, ""
            <> "! masses and widths:\n"
            <> WO`FortranSplit["real(kind=" <> kind <> ") :: " <> WO`Concat[reals, ", "]] <> "\n",
            ""
         ] <> If[cmplxs != {}, ""
            <> "! vertex factors:\n"
            <> WO`FortranSplit["complex(kind=" <> kind <> ") :: " <> WO`Concat[cmplxs, ", "]] <> "\n",
            ""
         ];
   ];

   (* Render a coupling. *)
   RenderCoupling[{cpl_, defs_List}] := StringJoin @@
      (RenderCoupling[{cpl <> "(" <> ToString[#] <> ")", defs[[#]]}]& /@ Range[Length[defs]]);
   RenderCoupling[{cpl_, def_}] :=
      WO`FortranSplit[cpl <> " = " <>
         StringReplace[ToString[FortranForm[def]], RegularExpression["(\\d+\\.\\d*)"] :> ("$1_" <> kind)]
      ] <> "\n";

   DoImport[] := Module[{mwlist, pars, realdefs, reals, cmplxs, cpltmp={}},

      (* All external and internal parameters. *)
      pars = Join[
         {#[[2, 1]], #[[2, -2]]}& /@ Flatten[#[[2]]& /@ WO`EParamList, 1],
         {#[[1]], #[[-2]]}& /@ WO`IParamList
      ];
      (* All masses and widths. *)
      mwlist = #[[2]]& /@ Join[WO`masslist, WO`widthlist];
      (* We have to salvage all real parameters from the WHIZARD par structure (1.9x) resp. the array (2x). *)
      reals = WO`WeedOutList[Join[
         ToString[#[[1]]]& /@ Select[pars, Not[#[[2]]]&], ToString /@ mwlist]];
      (* Only the parameters which haven't already been declared globally (which excludes masses and widths) *
       * have to be declared locally.                                                                        *)
      realdefs = WO`WeedOutList[ToString[#[[1]]]& /@ Select[pars, (Not[#[[2]]] && FreeQ[mwlist, #[[1]]])&]];
      (* If we are using Rxi gauge, we must filter out the Rxi symbol, which is already declared globally. *)
      If[WO`gauge === WO`WORxi,
         realdefs = Select[realdefs, (# != ToString[WO`gsym])&];
      ];
      (* All complex parameters have to be salvaged from WHIZARD, recombining real and imaginary parts. *)
      cmplxs = WO`WeedOutList[ToString[#[[1]]]& /@ Select[pars, #[[2]]&]];
      (* Build the expressions for calculating the couplings, splitting this properly in several blocks.*)
      For[i = 1, i <= Length[WO`cpldeflist], i++,
         AppendTo[cpltmp, RenderCoupling[WO`cpldeflist[[i]]]];
         If[Length[cpltmp] == WO`MaxCouplingsPerFile,
            AppendTo[couplings, cpltmp];
            cpltmp = {};
         ];
      ];
      If[Length[cpltmp] != 0, AppendTo[couplings, cpltmp]];
      (* Build the local declaration module. *)
      local = ""
         <> WO`CommentMaker[WO`fileheader, "! ", "! ", ""] <> "\n"
         <> "\n"
         <> "module " <> namepre <> "_local\n"
         <> "use " <> namepre <> "_global\n"
         <> "implicit none\n"
         <> "public\n"
         <> "save\n"
         <> "\n"
         <> If[reals != {},
            WO`FortranSplit["real(kind=" <> kind <> ") :: " <> WO`Concat[realdefs, ", "]] <> "\n",
            ""]
         <> If[cmplxs != {},
            WO`FortranSplit["complex(kind=" <> kind <> ") :: " <> WO`Concat[cmplxs, ", "]] <> "\n",
            ""]
         <> "\n"
         <> "end module " <> namepre <> "_local";
      (* Build the import function. *)
      import = ""
         <> If[WO`whizv23[], "subroutine import_from_whizard (par, scheme)\n", "subroutine import_from_whizard (par)\n"]
         <> "use " <> namepre <> "_local\n"
         <> WO`Concat[Table["use " <> namepre <> "_cpl" <> ToString[i], {i, 1, Length[couplings]}], "\n"] <> "\n"
         <> If[WO`whizv19x[], ""
            <> "type(parameter_set), intent(in) :: par\n"
            <> "\n"
            <> WO`Indent[""
               <> StringJoin @@ ((# <> " = par%" <> # <> "\n")& /@ reals)
               <> StringJoin @@
                  ((# <> " = par%" <> # <> "_r + par%" <> # <> "_i * (0._omega_prec, 1._omega_prec)\n")& /@ cmplxs), 3]
         , ""
            <> "real(kind=default), dimension("
               <> ToString[Plus @@ (If[#[[2]], 2, 1]& /@ WO`paramlist) + If[WO`appendAlphas, 1, 0]]
               <> "), intent(in) :: par\n" <> If[WO`whizv23[], "integer, intent(in) :: scheme\n",""]
            <> WO`Indent[Module[{val, j, par},
               val = "";
               j = 1;
               For[i = 1, i <= Length[WO`paramlist], i++,
                  par = ToString[WO`paramlist[[i, 1]]];
                  val = val <>
                     If[WO`paramlist[[i, 2]],
                        j = j + 2;
                        par <> " = par(" <> ToString[j - 2] <> ") + par(" <> ToString[j - 1] <> ") * (0._default, 1._default)\n"
                     ,
                        j++;
                        par <> " = par(" <> ToString[j - 1] <> ")\n"
                     ]
                  ];
               val
            ], 3]
         ]
         <> WO`Indent[WO`Concat[
            Table["call calc_cpl" <> ToString[i], {i, 1 Length[couplings]}], "\n"] <> "\n", 3]
         <> "end subroutine import_from_whizard\n";
   ];

   DoRun[] := Module[{epars, ipars, redefext, redefint, calcpl},
      epars = #[[2, 1]]& /@ Flatten[#[[2]]& /@ WO`EParamList, 1];
      ipars = {#[[1]], #[[2]]}& /@ WO`IParamList;
      redefext = StringJoin @@ Append[Switch[#
         , aS, "   aS = alpha_s\n"
         , _, ""]& /@ epars, ""];
      redefint = StringJoin @@ Append[If[Not[FreeQ[WO`RunParameters, #[[1]]]]
         , WO`FortranSplit["   " <> ToString[#[[1]]] <> " = " <> StringReplace[
            ToString[FortranForm[#[[2]]]] <> "\n", RegularExpression["(\\d+\\.\\d*)"] :> ("$1_" <> kind)], 67, 3]
         , ""]& /@ ipars, ""];
      calcpl = If[Length[WO`runningcouplings] > 0
         , WO`Indent[StringJoin @@ (RenderCoupling[WO`cpldeflist[[WO`runningcouplings[[#]]]]]& /@
            Range[Length[WO`runningcouplings]]), 3]
         , ""
      ];
      run = ""
         <> "subroutine model_update_alpha_s (alpha_s)\n"
         <> "use " <> namepre <> "_local\n"
         <> "real(kind=default), intent(in) :: alpha_s\n"
         <> ""
         <> redefext
         <> redefint
         <> calcpl
         <> "end subroutine model_update_alpha_s\n"
   ];

   DoPubdefs[];
   DoImport[];
   DoRun[];
   global = ""
      <> WO`CommentMaker[WO`fileheader, "! ", "! ", ""] <> "\n"
      <> "\n"
      <> "module " <> namepre <> "_global\n"
      <> preamble
      <> "implicit none\n"
      <> "public\n"
      <> "private :: sec_re, sec_cmplx, csc_re, csc_cmplx\n"
      <> "save\n"
      <> "\n"
      <>   pubdefs
      <> "\n"
      <> "interface sec\n"
      <> "   module procedure sec_re\n"
      <> "   module procedure sec_cmplx\n"
      <> "end interface sec\n"
      <> "\n"
      <> "interface csc\n"
      <> "   module procedure csc_re\n"
      <> "   module procedure csc_cmplx\n"
      <> "end interface csc\n"
      <> "\n"
      <> "contains\n"
      <> "\n"
      <> "function sec_re (x) result (y)\n"
      <> "real(kind=" <> kind <> "), intent(in) :: x\n"
      <> "real(kind=" <> kind <> ") :: y\n"
      <> "   y = 1._" <> kind <> " / cos (x)\n"
      <> "end function sec_re\n"
      <> "\n"
      <> "function sec_cmplx (x) result (y)\n"
      <> "complex(kind=" <> kind <> "), intent(in) :: x\n"
      <> "complex(kind=" <> kind <> ") :: y\n"
      <> "   y = 1._" <> kind <> " / cos (x)\n"
      <> "end function sec_cmplx\n"
      <> "\n"
      <> "function csc_re (x) result (y)\n"
      <> "real(kind=" <> kind <> "), intent(in) :: x\n"
      <> "real(kind=" <> kind <> ") :: y\n"
      <> "   y = 1._" <> kind <> " / cos (x)\n"
      <> "end function csc_re\n"
      <> "\n"
      <> "function csc_cmplx (x) result (y)\n"
      <> "complex(kind=" <> kind <> "), intent(in) :: x\n"
      <> "complex(kind=" <> kind <> ") :: y\n"
      <> "   y = 1._" <> kind <> " / cos (x)\n"
      <> "end function csc_cmplx\n"
      <> "\n"
      <> "end module " <> namepre <> "_global";
   master = ""
      <> WO`CommentMaker[WO`fileheader, "! ", "! ", ""] <> "\n"
      <> "\n"
      <> "module " <> namepre <> "\n"
      <> "use " <> namepre <> "_global\n"
      <> "implicit none\n"
      <> "public\n"
      <> "private :: sec, csc\n"
      <> "save\n"
      <> "\n"
      <> "contains\n"
      <> "\n"
      <> import
      <> "\n"
      <> If[WO`whizv2x[], run <> "\n", ""]
      <> "end module " <> namepre;
   handle = VOpen[filestem <> ".global.f90"];
   WriteString[handle, global];
   Close[handle];
   (* Make sure no old coupling modules are lying around. *)
   (
      Print["Deleting " <> # <> " ..."];
      If [DeleteFile[#] === $Failed, Throw["ERROR: unable to delete " <> #, WO`EAbort]];
   )& /@ Select[FileNames[filestem <> "*f90"], StringMatchQ[#,
      RegularExpression[StringReplace[filestem, "." -> "\\."] <> "\\.cpl\\d+\\.f90"]]&];

   For[i=1, i <= Length[couplings], i++,
      handle = VOpen[filestem <> ".cpl" <> ToString[i] <> ".f90"];
      WriteString[handle, ""
         <> WO`CommentMaker[WO`fileheader, "! ", "! ", ""] <> "\n"
         <> "\n"
         <> "module " <> namepre <> "_cpl" <> ToString[i] <> "\n"
         <> "use " <> namepre <> "_global\n"
         <> "use " <> namepre <> "_local\n"
         <> "implicit none\n"
         <> "\n"
         <> "contains\n"
         <> "\n"
         <> "subroutine calc_cpl" <> ToString[i] <> "\n"
         <> WO`Indent[StringJoin @@ couplings[[i]], 3]
         <> "end subroutine calc_cpl" <> ToString[i] <> "\n"
         <> "\n"
         <> "end module " <> namepre <> "_cpl" <> ToString[i]
      ];
      Close[handle];
   ];
   handle = VOpen[filestem <> ".local.f90"];
   WriteString[handle, local];
   Close[handle];
   handle = VOpen[filestem <> ".f90"];
   WriteString[handle, master];
   Close[handle];
];


(* ********************************************************************
   The functions below are for im- and export of external parameters.
   ******************************************************************** *)

WO`WriteExtParams[file_String, options___] := Module[{parlist, outfile, omode, content,
      epars, handle, unknownopts, modelname},
   (* Check for unknown options. *)
   If[Length[unknownopts =
         Select[#[[1]]& /@ {options}, FreeQ[#[[1]]& /@ Options[WO`WriteExtParams], #]&]] > 0,
      Print["ERROR: WO`WriteExtParams: unknown option(s) " <> WO`Concat[ToString /@ unknownopts, " , "]];
      Return[Null];
   ];

   (* Setup environment *)
   WO`GlobalSetup[];
   WO`EParamList = WO`WOEParamList /. {options} /. Options[WO`WriteExtParams];
   WO`masslist = WO`WOMassList /. {options} /. Options[WO`WriteExtParams];
   WO`widthlist = WO`WOWidthList /. {options} /. Options[WO`WriteExtParams];
   WO`whizv = WO`WOWhizardVersion /. {options} /. Options[WO`WriteExtParams];
   modelname = WO`WOModelName /. {options} /. Options[WO`WriteExtParams];
   Catch[WO`whizvn[], WO`EAbort, (Print["ERROR: invalid WHIZARD version"]; Return[])&];

   (* Determine the output file. *)
   outfile = If[file == "",
      WO`SanitizeString[modelname] <> If[WO`whizv2x[], ".sin", ".in"], file];
   outfile = If[StringMatchQ[file, RegularExpression["^\\s*" <> WO`fileSlashRE <> ".*"]], outfile,
      Directory[] <> WO`fileSlash <> outfile];
   Print[""
      <> "Writing external parameters to file\n"
      <> "   \"" <> outfile <> "\"\n"
      <> "in " <> If[WO`whizv2x[], "WHIZARD 2.x sindarin", "WHIZARD 1.9x namelist"] <> " format..."
   ];

   (* Prepare the parameter list. *)
   epars = {#[[2, 1]], #[[2, -3]], #[[2, -2]]}& /@ Flatten[#[[2]] & /@ WO`EParamList, 1];
   parlist = WO`WeedOutList[Join[
      ({ToString[#[[1]]], #[[2]]}& /@ Select[epars, (! #[[3]])&]),
      Flatten[{
         {ToString[#[[1]]] <> "_r", N[Re[#[[2]]]]}, {ToString[#[[1]]] <> "_i", N[Im[#[[2]]]]}
      }& /@ Select[epars, #[[3]]&], 1],
      Select[{ToString[#[[2]]], #[[3]]} & /@ Join[WO`masslist, WO`widthlist], NumericQ[#[[2]]]&]
   ], FreeQ[#2, #1[[1]]]&];

   (* Create the contents of the parameter file *)
   content = If[WO`whizv2x[], "", "&parameter_input\n"];
   (
      content = content <> If[WO`whizv2x[], "", "   "] <> #[[1]] <> " = " <> ToString[#[[2]]] <> "\n"
   )& /@ parlist;
   content = content <> If[WO`whizv2x[], "", "/\n"];

   (* Write out. *)
   handle = OpenWrite[outfile];
   WriteString[handle, content];
   Close[handle];
   (* Finito. *)
   Print["... done!"];
];
WO`WriteExtParams[options___] := WO`WriteExtParams["", options];
