(* ::Package:: *)

(* ::Title:: *)
(* FeynRules - PyR@TE Interface*)


(* ::Text:: *)
(*Author : Lohan Sartore*)
(*Version : 1.0  --  March 2024 *)


(* ::Section::Closed:: *)
(*Load / configure the interface*)


(* ::Subsection::Closed:: *)
(*Initialisation*)


PR$PyratePath = "";
PR$PythonExecutable = "";
PR$PyLiePath := FileNameJoin[{PR$PyratePath, "src", "PyLie"}];

PR$Path = FileNameJoin[{DirectoryName[$InputFileName], "PyRATE"}];
PR$Configured = False;


(* ::Subsection::Closed:: *)
(*Config file*)


Options[UpdateConfigFile] = {
	Enabled -> None,
	PyRATEpath -> None,
	PythonExecutable -> None
};

UpdateConfigFile[OptionsPattern[]] := Block[{fpath, config, enabled, pyratePath, pythonExec},
	fpath = FileNameJoin[{PR$Path, "config.json"}];
	{enabled, pyratePath, pythonExec} = {OptionValue@Enabled, OptionValue@PyRATEpath, OptionValue@PythonExecutable};
	
	If[!FileExistsQ[fpath],
		config = Association["Enabled" -> False, "PyRATEpath" -> "", "PythonExecutable" -> ""];
		Export[fpath, config];
	];
	
	If[{enabled, pyratePath, pythonExec} === {None, None, None},
		Return[];
	];
	
	config = Association[Import[fpath]];
	
	If[enabled =!= None,
		config[["Enabled"]] = enabled;
	];
	If[pyratePath =!= None,
		config[["PyRATEpath"]] = pyratePath;
	];
	If[pythonExec =!= None,
		config[["PythonExecutable"]] = pythonExec;
	];
	
	Export[fpath, config];
	LoadConfigFile[];
];


GetConfigSetting[setting_] := Block[{fpath, config},
	If[!MemberQ[{"Enabled", "PyRATEpath", "PythonExecutable"}, setting],
		Print["Error: GetConfigSetting[]'s argument can only be '\"Enabled\"', \"PyRATEpath\" or \"PythonExecutable\"."];
		Abort[];
	];
	
	fpath = FileNameJoin[{PR$Path, "config.json"}];
	
	If[!FileExistsQ[fpath],
		UpdateConfigFile[];
	];
	
	config = Import[fpath];
	
	Return[Association[config][[setting]]];
]


LoadConfigFile[] := (
	{PR$PyratePath, PR$PythonExecutable} = GetConfigSetting /@ {"PyRATEpath", "PythonExecutable"};
);


(* ::Subsection::Closed:: *)
(*Enable / disable the interface*)


PyRATEInterfaceEnabledQ[] := GetConfigSetting["Enabled"] === True;

EnablePyRATEInterface[] := Block[{},
	UpdateConfigFile[Enabled -> True];
	
	LoadPyRATEInterface[];
];

DisablePyRATEInterface[] := (UpdateConfigFile[Enabled -> False]; Print[Style["PyR@TE Interface was disabled.", Darker@Orange]]);


(* ::Subsection::Closed:: *)
(*Load the interface*)


Options[LoadPyRATEInterface] = {Quiet -> False, Detail -> False};

LoadPyRATEInterface[OptionsPattern[]] := Block[{pyrateConfigured, pythonConfigured, pythonValidated, configNeeded = False},
	If[!PyRATEInterfaceEnabledQ[],
		Return[];
	];
	
	If[!OptionValue[Quiet] && !OptionValue[Detail],
		Print[Style["Loading PyR@TE Interface...", Darker@Orange]];
	];
	
	(* Read the config file *)
	LoadConfigFile[];
	
	pyrateConfigured = ValidPyRATEpathQ[PR$PyratePath];
	pythonConfigured = ValidPythonExecutableQ[PR$PythonExecutable];
	
	If[!pyrateConfigured,
		configNeeded = True;
		
		If[!OptionValue[Quiet],
			Print[Style["\t* No valid path to PyR@TE 3's main folder is currently provided.", Red]];
			
			Print[Style["\t  * Option 1:", Darker@Orange, Bold], " you may call the function ", Style["ConfigurePyRATEInterface[]", Bold],
					" with the option ", Style["PyRATE->Automatic", Bold], ". The bash command"];
			Print["\t\t              'git clone https://github.com/LSartore/pyrate.git $dirpath/$dirname'"];
			Print["\t              will be automatically run, where $dirpath = $FeynrulesPath/Interfaces/PyRATE and $dirname = \"PyR@TE_3\" by default."];
			Print["\t              If needed, $dirpath and $dirname can be provided using the options ",
					Style["GitCloneDirectory", Bold], " and ", Style["GitCloneDirectoryName", Bold], ", respectively."];
			
			Print[Style["\t  * Option 2:", Darker@Orange, Bold], " you may manually download PyR@TE 3 and provide its path calling the function"];
			Print["\t\t          ", Style["ConfigurePyRATEInterface[PyRATE->", Bold], "[path to PyR@TE 3's main directory]", Style["]", Bold], "."];
		];
	];
	
	If[!pythonConfigured,
		configNeeded = True;
		
		If[!pyrateConfigured && !OptionValue[Quiet],
			Print[""];
		];
		
		If[!OptionValue[Quiet],
			Print[Style["\t* No valid python executable is currently provided.", Red]];
			
			Print[Style["\t  * Option 1:", Darker@Orange, Bold], " you may call the function ", Style["ConfigurePyRATEInterface[]", Bold],
					" with the option ", Style["Python->Automatic", Bold], ". It will be checked whether"];
			Print["\t              either 'python' or 'python3' corresponds to a valid python executable on your machine."];
			
			Print[Style["\t  * Option 2:", Darker@Orange, Bold], " you may manually specify the absolute path to a python 3 executable using"];
			Print["\t\t          ", Style["ConfigurePyRATEInterface[Python->", Bold], "[path to a valid python executable]", Style["]", Bold], "."];
		];
	];
	
	If[configNeeded,
		Return[];
	];
	
	If[!OptionValue[Quiet],
		Print[Style["Done.", Darker@Orange]];
	];
	
	PR$Configured = True;
	Return[True];
];


(* ::Subsection::Closed:: *)
(*Configure the interface*)


Options[ConfigurePyRATEInterface] = {
	PyRATE -> None, Python -> None,
	GitCloneDirectory -> Automatic, GitCloneDirectoryName -> "PyR@TE_3", ForceGitClone -> False
};

ConfigurePyRATEInterface[OptionsPattern[]] := Block[{pyrate, python, gcpath, gcfolder, cloned, pythonDep},
	If[!PyRATEInterfaceEnabledQ[],
		Print["Please enable the Feynrules-PyR@TE interface by running ", Style["EnablePyRATEInterface[]", Bold], " before configuring it."];
		Return[];
	];
	
	{pyrate, python, gcpath, gcfolder} = {OptionValue@PyRATE, OptionValue@Python, OptionValue@GitCloneDirectory, OptionValue@GitCloneDirectoryName};

	If[pyrate =!= None,
		If[pyrate === Automatic,
			(* Automatic git cloning *)
			
			If[gcpath === Automatic,
				gcpath = PR$Path;
			];
			
			If[!StringQ[gcpath] || !FileExistsQ[gcpath] || !DirectoryQ[gcpath],
				Print[Style["Error: the directory path provided through the GitCloneDirectory option appears to be invalid (" <> gcpath <> ")", Red]];
				Return[];
			];
		
			If[!StringQ[gcfolder],
				Print[Style["Error: the directory name provided through the GitCloneDirectoryName option must be a string.", Red]];
				Return[];
			];
			
			pyrate = AutoGitClone[gcpath, gcfolder, ForceGitClone -> OptionValue[ForceGitClone]];
			
			If[pyrate === False,
				Return[];
			];
			
			UpdateConfigFile[PyRATEpath -> pyrate];
		,
			If[!ValidPyRATEpathQ[pyrate],
				Print["Error: the path provided through the PyRATE option should point to a valid PyR@TE 3 directory."];
				Return[];
			];
			
			UpdateConfigFile[PyRATEpath -> pyrate];
		];
	];
	
	If[python =!= None,
		If[!ValidPyRATEpathQ[pyrate],
			Print[Style["Warning: skipping python executable configuration. A valid path to PyR@TE 3 must first be provided.", Orange]];
			Return[];
		];
		
		If[python === Automatic,
			(* Automatic python executable testing *)
			
			python = AutoPythonConfig[];
			
			If[python === False,
				Return[];
			];
			
			UpdateConfigFile[PythonExecutable -> python];
		,
			If[!ValidPythonExecutableQ[python],
				Print[Style["Error: the string provided through the Python option doesn't correspond to a valid python executable.", Red]];
				Return[];
			,
				pythonDep = CheckPythonDependencies[python];
				
				If[pythonDep =!= True,
					Print[Style["Error: the string provided through the Python option does correspond to a valid python executable", Red]];
					Print[Style["but fails to meet PyR@TE 3's dependency requirements:", Red]];
					Print[Style[pythonDep, FontFamily->"Courier"]];
					
					Return[];
				];
				
				UpdateConfigFile[PythonExecutable -> python];
			];
		];
	];
	
	If[LoadPyRATEInterface[Quiet -> True] === True,
		Print[Style["The PyR@TE interface is properly configured and loaded.", Darker@Green]];
	];
];


PyRATEInterfaceConfiguredQ[] := PyRATEInterfaceEnabledQ[] && ( LoadPyRATEInterface[Quiet -> True] === True );


CheckInterfaceConfiguration[] := If[PyRATEInterfaceConfiguredQ[], Return[True], Print[Style["Error: the PyR@TE interface is not properly configured.", Bold, Red]]; LoadPyRATEInterface[Quiet -> False, Detail -> True]; Abort[];];


(* ::Subsection::Closed:: *)
(*PyR@TE path config*)


ValidPyRATEpathQ[path_] := StringQ[path] && path =!= "" && FileExistsQ[path] && DirectoryQ[path] && FileExistsQ[FileNameJoin[{path, "pyR@TE.py"}]];


Options[AutoGitClone] = {ForceGitClone -> False};

AutoGitClone[path_, name_, OptionsPattern[]] := Block[{dir, autogit, gitcmd, procReturn},
	dir = FileNameJoin[{path, name}];
	
	If[FileExistsQ[dir] && DirectoryQ[dir],
		If[!OptionValue[ForceGitClone],
			If[ValidPyRATEpathQ[dir],
				Return[dir];
			];
			
			Print[Style["Error while git-cloning PyR@TE 3:", Red], " the provided directory already exists (", dir, ")."];
			Print["You may run ", Style["ConfigureInterface[ForceGitClone->True]", Bold], " to overwrite its content."];
			Return[False];
		,
			DeleteDirectory[dir, DeleteContents->True];
		];
	];
	
	gitcmd = {"git", "clone", "https://github.com/LSartore/pyrate.git", dir};
	
	Print[Style["Cloning PyR@TE 3 from \n\thttps://github.com/LSartore/pyrate.git\nto\n\t" <> dir <> "...", Darker@Orange]];
	
	procReturn = RunProcess[gitcmd];
	
	If[procReturn === $Failed,
		Print[Style["Error: unable to automatically clone PyR@TE's git repository.", Red]];
		Return[False];
	];
	
	If[procReturn[["ExitCode"]] =!= 0,
		Print[Style["Error: unable to automatically clone PyR@TE's git repository.", Red]];
		Print[Style["The command ", Red]];
		Print["\t", Style[StringRiffle[gitcmd], Red, Bold]];
		Print[Style["has generated the following error message:", Red]];
		Print[Style[procReturn[["StandardError"]], FontFamily->"Courier", Red]];
		
		Return[False];
	];
	
	Print[Style["Done.", Darker@Orange]];
	Return[dir];
];


(* ::Subsection::Closed:: *)
(*Python executable config*)


Options[ValidPythonExecutableQ] = {SkipDependencyCheck -> False};

ValidPythonExecutableQ[pexec_, OptionsPattern[]] := Block[{out},
	If[pexec === "" || !StringQ[pexec],
		Return[False];
	];
	
	out = Quiet @ Check[RunProcess[{pexec, "-V"}], False];
	
	Return[out =!= False && out[["ExitCode"]] == 0];
]


AutoPythonConfig[] := Block[{pyexecList = {"python", "python3"}, validexecList, validDependencies, summary, validExecPos},
	validexecList = ValidPythonExecutableQ /@ pyexecList;
	validDependencies = Table[If[validexecList[[i]] === False, False, CheckPythonDependencies[pyexecList[[i]]]], {i, Length@pyexecList}];
	
	summary = Transpose[{pyexecList, validexecList, validDependencies}];

	validExecPos = FirstPosition[summary, {_, True, True}, 0];
	
	If[validExecPos == 0,
		Print[Style["Error: Unable to automatically find a valid python executable.", Red]];
		Do[
			If[s[[2]] === True,
				Print["\t* ", Style[s[[1]], Bold], " is a valid python executable but has missing dependencies:"];
				Print[Style[s[[3]], FontFamily->"Courier"]];
			,
				Print["\t* ", Style[s[[1]], Bold], " is not a valid python executable."];
			];
		, {s, summary}];
		
		Return[False];
	];
	
	Return[pyexecList[[validExecPos[[1]]]]];
];


CheckPythonDependencies[exec_] := Block[{dependencyScriptPath, out, logPath, mess},
	dependencyScriptPath = FileNameJoin[{PR$PyratePath, "src", "IO", "Dependencies.py"}];
	
	out = RunProcess[{exec, dependencyScriptPath}];
	
	If[out[["ExitCode"]] === 0,
		Return[True];
	];

	logPath = FileNameJoin[{PR$PyratePath, "log", "dependencies.log"}];
	mess = Import[logPath, "Text"];
	DeleteFile[logPath];
	
	Return[mess];
]


(* ::Subsection::Closed:: *)
(*Actually loading the interface*)


LoadPyRATEInterface[]


(* ::Section::Closed:: *)
(*Interacting with PyLie and PyR@TE*)


(* ::Subsection::Closed:: *)
(*Interacting with the DB*)


(* ::Subsubsection::Closed:: *)
(*Open/read*)


DB[el_:Nothing] := Block[{DBcontent, dbFile, missingDBfile = False},
	dbFile = FileNameJoin[{PR$PyLiePath, "altPyLieDB.hd5f.gz"}];
	If[PR$DBcontent === Unevaluated[PR$DBcontent], 
		If[FileExistsQ[dbFile],
			PR$DBcontent = Import[dbFile]
		,
			missingDBfile = True
		]; 
	];
	
	If[missingDBfile || (el =!= Nothing && !MemberQ[PR$DBcontent, el]),
		Return[None];
	];
	
	Return @ Import[dbFile, el]
]


(* ::Subsubsection::Closed:: *)
(*Compute missing element*)


DBCompute[gp_, request_, arg_:None, kwargs_:None] := Block[{formatedArg, formatedKWargs, command, exitCode},
	command = PR$PythonExecutable <> " " <> FileNameJoin[{PR$PyLiePath, "dbRequest.py"}];
	command = command <> " " <> PR$PyLiePath;
	
	command = command <> " " <> ToString[gp];
	command = command <> " " <> ToString[request];
	
	If[arg =!= None,
		formatedArg = StringReplace[ToString[arg], {"{"->"[","}"->"]"}];
		command = command <> " " <> formatedArg;
		If[kwargs =!= None,
			formatedKWargs = ("\\'" <> ToString[#[[1]]] <> "\\':" <>
							  If[StringQ[#[[2]]],
								  "\\'" <> ToString[#[[2]]] <> "\\'",
								  ToString[#[[2]]]
							  ])& /@ kwargs;
			formatedKWargs = "{" <> StringReplace[StringRiffle[formatedKWargs, ","], {"{"->"[", "}"->"]"}] <> "}";
			command = command <> " " <> formatedKWargs;
		];
	];
	
	exitCode = Run[command];
	Clear @ PR$DBcontent;
	
	If[exitCode != 0,
		Print["Error while trying to update the DB : "];
		If[exitCode/256 == 1,
			Print["dbRequest.py called with too few arguments"];
		];
		If[exitCode/256 == 2,
			Print["Unable to parse the arguments of dbRequest.py"];
		];
		If[exitCode/256 == 3,
			Print["Unable to load the DB. Perhaps is it still busy?"];
		];
		If[exitCode/256 == 4,
			Print["The DB raised an error while answering the request"];
		];
	];
	Return @ exitCode;
]


(* ::Subsection::Closed:: *)
(*Auxiliary functions*)


(* ::Subsubsection::Closed:: *)
(*PyRep*)


PyRep[rep_, modelFile_:False] := Block[{pyRep},
	If[!modelFile,
		pyRep = StringReplace[ToString[rep], {"{"->"(", "}" -> ")", " "->""}];
		If[Length[rep] == 1,
			pyRep = StringReplace[pyRep, ")"->",)"]
		];
	,
		pyRep = StringReplace[ToString[rep, InputForm], {"{"->"[", "}" -> "]"}];
	];
	Return @ pyRep;
];	


(* ::Subsubsection::Closed:: *)
(*ReadValue*)


ReadValue[b_] := Block[{val},
	val = StringReplace[b, {"s("~~Shortest[n__]~~")" :> "Sqrt["<> n <> "]", "**"->"^"}];
	Return[ToExpression[val]];
]


(* ::Subsubsection::Closed:: *)
(*BuildInvariant*)


BuildInvariant[s_, k_, v_, ordering_, tensorForm_:False] := Block[{ret, a,b,c,d, chars},
	If[tensorForm,
		ret = Table[SparseArray[{}, s[[2;;]]], s[[1]]];
		Do[
			ret[[k[[count, 1]]+1, Sequence @@ (k[[count, 2;;]]+1)]] = ReadValue[v[[count]]];
		, {count, Length[k]}];
		Return[ret];
	];
	ret = Table[0, s[[1]]];
	chars = ToExpression[FromCharacterCode[96+#]]& /@ ordering;
	
	Do[
		ret[[k[[count, 1]]+1]] += ReadValue[v[[count]]] * Times @@ (Function[{p}, chars[[p]][k[[count,p+1]]+1]] /@ Range[Length[k[[count]]]-1]);
	, {count, Length[k]}];
	Return[ret];
]


(* ::Subsubsection::Closed:: *)
(*BuildMatrix*)


BuildMatrix[s_, k_, v_] := Block[{ret},
	ret = Table[SparseArray[{}, s[[2;;]]], s[[1]]];
	(ret[[k[[#, 1]]+1, Sequence @@ (k[[#, 2;;]]+1)]] = ReadValue[v[[#]]])& /@ Range[Length[k]];
	Return[ret];
]


(* ::Subsubsection::Closed:: *)
(*GoToRealBasis*)


GoToRealBasis[grp_, rep_, realBasis_] := Block[{FS, trueRep, adj},
	trueRep = If[rep[[-1]] === True || rep[[-1]] === False, rep[[;;-2]], rep];
	FS = GetFrobenius[grp, trueRep];
	adj = GetAdjointRep[grp];
	
	If[FS == 0 && (realBasis === "all" || (realBasis === "adjoint" && trueRep === adj)),
		Return[True];
	];
	Return[False];
]


(* ::Subsection::Closed:: *)
(*Retrieve info from the DB*)


(* ::Subsubsection::Closed:: *)
(*AdjointRep, Rank, Dimension*)


GetBasicInfo[grp_, info_] := Block[{key, val},
	key = "/" <> ToString[grp] <> "/" <> info;
	
	val = DB[key];
	If[val =!= None,
		Return[val];
	];
	PrintTemporary[Style["[PyLie]", Bold], " Computing missing basic info : ", info, "..."];
	If[DBCompute[grp, info] == 0,
		Return @ GetBasicInfo[grp, info];
	];
	Return[];
]


GetAdjointRep[grp_] := GetBasicInfo[grp, "adjointrep"];
GetRank[grp_] := GetBasicInfo[grp, "rank"];
GetDimension[grp_] := GetBasicInfo[grp, "dimension"];


(* ::Subsubsection::Closed:: *)
(*Conjugate representation*)


GetConjugateRep[grp_, rep_, realBasis_:False] := Block[{trueRep, ret},
	If[ToString[grp] == "U1", Return[-1*rep]];
	If[rep === PR$defaultNA, Return[rep]];
	If[rep === 1, Return[rep]];
	If[rep[[-1]] === True, Return[Append[rep[[;;-2]], False]]];
	
	trueRep = If[rep[[-1]] === False, rep[[;;-2]], rep];
	ret = GetConjugateRepAux[grp, trueRep];
	
	If[ret[[-1]] === True && GoToRealBasis[grp, ret[[;;-2]], realBasis],
		ret = ret[[;;-2]];
	];
	
	Return[ret]	
]


GetConjugateRepAux[grp_, rep_] := Block[{pyRep, key, val, ret},
	If[ToString[grp] == "U1", Return[-1*rep]];
	If[rep === PR$defaultNA, Return[rep]];
	If[rep === 1, Return[rep]];
	If[rep[[-1]] === True, Return[Append[rep[[;;-2]], False]]];
	If[rep[[-1]] === False, Return[Append[rep[[;;-2]], True]]];
	
	pyRep = PyRep[rep];
	key = "/" <> ToString[grp] <> "/conjugaterep/" <> pyRep;
	
	val = DB[key];
	If[val =!= None,
		If[val =!= rep,
			Return[val]];
		
		Return[Append[val, True]];
	];
	
	PrintTemporary[Style["[PyLie]", Bold], " Computing missing conjugate irrep ..."];
	If[DBCompute[grp, "conjugaterep", rep] == 0,
		ret = GetConjugateRepAux[grp, rep];
		If[ret =!= rep,
			Return[ret]];
		Return[Append[ret, True]];
	];
	Return[];
]


(* ::Subsubsection::Closed:: *)
(*Dynkin labels*)


GetDynkinLabels[grp_, repDim_, realBasis_:False] := Block[{key, val},
	If[Head @ repDim === List, Return[repDim]];
	key = "/" <> ToString[grp] <> "/dynkinlabels/" <> ToString[repDim];
	
	val = DB[key];
	If[val =!= None,
		If[val[[-1]] === True && GoToRealBasis[grp, val, realBasis], val = val[[;;-2]]];
		Return[Replace[val, -1 -> True, -1]];
	];
	
	PrintTemporary[Style["[PyLie]", Bold], " Computing missing Dykin label ..."];
	If[DBCompute[grp, "dynkinlabels", repDim] == 0,
		Return @ GetDynkinLabels[grp, repDim, realBasis];
	];
	Return[];
]


(* ::Subsubsection::Closed:: *)
(*DimRep*)


GetDimR[grp_, rep_] := Block[{pyRep, key, val},
	If[rep[[-1]] === True || rep[[-1]] === False, Return[GetDimR[grp, rep[[;;-2]]]]];
	pyRep = PyRep[rep];
	key = "/" <> ToString[grp] <> "/dimr/" <> pyRep;
	
	val = DB[key];
	If[val =!= None,
		Return[val];
	];
	
	PrintTemporary[Style["[PyLie]", Bold], " Computing missing rep dimension ..."];
	If[DBCompute[grp, "dimr", rep] == 0,
		Return @ GetDimR[grp, rep];
	];
	Return[];
]


(* ::Subsubsection::Closed:: *)
(*Frobenius-Schur indicator*)


GetFrobenius[grp_, rep_] := Block[{pyRep, key, val},
	pyRep = PyRep[rep];
	key = "/" <> ToString[grp] <> "/frobenius/" <> pyRep;
	
	val = DB[key];
	If[val =!= None,
		Return[val];
	];
	
	PrintTemporary[Style["[PyLie]", Bold], " Computing missing frobenius ..."];
	If[DBCompute[grp, "frobenius", rep] == 0,
		Return @ GetFrobenius[grp, rep];
	];
	Return[];
]


(* ::Subsubsection::Closed:: *)
(*Invariants*)


GetInvariant[grp_, reps_, conjs_, realBasis_:False, pyNorm_:True] := Block[{pyReps, storeReps, storeConjs, pyConjs, key, pyKey, val, RBsequence, ordering, pyNormStr, shape, keys, vals},
	If[PR$DBStoreInvariants === Unevaluated[PR$DBStoreInvariants], PR$DBStoreInvariants = {}];
	
	pyReps = {}; (*PyRep /@ reps;*)
	storeConjs = {};
	pyConjs = {};
	RBsequence = {};
	
	(* Construct the key-string to read from the DB *)
	Do[
		If[GoToRealBasis[grp, reps[[i]], realBasis] === True,
		(* This is a real rep which is rotated to real basis *)
			RBsequence = Append[RBsequence, 1];
			pyConjs = Append[pyConjs, 0];
			storeConjs = Append[storeConjs, False];
			pyReps = Append[pyReps, reps[[i]]];
			Continue[];
		];
		If[GetFrobenius[grp, reps[[i]]] == 1 && conjs[[i]] === True,
		(* This is a conjugated pseudo-real rep *)
			RBsequence = Append[RBsequence, 0];
			pyConjs = Append[pyConjs, 0];
			storeConjs = Append[storeConjs, False];
			pyReps = Append[pyReps, GetConjugateRep[grp, reps[[i]], realBasis]];
			Continue[];
		];
		RBsequence = Append[RBsequence, 0];
		pyConjs = Append[pyConjs, Boole @ conjs[[i]]];
		storeConjs = Append[storeConjs, conjs[[i]]];
		pyReps = Append[pyReps, reps[[i]]];
	, {i, Length[reps]}];
	
	If[DeleteDuplicates[storeConjs] === {True},
		storeConjs = Not /@ storeConjs;
		pyConjs = If[#==1, 0, 1]& /@ pyConjs;
	];
	
	storeReps = pyReps;
	
	(* Re-order the reps *)
	ordering = Ordering[{pyReps[[#]], pyConjs[[#]], RBsequence[[#]]}& /@ Range[Length[pyReps]]];
	pyReps = pyReps[[#]]& /@ ordering;
	pyConjs = pyConjs[[#]]& /@ ordering;
	RBsequence = RBsequence[[#]]& /@ ordering;
	
	pyReps = "(" <> StringRiffle[PyRep /@ pyReps, ","] <> ")";
	pyConjs = StringRiffle[ToString /@ pyConjs, ""];
	pyNormStr = ToString @ Boole @ pyNorm;
	RBsequence = StringRiffle[ToString /@ RBsequence, ""];
	
	key = pyReps <> ";" <> pyConjs <> ";" <> pyNormStr <> ";" <> RBsequence;
	
	(* Check if invariant has been computed already *)
	If[MemberQ[PR$DBStoreInvariants[[All, 1]], key],(*
		Print["\t Reading stored invariant : ", key];
		Print[".. result should be ", BuildInvariant[pyKey <> "/ks", pyKey <> "/k", vals]*)
		{shape, keys, vals} = PR$DBStoreInvariants[[All, 2]][[Position[PR$DBStoreInvariants[[All,1]], key][[1,1]]]];
		Return[BuildInvariant[shape, keys, vals, ordering]];
	];
	
	pyKey = "/" <> ToString[grp] <> "/invariants/" <> key;
	
	shape = DB[pyKey <> "/s"];
	If[shape =!= None,
		If[shape === {},
			Return[shape];
		];
		keys = DB[pyKey <> "/k"];
		vals = DB[pyKey <> "/v"];
		
		PR$DBStoreInvariants = Append[PR$DBStoreInvariants, {key, {shape, keys, vals}}];
		
		Return[BuildInvariant[shape, keys, vals, ordering]];
	];
	
	PrintTemporary[Style["[PyLie]", Bold], " Computing missing invariant ..."];
	If[DBCompute[grp, "invariants", storeReps, {"conj"->storeConjs, "realBasis"->realBasis, "pyrateNormalization"->pyNorm}] == 0,
		Return @ GetInvariant[grp, storeReps, storeConjs, realBasis, pyNorm];
	];
	Return[-1];
]


(* ::Subsubsection::Closed:: *)
(*Representation matrices*)


GetRepMat[grp_, rep_, realBasis_:False, posInGlobalList_:False] := Block[{trueRep, conj, RB, pyRep, key, s,k,v, pos, ret},
	If[PR$DBStoreRepMats === Unevaluated[PR$DBStoreRepMats], PR$DBStoreRepMats = {}];
	
	conj = 0;
	trueRep = rep;
	If[rep[[-1]] === True || rep[[-1]] === False,
		conj = Boole @ rep[[-1]]; 
		trueRep = rep[[;;-2]];
	];
	
	RB = 0;
	If[GoToRealBasis[grp, rep, realBasis],
	(* This is a real rep which is rotated to real basis *)
		RB = 1;
		conj = 0;
	];
	
	pyRep = PyRep[trueRep];
	key = "/" <> ToString[grp] <> "/repmatrices/" <> pyRep <> ";" <> ToString@conj <> ";" <> ToString@RB;
	
	If[MemberQ[PR$DBStoreRepMats[[All, 1]], key],(*
		Print["\t Reading stored invariant : ", key];
		Print[".. result should be ", BuildInvariant[pyKey <> "/ks", pyKey <> "/k", vals]*)
		pos = Position[PR$DBStoreRepMats[[All,1]], key][[1,1]];
		
		If[!posInGlobalList,
			ret = PR$DBStoreRepMats[[All, 2]][[pos]];
			Return[ret]];
		Return[pos];
	];
	
	s = DB[key <> "/s"];
	
	If[s =!= None,
		k = DB[key <> "/k"];
		v = DB[key <> "/v"];
		
		ret = BuildMatrix[s, k, v];
		PR$DBStoreRepMats = Append[PR$DBStoreRepMats, {key, ret}];
		If[!posInGlobalList,
			Return[ret]];
		Return[Length[PR$DBStoreRepMats]];
	];
	
	PrintTemporary[Style["[PyLie]", Bold], " Computing missing rep matrices ..."];
	If[DBCompute[grp, "repmatrices", rep, {"realBasis"->realBasis}] == 0,
		Return @ GetRepMat[grp, rep, realBasis, posInGlobalList];
	];
	Return[];
]


(* ::Subsubsection::Closed:: *)
(*Structure constants*)


GetStructureConstants[grp_, posInGlobalList_:False] := Block[{key, ret, pos, s,k,v},
	If[PR$DBStoreRepMats === Unevaluated[PR$DBStoreRepMats], PR$DBStoreRepMats = {}];
	
	key = "/" <> ToString[grp] <> "/structureconstants";
	
	If[MemberQ[PR$DBStoreRepMats[[All, 1]], key],
		pos = Position[PR$DBStoreRepMats[[All,1]], key][[1,1]];
		
		If[!posInGlobalList,
			ret = PR$DBStoreRepMats[[All, 2]][[pos]];
			Return[ret]];
		Return[pos];
	];
	
	s = DB[key <> "/s"];
	If[s =!= None,
		k = DB[key <> "/k"];
		v = DB[key <> "/v"];
		
		ret = BuildMatrix[s, k, v];
		PR$DBStoreRepMats = Append[PR$DBStoreRepMats, {key, ret}];
		If[!posInGlobalList,
			Return[ret]];
		Return[Length[PR$DBStoreRepMats]];
	];
	
	PrintTemporary[Style["[PyLie]", Bold], " Computing missing structure constants ..."];
	If[DBCompute[grp, "structureconstants"] == 0,
		Return @ GetStructureConstants[grp, posInGlobalList];
	];
	Return[];
];


(* ::Subsection:: *)
(*-----------------------------------*)


(* ::Subsection::Closed:: *)
(*Running PyR@TE*)


ReadLogFiles[] := Block[{logPath},
	logPath = FileNameJoin[{PR$PyratePath, "log"}];
	If[!FileExistsQ[logPath], CreateDirectory[logPath]];
	Return[FileNames[All, logPath]];
];


ReadOutput[] := Block[{newLogFiles, log, content},
	newLogFiles = ReadLogFiles[];
	log = If[!MemberQ[PR$logFiles, #], #, Nothing]& /@ newLogFiles;
	If[Length[log] =!= 1, Return[""]];
	Return@StringReplace[Import[log[[1]], "String"], "[Log] "->""];
];


(* ::Section::Closed:: *)
(*Useful functions*)


(* ::Subsection::Closed:: *)
(*Indices*)


(* ::Subsubsection::Closed:: *)
(*GetGenIndexPos*)


GetGenIndexPos[p_] := Block[{particleIndices = Flatten[GetRule[p, Indices] /. (Index[index_] :> index)], nonGaugeIndices},
							nonGaugeIndices = Select[particleIndices, (!GaugeGroupIndexQ[#])&];

	If[Length[nonGaugeIndices] == 1,
		Return[Flatten[Position[particleIndices, nonGaugeIndices[[1]]]][[1]] + If[p[[1,0]]===F, 1, 0]],
		If[Length[nonGaugeIndices]== 0,
			Return[0],
			Print["Error : Unable to determine the generation index of particle '", GetRule[p, ClassName], "'"]
		]
	]
]


(* ::Subsubsection::Closed:: *)
(*GetIndexName / GetIndicesNames*)


GetIndexName[p_, ipos_] := GetIndicesNames[p][[ipos]]
GetIndicesNames[p_] := p /. (_Symbol[iii___] | _Symbol) :> {iii}


(* ::Subsubsection::Closed:: *)
(*GetIndicesTypes*)


(* ::Text:: *)
(*Determines the type of all indices carried by the object p.*)


GetIndicesTypes[p_] := Block[{pName, pos, object}, 
	pName = (If[Head[#] =!= Symbol, Head[#], #]& @ (p /. Conjugate[c_] | HC[c_] :> c));
	If[StringContainsQ[ToString[pName], "bar"], pName = GetConjugate[pName]];
	
	(* Try and find p in list of particles, then list of parameters *)
	object = Flatten[Join[PR$Fermions, PR$Scalars][[Flatten[Position[Name /. Join[PR$Fermions, PR$Scalars], pName]]]]];
	If[object === {}, object = Flatten[PR$Parameters[[All,2]][[Flatten[Position[PR$Parameters[[All, 1]], pName]]]]]];
	
	If[object =!= {}, Return[If[# =!= Indices, #, {}]& @ ( (Indices /. object) /. Index[i_] :> i)]];
	
	(* If not found, then look at group representation matrices *)
	pos = Flatten[Position[Representations /. PR$GaugeGroups, pName]];
	If[pos === {}, Return[{}]];

	If[pos[[2]] =!= Length[Representations /. PR$GaugeGroups[[pos[[1]]]]], 
		Return[{#[[-1]], #[[pos[[2]]]], #[[pos[[2]]]]}& @ (Indices /. PR$GaugeGroups[[pos[[1]]]])];
	, 
		Return[Table[#[[-1]], 3]& @ (Indices /. PR$GaugeGroups[[pos[[1]]]])]
	];
];


(* ::Subsubsection::Closed:: *)
(*GetIndexRange*)


(* ::Text:: *)
(*Returns the index range*)


GetIndexRange[index_] := Block[{iRange},
	iRange = IndexRange[If[Head[index] =!= Index, Index[index], index]] /. Unfold | NoUnfold -> Identity;
	If[MatchQ[iRange, IndexRange[Index[_]]],
		Print["Error : unable to determine the range of index '", index, "'."];
		Abort[];
	];
	Return[Length@iRange];
]


(* ::Subsubsection::Closed:: *)
(*GetRepresentation*)


(* ::Text:: *)
(*Returns the representation of the group in which the index lives, by looking at PyrateRep[index]. If undefined, returns the index range.*)


Options[GetRepresentation] = {DisplayedWarning->{}};
GetRepresentation[index_, OptionsPattern[]] := Block[{pyRep = PyrateRep[index], iRange},
	If[MatchQ[pyRep, PyrateRep[_]],
		iRange = GetIndexRange[index];
		If[!MemberQ[OptionValue[DisplayedWarning], index],
			Print["  Warning : index " <> ToString[index] <> " has no PyrateRep. Setting its representation to ", iRange, "."];
			SetOptions[GetRepresentation, DisplayedWarning->Append[OptionValue[DisplayedWarning], index]]
		];
		Return[iRange];
	];
	Return[pyRep];
]


(* ::Subsubsection::Closed:: *)
(*GaugeGroupFromIndex*)


GaugeGroupFromIndex[Index[index_]] := GaugeGroupFromIndex[index];
GaugeGroupFromIndex[index_] := Block[{gps},
	If[!GaugeGroupIndexQ[index],
		Return[{}];
	];
	gps = Select[PR$GaugeGroups, MemberQ[GetRule[#, Indices], index]&];
	If[Length[gps] != 1,
		Print["Error : unable to determine the gauge group associated with index '", index, "'."];
	];
	Return[gps[[1]]];
]


(* ::Subsubsection::Closed:: *)
(*GaugeGroupIndexQ*)


(* ::Text:: *)
(*Returns True if the index belongs to one of the group representation, False otherwise. If 'grp' is left unspecified, checks all the gauge groups.*)


GaugeGroupIndexQ[Index[index_], grp___] := GaugeGroupIndexQ[index, grp];
GaugeGroupIndexQ[index_, grp_] := MemberQ[GetRule[#, Indices]& /@ If[Head @ grp[[1]] === List, grp, {grp}], index, -1];
GaugeGroupIndexQ[index_] := GaugeGroupIndexQ[index, PR$GaugeGroups];


(* ::Subsection::Closed:: *)
(*Lagrangian Manipulation*)


(* ::Subsubsection::Closed:: *)
(*FinalTermExpression*)


FinalTermExpression[coupling_, explicit_] := Block[{expr, cgci, CG, cgcs, strExpr},
	If[explicit, Return @ GetRule[coupling, CGCs]];
	
	cgci = GetRule[coupling, CGCindices];
	expr = Dot @@ cgci[[2]];
	cgcs = MapThread[(#1 * Times @@ MapThread[Function[{n, inds}, ToExpression["C" <> ToString[n]][Sequence @@ (ToExpression /@ inds)]] , {#2, cgci[[1]]}])&, {-1 * GetRule[coupling, Norm], GetRule[coupling, CGCs]}];
	
	Return[Plus @@ ((#*expr)& /@ cgcs)];
]


(* ::Subsubsection::Closed:: *)
(*ExpandTerm*)


(* ::Text:: *)
(*Expand all the indices of a Lagrangian term*)


ExpandTerm[term_, rawFields_] := Block[{fields, allPieces, indList, indTypes, inds, unknown, uPos, uHead, uiPos, uNinds, identified, definitions, result, nullInd},
	(* For fermions, delete the spin index as well as the generation index if it exists *)
	fields = If[(Head @ #) =!= Symbol && (MemberQ[Flatten[Name /. PR$Fermions], Head @ #] || MemberQ[Flatten[Name /. PR$Fermions], GetConjugate@Head[#]]),
				Function[{newIndices}, If[newIndices =!={}, (Head @ #)[Sequence @@ newIndices], Head@#]] @ Extract[GetIndicesNames[#][[2;;-1]], Position[GaugeGroupIndexQ /@ GetIndicesTypes[#], True]]
				,
				#]& /@ rawFields; 
	
	allPieces = Join[fields, OtherPieces /. term];
	
	indList = (Function[{ind}, If[!NumberQ[ind], ind, Nothing]] /@ GetIndicesNames[#])& /@ (allPieces /. HC -> Identity);
	indTypes = (Cases[GetIndicesTypes[#], _?GaugeGroupIndexQ]& /@ (allPieces /. HC -> Identity)) ;
	
	inds = DeleteDuplicates[Flatten[MapThread[Partition[If[#2 =!= {}, Riffle[#1, #2], Riffle[#1, {{}, {}}]], 2]&, {indList, indTypes}], 1]];
	inds = DeleteCases[inds, {i_, {}} /; Length@Position[inds, i] > 1];
	unknown = Cases[inds, {_, {}}];
	inds = DeleteCases[inds, {_, {}}];
	
	identified = {};
	If[unknown =!= {},
		(* Some of the ranges are still undetermined. One last thing to try is to see if we're dealing with some well-known quantities : 
			- Pauli Matrix
			- Levi Civita
		*)
		Do[
			uPos = Position[allPieces, uInd[[1]]];
			Do[
				uHead = Head @ allPieces[[p[[1]]]];
				uNinds = Length[List @@ allPieces[[p[[1]]]]];
				uiPos = p[[2]];
				
				If[uHead === Eps || uHead === LeviCivitaTensor,
					identified = Append[identified, {uInd[[1]], 1, uNinds}];
					Break[];
				];
				If[uHead === PauliSigma || uHead === PauliMatrix,
					If[uiPos > 1,
						identified = Append[identified, {uInd[[1]], 1, 2}];
						,
						identified = Append[identified, {uInd[[1]], 1, 3}];
					];
					Break[];
				];
			,{p, uPos}];
		,{uInd, unknown}];
		
		If[Length[identified] < Length[unknown],
			Print["Unable to expand the term. It seems there are some indices whose range cannot be guessed."];
			Return[0]
		]];
	
	inds = {#[[1]], 1, GetIndexRange[#[[2]]]}& /@ inds;
	inds = Join[inds, identified];
	
	definitions = Flatten[GetRule[#, Definitions]& /@ Join[PR$Parameters[[All, 2]], PR$GaugeGroups]];
	
	If[inds === {}, inds = {{nullInd, 1, 1}}];
	
	result = Sum[(Norm /. term) * (Times @@ allPieces), Evaluate[Sequence @@ inds]] /. definitions;
	
	Return[result];
]


(* ::Subsubsection::Closed:: *)
(*FactorConjugateCoeffs*)


(* ::Text:: *)
(*Split terms with complex coefficient into real + imaginary parts*)


FactorConjugateCoeffs[potList_] := Block[{allCoeffs , newList = {}, nonConjTerm, coeff, expr, conjexpr},
	allCoeffs = GetRule[#, Coeff]& /@ potList;

	Do[
		If[Head @ GetRule[term, Coeff] =!= Conjugate,
			If[Conjugate[GetRule[term, Coeff]] === GetRule[term, Coeff] || !MemberQ[allCoeffs, Conjugate[GetRule[term, Coeff]]],
				newList = Append[newList, term];
			];
			Continue[];
		];
		nonConjTerm = potList[[Position[allCoeffs, Conjugate[GetRule[term, Coeff]]][[1,1]]]];
		coeff = GetRule[nonConjTerm, Coeff];
		expr = GetRule[nonConjTerm, FinalExpr];
		conjexpr = GetRule[term, FinalExpr];
		
		newList = Append[newList, SetRules[nonConjTerm, {Coeff, FinalExpr}, {Re[coeff], expr + conjexpr}]];
		newList = Append[newList, SetRules[nonConjTerm, {Coeff, FinalExpr}, {Im[coeff], I*(expr - conjexpr)}]];
	,{term, potList}];
	
	Return[newList];
]


(* ::Subsubsection::Closed:: *)
(*FactorPotentialTerms*)


(* ::Text:: *)
(*Gathers terms with identical coefficient*)


FactorPotentialTerms[potList_] := Block[{newPotList, newList = {}, newTerm, gathered},
	gathered = GatherBy[potList, GetRule[#, Coeff]&];
	Do[
		newTerm = terms[[1]];
		If[Length[terms] > 1,
			newTerm = SetRule[newTerm, FinalExpr, GetRule[#, FinalExpr]& /@ terms];
			newTerm = SetRule[newTerm, Fields, GetRule[#, Fields]& /@ terms];
		];
		newList = Append[newList, newTerm]
	,{terms, gathered}];
	
	Return[newList];
]


(* ::Subsubsection::Closed:: *)
(*FormatExpression*)


FormatExpression[term_, fields_, otherPieces_, norm_] := Block[{sNorm, expr, ind, indList, indReplacements = {}, indCount = 1, i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x},	
	expr = If[MemberQ[Flatten[Name /. PR$Fermions] && (Head @ #) =!= Symbol, Head @ #], Function[{newIndices}, If[newIndices =!={}, (Head @ #)[Sequence @@ newIndices], Head@#]] @ Extract[GetIndicesNames[#][[2;;-1]], Position[GaugeGroupIndexQ /@ GetIndicesTypes[#], True]], #]& /@ fields;
	expr = Join[otherPieces, expr];
	indList = DeleteDuplicates @ Flatten[GetIndicesNames /@ (expr /. HC -> Identity)];
	
	Do[
		(* Detect and replace indices generated by Mathematica modules (of type i$000000) *)
		If[StringContainsQ[ToString@ind, "$"],
			indReplacements = Append[indReplacements, ind -> ToExpression[FromCharacterCode[104 + indCount]]];
			indCount++;
		]
	,{ind, indList}];
	expr = expr /. indReplacements;
	
	Return[-1*GetRule[term, Norm]*Times@@expr];
];


(* ::Subsubsection::Closed:: *)
(*GetCGCs*)


GetCGCs[term_] := Block[{fields, fieldNames, explicit=False, explicitRepMats, otherPieces, expr, fieldsToSolve, Qnbs, Qlist, gList, gaugeSinglets,
						 indexDims, indexNames, fieldsWindices, inv, a,b,c,d, dynkins, cjs, auxInv, invRepRules, cgcIndices,
						 x, norm, n, CGCs, expTerm, termsToSolve, solvedNorm, ret = {}, tmp},
	If[GetRule[term, Expansion] === {}, 
		fields = DeleteCases[Join[GetRule[term, Scalars], GetRule[term, Fermions], {GetRule[term, Scalar]}], {}];
		fieldNames = If[Head[#] =!= Symbol, Head[#], #]& /@ fields;
		expTerm = ExpandTerm[term, fields];
		otherPieces = GetRule[term, OtherPieces];
		{fields, otherPieces} = SimplifyOtherPieces[fields, otherPieces];
		explicitRepMats = Select[otherPieces,  MemberQ[Flatten[Representations /. PR$GaugeGroups], Head@#]&];
		If[OptionValue[WritePyRATE, UsePyReps] === True,
			If[expTerm =!= 0 && (otherPieces == {} || AllTrue[otherPieces, Head@# === Eps || MemberQ[explicitRepMats, #]&]) , explicit = True];
		,
			If[expTerm =!= 0 && (otherPieces == {} || AllTrue[otherPieces, Head@# === Eps &]) , explicit = True];
		];
	,
		fieldNames = GetRule[term, FieldNames];
		expTerm = GetRule[term, Expansion];
	];
	
	(* Handle explicit expressions of lag term *)
	If[explicit,
		Return[{FormatExpression[term, fields, otherPieces, norm], fieldNames, explicitRepMats}];
	];
	
	Qlist = Table[{}, Length[fieldNames]];
	gList = {};
	(* Go through each gauge group. Check if Sum[qnbs] = 0 .  *)
	Do[
		(*Qnbs = (Function[{Q}, If[!MemberQ[ChargeConjugated /. term, fields[[#]]], Q, GetConjugateRep[g, Q]]] @ (GetRule[g, Name] /. ParticleFromName[ fieldNames[[#]] ][[1]]) )& /@ Range[Length[fieldNames]] ;*)
		Qnbs = (GetRule[g, Name] /. ParticleFromName[ fieldNames[[#]] ][[1]])& /@ Range[Length[fieldNames]] ;
		
		If[(Abelian /. g)  &&  Total[Qnbs] != 0, Print["Term is not invariant under abelian group ", Name /. g];Return[]];
		If[!(Abelian /. g), AppendTo[Qlist[[#]], Qnbs[[#]]]& /@ Range[Length[Qnbs]]; AppendTo[gList, g];];
	, {g, PR$GaugeGroups}];
	
	indexDims = MapThread[Function[{g, q}, Function[{dim}, If[dim != 1, dim, 0]] @ If[q =!= 1, GetDimR[Tag /. g, q], 1]], {gList, #}]& /@ Qlist;
	
	indexNames = Function[{indnb}, If[indexDims[[indnb]][[#]] != 0, ToExpression[FromCharacterCode[96 + indnb]<>ToString[#]], Nothing]& /@ Range[Length[indexDims[[indnb]]]]] /@ Range[Length[fieldNames]];
	indexDims = indexDims /. 0->Nothing;
	
	fieldsWindices = fieldNames[[#]][Sequence @@ indexNames[[#]]]& /@ Range[Length[fieldNames]] /. s_[] :> s ;

	inv = {};
	cgcIndices = {};
	gaugeSinglets = {};
	Do[
		dynkins = If[# === 1, Table[0, GetRule[gList[[gNb]], Rank]], #]&/@ Qlist[[All, gNb]];
		If[DeleteDuplicates@Flatten@dynkins === {0}, AppendTo[gaugeSinglets, gNb]; Continue[]];
		cjs = If[#[[-1]] === True, True, False]& /@ dynkins;
		dynkins = If[#[[-1]] === True || #[[-1]] === False, #[[;;-2]], #]& /@ dynkins;
		invRepRules = Function[{i}, If[# === 1, ToExpression[FromCharacterCode[96 + i] <> "[_] \[Rule] 1"], ToExpression[FromCharacterCode[96 + i] <> "[n_] :>" <> FromCharacterCode[96 + i] <> ToString[gNb] <> "[n]"]]& @ Qlist[[All, gNb]][[i]]] /@ Range[Length[Qlist[[All, gNb]]]];
		
		auxInv = GetInvariant[Tag /. gList[[gNb]], dynkins, cjs, "adjoint", True];
		auxInv = auxInv /. invRepRules;
		
		cgcIndices = Append[cgcIndices, Function[{i}, If[# === 1, Nothing, FromCharacterCode[96 + i] <> ToString[gNb]]& @ Qlist[[All, gNb]][[i]]] /@ Range[Length[Qlist[[All, gNb]]]]];
		inv = Append[inv, auxInv];
	, {gNb, Length[gList]}];
	If[MemberQ[inv, {}], Print["It seems that the term is not gauge-invariant...  : ", term]; Return[{}]];
	
	inv = inv /. s_Symbol[n_] /; s =!= List :> KroneckerDelta[s, n];
	
	CGCs = Tuples[Range[Length[#]]& /@ inv];
	inv = Tuples[inv];
	norm = Array[n, Length[CGCs]];
	
	inv = norm . (Times @@@ inv);
	inv = (Times @@ fieldsWindices) * inv;
	
	If[Flatten[indexNames] =!= {},
		inv = Sum[inv, Evaluate[Sequence @@ MapThread[{#1, 1, #2}&, {Flatten[indexNames], Flatten[indexDims]}]]];
	];
	
	termsToSolve = List @@ (If[Head[#] === Plus, #, {#}]& @ ( (inv /. n[m_] :> 1) /. Times[x_?NumericQ, r___] :> Times[r] ));
	solvedNorm = Flatten[Solve[Expand@Coefficient[inv, termsToSolve] == Coefficient[expTerm, termsToSolve]]];
	
	If[solvedNorm === {} || DeleteDuplicates[solvedNorm] === {0}, Print["It seems that the term is not gauge-invariant...  : ", term]; Return[{}]];
	
	solvedNorm = norm /. solvedNorm;
	solvedNorm = solvedNorm /. Reverse[Flatten[Solve[# == 0]& /@ solvedNorm]][[1;;Length[DeleteDuplicates[Cases[solvedNorm, n[_], -1]]]]];
	
	CGCs = Extract[CGCs, Position[solvedNorm, _?(# != 0&), {1}]];
	solvedNorm = Extract[solvedNorm, Position[solvedNorm, _?(# != 0&), {1}]];
	
	
	CGCs = {Transpose[Function[{gNb}, With[{gpos = Complement[Range[Length[gList]], gaugeSinglets][[gNb]]},
										Function[{cgc}, {(Tag /. gList[[gpos]]),
														Sequence @@ (GetRule[ParticleFromName[#][[1]], Name /. gList[[gpos]]]& /@ Select[fieldNames, ChargedQ[#, gList[[gpos]]]&]),
														cgc-1}] /@ CGCs[[All, gNb]]] ] /@ Range[Length[gList]-Length[gaugeSinglets]]], solvedNorm};
	Do[
		Do[
			If[!MemberQ[PR$CGCs, CGCs[[1,i,j]]],
				PR$CGCs = Append[PR$CGCs, CGCs[[1,i,j]]];
			];
			CGCs[[1,i,j]] = Position[PR$CGCs, CGCs[[1,i,j]]][[1,1]];
		,{j, Length[CGCs[[1,i]]]}];
	,{i, Length[CGCs[[1]]]}];
	
	Return[Join[CGCs, {fieldNames, {cgcIndices, fieldsWindices}}]];
]


(* ::Subsubsection::Closed:: *)
(*HandleExpandedTerms*)


(* ::Text:: *)
(*Handle Lagrangian terms where indices are already (fully or partially) expanded.*)


HandleExpandedTerms[termList_] := Block[{fields, fieldNames, expanded, listExp={}, listNotExp={}},
	Do[
		fields = DeleteCases[Join[GetRule[term, Scalars], GetRule[term, Fermions], {GetRule[term, Scalar]}], {}];
		fieldNames = Sort[If[Head[#] =!= Symbol, Head[#], #]& /@ fields];
	
		If[(AllTrue[#, IntegerQ] && # =!= {} && GetRule[term, OtherPieces] === {})& @ Flatten[GetIndicesNames /@ fields], 
			AppendTo[listExp, Join[term, {Fields->fields, FieldNames->fieldNames}]];
			Continue[]];
		If[AnyTrue[#, IntegerQ]& @ Flatten[GetIndicesNames /@ fields],
			expanded = ExpandTerm[term, fields];
			If[Head @ expanded === Plus, expanded = List @@ expanded];
			If[Head @ expanded === Times, expanded = {expanded}];
			Do[
				AppendTo[listExp, Join[term, {Fields -> exp, FieldNames->fieldNames}]];
			,{exp, expanded}];
			Continue[]];
			
		AppendTo[listNotExp, term];
	, {term, termList}];

	If[listExp === {}, Return[termList]];
	listExp = SetRules[#[[1]], {Norm, Expansion}, {1, Sum[(Times @@ GetRule[#[[ii]], Fields]), {ii, Length[#]}]}]& /@  GatherBy[listExp, {GetRule[#, Coeff], GetRule[#, FieldNames]}&];

	Return[Join[listExp, listNotExp]];
]


(* ::Subsubsection::Closed:: *)
(*InactivateLag*)


(* ::Text:: *)
(*Inactivates functions ExpandIndices, DC, FS and HC before returning the Lagrangian*)


SetAttributes[InactivateLag, HoldAll];
InactivateLag[{lag__}] := Block[{ExpandIndices = PR$iExpand, DC = PR$DCi, FS = PR$FSi, HC = PR$HCi},
	Return[Total[{lag}]]
]


(* ::Subsubsection::Closed:: *)
(*LagToParts*)


LagToParts[lag_] := Flatten[{( (lag //. PR$iExpand[a_, ___] :> Expand[a]) ) /. Plus->List}] ;


(* ::Subsubsection::Closed:: *)
(*SimplifyOtherPieces*)


SimplifyOtherPieces[fields_, otherPieces_] := Block[{newFields, newOtherPieces = {}, param, pName, nInds, inds, def, a,b, repRules = {}, count, simplifyDeltas, el, rest},
	Do[
		param = ParameterFromName[Head @ p];
		If[param == {}, AppendTo[newOtherPieces, p]; Continue[]];
		def = (Definitions /. param);
		If[Length[def] > 1, AppendTo[newOtherPieces, p]; Continue[]];
		def = def[[1]];
		pName = Head @ p;
		nInds = Length[Indices /. param];
		inds = List @@ p;
		
		(* Detect kronecker delta *)
		If[nInds == 2 && ((pName[a,b] /. def) === IndexDelta[a,b] || (pName[a,b] /. def) === KroneckerDelta[a,b]),
			repRules = Append[repRules, inds];
		];
		(* Detect Levi-civita *)
		If[(p /. def) === Eps[Sequence @@ inds],
			newOtherPieces = Append[newOtherPieces, p /. def];
		];
	,{p, otherPieces}];
	
	If[repRules =!= {},
		simplifyDeltas = (Length[DeleteDuplicates@Flatten@repRules] < Length[Flatten@repRules]) ;
		count = 1;
		While[simplifyDeltas,
			el = repRules[[count]];
			rest = Drop[repRules, {count}];
			If[MemberQ[Flatten@rest, el[[1]]],
				repRules = rest /. el[[1]] -> el[[2]];
				count --;
			];
			If[MemberQ[Flatten@rest, el[[2]]],
				repRules = rest /. el[[2]] -> el[[1]];
				count--;
			];
			count ++;
			simplifyDeltas = count <= Length[repRules] && (Length[DeleteDuplicates@Flatten@repRules] < Length[Flatten@repRules]);
		];
		repRules = #[[2]]->#[[1]]& /@ repRules;
		Do[
			newOtherPieces = newOtherPieces /. r;
		,{r, repRules}];
	];
	newFields = fields /. repRules;

	Return[{newFields, newOtherPieces}];
]


(* ::Subsubsection::Closed:: *)
(*UpdateCGCs*)


UpdateCGCs[HtermList_] := Module[{termList, ret, newTerm, cgc, count=1, explicit, t},
	termList = ReleaseHold @ HtermList;
	(*If[Length@termList > 0,
		PR$Messages[Hold["  Computing the terms in " <> StringReplace[ToString[HtermList], {"Hold["|"]"->""}] <> " ..." <> If[ValueQ[count] && count <= Length@termList, " (" <> ToString[count] <> "/" <> ToString[Length@termList] <> ")", ""]], 1, Dynamic->True];
	];*)
	
	ret = {};
	Do[
		count++;
		cgc = GetCGCs[t];
		If[cgc === {},
			Continue[];
		];
		If[Length[cgc] == 4,
			newTerm = Join[SetRule[t, Norm, cgc[[2]]], {Fields -> cgc[[3]], CGCs -> cgc[[1]], CGCindices -> cgc[[4]]}];
			explicit = False;
		];
		If[Length[cgc] == 3,
			newTerm = Join[t, {Norm -> 1, Fields -> cgc[[2]], CGCs -> cgc[[1]], RepMats -> DeleteDuplicates[Head /@ cgc[[3]]]}];
			explicit = True;
		];
		newTerm = Append[newTerm, FinalExpr -> FinalTermExpression[newTerm, explicit]];
		ret = Append[ret, newTerm];
	,{t, termList}];
	Return@ret
]


(* ::Subsection::Closed:: *)
(*Lagrangian terms reading*)


(* ::Subsubsection::Closed:: *)
(*FindGeneralScalarTerm   +   FindScalarMasses / FindTrilinears / FindQuartics*)


FindGeneralScalarTerm[termList_, N_, sList_, otherBosonsList_] := Module[{auxTermList, coeff, coeffTex, norm, otherPieces, notAparam, TeXcoeff, paramNames, def, values, squared = False},

	If[Length[sList] != N || Length[otherBosonsList] > 0, Return[{}]];
	
	norm = Cases[termList, _?NumericQ];
	
	(* otherPieces comprises : Eps terms, KroneckerDeltas, representation matrices (for fields living in adjoint reps), and user defined parameters carriyng gauge-related indices *)
	otherPieces = Cases[termList, _Eps | _KroneckerDelta | _?(MemberQ[Flatten[Representations /. PR$GaugeGroups], Head[#]]&) | _?((MemberQ[PR$Parameters[[All, 1]], Head[#]] && (DeleteDuplicates[GaugeGroupIndexQ /@ GetRule[ParameterFromName[Head[#]], Indices]] === {True}))&)];
	
	(* Remove norm, otherPieces and scalars from termList : only coeffs left *)
	auxTermList = DeleteDuplicates @ DeleteCases[termList, t_ /; (MemberQ[norm, t] || MemberQ[sList, t] || MemberQ[otherPieces, t])];
	(* Norm goes from a list to a single number *)
	norm = If[norm == {}, norm = 1, Times @@ norm];
	
	(* Let's look for non-parameters quantities a second time *)
	notAparam = Select[auxTermList, !MemberQ[PR$Parameters[[All, 1]], # /. {Conjugate->Identity, Power[x_, _Integer] :> x}] && !MemberQ[PR$Parameters[[All, 1]], Head@(# /. {Conjugate->Identity, Power[x_, _Integer] :> x})]&];
	otherPieces = Join[otherPieces, notAparam];
	auxTermList = Select[auxTermList, !MemberQ[notAparam, #]&];
	
	If[Length[auxTermList]==1, 
		coeff = auxTermList[[1]],
		Print["Ambiguity regarding scalar-term (N=", N, ") coefficient...  -> ", auxTermList];
		Print[termList // FullForm];
		Return[{}];
	];
	
	If[Head[coeff] === Conjugate, Return[{}]];
		
	(TeXcoeff = GetRule[#, TeX]; paramNames = GetRule[#, ParameterName];
	 def = GetRule[#, Definitions]; values = GetRule[#, Value];)& @ ( ParameterFromName[If[Head[#] =!= Symbol, Head[#], #]& @ If[N != 2, coeff, coeff /. Power[c_, 2] :> c]] );
	If[Head@paramNames =!= List, paramNames = (coeff -> paramNames)];
	
	Return[{Norm -> norm, Coeff -> If[N != 2, coeff, coeff /. Power[c_, 2]:>(squared=True; c)], Scalars -> sList, OtherPieces -> otherPieces, TeX -> TeXcoeff, Expansion->{}, Names->paramNames, Definitions->def, Values->values, Squared->squared}];
];


FindScalarMasses[termList_, sList_, otherBosonsList_] := FindGeneralScalarTerm[termList, 2, sList, otherBosonsList];
FindTrilinears[termList_, sList_, otherBosonsList_] := FindGeneralScalarTerm[termList, 3, sList, otherBosonsList];
FindQuartics[termList_, sList_, otherBosonsList_] := FindGeneralScalarTerm[termList, 4, sList, otherBosonsList];


(* ::Subsubsection::Closed:: *)
(*FindFermionMasses*)


FindFermionMasses[termList_, fList_, sList_, otherBosonsList_] := Module[{auxTermList, coeff, coeffTex, norm, otherPieces, notAparam, TeXcoeff, paramNames, def, values, cplx, ret},

	If[!(Length[fList] == 2 && Length[sList] == 0) || Length[otherBosonsList] > 0, Return[{}]];
	
	norm = Cases[termList, _?NumericQ];
	
	(* otherPieces comprises : Eps terms, KroneckerDeltas, representation matrices (for fields living in adjoint reps), and user defined parameters carriyng gauge-related indices *)
	otherPieces = Cases[termList, _Eps | _KroneckerDelta | _?(MemberQ[Flatten[Representations /. PR$GaugeGroups], Head[#]]&) | _?((MemberQ[PR$Parameters[[All, 1]], Head[#]] && (DeleteDuplicates[GaugeGroupIndexQ /@ GetRule[ParameterFromName[Head[#]], Indices]] === {True}))&)];
	
	(* Remove norm, otherPieces and fermions from termList : only coeffs left *)
	auxTermList = DeleteDuplicates @ DeleteCases[termList, t_ /; (MemberQ[norm, t] || MemberQ[fList, t] || MemberQ[otherPieces, t])];
	(* Norm goes from a list to a single number *)
	norm = If[norm == {}, norm = 1, Times @@ norm];
	
	(* Let's look for non-parameters quantities a second time *)
	notAparam = Select[auxTermList, !MemberQ[PR$Parameters[[All, 1]], # /. Conjugate->Identity] && !MemberQ[PR$Parameters[[All, 1]], Head@(# /. Conjugate->Identity)]&];
	otherPieces = Join[otherPieces, notAparam];
	auxTermList = Select[auxTermList, !MemberQ[notAparam, #]&];
	
	(* Find the Fermion mass coeff. If only one coeff left, no ambiguity *)
	If[Length[auxTermList]==1, 
		(* coeff = If[Head[#] =!= Symbol, Head[#], #]& @ termList[[1]] *)
		coeff = auxTermList[[1]]
	,
		If[Length@# === 1,
			coeff = #[[1]];
		,
			Print["Warning : There is an ambiguity in the determination of the coupling in ", Times @@ termList, "."];
			Print["Please add the keyword 'PyrateCoupling -> True' in the definition of the relevant coupling"];
			Return[{}]
		]& @ Select[auxTermList, GetRule[Flatten@ParameterFromName[Head @ #], PyrateCoupling] === True &];
	];
	
	(TeXcoeff = GetRule[#, TeX]; paramNames = GetRule[#, ParameterName];
	 def = GetRule[#, Definitions]; values = GetRule[#, Value];
	 cplx = GetRule[#, ComplexParameter];)& @ ParameterFromName[If[Head[coeff] =!= Symbol, Head[coeff], coeff]];
	
	ret = {Norm -> norm, Coeff -> coeff, Fermions -> fList, OtherPieces -> otherPieces, TeX -> TeXcoeff, Expansion->{}, Names->paramNames, Definitions->def, Values->values};
	If[cplx =!= True, ret = Append[ret, Assumptions -> {"real"}]];
	
	Return[ret]
];


(* ::Subsubsection::Closed:: *)
(*FindYukawas*)


FindYukawas[termList_, sList_, fList_, otherBosonsList_] := Module[{auxTermList, coeff, coeffTex, norm, repRules, otherPieces, notAparam, TeXcoeff, paramNames, def, values, cplx, ret, unitary},

	(* Keep only yukawa terms : 2 fermions & 1 scalar *)
	If[!(Length[fList] == 2 && Length[sList] == 1) || Length[otherBosonsList] > 0, Return[{}]];
	
	norm = Cases[termList, _?NumericQ];
	
	otherPieces = Cases[termList, _Eps | _KroneckerDelta | _?(MemberQ[Flatten[Representations /. PR$GaugeGroups], Head[#]]&) | _?((MemberQ[PR$Parameters[[All, 1]], Head[#]] && (DeleteDuplicates[GaugeGroupIndexQ /@ GetRule[ParameterFromName[Head[#]], Indices]] === {True}))&)];
	
	(* Remove norm, fermions, scalars, otherPieces and gamma matrices from termList : only coeffs left *)
	auxTermList = DeleteCases[termList, t_ /; (MemberQ[norm, t] || MemberQ[fList, t] || MemberQ[sList, t] || MemberQ[otherPieces, t] || Head[t] === Ga )];
	
	(* Norm goes from a list to a single number *)
	norm = If[norm == {}, norm = 1, Times @@ norm];
	
	(* Let's look for non-parameters quantities a second time *)
	notAparam = Select[auxTermList, !MemberQ[PR$Parameters[[All, 1]], # /. Conjugate->Identity] && !MemberQ[PR$Parameters[[All, 1]], Head@(# /. Conjugate->Identity)]&];
	otherPieces = Join[otherPieces, notAparam];
	auxTermList = Select[auxTermList, !MemberQ[notAparam, #]&];
	
	(* Find the Yukawa coeff. If only one coeff left, no ambiguity *)
	If[Length[auxTermList]==1,
		coeff = auxTermList[[1]]
	,
		If[Length@# === 1,
			coeff = #[[1]];
		,
			If[OptionValue[WritePyRATE, GuessYukawaCoeff] === True,
				coeff = Flatten[{FindYukawaCoeff[auxTermList]}];
			];
		
			If[Length[coeff] != 1, 
				Print["Warning : There is an ambiguity in the determination of the coupling in ", Times @@ termList, "."];
				Print["Please add the keyword 'PyrateCoupling -> True' in the definition of the relevant coupling"];
				Return[{}]
			,
				coeff = coeff[[1]];
			];
		]& @ Select[auxTermList, GetRule[Flatten@ParameterFromName[Head @ #], PyrateCoupling] === True &];
	];
	
	(* Remove Yukawa coeff from list *)
	auxTermList = DeleteCases[auxTermList, t_ /; t === coeff];
	
	(* Converts remaining matrices (eg CKM, PMNS) in Kronecker deltas, using replacement rules *)
	repRules = Flatten[Cases[auxTermList, Conjugate[_Symbol[iiii___]] |  _Symbol[iiii___] :> If[Length[{iiii}]>1, ({iiii}[[#]] -> {iiii}[[1]])& /@ Range[2, Length[{iiii}]], 0]]] ;
	
	coeff = coeff /. repRules;
	
	(TeXcoeff = GetRule[#, TeX]; paramNames = GetRule[#, ParameterName];
	 def = GetRule[#, Definitions]; values = GetRule[#, Value];
	 cplx = Function[{rawCplx}, If[rawCplx === {}, If[GetRule[#, Indices] =!= {}, True, False], rawCplx]][GetRule[#, ComplexParameter]];
	 unitary = GetRule[#, Unitary];)& @ ParameterFromName[If[Head[coeff] =!= Symbol, Head[coeff], coeff]];
	
	ret = {Norm -> norm, Coeff -> coeff, Scalar -> (sList /. repRules)[[1]], Fermions -> (fList /. repRules), OtherPieces -> otherPieces, TeX -> TeXcoeff, Expansion->{}, Names->paramNames, Definitions->def, Values->values};
	If[cplx =!= True, ret = Append[ret, Assumptions -> {"real"}]];
	If[unitary === True, ret = SetRule[ret, Assumptions, Append[GetRule[ret, Assumptions], "unitary"]]];
	
	Return[ret]
]




(* ::Subsubsection::Closed:: *)
(*FindYukawaCoeff*)


FindYukawaCoeff[coeffList_, conj_:False] := Block[{parList, yukPos},
	parList = Select[coeffList, (Head@# =!= Conjugate && ParameterFromName[Head@#] =!= {}) || (Head@# === Conjugate && ParameterFromName[Head@#[[1]]] =!= {})&];
	parList = Select[parList, !AnyTrue[GetRule[ParameterFromName@Head[# /. Conjugate->Identity], Indices], GaugeGroupIndexQ]&];
	If[Length[parList] == 1, Return[parList[[1]]]];
	
	yukPos = Flatten[Position[YukawaQ[# /. ( (p_Symbol | p_[___]) -> p )]& /@ parList, True]];
	If[Length[yukPos] == 1, Return[parList[[yukPos[[1]]]]],
		If[!conj, Return[If[# =!= {}, Conjugate[#], #]& @ FindYukawaCoeff[Conjugate /@ coeffList, True]];
	,
		Return[{}]];
	];
	 
	Return[parList];
];


(* ::Subsection::Closed:: *)
(*Parameters*)


(* ::Subsubsection::Closed:: *)
(*ParameterFromName*)


ParameterFromName[parName_] := Block[{p = Cases[PR$Parameters, parName == x_ -> x]}, If[p != {}, p = p[[1]]]; Return[p]]


(* ::Subsubsection::Closed:: *)
(*YukawaQ*)


(* ::Text:: *)
(*Returns True if parameter 'par' corresponds to a Yukawa matrix/constant. The main purpose of this function is to try to distinguish between CKM and Yukawa.*)


YukawaQ[parName_] := Block[{containedPar, intParNames = Cases[PR$Parameters, pn_ == lp_ /; (ParameterType /. lp) === Internal -> pn], 
						par = ParameterFromName[parName], parType = GetRule[ParameterFromName[parName], ParameterType],
						extParNames = Cases[PR$Parameters, pn_ == lp_ /; (ParameterType /. lp) === External -> pn]},

	If[StringContainsQ[ToUpperCase[ToString[BlockName /. par]], "YUKAWA"], Return[True]];
	If[StringContainsQ[ToUpperCase[ToString[BlockName /. par]], "YUK"], Return[True]];
	If[StringContainsQ[ToUpperCase[ToString[Description /. par]], "YUKAWA"], Return[True]];
	If[StringContainsQ[ToUpperCase[ToString[Description /. par]], "YUK"], Return[True]];
	(* If[StringContainsQ[ToUpperCase[ToString[parName]], "Y"], Return[True]]; *)
		
	If[parType === Internal,
		containedPar = DeleteDuplicates[Cases[Value /. par, (x_Symbol | x_[___]) /; (x =!= parName && ParameterFromName[x] != {}) -> x, -1] ];
		Return[Or @@ (YukawaQ /@ containedPar)];
	];
	If[parType === External,
		Return[False];
	];	
]


(* ::Subsubsection::Closed:: *)
(*ParameterToRealComponents*)


Options[ParameterToRealComponents] = {Riffle -> True, Conjugate -> False};
ParameterToRealComponents[param_, OptionsPattern[]] := Block[{pStruc, cplx, shape, pList, imFactor},
	pStruc = MR$ParameterRules[param];
	cplx = Function[{rawCplx}, If[rawCplx === {}, If[GetRule[pStruc, Indices] =!= {}, True, False], rawCplx]][GetRule[pStruc, ComplexParameter]];
	shape = If[# =!= {}, GetIndexRange /@ #, {}]&[GetRule[pStruc, Indices]];
	
	pList = If[shape === {}, {param}, (ToExpression[ToString[param] <> StringRiffle[ToString /@ #, "x"]])& /@ Tuples[Range /@ shape]];
	
	If[cplx, 
		imFactor = If[OptionValue[Conjugate] === False, 1, -1]; 
		If[OptionValue[Riffle] === True,
			pList = Riffle[Re /@ pList, imFactor*(Im /@ pList)]
		,
			pList = Re /@ pList + imFactor*I*(Im /@ pList)
		];
	];
		
	Return[pList]
]


(* ::Subsection::Closed:: *)
(*Particles*)


(* ::Subsubsection::Closed:: *)
(*AddRealFields*)


AddRealFields[listS_] := Block[{i, s, complexCount, newlistS = listS, conjSPos},
	complexCount = 1;
	For[i=1, i<Length[listS], i++,
		s = newlistS[[i]];
		If[GetRule[s, Complex] && GaugeEigenstateQ[s] && GetRule[s, RealFields]=="[]", 
			newlistS[[i]] = SetRule[s, RealFields, { Symbol["Pi"<>ToString[complexCount]], I*Symbol["Sigma"<>ToString[complexCount]] } ] ;
			conjSPos = Flatten[Position[newlistS, x_ /; GetRule[x, Name] === GetConjugate[GetRule[s, Name]], {1}]][[1]] ;
			newlistS[[conjSPos]] = SetRule[newlistS[[conjSPos]], RealFields, { Symbol["Pi"<>ToString[complexCount]], -I*Symbol["Sigma"<>ToString[complexCount]] }];
			complexCount++;
		]
	];
	
	Return[newlistS]
]


(* ::Subsubsection::Closed:: *)
(*ChargedQ*)


(* ::Text:: *)
(*Checks if particle with name 'p' is charged under gauge group 'gp' or with name 'gp'. *)


ChargedQ[p_, gp_] := Block[{pList, gList, qnb, abel},
	pList = ParticleFromName[p][[1]];
	gList = If[Head @ gp === List, gp, Select[PR$GaugeGroups, GetRule[#, Name] === gp&][[1]]];
	abel = GetRule[gList, Abelian];
	qnb = GetRule[pList, GetRule[gList, Name]];
	
	If[abel && qnb === 0, Return[False]];
	If[!abel && (qnb === 1 || (Head@qnb === List && DeleteDuplicates[qnb] === {0})), Return[False]];
	Return[True];
]


(* ::Subsubsection::Closed:: *)
(*ConjugateParticleList*)


ConjugateParticleList[pList_] := DeleteDuplicates @ DeleteCases[Riffle[pList, GetConjugate[#]& /@ pList], {}];


(* ::Subsubsection::Closed:: *)
(*FindGaugeEigenstates*)


(* ::Text:: *)
(*Updates the rules 'UsedIn', 'ComposedOf' and 'GaugeEigenstate' in pList, then returns the updated list.*)


FindGaugeEigenstates[pList_, FRpList_] := Block[{x, i, j, k, newpList = pList, p, pContained, pContainedList, pbarContained, pbarContainedList, pContainedPos,
												pListNames = Name /. pList, sameQNbs, massBasisFields},
	For[i=1, i<=Length[FRpList], i++, (* k=2i-1;*) k = i;
		p = FRpList[[i]];
		pContained = DeleteDuplicates[Cases[GetRule[p, Definitions],  (x_Symbol | x_[___]) /; (MemberQ[pListNames(*[[1;;-1;;2]]*), x] && !(x === GetRule[p, ClassName]))  -> x, -1]];
			pContainedPos = Flatten[Position[ pListNames , #]][[1]]& /@ pContained;
		pbarContained = DeleteDuplicates[Cases[GetRule[p, Definitions],  (x_Symbol | x_[___]) /; (MemberQ[pListNames(*[[1;;-1;;2]]*), GetConjugate[x]] && HC[x] =!= x)  -> x, -1]];
			pContainedPos = Join[pContainedPos, Flatten[Position[ pListNames , GetConjugate[#]]][[1]]& /@ pbarContained ];
		
		(* Update ComposedOf *)
		newpList[[k]] = SetRule[newpList[[k]], ComposedOf, pContained];
		newpList[[k]] = SetRule[newpList[[k]], ComposedOf, Join[GetRule[newpList[[k]], ComposedOf], pbarContained]];
		(* newpList[[k+1]] = SetRule[newpList[[k+1]], ComposedOf, GetConjugate[pContained]]; *)
		
		(* Update UsedIn *)
		For[j=1, j<=Length[pContainedPos], j++,
			newpList[[ pContainedPos[[j]] ]] = UpdateUsedIn[newpList[[pContainedPos[[j]]]], pList[[k]]];
			(* newpList[[ pContainedPos[[j]] + 1]] = UpdateUsedIn[newpList[[pContainedPos[[j]] + 1]], pList[[k+1]]]; *)
		];
		
		sameQNbs = SameQuantumNumbersQ[pList[[#]]& /@ pContainedPos];
		
	];
	
	For[k=1, k<=Length[newpList], k++,
		pContained = If[ParticleFromName[#, FRpList] =!= {}, ParticleFromName[#, FRpList][[1]], Nothing(*GetConjugate[ParticleFromName[GetConjugate[#]][[1]]]*)]& /@ (ComposedOf /. newpList[[k]]);
		sameQNbs = SameQuantumNumbersQ @ pContained ;
		newpList[[k]] = If[GetRule[newpList[[k]], UsedIn] === {} && (Length[pContained] == 0 || sameQNbs), Replace[newpList[[k]], HoldPattern[GaugeEigenstate -> _] :> (GaugeEigenstate -> True), 1], newpList[[k]] ];
	];
	
	(* ASPERGE-style implementations: particles appearing in "MassBasis" aren't gauge eigenstates *)
	
	massBasisFields = DeleteDuplicates[Join[#, GetConjugate /@ #]& @ (
							DeleteDuplicates[If[Head[#] =!= Symbol, Head[#], #]& /@ (If[# =!= {}, Flatten[MassBasis /. #[[All,2]]]]& @ M$MixingsDescription)]
						)];
	
	For[k=1, k<=Length[newpList], k++,
		newpList[[k]] = If[MemberQ[massBasisFields, GetRule[newpList[[k]], Name]], SetRule[newpList[[k]], GaugeEigenstate, False], newpList[[k]]];
	];
	
	Return[newpList];
]


(* ::Subsubsection::Closed:: *)
(*GaugeEigenstateQ*)


(* ::Text:: *)
(*p : particle as stored in PR$Fermions and PR$Scalars lists*)
(*pname : particle name.  It can carry indices.*)


GaugeEigenstateQ[p_List] := If[GetRule[p, GaugeEigenstate] === True, Return[True], Return[False]];
GaugeEigenstateQ[pname_] := Block[{p},
	p = ParticleFromName[If[Head[pname] =!= Symbol, Head[pname], pname]];
	
	If[p === {} && StringContainsQ[ToString[pname], "bar"], p = ParticleFromName[GetConjugate @ If[Head[pname] =!= Symbol, Head[pname], pname]]];

	If[p === {} || GetRule[p[[1]], GaugeEigenstate] === False, Return[False]];
	If[GetRule[p[[1]], GaugeEigenstate] === True, Return[True]];
]


(* ::Subsubsection::Closed:: *)
(*GetConjugate*)


(* ::Text:: *)
(*Not a real conjugation operation. Always transforms p to pbar ( pbar to p ) in expression.*)
(*Eg :*)
(*	phi -> phibar*)
(*	phibar[i] -> phi[i]*)
(*	QL[s, i ,c] -> QLbar[s,i, c]*)


GetConjugate[p_] := If[((Spinor /. #) === Majorana)& @ Flatten[ParticleFromName @ p], GetConjugate[#], #]& @ If[Head @ p === Symbol, HC @ p /. ( (Dot | Times)[Ga[___], f_] | (Dot | Times)[f_, Ga[___]] -> f ) , (HC @ p[[0]] /. ( (Dot | Times)[Ga[___], f_] | (Dot | Times)[f_, Ga[___]] -> f )) @@ p];

GetConjugate[p_List] := Block[{pConj, pbar},
	(* If[p === {}, Return[{}]]; *)
	If[ (Complex /. p ) === False || (Spinor /. p) === Majorana, Return[p]];
	
	pConj = p;
	pbar = GetConjugate[GetRule[p, Name]];
	
	pConj = SetRule[pConj, Name, pbar];
	pConj = SetRules[pConj, GetRule[#, Name]& /@ PR$GaugeGroups, GetConjugateRep[GetRule[#, Tag], GetRule[p, GetRule[#, Name]], OptionValue[WritePyRATE, RealBasis]]& /@ PR$GaugeGroups];
	pConj = SetRule[pConj, ComposedOf, GetConjugate /@ GetRule[pConj, ComposedOf]];
	pConj = SetRule[pConj, Conjugated, True];
	Return[pConj];
]


(* ::Subsubsection::Closed:: *)
(*GetGenerationNb*)


GetGenerationNb[p_] := Block[{particleIndices = Flatten[GetRule[p, Indices] /. (Index[index_] :> index)], nonGaugeIndices},
							nonGaugeIndices = Select[particleIndices, (!GaugeGroupIndexQ[#])&];
	
	If[Length[nonGaugeIndices] == 1,
		Return[GetIndexRange[nonGaugeIndices[[1]]]],
		If[Length[nonGaugeIndices] == 0,
			Return[1],
			Print["Error : Unable to determine the number of generations of particle '", GetRule[p, ClassName], "'"]
		]
	]
]


(* ::Subsubsection::Closed:: *)
(*GetQuantumNumber*)


(* ::Text:: *)
(*Returns the quantum number associated with group 'g' , by looking at particle's indices.*)
(*Converts it into its Dynkin-label form.*)
(*If undetermined, returns PR$defaultA / PR$defaultNA.  (Abelian / Non-Abelian).*)


GetQuantumNumber[p_, g_] := Block[{particleIndices = GetRule[p, Indices] /. Index[iname_] :> iname, commonIndices, qNbs = DeleteDuplicates[GetRule[p, QuantumNumbers]]},	
	commonIndices = Select[particleIndices, GaugeGroupIndexQ[#, g]&];
	
	If[Length[commonIndices]==1, 
		Return[GetDynkinLabels[GetRule[GaugeGroupFromIndex[#], Tag], GetRepresentation[#], OptionValue[WritePyRATE, RealBasis]]& @ commonIndices[[1]]], 
		
		If[GetRule[g, Abelian],
			If[GetRule[qNbs,  GetRule[g, Charge]] =!= {},
				Return[GetRule[qNbs,  GetRule[g, Charge]]],
				Return[PR$defaultA];
			],
			Return[PR$defaultNA];
		]
	]
]


(* ::Subsubsection::Closed:: *)
(*ParticleFromName*)


(* ::Text:: *)
(*Works with both PyR@TE list (PR$Fermions & PR$Scalars) and FR list (M$ClassesDescription)*)


ParticleFromName[pname_, pList_:Join[PR$Fermions, PR$Scalars]] := If[Length[pList] > 0 && Head[pList[[1]]]===Rule, Select[pList[[All, 2]], (GetRule[#, ClassName] === pname)&], Select[pList, ( GetRule[#, Name] === pname )&] ];


(* ::Subsubsection::Closed:: *)
(*ReadParticle / ReadFermion / ReadScalar*)


(* ::Text:: *)
(*Collects info on particle 'p' *)


ReadParticle[p_] := Block[{name, returnList},
	name = GetRule[p, ClassName];
	
	returnList = {Name -> name};
	AppendTo[returnList, GetRule[#, Name] -> GetQuantumNumber[p, #]]& /@ PR$GaugeGroups;
	AppendTo[returnList, UsedIn -> {}];
	AppendTo[returnList, ComposedOf -> {}];
	AppendTo[returnList, GaugeEigenstate -> False];
	AppendTo[returnList, Conjugated -> False];
	AppendTo[returnList, Indices -> Cases[GetRule[p, Indices], Index[i_] :> i]];
	AppendTo[returnList, Definitions -> GetRule[p, Definitions]];
	AppendTo[returnList, Gen -> GetGenerationNb[p]];
	AppendTo[returnList, GenIndexPos -> GetGenIndexPos[p]];
	Return[returnList]
]


ReadFermion[f_] := Block[{returnF = ReadParticle[f]},
	returnF = Append[returnF, Spinor -> If[TrueQ[GetRule[f, SelfConjugate]], Majorana, If[GetRule[f, Chirality] =!= {}, Weyl, Dirac]]];
	returnF = Append[returnF, WriteInFile -> False]; (* May be set to true after Yukawas are evaluated *)
	
	Return[returnF];
]


ReadScalar[s_] := Block[{returnS = ReadParticle[s]},
	returnS = Insert[returnS, RealFields -> "[]", 2];
	returnS = Insert[returnS, Norm -> 1/Sqrt[2], 3];
	returnS = Insert[returnS, Complex -> !GetRule[s, SelfConjugate], 4];
	
	Return[returnS];
]


(* ::Subsubsection::Closed:: *)
(*SameQuantumNumbersQ*)


SameQuantumNumbersQ[pList_List] := (Length[DeleteDuplicates[Select[#, MatchQ[HoldPattern[a_ -> b_] /; MemberQ[MR$GaugeGroupList, a]]]& /@ pList]] == 1);


(* ::Subsubsection::Closed:: *)
(*SplitDiracFermions*)


SplitDiracFermions[fList_] := Block[{i, diracNotUsed, newfList = fList, posInList},
	
	(* Handle possible spectator fermions *)
	diracNotUsed = Select[fList, (GetRule[#, UsedIn] == {} && GetRule[#, ComposedOf] == {} && GetRule[#, GaugeEigenstate] == False && GetRule[#, Spinor] === Dirac)&];

	For[i=1, i<=Length[diracNotUsed], i++,
		posInList = Flatten[Position[ Name /. newfList , GetRule[diracNotUsed[[i]], Name]]][[1]];

		newfList = Insert[newfList, newfList[[posInList]], posInList];
		newfList[[posInList]] = SetRules[newfList[[posInList]], {Name, GaugeEigenstate}, {Symbol[ToString[GetRule[newfList[[posInList]], Name]] <> "L"], True}];
		newfList[[posInList+1]] = SetRules[newfList[[posInList+1]], {Name, GaugeEigenstate}, {Symbol[ToString[GetRule[newfList[[posInList+1]], Name]] <> "R"], True}];
	];
	
	Return[newfList]
]


(* ::Subsubsection::Closed:: *)
(*SplitGenerationsScalars*)


SplitGenerationsScalars[lagParts_, lagParts2_] := Block[{removeInds, newScalars, newL, genScalars, i, repRules, indices, coeff, genS, newTerm, tuples, newCoeffs, newCoeff, pos, sub},
	newScalars = {};
	newCoeffs = {};
	newL = {{}, {}};
	
	genScalars = {GetRule[#, Name], GetRule[#, Gen], GetRule[#, GenIndexPos]}& /@ Select[PR$Scalars, GetRule[#, Gen] != 1 &];
	
	removeInds[f_, indPos_, removeAllInds_] := Block[{inds, newInds, newF},
		inds = (List @@ f)[[Flatten[{indPos}]]];
		newInds = If[!removeAllInds, Select[List @@ f, !MemberQ[inds, #]&], {}];
		newF = ToExpression[ToString[Head@f] <> StringRiffle[ToString /@ inds, ""]];
		If[Length@newInds > 0, Return[newF @ (Sequence @@ newInds)]];
		Return[newF];
	];
	
	Do[
		repRules = {};
		indices = {};
		Do[
			genS = Cases[term, s_[i__] /; MemberQ[genScalars[[All, 1]], s] :> ({s[i], #[[2]], #[[3]]}& @ Extract[genScalars, Position[genScalars, s][[1,1]]])];
			If[genS === {}, AppendTo[newL[[iLag]], term]; Continue[]];
			indices = {GetIndexName[#[[1]], #[[3]]], #[[2]]}& /@ genS;
			coeff = Select[List @@ term, !MemberQ[genS[[All, 1]], #] && MemberQ[Flatten[PR$Parameters[[All,1]]], Head@#]&];
			If[Length@coeff > 1,
				Print["Unable to identify the coupling in term ", term];
				Continue[];
			];
			coeff = {coeff[[1]], Flatten[Position[List @@ coeff[[1]], #] & /@ indices[[All, 1]]]};
			tuples = Tuples[Range[#] & /@ genS[[All, 2]]];
			repRules = Function[{t}, MapThread[#[[1, #[[3]]]] -> #2&, {genS, t}]] /@ tuples;
			
			(* Prepare the lists for the definition of new coeffs & scalars *)
			If[!MemberQ[newCoeffs[[All,1]], Head @ coeff[[1]]],
				newCoeffs = Append[newCoeffs, {Head@coeff[[1]], coeff[[2]], removeInds[#, coeff[[2]], True]& /@ DeleteDuplicates[coeff[[1]] /. repRules]}];
			];
			Do[
				If[!MemberQ[newScalars[[All,1]], Head @ s[[1]]],
					newScalars = Append[newScalars, {Head@s[[1]], s[[3]], removeInds[#, s[[3]], True]& /@ DeleteDuplicates[s[[1]] /. repRules]}];
				];
			,{s, genS}];
			
			newTerm  = (# /. x_[inds__] /; MemberQ[Append[Head /@ genS[[All,1]], Head@coeff[[1]]], x] :> removeInds[x[inds], Flatten[Position[{inds}, _Integer]], False])& /@ (term /. repRules);
			newL[[iLag]] = Join[newL[[iLag]], newTerm];
		
		, {term, {lagParts, lagParts2}[[iLag]]}];
		(*AppendTo[newL, lag /. repRules];*)
	,{iLag, {1,2}}];
	
	Do[
		pos = Position[PR$Parameters[[All, 1]], nc[[1]]][[1, 1]];
		sub = ToExpression[StringReplace[ToString@#, c_ /; !DigitQ[c] -> ""]]&;
		coeff = PR$Parameters[[pos, 2]];
		Function[{c}, AppendTo[PR$Parameters, c == SetRules[coeff, {Indices, TeX}, {Delete[GetRule[coeff, Indices], Transpose@{nc[[2]]}], If[# =!= {}, Subscript[#, sub[c]], {}]& @ GetRule[coeff, TeX]}]]] /@ nc[[3]];
		PR$Parameters = Drop[PR$Parameters, {pos}];
	,{nc, newCoeffs}];
	
	Do[
		pos = Position[Name /. PR$Scalars, ns[[1]]][[1, 1]];
		sub = ToExpression[StringReplace[ToString@#, c_ /; !DigitQ[c] -> ""]]&;
		genS = PR$Scalars[[pos]];
		Function[{s}, AppendTo[PR$Scalars, SetRules[genS, {Name, Gen, GenIndexPos, Indices, TeX}, {s, 1, 0, Drop[GetRule[genS, Indices], {ns[[2]]}], If[# =!= {}, Subscript[#, sub[s]], {}]& @ GetRule[genS, TeX]}]]] /@ ns[[3]];
		(ScalarFieldQ[#] := True; If[Complex /. genS, HC[#] := ToExpression[ToString @ # <> "bar"], HC[#] := #]; HC[ToExpression[ToString @ # <> "bar"]] = #;)& /@ ns[[3]];
		PR$Scalars = Drop[PR$Scalars, {pos}];
	,{ns, newScalars}];
	
	Return[{newL[[1]], newL[[2]]}];
];


(* ::Subsubsection::Closed:: *)
(*UpdateUsedIn*)


(* ::Text:: *)
(*Update the particle's rule UsedIn*)


UpdateUsedIn[p_, compositeP_] := SetRule[p, UsedIn, Append[GetRule[p, UsedIn], GetRule[compositeP, Name]]]








(* ::Subsection::Closed:: *)
(*Yukawa-related functions*)


(* ::Subsubsection::Closed:: *)
(*SingleGenerationYukawas*)


(* ::Text:: *)
(*Find the list of fermions having single-generation couplings and propagates it.*)
(*Find the list of associated Yukawa couplings.*)
(**)
(*Returns { A  ,  B } where :*)
(*	-> A is an updated PR$Fermions list where fermions have been split*)
(*	-> B is an updated PR$Yukawas list where Yukawas have been split*)


SingleGenerationYukawas[yukList_] := Block[{i, pos, singleFermionsList = {}, allSingleFermions, primaryYukToSplit = {}, secondaryYukToSplit, newYuk},
	(* Find fermions with integer generation-index *)
	
	Do[
		pos = Position[GetIndicesNames /@ GetRule[yuk, Fermions], int_Integer];
		If[pos != {},
			primaryYukToSplit = Join[primaryYukToSplit, Position[yukList, yuk]];
			singleFermionsList = Join[singleFermionsList, Extract[GetRule[yuk, Fermions], {First[#], 0}]& /@ pos];
		]
	, {yuk, yukList}];
	
	allSingleFermions = singleFermionsList;
	(* Find all fermions associated with previously listed fermions : they will be split in single-generation fermions *)

	allSingleFermions = NestWhile[(Join[DeleteDuplicates[DeleteCases[ Head /@ Flatten[(Fermions /. Cases[yukList, y_ /; MemberQ[Head /@ (Fermions /. y), #] ])& /@ #], fn_ /; (MemberQ[#, fn] || GetGenerationNb[ParticleFromName[fn]] == 1)]], #])&, allSingleFermions, (#1 =!= #2)&, 2] ;
	
	secondaryYukToSplit = Position[yukList, y_ /; (IntersectingQ[Head /@ (Fermions /. y), allSingleFermions]), {1}, Heads->False];
	
	GenIndexToName[f_, ind__] := Block[{iPos, iValue},
		iPos = GetGenIndexPos[ParticleFromName[f]];
		iValue = {ind}[[iPos]];
		Return[ToExpression[ToString[f] <> ToString[iValue]][Sequence @@ Delete[{ind}, iPos]]];
	];
		
	newYuk = yukList;
	For[i=1, i<=Length[yukList], i++,
		If[MemberQ[Flatten[primaryYukToSplit], i], 
			newYuk = Replace[newYuk, yukList[[i]] :> Append[yukList[[i]], Split->True], {1}];
		];
		If[MemberQ[Flatten[secondaryYukToSplit], i] && !MemberQ[Flatten[primaryYukToSplit], i], 
			newYuk = Replace[newYuk, yukList[[i]] :> Sequence @@ SplitYukawa[Append[yukList[[i]], Split->True]], {1}];
		];
	];
	
	For[i=1, i<=Length[newYuk], i++,
		If[(Split /. newYuk[[i]]) === True, 
			newYuk[[i]] = SetRule[newYuk[[i]], Fermions, (Fermions /. newYuk[[i]]) /. f_[ind__] /; MemberQ[allSingleFermions, f] :> GenIndexToName[f, ind] ];
			newYuk[[i]] = SetRule[newYuk[[i]], Fields, If[Head[#] =!= Symbol, Head[#], #]& /@ Append[Fermions /. newYuk[[i]], Scalar /. newYuk[[i]]]];
		];
	];
	
	(* Split Fermions *)
	
	(* Split yukawas *)
	(* Handling of primary 1-gen fermions is different than secondary ones.  \[Rule] Have to replace uR[3] with uR3 *)
	
	Return[{ Replace[PR$Fermions, f_ /; MemberQ[allSingleFermions, (Name /. f)] :> SplitFermion[f], {1}],
			newYuk}];
];


(* ::Subsubsection::Closed:: *)
(*SplitFermion*)


SplitFermion[f_] := Sequence @@ ( SetRules[f, {Name, Gen, GenIndexPos, Indices}, {ToExpression[ToString[Name /. f] <> ToString[#]], 1, 0, Select[Indices /. f, GaugeGroupIndexQ[#]&]}]& /@ Range[GetGenerationNb[f]]);


(* ::Subsubsection::Closed:: *)
(*SplitYukawa*)


SplitYukawa[yuk_] := Block[{indList, tuplesList = {}, ipos, pp, iRange, newTerm},
	indList = GetIndicesNames[GetRule[yuk, Coeff]];
	If[indList == {}, Return[yuk]];
	
	Do[	
		(* Try and guess the range of the index by looking at the fermion carrying it *)
		iRange = Cases[yuk, p_ /; MatchQ[p, _[___, currentIndex, ___]] && FermionQ[p] :> 
				(pp = ParticleFromName[Head[p], PR$Fermions]; ipos = Flatten[Position[GetIndicesNames[p], currentIndex]]; If[Length[ipos]==1 && ipos[[1]]==GetRule[pp, GenIndexPos][[1]], GetRule[pp, Gen][[1]]]), -1]; 
	
		iRange = DeleteDuplicates[iRange];
		If[Length[iRange] == 1, iRange = iRange[[1]], Print["Could not guess the range of the index ", currentIndex]];
	
		tuplesList = Append[tuplesList, (currentIndex -> #)& /@ Range[iRange]];
	, {currentIndex, indList}];
	(* tuplesList = Tuples[tuplesList] /. HoldPattern[ii_ \[Rule] nn_] \[RuleDelayed] Sequence @@ {ii \[Rule] nn, s_String /; StringContainsQ[s, ToString[ii]] \[RuleDelayed] StringReplace[s, ToString[ii] \[RuleDelayed] ToString[nn]]}; *)
	
	tuplesList = Tuples[tuplesList];
	tuplesList = Function[{el}, Append[el, ToExpression["s_String :> StringReplace[s, {" <> StringRiffle[("\"" <> ToString[#[[1]]] <> "\" -> \"" <> ToString[#[[2]]] <> "\"")& /@ Cases[el, HoldPattern[i_ -> int_Integer] :> {i, int}], ", "] <> "}]"]]] /@ tuplesList;
	
	Return[SetRule[#, TeX, If[Head[TeX /. #] === String, ToExpression[TeX /. #, TeXForm], TeX /. #]]& /@ (SetRule[yuk, TeX, "(" <> ToString[TeX /. yuk, TeXForm] <> ")_" <> ToString[indList, InputForm]] /. tuplesList)]
]


(* ::Subsubsection::Closed:: *)
(*YukawaConjugate + YukawaConjugateTranspose*)


(* YukawaConjugate[yuk_] := {Norm -> (Norm /. yuk), Coeff -> YukawaConjugateTranspose[Coeff /. yuk], Scalar -> GetConjugate[Scalar /. yuk], Fermions -> Reverse[GetConjugate /@ (Fermions /. yuk)], OtherPieces -> (HC /@ (OtherPieces /. yuk)), TeX -> ToExpression["(" <> ToString[(TeX /. yuk) // TeXForm] <> ")^\\dagger", TeXForm], ChargeConjugated->(Reverse[GetConjugate /@ (ChargeConjugated /. yuk)])}; *)
YukawaConjugate[yuk_] := {Norm -> (Norm /. yuk), Coeff -> YukawaConjugateTranspose[Coeff /. yuk], Scalar -> GetConjugate[Scalar /. yuk], Fermions -> Reverse[GetConjugate /@ (Fermions /. yuk)], OtherPieces -> (HC /@ (OtherPieces /. yuk)), TeX -> ToExpression["(" <> ToString[(TeX /. yuk) // TeXForm] <> ")^\\dagger", TeXForm]};


YukawaConjugateTranspose[yukMat_] := HC[Head[yukMat]] @@ Reverse[yukMat];


(* ::Subsection::Closed:: *)
(*Misc*)


(* ::Subsubsection::Closed:: *)
(*Messages*)


Options[PR$Messages] = {VerboseLevel -> 1, Dynamic->False};
PR$Messages[message_, vLevel_, OptionsPattern[]] := (If[vLevel <= OptionValue[VerboseLevel], If[OptionValue[Dynamic] == False, Print[ReleaseHold @ message], Print[Dynamic[ReleaseHold @ message]]]]);


(* ::Subsubsection::Closed:: *)
(*GetRule*)


(* ::Text:: *)
(*Examples : GetRule[particle, Indices] ; GetRule[group, Representations] ...*)
(*Returns { } if no such rule in list.*)


Clear@GetRule;
GetRule[list_, rule_] := Block[{ret},
	If[Head @ list =!= List, Return[{}]];
	ret = rule /. list;
	If[ret === rule,
		Return[{}];
	];
	Return[rule /. list];
];


(* ::Subsubsection::Closed:: *)
(*SetRule / SetRules*)


(* ::Text:: *)
(*Affect a new value to a rule in list x.*)
(*Affect a new list of values to a list of rules of x.*)


SetRule[x_, rule_, newValue_] := Replace[x, HoldPattern[rule -> _] :> (rule -> newValue), 1]
SetRules[x_, rules_List, newValues_List] := Replace[x, MapThread[(HoldPattern[#1 -> _] :> (#1 -> #2))&, {rules, newValues}], 1]


(* ::Section::Closed:: *)
(*Getting info from FR model*)


(* ::Subsection::Closed:: *)
(*GetGaugeGroups*)


(* ::Text:: *)
(*Return the list of gauge groups*)
(*[ TO-DO: remove the do loop ]*)


GetGaugeGroups[usePyReps_:False] := Block[{ret, gCouple, gName, gp, pyGp, tag, reps, pyDefs, dynkins, repMatsPos, newDef},
	PR$Messages["Reading gauge groups...", 1];
	ret = {};
	Do[
		{gName, gp} = List @@ gCouple;
		pyGp = {Name -> gName};
		pyGp = Append[pyGp, Abelian -> GetRule[gp, Abelian]];
		pyGp = Append[pyGp, GaugeBoson -> GetRule[gp, GaugeBoson]];
		pyGp = Append[pyGp, CouplingConstant -> GetRule[gp, CouplingConstant]];
		reps = GetRule[gp, Representations];
		pyGp = Append[pyGp, Representations -> reps[[All,1]]];
		pyGp = Append[pyGp, Indices -> reps[[All, 2]]];
		pyGp = Append[pyGp, Charge -> GetRule[gp, Charge]];
		pyGp = Append[pyGp, Definitions -> GetRule[gp, Definitions]];
		
		tag = GetRule[gp, PyrateTag];
		If[tag == {},
			If[(Abelian /. pyGp) === True, tag = "U1",
				Print["Error : Gauge group '", gName, "' has no PyrateTag."];
				Abort[];
			];
		,
			tag = ToString @ tag; 
		];
		pyGp = Append[pyGp, Tag -> tag];
		
		(* Now getting some info from PyLie's DB *)
		If[!(Abelian /. pyGp),
			pyGp = Append[pyGp, Rank -> GetRank[tag]];
			pyGp = Append[pyGp, Dimension -> GetDimension[tag]];
			pyGp = Append[pyGp, Adjoint -> GetAdjointRep[tag]];
			
			(* Use the same gauge generators & structure constants as PyLie \[Rule] Read them from the DB *)
			If[usePyReps,
				reps = Representations /. pyGp;
				pyDefs = {};
				Do[
					If[i != Length[reps],
						(* Representation matrices *)
						dynkins = GetDynkinLabels[Tag /. pyGp, GetRepresentation[(Indices /. pyGp)[[i]]], OptionValue[WritePyRATE, RealBasis]];
						repMatsPos = GetRepMat[Tag /. pyGp, dynkins, "adjoint", True];
						pyDefs = Append[pyDefs, ToExpression["(" <> ToString[reps[[i]]] <> "[a_,b_,c_] :> PR$DBStoreRepMats[[" <> ToString[repMatsPos] <> ", 2, a, b, c]])"]];
					,
						(* Structure constants *)
						dynkins = GetDynkinLabels[Tag /. pyGp, GetRepresentation[(Indices /. pyGp)[[i]]], OptionValue[WritePyRATE, RealBasis]];
						repMatsPos = GetStructureConstants[Tag /. pyGp, True];
						pyDefs = Append[pyDefs, ToExpression["(" <> ToString[reps[[i]]] <> "[a_,b_,c_] :> PR$DBStoreRepMats[[" <> ToString[repMatsPos] <> ", 2, a, b, c]])"]];
					];
				,{i, Length[reps]}];
				pyGp = SetRule[pyGp, Definitions, pyDefs];
			];
		];
		ret = Append[ret, pyGp];
	, {gCouple, MR$GaugeGroups}];
	Return @ ret;
];


(* ::Subsection::Closed:: *)
(*GetFermions*)


GetFermions[] := Block[{FRfList, fList},
	PR$Messages["Reading fermions ...", 1];
	
	FRfList = Cases[MR$ClassesDescription, HoldPattern[(F|W)[_?IntegerQ] -> x_] :> x];
	fList = ReadFermion[#]& /@ FRfList;
	fList = FindGaugeEigenstates[fList, FRfList];
	
	Return[fList]
];


(* ::Subsection::Closed:: *)
(*GetScalars*)


GetScalars[] := Block[{FRsList, sList}, 
	PR$Messages["Reading scalars ...", 1];
	
	FRsList = Cases[MR$ClassesDescription, HoldPattern[S[_?IntegerQ] -> x_] :> x];
	sList = ReadScalar /@ FRsList;
	sList = FindGaugeEigenstates[sList, FRsList];
	
	Return[sList]
]


(* ::Subsection::Closed:: *)
(*GetLagTerms*)


(* ::Text:: *)
(*Goes through all the terms of the Lagrangian a single time, and identifies Yukawas, Quartics, Trilinears, Scalar masses*)


GetLagTerms[lagParts_, listSnames_, listFnames_] := Block[{returnList, termList, Snames, Fnames, majoranaRepRules, fList, sList, otherBosonsList, chargeConj},
	If[lagParts === {} || lagParts === {0}, Return[{}]];
	
	termList = Function[{term}, Flatten[ ( If[Head[#] === Dot, List @@ #, #]& /@ (List @@ term) ) /. (Power[f_, n_] /; !MemberQ[ PR$Parameters[[All, 1]], f] && IntegerQ[n] :> Table[f, n])]] /@ lagParts;
	returnList = {};
	
	Snames = If[listSnames === Name, {}, listSnames];
	Fnames = DeleteDuplicates[Join[If[listFnames === Name, {}, listFnames], GetConjugate /@ If[listFnames === Name, {}, listFnames]]];
	majoranaRepRules = (ToExpression[ToString[#]<>"bar"] -> #)& /@ Select[Fnames, (Spinor /. Flatten[ParticleFromName[#]]) === Majorana &];
	
	Do[
		term = term /. majoranaRepRules;
		sList = Cases[term, f_ /; (ScalarFieldQ[f] === True && GaugeEigenstateQ[f] === True)];
		(* fList = Cases[term(* /. CC -> Identity*), (f_ | CC[f_] | CC[f_][__]) /; (MemberQ[Fnames, Head[f]] || MemberQ[Fnames, f] || MemberQ[Fnames, GetConjugate[Head[f]]] || MemberQ[Fnames, GetConjugate[f]])]; *)
		fList = Cases[term, f_  /; (MemberQ[Fnames, Head[f]] || MemberQ[Fnames, f] || MemberQ[Fnames, GetConjugate[Head[f]]] || MemberQ[Fnames, GetConjugate[f]])];
		
		otherBosonsList = Cases[term, f_ /; (!MemberQ[sList, f] && BosonQ[f])];
		
		(*AppendTo[returnList, If[# =!= {}, Append[# /. CC->Identity, ChargeConjugated -> (Cases[#, (CC[f_] | CC[f_][i__]) :> If[i =!= Nothing, f[i], f], -1]& @ #)], #]& /@ *)
		AppendTo[returnList,   {FindYukawas[term, sList, fList, otherBosonsList],
								FindQuartics[term, sList, otherBosonsList],
								FindTrilinears[term, sList, otherBosonsList],
								FindScalarMasses[term, sList, otherBosonsList],
								FindFermionMasses[term, fList, sList, otherBosonsList]}];
	
	, {term, termList}];

	Return[returnList];
]



(* ::Subsection::Closed:: *)
(*GetDefinitions*)


GetDefinitions[] := Block[{allDefs = {}, reps},
	(* Define the rep matrices used in the Lagrangian *)
	reps = DeleteDuplicates @ Flatten[GetRule[#, RepMats]& /@ Join[PR$Yukawas, PR$Quartics, PR$Trilinears, PR$ScalarMasses, PR$FermionMasses]];
	reps = {#, GaugeGroupFromIndex[GetIndicesTypes[#][[-1]]], GetRepresentation@GetIndicesTypes[#][[-1]]} & /@ reps;
	allDefs = Join[allDefs, {ToString[#[[1]]], "t(" <> ToString[Name /. #[[2]]] <> ", " <> StringReplace[ToString[GetDynkinLabels[Tag /. #[[2]], #[[3]], OptionValue[WritePyRATE, RealBasis]]], {"{"->"[", "}"->"]"}] <> ")"}& /@ reps];
	
	(* Read CGCs *)
	allDefs = Join[allDefs, MapThread[{"C" <> ToString[#1] , "cgc(" <> StringTake[StringReplace[ToString[#2], {"{"->"[", "}"->"]"}], {2,-2}] <> ")"}&, {Range[Length[PR$CGCs]], PR$CGCs}]];
	
	Return[allDefs];
]


(* ::Subsection::Closed:: *)
(*GetYukawas*)


GetYukawas[lagTerms_] := Block[{yukList, returnList},
	If[Flatten @ lagTerms[[All, 1]] == {}, Return[{}]];
	PR$Messages["  --> Yukawa couplings ...", 1];
	yukList = Select[lagTerms[[All, 1]], (# =!= {})&];
	
	Return[HandleExpandedTerms @ yukList];
]


(* ::Subsection::Closed:: *)
(*GetQuartics*)


GetQuartics[lagTerms_] := Block[{},
	If[Flatten @ lagTerms[[All, 2]] == {}, Return[{}]];
	PR$Messages["  --> quartic couplings ...", 1];
	Return[HandleExpandedTerms @ Select[lagTerms[[All, 2]], (# =!= {})&]]
]


(* ::Subsection::Closed:: *)
(*GetTrilinears*)


GetTrilinears[lagTerms_] := Block[{},
	If[Flatten @ lagTerms[[All, 3]] == {}, Return[{}]];
	PR$Messages["  --> trilinear couplings ...", 1];
	Return[HandleExpandedTerms @ Select[lagTerms[[All, 3]], (# =!= {})&]]
]


(* ::Subsection::Closed:: *)
(*GetFermionMasses*)


GetFermionMasses[lagTerms_] := Block[{},
	If[Flatten @ lagTerms[[All, 5]] == {}, Return[{}]];
	PR$Messages["  --> fermion mass couplings ...", 1];
	Return[HandleExpandedTerms @ Select[lagTerms[[All, 5]], (# =!= {})&]]
]


(* ::Subsection::Closed:: *)
(*GetScalarMasses*)


GetScalarMasses[lagTerms_] := Block[{},
	If[Flatten @ lagTerms[[All, 4]] == {}, Return[{}]];
	PR$Messages["  --> scalar mass couplings ...", 1];
	Return[HandleExpandedTerms @ Select[lagTerms[[All, 4]], (# =!= {})&]]
];


(* ::Subsection::Closed:: *)
(*GetSubstitutions*)


GetSubstitutions[] := Block[{allDefs = {}, gaugeCouplings},
	(* Gauge couplings *)
	gaugeCouplings = {"g_" <> ToString @ GetRule[#, Name], GetRule[#, CouplingConstant]}& /@ PR$GaugeGroups;
	allDefs = Join[allDefs, gaugeCouplings];

	Return[allDefs]
]


(* ::Subsection::Closed:: *)
(*GetLatex*)


GetLatex[tex_:{}] := Block[{userTex = {}, allTex = {}, greek, cGreek, greekReplacements, gaugeCouplings, particles, couplings, Tmp},
	(* User-provided TeX replacements *)
	If[tex =!= {},
		userTex = {ToString@#[[1]], If[Head@#[[2]] === String, #[[2]], ToString@TeXForm@#[[2]]]}& /@ (List @@@ tex);
	];
	
	greek = {"Alpha","Beta","Gamma","Delta","Epsilon","Zeta","Eta","Theta","Iota","Kappa","Lambda","Mu","Nu","Xi","Omicron","Rho","Sigma","Tau","Upsilon","Phi","Chi","Psi","Omega"};
	cGreek = "Capital" <> # & /@ greek;
	greekReplacements = ToString@ToExpression["\\[" <> # <> "]"] -> "\\" <> If[StringContainsQ[#, "Capital"], StringReplace[#, "Capital"->""], ToLowerCase[#]] & /@ Join[greek, cGreek];

	
	(* Gauge couplings *)
	gaugeCouplings = If[GetRule[ParameterFromName[#], TeX] =!= {}, {#, GetRule[ParameterFromName[#], TeX]}, Nothing]& /@ (GetRule[#, CouplingConstant]& /@ PR$GaugeGroups);
	allTex = Join[allTex, {#[[1]], ToString@TeXForm[#[[2]]]}& /@ gaugeCouplings];
	
	(* Particles *)
	particles = Select[Join[PR$Fermions, PR$Scalars], GaugeEigenstateQ[#] && !GetRule[#, Conjugated] &];
	allTex = Join[allTex, {GetRule[#, Name], ToString @ GuessParticleLatex[#]}& /@ particles];

	(* CGCs *)
	allTex = Join[allTex, {"C" <> ToString@#, "C_{" <> ToString@# <> "}"}& /@ Range[Length[PR$CGCs]]];
	
	(* Couplings *)
	couplings = If[GetRule[#, TeX] =!= {}, {GetRule[#, Coeff], GetRule[#, TeX]}, {GetRule[#, Coeff], GetRule[#, Coeff], 0}]& /@ Join[PR$Yukawas, PR$Quartics, PR$Trilinears, PR$ScalarMasses, PR$FermionMasses];
	couplings = {If[Head@#[[1]] =!= Symbol && Head@#[[1]] =!= Re && Head@#[[1]] =!= Im, Head@#[[1]], #[[1]]],
				 If[Length@# == 2, StringReplace[ToString[If[Head@#[[2]] =!= Subsuperscript, TeXForm @ #[[2]], (ToString[TeXForm @ #1] <> "_{" <> ToString[#2] <> "}^{" <> ToString[#3] <> "}")& @ (Sequence@@#[[2]])]], " "->""], ToString@#[[2]]], If[Length[#]==3, 0, Nothing]}& /@ couplings;
	couplings =  If[Head@#[[1]] === Re, {#[[1]], "{"<>If[Length@#!=3, #[[2]], ToString@(Identity@@#[[1]])]<>"}^r"}, If[Head@#[[1]] === Im, {#[[1]], "{"<>If[Length@#!=3, #[[2]], ToString@(Identity@@#[[1]])]<>"}^i"}, If[Length[#] =!= 3, #, Nothing]]]& /@ couplings;
	couplings[[All, 2]] = StringReplace[#, {"\\text" -> "", "$"->""}]& /@ couplings[[All, 2]];

	allTex = Join[allTex, couplings];
	allTex[[All, 2]] = StringReplace[#, greekReplacements]& /@ allTex[[All, 2]];
	
	(* Combine userTex and allTex, giving priority to userTex *)
	allTex = Join[userTex, If[!MemberQ[userTex[[All,1]], ToString@#[[1]]], #, Nothing]& /@ allTex];
	
	Return[allTex]
]


GuessParticleLatex[p_] := Block[{greek, fermionQ, strName, subscripts={}, extractNb, capital, ret},
	greek = ToLowerCase /@ {"Alpha","Beta","Gamma","Delta","Epsilon","Zeta","Eta","Theta","Iota","Kappa","Lambda","Mu","Nu","Xi","Omicron","Rho","Sigma","Tau","Upsilon","Phi","Chi","Psi","Omega"};
	fermionQ = (GetRule[p, Spinor] =!= {});
	strName = ToString @ GetRule[p, Name];
	
	If[True(*fermionQ*),
		If[StringTake[strName, -1] === "L",
			AppendTo[subscripts, "L"];
			strName = StringTake[strName, {1, -2}];
		];
		If[StringTake[strName, -1] === "R",
			AppendTo[subscripts, "R"];
			strName = StringTake[strName, {1, -2}];
		];
	];
	
	extractNb = StringCases[strName, s_ ~~ n:NumberString :> n];
	If[extractNb =!= {},
		AppendTo[subscripts, extractNb[[1]]];
		strName = StringTake[strName, {1, -1-StringLength[extractNb[[1]]]}]
	];
	
	If[MemberQ[greek, ToLowerCase[strName]],
		capital = True;
		If[ToLowerCase[StringTake[strName, 1]] === StringTake[strName, 1],
			capital = False;
			strName = Capitalize[strName];
		];
		strName = TeXForm @ ToExpression["\\[" <> If[capital, "Capital", ""] <> strName <> "]"];
	];
	
	ret = strName;
	If[subscripts =!= {},
		ret = StringReplace[ToString@ret, " "->""] <> "_{" <> StringRiffle[subscripts, ", "] <> "}";
	];
	
	Return[ret]
]


(* ::Subsection::Closed:: *)
(*GetUFOMapping*)


GetUFOMapping[] := Block[{mappingList = {}, ParamMapping, iNames, iRanges, allElements, allValues, gaugeSubs},
	(* Coupling constants *)
	mappingList = Join[mappingList, #]& @ ( ("g_"<>ToString[GetRule[#, Name]] -> 
											ToString @ (Function[{p}, If[# =!= {}, #, p]& @ GetRule[ParameterFromName[p], ParameterName]] @ GetRule[#, CouplingConstant]))& /@ PR$GaugeGroups );
	gaugeSubs = #[[1]] -> ToString@#[[2]]& /@ GetSubstitutions[];
	mappingList = mappingList /. gaugeSubs;
	
	
	ParamMapping[pList_] := Block[{},
		Do[
			If[Head@(Coeff /. p) === Conjugate, Continue[]];
			iNames = GetIndicesNames[Coeff /. p];
			iRanges = (GetIndexRange /@ GetIndicesTypes[Coeff /. p]);
			If[iRanges =!= {} && (Names /. p) === {}, Continue[]];
			allElements = (Coeff /. p) /. Tuples[ Function[{n}, (iNames[[n]] -> #)& /@ Range[iRanges[[n]]] ] /@ Range[Length[iNames]] ] ;
			allValues = If[(# /. Join[Definitions /. p, If[(Head@#) === List, #, {(Coeff /. p) -> #}]& @ (Values /. p)]) === 0, 0, # /. (Names /. p)]& /@ allElements;
			allValues = If[allValues[[#]] === allElements[[#]], StringReplace[ToString@allElements[[#]], {"["->"", "]"->"", ","->"x"," "->""}],allValues[[#]]]& /@ Range[Length[allValues]];
			mappingList = Join[mappingList, ((StringReplace[ToString[allElements[[#]] /. i_?IntegerQ :> i-1], " "->""] -> allValues[[#]])& /@ Range[Length[allElements]])];
		, {p, pList}];
	];
	
	ParamMapping[PR$Yukawas];
	ParamMapping[PR$Quartics];
	ParamMapping[PR$Trilinears];
	ParamMapping[PR$ScalarMasses];
	ParamMapping[PR$FermionMasses];
	
	mappingList = DeleteCases[mappingList, HoldPattern[a_ -> a_]];
	
	Return[mappingList];
]


(* ::Subsection::Closed:: *)
(*GetRunningCouplings*)


GetRunningCouplings[ggList_, yList_:{}, qList_:{}, tList_:{}, smList_:{}, fmList_:{}] := Block[{cList},
	(* Gauge *)
	cList = GetRule[#, CouplingConstant]& /@ ggList;

	(* Yukawa *)
	cList = Join[cList, (If[Head[#] =!= Symbol, Head@#, #]& @ GetRule[#, Coeff])& /@ yList];

	(* Quartics *)
	cList = Join[cList, GetRule[#, Coeff]& /@ qList];

	(* Scalar masses *)
	cList = Join[cList, GetRule[#, Coeff]& /@ smList];

	(* Trilinear *)
	cList = Join[cList, GetRule[#, Coeff]& /@ tList];

	(* Fermion masses *)
	cList = Join[cList, (If[Head[#] =!= Symbol, Head@#, #]& @ GetRule[#, Coeff])& /@ fmList];

	Return[cList];
];


GetRealRunningComponents[ggList_, yList_:{}, qList_:{}, tList_:{}, smList_:{}, fmList_:{}] := Block[{cList, pNameRules, internalReps},
	cList = GetRunningCouplings[ggList, yList, qList, tList, smList, fmList];

	(* Split parameters into their real components *)
	cList = Flatten[ParameterToRealComponents /@ cList];

	(* Decompose all internal parameters in terms of their External components *)
	internalReps = #[[1]] -> #[[2]]& /@ IParamList;
	cList = ComplexExpand[cList //. internalReps];

	Return[cList];
]


(* ::Section::Closed:: *)
(*Writing the model file*)


(* ::Subsection::Closed:: *)
(*WriteAll*)


WriteAll[fpath_, tex_, ggList_, fList_:{}, sList_:{}, yList_:{}, qList_:{}, tList_:{}, smList_:{}, fmList_:{}, cgcList_:{}] := Block[{fName, modelFilePath, fPath, fFolder, totalString, splitDir, overwrite=False},
	fPath = fpath;

	If[fPath == "", fPath = NotebookDirectory[], fPath = AbsoluteFileName[fPath]];

	fFolder = FileNameJoin[{fPath, StringReplace[M$ModelName, " "->"_"]}];
	fName = StringReplace[M$ModelName, " "->"_"] <> ".model";

	If[!DirectoryQ[fPath], CreateDirectory[fPath]];
	If[!DirectoryQ[fFolder], CreateDirectory[fFolder]];

	fName = FileNameJoin[{fFolder, fName}];
	If[FileExistsQ[fName],
		DeleteFile[fName];
		overwrite = True;
	];

	PR$Messages[If[overwrite, "(Over)w", "W"] <> "riting info in file : \"" <> fName <> "\" ...", 1];

	OpenWrite[fName];

	totalString = WriteHeader[];
	totalString = totalString <> WriteGaugeGroups[ggList];
	totalString = totalString <> WriteFermions[fList];
	totalString = totalString <> WriteScalars[sList];
	totalString = totalString <> "\n\nPotential: {\n\n";
	totalString = totalString <> WriteDefinitions[cgcList];
	totalString = totalString <> WriteYukawas[yList];
	totalString = totalString <> WriteQuartics[qList];
	totalString = totalString <> WriteScalarMasses[smList];
	totalString = totalString <> WriteTrilinears[tList];
	totalString = totalString <> WriteFermionMasses[fmList];
	totalString = totalString <> "\n}\n\n";

	totalString = totalString <> WriteSubstitutions[];
	totalString = totalString <> WriteLatex[tex];
	
	totalString = totalString <> WriteMapping[];

	WriteString[fName, totalString];
	Close[fName];

	Return[fName];
]


(* ::Subsection::Closed:: *)
(*WriteHeader*)


WriteHeader[] := Block[{ret},
	ret = "# YAML 1.1\n### Automatically generated by the FeynRules-PyR@TE interface ###\n";
	ret = StringJoin[ret, "## " <> ToString[M$ModelName] <> " ##\n---\n"];
	ret = StringJoin[ret, "Author: " <> StringRiffle[Authors /. M$Information, ", "] <> "\n"];
	(*strUpdated = StringJoin[ret, "Date: " <> (Date /. M$Information) <> "\n"];*)
	ret = StringJoin[ret, "Date: " <> DateString[{"Day", ".", "Month", ".", "Year"}] <> "\n"]; 
	Return @ StringJoin[ret, "Name: " <> StringReplace[M$ModelName, " " -> "_"] <> "\n"];
]


(* ::Subsection::Closed:: *)
(*WriteGaugeGroups*)


WriteGaugeGroups[ggList_] := "Groups: {" <> StringRiffle[(ToString[GetRule[#, Name]] <> ": " <> ToString[GetRule[#, Tag]]) & /@ ggList, ", "] <>"}\n";


(* ::Subsection::Closed:: *)
(*WriteFermions*)


WriteFermions[fList_] := Block[{ret}, 
	ret = "\n##############################\n#Fermions assumed weyl spinors\n##############################\n\nFermions: {";
	Return @ StringJoin[ret, StringRiffle["\n    " <> (ToString[GetRule[#, Name]] <>
						": {Gen: " <> ToString[GetRule[#, Gen]] <>
						", Qnb:{ " <> StringRiffle[ Cases[#, ( HoldPattern[gname_ -> qnb_] /; MemberQ[MR$GaugeGroupList, gname] ) :>
					  					 ToString[gname] <> ": " <> PyRep[qnb, True] ], ", "] <> "}}")& /@
					  					 Select[fList, (GetRule[#, GaugeEigenstate] && !GetRule[#, Conjugated])&],
					  					 ","] <> "\n}"];
]








(* ::Subsection::Closed:: *)
(*WriteScalars*)


WriteScalars[sList_] := Block[{ret}, 
	(* Real Scalars *)
	ret = "\n\n#############\n#Real Scalars\n#############\n\nRealScalars: {";
	ret = StringJoin[ret, StringRiffle["\n    " <> (ToString[GetRule[#, Name]] <> ": {"
								 <> StringRiffle[ Cases[#, ( HoldPattern[gname_ -> qnb_] /; MemberQ[MR$GaugeGroupList, gname] ) :>
					  					 ToString[gname] <> ": " <> PyRep[qnb,True] ], ", "] <> "}")& /@ Select[sList, (GaugeEigenstateQ[#] && !GetRule[#, Complex])&],
					  					 ","] <> "\n}"];
					  					 
	(* Complex Scalars *)
	ret = StringJoin[ret, "\n\n################"];
	ret = StringJoin[ret, "\n#Complex Scalars"];
	ret = StringJoin[ret, "\n################\n\nCplxScalars: {"];
	Return @ StringJoin[ret, StringRiffle["\n    " <> (ToString[GetRule[#, Name]] <>
							": {RealFields: " <> StringReplace[ToString[GetRule[#, RealFields] /. I -> 1, InputForm] , {"{" -> "[", "}" -> "]", "(" -> "", ")" -> ""}] <> 
							", Norm: " <> StringReplace[ToString[GetRule[#, Norm], InputForm] , {"[" -> "(", "]" -> ")"}] <>
							", Qnb:{ " <> StringRiffle[ Cases[#, ( HoldPattern[gname_ -> qnb_] /; MemberQ[MR$GaugeGroupList, gname] ) :>
					  					 ToString[gname] <> ": " <> PyRep[qnb,True] ], ", "] <> "}}")& 
					  					 /@ Select[sList, (GaugeEigenstateQ[#] && GetRule[#, Complex] && !GetRule[#, Conjugated])&],
					  					 ","] <> "\n}"];
]





(* ::Subsection::Closed:: *)
(*WriteDefinitions*)


WriteDefinitions[cgcs_] := Block[{ret},
	ret = "Definitions: {\n";
	ret = ret <> StringRiffle[("    " <> #[[1]] <> " : " <> #[[2]])& /@ GetDefinitions[], ",\n"];
	ret = ret <> "\n},\n\n";
	
	Return[ret];
]


(* ::Subsection::Closed:: *)
(*WriteYukawas*)


WriteYukawas[yList_] := Block[{ret},
	ret = "Yukawas: {";
	
	Return @ StringJoin[ret, StringRiffle[("\n    " <> If[(Head@#) =!= Conjugate, ToString[Head@#], ToString[Head@@#]<>"star"]& @ GetRule[#, Coeff] <>  " : "
									<> Function[{expr}, If[GetRule[#, Assumptions] =!= {}, "{" <> expr <> ", " <> StringRiffle[GetRule[#, Assumptions], ", "] <> "}", expr]] @ ExprToString[GetRule[#, FinalExpr], GetRule[#, Fields]] 
									)& /@ yList, ","] <> "\n},\n"];
]


(* ::Subsection::Closed:: *)
(*WriteScalarTerm*)


WriteScalarTerm[sList_] := Block[{},
	Return[StringRiffle[("\n    " <> If[(Head@#) =!= Conjugate, ToString[#], ToString[# /. Conjugate->Identity]<>"star"]& @ GetRule[#, Coeff]
									<>  " : " <> Function[{expr}, If[GetRule[#, Squared] === True, "{" <> expr <> ", squared}", expr]] @ ExprToString @ GetRule[#, FinalExpr]
									)& /@ sList, ","] <> "\n},\n"];
]


(* ::Subsection::Closed:: *)
(*WriteQuartics*)


WriteQuartics[qList_] := Block[{ret},
	ret = "\nQuarticTerms: {";
	Return @ StringJoin[ret, WriteScalarTerm[qList]];
]


(* ::Subsection::Closed:: *)
(*WriteScalarMasses*)


WriteScalarMasses[smList_] := Block[{ret},
	ret = "\nScalarMasses: {";
	Return @ StringJoin[ret, WriteScalarTerm[smList]];
]


(* ::Subsection::Closed:: *)
(*WriteTrilinears*)


WriteTrilinears[tList_] := Block[{ret},
	ret = "\nTrilinearTerms: {";
	Return @ StringJoin[ret, WriteScalarTerm[tList]];
]


(* ::Subsection::Closed:: *)
(*WriteFermionMasses*)


WriteFermionMasses[fmList_] := Block[{ret},
	ret = "\nFermionMasses: {";
	Return @ StringJoin[ret, StringRiffle[("\n    " <> If[(Head@#) =!= Conjugate, ToString[Head@#], ToString[Head@@#]<>"star"]& @ GetRule[#, Coeff]
									<>  " : " <> Function[{expr}, If[GetRule[#, Assumptions] =!= {}, "{" <> expr <> ", " <> StringRiffle[GetRule[#, Assumptions], ", "] <> "}", expr]] @ ExprToString[GetRule[#, FinalExpr], GetRule[#, Fields]]
									)& /@ fmList, ","] <> "\n},\n"];
]


(* ::Subsection::Closed:: *)
(*WriteSubstitutions*)


WriteSubstitutions[] := Block[{ret},
	ret = "Substitutions: {\n";
	ret = StringJoin[ret, StringRiffle[("    " <> ToString[#[[1]]] <> " : " <> ToString @ #[[2]])& /@ GetSubstitutions[], ",\n"]];
	ret = ret <> "\n}\n\n";
	Return@ret;
]


(* ::Subsection::Closed:: *)
(*WriteLatex*)


WriteLatex[tex_] := Block[{ret},
	ret = "Latex: {\n";
	ret = StringJoin[ret, StringRiffle[("    " <> ToString[#[[1]]] <> " : " <> ToString @ #[[2]])& /@ GetLatex[tex], ",\n"]];
	ret = ret <> "\n}\n";
	Return@ret;
]


(* ::Subsection::Closed:: *)
(*WriteMapping*)


WriteMapping[] := Block[{mapping, ret},
	mapping = GetUFOMapping[];
	If[mapping == {}, Return[""]];
	
	ret = "\n\nUFOMapping: {" <>
			StringReplace[StringRiffle[StringReplace[""<>ToString[#]<>"", {" "->"", "->"->": "}]& /@ mapping, ", "], "'0'"->"0"] <>
			"}\n\n";
	
	Return[ret];
]


(* ::Subsection::Closed:: *)
(*ExprToString*)


ExprToString[expr_, fieldsIn_:{}] := Block[{s = "", termList, tList, tmp, fields},
	termList = If[Head@expr === List, expr, {expr}];
	termList = (List @@@ termList) /. {Power[x_, n_] /; IntegerQ@n :> Sequence @@ Table[x,n], Dot->Sequence};
	
	If[fieldsIn =!= {},
		fields = If[Head@expr === List, fieldsIn, {fieldsIn}];
		termList = MapThread[SortBy[#1, Function[{el}, Position[#2, If[Head[el] =!= Symbol, Head[el], el]]]]&, {termList, fields}];
	];
	
	Do[
		tList = termList[[$i]];
		tmp = "";
		Do[
			If[term =!= -1,
				tmp = tmp <> ToString[term, InputForm] <> "*";
			,
				tmp = tmp <> "-";
			];
		,{term, tList}];
		tmp = StringTake[tmp, {1, -2}];
		
		If[$i == 1,
			s = s <> tmp;
		,
			If[StringTake[tmp, 1] === "-",
				s = s <> " - " <> StringTake[tmp, {2, -1}];
			,
				s = s <> " + " <> tmp;
			];
		];
	,{$i, Length@termList}];
	
	s = StringReplace[s, "Sqrt[" ~~ Shortest[x_ /; !StringContainsQ[x, "["] && !StringContainsQ[x, "]"]] ~~ "]" :> "sqrt(" <> x <> ")"]; 
	s = StringReplace[s, "^" -> "**"];
	
	Return[s];
]


(* ::Subsection::Closed:: *)
(*WriteCouplings*)


WriteCouplings[ggList_, yList_:{}, qList_:{}, tList_:{}, smList_:{}, fmList_:{}] := Block[{realComponents, formatted, str},
	realComponents = GetRealRunningComponents[ggList, yList, qList, tList, smList, fmList];
	formatted = Partition[PythonForm /@ realComponents, UpTo[5]];
	str = "[" <> StringRiffle[formatted, ",\n", ", "] <> "]\n";
	Return[str];
];


(* ::Section::Closed:: *)
(*Modified ASperGE interface*)


$ASperGeDir = "";
PR$MainASperGePath = "";


(* ::Subsection::Closed:: *)
(*Useful tools*)


(* ::Subsubsection::Closed:: *)
(*IndexifyParameter*)


(* ::Text:: *)
(*This function adds the necessary indices to every nude parameter*)


IndexifyParameter[param_?(ListQ[$IndList[#]]&)]:=param[Sequence@@($IndList[param]/.Index[type_]:>Index[type,Unique["indu"]])];
IndexifyParameter[param_?(ListQ[$IndList[#]]===False&)]:=param;


(* ::Subsubsection::Closed:: *)
(*ExpandAndDefine*)


(* ::Text:: *)
(*This function simply adds the corresponding indices to a given parameter and expands these indices. Takes as input a definition in the format*)
(*Equal[ parameter, declaration]*)


ExpandParam[def_]:=Block[{tmpp},
  (*Put and expand indices*)
  tmpp=def/.Equal[a_,b__]:>Equal[IndexifyParameter[a],b];
  tmpp=tmpp/.Equal[a_[indu__],b__]:>MyTable[Equal[a[indu],b],Sequence@@(List@(Sequence@indu)/.Index[typ_,name_]:>{name,Length[IndexRange[Index[typ]]]})];
  tmpp=Flatten[tmpp/.MyTable->Table]/.Index[typ_,name_]:>name;
  tmpp=If[tmpp===def,{tmpp},tmpp];
  Return[tmpp];
];


(* ::Text:: *)
(*This function processes the parameters' declarations and only keeps the relevant inforamtion for the definitions. *)
(*	Input: a definition with one of the following options {BlockName and Orderblock, BlockName and Indices, Values and/or Definitions}. As input there should not be definitions/values and  blocks for a parameter.*)
(*	Output: List[ par, value] Or List[par, blockname, {ind} ]*)


DefineParam[def_]:=Block[{tmpp}, 
  Which[
    (*Parameters with no indices and a block*)
    Position[def[[2]],BlockName]=!={}&&Position[def[[2]],OrderBlock]=!={}, 
      tmpp=List[def[[1]],OptionValue[def[[2]],BlockName], List[OptionValue[def[[2]],OrderBlock]]],
    (*Parameters with blocks and indices*)
    Position[def[[2]],BlockName]=!={}&&Position[def[[2]],Indices]=!={},
      tmpp=ExpandParam[def];
     tmpp=tmpp/.Equal[a_[indu__],list_]:>List[a[indu],OptionValue[list,BlockName],List[indu]],
    (*Definitions and/or Value no block  *)
    Position[def[[2]],BlockName]=={}&&Position[def[[2]],a_?(#==Value||#==Definitions&)]=!={} ,
      tmpp=Equal[def[[1]],DeleteCases[def[[2]],Rule[Indices,_]]];
(*AA: 08.27.13 > Modified this rule to have it working with more cases. *)
   tmpp=#//.{
     Equal[par_,list_]:>If[(ApplyDefinitions[par]/.Index[typ_,indu_]:>indu)===par,List[par,OptionValue[list,Value]],List[par,ApplyDefinitions[par]]],
     List[par_,list_?(ListQ[#]&)]:>List[par,ReplaceAll[par,list]]}&/@ExpandParam[tmpp];
  ];
  (*Convert integers to reals*)
  tmpp=Expand[tmpp]/.List[par_?(IntegerQ[#]=!=True&),num_Integer]:>List[par,N[num,30]];
  (*Check if the parameter is real or not*)
  tmpp=tmpp/.{List[a_?(CnumQ[#]&),b_?(ListQ[#]=!=True&),c___]:>List[a,b,c,Complex],List[a_?(CnumQ[#]=!=True&),b_?(ListQ[#]=!=True&),c___]:>List[a,b,c,Real]};
  Return[tmpp]];


(* ::Subsubsection::Closed:: *)
(*Check definitions*)


(* ::Text:: *)
(*AA. 08.27.13 > This function has been re-written to be more generic. Now it only tests if one of the masses to be computed in M$MixingsDescription appears in MR$Paremeters. If yes, calculations are aborted. This is more general than the previous version in the sense that it allows to have mixings defined in M$MixingsDescription and others in M$ClassesDescription.*)


CheckDefinitions[def_]:=Block[{masslist={},fieldmasses={}},
  (*fieldmasses contains the masses of the fields present in M$MixingsDescription*)
  fieldmasses=Mass/@Cases[Flatten[M$MixingsDescription[[All,2]]/.Rule[a_,b_]:>List[a,b]],_?FieldQ];
  (*We check that none of the masses in fieldmasses is considered as an input in MR$Parameters. *)
  If[MemberQ[fieldmasses,(#[[1]]/.a_[ind__]:>a)], AppendTo[masslist,#[[1]]/.a_[ind__]:>a]]&/@def;
  If[masslist=!={},
    Print["The mass parameters ",masslist," should not appear in M$Parameters. Calculations aborted"];Abort[];]
  ];



(* ::Subsubsection::Closed:: *)
(*DefinitionsToStrings*)


(* ::Text:: *)
(*Function to transform the definitions into strings*)
(*Input: one definition at a time. Can be *)
(*	1- Equal[ param , whatever] *)
(*	2- param*)
(*	3- whatever*)
(*Output: the string version of the format compatible with the c++ code*)


DefinitionsToStrings[def_]:=Block[{TransformParamsNames,tmpdef,tmpp},
  (*this rule is to transform something like yu[1,1] to yu1x1*)
  TransformParamsNames={"{"->"",","->"x"," "->"","}"->""};
  tmpdef=def/.par_?(MemberQ[M$Parameters[[All,1]],#]&)[ind__]:>ToExpression[ToString[par]<>StringReplace[ToString[List@ind],TransformParamsNames]];
  tmpdef=If[MatrixQ[def], FormatOutput[tmpdef], tmpp=tmpdef/.List[par_,value_,typ_?(#===Complex || #===Real&),des_]:>List[par,FormatOutput[value],typ,des]; #/.a_?(StringQ[#]=!=True&):>ToString[a] &/@tmpp];
  Return[tmpdef]];


(* ::Subsubsection::Closed:: *)
(*Get Parameter description*)


GetParamDescription[param_]:=Return[(Cases[M$Parameters,_?(MatchQ[#,Equal[param/.a_[ind__]:>a,rule__]]&)]/.Equal[_,b__]:>(Description/.b)/.Description->"No description found")[[1]] ];


(* ::Subsubsection::Closed:: *)
(*Format Output*)


(* ::Text:: *)
(*This function adds to every parameter in the right-hand side of the equality the strings ->value. Example*)
(*  yl1x1 = new Par((sqrt(2)*(yme -> value))/(vev -> value));*)
(*  	Input: whatever as long as it is a string. *)
(*  	Output: If "whatever" belongs to M$Parameters, than it adds to it a " -> getValue() " and the final output is a string in CForm*)


FormatOutput[def_]:=Block[{MyArrow,tmpp,CRules},
  tmpp=Expand[def]/.n_?(IntegerQ[#]||NumericQ[#]&)*pars__:>N[n,30]*pars;  
 (*some definitions for MyArrow function. This is used to add an " -> getValue()" to every parameter*)
  MyArrow=Unique["Arrow"];
  MyArrow[pars__?(Head[#]===Plus &),value]:=(MyArrow[#,value]&/@pars); MyArrow[pars__?(Head[#]===Times&),value]:=(MyArrow[#,value]&/@pars); MyArrow[par_?(NumericQ[#]&),value]:=par; MyArrow[a_?(ListQ[#]&),value]:=(MyArrow[#,value] &/@a);MyArrow[Power[par_,x_],value]:=Power[MyArrow[par,value],x];
  MyArrow[Conjugate[par_],value]:=Conjugate[MyArrow[par,value]];
  MyArrow[func_[par_?(MemberQ[MR$Parameters[[All,1]],#]&)],value]:=func[MyArrow[par,value]];  
  (*CForm doesn't seem to work that well, so some extra replacement rules need to be provided*)
  
  (* Lohan: test to get rid of the buggy "1/"->"1./" replacement rule *)
  (*
  CRules={"1/"->"1./","Power"->"pow","Pi"->"M_PI","Sqrt("~~n:DigitCharacter:>"sqrt("<>ToString[n]<>".","Sqrt"->"sqrt","Cos"->"cos","Sin"->"sin","Tan"->"tan","ArcTan"->"atan","Complex"->"complex<double>","Conjugate"->"conj"};
  (*Apply MyArrow to the def*)
  tmpp=MyArrow[tmpp,value];
  *)
  tmpp=MyArrow[tmpp,value];
  CRules={"Power"->"pow","Pi"->"M_PI","Sqrt("~~n:DigitCharacter:>"sqrt("<>ToString[n]<>".","Sqrt"->"sqrt","Cos"->"cos","Sin"->"sin","Tan"->"tan","ArcTan"->"atan","Complex"->"complex<double>","Conjugate"->"conj"};
  tmpp = tmpp /. Power[x_, n_Integer] /; n < 0 :> 1./x^(-n);
  
  (*CForm doesn't seem to work that well, so some extra replacement rules need to be provided*)
  tmpp=If[MatrixQ[tmpp],
    StringReplace[StringReplace[ToString/@(CForm/@#),CRules],{ToString[MyArrow]<>"("->"",",value)"->"->getValue()"}]&/@tmpp,
    StringReplace[StringReplace[ToString[CForm[tmpp]],CRules],{ToString[MyArrow]<>"("->"",",value)"->"->getValue()"}]];
  Return[tmpp]];


(* ::Subsubsection::Closed:: *)
(*Define blocks*)


(* ::Text:: *)
(*takes in input the list of the parameters defined with a block and the name of the block to define it returns*)
(*{name, SLHABlockxxx, range of the orderblock, number of indices}*)
(*one remark concerning the range of the orderblock, it is calculted by looking at the parameter that has the largest orderblock. *)


DefineBlocks[blockdefined_,block_]:=Block[{tmpp},
  tmpp=Cases[blockdefined,List[par_,block,ind_,_,_]][[All,3]];
  tmpp=List[block,Unique["SLHABlock"],Max[tmpp],Length[tmpp[[1]]]];
  Return[tmpp]];


(* ::Subsubsection::Closed:: *)
(*Checking hermiticity*)


CheckMatrices[]:=Block[{matrix,checkhermiticity},
  checkhermiticity[matrix_,id_]:=Block[{},
    If[matrix[[1,2]]=!=Conjugate[matrix[[2,1]]]&&matrix[[1,2]]=!=Conjugate[matrix[[1,1]]],
       Print["The mixing ",id," leads to a non hermitian matrix. Please check it"];Abort[]]];
  Which[
    Type[#]==="FLR", checkhermiticity[MassMatrix[#,"L"] . ConjugateTranspose[MassMatrix[#,"L"]],#],
    Type[#]==="CWeyl",checkhermiticity[MassMatrix[#] . ConjugateTranspose[MassMatrix[#]],#],
    Type[#]==="SPS",checkhermiticity[MassMatrix[#,"PS"],#],
    True,
      If[And@@(WeylFieldQ/@MassBasis),
        checkhermiticity[MassMatrix[#] . ConjugateTranspose[MassMatrix[#]],#],
        checkhermiticity[MassMatrix[#],#]]]&/@FR$MassMatrices;
  Return[]];


(* ::Subsubsection::Closed:: *)
(*Mass position in externals list*)


MassPositionFromPDG[pdg_] := Block[{res},
	res = Select[MassList[[2]] /. PDG -> Identity, MemberQ[#[[1]], pdg]&];
	res = If[Length[res] == 1, Position[PR$allExternals, res[[1,2]]][[1,1]], 0]
]


(* ::Subsection::Closed:: *)
(*Write the files*)


(* ::Subsubsection::Closed:: *)
(*WriteFiles function*)


(* ::Text:: *)
(*This function writes the headers of the different files.*)
(*If the option DiagMass is set to True, than the mass matrices are defined.*)


Options[WriteFiles]={DiagMass->True};


WriteFiles[OptionsPattern[]]:=Block[{cppstream,hppstream,stream,mainstream},
	(*Nice introduction in all files*)
	cppstream=OpenWrite["src/Parameters.cpp"];
	hppstream=OpenWrite["inc/Parameters.hpp"];
	If[OptionValue[DiagMass],
		mainstream=OpenWrite["src/running.cpp"]; 
		stream=List[cppstream,hppstream,mainstream]
	,
		stream=List[cppstream,hppstream]
	];
		
	WriteString[#,"////////////////////////////////////////////////////////////////////////////////\n"]&/@stream;
	WriteString[#,"//                                                                            //\n"]&/@stream;
	WriteString[#,"// Automated mass matrix diagonalization                                      //\n"]&/@stream;
	WriteString[#,"// A. Alloul, K. De Causmaecker, B. Fuks                                      //\n"]&/@stream;
	WriteString[cppstream,"// Cpp source file for the parameters of the model "<>M$ModelName<>" //\n"];
	WriteString[hppstream,"// Cpp header file for the parameters of the model "<>M$ModelName<>" //\n"];
	
	If[OptionValue[DiagMass],
		WriteString[mainstream,"//  Main file for the model "<>M$ModelName<>" //\n"];
	];   
	
	WriteString[hppstream,"// File generated by FeynRules "<>FR$VersionNumber<>"                                          //\n"];
	WriteString[cppstream,"// File generated by FeynRules "<>FR$VersionNumber<>"                                         //\n"];
	If[OptionValue[DiagMass],
		WriteString[mainstream,"// File generated by FeynRules "<>FR$VersionNumber<>"                                         //\n"]
	];
	WriteString[#,"//                                                                            //\n"]&/@stream;
	WriteString[#,"////////////////////////////////////////////////////////////////////////////////\n\n"]&/@stream;
	
	Close[#]&/@stream; 
	If[OptionValue[DiagMass],
		DefineParameters[DiagMass->True];
		DefineMassMatrices[];
	,
		DefineParameters[];
	];
];


(* ::Subsubsection::Closed:: *)
(*Define the parameters*)


(* ::Text:: *)
(*This functions is used to write in the files Parameters.cpp and Parameters.hpp all the needed informations to have every parameter defined. *)
(*If DiagMass is set to False, there will be no room for mass matrices definitions in the latter files*)


Options[DefineParameters]={DiagMass->False};


DefineParameters[OptionsPattern[]]:=Block[{def,blockdefined,blockrule,mixingslist,parameterslist,hppstream,cppstream,stream,blocklist},
	(*Initialization*)
	(*If both block and value are provided, only block is kept*)
	def=#/.Equal[par_,List[rule1___,Rule[BlockName,block_],rule3___,Rule[toto_?(#===Definitions||#===Value&),value_],rule5___]]:>Equal[par,List[rule1,Rule[BlockName,block],rule3,rule5]]&/@M$Parameters;
	(*Only keep relevant information*)
	def=#//.Equal[a__,List[opt1___,Rule[name_?(!MemberQ[{BlockName,OrderBlock,Definitions,Value,Indices},#]&),value_],opt3___]]:>Equal[a,List[opt1,opt3]] &/@def;
	(*Do not keep the mass matrices*)
	(*It is important to note here that a mixing matrix appearing in both M$MixingsDescription AND MR$Parameters is supposed
	  to be known and provided by the user. It will be thus treated as a known parameter *)
	mixingslist=Flatten[FilterRules[M$MixingsDescription[[All,2]],MixingMatrix]/.{Rule[_,a_]:>a,a_?(#===_&):>{}}];
	mixingslist=DeleteCases[mixingslist,a_?(MemberQ[Intersection[mixingslist,MR$Parameters[[All,1]]],#]&)];
	mixingslist=Flatten[List[#,ToExpression["R"<>ToString[#]],ToExpression["I"<>ToString[#]]]&/@mixingslist];
	def=DeleteCases[def,Equal[a_?(MemberQ[mixingslist,#]&),def__]];
	def=#/.List[list_?(ListQ[#]&),list2___]:>Sequence@@List[list,list2]&/@(DefineParam/@def);
	(*Add parameters descriptions if any*)
	def=Join[#,{GetParamDescription[#[[1]]]}]&/@def;

	def=Block[{fullName = ToExpression@StringReplace[ToString[#[[1]]], {"[" -> "", "]" -> "", ", " -> "x"}]},
		If[MemberQ[PR$allExternals, fullName], Append[#, Position[PR$allExternals, fullName][[1,1]]], #]
	]& /@ def;

	(*Checking definitions*)
	CheckDefinitions[def];
  
	(*for parameters defined in a paramcard.dat*)
	blockdefined=Union[Cases[def,List[_,_,_,_,_]]];
	blocklist=Union[blockdefined/.List[par_,blockname_,ind_,typ_,des_]:>blockname];
	blocklist=DefineBlocks[blockdefined,#]&/@blocklist;
	blockrule=Rule[WordBoundary~~ToString[#[[1]]]~~WordBoundary, ToString[#[[2]]]]&/@blocklist;
	(*Transform the definitions into strings*)
	def=DefinitionsToStrings[#]&/@def;

	(* Store the parameters names to build the destructor later *)
	PR$CppParams = def[[All,1]];
  
	(*Create and open files for writing*)
	hppstream=OpenAppend["inc/Parameters.hpp"];
	cppstream=OpenAppend["src/Parameters.cpp"];
	stream={hppstream,cppstream};
  
	(*The .hpp file*)
	WriteString[hppstream, "#ifndef PARAMETERS_HPP 
#define PARAMETERS_HPP 

#include \"headers.hpp\" 
#include \"Par.hpp\"

///class storing and treating all the parameters of the model
class Parameters 
{ 
    public:
        //empty constructor
        Parameters(){};

        //destructor
        ~Parameters();

        //calling this functions will initialise the parameters
        void init(double*);

"];

	WriteString[hppstream,"        //Declaration of the parameters\n"];
	If[#[[-2]]==="Complex",
		WriteString[hppstream,"        CPar* "<> First[#] <>";  \n"]
	,
		WriteString[hppstream,"        RPar* "<> First[#]<>";   \n"]
	]& /@ def;
  
	(*The .cpp file*)
	WriteString[cppstream,"#include \"Parameters.hpp\"\n\n"];
	WriteString[cppstream,"void Parameters::init(double* externals)\n{\n"];
  
	(*Declare every parameter*)
	WriteString[cppstream,"    //Initialisation of the parameters\n"];
  
	Which[
		Length[#]===6,
			WriteString[cppstream,"    "<>#[[1]]<>" = new RPar(externals["<>ToString[ToExpression[#[[-1]]]-1]<>"]);\n"],
		Length[#]===4 && #[[-2]]=="Complex",
			WriteString[cppstream,"    "<>#[[1]]<>" = new CPar("<>#[[2]]<>");\n"],
		Length[#]===4 && #[[-2]]==="Real",
			WriteString[cppstream,"    "<>#[[1]]<>" = new RPar("<>#[[2]]<>");\n"]
	]& /@ def;
        
	WriteString[cppstream,"\n"];

	(*If these files are created for the mass matrices diagonalization routine, than output a list of the parameters*)
	If[OptionValue[DiagMass], 
		Close[#]&/@stream;
	,
		WriteString[hppstream,"};\n"];
		WriteString[hppstream,"#endif"];
		Close[hppstream];
		WriteString[cppstream,"};\n"];
		Close[cppstream];
	];
	
	Print["Parameters.cpp and Parameters.hpp output is finished.\n They have been stored in " <> Directory[]];
];


(* ::Subsubsection::Closed:: *)
(*Define the mass matrices*)


(* ::Text:: *)
(*This function is only used when the mass diagonalisation is needed.*)
(*It appends to the files Parameters.cpp Parameters.hpp and main.cpp all the necessary declarations/initialisations and definitions needed to perform the diagonalisation.*)
(*First step in the calculation consists in storing for every mass matrix that has been calculated the following informations:*)
(*List[ MatrixSymbol, BlockName, Size of the matrix, the analytical expression of the matrix ]*)


CalculatePDGFLR[field_]:=Block[{resu},
	resu=PartPDG[field];
	If[Not[FreeQ[resu,PartPDG]], resu=PartPDG[ClassMemberList[field/.fi_[__]:>fi][[field/.fi_[inds1___,a_?NumericQ,inds2___] :>a]]]];
	If[Not[FreeQ[resu,PartPDG]], Print["Error with the PDG identification ("<>ToString[InputForm[field]]<>"). Please contact the FeynRules authors."]];
	Return[resu];
];


CalculatePDGWeyl[field_]:=Block[{Ferm4,inds},
	Ferm4=To4Components[field/.fi_[__]:>fi];
	inds=List@@field;
	Return[CalculatePDGFLR[Ferm4[Sequence@@inds]]];
];


DefineMassMatrices[]:=Block[{tmpdef,cppstream,hppstream,mainstream,matrixelement,matrixsymbol,blockname,massmatrix,pdgids,massbasis,liste, AuxBlockStrings},
	tmpdef = Which[
		(*Dirac Fermions*)
		Type[#]==="FLR" || Type[#]==="F4",
			matrixsymbol=ToString[MatrixSymbol[#,"L"]];
			blockname=ToString/@List[BlockName[#,"L"],BlockName[#,"R"]];
			massmatrix=DefinitionsToStrings/@((Expand[List[MassMatrix[#] . ConjugateTranspose[MassMatrix[#]], ConjugateTranspose[MassMatrix[#]] . MassMatrix[#] ]]/.n_?(NumericQ[#]&)*pars__:>N[n,30]*pars)/.Index[typ_,num_]:>num);
			pdgids=ToString/@(Sort/@List[CalculatePDGFLR/@MassBasis[#],CalculatePDGFLR/@MassBasis[#]]); 
			massbasis=ToString/@(MassBasis[#]/.part_[n_Integer,_]:>part[n]);
			liste=List[matrixsymbol,blockname,ToString[Length[massbasis]],massmatrix,pdgids,massbasis],

		(*Charged Weyls*)
		Type[#]==="CWeyl",
			matrixsymbol=ToString/@(List[MatrixSymbol[#][[1]],MatrixSymbol[#][[2]]]);
			blockname =ToString/@List[BlockName[#][[1]],BlockName[#][[2]]];
			massmatrix=DefinitionsToStrings/@((Expand[List[MassMatrix[#] . ConjugateTranspose[MassMatrix[#]],ConjugateTranspose[MassMatrix[#]] . MassMatrix[#]]]/.n_?(NumericQ[#]&)*pars__:>N[n,30]*pars)/.Index[typ_,num_]:>num);
			pdgids=ToString/@(List[Sort[CalculatePDGWeyl/@MassBasis[#][[1]]],-Sort[CalculatePDGWeyl/@MassBasis[#][[2]]]]);
			massbasis=List[ToString/@(MassBasis[#][[1]]/.par_[n_Integer,_]:>par[n]),ToString/@(MassBasis[#][[2]]/.par_[n_Integer,_]:>par[n])];
			liste=List[matrixsymbol,blockname,ToString/@(Length/@massbasis),massmatrix,pdgids,massbasis];
			Sequence@@List[liste[[All,1]],liste[[All,2]]],
			
		(*Scalars and pseudo scalars*)
		Type[#]==="SPS",
			matrixsymbol=ToString/@(List[MatrixSymbol[#,"S"],MatrixSymbol[#,"PS"]]);
			blockname=ToString/@List[BlockName[#,"S"],BlockName[#,"PS"]];
			massmatrix=DefinitionsToStrings/@((Expand[List[MassMatrix[#,"S"], MassMatrix[#,"PS"]]]/.n_?(NumericQ[#]&)*pars__:>N[n,30]*pars)/.Index[typ_,num_]:>num);
			pdgids=ToString/@(Sort/@List[CalculatePDGFLR/@MassBasis[#,"S"],CalculatePDGFLR/@MassBasis[#,"PS"]]);
			massbasis=List[ToString/@(MassBasis[#,"S"]/.par_[n_Integer,_]:>par[n]),ToString/@(MassBasis[#,"PS"]/.par_[n_Integer,_]:>par[n])];                 
			liste=List[matrixsymbol,blockname,ToString/@(Length/@massbasis),massmatrix,pdgids,massbasis];
			Sequence@@List[liste[[All,1]],liste[[All,2]]],
			
    (*Others*)
	True,
		(*It could be a neutral Weyl*)
		If[WeylFieldQ[First[MassBasis[#]]]===True,
			matrixsymbol=ToString[MatrixSymbol[#]];
			blockname=ToString[BlockName[#]];
			massmatrix=DefinitionsToStrings[(Expand[MassMatrix[#]]/.n_?(NumericQ[#]&)*pars__:>N[n,30]*pars)/.Index[typ_,num_]:>num];
			pdgids=ToString[Sort[CalculatePDGWeyl/@MassBasis[#]]];
			massbasis=ToString/@(MassBasis[#]/.part_[n_Integer,_]:>part[n]);
			liste=List[matrixsymbol,blockname,ToString[Length[massbasis]],massmatrix,pdgids,massbasis,"nsq"]
		,
			(*or a vector field*)
			matrixsymbol=ToString[MatrixSymbol[#]];
			blockname=ToString[BlockName[#]];
			massmatrix=DefinitionsToStrings[Expand[MassMatrix[#]]/.Index[typ_,num_]:>num];
			pdgids=ToString[Sort[CalculatePDGFLR/@MassBasis[#]]];
			massbasis=ToString/@(MassBasis[#]/.part_[n_Integer,_]:>part[n]);
			liste=List[matrixsymbol,blockname,ToString[Length[massbasis]],massmatrix,pdgids,massbasis]
		]
	]& /@ FR$MassMatrices;

	(*just making sure that string characters will have a double quote*)
	tmpdef[[All,6]] = StringReplace[#,"\"]"->"]\""]&/@(StringReplace[#,{WordBoundary~~a__~~WordBoundary:>"\""<>ToString[a]<>"\""}]&/@tmpdef[[All,6]]);

	cppstream = OpenAppend["src/Parameters.cpp"];
	hppstream = OpenAppend["inc/Parameters.hpp"];
  
	PR$MainASperGePath = FileNameJoin[{Directory[], "src", "running.cpp"}];
	mainstream = OpenAppend["src/running.cpp"];
  
	(*First: the Parameters.hpp*)
	WriteString[hppstream,"\n    ///map to store the pointers to the functions initialising the mass matrices \n"];
	WriteString[hppstream,"    map <string,initFP> massInit;\n"];
	WriteString[hppstream,"    map <string,initFP2> massInit2;\n"];
	(WriteString[hppstream,"    ///function to initialise the "<>#[[1]]<>" mass matrix\n"];
	If[Length[#[[2]]]==0,
		WriteString[hppstream,"    void init"<>#[[1]]<>"(gsl_matrix_complex *);\n"];
	,
		WriteString[hppstream,"    void init"<>#[[1]]<>"(gsl_matrix_complex *,gsl_matrix_complex *);\n"];
	])&/@tmpdef;
	WriteString[hppstream,"};\n#endif"];
	Close[hppstream];
  
	(*Second: the Parameters.cpp*)
	WriteString[cppstream,"    //map string \"MassSymbol\" to the pointer to the initialising function\n"];
	If[Length[#[[2]]]==0,
		WriteString[cppstream,"    massInit[\""<>#[[1]]<>"\"] = &Parameters::init"<>#[[1]]<>";\n\n"];
	,
		WriteString[cppstream,"    massInit2[\""<>#[[1]]<>"\"] = &Parameters::init"<>#[[1]]<>";\n\n"];
	]&/@tmpdef;
	WriteString[cppstream,"}\n"];

	If[Length[#[[2]]]==0,
		(*Non Dirac matrices*)
		WriteString[cppstream,"//functions to initialise the "<>#[[1]]<>" mass matrix\n"];
		WriteString[cppstream,"void Parameters::init"<>#[[1]]<>"(gsl_matrix_complex *m)\n"];
		WriteString[cppstream,"{\n    gsl_matrix_complex_set_zero(m);\n"];
		WriteString[cppstream,"    gsl_complex comp;\n"];
		
		Table[
			matrixelement=If[StringMatchQ[#[[4,ii,jj]],"0"],"complex<double>(0.,0.)",#[[4,ii,jj]]];
			WriteString[cppstream,"    GSL_SET_COMPLEX(&comp,\n    realpart("<>matrixelement<>"),\n    imagpart("<>matrixelement<>"));\n"];
			WriteString[cppstream,"    gsl_matrix_complex_set(m,"<>ToString[ii-1]<>","<>ToString[jj-1]<>",comp);\n\n"];
		,{ii,ToExpression[#[[3]]]},{jj,ToExpression[#[[3]]]}];
		WriteString[cppstream,"}\n"]
	,

		(*Dirac matrices*)
		WriteString[cppstream,"//functions to initialise the "<>#[[1]]<>" mass matrix\n"];
		WriteString[cppstream,"void Parameters::init"<>#[[1]]<>"(gsl_matrix_complex *m,gsl_matrix_complex *m2)\n"];
		WriteString[cppstream,"{\n    gsl_matrix_complex_set_zero(m);gsl_matrix_complex_set_zero(m2);\n"];
		WriteString[cppstream,"    gsl_complex comp;\n"];
		WriteString[cppstream,"   //fill m;\n"];

		Table[
			matrixelement=If[StringMatchQ[#[[4,1,ii,jj]],"0"],"complex<double>(0.,0.)",#[[4,1,ii,jj]]];
			WriteString[cppstream,"    GSL_SET_COMPLEX(&comp,\n    realpart("<>matrixelement<>"),\n    imagpart("<>matrixelement<>"));\n"];
			WriteString[cppstream,"    gsl_matrix_complex_set(m,"<>ToString[ii-1]<>","<>ToString[jj-1]<>",comp);\n\n"];
		,{ii,ToExpression[#[[3]]]},{jj,ToExpression[#[[3]]]}];
		WriteString[cppstream,"\n"];
		WriteString[cppstream,"   //fill m2;\n"];

		Table[
			matrixelement=If[StringMatchQ[#[[4,2,ii,jj]],"0"],"complex<double>(0.,0.)",#[[4,2,ii,jj]]];
			WriteString[cppstream,"    GSL_SET_COMPLEX(&comp,\n    realpart("<>matrixelement<>"),\n    imagpart("<>matrixelement<>"));\n"];
			WriteString[cppstream,"    gsl_matrix_complex_set(m2,"<>ToString[ii-1]<>","<>ToString[jj-1]<>",comp);\n\n"];
		,{ii,ToExpression[#[[3]]]},{jj,ToExpression[#[[3]]]}];
		WriteString[cppstream,"}\n"]
	]& /@ tmpdef;
  
	(* Destructor *)
	WriteString[cppstream, "

Parameters::~Parameters()
{\n"
	];

	(WriteString[cppstream,"    delete " <> # <> ";\n"])& /@ PR$CppParams;
	WriteString[cppstream,"}\n\n"];

	Close[cppstream];

	(* running.cpp *)
  
	AuxBlockStrings[def_] := Block[{blockList, extList, tabString, argString, massPDG},
		blockList = def[[2]];
		If[Head@blockList =!= List, blockList = {blockList}];

		blockList = Riffle[blockList, "IM" <> # & /@ blockList];
		extList = ({#[[1]], #[[2,All,2,1]]}& @Select[EParamList, Function[{$E}, ToString[$E[[1]]] === # ]][[1]])& /@ blockList;
 
		extList = Function[{b},{b[[1]], Position[PR$allExternals, #][[1,1]]& /@ b[[2]]}] /@ extList;

		extList = Prepend[extList, {def[[1]], MassPositionFromPDG /@ToExpression[#]}]& @ If[Head@def[[-2]] =!= List, def[[-2]], def[[-2,1]]];

		tabString = StringJoin[("    double* p" <> ToString[#[[1]]] <> "[] = {" 
						<> StringRiffle[Function[{p},If[p != 0, "res+"<> ToString[p-1], "NULL"]]/@ #[[2]], ","] <> "};\n"  )& /@ extList];

		argString = StringRiffle["p" <> ToString[#[[1]]]& /@ extList, ","];

		Return[{tabString, argString}];
	];

	WriteString[mainstream,"#include \"headers.hpp\"
#include \"Parameters.hpp\"
#include \"MassMatrix.hpp\"
#include \"MassMatrixNF.hpp\"
#include \"MassMatrixTwoMix.hpp\"
#include \"Par.hpp\"

int ppsolver(double t0, double t1, double step, double* initialCouplings, double** resArray, int nG, int nY, int nQ);

void diagonalize(double* externals, double* res);
int runAndDiagonalize(double* externals, double* res, double t0, double t1, double step);

extern \"C\"{
    // Running of external parameters -- To be called from Fortran routine
	int eparamrunning_(double* externals, double* res, double* t0, double* t1, double* step)
	{
	    return runAndDiagonalize(externals, res, *t0, *t1, *step);
	}
}

void diagonalize(double* externals, double* res)
{
    //initialise the parameters
    int nExternals = " <> ToString[Length[PR$allExternals]] <> ";
    for(int i=0; i<nExternals; i++)
        res[i] = externals[i];

    MassMatrix::par = new Parameters();
    MassMatrix::par->init(externals);\n\n"
	];

	WriteString[mainstream,"    //mass matrix constructor arguments:\n"];
	WriteString[mainstream,"    //Symbol, block, IMblock, matrix dimension, Pdg-Ids, particles' names.//\n"];

	(*Karen 08/05/2013 changed the following line in order to make ASperGe compatible with gcc 4.8*)
	Block[{auxStrings = AuxBlockStrings[#]},
		If[Length[#[[2]]]==0,
			WriteString[mainstream, "\n" <> auxStrings[[1]]];
			
			If[Length[#]===6,
				WriteString[mainstream,"    MassMatrix* M" <> #[[1]] <> " = new MassMatrix(\""<>#[[1]]<>"\"," <> auxStrings[[2]] <> ","<>#[[3]]<>");\n"]
			,
				WriteString[mainstream,"    MassMatrixNF* M" <> #[[1]] <> " = new MassMatrixNF(\""<>#[[1]]<>"\"," <> auxStrings[[2]] <> ","<>#[[3]]<>");\n"]
			];
		,
			WriteString[mainstream, "\n" <> auxStrings[[1]]];
			WriteString[mainstream,"    MassMatrixTwoMix* M" <> #[[1]] <> " = new MassMatrixTwoMix(\""<>#[[1]]<>"\"," <> auxStrings[[2]] <> ","<>#[[3]]<>");\n"]
		];
	]& /@ tmpdef;
	
	WriteString[mainstream,"\n    //diagonalizes all the members of the class MassMatrix//\n"];
	WriteString[mainstream,"    MassMatrix::generateAll();\n\n    // Clean\n"];
	(WriteString[mainstream,"    delete M" <> #[[1]] <> ";\n"])& /@ tmpdef;
	WriteString[mainstream,"\n    delete MassMatrix::par;\n}\n\n"];

	Close[mainstream];
];


(* ::Subsubsection::Closed:: *)
(*Append RGE solving functions to the main ASperGe file*)


CppRGESolving[] := Block[{mainstream, nl=ToString[PR$loops], runningExternals, runningPos, pad, runningPosStr, runningPosErr},
	runningExternals = DeleteDuplicates[GetRealRunningComponents[PR$GaugeGroups, PR$Yukawas, PR$Quartics, PR$Trilinears, PR$ScalarMasses, PR$FermionMasses] /. Conjugate -> Identity];
	runningPos = Check[Position[PR$allExternals, #][[1,1]]-1, {#, False}]& /@ runningExternals;
	runningPosErr = Cases[runningPos, {x_, False} :> x];
	
	If[runningPosErr =!= {},
		Print["Error: the following external parameters appear to be missing from the model file:\n", runningPosErr];
		Abort[];
	];
	
	pad = StringRepeat[" ", 32];
	runningPosStr = "{" <> StringReplace[StringRiffle[Riffle[ToString /@ runningPos, "\n" <> pad, 11], ","], ("\n" <> pad <> ",") -> ("\n" <> pad)] <> "}";
	
	Print["[DEBUG] Writing RGE file in ", PR$MainASperGePath];
	mainstream = OpenAppend[PR$MainASperGePath];

	WriteString[mainstream,
"

int runAndDiagonalize(double* externals, double* res, double t0, double t1, double step)
{
    int nExternals = " <> ToString[Length[PR$allExternals]] <> ";

    double auxExternals[nExternals];
    for(int i=0; i<nExternals; i++)
        auxExternals[i] = externals[i];

    int nRunning = " <> ToString[Length[runningExternals]] <> ";
    int runningPos[nRunning] = " <> runningPosStr <> ";
    int landau;

    double initialCouplings[nRunning];
    double* runExternals[nRunning];

    for(int i=0; i<nRunning; i++)
    {
        initialCouplings[i] = externals[runningPos[i]];
        runExternals[i] = auxExternals + runningPos[i];
    }

    // Call the RGE solver

    if(step == 0) // Set the step to some default value
    {
    	if(t1 > t0){
        	step = (t1-t0)/15.;
        }
        else{
        	step = (t0-t1)/15.;
        }
    }

    landau = ppsolver(t0, t1, step, initialCouplings, runExternals, "<>nl<>", "<>nl<>", "<>nl<>");

    // Perform diagonalization
    diagonalize(auxExternals, res);

    return landau;
}
"];
	Close[mainstream];
];


(* ::Subsection::Closed:: *)
(*Main*)


(* ::Text:: *)
(*This is the main function from which everything starts.*)


PR$WriteASperGe[ufoFolder_, lag_] := Block[{dirname, location, interfacedir, mix, paramAliases, mkStr},
	(*mix = If[OptionValue[Mix] === None, M$MixingsDescription, OptionValue[Mix]];*)
	location = Directory[];
	dirname = "PyRATE";

	SetDirectory[ufoFolder];
	$ASperGeDir = ufoFolder <> "/" <> dirname;

	(* Path to the mass diag interface folder *)
	interfacedir = StringReplace[Global`$FeynRulesPath<>"/Interfaces/PyRATE/MassDiag","//"->"/"];

	(* Delete (if necessary) and create a new directory *)
	If[DirectoryQ[dirname], DeleteDirectory[dirname,DeleteContents->True]];
	CopyDirectory[interfacedir, dirname]; 

	SetDirectory[dirname];
	Print[Style["* Computing Mass Matrices",Orange]]; 
	ComputeMassMatrix[lag, ScreenOutput->True];
	CheckMatrices[];

	(* Establish the full list of external params *)
	paramAliases = Function[{pn},  If[pn =!= {}, pn -> #[[1]], Nothing]][GetRule[#[[2]], ParameterName]]&  /@ MR$Parameters;
	PR$allExternals = PRIVATE`ChangeEParameterConventions[EParamList, MassList, WidthList][[All,2]] /. paramAliases;

	Print[Style["* Writing Files",Orange]];
	WriteFiles[];
	Print[Style["Done.",Orange]];
	
	(* Come back to where we were *)
	SetDirectory[location];
];


(* ::Section:: *)
(*Main functions*)


(* ::Subsection::Closed:: *)
(*WritePyRATE*)


SetAttributes[WritePyRATE, HoldAll];
Options[WritePyRATE] = {
	OutputFolder        -> "",
	UsePyReps           -> True,
	Latex               -> {},
	RealBasis           -> "adjoint",
	GuessYukawaCoeff    -> True
};


WritePyRATE[opt:OptionsPattern[]] := WritePyRATE[{Nothing}, Evaluate @ opt];
WritePyRATE[lags__, opt:OptionsPattern[]] := WritePyRATE[{lags}, Evaluate @ opt];


WritePyRATE[{lags___}, OptionsPattern[]] := Block[{iLag, err, lagParts, lagPartsNoHC, lagTerms, lagTermsNoHC, sortingFunction},
	CheckInterfaceConfiguration[];
	
	Print[Style["Generating the PyR@TE model file", Bold, Orange]];
	(* Retrieve info on groups, fermions & scalars *)
	PR$GaugeGroups = GetGaugeGroups[OptionValue[UsePyReps]]; 
	PR$Fermions = GetFermions[];
	PR$Scalars = GetScalars[];
	PR$Parameters = M$Parameters;

	PR$Lagrangian = {lags};
	(* Getting information on the Lagrangian, stored in three containers *)
	If[Hold[{lags}] =!= Hold[{}],
		PR$Messages["Reading the Lagrangian ...", 1];
		err = Check[
				iLag = InactivateLag[{lags}];
				lagParts = LagToParts[Expand[iLag] /. PR$HCi->HC] /. CC->GetConjugate;
				lagPartsNoHC = LagToParts[Expand[iLag] /. PR$HCi[___]->0] /. CC->GetConjugate;
			,
			False];
	];

	If[err === False || Head@lagParts =!= List || Head@lagParts[[1]] === Symbol,
		Print[Style["An error occured while reading the Lagrangian. Aborting.", Red]];
		Abort[];
	];

	(* Read, processing and sorting the different Lagrangian terms *)
	lagTerms = GetLagTerms[lagParts,  Name /. PR$Scalars, Name /. PR$Fermions];
	lagTermsNoHC = GetLagTerms[lagPartsNoHC,  Name /. PR$Scalars, Name /. PR$Fermions];

	PR$Yukawas = GetYukawas[lagTermsNoHC];
	PR$Quartics = GetQuartics[lagTerms];
	PR$Trilinears = GetTrilinears[lagTerms];
	PR$ScalarMasses = GetScalarMasses[lagTerms];
	PR$FermionMasses = GetFermionMasses[lagTermsNoHC];

	(* Updating the fermion list *)
	PR$Messages["Updating particles ...", 1];
	PR$Fermions = ConjugateParticleList[PR$Fermions];
	PR$Fermions = PR$Fermions /. {PR$defaultA -> 0, PR$defaultNA -> 1};
	(*PR$Fermions = SplitDiracFermions[PR$Fermions];*)

	(* Updating the scalar list *)
	PR$Scalars = ConjugateParticleList[PR$Scalars];
	PR$Scalars = PR$Scalars /. {PR$defaultA -> 0, PR$defaultNA -> 1};
	PR$Scalars = AddRealFields[PR$Scalars];

	(* Compute CGCs *)
	PR$Messages["Building PyR@TE's Lagrangian ...", 1];
	PR$DBStoreInvariants = {}; PR$CGCs = {};
	PR$Yukawas = UpdateCGCs[Hold @ PR$Yukawas];
	PR$Quartics = UpdateCGCs[Hold @ PR$Quartics];
	PR$Trilinears = UpdateCGCs[Hold @ PR$Trilinears];
	PR$ScalarMasses = UpdateCGCs[Hold @ PR$ScalarMasses];
	PR$FermionMasses = UpdateCGCs[Hold @ PR$FermionMasses];

	(* Sorting the output *)
	sortingFunction[e_] := Append[PadRight[StringSplit[ToString[# /. Im | Re | Conjugate -> Identity], nPar:DigitCharacter .. :> FromDigits[nPar]]], MatchQ[#, Im[_]]]& @ GetRule[e, Coeff];               

	PR$Yukawas = SortBy[FactorPotentialTerms[PR$Yukawas], sortingFunction];
	PR$Quartics = SortBy[FactorPotentialTerms[PR$Quartics], sortingFunction];
	PR$Trilinears = SortBy[FactorPotentialTerms[PR$Trilinears], sortingFunction];
	PR$ScalarMasses = SortBy[FactorPotentialTerms[PR$ScalarMasses], sortingFunction];
	PR$FermionMasses = SortBy[FactorPotentialTerms[PR$FermionMasses], sortingFunction];
	
	PR$ModelFile = WriteAll[OptionValue[OutputFolder], OptionValue[Latex],
							 PR$GaugeGroups, PR$Fermions, PR$Scalars,
							 PR$Yukawas, PR$Quartics, PR$Trilinears, PR$ScalarMasses, PR$FermionMasses];

	Print[Style["Done.", Orange]];
];


(* ::Subsection:: *)
(*RunPyRATE*)


Options[RunPyRATE] = {
	Model -> Automatic,
	UFOFolder    -> None, (* Relative or absolute path *)
	PyRATEOutput -> Automatic, (* Automatic / True / False *)

	LoopLevel -> 1,
	SkipGaugeInvarianceCheck -> False,

	OutputRefreshInterval -> .2,
	Verbose -> False
};


RunPyRATE[OptionsPattern[]] := Block[
{model, ufoOut = False, ufoDir, mainUfoDir, pyrateOut = False, pyrateResultsDir, fullCommand, args, loop, output, ret, ascentFolder, ascentTarget},
	CheckInterfaceConfiguration[];
	
	model = If[OptionValue[Model] === Automatic, PR$ModelFile, OptionValue[Model]];
	
	If[OptionValue[Model] === Automatic && PR$ModelFile === Unevaluated[PR$ModelFile],
		Print["Please first produce a PyR@TE model file using WritePyRATE[]."];
		Print["It is also possible to run PyR@TE using an existing model file, using"];
		Print["\t", Style["RunPyRATE[Model -> ", Bold], "<Path to PyR@TE model file>", Style["].", Bold]];
		Return[];
	];
	If[!FileExistsQ[model],
		Print["Error: the PyR@TE model file does not exist (provided path was '" <> model <> "')."];
		Return[];
	];
	
	If[OptionValue[UFOFolder] =!= None,
		ufoOut = True;
	];
	Switch[OptionValue[PyRATEOutput],
			Automatic, 
				pyrateOut = Not[ufoOut],
			True, 
				pyrateOut = True,
			False,
				pyrateOut = False,
			_,
				Print["Error: the option PyRATEOutput can only be set to Automatic, True or False."];
				Return[];
	];

	If[ufoOut === False && pyrateOut === False,
		Print["Error: both the PyR@TE output and UFO output have been disabled. Aborting."];
		Return[];
	];
	If[ufoOut === True,
		ufoDir = Quiet@Check[AbsoluteFileName[OptionValue[UFOFolder]], False];
		If[ufoDir === False,
			Print["Error: option 'UFOFolder' corresponds to an invalid path: ", FileNameJoin[{Directory[], OptionValue[UFOFolder]}]];
			Abort[];
		];
		
		mainUfoDir = ufoDir;
		ufoDir = FileNameJoin[{ufoDir, "Cpp"}];
		
		If[DirectoryQ[ufoDir],
			DeleteDirectory[ufoDir, DeleteContents->True]
		];
		CreateDirectory[ufoDir];

		Print[Style["Running the ASperGe interface", Bold, Orange]];
		PR$WriteASperGe[ufoDir, Total[PRIVATE`PR$Lagrangian]];
	];

	Clear @ PR$PyrateOutput;
	PR$logFiles = ReadLogFiles[];

	pyrateResultsDir = FileNameJoin[{DirectoryName[PR$ModelFile], "results"}];

	(* Prepare the run command *)
	fullCommand = PR$PythonExecutable <> " " <> FileNameJoin[{PR$PyratePath, "pyR@TE.py"}];

	args = {"-m", PR$ModelFile, "-log", "-res", pyrateResultsDir, "--CreateFolder",  "False", "-lcpp", DirectoryName[PR$MainASperGePath], "--LightCppSolverOnly"};
	If[OptionValue[SkipGaugeInvarianceCheck] === True, args = Append[args, "-no-gi"]];
	
	PR$loops = OptionValue[LoopLevel];
	If[PR$loops =!= 1, args = Join[args, {"-l", ToString @ PR$loops}]];

	fullCommand = fullCommand <> " " <> StringRiffle[args, " "];

	SetSharedVariable[fullCommand];
	(* Prepare the dynamic printing *)
	PR$PyrateOutput := ReadOutput[];
	output = Dynamic[PR$PyrateOutput, UpdateInterval -> OptionValue[OutputRefreshInterval]];

	Print[Style["\nRunning PyR@TE:", FontWeight->Bold, Orange],
		Sequence @@ If[OptionValue[Verbose],
			{"\n\n[Info] The full command is \"", Style[fullCommand, FontFamily->"Courier"], "\"\n"}
		,
			{""}
		]
	];
	Print[Style[output, FontFamily->"Courier"]];

	(* Actual run *)
	ret = Parallelize[{Run[fullCommand]}];
	PR$PyrateOutput = ReadOutput[];

	If[PR$PyrateOutput == "", PR$PyrateOutput = "Error : unable to run PyR@TE ...\n The full command was :\n\t" <> fullCommand];
	If[Mod[ret[[1]], 256] =!= 0, Print[Style["An error occurred while running PyR@TE...", Red]]];

	If[ufoOut,
		(* Merge the main asperge file with RGE solving functions + compile *)
		Print[Style["Generating C++ output...", Bold, Orange]];
		CppRGESolving[];

		If[pyrateOut === False,
			(* Copy the PyR@TE model file to the UFO directory to keep track of how RGEs were computed *)
			CopyFile[model, FileNameJoin[{ParentDirectory@DirectoryName[PR$MainASperGePath], FileNameTake[model]}]];
			(*DeleteFile[model];
			DeleteDirectory[DirectoryName[model]];*)
		];

		ascentFolder = FileNameJoin[{PR$PyratePath, "src", "IO", "include", "ascent"}];
		ascentTarget = FileNameJoin[{ParentDirectory@DirectoryName[PR$MainASperGePath], "inc", "ascent"}];

		CopyDirectory[ascentFolder, ascentTarget];
		Print[Style["Done.", Orange]];
	];
];
