(* ::Package:: *)

(* ::Title:: *)
(* FeynRules 2  *)


(* ::Text:: *)
(*Authors : A. Alloul, N. Christensen, C. Degrnade, C. Duhr, B. Fuks 2013*)
(*    *)
(*    *)
(*Wiki-page for the FeynRules package:   http://feynrules.irmp.ucl.ac.be/*)
(**)
(*  *)


(* ::Section:: *)
(*Compatibility with Mathematica 12.2*)


If[$VersionNumber>=12.2, SetOptions[ValueQ, Method->"Legacy"]];


(* ::Section:: *)
(*Main call to the package*)


(* If the package is already loaded, then it will not be loaded again *)

If[FR$Loaded =!= True, 
	Get[ToFileName[$FeynRulesPath, "FeynRulesPackage.m"]];
	(*Parallelize - NC*)
	If[FR$Parallel===False,FR$Parallelize=False,FR$Parallelize=True];
	If[FR$Parallelize,
        If[ValueQ[FR$KernelNumber],
           LaunchKernels[FR$KernelNumber],
           LaunchKernels[];
           FR$KernelNumber = $KernelCount
           ];
		DistributeDefinitions[$FeynRulesPath];
		ParallelEvaluate[
			$Output={};
			If[$VersionNumber>=12.2, SetOptions[ValueQ, Method->"Legacy"]]; (* Compatilibity with Mathematica 12.2 *)
			SetDirectory[$FeynRulesPath];
			Get[ToFileName[$FeynRulesPath, "FeynRulesPackage.m"]];
			$Output={OutputStream["stdout",1]};
		];
	];
	(*End Parallelize - NC*)
	
,
	Print["Package already loaded..."]];
