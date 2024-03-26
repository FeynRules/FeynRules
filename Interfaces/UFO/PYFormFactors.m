(* ::Package:: *)

(* ::Title:: *)
(*UFO Form factors*)


(* ::Section:: *)
(*AddFormFactorsToLorentzObject*)


(* ::Text:: *)
(*Takes a Lorentz object LorentzObject[name, spins, expr] as entry, and return a Lorentz object with a list appended that contains all the form factors that appear in expr.*)


CountFormFactors[expi_, fff_] := If[expi === fff, Return[1], Return[Count[expi, fff, Infinity]]];


AddFormFactorsToLorentzObject[LorentzObject[name_, spins_, exp_]] := Block[{ffs},
   
    (* Make a list which counts how many times a given form factor appear inside the expression,
       then delete those that appear 0 times *)
    ffs = {#, CountFormFactors[exp, #]}& /@ FR$FormFactors;
    ffs = DeleteCases[ffs, {_, 0}];
    ffs = First /@ ffs;

    Return[LorentzObject[name, spins, exp, ffs]];

];


(* ::Section:: *)
(*CreatePYFormFactorEntry*)


CreatePYFormFactorEntry[{nameff_, expff_}] := Block[{
   name = nameff /. ParamRules,
   parts = FormFactor$Particles[nameff],
   compl = CnumQ[nameff],
   expf = Expand[expff]
   },

   (* Open the scalar products *)
    expf = expf /. SP[i_, j_] :> PYSPOpen[i,j];
    expf = OptimizeIndexName[expf];

   (* Pass to Python form *)
    expf = PythonForm[expf];
  
    Return[FormFactorObject[ToString[name], {{"name",  PYString[ToString[name]]},
                                   {"type",  PYString[If[compl, "complex", "real"]]},
                                   {"value", PYString[expf]}}]];
];
    

   

   


(* ::Section:: *)
(*WriteFormFactorObject*)


WriteFormFactorObject[file_, FormFactorObject[name_String, entries_List]] := Block[{},

     WritePYObject[file, name, "FormFactor", entries];
     WriteString[file, "\n"];

];
   


(* ::Section:: *)
(*WritePYFormFactors*)


WritePYFormFactors[list_] := Block[{outfile},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# Form Factor definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write vertices.py *)
   DeleteFileIfExists["form_factors.py"];
   outfile = OpenWrite["form_factors.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_form_factors, FormFactor\n"];
   WriteString[outfile, "\nfrom function_library import ", Sequence @@ Riffle[PY$NewCMathFunctions, ", "], "\n\n"];
   WriteString[outfile, "\n\n"];

   WriteFormFactorObject[outfile, #]& /@ (CreatePYFormFactorEntry /@ list);

   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[list] == 1, "1 form factor", ToString[Length[list]] <> " form factors"] <> " written."];
   TestQ[FileExistsQ, "form_factors.py", "   * form_factors.py written.", "   * form_factors.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 
