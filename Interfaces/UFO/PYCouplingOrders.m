(* ::Package:: *)

(* ::Title:: *)
(*Coupling orders*)


(* ::Section:: *)
(*PYConvolveCouplingOrdersLists*)


(* ::Text:: *)
(*PYConvolveCouplingOrdersLists[ hierarchy, limits ] does the following:*)
(*Both hierarchy and limits are nx2 matrices of the same size.*)
(**)
(*If for example *)
(*    *)
(*    hierarchy = {{QED, 2}, {QCD, 1}, ...}*)
(*    *)
(* and*)
(* *)
(*    limits = {{QED, 23}, {QCD, 35}, ...}*)
(*    *)
(* and*)
(* *)
(*    perturbed = {{QED, 0}, {QCD,1}, ...}*)
(*    *)
(* then we return the nx4 matrix  *)
(* *)
(* {{QED, 2, 23}, {QCD, 1, 35}, ...}*)


PYConvolveCouplingOrdersLists[hierarchy_List, limits_List, perturbed_List] := Join[#[[1]], {#[[2,2]], #[[3,2]]}]& /@ GatherByFirstElement[Join[hierarchy, limits /. Infinity -> 99, perturbed]];


(* ::Section:: *)
(*CreateCouplingObjectEntry*)


(* ::Text:: *)
(*CreateCouplingObjectEntry[list] take a list of the form*)
(**)
(*      {QED, 2, 23, 1}*)
(*      *)
(*as provided by PYConvolveCouplingOrdersLists and returns the list*)
(**)
(*  {QCD ,*)
(*        {name, 'QCD'},*)
(*        {hierarchy, 2},*)
(*        {expansion_order, 23},*)
(*        {perturbative_expansion, 1}*)
(*    }    *)
(*    *)
(* If perturbative_expansion is 0, this element is removed.*)


(* ::Text:: *)
(* *)


CreateCouplingObjectEntry[{order_, hier_, lim_, perturbed_}] := DeleteCases[{ToString[order], {"name", PYString[ToString[order]]},
                                                                      {"expansion_order", ToString[lim]},
                                                                      {"hierarchy", ToString[hier]},
                                                                      {"perturbative_expansion", ToString[perturbed]}
                                                                      },
                                                                      {"perturbative_expansion", "0"}];


(* ::Section:: *)
(*WriteCouplingOrderObject*)


(* ::Text:: *)
(*WriteCouplingOrderObject[file, list] takes a list as provided by CreateCouplingObjectEntry, and write it to <file> as*)
(**)
(*QCD   =   CouplingOrder(name = 'QCD',*)
(*                                          hierachy = 2,*)
(*                                          expansion_order = 23*)
(*                                          )*)


WriteCouplingOrderObject[file_, list_] := Block[{},

     WritePYObject[file, list[[1]], "CouplingOrder", Rest[list]];
     WriteString[file, "\n"];

];


(* ::Section:: *)
(*WritePYCouplingOrders*)


(* ::Text:: *)
(*WritePYCouplingsOrders[ ] writes all the interaction order in list to coupling_orders.py*)


WritePYCouplingOrders[] := Block[{outfile},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# Coupling order definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write couplings.py *)
   DeleteFileIfExists["coupling_orders.py"];
   outfile = OpenWrite["coupling_orders.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_orders, CouplingOrder\n"];
   WriteString[outfile, "\n\n"];

   WriteCouplingOrderObject[outfile, #]& /@ (CreateCouplingObjectEntry /@ PYConvolveCouplingOrdersLists[FR$InteractionOrderHierarchy,FR$InteractionOrderLimit,FR$InteractionOrderPerturbativeExpansion]);
   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[list] == 1, "1 coupling order", ToString[Length[list]] <> " couplings orders"] <> " written."];
   TestQ[FileExistsQ, "coupling_orders.py", "   * coupling_orders.py written.", "   * coupling_orders.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 
