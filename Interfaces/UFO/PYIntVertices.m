(* ::Package:: *)

(* ::Title:: *)
(*Vertices.py*)


(* ::Section:: *)
(*PYSplitVertices*)


(* ::Subsection::Closed:: *)
(*StrictInner*)


(* ::Text:: *)
(*StrictInner is the same as Inner, except that it acts "stricly", i.e., it does not thread over matrices, like Inner does.*)


StrictInner[f_, list1_List, list2_List, g_] := g @@ Table[f[list1[[i]], list2[[i]]], {i, Length[list1]}];


(* ::Subsection::Closed:: *)
(*PrependToLines*)


(* ::Text:: *)
(*PrependToLines[matrix, elem] prepends elem to each line of matrix.*)


PrependToLines[matrix_List, elem_] := ({LorentzObject @@ Prepend[#, elem]})& /@ matrix;


(* ::Subsection:: *)
(*PYSplitVertices*)


(* ::Text:: *)
(*PYSplitVertices[ vertexlist ] takes a vertex list, and returns a new list of vertices, each vertex being of the form*)
(**)
(*{particles,    {C1, L_1, GC_1}, .... }*)
(**)
(*It also builds the lists PY$LorentzObjects and PY$CouplObjects.*)


PYSplitVertices[vertices_] := Block[{
    verts = vertices, particles = vertices[[All, 1]],
    split1, split2,
    colorstruc, lorentzstruc, couplstruc,
    lorobjrules,couplobjrules, numberrpls, reversenumberrpls,
    PYSVPower, PYSVPi, PYSVE, PYSVSqrt, PYSVLog,
    PYSVSin, PYSVCos, PYSVTan, PYSVCot, PYSVCsc, PYSVSec, 
    PYSVArcSin, PYSVArcCos, PYSVArcTan, PYSVArcCot, PYSVArcSec, PYSVArcCsc,
    PYSVSinh, PYSVCosh, PYSVTanh, PYSVCoth, PYSVCsch, PYSVSech, 
    PYSVArcSinh, PYSVArcCosh, PYSVArcTanh, PYSVArcCoth, PYSVArcSech, PYSVArcCsch},

    (* We have to rename certain numbers, which should not be trated like numbers *)
    numberrpls = {Power[x_, r_Rational] :> PYSVPower[x,r],
                  Pi -> PYSVPi, 
                  E  -> PYSVE,  
                  Sqrt:>PYSVSqrt,
                  Log :> PYSVLog,
                  Sin :> PYSVSin,
                  Cos :> PYSVCos,
                  Tan :> PYSVTan,
                  Cot :> PYSVCot,
                  Csc :> PYSVCsc,
                  Sec :> PYSVSec,
                  ArcSin :> PYSVArcSin,
                  ArcCos :> PYSVArcCos,
                  ArcTan :> PYSVArcTan,
                  ArcCot :> PYSVArcCot,
                  ArcCsc :> PYSVArcCsc,
                  ArcSec :> PYSVArcSec,
                  Sinh :> PYSVSinh,
                  Cosh :> PYSVCosh,
                  Tanh :> PYSVTanh,
                  Coth :> PYSVCoth,
                  Csch :> PYSVCsch,
                  Sech :> PYSVSech,
                  ArcSinh :> PYSVArcSinh,
                  ArcCosh :> PYSVArcCosh,
                  ArcTanh :> PYSVArcTanh,
                  ArcCoth :> PYSVArcCoth,
                  ArcCsch :> PYSVArcCsch,
                  ArcSech :> PYSVArcSech
                  };
    reversenumberrpls = Prepend[Reverse /@ Rest[numberrpls], PYSVPower -> Power];
    verts = verts //. numberrpls;

    (* We separate everything *)
    If[Global`FR$Parallelize === False,
       split1 = FullSplitL[verts],
       (* else, parallellize *)
       verts = PartitionVertexList[verts, FR$MaxKernels];
       Print["Splitting of vertices distributed over ", Length[verts], " kernels."];
       DistributeDefinitions[verts];

       split1 = Table[ParallelSubmit[{iii},
                        FullSplitL[verts[[iii]]]],
                        {iii, Length[verts]}
                        ];
       split1 = Join @@ WaitAll[split1];
      ];

    lorentzstruc = Map[Take[#,{1,1}]&, split1, {2}];  

    If[Global`FR$Parallelize === False,
       split2 = FullSplitC[Map[Take[#,{2,2}]&, split1, {2}]],
       (* else, parallellize *)
       split2 = PartitionVertexList[Map[Take[#,{2,2}]&, split1, {2}], FR$MaxKernels];
       DistributeDefinitions[split2];
       split2 = Table[ParallelSubmit[{iii},
                        FullSplitC[split2[[iii]]]],
                        {iii, Length[split2]}
                        ];
       split2 = Join @@ WaitAll[split2];
      ];

    colorstruc = Map[Take[#,{1,1}]&, split2, {2}];    
    couplstruc = Map[Take[#,{2,2}]&, split2, {2}] //. reversenumberrpls; 
   

(*    colorstruc = ColorList[verts];
    lorentzstruc = StructureList[verts];
    couplstruc = CoupleList[verts];*)

    (* we add the particles spins to the Lorentz structures, and we tag Lorentz and couplings as LorentzObjects and CouplingObjects respectively*)
    lorentzstruc = StrictInner[PrependToLines, lorentzstruc, (ParticleToSpin[#1]& @@@ #)& /@ particles, List];
    couplstruc = ({CouplingObject[#]}& @@@ #)& /@ couplstruc;

    (* We convolve them back into an object {colors, lorentzes, couplings} per vertex *)

    verts = Transpose[ {colorstruc, lorentzstruc, couplstruc} ];
    verts = Transpose /@ verts;
    verts = (Join @@@ # &) /@ verts;

   (*And we add the particles *)
   verts = StrictInner[Prepend, verts, particles, List];

   (* Set the progress bar *)
   If[$VersionNumber >= 6, 
      Print["    - Optimizing: ", Dynamic[PY$SplitVertexCounter], "/", Length[verts], " ."],
      (* else *)
      Print["    - Optimizing."]];

   (* Now we optimize *)
   verts = MapIndexed[OptimizePYSplitVertices, verts];
   verts = OptimizeInteractionOrders @@@ verts;

   (* We now build the Lorentz objects and the coupling objects*)
   lorentzstruc = DeleteCases[Flatten[verts], Except[(LorentzObject|CouplingObject)[__]]];
   couplstruc = Cases[lorentzstruc, CouplingObject[__]];
   lorentzstruc = Complement[lorentzstruc, couplstruc];


   PY$LorentzObjects={CreateLorentzObjectName[#1[[1]]], #1}& @@@ List /@ Union[Flatten[lorentzstruc]];
   PY$CouplObjects = {"GC_" <> ToString[#2], #1}& @@@ MapIndexed[Join, List /@ Union[Flatten[couplstruc]]];

   (* We build the replacement rules for the reverse replacement *)
   lorobjrules=Reverse /@ (Rule @@@ PY$LorentzObjects);
   couplobjrules=Reverse /@ (Rule @@@ PY$CouplObjects);




   (* and perform the replacements *)
   verts= verts //. Join[lorobjrules, couplobjrules];

   (* Finally, we just adjust the coupling and Lorentz objects slightly, to include the name *)
   PY$LorentzObjects = Flatten[PY$LorentzObjects /. {name_, LorentzObject[entries__]} :> {LorentzObject[name, entries]}];
   PY$CouplObjects = Flatten[PY$CouplObjects /. {name_, CouplingObject[entries__]} :> {CouplingObject[name, entries]}];


   (* Final touch *)
   PY$LorentzObjects = (PY$LorentzObjects /. L$Eps[indices__] :> Signature[List[indices]]*(L$Eps @@ Sort[List[indices]]))/.PYTimes->Times;(*replacement for PYTimes added by celine*)

   Return[verts];

];


(* ::Section:: *)
(*OptimizePYSplitVertices*)


(* ::Text:: *)
(*OptimizePYSplitVertices[ splitvertex ] takes a vertex that is split into color, lorentz and coupling, and optimized it.*)
(** Open up color and Spin tensor products. This is done via the helper function PYOpenTensorProducts*)
(** Open up L$SP and L$SlashedP and C$ff. This is done via the helper functions PYSPOpen and PYSlashedPOpen and PYffOpen*)
(** It replaces internal contrated indices by indices of the form Index[name, Int[i]]. This is done via the helper function OptmizedIndexNames.*)
(** If combines structures that have identical Color and Lorentz structures, i.e.,*)
(*   { { C1, L1, G1}   ,  {  C1, L1, G2}  , .... }   ->    {{ C1, L1, G1+G2}, .....}*)


(* ::Subsection:: *)
(*PYOpenTensorProducts*)


(* ::Text:: *)
(*PYOpenTensDot[x,y][i,j] opens up TensDot[x,y][i,j] into TensDot[x][i,   intnnn] TensDot[y][intnnn, j].*)


PYMergeIndices[HC[yy_], i1_,i2_] := Conjugate[If[MatchQ[yy, _[___]], Append[Append[yy, i2], i1], yy[i2,i1]]];
PYMergeIndices[yy_?(Head[#]=!=HC&), i1_,i2_] := If[MatchQ[yy, _[___]], Append[Append[yy, i1], i2], yy[i1,i2]];


PYTensDotOpen[xx__, yy_][Index[name_ ,s1__], Index[name_, s2__]] := Block[{newindex = Unique["int$$"]},
 
      PYTensDotOpen[xx][Index[name, s1], Index[name, newindex]] * PYMergeIndices[yy, Index[name, newindex], Index[name, s2]]
];

PYTensDotOpen[xx_][s1_, s2_] := PYMergeIndices[xx, s1, s2];


(* ::Subsection:: *)
(*PYSPOpen*)


(* ::Text:: *)
(*PYSPOpen[p1, p2] return the opened scalar product   L$FV[p1, Index[Lorentz, munnnn] ] * L$FV[p2, Index[Lorentz, munnnn] ].*)


PYSPOpen[p1_, p2_] := Block[{ newindex = Unique["mu$$"] },
   
       L$FV[p1, Index[Lorentz, newindex]] * L$FV[p2, Index[Lorentz, newindex]]

];


(* ::Subsection::Closed:: *)
(*PYffOpen*)


(* ::Text:: *)
(*PYffOpen[a1,a2,a3,a4] return the opened scalar product   C$f[ a1, a2,   annn] * C$f[annn, a3,a4].*)


PYffOpen[a1_, a2_, a3_, a4_] := Block[{ newindex = Unique["a$$"] },
   
       C$f[a1, a2, Index[Gluon, newindex]] * C$f[Index[Gluon, newindex], a3, a4]

];


(* ::Subsection::Closed:: *)
(*PYSlashedPOpen*)


(* ::Text:: *)
(*PYSlashedPOpen[ p , s1, s2 ] returns the opened slashed matrix  L$Ga[munnn, s1,s2] L$FV[p, munnn].*)


PYSlashedPOpen[p_, s1_, s2_] := Block[{newindex = Unique["mu$$"]},

    L$Ga[Index[Lorentz, newindex], s1, s2] * L$FV[p, Index[Lorentz, newindex]]

];


(* ::Subsection:: *)
(*OptmizedIndexNames*)


(* ::Text:: *)
(*In a sum, each term is treated separately, since those terms do not interfere.*)


MakeOptimizedIndexNameRule[Index[name_, val__], {k_}] := Rule[Index[name, val], Index[name, FR$OptInt[k]]];


Options[OptimizeIndexName] = {Strict -> True};


OptimizeIndexName[ expr_, OptionsPattern[] ] := Block[{workexpr = expr, internalindices = {}},

      (* If there is no index, then nothing to be done *)
      If[FreeQ[workexpr, Index[_, Except[Ext[_]], ___]],
         Return[workexpr]
        ];
  
      (* else, start *)
      workexpr = Expand[workexpr];

      (* if expr is a sum, then treat each term separately *)
      If[Head[workexpr] === Plus, 
         Return[OptimizeIndexName /@ workexpr]
        ];

      (* otherwise, read out the internal indices *)
      workexpr /. {Index[name_, Except[Ext[__], val_], opt___] :> (AppendTo[internalindices, Index[name, val, opt]] ; Index[name, val, opt])};
 
      (* Remove double entries *)
      internalindices = Union[internalindices];

      (* Create the replacement list *)
      (* We start by collecting Index[ name ,i ] according to name *)
      internalindices = If[OptionValue[Strict],
            {internalindices},
            GatherByFirstElement[internalindices];
           ];

      (* Now we create a replacement list for each of them , and then join the individual replacement lists*)
      internalindices = Dispatch[Join @@ (MapIndexed[MakeOptimizedIndexNameRule, #]& /@ internalindices)];


      (* Finally apply the replacements *)
      workexpr = workexpr //. internalindices //. FR$OptInt -> Int;
 
      (* Return and exit *)
      Return[workexpr];

];

      

      

     


(* ::Subsection:: *)
(*OptimizePYSplitVertices*)


CollectCouplObject /: CollectCouplObject[a_] + CollectCouplObject[b_] := CollectCouplObject[a+b];
CollectCouplObject /: CollectCouplObject[a_] * (n_?IntegerQ) := CollectCouplObject[a * n];


CollectPYStructures[lists__] := {{lists}[[1,1]], {lists}[[1,2]], Plus @@ {lists}[[All, 3]]};


OptimizePYSplitVertices[splitvertex_, {counter_}] := Block[{svertex = Rest[splitvertex], parts = First[splitvertex], PYTimes},

    (* Set the counter *)
    PY$SplitVertexCounter = counter;

    (* We start by renaming internal contracted indices *)
    svertex = {OptimizeIndexName[#1], MapAt[OptimizeIndexName, #2, 2], Map[OptimizeIndexName, #3]}& @@@ svertex;

    (* Now we collect structures that are the same, using the helper function *)
    svertex = MappedGatherByFirstTwoElements[CollectPYStructures, svertex] /. CouplingObject -> CollectCouplObject /. CollectCouplObject -> CouplingObject;

    (* Now we open the TensDots *)
    svertex = svertex /. {L$TensDot|C$TensDot -> PYTensDotOpen};

    (* Now squares of SP's are flattened out ... *)
    svertex = PowerExpand[Expand[svertex]] /. Power[L$SP[k1_, k2_], n_] :> PYTimes @@ Table[L$SP[k1,k2], {n}];

    (* ... and then opened, together with SlashedP and ff *)
    svertex = svertex /. {L$SP -> PYSPOpen , L$SlashedP -> PYSlashedPOpen, C$ff -> PYffOpen};

    (* last but not least, another renaming of internal indices *)
    svertex = {OptimizeIndexName[#1], MapAt[OptimizeIndexName, #2, 2], Map[OptimizeIndexName, #3]}& @@@ svertex;  

    (* Return and exit *)
    Return[Prepend[svertex ,parts]];

];  

    


(* ::Subsection:: *)
(*OptimizeInteractionOrders*)


(* ::Text:: *)
(*If a coupling consists of more than one term, we have to check that the orders are the same for each term. If not, need to split into different terms.*)
(*We use for this a helper function, that looks at individual structures per vertex*)


OptimizeInteractionOrders[particles_, structures__] := Block[{

    struclist = {structures}

   },
  
   
    (* Proceed term by term via helperfunction *)
    struclist = OptimizeInteractionOrdersInStructure @@@ struclist;

    (* Add back the particles*)
    struclist = Prepend[struclist, particles];

    (* Return and exit *)
    Return[struclist];

];
    
   


OptimizeInteractionOrdersInStructure[color_, lorentz_, coupling_] := Block[{

    coupl = Expand[coupling[[1]]]

    },

    (* coupling is just one term, nothing to be done *)
    If[Head[coupl] =!= Plus,
       Return[{color, lorentz, {coupling}}];
      ];

    (* Get the interaction order for each term in the sum, and collect accordingly *)
    coupl = GatherByFirstElement[{GetIntOrder[#], #}& /@ (List @@ coupl)]; 
    
    (* Recombine *)
    coupl = CouplingObject /@ (Plus @@@ (Map[Last, #]& /@ coupl));

    (* Return and exit *)
    Return[{color, lorentz, coupl}];

];

    




(* ::Section:: *)
(*Particle reordering*)


(* ::Text:: *)
(*Reorders the particles that enter a vertex into IFOF.... U.... V.... S... T.... <whatever>....*)
(**)
(*The reordering is done by constructing a boolean function, that returns True, if two particles are in order, False otherwise, and the sorting is done via Sort [ ].*)
(*Finally, pairs  {anti_fermion_1,  fermion_2} are reordered into {fermion_2, anti_fermion_1}, and the same for ghosts.*)


UnknownFieldTypeQ = ((DiracFieldQ[#] === False) && (MajoranaFieldQ[#] === False) && (GhostFieldQ[#] === False) && (VectorFieldQ[#] === False) && (ScalarFieldQ[#] === False) && (Spin2FieldQ[#] === False)) &


PYParticleOrder[{_?((DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True) || (Spin32FieldQ[#] === True)&), _}] := 1;
PYParticleOrder[{_?(GhostFieldQ[#] === True &), _}] := 2;
PYParticleOrder[{_?(VectorFieldQ[#] === True &), _}] := 3;
PYParticleOrder[{_?(ScalarFieldQ[#] === True &), _}] := 4;
PYParticleOrder[{_?(Spin2FieldQ[#] === True &), _}] := 5;
PYParticleOrder[{_?(UnknownFieldTypeQ[#] === True &), _}] := 6;

PYParticleSortFunction[p1_, p2_] := PYParticleOrder[p1] <= PYParticleOrder[p2];


PYReorderParticles[particles_] := Block[{parts = particles},

   (* First, reorder according to the boolean function *)
   parts = Sort[parts, PYParticleSortFunction];


   (* return and exit*)
   Return[parts];
];


(* ::Text:: *)
(*PYOrderFermions orders the fermions into the IOIO form.*)


(*PYOrderFermions[particles_] := Block[{parts = particles, fermions, nonfermions,
      CanonicalOrder},

      (* CD, 02.10.2011: Change made to comply with MG5 canonical order for *all* fermion chains *)
      CanonicalOrder= {#1,#2}&(*If[(DiracFieldQ[#1[[1]]] === True) && (DiracFieldQ[#2[[1]]] === True) && (AntiFieldQ[#2[[1]]] === True) && Not[(AntiFieldQ[#1[[1]]] === True)],
                         {#1, #2}, {#2,#1}]&; *);


      (* Now, read out the fermionic fields *)
      fermions = Cases[parts, {_?((DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True)&), _}];
      nonfermions = DeleteCases[parts, _?(MemberQ[fermions, #]&)];

      (* Construct the flows *)
      fermions = Join @@ Table[CanonicalOrder[fermions[[i+1]], fermions[[i]]], {i, 1, Length[fermions]-1, 2}];
  
      (* recombine *)
      parts = Join[fermions, nonfermions];

      (* Return and exit *)
      Return[parts];
];*)


PYOrderFermions[particles_,vertex_] := Block[{parts = particles, fermions, nonfermions,
      chainorder = Expand[vertex], hold, structures, newvertex = vertex, rules},

      (* Now, read out the fermionic fields *)
      fermions = Cases[parts, {_?((DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True) || (Spin32FieldQ[#] === True)&), _}];
      nonfermions = DeleteCases[parts, _?(MemberQ[fermions, #]&)];

      (* Get the fermion chain *)
      If[FreeQ[chainorder,Spin], Return[{particles, vertex}]];

      chainorder = If[Head[chainorder] === Plus, List@@chainorder,{chainorder}];

      chainorder=chainorder[[1]];

      chainorder = If[Head[chainorder]===Times,
                      List@@chainorder, {chainorder}
                     ];
      chainorder = Select[chainorder,Not[FreeQ[#,Spin]]&];
      structures = chainorder;

      If[Length[chainorder] > 1,
         Print[Style["Warning: Multi-Fermion operators are not yet fully supported!", Red]]; 
        ];

      chainorder = List @@ (chainorder /. _[___,Index[Spin, i_], Index[Spin,j_]] :> hold[i,j] //. hold[i_,j_]hold[j_,k_]:>hold[i,k]);

      chainorder = chainorder /. hold[Ext[i_],Ext[j_]] :> Sequence[j,i];

      fermions = Table[particles[[i]],{i,chainorder}];

      (* Check if there are IndexDeltas that need to be turned *)
      fermions = Sequence @@@ Inner[TurnFermionicIndexDeltas, Table[dummylist[fermions[[i]], fermions[[i+1]]], {i,1,Length[fermions]-1,2}], structures,List];
      rules = Flatten[Inner[CheckFermionicIndexDeltas, Table[dummylist[fermions[[i]], fermions[[i+1]]], {i,1,Length[fermions]-1,2}], structures,List]];
      newvertex = newvertex /. rules;

      (* recombine *)
      parts = Join[fermions, nonfermions];

      (* Return and exit *)
      Return[{parts, newvertex}];
];

TurnFermionicIndexDeltas[_[psi1_,psi2_],struc_]:= Block[{},

    (* We check if psi1 is an antifield, and if struc is a delta function. If so, reverse the fermions. This does not change anything for the fermion flow*)
    If[Not[MatchQ[struc, IndexDelta[_,_]]], 
       Return[{psi1,psi2}]];
  
    If[AntiFieldQ[psi1[[1]]],
       Return[{psi2,psi1}],
       Return[{psi1,psi2}]
       ];
];

CheckFermionicIndexDeltas[_[psi1_,psi2_], struc_] := Block[{newindexdelta,rule},

    (* We check if the order of the indices of the IndexDelta is the same as for the order of the fermions. 
       This is pure cosmetics, as the index delta does not feel the flow 
       The output is a replacement rule list that contains the reversal, if any *)

        If[Not[MatchQ[struc, IndexDelta[_,_]]], 
           Return[{}]];
    
        newindexdelta = IndexDelta[Index[Spin, Ext[psi2[[2]]]], Index[Spin, Ext[psi1[[2]]]]];
        rule = If[newindexdelta =!= struc, 
                  {struc -> newindexdelta},
                  {}
                  ];

        Return[rule];
];
        
        



(* ::Section:: *)
(*CreateVertexObjectEntry*)


(* ::Subsection:: *)
(*CreateCouplingEntry*)


(* ::Text:: *)
(*CreateCouplingEntry[{i, j, g}], where i and j are integers and g is a string, returns the string*)
(**)
(*{(i,j), g}*)


CreateCouplingEntry[{i_,j_, gc_}] := {"(" <> ToString[i] <> "," <> ToString[j] <> ")",  If[Length[gc] == 1, gc[[1]], PYList[gc]]};
CreateCouplingEntry[{i_,j_, k_, gc_}] := {"(" <> ToString[i] <> "," <> ToString[j] <> "," <> ToString[k] <> ")",  If[Length[gc] == 1, gc[[1]], PYList[gc]]};


(* ::Subsection:: *)
(*CreateVertexObjectEntry*)


(* ::Text:: *)
(*CreateVertexObjectEntry[ list] takes a definition for a single vertex and transforms into the format required by for the Python vertex objects.*)


CreateVertexObjectEntry[list_, {counter_Integer}] := Block[{
     particles = list[[1, All, 1]],
     colors,
     lorentz = Union[#[[2]]& /@ Rest[list]],
     coupl = Rest[list],
     lorentzreverse, colorreverse,
     pyvertex
    },

   (* Remove the CC statements, and the bar for Majoranas, 
      then introduce the particle names, and finally 
      check if we need new Particle object names.
    *)

    particles = MakeIdenticalFermions[particles];
    particles = CreateObjectParticleName[PartNameMG[#]]& /@ particles;


    (* Now, create the color structure strings *)
    coupl = {PythonForm[#1], #2, #3}& @@@ coupl;
    colors = Union[First /@ coupl];
    
    (* We now build replacement rules to identify back the Lorentz and color structures,
       and how they are convolved.
     *)
    lorentzreverse = Rule @@@ MapIndexed[Join, List /@ lorentz];
    colorreverse = Rule @@@ MapIndexed[Join, List /@ colors];

    (* And we then apply these reverse replacements to coupl *)
    coupl = coupl /. colorreverse /. lorentzreverse;

    (* Finally, since a list in Python starts with 0, and not one, we 
       have to rescale everything by one unit 
     *)
     coupl = {#1-1, #2-1, #3}& @@@ coupl;

     (* Add the tags P., L., and C. to the particle, lorentz and coupling objects *)
     particles = ("P." <> # &) /@ particles;
     lorentz = ("L." <> # &) /@ lorentz;
     coupl = {#1, #2, "C." <> #& /@ #3}& @@@ coupl;

     (* We now construct the output object *)
     pyvertex = {{"name",      PYString["V_" <> ToString[counter]]},
                 {"particles", PYList[particles]},
                 {"color",     PYList[PYString /@ colors]},
                 {"lorentz",   PYList[lorentz]},
                 {"couplings", PYShortDictionary[CreateCouplingEntry /@ coupl]}
                 };

     (* Return and exit *)
     Return[pyvertex];
                 
];

    


(* ::Section::Closed:: *)
(*WriteVertexObject*)


(* ::Text:: *)
(*WriteVertexObject[file, vertex, {i}]  write the vertex, which is an output of CreateVertexObjectEntry,*)
(*to file. The vertex object is given the name V_i.*)


WriteVertexObject[file_, vertex_List] := Block[{
     name = StringReplace[vertex[[1,2]], {"'" -> ""}]
     },

     WritePYObject[file, name, "Vertex", vertex];
     WriteString[file, "\n"];

];
   


(* ::Section:: *)
(*WritePYVertices*)


(* ::Text:: *)
(*WritePYVertices[ list ] writes all the vertices in list to vertices.py*)


WritePYVertices[vertexlist_] := Block[{outfile, vertlist=vertexlist, PullOutSingleIOCouplings, VertObj},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# Vertex definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write vertices.py *)
   DeleteFileIfExists["vertices.py"];
   outfile = OpenWrite["vertices.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_vertices, Vertex\n"];
   WriteString[outfile, "import particles as P\n"];
   WriteString[outfile, "import couplings as C\n"];
   WriteString[outfile, "import lorentz as L\n"];
   WriteString[outfile, "\n\n"];

   (* CD, 31.03.2015: With SH and MG5 we decided to go back to one coupling per vertex, not list thereof if they have different InteractionOrders *)
   vertlist = VertObj @@@ vertlist;

   PullOutSingleIOCouplings[vo_]:=Block[{parts=vo[[1]],vertsparts=(List@@Rest[vo]),maxiter, VertTermObj},
      vertsparts = VertTermObj @@@ vertsparts;
      vertsparts = vertsparts /. VertTermObj[as__, clist_List] :> (VertTermObj[as, {#}]& /@ clist);

      maxiter=Max[Length/@vertsparts];
      If[maxiter<2,Return[vo]];
  
      vertsparts=Table[If[Length[#]<i,{},#[[i]]]&/@vertsparts,{i,maxiter}];
      vertsparts=DeleteCases[#,{}]&/@vertsparts;
      vertsparts=vertsparts//.VertTermObj->List;
      vertsparts=VertObj[parts,Sequence@@#]&/@vertsparts;
      Return[vertsparts]
   ];

   vertlist = Flatten[(PullOutSingleIOCouplings /@ vertlist)];
   vertlist = List @@@ vertlist;
   (* CD End*)

   WriteVertexObject[outfile, #1]& /@ MapIndexed[CreateVertexObjectEntry, vertlist];
   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[vertlist] == 1, "1 vertex", ToString[Length[vertlist]] <> " vertices"] <> " written."];
   TestQ[FileExistsQ, "vertices.py", "   * vertices.py written.", "   * vertices.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 


(* ::Section:: *)
(*IntOrder*)


(* ::Text:: *)
(*IntOrder[ x ] returns the interaction order of a parameter,  in the form QED^2. If no Interaction order was defined, then it returns 1.*)


IntOrder[x_] := If[(ParamIntOrder[x] =!= {}) && ValueQ[ParamIntOrder[x]],
     Times @@ (Power @@@ ParamIntOrder[x]),
     1
     ];


(* ::Text:: *)
(*GetIntOrder[expr] determines the interorder of expr.*)
(**)
(*If the Head of expression is a Symbol, then return the interaction order*)
(*If it is number, return 1.*)


GetIntOrder[expr_?(Head[#] === Symbol&)] := IntOrder[expr];
GetIntOrder[_?NumericQ] := 1;
GetIntOrder[Conjugate[x_]] := GetIntOrder[x];
GetIntOrder[Re[x_]] := GetIntOrder[x];
GetIntOrder[Im[x_]] := GetIntOrder[x];
GetIntOrder[If[x_,1,0]] := 1;
GetIntOrder[If[x_,0,1]] := 1;


(* ::Text:: *)
(*If is is a product, then multiply the interaction orders*)


GetIntOrder[expr_Times] := GetIntOrder /@ expr;
GetIntOrder[expr_Power] := (MapAt[GetIntOrder, expr, 1]/.{Sqrt[gioa_^gion_]->gioa^(gion/2),(gioa_^giob_)^gioc_->gioa^(giob*gioc)});


(* ::Text:: *)
(*If it is a sum, then check if all term sin the sum have the same order.*)
(*If not, the variable PY$IntOrderSum is turned to True.*)
(*In any case, the interaction order of the first term is returned.*)


GetIntOrder[expr_Plus] := Block[{
    ios = Union[GetIntOrder /@ (List @@ Expand[expr])]
   },

    If[Length[ios != 1], PY$IntOrderSum = True];

     Return[ios[[1]]];

];
    


GetIntOrder[Eps[args__]]:=1;


(* ::Text:: *)
(*For functions, use the interaction order is 1;*)
(**)


PY$GetIntFunctions = {Sin, Cos, Tan, Cot, Sec, Csc,
                      Sinh, Cosh, Tanh, Coth, Sech, Csch,
                      ArcSin, ArcCos, ArcTan, ArcCot, ArcSec, ArcCsc,
                      ArcSinh, ArcCosh, ArcTanh, ArcCoth, ArcSech, ArcCsch,
                      Log,RenormLog};

GetIntOrder[_?(MemberQ[PY$GetIntFunctions, #]&)[_]] := 1;


(* ::Subsection:: *)
(*CreateInteractionOrderList*)


(* ::Text:: *)
(*CreateInteractionOrderList[ expr ], where expr is a monomial QED^n QCD^m... , creates the matrix*)
(**)
(*{ { 'QED', n}, *)
(*   { 'QCD', m},*)
(*    ...}*)


CreateInteractionOrderList[monomial_] := Block[{working = monomial},

   (* We first need to distinguish between monomials that are products, and those that are pure powers.*)
   working = If[Head[working] === Times,
                List @@ working,
               (* else *)
               {working}
               ];

   (* We now have a list of powers, that is replaced by {a,b} *)
   working = Replace[working, x_^n_. :> {x,n}, 1];

   (* Return and exit *)
   Return[working];

];


(* ::Subsection:: *)
(*CheckForNegativeInteractionOrders*)


(* ::Text:: *)
(*CheckForNegativeInteractionOrders[  ]  checks if an interactio order is negative, and if so, acts according to the option specified in the interface.*)
(**)
(** NegativeInteractionOrders -> Automatic : They are allowed, but warning printed to screen and logfile.*)
(** NegativeInteractionOrders -> True :  No check.*)
(** NegativeInteractionOrders -> False : All negative interaction orders are put to 0, and warning printed to screen and logfile.*)


CheckForNegativeInteractionOrders[matrix_?MatrixQ, coupling_] := CheckForNegativeInteractionOrders[#, coupling]& /@ matrix;


CheckForNegativeInteractionOrders[vector_?VectorQ, coupling_] := Block[{name, order},

    If[Length[vector] == 2,
       {name, order} = vector
      ];

    If[order <= 0,
       AppendTo[GenInt$LogFile, "   >>>> Non positive interaction order "<> ToString[name] <> " found for coupling " <> coupling <> "."];

       If[PY$NegativeInteractionOrder === False,
          order = 0
         ];

       If[Not[PY$NegativeInteractionOrderFound],
          PY$NegativeInteractionOrderFound = True;
          Print[Style["Warning: Non positive interaction order "<> ToString[name] <> ".\n                This might reduce the efficiency of certain matrix element generators.\n                See logfile for more details.", Red]]
          ];
       ];

     Return[{name, order}];

];

       




(* ::Section::Closed:: *)
(*CreateLorentzObjectEntry*)


(* ::Subsection:: *)
(*CreateLorentzObjectName*)


(* ::Text:: *)
(*CreateLorentObjectName[ { i, j, k, l, ...} ], where i,j,k,... are integers, returns a string with*)
(**)
(*1 -> S*)
(*-1 -> U*)
(*2 -> F*)
(*3 -> V*)
(*4 -> R*)
(*5 -> T*)
(**)


ConvertSpinToString[-1] = "U";
ConvertSpinToString[1] = "S";
ConvertSpinToString[2] = "F";
ConvertSpinToString[3] = "V";
ConvertSpinToString[4] = "R";
ConvertSpinToString[5] = "T";


CreateLorentzObjectName[list_List] := Block[{counter, name},

     (* Increase the counter, if exists. If not, create it *)
     counter = ToString[If[ValueQ[PYLorentzStrucCounter[list]],
                  ++PYLorentzStrucCounter[list],
                  PYLorentzStrucCounter[list] = 1
                  ]];

     name = StringJoin @@ (Append[ConvertSpinToString /@ list, counter]);


     Return[name];

];

    


(* ::Subsection:: *)
(*CreateLorentzObjectEntry*)


(* ::Text:: *)
(*CreateLorentzObjectEntry[ LorentzObject] takes a definition for a single Lorentz object and transforms into the format required by for the Python vertex objects.*)


CreateLorentzObjectEntry[LorentzObject[name_, spins_, expr_, formfs_]] := Block[{outlist, 
   exp = expr},

   (* We have to open the tensor products *)
   outlist = LorentzObject[name, DeleteCases[{{"name",        PYString[name]},
                                  {"spins",       PYList[ToString /@ spins]},
                                  {"structure",   PYString[PythonForm[exp]]},
                                  {"formfactors", PYList[ ("ForFac."<>#&)/@ PythonForm/@formfs]}}, {"formfactors","[]"}]
                          ];

   Return[outlist];

];


(* ::Section::Closed:: *)
(*WriteLorentzObject*)


(* ::Text:: *)
(*WriteLorentzObject[file, Lorentzobject]  writes the Lorent object, which is an output of CreateLorentzObjectEntry,*)
(*to file. *)


WriteLorentzObject[file_, LorentzObject[name_String, entries_List]] := Block[{},

     WritePYObject[file, name, "Lorentz", entries];
     WriteString[file, "\n"];

];
   


(* ::Section::Closed:: *)
(*WritePYLorentz*)


(* ::Text:: *)
(*WritePYLorentz[ list ] writes all the vertices in list to lorentz.py*)


WritePYLorentz[list_] := Block[{outfile},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# Lorentz structure definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write vertices.py *)
   DeleteFileIfExists["lorentz.py"];
   outfile = OpenWrite["lorentz.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_lorentz, Lorentz\n"];
   WriteString[outfile, "\nfrom function_library import ", Sequence @@ Riffle[PY$NewCMathFunctions, ", "], "\n"];
   WriteString[outfile, "try:\n"];
   WriteString[outfile, "   import form_factors as ForFac \n"];
   WriteString[outfile, "except ImportError:\n"];
   WriteString[outfile, "   pass\n"];
   WriteString[outfile, "\n\n"];

   WriteLorentzObject[outfile, #]& /@ (CreateLorentzObjectEntry /@ list);
   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[list] == 1, "1 lorentz structure", ToString[Length[list]] <> " lorentz structures"] <> " written."];
   TestQ[FileExistsQ, "lorentz.py", "   * lorentz.py written.", "   * lorentz.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 


(* ::Section:: *)
(*CreateCouplingObjectEntry*)


(* ::Text:: *)
(*CreateCouplingObjectEntry[ CouplingObject] takes a definition for a single coupling object and transforms into the format required by for the Python vertex objects.*)


CreateCouplingObjectEntry[object_R2CouplingObject] := CreateCouplingObjectEntry[CouplingObject @@ object];


CreateCouplingObjectEntry[object_UVCouplingObject] := CreateCouplingObjectEntry[CouplingObject @@ object, True];


CreateCouplingObjectEntry[CouplingObject[name_, expr_], laurentseries_:False] := Block[{
    outlist, 
    order, 
    exp = expr //. ParamRules
    },

   (* We compute the interaction order *)
   PY$IntOrderSum = False;
   order = GetIntOrder[exp]/.Power[Power[muf_,2],Rational[n_,2]]->Power[muf,n];

   (* If PY$IntOrderSum is True, then there was a problem with a sum (e.g., gs+ ee).
      A warning is printed.
   *)
   If[PY$GenIntSum,
      Message[IntOrder::Sum, name];
      AppendTo[GenInt$LogFile, "   >>>> Different terms in the coupling " <> name <> " have different interaction orders."];
      AppendTo[GenInt$LogFile, "        Only the interaction order of the first term is kept."];
     ];


   (* we now map eveything to a matrix *)
   order = CreateInteractionOrderList[order];

   order = CheckForNegativeInteractionOrders[order, name];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

   (* We need to distinguish whether the value is a Laurent series or not.
      If so, we need to convert into a dictionary with the coefficients *)
   exp = If[laurentseries,
            CreatePYLaurentSeriesDic[exp],
            PYString[PythonForm[exp]]
           ];
      
   

   (* and finally we introduce the Python notation *)
   order = {PYString[ToString[#1]], ToString[#2]}& @@@ order;

   outlist = CouplingObject[name, {{"name",      PYString[name]},
                                  {"value",      exp},
                                  {"order",      PYShortDictionary[order]}}
                          ];

   Return[outlist];

];


(* ::Subsection:: *)
(*CreatePYLaurentSeriesDic*)


LaurentSeriesCoefficientExtractor[pow_, sum_Plus] := LaurentSeriesCoefficientExtractor[pow, #]& /@ sum;
LaurentSeriesCoefficientExtractor[pow_, coeff_?(FreeQ[#,FR$Eps]&) FR$Eps^n_] := LaurentSeriesCoefficientExtractor[pow FR$Eps^n, coeff];
LaurentSeriesCoefficientExtractor[pow_, exp_?(Not[FreeQ[#,FR$Eps]]&)] := LaurentSeriesCoefficientExtractor[pow , Normal[Series[exp,{FR$Eps,0,1}]]];


CombineLaurentSeriesCoefficients[list_List] := Block[{
    lisi = list,
    eppower = list[[1,1]]
    },
    
    (* Pull out the exponent of epsilon *)
    eppower = Which[eppower === 1, 0, 
                    eppower === FR$Eps, 1,
                    MatchQ[eppower, Power[FR$Eps, _]], eppower[[2]],
                    True, Message[NLO::Failed]
                    ];

    Return[{eppower, Plus@@list[[All,2]]}];
];


CreatePYLaurentSeriesDic[laurentseries_] := Block[{
   laurser = Expand[laurentseries]
   },

   (* We pull out the powers of FR$Eps form the different terms and store them in a list *)
   laurser = LaurentSeriesCoefficientExtractor[1, laurser];
   laurser = Expand[laurser];
   laurser = If[Head[laurser] === Plus,
                List @@ laurser,
                {laurser}
                ];

   (* We then collect those coefficients that correspond to the same power of FR$Eps *)
   laurser = If[Length[laurser] > 1,
                GatherByFirstElement[laurser],
                {laurser}
                ];
   laurser = CombineLaurentSeriesCoefficients /@ laurser;

   (* Check if the coefficients do no longer depend on FR$Eps. If they do, the coefficients were not properly expanded *)
   If[Not[FreeQ[laurser, FR$Eps]],
      Message[NLO::FR$Eps]
     ];

   (* Sort the coefficients into ascending order *)
   laurser = Sort[laurser, #1[[1]] <= #2[[1]]&];

   (* We reintroduce the renomalisation logs *)
   laurser = laurser /.RenormLog[1]->0 //. RenormLog[x_] :> Log[x(*/FR$MU*)];
   laurser[[All,2]]=GatherIf/@laurser[[All,2]]/.If[a_==0,b_,c_]->If[a,c,b]; (*Special Madgraph case*)
   laurser=laurser/.Log->RenormLog;
   
   (* Convert everything to strong *)
   laurser = {ToString[#1], PYString[PythonForm[#2]]}& @@@ laurser;
   (* Create the dictionary *)
   laurser = PYShortDictionary[laurser];

   Return[laurser];
];
   

   


(* ::Section:: *)
(*WriteCouplingObject*)


(* ::Text:: *)
(*WriteCouplingObject[file, Couplingobject]  writes the Lorent object, which is an output of CreateCouplingObjectEntry,*)
(*to file. *)


WriteCouplingObject[file_, CouplingObject[name_String, entries_List]] := Block[{},


     WritePYObject[file, name, "Coupling", entries];
     WriteString[file, "\n"];

];
   


(* ::Section:: *)
(*WritePYCouplings*)


(* ::Text:: *)
(*WritePYCouplings[ list ] writes all the vertices in list to lorentz.py*)


WritePYCouplings[list_] := Block[{outfile},

   (* Prepare the log file *)
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, "# Coupling definitions"];
   AppendTo[GenInt$LogFile, "#"];
   AppendTo[GenInt$LogFile, ""];

   (* Write couplings.py *)
   DeleteFileIfExists["couplings.py"];
   outfile = OpenWrite["couplings.py"];

   WritePYFRHeader[outfile];
   WriteString[outfile, "from object_library import all_couplings, Coupling\n"];
   WriteString[outfile, "\nfrom function_library import ", Sequence @@ Riffle[PY$NewCMathFunctions, ", "], "\n\n"];
   WriteString[outfile, "\n\n"];

   WriteCouplingObject[outfile, #]& /@ (CreateCouplingObjectEntry /@ list);
   
   Close[outfile];
  
   AppendTo[GenInt$LogFile, "   * " <> If[Length[list] == 1, "1 coupling", ToString[Length[list]] <> " couplings"] <> " written."];
   TestQ[FileExistsQ, "couplings.py", "   * couplings.py written.", "   * couplings.py not written"];

   (* Write the log file *)
   WriteToLogFile[GenInt$LogFileName];

]; 


(* ::Section:: *)
(*MakeOutgoingParticles*)


(* ::Text:: *)
(*MakeOutgoingParticles[vertex] takes a  vertex as an input, and changes the convention from ingoing to outgoing, i.e., all particles are replaced by their anitparticles, and  FV  ->  -FV, SlashedP  ->  -SlahedP. Note that SP ->  SP.*)


MakeOutgoingParticles[vertex_] := Block[{
   particles = vertex[[1]],
   expr = vertex[[2]]
   },

   (* Replace Particles by antiparticles *)
   particles = MapAt[anti[MakeIdenticalFermions[#]]&, #, 1]& /@ particles;

   (* Reverse momenta *)

   expr = expr /. {FV[i_, mu_] :> -FV[i, mu],
                   SlashedP[i_,inds__] :> -SlashedP[i, inds],
                   TensDot[ga1___][inds__] :> (-1)^Count[{ga1},_SlashedP,\[Infinity]]TensDot[ga1][inds]/;Not[FreeQ[ga1,SlashedP]]};

   (* Return and exit*)
   Return[{particles, expr}];

];


(* ::Section::Closed:: *)
(*Optimization*)


(* ::Subsection:: *)
(*OptimizeColors*)


(* ::Text:: *)
(*OptimizeColors[particles,  vertex] reorganizes vertex according to*)
(**)
(*{{C1, L1, G1}, {C2, L1, G1} }  ->  {{C1+C2, L1, G1}}*)


OptimizeColors[particles_, vertex___] := Block[{
    VertObjPart, verts = {vertex}
    },

    verts = VertObjPart @@@ verts;
 
    verts = verts //. {vv1___, VertObjPart[col1_, lor1_, coup1_], vv2___, VertObjPart[col2_, lor1_, coup1_], vv3___} :> {vv1, VertObjPart[col1+col2, lor1, coup1], vv2, vv3};

    verts = List @@@ verts;
    
    Return[Prepend[verts, particles]];

];


(* ::Subsection:: *)
(*FindZeroCouplings*)


(* ::Text:: *)
(*FindZeroCouplings[PY$CouplObjects] checks the object in PY$CouplObjects whether the numerical value of the second component is below 10^(-15). If so, it creates a rule*)
(**)
(*"GC_i"  ->  0*)
(**)
(*and returns the list of these rules*)


FindZeroCouplings[list_] := Rule @@@ Select[list, NumericalValue[#[[2]]] < 10^(-15)&];


(* ::Subsection:: *)
(*OptimizeUFOVertices*)


(* ::Text:: *)
(*OptimizeUFOVertices[vertices, PY$CouplObjects, PY$LorentzObjects] optimizes the vertices, by removing those vertices which have zero couplings. Finally, it cleans PY$CouplObjects and PY$LorentzObjects such that zero couplings are deleted, and Lorentz structures that are no longer used are also removed.*)


OptimizeUFOVertices[vertices_List, couplings_List, lorentz_List] := Block[{
    newvertices, newcouplings, newlorentz,
    zeroes = FindZeroCouplings[couplings],
    usedlorentz
    },

    (* Replace the couplings that are zero, and remove them *)
    newvertices = DeleteCases[#, {_, _, 0}]& /@ (vertices /. zeroes);

    (* Clean up the couplings *)
    newcouplings = DeleteCases[couplings /. zeroes, CouplingObject[0, _]];

    (* Find the Lorentz structures that are still in use *)
    usedlorentz = Union[#[[2]]& /@ (Sequence @@@ (Rest /@ newvertices))];

    (* and then remove those that are no longer used *)
    newlorentz = Select[lorentz, MemberQ[usedlorentz, #[[2]]]&];

    (* output *)
    PY$CouplObjects = newcouplings;
    PY$LorentzObjects = newlorentz;
    Return[newvertices];

];
    
    

    
   

    



(* ::Section::Closed:: *)
(*New Color structures*)


IsAntiParticleQ[Except[CC[_], field_]] := AntiFieldQ[field];
IsAntiParticleQ[CC[field_]] := Not[AntiFieldQ[field]];


(* ::Subsection:: *)
(*MakeAntiColour*)


(* ::Text:: *)
(*MakeAntiColour[colour, field] returns colour if field is an antifield, and colur otherwise.*)


MakeAntiColour[Colour, field_] := If[IsAntiParticleQ[field] === True, AntiColour, CColour];
MakeAntiColour[Sextet, field_] := If[IsAntiParticleQ[field] === True, AntiSextet, SSextet];


(* ::Subsection:: *)
(*RenameAntiColourStructures*)


(* ::Text:: *)
(*RenameAntiColourStructures[particles, vertex] applies RenameAntiColour to each color structure*)


RenameAntiColourStructures[particles_, vertex__] := Block[{
    verts = {vertex}, VertObjPart
    },

    verts = VertObjPart @@@ verts;

    verts = verts /. VertObjPart[col_, lor_, coup_] :> VertObjPart[RenameAntiColour[col, particles], lor, coup];

    verts = List @@@ verts;

    Return[Prepend[verts, particles]];

];


(* ::Subsection:: *)
(*RenameAntiColour*)


(* ::Text:: *)
(*RenameAntiColour[color_struc, particles] intoduces antitriplet (sextet) notations, like EpsBar, and K6Bar in color_struc*)


RenameAntiColour[colorstruc_, particles_List] := Block[{
    colstruc = Expand[colorstruc]},

   (* If no Eps, and no K6, nothing to be done *) 
   If[FreeQ[colstruc, C$Eps|C$K6], 
      Return[colstruc];
     ];

   (* If sum, start over *)
   If[Head[colstruc] === Plus, 
      Return[RenameAntiColour[#, particles]& /@ colstruc];
     ];

   (* Start with fundamental or sextet matrices, as well as K6 *)
   colstruc = colstruc /. {C$T[a_, Index[Colour, i1_], Index[Colour, i2_]] :> C$T[a, Index[CColour, i1], Index[AntiColour, i2]],
                           C$T6[a_, Index[Sextet, i1_], Index[Sextet, i2_]] :> C$T6[a, Index[SSextet, i1], Index[AntiSextet, i2]],
                           C$K6[Index[Sextet, a_], Index[Colour, b_], Index[Colour, c_]] :> C$K6[Index[SSextet, a], Index[AntiColour, b], Index[AntiColour, c]],
                           C$K6bar[Index[Sextet, a_], Index[Colour, b_], Index[Colour, c_]] :> C$K6[Index[AntiSextet, a], Index[CColour, b], Index[CColour, c]]
                           };


   (* Start with the external particles *)
   colstruc = colstruc /. {Index[Colour, Ext[k_]] :> Index[MakeAntiColour[Colour, particles[[k, 1]]], Ext[k]],
                           Index[Sextet, Ext[k_]] :> Index[MakeAntiColour[Sextet, particles[[k, 1]]], Ext[k]]
                           };


   (* Now the remaining internal ones *)
   colstruc = colstruc //. {f_[inds1___, Index[Colour, kk_], inds2___]g_[ind3___, Index[CColour, kk_], inds4___] :> f[inds1, Index[AntiColour, kk], inds2]g[ind3, Index[CColour, kk], inds4],
                 f_[inds1___, Index[Colour, kk_], inds2___]g_[ind3___, Index[AntiColour, kk_], inds4___] :> f[inds1, Index[CColour, kk], inds2]g[ind3, Index[AntiColour, kk], inds4],
                 f_[inds1___, Index[Sextet, kk_], inds2___]g_[ind3___, Index[SSextet, kk_], inds4___] :> f[inds1, Index[AntiSextet, kk], inds2]g[ind3, Index[SSextet, kk], inds4],
                 f_[inds1___, Index[Sextet, kk_], inds2___]g_[ind3___, Index[AntiSextet, kk_], inds4___] :> f[inds1, Index[SSextet, kk], inds2]g[ind3, Index[AntiSextet, kk], inds4],
                 C$Eps[inds1___, Index[Colour, kk1_], inds2___, Index[CColour, kk2_], inds3___] :> C$Eps[inds1, Index[CColour, kk1], inds2, Index[CColour, kk2], inds3],
                 C$Eps[inds1___, Index[Colour, kk1_], inds2___, Index[AntiColour, kk2_], inds3___] :> C$Eps[inds1, Index[AntiColour, kk1], inds2, Index[AntiColour, kk2], inds3],
                 C$Eps[inds1___, Index[CColour, kk1_], inds2___, Index[Colour, kk2_], inds3___] :> C$Eps[inds1, Index[CColour, kk1], inds2, Index[CColour, kk2], inds3],
                 C$Eps[inds1___, Index[AntiColour, kk1_], inds2___, Index[Colour, kk2_], inds3___] :> C$Eps[inds1, Index[AntiColour, kk1], inds2, Index[AntiColour, kk2], inds3]
                };

    (* If everything went fine, we introduce the new notations *)
    colstruc = colstruc //. {C$Eps[Index[AntiColour, i1_], Index[AntiColour, i2_], Index[AntiColour, i3_]] :> C$EpsBar[Index[AntiColour, i1], Index[AntiColour, i2], Index[AntiColour, i3]]
                   };

    (* Clean up, and return *)
    colstruc = colstruc //. {Index[CColour|AntiColour, kk_] :> Index[Colour, kk],
                            Index[SSextet|AntiSextet, kk_] :> Index[Sextet, kk]
                           };
     Return[colstruc];

];


(* ::Section:: *)
(*If gathering*)


GatherIf[exp_]:=Module[{iflist},
  iflist=Union[Cases[exp,_If,\[Infinity]]/.If[a_,0,1]->If[a,1,0]];
  If[Length[iflist]>0,
    GatherIf[Coefficient[Coefficient[exp,iflist[[1]],0],iflist[[1]]/.If[a_,1,0]->If[a,0,1],0]]+
    If[iflist[[1,1]],Evaluate[GatherIf[Coefficient[exp,iflist[[1]] ]]],Evaluate[GatherIf[Coefficient[exp,iflist[[1]]/.If[a_,1,0]->If[a,0,1] ]]]],
    exp
  ]
];
