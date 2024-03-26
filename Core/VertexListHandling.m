(* ::Package:: *)

(* ::Title:: *)
(*Vertex List handling*)


(* ::Section:: *)
(*Relabelling of internal indices*)


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


(* ::Section:: *)
(*New sorting routine*)


(* ::Subsection::Closed:: *)
(*PrecollectVerticesAccordingToParticles*)


(* ::Text:: *)
(*PrecollectVerticesAccordingToParticles[vertexlist] returns a list of vertexlists, each individual vetrtexlist in the list being all the vertices that have the same particle content, after canonical ordering and identification of charge conjugated fermions.*)


PrecollectVerticesAccordingToParticles[vertexlist_List] := GatherBy[MakeIdenticalFermions /@ vertexlist, Sort[First/@#[[1]]]&];


(* ::Subsection::Closed:: *)
(*GetFermionChainFromVertex*)


(* ::Text:: *)
(*Takes a vertex, and splits it into the different fermion chain structures:*)
(**)
(*{ {coupling1, chain1}, ... }*)


GetFermionChainFromVertex[vertex_] := Block[{
   vert = Expand[vertex],
   ChainExtractor
   },

   (* This is the helpter function, an extractor, that pulls out the terms that have a spin index *)
   ChainExtractor[aa_, expr_. * tens_[inds___, Index[Spin,s1_], Index[Spin, s2_]]] := ChainExtractor[aa* tens[inds, Index[Spin, s1], Index[Spin, s2]], expr];
   
   (* Main routine *)
   vert = If[Head[vert] === Plus,
             List @@ vert,
             {vert}
            ];

   vert = GatherByFirstElement[List @@@ (ChainExtractor[1, #]& /@ vert)];
   vert = {Plus @@ #[[All,2]], #[[1,1]]}& /@ vert;

   Return[vert];

];

   


(* ::Subsection::Closed:: *)
(*Particle extraction*)


ExtractGhostsFromParticleList[list_List] := Cases[list, {_?(GhostFieldQ[#] === True&), _}];
ExtractFermionsFromParticleList[list_List] := Cases[list, {_?((FermionQ[#] === True) && (GhostFieldQ[#] =!= True) &), _}];
ExtractBosonsFromParticleList[list_List] := Cases[list, {_?(FermionQ[#] =!= True &), _}];


(* ::Subsection::Closed:: *)
(*FindFermionInList*)


FindFermionInList[fermions_List, {ferm_, _}] := Block[{
   sign0 = 1,
   pos,
   ferms = First /@ fermions
   },

   pos = Sort[Position[ferms, ferm]][[1,1]];
   
   If[pos === {},
      Print["Something went wrong. Could not reorder fermions."];
      ];
   
   (* -1 changed to +1 by celine *)
   sign0 = (1)^(pos+1);
   pos = fermions[[pos]];

   Return[{pos,sign0}];
];


(* ::Subsection::Closed:: *)
(*FindNewFermionOrder*)


FindNewFermionOrder[fermions_List, referenceorder_List] := Block[{
    sign = 1, 
    fermi = fermions, 
    ref = referenceorder,
    new = {}, work
    },

    While[ref =!= {},
          work = FindFermionInList[fermi, ref[[1]]];
          sign = sign * work[[2]];
          AppendTo[new, work[[1]]];
          fermi = DeleteCases[fermi, work[[1]]];
          ref = Rest[ref];
          ];

     Return[{new,sign}];

];
          
    


(* ::Subsection::Closed:: *)
(*ReorderFermionOperators*)


ReorderFermionOperators[{particles_List, vertex_}, referenceorder_List] := Block[{
    fermions = ExtractFermionsFromParticleList[particles],
    bosons, neworder,newvertex
    },

    bosons = Complement[particles, fermions];

    neworder = FindNewFermionOrder[fermions, referenceorder];

    newvertex = RelabelExt[{Join[neworder[[1]],bosons], neworder[[2]]*vertex}];

    Return[newvertex];

]; 


(* ::Subsection::Closed:: *)
(*SortTypeVertexParticles*)


(* ::Text:: *)
(*SortVertexParticles[ list ] sorts the particles in the vertex into a canonical order. The input list is an element of the output list from PrecollectVerticesAccordingToParticles.*)


SortVertexParticles[list_List] := Block[{referenceparts = list[[1,1]],
    ghosts, fermions, bosons,
    newreference, vertices = list[[All,2]], 
    all = list,
    parts,
    vert},

    ghosts = ExtractGhostsFromParticleList[referenceparts];
    fermions = ExtractFermionsFromParticleList[referenceparts];
    bosons = Sort[ExtractBosonsFromParticleList[referenceparts]];


    If[fermions === {},
       (* If there is just one contribution, nothing to be done *)
        If[Length[list]==1,
           Return[RelabelExt[{Join[ghosts, bosons], list[[1,2]]}]]
           ];
        (* If there are multiple contributions, add them up *)
        all=(RelabelExt[{Join[ExtractGhostsFromParticleList[#1], Sort[ExtractBosonsFromParticleList[#1]]], #2}]&)@@@all;
        If[Length[Union[all[[All,1]]]]<2,Return[{ all[[1,1]],Plus@@all[[All,2]] }], Print[Style["Error : vertex not merged correctly",Red]];];
       ];

    (* We can now already put the bosons into the right order *)
    all = RelabelExt[{Join[ExtractFermionsFromParticleList[#1],Sort[ExtractBosonsFromParticleList[#1]],ExtractGhostsFromParticleList[#1]],#2}]& @@@ all;

    (* For the fermions, we want to go to same flows in all contributions.
       In other words, we need to reorder the creation operators into the same
       order. This involves a sign *)
    all = ReorderFermionOperators[#, fermions]& /@ all;

    (* At this stage all the contributions have the same order for the creation
       operators, so we cann add them up *)
    If[Length[all] > 1,
       all = If[Length[Union[all[[All,1]]]]<2, RelabelExt[{all[[1,1]], Plus @@ all[[All,2]]}], Print[Style["Error : vertex not merged correctly",Red]];];,
       all = all[[1]]
      ];

    (* Now we have to bring the flows in the correct order
       We start by extracting all the flow structures *)
    parts = all[[1]];
    vert = all[[2]];

    vert = GetFermionChainFromVertex[vert];
  
    (* Now, reorder the flows *)
    If[$UseFierzIdentities,
       vert = {#1, ReorderVertexFermionFlows[#2, Length[fermions]]}& @@@ vert
      ];
  
    (* Finally, put is all together again *)
    vert = Plus @@ (Times @@@ vert);

    all = {parts, vert};

    Return[all];

];
    

    


ReorderVertexFermionFlows[flowstructure_, num_] := Block[{
   fullstruc = Expand[flowstructure],
   refstruc, MyExt, mu, nu, n = num, 
   GaSi, Gam5
   },

   (* Some definitions *)
   GaSi[mumu_, nunu_, ss_, rr_] := I/2*TensDot[Ga[mumu], Ga[nunu]][ss,rr] - I/2*TensDot[Ga[nunu], Ga[mumu]][ss,rr];
   Gam5[mumu_, ss_, rr_] := TensDot[Ga[mumu],Ga[5]][ss,rr];

   (* We take the first term to work out which operations to do.
      For all other terms it is anyhow the same *)
   refstruc = If[Head[fullstruc] === Plus,
                 fullstruc[[1]],
                 fullstruc
                 ];

    (* 
       Check if the last index (n) is contracted to the right.
       If not, the flow needs to be reversed.
       Note the minus sign in the flow reversal, coming form the fact that we are at the level of wave functions.
     *)
    If[FreeQ[refstruc, _[___,Index[Spin,Ext[n]]]],
       fullstruc = fullstruc /. {PP_[inds___, Index[Spin, Ext[n]], Index[Spin, Ext[n1_]]] :> -PRIVATE`CGa[PP[inds, Index[Spin, Ext[n1]], Index[Spin, Ext[n]]]],
                                 IndexDelta[Index[Spin, Ext[n]], Index[Spin, Ext[n1_]]] :> - IndexDelta[Index[Spin,Ext[n1]], Index[Spin, Ext[n]]]};
       refstruc = If[Head[fullstruc] === Plus,
                     fullstruc[[1]],
                     fullstruc
                    ];
      ];

    (* Next, check if the index (n-1) is contracted to the left *)
    If[FreeQ[refstruc, _[___,Index[Spin,Ext[n-1]],_]],
       fullstruc = fullstruc /. {PP_[inds___, Index[Spin, Ext[n1_]], Index[Spin, Ext[n-1]]] :> -PRIVATE`CGa[PP[inds, Index[Spin, Ext[n-1]], Index[Spin, Ext[n1]]]],
                                 IndexDelta[Index[Spin, Ext[n]], Index[Spin, Ext[n1_]]] :> - IndexDelta[Index[Spin,Ext[n1]], Index[Spin, Ext[n]]]};
       refstruc = If[Head[fullstruc] === Plus,
                     fullstruc[[1]],
                     fullstruc
                    ];
      ];

    (* Next, check if n and (n-1) are constracted *)
    If[FreeQ[refstruc, _[___,Index[Spin,Ext[n-1]],Index[Spin,Ext[n]]]],
       (* Rename those we want to recontract *)
        fullstruc = fullstruc /. {Ext[n-1] :> MyExt[n-1], Ext[n] :> MyExt[n]};
       (* Now, contract with the Fierz identity for the identity *)
        mu = Unique["mu"];
        nu = Unique["nu"];
        fullstruc = Expand[fullstruc (IndexDelta[Index[Spin,Ext[n-1]], Index[Spin,Ext[n]]]IndexDelta[Index[Spin,MyExt[n]], Index[Spin,MyExt[n-1]]] + 
                              Ga[Index[Lorentz, mu], Index[Spin,Ext[n-1]], Index[Spin,Ext[n]]]Ga[Index[Lorentz, mu],Index[Spin,MyExt[n]], Index[Spin,MyExt[n-1]]] + 
                              Sig[Index[Lorentz, mu], Index[Lorentz, nu], Index[Spin,Ext[n-1]], Index[Spin,Ext[n]]]Sig[Index[Lorentz, mu],Index[Lorentz, nu],Index[Spin,MyExt[n]], Index[Spin,MyExt[n-1]]]/2 - 
                              Gam5[Index[Lorentz, mu], Index[Spin,Ext[n-1]], Index[Spin,Ext[n]]]Gam5[Index[Lorentz, mu],Index[Spin,MyExt[n]], Index[Spin,MyExt[n-1]]] + 
                              Ga[5, Index[Spin,Ext[n-1]], Index[Spin,Ext[n]]]Ga[5,Index[Spin,MyExt[n]], Index[Spin,MyExt[n-1]]])/4];
        ];
     (* Go on *)
     n = n - 2;
     (* If n is now zero, that was it. If not, go on *)
     If[n == 0,
        Return[fullstruc],
        Return[ReorderVertexFermionFlows[fullstruc, n]];
       ];

];


(* ::Subsection::Closed:: *)
(*MergeAllVertices*)


MergeAllVertices[vertexlist_List] := SortVertexParticles/@PrecollectVerticesAccordingToParticles[vertexlist];


(* ::Section:: *)
(*Sorting vertices*)


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

	newparts = Sort[MakeIdenticalFermions[parts],FieldOrder[{#1,#2}]&];

    (* If nothing has changed after sorting, done! *)
    If[newparts === parts, 
       Return[{parts, vertex}]
       ];

   (* Otherwise we build the replacement list for Ext[] *)
   newvertex = RelabelExt[{newparts, vertex}];


    (* Return and exit *)
    Return[newvertex];

];
    
    


(* ::Section::Closed:: *)
(*Merge Vertex lists*)


(* ::Subsection:: *)
(*MergeSortedVertex*)


(* ::Text:: *)
(*MergeSortedVertex[{  vertexobj1, vertexobj2, ....} ] merges the vertex object, assuming that they have the same head.*)


MergeSortedVertex[list_List] := {list[[1,1]], Plus @@ list[[All, 2]]};


(* ::Subsection:: *)
(*MergeSortedVertices*)


(* ::Text:: *)
(*MergeSortedVertices[ <vertices >] merges the vertices in the list <vertices>.*)
(*The vertices must be sorted already according to SortVertexFields.*)
(*The routine gather the vertices by first elements, then maps MergeSortedVertex over it.*)


MergeSortedVertices[list_List] := MergeSortedVertex /@ GatherByFirstElement[list];


(* ::Section:: *)
(*SelectVertices*)


(* ::Subsection::Closed:: *)
(*Messages*)


SelVert::FreeContains = "The options Free and Contains must be a list or an alternative, or a combination of both. Option ignored.";

SelVert::Numeric = "MinAdjacency, MaxAdjacency and Adjacency require numeric inputs. Option ignored.";


(* ::Subsection::Closed:: *)
(*UnitTesting*)


(* ::Text:: *)
(*TestSelectVerticesFreeContains[ opt ] checks whether the option Free and contains have the correct form:*)
(** VectorQ   ->   OK: Transform into Alternatives*)
(** Alternatives   ->  OK: Return *)
(** Combination of both   ->  OK: Transform into Alternatives*)
(** Else: Print warning and return False*)


TestSelectVerticesFreeContains[opt_] := Block[{flatopt = opt},

   (* If we have the default value, nothing to be done *)
   If[ flatopt === {},
       Return[{}]
      ];

   (* If it is a list, transform it ito a flat alternative *)
   If[Head[flatopt] === List,
      Return[Flatten[Alternatives @@ flatopt]]
     ];
  
   (* If flatopt is now Alternative, then it is ok *)
   If[Head[flatopt] === Alternatives, 
      Return[flatopt],
      (* else print warning *)
      Message[SelVert::FreeContains];
      Return[False]
      ];

];


(* ::Text:: *)
(*TestSelectVerticesNumeric[opt] test whether opt is an integer. If not, a warning is printed, and false is returned.*)


TestSelectVerticesNumeric[opt_] :=  If[Not[IntegerQ[opt]] && Not[opt === Infinity], 
                                      Message[SelVert::Numeric];
                                      Return[False]
                                     ];


(* ::Subsection::Closed:: *)
(*SelectVertices*)


(* ::Text:: *)
(*These are the options for SelectVertices.*)
(*Note that Min/MaxParticles and SelectParticles have been renamed into Min/MaxAdjacency and Particles.*)


(*Options[SelectVertices] = {
                           MinAdjacency    -> 3,
                           MaxAdjacency    -> Infinity,
                           Adjacency       -> Automatic,
                           Free            -> {},
                           Contains        -> {},
                           Particles       -> {}
};*)


(*SelectVertices[vertexlist_List, pattern_:"#", OptionsPattern[]] := Block[{vertlist = vertexlist},

    If[OptionValue[Particles] =!= {},
       vertlist = Select[vertlist, #[[1]] === OptionValue[Particles]&]
       ];

    If[OptionValue[Adjacency] =!= {},
       vertlist = Select[vertlist, Length[#[[1]]] == OptionValue[Adjacency]&]
       ];



    If[pattern =!= "#",
       vertlist = Cases[vertlist, pattern]
      ];
       

    If[OptionValue[Contains] =!= {},
       vertlist = Select[vertlist, Not[FreeQ[#, Alternatives @@ OptionValue[Contains]]]&]
      ];

    If[OptionValue[Free] =!= {},
       vertlist = Select[vertlist, FreeQ[#, Alternatives @@ OptionValue[Free]]&]
      ];


    If[(OptionValue[MinAdjacency] =!= 3) || (OptionValue[MaxAdjecency] =!= Infinity),
       vertlist = Select[vertlist, OptionValue[MinAdjancency] <= Length[#[[1]]] <= OptionValue[MaxAdjacency]&]
      ];

];*)
 



(* ::Section::Closed:: *)
(*FlavorExpansion*)


(* ::Text:: *)
(*FlavorExpansion[ vertices ] expands the vertices over flavor.*)


Options[FlavorExpansion] = {OptimizeFermionChains -> True, SaveVertices -> False};


Abbr::Reset = "Warning: Running out of names for abbreviations. Some abbreviations might be overwritten.";


FlavorExpansion[vertices_, OptionsPattern[]] := Block[{verts = vertices},

      (* Initialistion *)
      MR$DefinitionsNoFields;
      FR$AbbIndexSum={};
      FR$AbbIndexSumCounter=1;
      Clear[FRIndexSum];
      FR$AbbreviationsCharactorCode++;
      If[FR$AbbreviationsCharactorCode > 122,
         FR$AbbreviationsCharactorCode = 97;
         Message[Abbr::Reset]
        ];

      (* Call the routines from OptimizedFlavorExpand.m *)
      (*verts = SortVertexFields[MakeIdenticalFermions[#]]& /@ verts;*)

      (*verts = MergeSortedVertices[verts];*)
      verts = MergeAllVertices[verts];

      verts = RemoveClassesSymmetryFactors /@ verts;

      verts = PerformVertexFlavorExpansion[verts, SaveVertices -> OptionValue[SaveVertices]];
      (*verts = SortVertexFields /@ verts;*)
      (*verts = MergeSortedVertices[verts];*)
      verts = MergeAllVertices[verts];
 (*     verts = verts /. IndexDelta[Index[Spin, i_], Index[Spin, j_]] :> IndexDelta @@ Sort[{Index[Spin, i], Index[Spin, j]}];*)
(* CD: June 2012: Bug fix for signs of identical fermions signalled by Celine in Tecket #84 *)
      verts=PutRightSignForIdenticalFermions/@verts;

      If[OptionValue[OptimizeFermionChains],
         verts = OptimizeFermionChains /@ verts
         ];

      (* Simplify Eps[i,j,k], and remove possible zeroes *)
      verts = verts /. {dd_?(PRIVATE`SymTensQ[#] === True &)[ind__] :> PRIVATE`SortSymTens[dd][ind], 
                        dd_?(PRIVATE`AntiSymTensQ[#] === True &)[ind__] :> PRIVATE`SortAntiSymTens[dd][ind],
                        ff_?(PRIVATE`StrucConstQ[#] === True &) :> PRIVATE`SortStrucConst[ff]};
      verts = verts /. {Eps[i_,j_,ks___] :> PRIVATE`OrderEps[Eps[i,j,ks]]};
      verts = DeleteCases[verts, {_, 0}];

      (* Return and exit *)
      Return[verts];

];


    



(* ::Section::Closed:: *)
(*CheckForMassEigenstates*)


(* ::Text:: *)
(*CheckForMassEigenstates[ vertexlist ] checks that all vertices involve only mass eigenstates (UnphysicalQ[ ] =!= True). *)
(*If not, a warning is issued.*)


CheckForMassEigenstates[ vertexlist_ ] := Block[{
     partlist = Union @@ ((First /@ # &) /@ vertexlist[[All,1]])
    },
  
   If[Not[And @@ (UnphysicalQ /@ partlist)],
      Message[Vertex::MassEigenstates]
     ];

];
   


(* ::Section:: *)
(*Runs*)


(* ::Subsection:: *)
(*AddToRunTable*)


(* ::Text:: *)
(*AddToRunTable[ vertexlist ] adds the list to the ist of previous runs, FR$InterfaceRuns. A tag is added to identify the interface that produced the vertices, togetherwith the date and time, and the default tag is ""*)


Options[AddToRunTable] = {Tag -> ""};


AddToRunTable[vertices_List] := Block[{tag, 
    date = PRIVATE`FR$DateFormat[],
    number = Length[FR$InterfaceRuns] + 1},

    AppendTo[FR$InterfaceRuns, InterfaceRunObject[number, date, tag, vertices]];

];

    


(* ::Subsection::Closed:: *)
(*DisplayRuns*)


DisplayRuns[] := Block[{header, output},

   header = {"Number", "Interface", "Date", "# vertices"};

   output = List @@@ (MapAt[Length, #, -1]& /@ FR$InterfaceRuns);

   output = Prepend[output, header];

   Return[TableForm[output]];

];



(* ::Subsection::Closed:: *)
(*InterfaceRun*)


InterfaceRun[n_Integer] := Cases[FR$InterfaceRuns, InterfaceRunObject[n, ___]][[1,-1]];


(* ::Section:: *)
(*VertexSimplify*)


(* ::Text:: *)
(*Simplifies the analytic expressions for the vertices, by trying to apply *)
(*1) Jacobi identity for f[a,b,c]*)


VertexSimplify[particles_, expr_] := Block[{
    vertex = Expand[expr],
    ff$Sim, ff$SimRules1, ff$SimRules2,
    jacobirules
    },

    (* Simplification Rules *)
    ff$SimRules1 = {ff$Sim[aa_,bb_,c_,d_] :> -ff$Sim[bb,aa,c,d] /; Not[OrderedQ[{aa,bb}]],
              ff$Sim[aa_,b_,c_,d_] :> -ff$Sim[aa,b,d,c] /; Not[OrderedQ[{c,d}]]
              };
    ff$SimRules2 = {ff$Sim[aa_,b_,cc_,d_] :> ff$Sim[cc,d,aa,b] /; Not[OrderedQ[{aa,cc}]]
                   };
    jacobirules = {g1_. ff$Sim[aa_,b_,c_,d_] + g2_. ff$Sim[aa_,c_,b_,d_] + g1_. ff$Sim[aa_,d_,b_,c_] :> 0 /; g2 === -g1
                  };

    (* Jacobi identity *)
    vertex = vertex /. {f[aa_,b_,e_]f[c_,d_,e_] :> ff$Sim[aa,b,c,d]};
    If[Not[FreeQ[vertex, ff$Sim]],
       vertex = Factor[vertex //. ff$SimRules1 //. ff$SimRules2];
       vertex = vertex //. jacobirules
       ];
    vertex = vertex //. ff$Sim[aa_,b_,c_,d_] :> Module[{xx}, f[aa,b,Index[Gluon, xx, 1]]f[c,d,Index[Gluon, xx, 1]]];

    Return[{particles, vertex}];

];
       
    


(* ::Section::Closed:: *)
(*RemoveSextets*)


(* ::Text:: *)
(*Removes all vertices involving sextets from a vertex list.*)
(*There is a similar function for PartList*)


Vertices::Sextets = "Vertex `1` involves sextets. Vertex ignored.";


RemoveSextetFromVertex[particles_, vertex_] := Block[{

    parts = First /@ particles,
    indices

    },

    (*Read out particle indices *)
    indices = Union[$IndList /@ parts];
    (* Check for sextets *)
    If[MemberQ[indices, Index[Sextet]] || Not[FreeQ[vertex, T6|K6|K6bar|Sextet]],
       Message[Vertices::Sextets, parts];
       Return[{parts, 0}]
      ];

    (* Else, return vertex *)
    Return[{particles, vertex}];

];
       

    


RemoveSextetsFromVertexList[vertexlist_] := DeleteCases[RemoveSextetFromVertex @@@ vertexlist, {_, 0}];


RemoveSextetParticleClass[class_, members_] := Block[{

   member1 = members[[1]]

   },

   If[member1[[7]] === PRIVATE`S6,
      Return[MR$Null],
      Return[{class, members}]
     ];

];


RemoveSextetsFromPartList[partlist_] := If[Not[FreeQ[partlist, PRIVATE`S6]],
           DeleteCases[RemoveSextetParticleClass @@@ partlist, MR$Null],
           partlist
];


(* ::Section::Closed:: *)
(*CheckQuantumNumberConservation*)


(* ::Text:: *)
(*CheckQuantumNumberConservation[ vertexlist,  qnumbers ] checks whether all the quantum in qnumbers are conserved in every vertex in vertexlist. if the second argument is omitted, all quantum numbers are checked.*)


CheckQuantumNumberConservation::usage = "CheckQuantumNumberConservation[ vertexlist,  qnumbers ] checks whether all the quantum in qnumbers are conserved in every vertex in vertexlist.";


Options[CheckQuantumNumberConservation] = {ConservedQuantumNumbers :> MR$QuantumNumbers};


CheckQuantumNumberConservation[vertices_List, qnumbers_List] := CheckQuantumNumberConservation[vertices, ConservedQuantumNumbers -> qnumbers];

CheckQuantumNumberConservation[vertices_List, OptionsPattern[]] := Block[{

    qnumbers = OptionValue[ConservedQuantumNumbers],
    particles = vertices[[All, 1, All, 1]]
    },

    Return[Sequence @@@ (PRIVATE`ConserveQN[#, qnumbers]& /@ particles)];

];




(* ::Section:: *)
(*Multi fermion flows*)


(* ::Subsection::Closed:: *)
(*CountFermionFields*)


(* ::Text:: *)
(*CountFermionFields[ ... ] takes vertex object {particles, expression} as input, and returns the number of fermionic fields (but not ghosts!) in <particles>.*)


CountFermionFields[{parts_List,vertex_}] := Length[Select[First/@parts,(FermionQ[#]&&Not[GhostFieldQ[#]===True])&]]


(* ::Subsection::Closed:: *)
(*GetMultiFermionOperators*)


(* ::Text:: *)
(*GetMultiFermionOperators[ list ] takes a vertex list as input, and returns precisely does that have strictly more than 2 fermions.*)


GetMultiFermionOperators[vertexlist_List]:=Select[vertexlist,CountFermionFields[#]>2&]


(* ::Subsection::Closed:: *)
(*SplitVertex*)


(* ::Text:: *)
(*SplitVertex[ ...] takes a vertex object {particles, expression} as input, and splits it into all the different terms if expression is a sum.*)
(*The output is a list, and each element is wrapped into *)
(**)
(*SplitVertex$Obj[ particles,  term ]*)
(**)
(*There is a also a version that maps over vertex lists, *)
(**)
(*SplitVertices[ list ]*)


SplitVertex[{parts_List,expr_}]:=Block[{
   temp = Expand[expr]
   },

   If[Head[temp]=!=Plus,
      Return[SplitVertex$Obj[parts,temp]]
     ];

   temp = List@@temp;
   temp = SplitVertex$Obj[parts,#]&/@temp;

   Return[temp];
];


SplitVertices[list_]:=Flatten[SplitVertex/@list];


(* ::Subsection:: *)
(*ComputeFermionFlowForTerm*)


(* ::Text:: *)
(*ComputeFermionFlowForTerm[ ... ]  takes a single SplitVertex$Obj[particles, term  ] as input, where terms is a single term (i.e., Head[term] =!= Plus),*)
(*and returns the fermion flow of term. The output is again a SplitVertex$Obj object, but with 3 entries, *)
(**)
(*SplitVertex$Obj[ particles, term, flow]*)
(**)
(*where flow is a list of integer, corresponding to *)
(**)
(*{out1, in1, out2, in2, ...}*)


FermionFlowExtractor[aa_, b_?(FreeQ[#,Spin]&)c_] := FermionFlowExtractor[aa b, c]


CheckSpinIndexOrder[pair_, list_] := Block[{
   ind1 = pair[[1]],
   ind2 = pair[[2]],
   part1, part2, outpair
   },

   part1 = list[[ind1,1]];
   part2 = list[[ind2,1]];

   outpair = If[AntiFieldQ[part2] && Not[AntiFieldQ[part1]],
                Reverse[pair],
                pair
               ];

   Return[outpair];
];
      


ComputeFermionFlowForTerm[split_SplitVertex$Obj]:=Block[{
   particles=split[[1]],
   expr=split[[2]],
   gammastructure,flow,
   rest,dummyGamma
   },

   dummyGamma/:dummyGamma[Index[Spin,aa_],Index[Spin,b_]]dummyGamma[Index[Spin,b_],Index[Spin,c_]]:=dummyGamma[Index[Spin,aa],Index[Spin,c]];

   expr=FermionFlowExtractor[1,expr];
   gammastructure=expr[[2]];
   flow=gammastructure;
   rest=expr[[1]];

   flow=flow/.Except[dummyGamma][___,Index[Spin,aa_],Index[Spin,b_]]:>dummyGamma[Index[Spin,aa],Index[Spin,b]]//.{Index[Spin,Ext[i_]]:>i};
   flow=Sort[(flow/.Times->List)];
   flow = flow /. aa_dummyGamma :> CheckSpinIndexOrder[aa, particles];

   flow=flow/.dummyGamma->Sequence;


   Return[SplitVertex$Obj[particles,gammastructure*rest,flow]];
]


(* ::Subsection:: *)
(*CanonicalizeFlows*)


(* ::Text:: *)
(*CanonicalizeFlows[ fermions ] takes care of relations like 1234 = 3412*)


CanonicalizeFlows[fermions_List, nferms_Integer] := Block[{
   left  = Table[fermions[[i]], {i, 1, nferms - 1, 2}],
   right = Table[fermions[[i]], {i, 2, nferms, 2}],
   bosons = fermions[[nferms+1;;]],
   pairs, unnumberedpairs, referenceordermap, orderQ
   },

   orderQ = (#1 /. {aa_, _Integer} :> aa /. referenceordermap) < (#2 /. {aa_, _Integer} :> aa /. referenceordermap)&;

   pairs = Transpose[{left,right}];
   unnumberedpairs = Sort[DeleteDuplicates[pairs /. {aa_, _Integer} :> aa]];
   referenceordermap = Table[Rule[unnumberedpairs[[i]], i], {i, Length[unnumberedpairs]}];

   pairs = Sort[pairs, orderQ];
   pairs = Sequence @@@ pairs;

   Return[Join[pairs, bosons]];

];
   


(* ::Subsection::Closed:: *)
(*PermuteFermionFields*)


(* ::Text:: *)
(*PermuteFermionFields[ particles, term, flow ] reoders the particles and the indices of expression into the order given by flow.*)
(*It also multiplies term by the signature of the femrionic permutation.*)


PermuteFermionFields[vertex_SplitVertex$Obj]:=Block[{
   particles=vertex[[1]],
   expr=vertex[[2]],
   flow=vertex[[3]],
   sig, particlenumbering,newvertex
   },

   particlenumbering=Join[flow,Range[Length[flow]+1,Length[particles]]];

   If[particlenumbering=!=Range[Length[particles]],
      particles=Permute[particles,particlenumbering];
     ];

   sig=Signature[flow];
   (* We need to canonicalize the flows, meaning that now 1234 = 3412 for example *)
   particles = CanonicalizeFlows[particles, Length[flow]];

   expr=sig*expr;

   newvertex=RelabelExt[{particles,expr}]/.IndexDelta[Index[Spin, i_], Index[Spin, j_]] :> IndexDelta @@ Sort[{Index[Spin, i], Index[Spin, j]}];

   Return[SplitVertex$Obj@@newvertex];
];


(* ::Subsection::Closed:: *)
(*CombineFermionFlows*)


SumTermsInSplitVertex$Obj[list_]:=If[Length[list]==1,
   list[[1]],
   SplitVertex$Obj[list[[1,1]],Plus@@(#[[2]]&/@list)]
];


CombineFermionFlows[list_List]:=SumTermsInSplitVertex$Obj/@GatherByFirstElement[list];


(* ::Subsection::Closed:: *)
(*BreakIntoFermionFlows (Master routine)*)


(* ::Text:: *)
(*BreakIntoFermionFlows[ vertexlist ] takes a vertex list as input, and brings all the multifermion vertices (i.e., > 2) into a canonical form:*)
(*The convention is that, at the end, a multifemrion vertex is broken into different fermion flows, without signs. *)


BreakIntoFermionFlows[vertexlist_] := Block[{
   multifermionvertices, nonmultifermionvertices
   },

   (* First, select the multifermion operators *)
   multifermionvertices = GetMultiFermionOperators[vertexlist];
   
   (* If there are none, nothing to be done *)
   If[multifermionvertices === {},
      Return[vertexlist];
     ];
   
   (* else *)
   nonmultifermionvertices = Complement[vertexlist, multifermionvertices];
   
   (* First, split each multifermion vertex into individual terms, and compute the flow of each term... *)
   multifermionvertices = SplitVertices[multifermionvertices];
Print[multifermionvertices];
   multifermionvertices = ComputeFermionFlowForTerm /@ multifermionvertices;
Print[multifermionvertices];

   (* ... and then permute the fermions into the right order *)
   multifermionvertices = PermuteFermionFields /@ multifermionvertices;

   (* !!!! We remove vertices that have become equal after permutation (e.g., t <-> u exchange *)
   multifermionvertices = Union[multifermionvertices] /. SplitVertex$Obj->List;
Print[multifermionvertices];

   (* Recombine vertices that correspond to the same particles in the same order *)
   multifermionvertices = CombineFermionFlows[multifermionvertices];
Print[multifermionvertices];

   (* Add the vertices without multifermions *)
   Return[Join[multifermionvertices, nonmultifermionvertices] /. SplitVertex$Obj -> List];

];

   
   


(* ::Section:: *)
(*FermionFlows*)


RecombineVerticesFromSum[list_List] := {list[[1,1]], Expand[Plus@@list[[All,2]]]};


IdenticalFermionCorrectionSign[gammastruc_,parts_]:=Block[{
   temp = gammastruc,
   ferms,
   AFieldQ
   },

   (* If there is no spin index, nothing to be done -> exit*)
   If[Not[MatchQ[temp,_[___,Index[Spin,Ext[_]], Index[Spin,_]]]],
     Return[temp];
     ];
  
   (* 
      else, go and read out the spin indices
      and check if they are in the right (=ascending) order. 
      if so, nothing to be done -> exit
    *)
   temp=temp/._[___,Index[Spin,Ext[i1_]],Index[Spin,Ext[i2_]]]:>{i1,i2};
  
   If[temp[[1]]<temp[[2]],
      Return[gammastruc];
     ];

    (* 
      If there is more than two fermion, nothing changes (added by celine)
    *)
    If[Count[FermionQ/@parts[[All,1]],True]>2,
       Return[gammastruc];
      ];


   (*
     Read out the two fermions from the particle content.
     and check if there both (anti)particles.
     if so, then there are necessarily majoranas or charge conjugation involved 
    *)
     ferms={parts[[temp[[1]],1]],parts[[temp[[2]],1]]};

     AFieldQ[fff_]:=(AntiFieldQ[fff]===True);
     ferms=Union[AFieldQ/@ferms];

    (* 
      If one is particle, the other antiparticle,
      then we do not have Majoranas or CC[], so nothing to be done.
      Note that I believe that this case should in principle never occur,
      in conjunction with descending index order, but better assume it can.
    *)
    If[Length[ferms]==2,
       Return[gammastruc];
      ];

    (* 
      Finally, if we got here, we have Majoranas or CC[ ] in descending order.
      Then we do not (do not added by celine and the sign has been removed) add a minus sign (fermion anticommutation for the creation operators which are flipped
      + reversal of flow
     *)

     temp=gammastruc/.{PP_[inds___, Index[Spin, Ext[i1_]], Index[Spin, Ext[i2_]]] :> PRIVATE`CGa[PP[inds, Index[Spin, Ext[i2]], Index[Spin, Ext[i1]]]]};
     Return[temp];
];


PutRightSignForIdenticalFermions[vertex_] := Block[{
   parts=vertex[[1]],
   expr=Expand[vertex[[2]]],
   ChainExtractor,chains,rest
   },

   (*
     If there is no Spin index in the vertex, nothing to be done, and exit 
    *)
   If[FreeQ[expr,Spin],
      Return[vertex];
     ];

    (* 
     If the expression for the vertex is a sum, treat each term separately.
    *)
    If[Head[expr]===Plus,
       Return[RecombineVerticesFromSum[PutRightSignForIdenticalFermions[{parts,#}]&/@(List@@expr)]];
      ];


    (* 
      If we get here, we have to deal with a single term (no sum!)
      Next, we read out all the fermion chains in this term.
      Note that each external fermion is conencted to one and only one chain.
      Then we go on and check whetehr we need to flip the flow and add a sign,
     *)
    ChainExtractor[aa_, exp_. * tens_[inds___, Index[Spin,s1_], Index[Spin, s2_]]] := ChainExtractor[aa* tens[inds, Index[Spin, s1], Index[Spin, s2]], exp];

    expr = ChainExtractor[1,expr];
    chains = expr[[1]];
    rest = expr[[2]];

    chains = If[Head[chains]===Times,
                List@@chains,
                {chains}
               ];

    chains=IdenticalFermionCorrectionSign[#,parts]&/@chains;
    chains=Times@@chains;
    expr=Expand[chains*rest];

    Return[{parts,expr}];
];
