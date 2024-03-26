(* ::Package:: *)

(* ::Title:: *)
(*From FeynArts to FeynRules*)


(* ::Text:: *)
(*These are the routines to reimport the FeynArts output for the R2 vertices and UV counterterms back into FeynRules.*)
(*The most important functions are those that*)
(**)
(** Change the particle names in FeynArts back to the FeynRules names*)
(** Change the Color names back to FeynRules, i.e., SUNF -> f, SUNT -.> T.*)


(* ::Section:: *)
(*Switching the particle names*)


(* ::Subsection:: *)
(*AddAntiForFeynArtsToFeynRulesParticles*)


(* ::Text:: *)
(*Takes a rule of the form*)
(**)
(*F[1] -> vl*)
(**)
(*And returns the pair*)
(**)
(*{ F[1] -> vl, -F[1] -> vlbar } *)


AddAntiForFeynArtsToFeynRulesParticles[rule_Rule]:=Block[{
   lhs = rule[[1]],
   rhs = rule[[2]],
   antirhs,
   newrule = {rule}
   },

   (* Make the anti particle *)
   antirhs=anti[rhs];

   If[antirhs=!=rhs,
      newrule=Append[newrule,-lhs->antirhs]
   ];
   
   Return[newrule];

];


(* ::Subsection:: *)
(*MakeEntryForFeynArtsToFeynRulesParticles*)


(* ::Text:: *)
(*Take a rule of the form*)
(**)
(*F[1] -> vl*)
(**)
(*and checks if the class is flavored. If so, the list*)
(**)
(*{ F[1] -> vl, F[1, {1}] -> ve, F[1, {2}] -> vm, F[1, {3}] -> vt }*)
(**)
(*is returned. Otherwise the list*)
(**)
(*{ F[1] -> vl, F[1, {}] -> vl }*)
(**)
(*is returned.*)


MakeEntryForFeynArtsToFeynRulesParticles[rule_Rule]:=Block[{
   lhs = rule[[1]],
   rhs = rule[[2]],
   flavors,
   newrule = {rule}
   },

   (* Add flavors *)

   flavors = If[FlavoredQ[rhs],
                Table[Append[lhs,{i}] -> MR$ClassMembers[rhs][[i]],
                      {i,Length[MR$ClassMembers[rhs]]}],
                {Append[lhs,{}] -> rhs}
                ];

   newrule = Join[newrule,flavors];

   Return[newrule];
];


(* ::Subsection:: *)
(*BuildFeynArtsToFeynRulesParticles*)


(* ::Text:: *)
(*Builds the replacement list necessary to map the FA particles back to FeynRules*)
(*by mapping the previous functions over the list FA$ClassToName.*)
(*The function returns the replacement list, but also stores it in the global variable*)
(*FeynArtsToFeynRulesParticles.*)
(**)
(*This function is called by default when a model is loaded.*)


BuildFeynArtsToFeynRulesParticles[] := Block[{
    (* We remove the hardcoded fields form the SF module *)
    fa$classtoname=Drop[FA$ClassToName,-11]
    },

    fa$classtoname = Flatten[MakeEntryForFeynArtsToFeynRulesParticles/@fa$classtoname];

    fa$classtoname = Flatten[AddAntiForFeynArtsToFeynRulesParticles/@fa$classtoname];


    Return[FeynArtsToFeynRulesParticles=fa$classtoname];
];


(* ::Subsection:: *)
(*SwitchVertexParticlesFAToFR*)


(* ::Text:: *)
(*Takes a FA vertex list for R2 or UV vertices, and changes the particles names in the first argument to the FR names, *)
(*according to the replacement list given as the second argument. This replacement list should correspond to FeynArtsToFeynRulesParticles.*)


SwitchVertexParticlesFAToFR[{parts_List,vertexpr_}, repllist_:FeynArtsToFeynRulesParticles]:=Block[{
   particles = parts
   },

   particles = particles//.{{a_. S[mm_Integer,_],nn_Integer}:>{a S[mm],nn},
                            {a_. F[mm_Integer,_],nn_Integer}:>{a F[mm],nn},
                            {a_. V[mm_Integer,_],nn_Integer}:>{a V[mm],nn},
                            {a_. R[mm_Integer,_],nn_Integer}:>{a R[mm],nn},
                            {a_. T[mm_Integer,_],nn_Integer}:>{a T[mm],nn},
                            {a_. U[mm_Integer,_],nn_Integer}:>{a U[mm],nn}
                           };

   particles = particles //. repllist;

   Return[{particles, vertexpr}];

];


(* ::Section:: *)
(*Color algebra module*)


Color$TR = 1/2;
Color$Nc = 3;


SUNFOpen[Index[Gluon, a1_], Index[Gluon, a2_], Index[Gluon, a3_], Index[Gluon, a4_]] := Block[{
   bb = Unique["b"]
   },

   Return[f[Index[Gluon, a1], Index[Gluon, a2], Index[Gluon, bb]]*f[Index[Gluon, bb], Index[Gluon, a3], Index[Gluon, a4]]];

   ];


TrSUNTOpen[Index[Gluon, _]] := 0;

TrSUNTOpen[Index[Gluon, a1_], Index[Gluon, a2_]] := Color$TR * delta[Index[Gluon, a1], Index[Gluon, a2]];

TrSUNTOpen[as__, Index[Gluon, a1_], Index[Gluon, a2_]] := Block[{
   bb = Unique["b"]
   },

   Return[I/2*f[Index[Gluon, a1], Index[Gluon, a2], Index[Gluon, bb]] TrSUNTOpen[as, Index[Gluon, bb]] +
          1/(2*Color$Nc)*delta[Index[Gluon, a1], Index[Gluon, a2]]*TrSUNTOpen[as] + 
          1/2*dSUN[Index[Gluon, a1], Index[Gluon, a2], Index[Gluon, bb]] TrSUNTOpen[as, Index[Gluon, bb]]];
];



SUNTOpen[Index[Gluon, a1_], Index[Colour, i1_], Index[Colour, i2_]] := T[Index[Gluon, a1], Index[Colour, i1], Index[Colour, i2]];

SUNTOpen[as__, Index[Gluon, a1_], Index[Colour, i1_], Index[Colour, i2_]] := TensDot[Sequence @@ Table[T[aa], {aa, {as, Index[Gluon, a1]}}]][Index[Colour, i1], Index[Colour, i2]];


SortdSUN[expres_] := If[FreeQ[expres, dSUN],
    expres,
    Expand[expres /. dSUN[a_,b_,c_]:> dSUN @@ Sort[{a,b,c}]]];


(* ::Section:: *)
(*Vertices*)


SwitchFromFAToFRVertexConventions[{parts_, vertexpr_}] := Block[{
   expi = Expand[vertexpr]
   },

   expi = expi //. {SUNT[Index[Gluon, a_], Index[Colour, i1_], Index[Colour, i2_]] :> T[Index[Gluon, a], Index[Colour, i1], Index[Colour, i2]],
                    SUNT[as__, Index[Gluon, a_], Index[Colour, i1_], Index[Colour, i2_]] :> SUNTOpen[as, Index[Gluon, a], Index[Colour, i1], Index[Colour, i2]],
                    SUNF[Index[Gluon, a1_], Index[Gluon, a2_], Index[Gluon, a3_]] :> f[Index[Gluon, a1], Index[Gluon, a2], Index[Gluon, a3]],
                    SUNF[Index[Gluon, a1_], Index[Gluon, a2_], Index[Gluon, a3_], Index[Gluon, a4_]] :> SUNFOpen[Index[Gluon, a1], Index[Gluon, a2], Index[Gluon, a3], Index[Gluon, a4]],
                    SUNT[Index[Gluon, a1_], as___, Index[Gluon, a2_]] :> TrSUNTOpen[Index[Gluon, a1], as, Index[Gluon, a2]]
                   };

   expi = SortdSUN[Expand[expi] /. delta -> IndexDelta];

   Return[{parts,expi}];

];
