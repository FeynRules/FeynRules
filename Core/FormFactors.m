(* ::Package:: *)

(* ::Title:: *)
(*Form factors*)


(* ::Section:: *)
(*Helpers*)


FormFactorQ = MemberQ[FR$FormFactors, #]&;


(* ::Section:: *)
(*Declaration*)


FR$FormFactorAttributes = {Particles, ComplexParameter, Value, ParameterName};


(* ::Subsection:: *)
(*Test functions*)


FormFactors$IsCorrectHeadEqual := If[Head[#] =!= Equal,
         Message[FormFactor::Equal, #[[1]]]
         ]&;

FormFactors$IsCorrectHeadRule[formf_] := Block[{ff = formf[[1]],
         rules=formf[[2]],
         tester
         },

         tester = DeleteCases[Head /@ rules, Rule|RuleDelayed];
         If[tester =!= {},
            Message[FormFactor::Rule, ff]
            ];

         tester = Complement[Union[First /@ rules], FR$FormFactorAttributes];
         If[tester =!= {},
            Message[FormFactor::Option, #]& /@ tester
           ];

];


(* ::Subsection:: *)
(*DeclareIndividualFormFactor*)


DeclareIndividualFormFactor[formf_Equal] := Block[{
    ff=formf[[1]],
    options = formf[[2]],
    particles,
    complex,
    value,
    parametername,
    attributes
    },

    (* Check if mandatory attributes are present *)
    attributes = First /@ options;
    If[FreeQ[attributes, Particles], 
       Message[FormFactor::Particles, ff]
      ];
    If[FreeQ[attributes, Value], 
       Message[FormFactor::value, ff]
      ];

    (* Read in attributes *)


    particles = Particles /. options /. Particles -> {};
    complex = ComplexParameter /. options /. ComplexParameter -> False;
    value   = Value /. options /. Value -> 1;
    parametername = ParameterName /. options /. ParameterName -> ff;

    (* Set numQ *)
    numQ[ff] = True;
    CnumQ[ff] = complex;

    FormFactor$Particles[ff] = MapIndexed[{#1, Sequence @@ #2}&, particles];
    FormFactor$Value[ff] = value;
    AppendTo[ParamRules, ff -> parametername];
    AppendTo[FR$FormFactors, ff];

];

    

     
    


(* ::Subsection:: *)
(*DeclareFormFactors*)


DeclareFormFactors[list_] := Block[{formfactors=list},

    If[formfactors === {},
       Return[]
      ];

     (* Checking the Input *)
     FormFactors$IsCorrectHeadEqual /@ formfactors;
     FormFactors$IsCorrectHeadRule /@ formfactors;

     (* Map to declare *)
     DeclareIndividualFormFactor /@ formfactors;

];

     

     
     
    


(* ::Section:: *)
(*MatchFormFactorsToVertexList*)


(* ::Text:: *)
(*This function takes as an argument a vertex list, and checks if there is any vertex in there for which a form factor was defined. If a given form factor appears in more than one vertex, a warning is produced (there might be problems with how to assign the momenta in that case).*)
(*The output is a list whose entries are*)
(**)
(*{formfactor,   expression}*)
(**)
(*where in <expression> the momenta have been relabeled according to the order of particles in the vertex.*)


FindParticlePermutation[list1_List, list2_List] := Block[{
   work,
   rest,
   newinput,
   match, result
   },

   If[Length[list1] =!= Length[list2],
      Print[Style["Fatal Error: Cannot find particle permutation.", Red]]
      ];
  
   If[list1 === {},
      Return[{}];
     ];

   work = list1[[1]];
   rest = Rest[list1];

   match = Catch[Do[If[ll[[1]] === work[[1]], Throw[ll]], {ll, list2}]];
   
   newinput = DeleteCases[list2, match];

   Return[Flatten[{work -> match, FindParticlePermutation[rest, newinput]}]];
];
   

   


Options[RelabelFormFactorExt] = {Ingoing -> True};


RelabelFormFactorExt[{formf_, vertex_}, OptionsPattern[]] := Block[{
    vertsparts = vertex[[1,1]],
    formexpr = FormFactor$Value[formf],
    formparts = FormFactor$Particles[formf],
    perm, dummyrule,
    ingoing = OptionValue[Ingoing]
    },

    If[Not[ingoing],
       formparts = (MapAt[anti[MakeIdenticalFermions[#1]]&,#1,1]&)/@formparts;
      ];

    (* Build the replacement table *)
    perm = FindParticlePermutation[formparts, vertsparts];
    perm = dummyrule @@@ perm;
    perm = perm /. dummyrule[{_,i_}, {_,j_}] :> dummyrule[Ext[i], Ext[j]];
    perm = Rule @@@ perm;

    (* Apply the rules to the expression *)
    formexpr = formexpr /. {SP[Except[_Ext,i_], Except[_Ext,j_]] :> SP[Ext[i], Ext[j]]} /. perm //. Ext -> Identity;
    
    Return[{formf, formexpr}];

];


Options[MatchFormFactorsToVertexList] = {Ingoing -> True};


MatchFormFactorsToVertexList[vertexlist_List, OptionsPattern[]] := Block[{ffverts, dummy,
    ingoing = OptionValue[Ingoing]
    },

    ffverts = Table[{aa, Select[vertexlist, Not[FreeQ[#, aa]]&]}, {aa, FR$FormFactors}];
    (* Remove form factors that are not used *)
    ffverts = DeleteCases[ffverts, {_, {}}];

    (* If there is nothing left, do nothing *)
    If[ffverts === {}, 
       Return[{}];
      ];


    (* Check if every form factor appears at most once *)
    If[Length[#[[2]]] > 1, Message[FormFactor::Vertex, #[[1]]]]& /@ ffverts;

    (* Check if particle content matches *)
    If[Sort[First/@#[[1]]] =!= Sort[First/@#[[2,1]]]&, Message[FormFactor::VertexParticles, #[[1]]]]& /@ ffverts;
 
    (* Build the list of form factors with the external momenta permuted *)
    ffverts = RelabelFormFactorExt[#, Ingoing -> ingoing]& /@ ffverts;

    Return[ffverts];
];
    



(* ::Section:: *)
(*UFO part*)
