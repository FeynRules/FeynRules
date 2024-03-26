(* ::Package:: *)

(* ::Text:: *)
(*This file contains all the routines necessary for the extraction of the counterterm Lagrangian.*)


(* ::Section:: *)
(*Field renormalization*)


(* ::Subsection:: *)
(*Rewriting the arguments of the functions*)


FieldRenormalization[field_[inds__]]:=RenField[field,{inds}];

FieldRenormalization[field_]:=RenField[field,{}];


(* ::Subsection:: *)
(*Core function: returns a renormalized field from a bare field*)


(* ::Text:: *)
(*The list of indices must be given as a second argument*)


RenField[field_,inds__List]:=Block[{rfield=field,NewIndices,FreeIndices, ColorIndicesRules, fsym,MixList, spi, result,myRule,ffla},
  (* Restore the types of the indices *)
  FreeIndices=Inner[Index,$IndList[field]/.Index[aa_]->aa,inds/.Index[_,bb_]->bb,List];
  ColorIndicesRules=(myRule[#/.Index[typ_,bla_]->Index[typ,Blank[]],#]&/@Cases[FreeIndices,Index[Colour,_]|Index[Gluon,_]])/.myRule->Rule;

  (* Create new id for a new flavor index *)
  NewIndices=Unique["idx"];

  (* Compute the list of fields which may mix with 'field' at the one-loop level *)
  If[AntiFieldQ[field]===True,rfield=anti[field]];
  fsym=Plus@@Extract[MR$ClassesList,Position[ClassName/.MR$ClassesRules[#]&/@ MR$ClassesList,rfield]];
  MixList={ClassName,FlavorIndex}/.MR$ClassesRules[#]&/@  (DeleteCases[MR$ClassesList,_?( (Unphysical/.MR$ClassesRules[#])===True || 
    DeleteCases[$IndList[(ClassName/.MR$ClassesRules[#])],Index[(FlavorIndex/.MR$ClassesRules[#])]]=!=DeleteCases[$IndList[rfield],Index[(FlavorIndex/.MR$ClassesRules[fsym])]]|| 
   (QuantumNumbers/.MR$ClassesRules[#])=!=(QuantumNumbers/.MR$ClassesRules[fsym])&)]); 

  ffla=FlavorIndex/.MR$ClassesRules[fsym];

  (* Replace the bare 'field' by the corresponding renormalized quantity *)
  result=Which[
    (* Fermion fields *)
    FermionQ[rfield]===True && GhostFieldQ[rfield]=!=True, 
      spi=Index[Spin,Unique["sp"]];
      Plus@@ List[ rfield[Sequence@@FreeIndices], 
        Sequence@@ (1/2 FR$deltaZ[{rfield,#[[1]]},
          {DeleteCases[Append[Cases[FreeIndices,Index[ffla,_]],Index[#[[2]],NewIndices]],Index[FlavorIndex,_]]},"L"] * ProjM[FreeIndices[[1]],spi]
          #[[1]][spi,Sequence@@DeleteCases[Append[DeleteCases[FreeIndices,Index[ffla,_]],Index[#[[2]],NewIndices]],Index[FlavorIndex,_]|Index[Spin,_]]]&/@MixList), 
        Sequence@@ (1/2 FR$deltaZ[{rfield,#[[1]]},
          {DeleteCases[Append[Cases[FreeIndices,Index[ffla,_]],Index[#[[2]],NewIndices]],Index[FlavorIndex,_]]},"R"] * ProjP[FreeIndices[[1]],spi]
          #[[1]][spi,Sequence@@DeleteCases[Append[DeleteCases[FreeIndices,Index[ffla,_]],Index[#[[2]],NewIndices]],Index[FlavorIndex,_]|Index[Spin,_]]]&/@MixList)],
    (* Vector and scalar fields *)
    VectorFieldQ[rfield]===True||ScalarFieldQ[rfield]===True||Spin2FieldQ[rfield]===True,
    Plus@@ List[ rfield[Sequence@@FreeIndices]/.rfield[]->rfield,Sequence@@ (1/2 FR$deltaZ[{rfield,#[[1]]},
        {DeleteCases[Append[Cases[FreeIndices,Index[ffla,_]],Index[#[[2]],NewIndices]],Index[FlavorIndex,_]]}] *
        (#[[1]][Sequence@@DeleteCases[Append[DeleteCases[FreeIndices,Index[ffla,_]],Index[#[[2]],NewIndices]],Index[FlavorIndex,_]]]/.#[[1]][]->#[[1]])&/@MixList)]
  ]; 

  If[AntiFieldQ[field]===True,result=result/.{rfield->field,FR$deltaZ[args__]->Conjugate[FR$deltaZ[args]],ProjP[a_,b_]->ProjM[b,a],ProjM[a_,b_]->ProjP[b,a]}];

  (* Return the result *)
result];


(* ::Subsection:: *)
(*Derive all the renormalized fields and antifields*)


(* ::Text:: *)
(*This produces a list of rules allowing to replace bare fields by renormalized ones for the while model field content.*)


FieldRenormalization[]:=Block[{MyModule,MyRuleDelayed},
  Flatten[Block[{Inds,bare,ren,antibare,antiren,SummedInds,IDX, MyPattern},
  (* Bare and renormalized fields *)
  Inds=Table[Unique["idx"],{Length[$IndList[#]]}];
  bare=#[Sequence@@Inds]/.#[]->#;
  ren=FieldRenormalization[bare]; 

  (* Gets the summed indices appearing in the renormalized field *)
  SummedInds=Complement[Union[Cases[ren,Index[a_,b_]->b,\[Infinity]]],Inds];

  (* Prepare the replacement rule *)
  Inds=(MyPattern[#,Blank[]]&/@Inds);
  bare=#[Sequence@@Inner[Index,$IndList[#]/.Index[aa_]->aa,Inds,List]]/.#[]->#;
  If[SummedInds=!={},ren=MyModule[SummedInds,ren]];

  (* Antifields *)
  If[SelfConjugateQ[#]=!=True || (SelfConjugateQ[#]===True && FermionQ[#]===True),
    antibare=If[FermionQ[#]===True && GhostFieldQ[#]=!=True,bare/.#->HC[#].Ga[0],bare/.#->anti[#]];
    antiren=ren/.{fi_?(FieldQ[#]===True&)[args__]->(anti[fi])[args],fi_?(FieldQ[#]===True&)->anti[fi]}/.{
      FR$deltaZ[args__]->Conjugate[FR$deltaZ[args]],ProjP[a_,b_]->ProjM[b,a],ProjM[a_,b_]->ProjP[b,a]}/.
      Conjugate[FR$deltaZ[{fi_?(AntiFieldQ[#]===True&),gi_?(AntiFieldQ[#]===True&)},rest_List,opt___]]->Conjugate[FR$deltaZ[{anti[fi],anti[gi]},rest,opt]];
  ];

  (* output *)
  If[SelfConjugateQ[#]===True && FermionQ[#]=!=True,
    {MyRuleDelayed[bare,ren]}, 
    {MyRuleDelayed[bare,ren],MyRuleDelayed[antibare,antiren]}]/.MyPattern->Pattern]&/@(DeleteCases[MR$ClassNameList,_?(UnphysicalQ[#]===True&)])/.
    MyRuleDelayed:>RuleDelayed/.MyModule->Module]
];


(* ::Section::Closed:: *)
(*Parameter renormalization*)


(* ::Subsection:: *)
(*Formatting the functions*)


ParameterRenormalization[param_[inds__]]:=RenPrm[param,{inds}];

ParameterRenormalization[param_]:=RenPrm[param,{}];


(* ::Subsection:: *)
(*Main function: returns a renormalized parameter*)


RenPrm[param_,inds__List]:=Block[{PrmIndices={}, result},
  (* Restore the types of the indices *)
  If[inds=!={},PrmIndices=Inner[Index,$IndList[param]/.Index[aa_]->aa,inds/.Index[_,bb_]->bb,List]];

  (* Results *)
  (param[Sequence@@PrmIndices]/.param[]->param)+FR$delta[{param},PrmIndices]
];


(* ::Subsection:: *)
(*Derive all the renormalized parameters*)


(* ::Text:: *)
(*This produces a list of rules allowing to replace bare external parameters by renormalized ones*)


ParameterRenormalization[]:=Flatten[Block[{bare, ren, Inds,MyPattern,bare2,ren2,results,testpname,dum=#},
  (* Initialization *)
  testpname=If[MemberQ[MR$ParameterList,#], bare2=(ParameterName/.MR$ParameterRules[#]); bare2=!=ParameterName,False];
  If[testpname, If[ListQ[bare2], bare2=bare2/.Rule[a_,b_]->b, bare2={bare2}],False];

  (* Bare and renormalized parameter *)
  Inds=If[ListQ[$IndList[#]],Table[Unique["idx"],{Length[$IndList[#]]}],{}];
  bare=#[Sequence@@Inds]/.#[]->#;
  ren=ParameterRenormalization[bare]; 
  If[testpname, ren2=RenPrm[#,{}]&/@bare2];

  (* Prepare the replacement rule *)
  If[Inds=!={}, Inds=(MyPattern[#,Blank[]]&/@Inds); bare=#[Sequence@@Inner[Index,$IndList[#]/.Index[aa_]->aa,Inds,List]]/.#[]->#];

  (* output *)
  results=If[testpname, Join[{Rule[bare,ren]},Inner[Rule,bare2,ren2,List]], List[Rule[bare,ren]]];
  results/.MyPattern->Pattern]&/@
    (Flatten[Union[MassList[[2,All,2]],MR$ParameterList]]/.Rule[a_,b_]:>b)
];


(* ::Section:: *)
(*Tadpole renormalization*)


(* ::Subsection:: *)
(*Derive all the field shifts *)


(* ::Text:: *)
(*This produces a list of rules allowing to replace the fields with a vev by temself and a tadpole*)


TadpoleRenormalization[]:=Block[{tadrep={},vevtmp,vevfi,tadrules,vevkk,vevll},
tadrep={};
For[vevkk=1,vevkk<=Length[M$vevs],vevkk++,
  (*get the properties of the field with the vev*)
  vevtmp=Cases[M$ClassesDescription,_?(Not[FreeQ[#,If[Depth[M$vevs[[vevkk,1]]]>1,ClassName->Head[M$vevs[[vevkk,1]]],ClassName->M$vevs[[vevkk,1]]]]]&)];
  (*Abort if the field is not in the classes description*)
  If[Length[vevtmp]<1,Print[Style["Error : not all field in M$vevs are properly declared",Red]];Abort[]];
  (*Abort if the class description of that fields does contain its definition*)
  If[FreeQ[vevtmp,Definitions],Print[Style["Error : not all field in M$vevs are properly defined",Red]];Abort[]];
  (*the field after replacing it by its definition*)
  vevfi=M$vevs[[vevkk,1]]/.(Definitions/.vevtmp[[1,2]]);
  (*switch depending how many physical fields appear in vevfi*)
  Switch[Length[Cases[vevfi,_?FieldQ,\[Infinity]]],
    1,tadrules=Cases[vevfi,_?FieldQ,\[Infinity]][[1]];,
    (*shift the scalars or the pseudo scalars depending if the vev has a real or imaginary coefficient*)
    2,If[Im[Coefficient[vevfi,M$vevs[[vevkk,2]]]]===0,
       If[Im[Coefficient[vevfi,Cases[vevfi,_?FieldQ,\[Infinity]][[1]]]]===0&&SelfConjugateQ[Cases[vevfi,_?FieldQ,\[Infinity]][[1]]],
         tadrules=Cases[vevfi,_?FieldQ,\[Infinity]][[1]];,
         tadrules=Cases[vevfi,_?FieldQ,\[Infinity]][[2]];
       ],
       If[Im[Coefficient[vevfi,Cases[vevfi,_?FieldQ,\[Infinity]][[1]]]]===0&&SelfConjugateQ[Cases[vevfi,_?FieldQ,\[Infinity]][[1]]],
         tadrules=Cases[vevfi,_?FieldQ,\[Infinity]][[2]];,
         tadrules=Cases[vevfi,_?FieldQ,\[Infinity]][[1]];
       ];
     ],
    _,
    If[And@@SelfConjugateQ/@Cases[vevfi,_?FieldQ,\[Infinity]],
      If[Im[Coefficient[vevfi,M$vevs[[vevkk,2]]]]===0,tadrules=Cases[Simplify[vevfi+HC[vevfi]],_?FieldQ,\[Infinity]],tadrules=Cases[Simplify[vevfi+I*HC[vevfi]],_?FieldQ,\[Infinity]]],
      Print[Style["Error : Physical fields are not SelfConjugate",Red]];Abort[]];
  ];
  tadrules=Cases[{ExpandIndices[tadrules,FlavorExpand->True]},_?FieldQ,\[Infinity]];
  Print["tadruels"];
  Print[InputForm[tadrules]];
  For[vevll=1,vevll<=Length[tadrules],vevll++,
    tadrep=Append[tadrep,tadrules[[vevll]]->tadrules[[vevll]]-FR$CT*FR$deltat[tadrules[[vevll]]]/( Union[
    Cases[M$ClassesDescription,{c___,ClassName->tadrules[[vevll]],b___}:>(Mass/.{c,b})[[1]],2],
    Cases[M$ClassesDescription,{c___,ClassMembers->{xx___,tadrules[[vevll]],yy___},b___}:>(Mass/.{c,b})[[-Length[{yy}]-1,1]],2]][[1]])^2];
  ];
];
Union[tadrep]
];


(* ::Section:: *)
(*Mixing renormalization*)


(* ::Subsection::Closed:: *)
(*Tool functions*)


RmNoFInd[rep_,keptpos_,indname_]:=Module[{P2},
  {(Part[#[[1]],Flatten[keptpos]]->DeleteCases[#[[2]],Alternatives@@(List@@Delete[#[[1]],keptpos]/.Pattern->P2/.P2[a_,b_]->a),\[Infinity]]&)/@rep,indname}];

FlavorIndexQ[ind_]:=Not[FreeQ[M$ClassesDescription,FlavorIndex->ind]];

FlavorRules[rep_,ind_]:=Block[{frtmp=rep,P2},
  For[kfr=1,kfr<=Length[ind],kfr++,
    frtmp=Flatten[(If[IntegerQ[#[[1,kfr]]],#,Table[(#/.Pattern->P2/.P2[a_,b_]->a)/.#[[1,kfr,1]]->kifr,{kifr,Last[IndexRange[Index[ind[[kfr]]]]]}]]&)/@frtmp]
  ];
  frtmp
];

ToMembers[]:=Flatten[(Table[#1[ktm]->#2[[ktm]],{ktm,Length[#2]}]&)@@@(({ClassName,ClassMembers}/.#[[2]]&)/@DeleteCases[MR$ClassesDescription,_?(FreeQ[#,ClassMembers]&)])];


(* ::Subsection::Closed:: *)
(*Get the Vector mixing*)


(* ::Text:: *)
(*This produces a list of interaction eigenstates, mass eigenstates and mixing matrices for the vectors*)


VectorMixing[]:=Block[{vmix,vtmpmix,vimmix,kvm,masseg,posvm},
  (*gather the definitions of the vector*)
  vtmpmix=Cases[M$ClassesDescription,_?(Not[FreeQ[#,Definitions]]&&Head[#[[1]]]===V&)];
  (*remove the lorentz index and non flavor indices*)
  vtmpmix=({(Definitions/.#[[2]]/.a_?FieldQ:>Delete[a,1]),
            Position[FlavorIndexQ/@(Indices/.#[[2]]/.Index->Identity),True],
            Cases[Indices/.#[[2]]/.Index->Identity,_?FlavorIndexQ]}&)/@vtmpmix;
  vmix = RmNoFInd@@@vtmpmix;
  vmix=FlavorRules@@@vmix;
  vmix=Flatten[vmix]/.a_[]:>a;
  vmix=((#[[1]]->ExpandIndices[#[[2]],FlavorExpand->True]&)/@vmix)/.ToMembers[];
  vimmix={};
  For[kvm=1,kvm<=Length[vmix],kvm++,
    (*find the mass eigenstates from the definition*)
    masseg=DeleteDuplicates[Cases[vmix[[kvm,2]],_?FieldQ,\[Infinity]]];
    If[And@@(FreeQ[vimmix,#]&)/@masseg,
       (*mass eigenstates have not been found so far, add a new mixing line*)
      vimmix=Append[vimmix,{{vmix[[kvm,1]]},masseg,{(Coefficient[vmix[[kvm,2]],#]&)/@masseg}}],
       (*mass eigenstates have already been used, add the new state in the corresponding mixing*)
      posvm=((Position[vimmix,#][[1,1]]&)/@masseg)[[1]];
      vimmix[[posvm,1]]=Append[vimmix[[posvm,1]],vmix[[kvm,1]]];
      (*add new mass eigenstate if needed*)
      masseg = Join[vimmix[[posvm,2]],Complement[masseg,vimmix[[posvm,2]]]];
      (*add zero coefficients for the new states*)
      If[Length[masseg]>Length[vimmix[[posvm,2]]],vimmix[[posvm,3,;;]]=(Join[#,Table[0,{Length[masseg]-Length[vimmix[[posvm,2]]]}]]&)/@vimmix[[posvm,3,;;]]];
      vimmix[[posvm,2]]=masseg;
      vimmix[[posvm,3]]=Append[vimmix[[posvm,3]],(Coefficient[vmix[[kvm,2]],#]&)/@masseg];  
    ];
  ];
vimmix
];


(* ::Subsection::Closed:: *)
(*Get the Scalar mixing*)


(* ::Text:: *)
(*This produces a list of interaction eigenstates, mass eigenstates and mixing matrices for the scalars*)


ScalarMixing[]:=Block[{smix,stmpmix,simmix,ksm,masseg,possm},
  (*gather the definitions of the scalar*)
  stmpmix=Cases[M$ClassesDescription,_?(Not[FreeQ[#,Definitions]]&&Head[#[[1]]]===S&)];
  (*remove non flavor indices*)
  stmpmix=({(Definitions/.#[[2]]),
            Position[FlavorIndexQ/@(Indices/.#[[2]]/.Index->Identity),True],
            Cases[Indices/.#[[2]]/.Index->Identity,_?FlavorIndexQ]}&)/@stmpmix;
  smix = RmNoFInd@@@stmpmix;
  smix=FlavorRules@@@smix;
  smix=Flatten[smix]/.a_[]:>a;
  smix=((#[[1]]->ExpandIndices[#[[2]],FlavorExpand->True]&)/@smix)/.ToMembers[];
  simmix={};
  For[ksm=1,ksm<=Length[smix],ksm++,
    (*find the mass eigenstates from the definition*)
    masseg=DeleteDuplicates[Cases[{smix[[ksm,2]]},_?FieldQ,\[Infinity]]];
    If[And@@(FreeQ[simmix,#]&)/@masseg,
       (*mass eigenstates have not been found so far, add a new mixing line*)
      simmix=Append[simmix,{{smix[[ksm,1]]},masseg,{(Coefficient[smix[[ksm,2]],#]&)/@masseg}}],
       (*mass eigenstates have already been used, add the new state in the corresponding mixing*)
      possm=((Position[simmix,#][[1,1]]&)/@masseg)[[1]];
      simmix[[possm,1]]=Append[simmix[[possm,1]],smix[[ksm,1]]];
      (*add new mass eigenstate if needed*)
      masseg = Join[simmix[[possm,2]],Complement[masseg,simmix[[possm,2]]]];
      (*add zero coefficients for the new states*)
      If[Length[masseg]>Length[simmix[[possm,2]]],simmix[[possm,3,;;]]=(Join[#,Table[0,{Length[masseg]-Length[simmix[[possm,2]]]}]]&)/@simmix[[possm,3,;;]]];
      simmix[[possm,2]]=masseg;
      simmix[[possm,3]]=Append[simmix[[possm,3]],(Coefficient[smix[[ksm,2]],#]&)/@masseg];  
    ];
  ];
simmix
];


(* ::Subsection::Closed:: *)
(*Get the Fermion mixing*)


(* ::Text:: *)
(*This produces a list of interaction eigenstates, mass eigenstates and mixing matrices for the left and right fermions*)


FermionMixing[]:=Block[{smix,smixL,smixR,simmixL,simmixR,PR,PL,stmpmix,simmix,ksm,masseg,possm},
  (*gather the definitions of the scalar*)
  stmpmix=Cases[M$ClassesDescription,_?(Not[FreeQ[#,Definitions]]&&Head[#[[1]]]===F&)];
  (*remove non flavor indices*)
  stmpmix=({(Definitions/.#[[2]]/.a_?FieldQ:>Delete[a,1]),
            Position[FlavorIndexQ/@(Indices/.#[[2]]/.Index->Identity),True],
            Cases[Indices/.#[[2]]/.Index->Identity,_?FlavorIndexQ]}&)/@stmpmix;
  smix = RmNoFInd@@@stmpmix; 
  smix=FlavorRules@@@smix;
  smix=Flatten[smix]/.a_[]:>a;
  smix=(#[[1]]->ExpandIndices[Expand[(PR+PL)#[[2]]/.{_ProjP->PR,_ProjM->PL,Ga[5,___]->PR-PL}]/.{PR^2->PR,PL^2->PL,PL*PR->0},FlavorExpand->True]&)/@smix;
(*by default turn the first fermion index in spin but it is not as it has been removed*)
  smix=smix/.Index[Spin,a_]->a/.ToMembers[];
  smixL=smix/.PR->0;
  smixR=smix/.PL->0;
  For[kLR=1,kLR<=2,kLR++,
    If[kLR===1,smix=smixL,smix=smixR];
      simmix={};
      For[ksm=1,ksm<=Length[smix],ksm++,
        (*find the mass eigenstates from the definition*)
        masseg=DeleteDuplicates[Cases[{smix[[ksm,2]]},_?FieldQ,\[Infinity]]];
        If[And@@(FreeQ[simmix,#]&)/@masseg,
          (*mass eigenstates have not been found so far, add a new mixing line*)
          simmix=Append[simmix,{{smix[[ksm,1]]},masseg,{(Coefficient[smix[[ksm,2]],#]&)/@masseg}}],
          (*mass eigenstates have already been used, add the new state in the corresponding mixing*)
          possm=((Position[simmix,#][[1,1]]&)/@masseg)[[1]];
          simmix[[possm,1]]=Append[simmix[[possm,1]],smix[[ksm,1]]];
          (*add new mass eigenstate if needed*)
           masseg = Join[simmix[[possm,2]],Complement[masseg,simmix[[possm,2]]]];
          (*add zero coefficients for the new states*)
          If[Length[masseg]>Length[simmix[[possm,2]]],
            simmix[[possm,3,;;]]=(Join[#,Table[0,{Length[masseg]-Length[simmix[[possm,2]]]}]]&)/@simmix[[possm,3,;;]]];
          simmix[[possm,2]]=masseg;
          simmix[[possm,3]]=Append[simmix[[possm,3]],(Coefficient[smix[[ksm,2]],#]&)/@masseg];  
       ];
    ];
  simmix=DeleteCases[simmix,{{__},{},{{}}}];
  If[kLR===1,simmixL=simmix,simmixR=simmix];
  ];
  {simmixL,simmixR}/.{PR->1,PL->1}
];


(* ::Subsection:: *)
(*Get the mixing to renormalised and the associated masseigenstates*)


MixingToReno[osm_,intparam_,extparam_]:=Block[{osmix=osm,resom,kkom,kom,fimix,posom,tmp,tmp2,extrules,matparam,matdZL,matdZ,matdZR,mixrules},

(*replace classname by classmember*)
osmix=osmix/.Cases[(ClassName->ClassMembers/.#[[2]]&)/@MR$ClassesDescription,_?(FreeQ[#,ClassMembers]&)];
(*if not a list of list only, only one mixing*)
If[Not[Union[Head/@osmix]==={List}],osmix={osmix};];
(*flatten for classmembers mixing with other stuffs, remove double entries*)
osmix=Union/@Flatten/@osmix;

resom={};
For[kkom=1,kkom<=Length[osmix],kkom++,
  If[FermionQ[osmix[[kkom,1]]],tag="F";fimix=FermionMixing[];];
  If[ScalarFieldQ[osmix[[kkom,1]]],tag="B";fimix=ScalarMixing[];];
  If[VectorFieldQ[osmix[[kkom,1]]],tag="B";fimix=VectorMixing[];];
  (*contain the mixing fields in any order and/or as subset *)
  If[FreeQ[fimix,Alternatives@@Permutations[Append[osmix[[kkom]],___]]],Message[NLO::OnShellMixing];Abort[];];

  posom=Position[fimix,Alternatives@@Permutations[Append[osmix[[kkom]],___]]];
  If[tag==="F",
    For[kf=1,kf<=Length[posom],kf++,
   (*tag for a left or right mixing*)
    If[posom[[kf,1]]===1,tag="L",tag="R"];
    tmp=Extract[fimix,Delete[posom [[kf]],-1]] ;
    (*check if the interaction states are connected to other mass states and belong to the same multiplet(same Head and not Symbol)*)
    If[Not[Head[tmp[[1,1]]]===Symbol]&&Not[FreeQ[DeleteCases[fimix,tmp,{2}],Head[tmp[[1,1]]]]]&&Length[Union[Head/@tmp[[1]]]]===1,
      tmp2=Union[Extract[DeleteCases[fimix,tmp,{2}],((Drop[#,-3]&)/@Position[DeleteCases[fimix,tmp,{2}],Head[tmp[[1,1]]]])]];
      (*check for the same mumber of states, if multiplet has one index for the flavor and another one, *)
      If[Length[tmp2]===Length[tmp[[1]]]&&Length[tmp[[1,1]]]===2&&Sort[{Length[Union[tmp[[1,;;,1]]]],Length[Union[tmp[[1,;;,2]]]]}]==={1,Length[tmp[[1]]]},
        If[tmp[[1,1,1]]===tmp[[1,2,1]],
          (*first index is the same*)
          resom=Append[resom,{tag,tmp/.(#->Cases[tmp2,{{ReplacePart[#,1->_]},{me_},{{1}}}->me][[1]]&)/@tmp[[1]]}];,
          (*second index is the same*)
          resom=Append[resom,{tag,tmp/.(#->Cases[tmp2,{{ReplacePart[#,2->_]},{me_},{{1}}}->me][[1]]&)/@tmp[[1]]}];
        ],
        Print["No right state (cannot resolve the indices) found for the fermion mixing "<>ToString[osmix[[kkom]]]];
        resom=Append[resom,{tag,ReplacePart[tmp,1->{}]}];
      ],
      Print["No right state (not part of a larger multiplet) found for the fermion mixing "<>ToString[osmix[[kkom]]]];
      resom=Append[resom,{tag,ReplacePart[tmp,1->{}]}];
    ];
  ];
  ,(*not a fermion*)
  tmp=Extract[fimix,Delete[posom [[1]],-1]] ;
  resom=Append[resom,{tag,ReplacePart[tmp,1->{}]}];
  ];
];
resom=resom/.intparam;

(*solve for each mixing external parameter*)
mixrules={};
extrules=DeleteCases[extparam,_?(FreeQ[resom,#]&)];
extrules=(#->#+FR$CT *FR$delta[{#},{}]&)/@extrules;
For[kom=1,kom<=Length[resom],kom++,
  (*delta mixing from the param*)
  matparam=Normal[SeriesCoefficient[resom[[kom,2,3]]/.extrules,{FR$CT,0,1}]];
  matdZL=(Table[Table[DeleteCases[FR$deltaZ[{#[[ll]],#[[kk]]},{{}},If[Not[resom[[kom,1]]==="B"],resom[[kom,1]],MR$Null]],MR$Null,2],{ll,Length[#]}],{kk,Length[#]}]&)[resom[[kom,2,2]]];
  matdZL=matdZL-ConjugateTranspose[matdZL]/.Conjugate[FR$deltaZ[{a_,a_},b__]]->FR$deltaZ[{a,a},b]/.If[FreeQ[matparam,I],Conjugate->Identity,{}];
  matdZR=0*resom[[kom,2,3]];
  If[Length[resom[[kom,2,1]]]>0,
    matdZR=(Table[Table[FR$deltaZ[{#[[ll]],#[[kk]]},{{}},resom[[kom,1]]],{ll,Length[#]}],{kk,Length[#]}]&)[resom[[kom,2,1]]]; 
    matdZR=matdZR-ConjugateTranspose[matdZR]/.Conjugate[FR$deltaZ[{a_,a_},b__]]->FR$deltaZ[{a,a},b]/.If[FreeQ[matparam,I],Conjugate->Identity,{}];
  ];
  (*deltamixingn from the wf*)
  matdZ=Simplify[matdZR.resom[[kom,2,3]]/4+resom[[kom,2,3]].matdZL/4];
  mixrules=Join[mixrules,Solve[DeleteCases[(#1==#2&)@@@Transpose[{Flatten[matdZ],Flatten[matparam]}],_?(FreeQ[#,FR$delta]&)],DeleteDuplicates[Cases[matparam,_FR$delta,\[Infinity]]]][[1]]];
];
mixrules
];


(* ::Section::Closed:: *)
(*Perturbative expansion of the renormalization constants*)


(* ::Text:: *)
(*This function perturbatively expand renormalization constants, factorizing out the couplings as well as canonical 2 Pi factors. All the new constants are added to the parameter list of the model.*)


(* ::Subsection:: *)
(*This function prepares the addition of a renormalization constants to the parameter lists*)


(* ::Text:: *)
(*Note the treatment of the internal parameters.*)


AddRenConstToLists[deltaprm_List,param_,order_,orderlist_List]:=Block[{PrmFlag,rules,Pindx,IntPrmFlag,Pvalue,alows},
  (* Initialization *)
  PrmFlag=MemberQ[Union[MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList],param] ;
  rules=If[ListQ[MR$ParameterRules[param]],MR$ParameterRules[param],List[]];
  Pindx=Indices/.rules/.Indices->{};
  alows=AllowSummation/.rules/.AllowSummation->False;
  IntPrmFlag=If[rules=!=List[],(ParameterType/.rules/.ParameterType->Internal)===Internal,False];

  (* Wave function renormalization constant *)
  If[param===MR$Null,CreateDeltaParams/@deltaprm];

  (* External parameters without indices, including external masses *)
  If[param=!=MR$Null && Not[IntPrmFlag] && Pindx==={},CreateDeltaParams/@deltaprm];

  (* External parameters with indices *)
  If[param=!=MR$Null && Not[IntPrmFlag] && Pindx=!={},
  CreateDeltaParams[Replace[#,fg_[iii__]:>fg[Sequence@@(Pindx/.Index[type_]->Index[type,Unique["bla"]])]],AllowSummation->alows]&/@deltaprm];

  (* Internal parameter without indices: the value function must be calculated *)
  If[param=!=MR$Null && IntPrmFlag && Pindx==={},
    (* Calculating the value *)
    Pvalue=Value/.rules;
    Pvalue=Block[{dependencies,valtmp,PrmToRenormList,PVr,PVl,muf,MyRule},
      PrmToRenormList=Flatten[Union[MassList[[2,All,2]],MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList]]/.Rule[a_,b_]:>b;
      valtmp=Pvalue/.{Cos[x_]->x,Sin[x_]->x,Exp[x_]->x, Conjugate[x_]->x};
      dependencies=Flatten[ReplaceRepeated[valtmp,fg_?(AtomQ[#]&&Not[NumericQ[#]]&&Not[MemberQ[PrmToRenormList,#]]&)->MR$Null]/.MR$Null->List];
      dependencies=Union[DeleteCases[dependencies,_?(NumericQ[#]&)]];
      PVr=Rule[#,# (1+muf[#])]&/@dependencies;
      PVl={muf[#],0,1}&/@dependencies;
      valtmp=Normal[Series[Pvalue/.PVr,Sequence@@PVl]];
      PVr=MyRule[muf[#],(ParameterRenormalization[#]-#)/#/.FR$delta[rg__]:>PerturbativelyExpand[FR$delta[rg],order]]&/@dependencies;
      PVr=PVr/.MyRule->Rule;
      valtmp/.PVr]; 
    (* Matching the coefficients to the right terms of the expansion *)
    Block[{ valrule, term,mufmuf,keeporder},
      term=orderlist[[Position[deltaprm,#][[1,1]]]];
      Pvalue=Expand[Pvalue];
      valrule=If[Head[Pvalue]===Plus,List@@Pvalue,{Pvalue}];
      keeporder[gr_List]:=Select[gr, MatchQ[#//.{
        FR$delta[ar__][ord__]:>mufmuf[{ord}],mufmuf[aaa_List] mufmuf[bbb_List]->mufmuf[aaa+bbb], Power[mufmuf[aaa_List],n_]->mufmuf[n*aaa]},_* mufmuf[term]]&];
      valrule=Plus@@keeporder[valrule];
      CreateDeltaParams[#,Rule[Value,valrule]]]&/@deltaprm
    ];

  (* Internal parameter with indices: all the value functions must be calculated *)
  If[param=!=MR$Null && IntPrmFlag && Pindx=!={},
    (* Calculating the value *)
    Pvalue=Value/.rules;
    Pvalue=Block[{dependencies,valtmp,PrmToRenormList,PVr,PVl,muf,MyRule},
      PrmToRenormList=Flatten[Union[MassList[[2,All,2]],MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList]]/.Rule[a_,b_]:>b;
      valtmp=Pvalue/.{Cos[x_]->x,Sin[x_]->x,Exp[x_]->x};
      dependencies=(ReplaceRepeated[#,fg_?(AtomQ[#]&&Not[NumericQ[#]]&&Not[MemberQ[PrmToRenormList,#]]&)->MR$Null]/.MR$Null->List)&/@valtmp[[All,2]];
      dependencies=Union[DeleteCases[Flatten[dependencies],_?(NumericQ[#]&)]];
      PVr=Rule[#,# (1+muf[#])]&/@dependencies;
      PVl={muf[#],0,1}&/@dependencies;
      valtmp=Rule[#[[1]],Normal[Series[#[[2]]/.PVr,Sequence@@PVl]]]&/@Pvalue;
      PVr=MyRule[muf[#],(ParameterRenormalization[#]-#)/#/.FR$delta[rg__]:>PerturbativelyExpand[FR$delta[rg],order]]&/@dependencies;
      PVr=PVr/.MyRule->Rule;
      valtmp/.PVr]; 
    (* Matching the coefficients to the right terms of the expansion *)
    Block[{ valrule, term,LeftRuleMember,mufmuf=#/.fg_[ii__]->fg,mufmuf2,keeporder},
      term=orderlist[[Position[deltaprm,#][[1,1]]]];
      Pvalue={#[[1]],Expand[#[[2]]]}&/@Pvalue;
      valrule=If[Head[#[[2]]]===Plus,List@@#[[2]],{#[[2]]}]&/@Pvalue;
      keeporder[gr_List]:=Select[gr,MatchQ[#//.{
        FR$delta[ar__][ord__]:>mufmuf[{ord}],mufmuf[aaa_List] mufmuf[bbb_List]->mufmuf[aaa+bbb],Power[mufmuf[aaa_List],n_]->mufmuf[n*aaa]},_* mufmuf[term]]&];
      valrule=(Plus@@keeporder[#])&/@valrule;
      LeftRuleMember=#/.param->mufmuf&/@Pvalue[[All,1]];
      valrule=Inner[Rule,LeftRuleMember,valrule,List]; 
      CreateDeltaParams[
        Replace[#,fg_[ii__]->fg[Sequence@@(Pindx/.Index[type_]->Index[type,Unique["bla"]])]],Rule[Value,valrule],Rule[AllowSummation,alows]]]&/@deltaprm
  ];
];


(* ::Subsection::Closed:: *)
(*Adding a renormalization constant to the parameter lists, once everything has been prepared *)


Options[CreateDeltaParams]={Value-> MR$Null, AllowSummation->False};


CreateDeltaParams[delta_,OptionsPattern[]]:= Block[{Iprm,Iinds={},Pindices, Pdescr, Pcomp, Pinter,Pall=OptionValue[AllowSummation],MyPattern,mySet,
   PValue= OptionValue[Value],description},
  (* Initialization *)
  Iprm = delta/.pr_[__]->pr;
  If[MatchQ[delta,_[__]],Iinds=delta/._[inds__]->{inds}/.Index[type_,_]->Index[type]];

  (* Add the renormalization constant to the renormalization constant list *)
  M$RenormalizationConstants=Union[{Iprm},M$RenormalizationConstants];

  (* Attributes *)
  Pindices=Rule[Indices,Iinds];
  Pdescr=Rule[Description,"Renormalization constant"];
  Pcomp=Rule[ComplexParameter,True];
  Pinter=Rule[ParameterType,Internal];

  (* Get the function for internal parameters *)
  description=List[Pinter,Pindices,Pcomp,Pdescr];  
  If[PValue=!=MR$Null,description= Append[description, Rule[Value,PValue]]];
  If[Pall=!=False,description= Prepend[description, Rule[AllowSummation,Pall]]];

  (* Declare the new parameter *)
  DeclareTensor[Iprm, Iinds,Pcomp];
  If[Iinds==={},numQ[Iprm] = True];

  (* Appending to the various list of parameters *)
  If[Not[MemberQ[M$Parameters[[All,1]],Iprm]],
    M$Parameters=Append[M$Parameters,Equal[Iprm,description]];
    MR$ParameterRules[Iprm]=description;
    If[Iinds=!={},M$IndParam=Append[M$IndParam,Iprm]]
  ];
];


(* ::Subsection:: *)
(*Main routine for the perturbative expansion of any renormalization constant*)


PerturbativelyExpand[FR$del_[args__],ExpOrder_List]:=Block[{NewOrder,param,Pindx,CTType,MyTable,MyPattern,dummy,dummy2,resu,Prules,newparam,
  PrmToRenomList,deltaFuncList,deltaParamList,deltaOrderList},

  (* Initialization *)
  NewOrder=If[MatrixQ[ExpOrder],ExpOrder,{ExpOrder}];
  PrmToRenomList=Flatten[Union[MassList[[2,All,2]],MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList]]/.Rule[a_,b_]:>b;
  param=FR$del[args]/.FR$del[{prm_},__]->prm/.FR$del[args]->MR$Null;
  newparam=param;
  Pindx=FR$del[args]/.FR$del[_List,Indx_List]->Indx/.FR$del[args]->MR$Null;
  Prules=If[ListQ[MR$ParameterRules[param]],MR$ParameterRules[param],List[]];
  Prules=ParameterName/.Prules/.ParameterName->{};
  If[Not[ListQ[Prules]],Prules={param->Prules}];
  CTType=If[param===MR$Null,"deltaZ","delta"];

  (* Creation of three lists *)
  (* 1. deltaFuncList: list of the coefficient of the series expansion of the renormalization constant under a functional form *)
  (* 2. deltaParamList: list of the parameters associated to each coefficient *)
  (* 3. deltaOrderList: place of each coefficient in the series *)
  deltaFuncList=Drop[Flatten[
    MyTable[FR$del[dummy][NewOrder[[All,1]]],Sequence@@(List[#[[1]],0,#[[2]]] &/@NewOrder)]/.MyTable->Table],1]/.FR$del[dummy]->FR$del[args];
  deltaParamList=(#/.FR$del[fld_List,inds_List,opt___][ord_List]:> 
    Symbol[StringJoin[CTType,opt, Sequence@@(ToString[#]&/@fld),Sequence@@(ToString[#]&/@ord)]][Sequence@@Flatten[inds]])&/@deltaFuncList;
  deltaParamList=If[deltaFuncList=!={},deltaParamList/.fff_[]->fff,{}];
  deltaOrderList=deltaFuncList/.FR$del[args][orde_List]->orde;
  deltaFuncList=#/.FR$del[fi_List,inds_List,opt___][orde_List]:>FR$del[fi,{MyPattern[ids,BlankNullSequence[]]},opt][Sequence@@orde]&/@deltaFuncList;

  (* We now add the series coefficient to the various parameter lists, if they are not in it already *)
  dummy=#/.fg_[__]->fg&/@deltaParamList;
  If[Complement[dummy,Intersection[dummy,M$RenormalizationConstants]]=!={},
    If[Prules=!={} && (param/.Prules)=!=param,
     (* Case 1, the ParameterName is different from the parameter label *)
       newparam=param/.Prules;
       (* Mapping function to parameter *)
       M$DeltaToParameters=Union[Inner[MyRule,(deltaFuncList/.param->newparam),
         If[MatchQ[#,_[__]], Print["test::",#, " :: ", param, " :: ",newparam];Print[#/.pr_[inds__]:>StringReplace[ToString[pr],ToString[param]->ToString[newparam]]]
           #/.pr_[inds__]:>Symbol[StringReplace[ToString[pr],ToString[param]->ToString[newparam]]][ids],
           #/.#->Symbol[StringReplace[ToString[#],ToString[param]->ToString[newparam]]]]&/@deltaParamList,
         List],M$DeltaToParameters];
       M$DeltaToParameters=Union[M$DeltaToParameters/.MyPattern->Pattern/.MyRule->RuleDelayed];
       (* Creating constants associated to the ParameterName *)
       dummy2=Symbol[StringReplace[ToString[#/.fg_[__]->fg],ToString[param]->ToString[newparam]]]&/@deltaParamList;
       If[Complement[dummy2,Intersection[dummy2,M$RenormalizationConstants]]=!={},
         AddRenConstToLists[Symbol[StringReplace[ToString[#],ToString[param]->ToString[newparam]]]&/@deltaParamList, newparam,NewOrder,deltaOrderList]],
   (* Case 2: the normal case *)
      (* Mapping function to parameter *)
      M$DeltaToParameters=Union[Inner[MyRule,deltaFuncList,#/.pr_[inds__]->pr[ids]&/@deltaParamList,List],M$DeltaToParameters];
      M$DeltaToParameters=Union[M$DeltaToParameters/.MyPattern->Pattern/.MyRule->RuleDelayed];
      (*Creating the new parameters *)
      AddRenConstToLists[deltaParamList, param,NewOrder,deltaOrderList]]];
  Clear[dummy];

  (* And now finally, the expansion of the renormalization constants *)
  resu=Expand[Normal[Series[FR$del[dummy][Sequence@@NewOrder[[All,1]]],Sequence@@(List[#[[1]],0,#[[2]]]&/@NewOrder)]]];
  resu=resu/.{FR$del[dummy][__]->0,Derivative[aa__][FR$del[dummy]][__]->FR$del[dummy][aa]};
  resu=resu/.(Rule[#[[1]],#[[1]]/(2 Pi)]&/@NewOrder)/.(Rule[Power[#[[1]],n_],Power[#[[1]],n]*Factorial[n]]&/@NewOrder);
  resu=resu/.FR$del[dummy]->FR$del[args];
  If[param=!=newparam,resu=resu/.param->newparam];
resu];


(* ::Section:: *)
(*Extraction of the counterterm Lagrangian*)


(* ::Subsection:: *)
(*ExtractCounterterms*)


ExtractCounterterms[lagr_,ExpOrder_List]:=Block[{NewOrder,ExpLag,WaveFunctions,PrmToRenormList,GrpMat,MyRuleDelayed},
  (* Initialization *)
  Print["Extraction of the counterterm Lagrangian."];
  NewOrder=If[MatrixQ[ExpOrder],ExpOrder,{ExpOrder}];
  ExpLag=Expand[ExpandIndices[lagr]];
  WaveFunctions=FieldRenormalization[];
  ParameterRenormalization[];
  PrmToRenormList=Flatten[Union[MassList[[2,All,2]],MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList]]/.Rule[a_,b_]:>b;
  ExpLag=If[Head[ExpLag]=!=Plus,{ExpLag},List@@ExpLag];

  GrpMat=Flatten[DeleteCases[(Representations/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList,Representations],1][[All,1]];
  GrpMat=Join[GrpMat,DeleteCases[(StructureConstant/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList,StructureConstant]];

  (* Field renormalization and simplifications *)
  Print["Field renormalization... "];
  ExpLag=Block[{tmpresu},
    tmpresu=Expand[#/.WaveFunctions];
    tmpresu=Expand[tmpresu/.Dot->FR$Dot]/.FR$Dot->Dot;
    tmpresu=OptimizeIndex[Expand[tmpresu]];
  tmpresu]&/@ExpLag;
  ExpLag=Plus@@ExpLag;
  ExpLag = If[Head[ExpLag]=!=Plus,{ExpLag},List@@ExpLag];

  (* Parameter renormalization *)
  (* Care must be taken when we have a term with a function of several parameters *)
  Print["Parameter renormalization..."];
  ExpLag = Block[{PValue,valtmp,dependencies,PVr, PVl,muf,tmpdot},
    PValue=#/.TensDot->tmpdot//.{
      del[_,_]->1,aa_?(FieldQ[#]===True || AntiFieldQ[#]===True&)[inds__]->1, aa_?(FieldQ[#]===True || AntiFieldQ[#]===True&)->1,
      Ga[__]->1,ProjP[__]->1,ProjM[__]->1,
      tmpdot[__][_,_]->1,
      FR$deltaZ[__]->1,
      aa_?(MemberQ[GrpMat,#]&)[inds__]->1};
    valtmp=PValue/.{Cos[x_]->x,Sin[x_]->x,Exp[x_]->x, Conjugate[x_]->x, IndexDelta[_,_]->1}; 
    dependencies = DeleteCases[Flatten[ReplaceRepeated[{valtmp}, {_?(NumericQ[#]&)->1,Times->List}]],1];
    If[dependencies=!={}, 
      PVr=Rule[#,# (1+muf[#])]&/@dependencies;
      PVl={muf[#],0,1}&/@dependencies;
      valtmp=Normal[Series[PValue/.PVr,Sequence@@PVl]];
      PVr=MyRule[muf[#],(ParameterRenormalization[#]-#)/#]&/@dependencies;
      PVr=PVr/.MyRule->Rule;
      OptimizeIndex[Expand[#/.PValue->valtmp/.PVr]],
      OptimizeIndex[#]]]&/@ExpLag;

  (* Expansion of the renormalization constants *)
  Print["Renormalization constant perturbative expansion..."];
  ExpLag = Expand[# /. FR$deltaZ[args__] :> PerturbativelyExpand[FR$deltaZ[args], NewOrder]] &/@ ExpLag;
  ExpLag = Expand[# /. FR$delta[args__] :> PerturbativelyExpand[FR$delta[args], NewOrder]] &/@ ExpLag; 

  (* Rejecting all terms which have an higher order *)
  ExpLag=Plus@@ExpLag;
  ExpLag = If[Head[ExpLag]=!=Plus,{ExpLag},List@@ExpLag];
  ExpLag = Block[{ttemp, mufmuf, result}, 
    ttemp = # //. {FR$delta[ar__][ord__] :> mufmuf[{ord}], FR$deltaZ[ar__][ord__] :> mufmuf[{ord}], 
      mufmuf[aaa_List]*mufmuf[bbb_List] -> mufmuf[aaa+bbb], Power[mufmuf[aaa_List],n_] -> mufmuf[n*aaa], Conjugate[mufmuf[arg__]] -> mufmuf[arg]};
    result = If[MatchQ[ttemp, _*mufmuf[__]], 
      ttemp = ttemp /.(_*mufmuf[argus__] :> Select[NewOrder[[All,2]] - argus, #1 < 0 & ]); If[ttemp =!= {}, 0, #],
      #];
    result] &/@ ExpLag;
  ExpLag=DeleteCases[ExpLag,0];

  (* Formatting *)
  ExpLag=ExpLag/.M$DeltaToParameters/.aa_?(MemberQ[M$RenormalizationConstants,#]&)[args1_?(Head[#]===List&),args___]:>aa[Sequence@@Join[args1,args]];

  (* Outputting *)
  Print["Done."];
Plus@@ExpLag];


(* ::Subsection:: *)
(*On-shell renormalisation*)


IndExpand[var_,ind_]:=Block[{indlistIE,indlist2IE,idIE},
indlistIE=Table[ToExpression["ind"<>ToString[kk]],{kk,Length[ind]}];
indlist2IE=Table[{indlistIE[[kk]],Length[IndexRange[ind[[kk]]]]},{kk,Length[ind]}];
idIE=Table[Index[ind[[kk,1]],indlistIE[[kk]]],{kk,Length[ind]}];
Table@@Prepend[indlist2IE,var@@idIE]
]


InternalRule[param_]:=If[FreeQ[param,Indices],
	If[FreeQ[param,Value],Definitions/.param[[2]],param[[1]]->Value/.param[[2]]],
    (Rule[#,(#/.Index[a_,b_]->b)/.(If[FreeQ[param,Definitions],
      Value,
      If[FreeQ[param,Value],Definitions,Join[Definitions,Value]]]/.param[[2]])]&)/@Flatten[IndExpand[param[[1]],(Indices/.param[[2]])]]
];


PlusI[x__]:=Plus@@(I*List[x])


(*Dot for the renormalization*)
RenDot[a_+b_,c_]:=RenDot[a,c]+RenDot[b,c];
RenDot[c_,a_+b_]:=RenDot[c,a]+RenDot[c,b];
RenDot[c_,FR$CT*b_]:=FR$CT*RenDot[c,b];
RenDot[FR$CT*b_,c_]:=FR$CT*RenDot[b,c];
RenDot[c_,a_FR$deltaZ*b_]:=a*RenDot[c,b];
RenDot[a_FR$deltaZ*b_,c_]:=a*RenDot[b,c];


CCtmp[a_+b_]:=CCtmp[a]+CCtmp[b];
CCtmp[FR$CT*a_]:=FR$CT*CCtmp[a];
CCtmp[1/2a_]:=CCtmp[a]/2;
CCtmp[a_FR$deltaZ*b_]:=a*CCtmp[b];
CCtmp[Conjugate[a_FR$deltaZ]*b_]:=Conjugate[a]*CCtmp[b];


Dmshift[x_]:=Block[{FR$delta2},If[GhostFieldQ[x[[1]]]||GoldstoneQ[x[[1]]],
  {},
  If[FreeQ[x,FR$delta],{},Solve[(x[[2]]/.{FR$deltat[_]->0,FR$delta[zz__]->FR$delta2[zz]}//Simplify)==(x[[2]]),{Cases[x[[2]],_FR$delta,\[Infinity]][[1]]}]/.FR$delta2->FR$delta],
  If[FreeQ[x,FR$delta],{},Solve[(x[[2]]/.{FR$deltat[_]->0,FR$delta[zz__]->FR$delta2[zz]}//Simplify)==(x[[2]]),{Cases[x[[2]],_FR$delta,\[Infinity]][[1]]}]/.FR$delta2->FR$delta]
]];

AntiField2Field[x_]:=If[AntiFieldQ[x],anti[x],x];

DZmixShift[fr_]:=Module[{xx,fi=AntiField2Field/@fr[[1,All,1]]},
{FR$deltaZ[fi,{{}}]->FR$deltaZ[fi,{{}}]+xx,FR$deltaZ[Reverse[fi],{{}}]->FR$deltaZ[Reverse[fi],{{}}]-xx}/.Solve[
  (Coefficient[fr[[2]],SP[2,2],0]/.FR$deltat[__]->0)==(Coefficient[fr[[2]],SP[2,2],0]/.{FR$deltaZ[fi,{{}}]->FR$deltaZ[fi,{{}}]+xx,
  FR$deltaZ[Reverse[fi],{{}}]->FR$deltaZ[Reverse[fi],{{}}]-xx}),xx][[1]]/.xx->0
];


Options[OnShellRenormalization] = {QCDOnly->False,FlavorMixing->True,Only2Point->False,Simplify2pt->True,Exclude4ScalarsCT->False,OnShellMixing->{},MixAndTadOnly->False,Assumptions->{}};


OnShellRenormalization[Lag_,options___]:=Module[{FieldRenoList,ExternalParamList,InternalParamList,internalMasses,massRules={}, deltaLagp,deltaLag,classname, classmembers,
flavor,fi,paramreno={},FreeM,Patbis,qcd,flm,only2,qcdind,qcdclasses,kk1,extNotMass,extNotMass2,cvar,tmppara,itp,tmp,tmpRule,lkinmass,GetnFlavor,extfla,MassFreeQ,skin,no4S,lag4S,Pow,
deltaLagt,massspec,replist,InternalParamList2,lagtmp,frtmp,logfile,TestPrm,mixreno={},mixfi,mixonly,assum,mytime,lagtmp2},

mytime=SessionTime[];

Off[Simplify::time];
qcd=QCDOnly/.{options}/.Options[OnShellRenormalization];
flm=FlavorMixing/.{options}/.Options[OnShellRenormalization];
only2=Only2Point/.{options}/.Options[OnShellRenormalization];
skin=Simplify2pt/.{options}/.Options[OnShellRenormalization];
no4S=Exclude4ScalarsCT/.{options}/.Options[OnShellRenormalization];
mixfi=OnShellMixing/.{options}/.Options[OnShellRenormalization];
mixonly=MixAndTadOnly/.{options}/.Options[OnShellRenormalization];
assum=Assumptions/.{options}/.Options[OnShellRenormalization];

Print[flm];

logfile=OpenWrite[];

  (* Initialization *)
  If[Length[FR$RmDblExt]===0,FR$RmDblExt={};];
  If[Length[FR$QCDRenormalize]===0,FR$QCDRenormalize={};];
  FR$Loop=True; (*Keep two point in the vertices list*)

(*Replace ParameterName as they are not used in the renormalization*)
deltaLag=Expand[Lag/.Reverse/@Cases[DeleteCases[Flatten[(ParameterName/.#[[2]]&)/@M$Parameters],ParameterName],_Rule]];

Print["Expanding over flavor at "<>ToString[SessionTime[]-mytime]];
deltaLag=ExpandIndices[deltaLag, FlavorExpand->True,MinParticles->1];
Print["Expansion over the flavor indices done at "<>ToString[SessionTime[]-mytime]];

(*danger for tadpole renormalisation as it can give 3 pt vertices then*)
If[no4S,
  Print["Putting 4 scalars term aside for the renormalization"];
  lag4S=Total[Cases[deltaLag,_?((Count[#//.{Dot->Times,Power[a_,b_Integer]:>Pow@@Table[a,{b}],del[a_,b_]:>Identity[a]},_?ScalarFieldQ,\[Infinity]])>3&)]];
  ,
  lag4S=0;
];
deltaLag=deltaLag-lag4S;
Print["4S on the side at "<>ToString[SessionTime[]-mytime]];

If[skin,
Print["Extracting the mass and kinetic terms to simplify them"];
  lkinmass=ExpandIndices[GetKineticTerms[deltaLag]+GetMassTerms[deltaLag]+ExpandIndices[deltaLag,MaxParticles->1,MinParticles->1],FlavorExpand->True];
  deltaLag=deltaLag-lkinmass;,
  lkinmass=0;
];
Print["<=2 fields on the side at "<>ToString[SessionTime[]-mytime]]; 

GetnFlavor[field_,n_,x_]:=If[AntiFieldQ[field],
  If[x==2,anti,Identity][((ClassMembers/.(Cases[M$ClassesDescription,_?(Not[FreeQ[#,ClassName->anti[field]]]&)][[1,2]]))[[n]])],
  (ClassMembers/.(Cases[M$ClassesDescription,_?(Not[FreeQ[#,ClassName->field]]&)][[1,2]]))[[n]]];

MassFreeQ[x_]:=And@@((FreeQ[x,#]&)/@MassList[[2,All,2]]);

FR$DoPara=If[Global`FR$Parallelize && Length[Lag]>10 && $KernelCount>1,True,False];

(*list of complex variables*)
cvar=Flatten[({#[___],#}&)/@Cases[M$Parameters,_?(Not[FreeQ[#,ComplexParameter->True]]||
  (Not[FreeQ[#,Indices]]&&FreeQ[#,Orthogonal->True]&&FreeQ[#,ComplexParameter->False])&)][[All,1]]];

qcdind=(Representations/.Flatten[Cases[M$GaugeGroups,_?(Not[FreeQ[#,gs]]&),{2}]])[[All,2]];
qcdclasses=DeleteCases[DeleteCases[M$ClassesDescription,_?(Not[FreeQ[#,Unphysical->True]]||Not[FreeQ[#,Ghost]]&)],
           _?(And@@Table[FreeQ[#,qcdind[[kk]]],{kk,Length[qcdind]}]&)];



(*Check if the on-shell scheme is feasible, i.e. all the masses are external parameters*)
If[Not[mixonly],
  internalMasses=Cases[Union[(DeleteCases[MassList,{_,m_?(Not[FreeQ[MassList,#]]&),Internal},2])[[2,All,2;;3]]],{xx_,Internal}->xx];
  If[Length[internalMasses]>0&&
    If[Length[FR$LoopSwitches]>0,
       Length[Complement[internalMasses,FR$LoopSwitches[[All,2]]]]>0,True],
       Message[NLO::ExtMass];Return[]];
];
Print["onshell check done at "<>ToString[SessionTime[]-mytime]];

Print["renormalizing the fields at"<>ToString[SessionTime[]-mytime]];
(*No renormalization of the ghost field is needed*)
FieldRenoList=DeleteCases[FieldRenormalization[], _?(GhostFieldQ[#[[1]]] &)]/.FR$deltaZ[xx__]->FR$CT*FR$deltaZ[xx];
(*Expand the flavors*)
For[fi=1,fi<=Length[FieldRenoList],fi++,
  If[FlavoredQ[FieldRenoList[[fi,1]]],
    classname=Head[FieldRenoList[[fi,1]]];
    {classmembers,flavor}={ClassMembers,FlavorIndex}/.Join[Cases[M$ClassesDescription,_?(Not[FreeQ[#,ClassName->classname]]&),{2}],
                                                          Cases[M$ClassesDescription,_?(Not[FreeQ[#,ClassName->anti[classname]]]&),{2}]][[1]];
    
   extfla=Cases[FieldRenoList[[fi,1]],Index[flavor,a_]->a][[1,1]];

      FieldRenoList[[fi]]=Table[FieldRenoList[[fi]],{jj,Length[classmembers]}];
      For[kk1=1,kk1<=Length[classmembers],kk1++,
        FieldRenoList[[fi,kk1,1]]=DeleteCases[FieldRenoList[[fi,kk1,1]],Index[flavor,_]]/.{classname->If[Not[AntiFieldQ[classname]],classmembers[[kk1]],anti[classmembers[[kk1]]]]};
        FieldRenoList[[fi,kk1,2,2]]=(DeleteCases[Coefficient[Refine[FieldRenoList[[fi,kk1,2,2]],Assumptions->{FR$CT\[Element]Reals}],FR$CT,0],Index[flavor,extfla]]/.
            If[Not[AntiFieldQ[classname]],{classname->classmembers[[kk1]]}, {classname->anti[classmembers[[kk1]]]}])+ 
          FR$CT*(Coefficient[Refine[FieldRenoList[[fi,kk1,2,2]],Assumptions->{FR$CT\[Element]Reals}],FR$CT,1]/.
            If[Not[AntiFieldQ[classname]],FR$deltaZ[{classname,b_},{{Index[flavor,extfla],c___}},d___]->FR$deltaZ[{classmembers[[kk1]],b},{{c}},d],FR$deltaZ[{anti[classname],b_},{{Index[flavor,extfla],c___}},d___]->FR$deltaZ[{classmembers[[kk1]],b},{{c}},d]]);
      ];
  ];
];
FieldRenoList=Flatten[FieldRenoList];
Print["renormalizing the fields 2 at"<>ToString[SessionTime[]-mytime]];
FieldRenoList=FieldRenoList/.Conjugate[FR$CT*b__]->Conjugate[b]*FR$CT;

FieldRenoList[[All,2,2]] = FieldRenoList[[All,2,2]]/.{FR$deltaZ[{a_,b_},{{c_}},d___]*b_[in___,c_,e___]:>
  Sum[FR$deltaZ[{a,GetnFlavor[b,kk,1]},{{}},d](GetnFlavor[b,kk,2])[in,e],{kk,Length[IndexRange[Index[c[[1]]]]]}],
  Conjugate[FR$deltaZ[{a_,bbar_},{{c_}},d___]]*b_[in___,c_,e___]:>
  Sum[Conjugate[FR$deltaZ[{a,GetnFlavor[b,kk,1]},{{}},d]](GetnFlavor[b,kk,2])[in,e],{kk,Length[IndexRange[Index[c[[1]]]]]}]/;anti[bbar]==b};
FieldRenoList=FieldRenoList/.x_[]:>x/;FieldQ[x];

Print["renormalizing the fields 3 at"<>ToString[SessionTime[]-mytime]];

If[Not[flm],
  Print["No mixing allowed for the renormalization"];
  FieldRenoList[[All,2]]=FieldRenoList[[All,2]]/.{FR$deltaZ[{a_,b_},z__]:>0/;Not[a===b]};,
  Print["All mixing allowed for the renormalization"];,
  Print["Some mixing allowed for the renormalization"];
  FieldRenoList[[All,2]]=FieldRenoList[[All,2]]/.{FR$deltaZ[{a_,b_},z__]:>0/;Not[a===b]&&FreeQ[flm,{a,b}]&&FreeQ[flm,{b,a}]};
];
Print["renormalizing the fields 4 at"<>ToString[SessionTime[]-mytime]];
If[mixonly,
  FieldRenoList=FieldRenoList/.FR$deltaZ[{a_,a_},z__]:>0;
];
Print["renormalizing the fields 5 at"<>ToString[SessionTime[]-mytime]];

If[qcd,FieldRenoList=DeleteCases[FieldRenoList,_?(FreeQ[qcdclasses,Head[#[[1]]]]&&FreeQ[qcdclasses,anti[Head[#[[1]]]]]&)];];
(*Print[FieldRenoList];*)
Print["renormalizing the parameters"];


InternalParamList=Cases[M$Parameters,xx_?(Not[FreeQ[#,Internal]]&)];

(*Replacement rules for the internal parameters*)
InternalParamList=Flatten[InternalRule/@InternalParamList];
Print["renormalizing the param at"<>ToString[SessionTime[]-mytime]];

(*Invert the replacement for the element in FR$LoopSwitches[[All,2]]*)
If[Length[FR$LoopSwitches]>0,
  InternalParamList=Flatten[(If[FreeQ[FR$LoopSwitches[[All,2]],#1],#1->#2,
    Solve[#1==(#2//.InternalParamList),FR$LoopSwitches[[Position[FR$LoopSwitches[[All,2]],#1][[1,1]],1]] ][[1]]]&)@@@InternalParamList/.ConditionalExpression[a_,b_]->a];
];

Print["renormalizing the param 2 at"<>ToString[SessionTime[]-mytime]];
If[Not[mixonly],Print["in if"];
  (*List of external parameters that should be renormalized*)
  (* Parameters to be renormalized: adding the matrix case (BENJ's modif) *)
  TestPrm[prm_]:=Block[{mypar=prm},
    If[MatchQ[prm,_[__]], mypar=prm/.a_[__]->a];
    Return[Not[FreeQ[Cases[M$Parameters,mypar=={x__}],QCD]] || MemberQ[FR$QCDRenormalize,mypar]];
  ];
  ExternalParamList=DeleteCases[M$Parameters,_?(FreeQ[#,External]&)][[All]];
  ExternalParamList=Flatten[(If[FreeQ[#,Indices],#[[1]],IndExpand[#[[1]],(Indices/.#[[2]])]]&)/@ExternalParamList];
  If[Length[FR$LoopSwitches]>0,ExternalParamList=ExternalParamList/.Rule@@@FR$LoopSwitches;];
  extNotMass=Cases[ExternalParamList/.FR$RmDblExt,_?MassFreeQ];
  extNotMass2=extNotMass;
  (*  BENJ's FIX *)
  (* old stuff:   If[qcd,extNotMass=DeleteCases[extNotMass,_?(FreeQ[Cases[M$Parameters,#=={x__}],QCD]&)]]; *)
  If[qcd, extNotMass=Select[extNotMass,TestPrm[#]&]];
  (* END of BENJ's FIX *)

  (*renormalization of the masses only*)
  Print["renormalizing the masses"];
  paramreno=((#->#+FR$CT*FR$delta[{#},{}])&)/@If[qcd,DeleteCases[Union[MassList[[2,All,2]]],_?(FreeQ[qcdclasses,#]&)],Union[MassList[[2,All,2]]]];

  If[Not[only2],
    Print["renormalizing the other external parameters"];
    paramreno=Join[paramreno,((#->(#+FR$CT*FR$delta[{#},{}]/.Pattern->Identity))&)/@extNotMass];];

  paramreno = DeleteCases[paramreno,_?((#[[1]]/.MR$Definitions)===0&)];

  Print["Internal parameter renormalization"];
  massRules=If[Length[FR$RmDblExt]>0,Join[FR$RmDblExt,Cases[InternalParamList,_?(Not[(#[[2]]/.FR$RmDblExt)===#[[2]]]&)]/.FR$RmDblExt],{}];
];
Print["renormalizing the param 3 at"<>ToString[SessionTime[]-mytime]];
Print["after if "];

InternalParamList = Join[InternalParamList,FR$RmDblExt];
InternalParamList[[All,2]]=InternalParamList[[All,2]]/.MR$Definitions;
InternalParamList[[All,2]] = InternalParamList[[All,2]]//.InternalParamList;
Print["renormalizing the param 4 at"<>ToString[SessionTime[]-mytime]];
Print["Simplifying the kinetic and mass terms"];
(*Group terms by field content, simplify each term on its onwn then add them to be faster*)
lkinmass=Expand[ lkinmass];
lkinmass=If[Head[lkinmass]===Plus,
  Plus@@((Simplify[#,Assumptions->assum]&)/@(Plus@@@GatherBy[List@@lkinmass,(Cases[#,_?FieldQ,\[Infinity]]&) ]/.InternalParamList)),
  Simplify[lkinmass,Assumptions->assum]];
If[lkinmass=!=0, lkinmass=Plus@@((Simplify[#,Assumptions->assum]&)/@(Plus@@@GatherBy[List@@Expand[ lkinmass],(Cases[#,_?FieldQ,\[Infinity]]&) ]/.InternalParamList))];

Print["simplification done at "<>ToString[SessionTime[]-mytime]];
InternalParamList2=InternalParamList;

(*mixing renormalization*)
If[Length[mixfi]>0,
  mixreno=MixingToReno[mixfi,InternalParamList2,extNotMass2];
  extNotMass2 = DeleteCases[extNotMass2,_?(FreeQ[mixreno,#]&)];
  paramreno=Join[paramreno,((#->(#+FR$CT*FR$delta[{#},{}]/.Pattern->Identity))&)/@extNotMass2];
];
Print["renormalizing the param 5 at"<>ToString[SessionTime[]-mytime]];

InternalParamList[[All,2]] = InternalParamList[[All,2]]/.massRules/.Union[paramreno,((Rule[#[[1]]/.Index[a_,b_]->b,#[[2]]]&)/@paramreno)];
InternalParamList=(If[Not[#2===0],{#1->#1(1+FR$CT*Simplify[SeriesCoefficient[#2,{FR$CT,0,1}]/Normal[Series[#2,{FR$CT,0,0}]],TimeConstraint->0.02]),
    Conjugate[#1]->Conjugate[#1](1+FR$CT*Conjugate[Simplify[SeriesCoefficient[#2,{FR$CT,0,1}]/Normal[Series[#2,{FR$CT,0,0}]],TimeConstraint->0.02]])},
  #1->#2]&)@@@(InternalParamList);
InternalParamList=Flatten[InternalParamList/.Conjugate[FR$delta[a__]]->FR$delta[a]];


Print["renormalizing the Lagrangian at"<>ToString[SessionTime[]-mytime]];
deltaLag = deltaLag+Expand[lkinmass];
(*Replace vanishing parameters*)
deltaLag = deltaLag/.(Cases[InternalParamList/.Rule->tmpRule,tmpRule[_,0]]/.tmpRule->Rule);

If[Not[mixonly],
  Print["with the parameters at"<>ToString[SessionTime[]-mytime]];
  If[FR$DoPara, 
     DistributeDefinitions[massRules,paramreno,InternalParamList,PlusI];
     tmp=Table[tmppara=deltaLag[[itp]];
       ParallelSubmit[{itp,tmppara},(Expand[(#(*//.massRules*)/.paramreno/.InternalParamList),FR$CT]&)[tmppara]],{itp,Length[deltaLag]}];
     deltaLagp=Plus@@(WaitAll[tmp]/.FR$CT^n_Integer:>0/;n>1);
     tmp=Table[tmppara=deltaLagp[[itp]];
       ParallelSubmit[{itp,tmppara},(Replace[#,Times[I*a_Plus]:>PlusI@@a,1]&)[tmppara]],{itp,Length[deltaLagp]}];
     deltaLagp=Plus@@WaitAll[tmp];
     tmp=Table[tmppara=deltaLagp[[itp]]/.Dot->RenDot/.RenDot->Dot;
       ParallelSubmit[{itp,tmppara},(SeriesCoefficient[#,{FR$CT,0,1}]&)[tmppara]],{itp,Length[deltaLagp]}];
     deltaLagp=Plus@@WaitAll[tmp];
     ,
     deltaLagp=((Expand[(#(*//.massRules*)/.paramreno/.InternalParamList),FR$CT]&)/@deltaLag)/.FR$CT^n_Integer:>0/;n>1;
     deltaLagp=(Replace[#,Times[I*a_Plus]:>PlusI@@a,1]&)/@deltaLagp;
     deltaLagp=(SeriesCoefficient[#,{FR$CT,0,1}]&)/@(deltaLagp/.Dot->RenDot/.RenDot->Dot);
  ];
  deltaLagp=If[Head[deltaLagp]===Plus,(FR$CT*#&)/@deltaLagp, FR$CT*deltaLagp]/.mixreno;
  ,
  deltaLagp=0;
];

(*Print[DeleteCases[deltaLagp,_?(FreeQ[#,FR$deltaZ]&)]];*)
(*Print[InputForm[deltaLagp]];*)
If[qcd,deltaLagt=0;lagtmp=0;,
  Print["with the tadpoles at "<>ToString[SessionTime[]-mytime]];
  deltaLagt=(Coefficient[#,FR$CT]&)/@(deltaLag/.TadpoleRenormalization[])*FR$CT;
  (*shift the mass renormalization constant to have them only in the mass terms*)
  If[Not[mixonly],
    $Output=logfile;
    lagtmp = Total[Cases[Expand[deltaLagt/.del[__]->0],_?((Count[#//.{Dot->Times,Power[a_,b_Integer]:>Pow@@Table[a,{b}],a_[b___]:>a/;FieldQ[a]},_?FieldQ,\[Infinity]])==3&)]];
    lagtmp = lagtmp + Total[Cases[Expand[deltaLagp/.del[__]->0],_?((Count[#//.{Dot->Times,Power[a_,b_Integer]:>Pow@@Table[a,{b}],a_[b___]:>a/;FieldQ[a]},_?FieldQ,\[Infinity]])==2&)]];
    lagtmp = DeleteCases[Expand[lagtmp],_?(Not[FreeQ[#,_?GhostFieldQ]]&)];
    lagtmp = DeleteCases[lagtmp,_?(If[Length[Cases[#,_?FieldQ]]==2,Not[Cases[#,_?FieldQ][[1]]===anti[Cases[#,_?FieldQ][[2]]]]]&)];
(*Print[InputForm[lagtmp]];Print["lagtmp at "<>ToString[SessionTime[]-mytime]];*)
    massspec=GetMassSpectrum[lagtmp];
    $Output={OutputStream["stdout",1]};
  Print["new mass spectrum with tadpole at "<>ToString[SessionTime[]-mytime]];
    replist=Flatten[(Dmshift/@massspec[[1,2;;,{1,2}]])];
Print[InputForm[replist]];Print[InputForm[massspec]];
    deltaLagp=Expand[(#/.replist&)/@deltaLagp];
    deltaLagt = (*Factor[*)Expand[deltaLagt+(deltaLagp/.FR$delta[__]->0)/.InternalParamList2](*]*);
  Print["new mass spectrum with tadpole at "<>ToString[SessionTime[]-mytime]];
  (*uses ee instead of alphaEWM1*)
    If[Not[FreeQ[InternalParamList2[[All,1]],ee]],
      deltaLagt=deltaLagt/.Solve[Cases[InternalParamList2/.Rule->tmpRule,tmpRule[ee,bb_]->ee==bb][[1]],{aEWM1}][[1]]/.{Sqrt[ee^2]->ee,Power[ee^2,Rational[n_Integer,2]]->ee^n}];
    deltaLagp = deltaLagp/.FR$deltat[_]->0;
  ];
];

Print["with the fields at "<>ToString[SessionTime[]-mytime]];
deltaLag=ComplexExpand[(deltaLag(*//.massRules*))/.{CC[x_][p__]:>CCtmp[x[p]]}/.FieldRenoList,cvar,TargetFunctions->{Conjugate}]/.CCtmp->CC;
Print["with the fields 2 at "<>ToString[SessionTime[]-mytime]];
deltaLag=Refine[deltaLag, Assumptions->{FR$CT>0}]/.Dot->RenDot/.RenDot->Dot/.Conjugate[FR$deltaZ[{x_,x_},y___]]->FR$deltaZ[{x,x},y];
Print["with the fields 3 at "<>ToString[SessionTime[]-mytime]];
(*Double needed for 1/2 and proj*)
deltaLag=deltaLag/.Dot->RenDot/.RenDot->Dot;
deltaLag=deltaLag/.{FR$CT^2->0,FR$CT^3->0,FR$CT^4->0};
deltaLag=Replace[deltaLag,Times[I*a_Plus]:>PlusI@@a,1];
Print["with the fields 4 at"<>ToString[SessionTime[]-mytime]];
If[FR$DoPara,
  tmp=Table[tmppara=deltaLag[[itp]];
     ParallelSubmit[{itp,tmppara},Normal[Series[tmppara,{FR$CT,0,1}]]],{itp,Length[deltaLag]}];
  deltaLag=Plus@@WaitAll[tmp];
  ,
  deltaLag = (Normal[Series[#,{FR$CT,0,1}]]&)/@deltaLag;
];
Print["with the fields 5 at"<>ToString[SessionTime[]-mytime]];


If[qcd(*||mixonly*),deltaLag=If[Head[deltaLag]===Plus,Expand/@deltaLag,Expand[deltaLag]],
  Print["tadpole shift at "<>ToString[SessionTime[]-mytime]];
(*3 because one field in the tadpole*)
  lagtmp=Total[Cases[Expand[deltaLagt],_?((Count[#//.{Dot->Times,Power[a_,b_Integer]:>Pow@@Table[a,{b}],del[a_,b_]:>Identity[a]},_?FieldQ,\[Infinity]])==3&)]];
(*no contribution to particle antiparticle kinetic term*)
  lagtmp = DeleteCases[lagtmp,_?(If[Length[Cases[#,_?FieldQ]]==2,Cases[#,_?FieldQ][[1]]===anti[Cases[#,_?FieldQ][[2]]],True]&)];
  If[Not[flm===True],lagtmp = DeleteCases[lagtmp,_?(FreeQ[Sort/@flm,Sort[Cases[#/.FR$deltat[x_]->1/.f_?AntiFieldQ->anti[f]/.del[a_,b_]->a,_?FieldQ,\[Infinity]]]]&)]];
Print["tadpole shift x at "<>ToString[SessionTime[]-mytime]];
  (*shift the wave function renormalization constant to absorb the tadpole contribution to the two points vertices*)
  lagtmp = Simplify[Expand[lagtmp//.(*If[mixonly,{},*)InternalParamList2(*]*)],TimeConstraint->1];
  $Output=logfile;
Print["tadpole shift a at "<>ToString[SessionTime[]-mytime]];
  frtmp=FeynmanRules[lagtmp];
Print["tadpole shift b at "<>ToString[SessionTime[]-mytime]];
  lagtmp2=DeleteCases[Expand[deltaLag],_?((Count[#//.{Dot->Times,FR$deltaZ[__]->1,Power[a_,b_Integer]:>Pow@@Table[a,{b}],del[a_,b_]:>Identity[a]},_?FieldQ,\[Infinity]])!=2&)];
  lagtmp2 = DeleteCases[lagtmp2,_?(If[Length[Cases[#,_?FieldQ]]==2,Cases[#,_?FieldQ][[1]]===anti[Cases[#,_?FieldQ][[2]]],True]&)];
  Print[InputForm[lagtmp2]];
  frtmp=(MomentumReplace[#,1]&)/@MergeVertices[frtmp,If[lagtmp2===0,{},FeynmanRules[lagtmp2,SelectParticles->frtmp[[All,1,All,1]]]]];
  $Output={OutputStream["stdout",1]};
Print["tadpole shift 2 at "<>ToString[SessionTime[]-mytime]];
  frtmp[[All,2]] = (Coefficient[#,FR$CT,1]&)/@frtmp[[All,2]];
Print["tadpole shift 3 at "<>ToString[SessionTime[]-mytime]];
  frtmp=Flatten[DZmixShift/@frtmp];
Print["tadpole shift 4 at "<>ToString[SessionTime[]-mytime]];
  deltaLag = Expand[deltaLag/.frtmp];
  deltaLagt = (*Factor[*)Expand[deltaLagt+(deltaLag/.FR$deltaZ[__]->0)/.InternalParamList2](*]*);
  deltaLagt = If[Head[deltaLagt]===Plus,(Coefficient[#,FR$CT,1]FR$CT&)/@deltaLagt,FR$CT*Coefficient[deltaLagt,FR$CT,1]];
  deltaLag = deltaLag/.FR$deltat[_]->0;
];
 Print["tadpole shift done at "<>ToString[SessionTime[]-mytime]];


On[Simplify::time];
Close[logfile];
DeleteFile[logfile[[1]]];

Return[Plus@@{deltaLag,lag4S,deltaLagp,deltaLagt}]

];
