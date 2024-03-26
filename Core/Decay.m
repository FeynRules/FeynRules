(* ::Package:: *)

(* ::Section:: *)
(*Useful functions: Getting masses, flavor indices, etc...*)


(* ::Subsection::Closed:: *)
(*ClassToLabel*)


(* ::Text:: *)
(*This functions map the particle names to the label of the class (e.g.:  A -> V[1])*)


ClassToLabel[partname_]:=Block[{plist=Cases[PartList[[All,1]],List[_,partname]]},
  If[plist=!={},Return[plist[[1,1]]],Return[MR$Null]]
];


(* ::Subsection::Closed:: *)
(*GetFlavorIndex*)


(* ::Text:: *)
(*This function returns the flavor index of a given class, if existing, otherwise it returns MR$Null*)


GetFlavorIndex[partname_]:=Block[{label=ClassToLabel[partname]},
  If[label=!=MR$Null,Return[FlavorIndex/.MR$ClassesRules[label]/.FlavorIndex->MR$Null],Return[MR$Null]];
];


(* ::Subsection::Closed:: *)
(*MemberToClass*)


(* ::Text:: *)
(*This function takes a class member as an argument and returns the corresponding classname. if the mapping does not work, MR$Null is returned.*)


MemberToClass[partname_?(AntiFieldQ[#]===True&)]:=(anti[MemberToClass[anti[partname]]]/.anti[MR$Null]:>MR$Null);


MemberToClass[partname_]:=Block[{list,result},
  list={ClassMembers,ClassName}/.MR$ClassesRules/@PartList[[All,1,1]];
  result=Select[list,MemberQ[#[[1]],partname]&];

  Return[If[result=!={},result[[1,2]],MR$Null]];
];


(* ::Subsection::Closed:: *)
(*MemberNumber*)


(* ::Text:: *)
(*This function takes a class member as an argument and returns the position in the list of the class members*)


MemberNumber[partname_?(AntiFieldQ[#]===True&)]:=MemberNumber[anti[partname]];


MemberNumber[partname_]:=Position[ClassMembers/.MR$ClassesRules[ClassToLabel[MemberToClass[partname]]],partname][[1,1]];


(* ::Subsection:: *)
(*GetMass*)


(* ::Text:: *)
(*This functions takes a particle as argument and return the corresponding mass. Works also with anti-fields and classmembers.*)


GetMass[{a_,_?IntegerQ}]:=GetMass[a];


Mass[CC[prt_]]:=Mass[prt];

PartNameMG[CC[a_]] := PartNameMG[MakeIdenticalFermions[a]];


GetMass[afi_?(AntiFieldQ[#]===True&)]:=GetMass[anti[afi]];


GetMass[partname_]:=Block[{label=ClassToLabel[partname],mass,flavo=GetFlavorIndex[partname]},
  If[label=!=MR$Null, 
  (* we have a generic particle *)
  (*mass= (Mass/.MR$ClassesRules[label]/.{0->{0}})[[1]];*)
  mass= Mass/.MR$ClassesRules[label]; If[Head[mass]===List, mass=mass[[1]]];
  If[flavo=!=MR$Null && mass=!=0,Return[mass[Index[flavo]]],Return[mass]],

  (* we have a class member *)
  Return[Mass[partname]];
  ];
];


(* ::Section::Closed:: *)
(*Computation and extraction of the relevant Feynman rules*)


(* ::Subsection::Closed:: *)
(*Removing vertices where we have three identical particles or massless particles*)


(* ::Text:: *)
(*This checks if a 3-point vertex contains three additional particles. If yes, the vertex is discarded (the function returns an empty list).*)


CheckParts[Prts__]:=Block[{tmpp=Prts[[All,1]]},
   (* Remove vertices with more than 3 particles *)
   If[Length[tmpp]=!=3, Return[False]];

   (* No Goldstone and ghosts *)
   If[Length[DeleteCases[tmpp,_?(GoldstoneQ[#]===True || GhostFieldQ[#]===True&)]]=!=3,Return[False]];

   (* Check if we have three identical particles *)
   If[MatchQ[Cases[Tally[tmpp/.fi_?(AntiFieldQ[#]===True&)->anti[fi]],{_,3}],{{_,3}}],Return[False]];  

   (* In the case we have a massless particle, the two others must be different *)
   tmpp=GetMass/@tmpp;
   If[MemberQ[tmpp,0.|0], If[Length[Tally[DeleteCases[tmpp,0.|0]]]===1 && Length[DeleteCases[tmpp,0.|0]]>1, Return[False]]];

   (* default *)
   Return[True];

];


MultiplicityTest[expr_]:=If[CheckParts[expr[[1]]],Return[expr],Return[{}]];


(* ::Subsection::Closed:: *)
(*Computing the relevant 3 point Feynman rules (unitarity gauge is used)*)


(* ::Text:: *)
(*This takes a single  Lagrangian as input*)


DecayFeynmanRules[lag_]:=Block[{TPrules,simplifiedlag},
  (* Switching on the unitarity gauge *)
  FeynmanGauge=False;
  simplifiedlag=lag//.{fi_?(GoldstoneQ[#]===True || GhostFieldQ[#]===True&)->0, fi_?(GhostFieldQ[#]===True&)[_]->0};

  (* Computing the Feynman rules *)
  TPrules=FeynmanRules[simplifiedlag,ScreenOutput->False,MaxParticles->3,FlavorExpand->True];
  TPrules=DeleteCases[MultiplicityTest/@TPrules,{}];

  (* Output *)
  Return[TPrules];
];


(* ::Section:: *)
(*Creation of the amplitude(s) associated to one vertex*)


(* ::Text:: *)
(*If we have fermions in the vertex, the three different combinations IOO are computed. Otherwise, only one amplitude is returned. Mass check is performed to compute the three combination only if necessary.*)


(* ::Subsection::Closed:: *)
(*Treatment of the gamma lines*)


TreatGLine[Spinor[ar__],Spinor[br__]]:=GLine[Spinor[ar],Spinor[br]];


TreatGLine[sp_,dirac__,sp2_]:=Block[{init,chain, factor,TDot,resu},
  (* Make a list from the chain *)
  init=If[Head[dirac]===Times,List@@dirac,{dirac}]/.TensDot->TDot;
  init=init/.TDot[br__][i1_,i2_]->br/.SlashedP[num_]:>Module[{mui},mui=Unique[mu];mySequence[FV[Ext[num],Index[Lorentz,mui]],Ga[Index[Lorentz,mui]]]]/.mySequence->Sequence;

  (* Select spin structures and non-spin structures*)
  chain= Cases[init,_?(MatchQ[#,(ProjM|ProjP|Ga|SlashedP)[ar__]]||MatchQ[#,SlashedP|ProjM|ProjP|Ga]&)];
  factor=Times@@Complement[init,chain];

  (* Merging  + treating the projectors *)
  resu = factor GLine[sp,Sequence@@chain,sp2]/.{
     GLine[hh___,ProjP[i1_,i2_],ggg___]->1/2GLine[hh,ggg]+1/2GLine[hh,Ga[5],ggg],
     GLine[hh___,ProjM[i1_,i2_],ggg___]->1/2GLine[hh,ggg]-1/2GLine[hh,Ga[5],ggg],
     GLine[hh___,ProjP,ggg___]->1/2GLine[hh,ggg]+1/2GLine[hh,Ga[5],ggg],
     GLine[hh___,ProjM,ggg___]->1/2GLine[hh,ggg]-1/2GLine[hh,Ga[5],ggg],
     GLine[hh___,SlashedP[i1_,i2_,i3_],ggg___]:>Module[{aa}, GLine[hh,Ga[Index[Lorentz,aa],i2,i3],ggg] FV[i1,Index[Lorentz,aa]]]};
  resu = resu/.Ga[num_,i1_,i2_]->Ga[num]/.IndexDelta[Index[Spin,_],_]->1;

  Return[resu];
]; 


(* ::Subsection::Closed:: *)
(*Core function*)


SPTimes[a_,b_]:={a*b};


Psigns={
  SlashedP[Ext[2]]->-SlashedP[Ext[2]], SlashedP[Ext[3]]->-SlashedP[Ext[3]], 
  FV[Ext[2],mu_]->-FV[Ext[2],mu], FV[Ext[3],mu_]->-FV[Ext[3],mu],
  FV[2,mu_]->-FV[Ext[2],mu], FV[3,mu_]->-FV[Ext[3],mu]};


CleanAmp[init_,final1_,final2_]:=Block[{},
  If[anti[final1]===init, Return[True]];
  If[anti[final2]===init, Return[True]];
  If[SelfConjugateQ[init]===True, Return[False]];
  If[(AntiFieldQ[init]===True) && (Head[init]=!=CC), Return[True]];
  If[(AntiFieldQ[init]=!=True) && (Head[init]===CC), Return[True]];
  Return[False];
];


CreateAmp[expr_]:=Block[{fields=expr[[1]]/.{fi_?(FieldQ[#]===True&),_?NumericQ}:>fi,Gamma=Expand[expr[[2]]],struc,wf,myidx,eext, masstest,dum, result,TheIndex},
  (* Ordering of the fermions, if relevant *)
  struc=If[Head[Gamma]===Plus,Gamma[[1]],Gamma];
  struc=GetFermionChain[struc]/._[___,Index[Spin,Ext[a_]],Index[Spin,Ext[b_]]]:>{Index[Spin,a],Index[Spin,b]};

  (* Treating scalar, vector and tensor wave-functions *)
  wf=fields/.{_?(ScalarFieldQ[#]===True&)->1,_?(ScalarFieldQ[#]===True&)[__]->1};  
  wf=wf//.{
    List[a_,fi_?(VectorFieldQ[#]===True&),b_]:>List[a,Polbar[Index[Lorentz,Ext[2]]],b],
    List[a__,fi_?(VectorFieldQ[#]===True&)]:>List[a,Polbar[Index[Lorentz,Ext[3]]]],
    List[fi_?(VectorFieldQ[#]===True&),b__]:>List[Pol[Index[Lorentz,Ext[1]]],b]};
  wf=wf//.{
    List[a_,fi_?(Spin2FieldQ[#]===True&),b_]:>List[a,TPolbar[Index[Lorentz,Ext[2,1]],Index[Lorentz,Ext[2,2]]],b],
    List[a__,fi_?(Spin2FieldQ[#]===True&)]:>List[a,TPolbar[Index[Lorentz,Ext[3,1]],Index[Lorentz,Ext[3,2]]]],
    List[fi_?(Spin2FieldQ[#]===True&),b__]:>List[TPol[Index[Lorentz,Ext[1,1]],Index[Lorentz,Ext[1,2]]],b]};

  (* Treating fermions (only 2 cases since 2 out of 3 are equal under the flip of the flow); tripling the amplitude in all cases (for different initial particle) *)
  If[struc=!=0,
    If[struc==={Index[Spin, 2], Index[Spin, 3]},
      wf=wf/.{
        List[a_,fi_?(Spin32FieldQ[#]===True&),b_]:>List[a,ToSpinor32[fi,Position[fields,fi][[1,1]],struc,Index[Lorentz,Ext[2]]],b],
        List[a__,fi_?(Spin32FieldQ[#]===True&)]:>List[a,ToSpinor32[fi,Position[fields,fi][[1,1]],struc,Index[Lorentz,Ext[3]]]],
        List[fi_?(Spin32FieldQ[#]===True&),b__]:>List[ToSpinor32[fi,Position[fields,fi][[1,1]],struc,Index[Lorentz,Ext[1]]],b]
      };
      TheIndex=1;wf=Spinorize[#,fields,struc,TheIndex++]&/@wf;
(*      wf=wf/.{fi_?(FermionQ[#]===True&):>ToSpinor[fi,Position[fields,fi][[1,1]],struc]};*)
      wf={wf/.{Spvbar->Spubar},wf,wf/.{Spvbar->Spubar,Spv->Spu}},
      wf=wf/.{
        List[a_,fi_?(Spin32FieldQ[#]===True&),b_]:>List[a,ToSpinor32[fi,Position[fields,fi][[1,1]],struc,Index[Lorentz,Ext[2]]] ,b],
        List[a__,fi_?(Spin32FieldQ[#]===True&)]:>List[a,ToSpinor32[fi,Position[fields,fi][[1,1]],struc,Index[Lorentz,Ext[3]]] ],
        List[fi_?(Spin32FieldQ[#]===True&),b__]:>List[ToSpinor32[fi,Position[fields,fi][[1,1]],struc,Index[Lorentz,Ext[1]]],b]
      };
      TheIndex=1;wf=Spinorize[#,fields,struc,TheIndex++]&/@wf;
(*      wf=wf/.{fi_?(FermionQ[#]===True&):>ToSpinor[fi,Position[fields,fi][[1,1]],struc]}; *)
      wf={wf,wf/.{Spvbar->Spubar,Spv->Spu},wf/.{Spvbar->Spubar}}      
    ],
    wf={wf,wf,wf}    
  ];
  fields={fields,{fields[[2]],fields[[1]],fields[[3]]},{fields[[3]],fields[[1]],fields[[2]]}};

  (*Merging the vertex and the wave functions *)
  wf=(GLine@@#)&/@wf//.{
    GLine[hh___,Polbar[bbb_],gg___]->GLine[hh,gg] Polbar[bbb],
    GLine[hh___,TPolbar[bbb__],gg___]->GLine[hh,gg] TPolbar[bbb],
    GLine[hh___,Polbar[bbb_] Spinor[ccc__],gg___]->GLine[hh,Spinor[ccc],gg] Polbar[bbb],
    GLine[hh___,Pol[bbb_],gg___]->GLine[hh,gg] Pol[bbb],
    GLine[hh___,TPol[bbb__],gg___]->GLine[hh,gg] TPol[bbb],
    GLine[hh___,Pol[bbb_] Spinor[ccc__],gg___]->GLine[hh, Spinor[ccc],gg] Pol[bbb],
    GLine[hh___,1,gg___]->GLine[hh,gg] ,GLine[]->1};
  wf=wf/.GLine[Spinor[ar__],Spinor[br__]]-> GLine[Spinor[ar],Gamma,Spinor[br]];
  wf=wf//.GLine[hh__,Plus[ar_,br__],ggg__]:>GLine[hh,Plus[br],ggg]+GLine[hh,ar,ggg];  

  wf=wf/.GLine->TreatGLine;
  wf=If[FreeQ[#,GLine],Expand[# Gamma],Expand[#]]&/@wf;
  wf = List/@wf;

  (* Treating the color for the vertices *)
  wf=wf/.IndexDelta[Index[type_?(#===Colour || #===Gluon&),i1_],Index[type_,i2_]]:> DeltaC[Index[type,i1],Index[type,i2]];

  (* Removing the cases of massless initial particle for the fermionic case *)  
  dum=If[GetMass[#[[1]]]===0||GetMass[#[[1]]]===0.,0,1]&/@fields;
  wf=Flatten[Inner[SPTimes,dum,wf,List],1]//.{
    FV[II_?(Head[#]=!=Ext&),etc__]->FV[Ext[II],etc],
    SP[II_?(Head[#]=!=Ext&),JJ_]:>SP[Ext[II],JJ],
    SP[II_,JJ_?(Head[#]=!=Ext&)]:>SP[II,Ext[JJ]]};

  (* Permutation of indices + signs for the outgoing momenta *)
  result={
    {fields[[1]],(wf[[1]]/.Psigns)},
    {fields[[2]],(wf[[2]]/.{IndexDelta->myidx,Ext[1,jj___]->eext[2,jj],Ext[2,jj___]->Ext[1,jj]}/.{eext->Ext,myidx->IndexDelta}/.Psigns)},
    {fields[[3]],(wf[[3]]/.{IndexDelta->myidx,Ext[1,jj___]->eext[2,jj],Ext[2,jj___]->eext[3,jj],Ext[3,jj___]->Ext[1,jj]}/.{eext->Ext,myidx->IndexDelta}/.Psigns)}};
  result=result/.{FV[Ext[II_],etc__]->FV[II,etc],SP[Ext[II_],Ext[JJ_]]->SP[II,JJ]};

  (* Removing anplitudes if the initial state is an antifield or if we have identical particles in the initial and final states *)
  If[CleanAmp[fields[[1,3]], fields[[1,1]],fields[[1,2]]], result=Drop[result,{3}]];
  If[CleanAmp[fields[[1,2]], fields[[1,3]],fields[[1,1]]], result=Drop[result,{2}]];
  If[CleanAmp[fields[[1,1]], fields[[1,3]],fields[[1,2]]], result=Drop[result,{1}]];
  result=DeleteCases[result,{_,{0}}];

  Return[result];
];


(* ::Subsection::Closed:: *)
(*Spinors, polarization vectors (u, v, ubar, vbar, epsilon, epsilonbar)*)


Spinor::usage="";
Spinor32::usage="";
Spu::usage="";
Spv::usage="";
Spubar::usage="";
Spvbar::usage="";


Pol::usage="";
Polbar::usage="";


TPol::usage="";
TPolbar::usage="";


(* ::Subsection::Closed:: *)
(*Constructing the fermion chains*)


(* ::Text:: *)
(*This function makes a field a spinor, if necessary*)


Spinorize[field_,Flist_,str_,index_]:=Block[{ThePos},
  If[Not[FermionQ[field]===True],Return[field]];
  ThePos=Position[Flist,field];
  If[Dimensions[ThePos]==={1,1},Return[ToSpinor[field,ThePos[[1,1]],str]],Return[ToSpinor[field,ThePos[[index,1]],str]]];
];


(* ::Text:: *)
(*This function only keeps objects acting in spin space*)


GetFermionChain[expr_]:=If[FreeQ[expr,Spin],0,expr//.Except[_[__,Index[Spin,_]]] a_:>a];


(* ::Text:: *)
(*This function extract the v, vbar,u and ubar spinors included in the diagram*)


ToSpinor[field_,position_,ordering_]:=If[Position[ordering,Index[Spin,position]]==={{1}},Spinor[Spvbar,Ext[position]],Spinor[Spv,Ext[position]]];


ToSpinor32[field_,position_,ordering_,loridx_]:=If[Position[ordering,Index[Spin,position]]==={{1}},
  Spinor[Spvbar,Ext[position],loridx],
  Spinor[Spv,Ext[position],loridx]
];


(* ::Section:: *)
(*Squaring the amplitudes and getting the decay width*)


(* ::Subsection:: *)
(*Reduction of the color trace*)


DeltaC::usage="";


Colorrules={
  DeltaC[Index[Colour,ii_],Index[Colour,ii_]]->3,
  DeltaC[Index[Gluon,ii_],Index[Gluon,ii_]]->8,
  DeltaC[Index[type_,ii_],Index[type_,jj_]]DeltaC[Index[type_,jj_],Index[type_,kkk_]]->DeltaC[Index[type,ii],Index[type,kkk]], 
  DeltaC[Index[type_,ii_],Index[type_,jj_]]DeltaC[Index[type_,jj_],Index[type_,ii_]]->DeltaC[Index[type,ii],Index[type,ii]], 
  T[aa_,ii_,ii_]->0,
  T[Index[Gluon,aa_],ll_,mm_] T[Index[Gluon,bb_],mm_,ll_]->1/2 DeltaC[Index[Gluon,aa],Index[Gluon,bb]],
  dSUN[a_,b_,c_]^2->40/3
};


(* ::Subsection:: *)
(*Extracting Lorentz invariants*)


ETA::usage="";
eext::usage="";


Index[Lorentz,Index[Lorentz,id__]]:=Index[Lorentz,id];


Lorentzrules={
  FV[ki_,kmu_] FV[kj_,kmu_]  :>Dot[FV[ki],FV[kj]],
  FV[ki_,kmu_] ME [kmu_,knu_] -> FV[ki,knu],
  FV[ki_,kmu_] ME [knu_,kmu_] -> FV[ki,knu],  
  ME[ka_,kb_] ME[kb_,kc_]->ME[ka,kc],
  ME[ka_,kb_] ME[kc_,kb_]->ME[ka,kc],
  FV[idx_, kmu_]^2 :> Dot[FV[idx],FV[idx]],
  ME[Ext[ii__],eext[ii__]]->4
};


(* ::Subsection:: *)
(*Tracing over the polarization vectors*)


PolRules[expr_,masses_]:= Return[expr//.{
   Polbar[Index[Lorentz,Ext[i1_,jj___]]] Pol[Index[Lorentz,eext[i1_,jj___]]]:> If[masses[[i1]]===0,
     -ME[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,eext[i1,jj]]]+
       If[FR$Spin2Tag,(ETA[Ext[i1],Index[Lorentz,Ext[i1]]] FV[Ext[i1],Index[Lorentz,eext[i1]]]+ETA[Ext[i1],Index[Lorentz,eext[i1]]]FV[Ext[i1],Index[Lorentz,Ext[i1]]])/(Dot[ETA[Ext[i1]],FV[Ext[i1]]]),0],
     -ME[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,eext[i1,jj]]]+ FV[Ext[i1],Index[Lorentz,eext[i1,jj]] ]FV[Ext[i1],Index[Lorentz,Ext[i1,jj]] ]/masses[[i1]]^2
   ],
   Pol[Index[Lorentz,Ext[i1_,jj___]]] Polbar[Index[Lorentz,eext[i1_,jj___]]]:> If[masses[[i1]]===0,
     -ME[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,eext[i1,jj]]]+
       If[FR$Spin2Tag,(ETA[Ext[i1],Index[Lorentz,Ext[i1]]] FV[Ext[i1],Index[Lorentz,eext[i1]] ]+ETA[Ext[i1],Index[Lorentz,eext[i1]]]FV[Ext[i1],Index[Lorentz,Ext[i1]]])/(Dot[ETA[Ext[i1]],FV[Ext[i1]]]),0],
     -ME[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,eext[i1,jj]]]+ FV[Ext[i1],Index[Lorentz,eext[i1,jj]] ]FV[Ext[i1],Index[Lorentz,Ext[i1,jj]] ]/masses[[i1]]^2
   ], 
	 	   ETA[Ext[1],indx___]:>FV[Ext[2],indx],ETA[Ext[2],indx___]:>FV[Ext[1],indx], ETA[Ext[3],indx___]:>FV[Ext[1],indx],
   TPol[Index[Lorentz,Ext[i1_,jj_]],Index[Lorentz,Ext[i1_,kk_]]] TPolbar[Index[Lorentz,eext[i1_,jj_]],Index[Lorentz,eext[i1_,kk_]]] :> 
     1/2*(
       DPP[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,eext[i1,jj]]] DPP[Index[Lorentz,Ext[i1,kk]],Index[Lorentz,eext[i1,kk]]] +
       DPP[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,eext[i1,kk]]] DPP[Index[Lorentz,Ext[i1,kk]],Index[Lorentz,eext[i1,jj]]]) - 
     1/3*DPP[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,Ext[i1,kk]]] DPP[Index[Lorentz,eext[i1,jj]],Index[Lorentz,eext[i1,kk]]],
   TPolbar[Index[Lorentz,Ext[i1_,jj_]],Index[Lorentz,Ext[i1_,kk_]]] TPol[Index[Lorentz,eext[i1_,jj_]],Index[Lorentz,eext[i1_,kk_]]] :> 
     1/2*(
       DPP[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,eext[i1,jj]]] DPP[Index[Lorentz,Ext[i1,kk]],Index[Lorentz,eext[i1,kk]]] +
       DPP[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,eext[i1,kk]]] DPP[Index[Lorentz,Ext[i1,kk]],Index[Lorentz,eext[i1,jj]]]) - 
     1/3*DPP[Index[Lorentz,Ext[i1,jj]],Index[Lorentz,Ext[i1,kk]]] DPP[Index[Lorentz,eext[i1,jj]],Index[Lorentz,eext[i1,kk]]],
  DPP[Index[Lorentz,HH1_[i1_,jj_]],Index[Lorentz,HH2_[i1_,kk_]]] :> If[masses[[i1]]===0,
    -ME[Index[Lorentz,HH1[i1,jj]],Index[Lorentz,HH2[i1,kk]]],
    -ME[Index[Lorentz,HH1[i1,jj]],Index[Lorentz,HH2[i1,kk]]] + 1/masses[[i1]]^2 FV[Ext[i1],Index[Lorentz,HH1[i1,jj]]]  FV[Ext[i1],Index[Lorentz,HH2[i1,kk]]]]
}];


(* ::Subsection::Closed:: *)
(*Reduction of the Dirac trace*)


FR$DiracTraceDictionnary={GLine[]->4,GLine[_]->0, GLine[_,_,_]->0, GLine[Ga[5],Ga[5]]->4,GLine[Ga[5],_]->0, GLine[_,Ga[5]]->0,
  GLine[Ga[mu1_],Ga[mu2_]]->4 ME[mu1,mu2]};


SolveGammaTrace[expr_]:=Block[{tr,list, len,length,G5,gmunu,mySolveGammaTrace,resu,nu1,nu2,nu3},
  (* checks if the trace has already been computed *)
  list=expr/.Dispatch[FR$DiracTraceDictionnary];
  If[Head[list]=!=GLine,Return[list]];
  list=List@@expr;

  (* initialization *)
  len=Plus@@(list/.Ga[5]->4/.Ga[_]->1);
  length=Length[list];
  list=tr@@(list/.Ga[5]->G5);

  (* Odd number of gamma matrices *)
  If[OddQ[len]||length===1||length===3,MakeDiracRule[expr,0]; Return[0]];

  (* Even number of gamma matrices *)
  If[len===0,Return[4]];
  If[length===2,resu=list/.{tr[G5,G5]->4, tr[Ga[mu1_],Ga[mu2_]]->4 ME[mu1,mu2]}; MakeDiracRule[expr,Expand[resu]];Return[resu]];

  (* Treats Gamma5's *)
  list=list//.{tr[a___,G5,b__]:>tr[b,a,G5]/;!MemberQ[{a,b},G5],tr[a___,G5,G5,b___]:>SolveGammaTrace[(GLine[a,b]/.G5->Ga[5])],tr[a___,G5,b_,c___]:>-tr[a,b,G5,c]};

  (* 4 gamma mu + one gamma5 *)
  list=list//.tr[Ga[mu1_],Ga[mu2_],Ga[mu3_],Ga[mu4_],G5]:> 4I Eps[mu1,mu2,mu3,mu4];

  (* Only Gamma[mu] *)
  If[FreeQ[list,G5] && Head[list]===tr,
    list=Plus@@Table[(-1)^(ii+1)(gmunu[list[[1]],list[[ii+1]]]/.Ga[mu_]->mu/.gmunu->ME)SolveGammaTrace[GLine@@Drop[list,{1,ii+1,ii}]],{ii,Length[list]-1}]];
  (* lots of Gamma[mu] + one gamma5 at the last spot *)
  If[Head[list]===tr,
    list=Expand[Plus@@Table[(-1)^(ii+1)(gmunu[list[[1]],list[[ii+1]]]/.Ga[mu_]->mu/.gmunu->ME) SolveGammaTrace[GLine@@Drop[list/.G5->Ga[5],{1,ii+1,ii}]] ,{ii,Length[list]-2}]-
      I/6 Eps[(list[[1]]/.Ga[mu_]->mu),Index[Lorentz,nu1],Index[Lorentz,nu2],Index[Lorentz,nu3]]*
      SolveGammaTrace[GLine@@Join[List@@Most[Rest[list]],{Ga[Index[Lorentz,nu1]],Ga[Index[Lorentz,nu2]],Ga[Index[Lorentz,nu3]]}]]]
  ];

  (* Output *)
  MakeDiracRule[expr,list]; Return[list];
];


MakeDiracRule[exp_,rul_]:=Block[{tmp,idx,myPattern,myRule},
  idx=DeleteCases[List@@exp/.Ga[Index[Lorentz,x_]]->x,Ga[5]];
  idx=Rule[#,Unique[mu]]&/@DeleteDuplicates[idx];
  tmp=myRule[exp/.idx/.Ga[Index[Lorentz,x_]]->Ga[Index[Lorentz,myPattern[x,Blank[]]]],rul/.idx];
  FR$DiracTraceDictionnary=Join[FR$DiracTraceDictionnary,{tmp/.myPattern->Pattern/.myRule->Rule}];
  Return[];
];


(* ::Subsection::Closed:: *)
(*Conjugating a diagram*)


Conj[Times[a_, b__]] := Conj[a] Conj[Times[b]];
Conj[Plus[a_, b__]] := Conj[a] + Conj[Plus[b]];
Conj[Spvbar] = Spv;
Conj[Spubar] = Spu;
Conj[Pol[a__]] = Polbar[a];
Conj[Polbar[a__]] = Pol[a];
Conj[TPol[a__]] = TPolbar[a];
Conj[TPolbar[a__]] = TPol[a];
Conj[Spv] = Spvbar; Conj[Spu] = Spubar;
Conj[GLine[Spinor[ty1_,Ext[i1_],lor1___],ar___,Spinor[ty2_,Ext[i2_],lor2___]]]:=GLine[Spinor[Conj[ty2],Ext[i2],lor2], Sequence@@Reverse[{ar}], Spinor[Conj[ty1],Ext[i1],lor1]];
Conj[DeltaC[args__]] := DeltaC[Sequence @@ Reverse[{args}]];
Conj[a_?(MemberQ[FR$GrpMat, #] &)[aa_, inds__]] := a[aa, Sequence @@ Reverse[{inds}]];


ConjugateAmp[amp_]:=Conj[amp]/.Conj->Conjugate/.Ga[5]->-Ga[5]//.GLine[ar__,-Ga[5],br__]->-GLine[ar,Ga[5],br];


(* ::Subsection:: *)
(*Core function*)


SquareAmp[amps_]:=Squaring[#[[1]],#[[2,1]]]&/@amps;


(* ::Text:: *)
(*This is the main function, piloting the decay*)


Squaring[{fields__},expr_]:=Block[{Amp,CAmp,ECAmp,M2,masses,gammalist,CArule={}},
  (* Masses and fields *)
  masses=((GetMass[#]/.Index[type_]:>Index[type,Ext[Position[{fields},#][[1,1]]]])&/@{fields});
  
  (* Conjugating the gamma lines *)
  CAmp=ConjugateAmp[expr]/.Index[Lorentz,Ext[i1__]]->Index[Lorentz,eext[i1]]/.IndexDelta->FR$myidx/.Index[Lorentz, sy_, 1]->Index[Lorentz,sy,2];
  ECAmp=Expand[CAmp];

  Block[{muf,newrules},
    muf=Cases[Tally[Cases[#,Index[Lorentz,_],Infinity]],{_,2}]; 
    muf=DeleteCases[muf,{Index[Lorentz,eext[__]],2}];
    newrules=If[muf=!={},MyRule[#[[1]],Index[Lorentz,Unique[mu]]]&/@muf]/.MyRule->Rule;
    If[newrules=!=Null,CArule=Join[CArule,newrules]];
    Return[];
  ]&/@If[Head[ECAmp]===Plus,ECAmp,{ECAmp}];
  CAmp=CAmp/.CArule;
  Amp=expr/.IndexDelta->FR$myidx;

  (* Squaring *)
  M2=Squarify[Amp,CAmp,masses];

  (* Solving the Dirac traces, the polarization vectors, the color traces and the lorentz structure *)
  M2 = M2//.Colorrules;
  M2=If[Head[M2]===Plus,List@@M2,{M2}];  
  M2=(#/.GLine[argus___]:>SolveGammaTrace[GLine[argus]])&/@M2; 
  M2=Plus@@(Expand[PolRules[#,masses]]&/@M2);

  (* Speed optimization required *)
   M2=If[Head[M2]===Plus,List@@M2,{M2}];  
  (* CD: Added rule for square of Levi Civita *)
   M2=Expand[M2]/. Eps[i1_,i2_,i3_,i4_]Eps[j1_,j2_,j3_,j4_] :> -(ME[i1, j4]*ME[i2, j3]*ME[i3, j2]*ME[i4, j1] - 
     ME[i1, j3]*ME[i2, j4]*ME[i3, j2]*ME[i4, j1] - 
     ME[i1, j4]*ME[i2, j2]*ME[i3, j3]*ME[i4, j1] + 
     ME[i1, j2]*ME[i2, j4]*ME[i3, j3]*ME[i4, j1] + 
     ME[i1, j3]*ME[i2, j2]*ME[i3, j4]*ME[i4, j1] - 
     ME[i1, j2]*ME[i2, j3]*ME[i3, j4]*ME[i4, j1] - 
     ME[i1, j4]*ME[i2, j3]*ME[i3, j1]*ME[i4, j2] + 
     ME[i1, j3]*ME[i2, j4]*ME[i3, j1]*ME[i4, j2] + 
     ME[i1, j4]*ME[i2, j1]*ME[i3, j3]*ME[i4, j2] - 
     ME[i1, j1]*ME[i2, j4]*ME[i3, j3]*ME[i4, j2] - 
     ME[i1, j3]*ME[i2, j1]*ME[i3, j4]*ME[i4, j2] + 
     ME[i1, j1]*ME[i2, j3]*ME[i3, j4]*ME[i4, j2] + 
     ME[i1, j4]*ME[i2, j2]*ME[i3, j1]*ME[i4, j3] - 
     ME[i1, j2]*ME[i2, j4]*ME[i3, j1]*ME[i4, j3] - 
     ME[i1, j4]*ME[i2, j1]*ME[i3, j2]*ME[i4, j3] + 
     ME[i1, j1]*ME[i2, j4]*ME[i3, j2]*ME[i4, j3] + 
     ME[i1, j2]*ME[i2, j1]*ME[i3, j4]*ME[i4, j3] - 
     ME[i1, j1]*ME[i2, j2]*ME[i3, j4]*ME[i4, j3] - 
     ME[i1, j3]*ME[i2, j2]*ME[i3, j1]*ME[i4, j4] + 
     ME[i1, j2]*ME[i2, j3]*ME[i3, j1]*ME[i4, j4] + 
     ME[i1, j3]*ME[i2, j1]*ME[i3, j2]*ME[i4, j4] - 
     ME[i1, j1]*ME[i2, j3]*ME[i3, j2]*ME[i4, j4] - 
     ME[i1, j2]*ME[i2, j1]*ME[i3, j3]*ME[i4, j4] + 
     ME[i1, j1]*ME[i2, j2]*ME[i3, j3]*ME[i4, j4]);
   M2=Plus@@(Expand[#] //.Lorentzrules &/@M2);

  (* Removing remaining epsilons *)
  M2 = M2/.{FV[Ext[i_],muf_]->FV[i,muf],FV[eext[i_],muf_]->FV[i,muf]}/.Eps[aa1___,b1_,aa2___,b2_,aa3___] FV[i_,b1_] FV[i_,b2_]->0;

  (* Replacing products of four-vector *)
  M2 = M2//.{
    Dot[FV[Ext[i_]],FV[Ext[j_]]]:>SP[Ext[i],Ext[j]],
    Dot[FV[i_?(Head[#]=!=Ext&)],FV[j_?(Head[#]=!=Ext&)]]:>SP[Ext[i],Ext[j]],
    SP[i_?(Head[#]=!=Ext&),j_]:>SP[Ext[i],j],
    SP[j_,i_?(Head[#]=!=Ext&)]:>SP[j,Ext[i]],
    SP[Ext[i_],Ext[i_]]:>masses[[i]]^2,
    SP[Ext[1],Ext[2]]:>-1/2(masses[[3]]^2-masses[[1]]^2-masses[[2]]^2),
    SP[Ext[2],Ext[3]]:>1/2(masses[[1]]^2-masses[[2]]^2-masses[[3]]^2),
    SP[Ext[3],Ext[1]]:>-1/2(masses[[2]]^2-masses[[3]]^2-masses[[1]]^2)
  };

  (* results *)
   Return[{{fields},Expand[M2]}];
];


(* ::Text:: *)
(*This function merges the amplitude and the conjugate amplitude and pre-process them so that the squared matrix element is ready to be simplified (dirac traces, ...). Simple Dirac traces are done on the fly.*)


Squarify[Amp_,CAmp_,masses_]:=Block[{msq},
  (* Merging the amplitude and the conjugate amplitude *)
  msq=Expand[Amp CAmp]//.{
    FR$myidx[Index[type1_,a_],Index[type1_,b_]] FR$myidx[Index[type1_,b_],Index[type1_,a_]]:>IndexDelta[Index[type1,a],Index[type1,b]],
    Power[FR$myidx[Index[type1_,a_],Index[type1_,b_]],2]:>IndexDelta[Index[type1,a],Index[type1,b]]};

  msq= msq/.{
   GLine[args1__,Spinor[Spv,Ext[i1_]]]GLine[Spinor[Spvbar,Ext[i1_]],args2__]:>GLine[SlashedP[i1],args2,args1]-masses[[i1]] GLine[args2,args1],
   GLine[args1__,Spinor[Spu,Ext[i1_]]]GLine[Spinor[Spubar,Ext[i1_]],args2__]:>GLine[SlashedP[i1],args2,args1]+masses[[i1]] GLine[args2,args1]
  };

  msq= msq/.{
   GLine[args1__,Spinor[Spv,Ext[i1_],a_]]GLine[Spinor[Spvbar,Ext[i1_],b_],args2__]:>If[masses[[i1]]=!=0,
      (-ME[b,a]+2/3 FV[Ext[i1],b] FV[Ext[i1],a]/masses[[i1]]^2)*(GLine[SlashedP[i1],args2,args1]-masses[[i1]] GLine[args2,args1])+
      1/3*(GLine[SlashedP[i1],Ga[a],Ga[b],args2,args1]-masses[[i1]] GLine[Ga[a],Ga[b],args2,args1])-
      1/(3 masses[[i1]])*FV[Ext[i1],b]*(GLine[SlashedP[i1],Ga[a],args2,args1]-masses[[i1]] GLine[Ga[a],args2,args1])+
      1/(3 masses[[i1]])*FV[Ext[i1],a]*(GLine[SlashedP[i1],Ga[b],args2,args1]-masses[[i1]] GLine[Ga[b],args2,args1])],
   GLine[args1__,Spinor[Spu,Ext[i1_],a_]]GLine[Spinor[Spubar,Ext[i1_],b_],args2__]:>If[masses[[i1]]=!=0,
      (-ME[b,a]+2/3 FV[Ext[i1],b] FV[Ext[i1],a]/masses[[i1]]^2)*(GLine[SlashedP[i1],args2,args1]+masses[[i1]] GLine[args2,args1])+
      1/3*(GLine[SlashedP[i1],Ga[a],Ga[b],args2,args1]+masses[[i1]] GLine[args1,Ga[a],Ga[b],args2,args1])+
      1/(3 masses[[i1]])*FV[Ext[i1],b]*(GLine[SlashedP[i1],Ga[a],args2,args1]+masses[[i1]] GLine[Ga[a],args2,args1])-
      1/(3 masses[[i1]])*FV[Ext[i1],a]*(GLine[SlashedP[i1],Ga[b],args2,args1]+masses[[i1]] GLine[Ga[b],args2,args1])]
  };

  msq= msq/.{
    GLine[args1___,Spinor[Spv,Ext[i1_]],Spinor[Spvbar,Ext[i1_]],args2___]:> GLine[args1,SlashedP[i1],args2]-masses[[i1]] GLine[args1,args2],
    GLine[args1___,Spinor[Spu,Ext[i1_]],Spinor[Spubar,Ext[i1_]],args2___]:> GLine[args1,SlashedP[i1],args2]+masses[[i1]] GLine[args1,args2],
    GLine[args1___,Spinor[Spvbar,Ext[i1_]],Spinor[Spv,Ext[i1_]],args2___]:> GLine[args1,SlashedP[i1],args2]-masses[[i1]] GLine[args1,args2],
    GLine[args1___,Spinor[Spubar,Ext[i1_]],Spinor[Spu,Ext[i1_]],args2___]:> GLine[args1,SlashedP[i1],args2]+masses[[i1]] GLine[args1,args2]
  };

  msq = Expand[msq/.{
    GLine[args1___,Spinor[Spv,Ext[i1_],a_],Spinor[Spvbar,Ext[i1_],b_],args2___]:> If[masses[[i1]]=!=0,
      (-ME[b,a]+2/3 FV[Ext[i1],b] FV[Ext[i1],a]/masses[[i1]]^2)*(GLine[args1,SlashedP[i1],args2]-masses[[i1]] GLine[args1,args2])+
      1/3*(GLine[args1,SlashedP[i1],Ga[a],Ga[b],args2]-masses[[i1]] GLine[args1,Ga[a],Ga[b],args2])-
      1/(3 masses[[i1]])*FV[Ext[i1],b]*(GLine[args1,SlashedP[i1],Ga[a],args2]-masses[[i1]] GLine[args1,Ga[a],args2])+
      1/(3 masses[[i1]])*FV[Ext[i1],a]*(GLine[args1,SlashedP[i1],Ga[b],args2]-masses[[i1]] GLine[args1,Ga[b],args2])],
    GLine[args1___,Spinor[Spu,Ext[i1_],a_],Spinor[Spubar,Ext[i1_],b_],args2___]:>  If[masses[[i1]]=!=0,
      (-ME[b,a]+2/3 FV[Ext[i1],b] FV[Ext[i1],a]/masses[[i1]]^2)*(GLine[args1,SlashedP[i1],args2]+masses[[i1]] GLine[args1,args2])+
      1/3*(GLine[args1,SlashedP[i1],Ga[a],Ga[b],args2]+masses[[i1]] GLine[args1,Ga[a],Ga[b],args2])+
      1/(3 masses[[i1]])*FV[Ext[i1],b]*(GLine[args1,SlashedP[i1],Ga[a],args2]+masses[[i1]] GLine[args1,Ga[a],args2])-
      1/(3 masses[[i1]])*FV[Ext[i1],a]*(GLine[args1,SlashedP[i1],Ga[b],args2]+masses[[i1]] GLine[args1,Ga[b],args2])],
    GLine[args1___,Spinor[Spvbar,Ext[i1_],b_],Spinor[Spv,Ext[i1_],a_],args2___]:> If[masses[[i1]]=!=0,
      (-ME[b,a]+2/3 FV[Ext[i1],b] FV[Ext[i1],a]/masses[[i1]]^2)*(GLine[args1,SlashedP[i1],args2]-masses[[i1]] GLine[args1,args2])+
      1/3*(GLine[args1,SlashedP[i1],Ga[a],Ga[b],args2]-masses[[i1]] GLine[args1,Ga[a],Ga[b],args2])-
      1/(3 masses[[i1]])*FV[Ext[i1],b]*(GLine[args1,SlashedP[i1],Ga[a],args2]-masses[[i1]] GLine[args1,Ga[a],args2])+
      1/(3 masses[[i1]])*FV[Ext[i1],a]*(GLine[args1,SlashedP[i1],Ga[b],args2]-masses[[i1]] GLine[args1,Ga[b],args2])],
    GLine[args1___,Spinor[Spubar,Ext[i1_],b_],Spinor[Spu,Ext[i1_],a_],args2___]:> If[masses[[i1]]=!=0,
      (-ME[b,a]+2/3 FV[Ext[i1],b] FV[Ext[i1],a]/masses[[i1]]^2)*(GLine[args1,SlashedP[i1],args2]+masses[[i1]] GLine[args1,args2])+
      1/3*(GLine[args1,SlashedP[i1],Ga[a],Ga[b],args2]+masses[[i1]] GLine[args1,Ga[a],Ga[b],args2])+
      1/(3 masses[[i1]])*FV[Ext[i1],b]*(GLine[args1,SlashedP[i1],Ga[a],args2]+masses[[i1]] GLine[args1,Ga[a],args2])-
      1/(3 masses[[i1]])*FV[Ext[i1],a]*(GLine[args1,SlashedP[i1],Ga[b],args2]+masses[[i1]] GLine[args1,Ga[b],args2])]
  }];

  (* Fast simplifications which gives simple results -> huge speed up of the calculations *)
  msq = msq//.GLine[hh___,SlashedP[i2_],ggg___]:> Module[{mu},GLine[hh,Ga[Index[Lorentz,mu]],ggg] FV[Ext[i2],Index[Lorentz,mu]]];
  msq=msq /.FR$DiracTraceDictionnary;

  (* Output *)
  Return[msq];
];


(* ::Section:: *)
(*Computing the squared matrix element relevant for the decay width*)


(* ::Subsection::Closed:: *)
(*Initialization*)


InitDecay[]:=Block[{},
  FR$GrpMat=Flatten[DeleteCases[(Representations/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList,Representations],1][[All,1]];
  FR$Spin2Tag = MemberQ[Head/@MR$ClassesList,T];
];


(* ::Subsection:: *)
(*Core*)


Options[CalculateM2Decays]={
  ExtractFeynmanRules->False
};


CalculateM2Decays[InLag__,OptionsPattern[]]:=Block[{Frules,Mat,Mattmp,tot,lag={InLag}, MakeDecaysOut,tmpr},  
  (* Feynman rules *)
  If[OptionValue[ExtractFeynmanRules]===True,Print[Style["Computing the three-point vertices relevant for the 1->2 decays...",Orange,Bold]]];
  Frules=If[OptionValue[ExtractFeynmanRules], DecayFeynmanRules/@lag,lag]; 
  Frules=If[Length[lag]=!=1,MergeVertices[Sequence@@Frules],Frules[[1]]];
  If[Not[OptionValue[ExtractFeynmanRules]],Frules=DeleteCases[MultiplicityTest[#]&/@Frules,{}]];
  tot=Length[Frules];

 (* Computation of the matrix elements *)
  FR$DecayCounter=0; 
  Print[Style["Computing the squared matrix elements relevant for the 1->2 decays: ",Orange,Bold]];
  InitDecay[];

  If[Global`FR$Parallelize && tot>40,
(*Parallellization*)
    DistributeDefinitions[FR$GrpMat,FR$Spin2Tag];SetSharedVariable[FR$DiracTraceDictionnary];
    Mattmp=ParallelizeMe[Composition[SquareAmp,CreateAmp],Frules,Counter->True];
    Mat=Union[DeleteCases[Flatten[Mattmp,1],{_,0}]],
(*No parallelization*)
    Print[Dynamic[FR$DecayCounter],  " / ", tot];
    Mat=Block[{},FR$DecayCounter++; SquareAmp[CreateAmp[#]]]&/@Frules;
    Mat = DeleteCases[Flatten[Mat,1],{_,0}];
  ];

  (* Output: make the last two particles outgoing *)
  MakeDecaysOut[{{x_, p1_, p2_}, exp_}] := {{x, anti[MakeIdenticalFermions[p1]], anti[MakeIdenticalFermions[p2]]}, exp};
  Mat = MakeDecaysOut /@ Mat;
  Return[Mat];
];


(* ::Section:: *)
(*Phase space*)


Kallen[a_,b_,c_]:=a^2+b^2+c^2-2 a b - 2 a c - 2 b c;


DPS2[masses_]:=Block[{massterm},

   (* Checking the masses *)
   If[masses[[1]]===0,Return[0]];

   massterm = Which[masses[[2]] === 0, masses[[1]]^2 - masses[[3]]^2,
                    masses[[3]] === 0, masses[[1]]^2 - masses[[2]]^2,
                    True, Sqrt[Kallen[Sequence@@(#^2&/@masses)]]
              ];

   Return[massterm/(16 Pi Abs[masses[[1]]]^3)];
];


(* ::Section:: *)
(*Get a particular width for the process p1 > p2 p3 *)


(* ::Subsection:: *)
(*Core function: compute all decays*)


Options[ComputeDecays]={
  Simplify->True,
  ProgressIndicator -> True,
  Save -> True
};


ComputeDecays[Msq_List,OptionsPattern[]]:=Block[{tmpmsq=Msq,decays,decaystmp,AllDecays,tot=Length[Msq],progressindicatorQ=OptionValue[ProgressIndicator],SimQ=OptionValue[Simplify]},
  (* Initialization *)
  FR$DecayCntb=0;
  
  
 (* From matrix elements to widths *)
  If[Global`FR$Parallelize && tot>40,
    If[progressindicatorQ, Print[Style["Computing all the partial 1->2 decay widths: ",Orange,Bold]] ];
    AllDecays=Union[myresults=ParallelizeMe[ComputeDecay,Msq[[All,1]],MyOptions->{Msq,Simplify->SimQ},Counter->True]],
    If[progressindicatorQ, Print[Style["Computing all the partial 1->2 decay widths: ",Orange,Bold], Dynamic[FR$DecayCntb],  " / ", tot]];
    AllDecays=Block[{},FR$DecayCntb++; ComputeDecay[#,Msq,Simplify->SimQ]]&/@tmpmsq[[All,1]]
  ];
 
  AllDecays = DeleteCases[AllDecays,0];
  AllDecays = MapAt[Sequence @@ #&, #, 2]& /@ AllDecays;

  (* Caching and output *)
  If[OptionValue[Save],
     If[FR$PartialWidths =!= {}, 
        Message[Decay::Overwrite]
       ];
     FR$PartialWidths = AllDecays;
     ];

  Return[AllDecays];
];


(* ::Subsection::Closed:: *)
(*Match the ordering (p1,p2,p3) to the right ordering used for the computation of the squared matrix elements*)


FiPermute[fields_List,filist_List]:=Block[{fi,eext,goodvertex,num, rul},
  (* Flipping rules *)
  rul = { {}, List[Ext[2]->eext[3],Ext[3]->Ext[2]] };

  (* Two possible permutations for the final state *)
  fi= {fields,{fields[[1]],fields[[3]],fields[[2]]}/.rul[[2]]/.eext->Ext};

  goodvertex=Position[filist,#]&/@fi;
  num= Position[goodvertex,{_Integer}];
  num=If[num==={},0,num[[1]]/.{a_,1}->a];
  goodvertex=If[num===0,num=1;0,(DeleteCases[goodvertex,{}]/.{}->0)[[1,1]]];
  Return[{goodvertex,rul[[num]]}];
];


(* ::Subsection:: *)
(*Compute one single decay*)


Options[ComputeDecay]={
  Simplify->True
};


ComputeDecay[lfields_List, Msq_List,OptionsPattern[]]:=Block[{M2,num,permrules,dps2,masses, lfi1,lfi2,lfi3,avg,width},
  (* fields *)
  lfi1=lfields[[1]]; lfi2=lfields[[2]]; lfi3=lfields[[3]];

  (* Fields and masses: compute all the permutations *)
  {num,permrules}=FiPermute[lfields,Msq[[All,1]]]; 
  If[num===0, Return[0]];
  M2=Msq[[num,2]];

  (* masses *)
  masses=((GetMass[#]/.Index[type_]:>Index[type,Ext[Position[lfields,#][[1,1]]]])&/@lfields);
  If[MemberQ[{masses[[2]],masses[[3]]},masses[[1]]], Return[0]];

  (* DPS2 *)
  dps2=DPS2[masses]/.permrules;

  (* Averaging over the initial state *)
  avg=If[FermionQ[lfi1]===True,1/2,1];
  If[Not[FreeQ[$IndList[lfi1],Colour]],avg=avg/3];
  If[Not[FreeQ[$IndList[lfi1],Gluon]],avg=avg/8];
  If[VectorFieldQ[lfi1]===True && masses[[1]]=!=0, avg=avg/3];
  If[VectorFieldQ[lfi1]===True && masses[[1]]===0, avg=avg/2];
  If[Spin2FieldQ[lfi1]===True && masses[[1]]=!=0, avg=avg/5];
  If[Spin2FieldQ[lfi1]===True && masses[[1]]===0, avg=avg/2];
  If[Spin32FieldQ[lfi1]===True && masses[[1]]=!=0, avg=avg/2];

  (* Identical final state particles *)
  If[lfi2===lfi3,avg=avg/2];

  (* output *)
   width=If[OptionValue[Simplify]===True,Simplify[M2 dps2 avg],M2 dps2 avg];
   lfi1=MakeIdenticalFermions[lfi1];
   Return[{{lfi1,lfi2,lfi3},width}];
];


(* ::Section:: *)
(*User functions*)


(* ::Subsection:: *)
(*ComputeWidths*)


Options[ComputeWidths] = {SimplifyDecays -> True, Save -> True};


ComputeWidths[list_List, OptionsPattern[]] := Block[{
    verts = list,
    simplify = OptionValue[SimplifyDecays],
    save = OptionValue[Save]  
    },
  
    verts = FlavorExpansion[verts];
    verts = CalculateM2Decays[verts];
    verts = ComputeDecays[verts, Simplify -> simplify, ProgressIndicator -> False, Save -> save];

    Return[verts];

];


(* ::Subsection:: *)
(*GetDecayChannel*)


(* ::Text:: *)
(*GetDecayChannel[decay, verts] reads out a decay from the verts.*)


GetDecayChannel[list_List, verts_List] := Block[{
   in = list[[1]],
   out = Sort[Rest[list]],
   channel = verts
   },

   If[channel === {},
      Message[Decay::NoChannels];
      Return[{list,0}];
     ];

   channel = Cases[channel, {{in, ___}, _}];
   
   If[channel === {},
      Message[Decay::Channel];
      Return[{list, 0}];
    ];

   channel = MapAt[Sort[Rest[#]]&, #, 1]& /@ channel;

   channel = Cases[channel, {out, _}];

   If[channel ==={},
      Message[Decay::Channel];
      Return[0];
     ];

   If[Length[channel] > 1,
      Message[Decay::Mult];
      Return[0];
     ];

    Return[{list, channel[[1,2]]}];

];


(* ::Subsection:: *)
(*PartialWidth*)


Options[PartialWidth] = {Verbose -> True};


PartialWidth[Rule[in_, list_List]|RuleDelayed[in_, list_List], options___] := PartialWidth[Prepend[list, in], options];


PartialWidth[list_List, vertices_:Automatic, OptionsPattern[]] := Block[{
   in = list[[1]],
   out = Rest[list],
   inmass, outmasses, numinmass, numoutmasses,width,
   verbose = OptionValue[Verbose], tempvertices = vertices
   },

   If[vertices === Automatic,
      tempvertices = FR$PartialWidths
     ];

   If[AntiFieldQ[in],
      Return[PartialWidth[anti /@ list]];
     ];

   inmass = Mass[in];
   outmasses = Mass /@ out;

   numinmass = Abs[NumericalValue[inmass]];
   numoutmasses = Abs/@(NumericalValue /@ outmasses);

   If[Not[NumberQ[numinmass]]||Not[And @@ (NumberQ /@ numoutmasses)], 
      Message[Decay::NumMass];
      Return[Null];
     ];
   
   If[numinmass < (Plus @@ numoutmasses),
      If[verbose, Message[Decay::NotOpen]];
      Return[0];
     ];

    width = Factor[GetDecayChannel[list, tempvertices][[2]]];

    Return[width];

];
      


(* ::Subsection:: *)
(*TotWidth*)


TotWidth[in_, vertices_:Automatic] := Block[{
   channels = vertices
   },

   If[channels === Automatic,
      channels = FR$PartialWidths
      ];

   If[AntiFieldQ[in],
      Return[TotWidth[anti[in]]];
     ];

   If[channels === {},
      Message[Decay::NoChannels];
      Return[0];
     ];

   channels = Cases[channels, {{in,___}, _}];

   If[channels === {},
      Message[Decay::Channel];
      Return[0];
     ];

   channels = First /@ channels;
   channels = PartialWidth[#, vertices, Verbose -> False]& /@ channels;
   channels = Factor[Plus @@ channels];

   Return[channels];

];
      

   


(* ::Subsection:: *)
(*BranchingRatio*)


BranchingRatio[Rule[a_, list_List]|RuleDelayed[a_, list_List], vertices_:Automatic] := BranchingRatio[Prepend[list,a], vertices];


BranchingRatio[list_List, vertices_:Automatic] := Block[{partW, totW, tempvertices = vertices},

   If[vertices === Automatic,
      tempvertices = FR$PartialWidths
     ];
 
     partW = PartialWidth[list, tempvertices];
     totW  = TotWidth[list[[1]], tempvertices];

     If[totW === 0,
        Return[Null];
       ];
    
     Return[NumericalValue[Factor[partW/totW]]]
]; 


(* ::Subsection:: *)
(*UpdateWidths[]*)


UpdateWidths[vertices_:Automatic] := Block[{
   initials,
   widths,
   testWidth, printWidth, tempvertices = vertices
   },

   If[vertices === Automatic,
      tempvertices = FR$PartialWidths
     ];
   
   (* subroutines *)

   testWidth[Rule[a_, b_]] := If[b === False,
            Message[Decay::NumWidth, a];
            ];

   printWidth[Rule[a_,b_]] := Print[ToString[a], " = ", ToString[b], "."];

  (*************)

   If[tempvertices === {},
      Message[Decay::NoChannels];
      Return[];
     ];

  (**************)

   initials = Union[First/@ (First /@ tempvertices)];
   widths = Width /@ initials;

   widths = DeleteCases[Inner[List, widths, initials, List], List[0, _]];
   initials = Last /@ widths;
   widths = First /@ widths;

   

   initials = NumericalValue[TotWidth[#, tempvertices]]& /@ initials;
   initials = If[Not[NumberQ[#]], False, #]& /@ initials;

   widths = Inner[Rule, widths, initials, List];

   testWidth /@ widths;

   widths = DeleteCases[widths, Rule[_,False]];

   printWidth /@ widths;

   UpdateParameters @@ widths;

   Return[];

];

   

   

   

   
