(* ::Package:: *)

(* ::Title:: *)
(*Superfields*)


(* ::Section:: *)
(*Misc*)


(* ::Subsubsection::Closed:: *)
(*TensDot2*)


TensDot2[___,0,___][__]=0;


(* ::Subsubsection::Closed:: *)
(*SuperfieldQ*)


SuperfieldQ[a_[inds__]]:=SuperfieldQ[a];

VectorSuperfieldQ[a_[inds__]]:=VectorSuperfieldQ[a];

ChiralSuperfieldQ[a_[inds__]]:=ChiralSuperfieldQ[a];


(* ::Subsubsection::Closed:: *)
(*Exponential of a superfield*)


SFExp[a_*f_?(VectorSuperfieldQ[#]===True&)]:=1+a*f+1/2*a^2*f^2;


(* ::Subsubsection::Closed:: *)
(*Superfield rules*)


M$SuperfieldRules[Sfield_]:=Flatten[DeleteCases[(#/.{Equal[Sfield,a__]->a,Equal[_,__]->{}})&/@M$Superfields,{}],1];

M$SuperfieldRules[SFName_?(SuperfieldQ[#]===True&)]:=
   Flatten[DeleteCases[(#/.{Equal[Sfield_,List[a___,Rule[ClassName,SFName],b___]]->{a,Rule[ClassName,SFName],b},Equal[_,__]->{}})&/@M$Superfields,{}],1];


(* ::Subsubsection::Closed:: *)
(*Grassmann variables*)


M$GrassmannTheta= {
  W[x1000] == {TeX->\[Theta], ClassName -> theta, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1001] == {ClassName -> eps0, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1002] == {ClassName -> eps1, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1003] == {ClassName -> eps2, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1004] == {ClassName -> eps3, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1005] == {ClassName -> eps4, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1006] == {ClassName -> eps5, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1007] == {ClassName -> eps6, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1008] == {ClassName -> eps7, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1009] == {ClassName -> eps8, Chirality->Left, SelfConjugate->False, Unphysical->True},
  W[x1010] == {ClassName -> eps9, Chirality->Left, SelfConjugate->False, Unphysical->True}
};


(* ::Section::Closed:: *)
(*Unit Testing*)


(* ::Subsubsection::Closed:: *)
(*SFExists*)


(* ::Text:: *)
(*SFExists[ name ] checks if a superfield is already used.*)
(*  * If not, a warning is printed.*)


SFExists[name_] := If[MemberQ[M$SuperfieldClasses,name], Message[SF::SF]];


(* ::Subsubsection::Closed:: *)
(*SFClassNameTest*)


(* ::Text:: *)
(*SFClassNameTest[ name ] checks if name is a symbol. *)
(*  * If not, a warning is printed.*)


SFClassNameTest[name_] := If[Not[Head[name] === Symbol], Message[SF::ClassName]];


(* ::Subsubsection::Closed:: *)
(*SFClassNameExists*)


(* ::Text:: *)
(*SFClassNameExists[ name ] checks if a name is already used.*)
(*  * If yes, a warning is printed, and MR$Null is returned.*)


SFClassNameExists[name_] := If[MemberQ[M$SuperfieldNames,name],Message[SF::SF]];


(* ::Subsubsection::Closed:: *)
(*SFGaugeBosonTest and VSFGaugeBosonTest*)


(* ::Text:: *)
(*SFGaugeBosonTest[ field ] checks if field is a vector field. *)
(*  * If not, a warning is printed.*)


SFGaugeBosonTest[field_] := If[Not[VectorFieldQ[field] === True || field===MR$Null], Message[SF::GaugeBoson]];


(* ::Text:: *)
(*VSFGaugeBosonTest[ field ] checks if field is an existing vector field. *)
(** If not, a warning is printed.*)


VSFGaugeBosonTest[field_] := If[Not[VectorFieldQ[field] === True], Message[VSF::GaugeBoson]];


(* ::Subsubsection::Closed:: *)
(*SFGauginoTest and VSFGauginoTest*)


(* ::Text:: *)
(*SFGauginoTest[ field ] checks if field is a left-handed Weyl fermionic field. *)
(*  * If not, a warning is printed.*)


SFGauginoTest[field_] := If[Not[(WeylFieldQ[field] === True && Chirality[field]===Left) || field===MR$Null], Message[SF::Gaugino]];


(* ::Text:: *)
(*VSFGauginoTest[ field ] checks if field is an existing Weyl fermionic field. *)
(** If not, a warning is printed.*)


VSFGauginoTest[field_] := If[Not[WeylFieldQ[field]===True], Message[VSF::Gaugino]];


(* ::Subsubsection::Closed:: *)
(*SFWeylTest, CSFWeylTest and CSFChiralityTest*)


(* ::Text:: *)
(*SFWeylTest[ field ] checks if field is a Weyl fermionic field. *)
(*  * If not, a warning is printed.*)


SFWeylTest[field_] := If[Not[WeylFieldQ[field] === True || field===MR$Null], Message[SF::Weyl]];


(* ::Text:: *)
(*CSFWeylTest[ field ] checks if field is an exsiting Weyl fermionic field. *)
(** If not, a warning is printed.*)


CSFWeylTest[field_] := If[Not[WeylFieldQ[field] === True], Message[CSF::Weyl]];


(* ::Text:: *)
(*CSFChiratlityTest[ chirality, weyl component ] checks if the Weyl component of a chiral superfield has the same chirality as the superfield.*)
(** If not, a warning is printed.*)


CSFChiralityTest[chir_, field_] := If[Not[Chirality[field] === chir], Message[CSF::Chirality]];


(* ::Subsubsection::Closed:: *)
(*SFScalarTest and CSFScalarTest*)


(* ::Text:: *)
(*SFScalarTest[ field ] checks if field is a scalar field. *)
(*  * If not, a warning is printed..*)


SFScalarTest[field_] := If[Not[ScalarFieldQ[field] === True  || field===MR$Null], Message[SF::Scalar]];


(* ::Text:: *)
(*CSFScalarTest[ field ] checks if field is an exsiting scalar field. *)
(** If not, a warning is printed.*)


CSFScalarTest[field_] := If[Not[ScalarFieldQ[field] === True], Message[CSF::Scalar]];


(* ::Subsubsection::Closed:: *)
(*SFAuxiliaryest, CSFAuxiliaryTest and VSFAuxiliaryTest*)


(* ::Text:: *)
(*SFAuxiliaryTest[ field ] checks if field is a scalar field and is unphysical. *)
(*  * If not, a warning is printed.*)


SFAuxiliaryTest[field_] := If[ (Not[ ((ScalarFieldQ[field] === True) &&  (UnphysicalQ[field]===True)) || field===MR$Null]), Message[SF::Auxiliary]];


(* ::Text:: *)
(*CSFAuxiliaryTest[ field ] checks if field is not self-conjugate*)
(** If not, a warning is printed.*)


CSFAuxiliaryTest[field_] := If[SelfConjugateQ[field] === True, Message[CSF::Auxiliary]];


(* ::Text:: *)
(*VSFAuxiliaryTest[ field ] checks if field is self-conjugate*)
(**  If not, a warning is printed.*)


VSFAuxiliaryTest[field_] := If[Not[SelfConjugateQ[field] === True], Message[VSF::Auxiliary]];


(* ::Subsubsection::Closed:: *)
(*CSFIndicesTest and VSFIndicesTest*)


(* ::Text:: *)
(*CSFIndicesTest[ weyl component, scalar component, auxiliary component, indices of the superfield ] checks if the components of a chiral superfield have the same indices as the superfield itself.*)
(** If not, a warning is printed.*)


CSFIndicesTest[weyl_, sca_, aux_, inds_List] := If[Not[Rest[$IndList[weyl]]===$IndList[sca] && inds===$IndList[sca] && $IndList[aux]===$IndList[sca]],
               Message[CSF::Indices]];


(* ::Text:: *)
(*VSFIndicesTest[ gaugino component, gauge boson component, auxiliary component, indices of the superfield ] checks if the components of a vector superfield have the same indices as the superfield itself.*)
(** If not, a warning is printed.*)


VSFIndicesTest[ino_, vec_, aux_, inds_List] := If[Not[Rest[$IndList[ino]]===$IndList[aux] && inds===$IndList[aux] && Rest[$IndList[vec]]===$IndList[aux]],
               Message[VSF::Indices]];


(* ::Subsubsection::Closed:: *)
(*CSFQNumbersTest*)


(* ::Text:: *)
(*CSFQNumbersTest[ sf, weyl component, scalar component, auxiliary component ] checks if the components of a chiral superfield have the same quantum numbers as the superfield itself.*)
(** If not, a warning is printed.*)


CSFQNumbersTest[sf_, weyl_, sca_, aux_] := Block[{
   Qsca=#[sca]&/@MR$QuantumNumbers/.func_[sca]->0, 
   Qweyl=#[weyl]&/@MR$QuantumNumbers/.func_[weyl]->0, 
   Qsf=#[sf]&/@MR$QuantumNumbers/.func_[sf]->0, 
   Qaux=#[aux]&/@MR$QuantumNumbers/.func_[aux]->0},
   If[Not[Qsca===Qweyl && Qsca===Qsf && Qsca ===Qaux], Message[CSF::QNumbers]]];


(* ::Subsubsection::Closed:: *)
(*VSFGroup*)


(* ::Text:: *)
(*This tests that if a vector superfield is attached to a gauge group, the associated gauge boson is the same*)


VSFGroup[classname_,gaugeboson_] := If[MemberQ[(Superfield/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList,classname],
   If[Not[MemberQ[{(GaugeBoson/.MR$GaugeGroupRules[#]),(Superfield/.MR$GaugeGroupRules[#])}&/@MR$GaugeGroupList,{gaugeboson,classname}]],
     Message[VSF::Boson]]];


(* ::Subsubsection::Closed:: *)
(*Gauge group test:  are Superfield and GaugeBoson consistent?*)


GaugeGroupTest[]:= If[DeleteCases[(SF2Boson[Superfield]==GaugeBoson)/.MR$GaugeGroupRules[#]&/@MR$GaugeGroupList,True | Equal[SF2Boson[Superfield],_]]=!=List[],
    Message[Gauge::SF]];


(* ::Section:: *)
(*Superfield declarations*)


(* ::Subsection::Closed:: *)
(*General superfield*)


(* ::Text:: *)
(*These are the options for the superfield classes:*)


SF$AuxTermCounter = 1;


Options[SuperfieldDeclaration] = {
       ClassName      -> MR$Null, 
       GaugeBoson     -> MR$Null,
       Gaugino        -> MR$Null,
       Weyl           -> MR$Null,
       Scalar         -> MR$Null,
       Auxiliary      -> MR$Null,
       Indices        -> {},
       Unphysical     -> False,
       Chirality      -> Left,
       QuantumNumbers -> {}
};


(* ::Text:: *)
(*SuperfieldDeclaration[list] declares the superfields coming from the model file.*)
(*list must be a list of superfields, either chiral or vector.*)


SuperfieldDeclaration[sflist_List] := SuperfieldDeclaration /@ sflist;


(* ::Text:: *)
(*SuperfieldDeclaration[ sf == {    } ]  declares a single superfield.*)


SuperfieldDeclaration[Equal[sf_, list_List]] := SuperfieldDeclaration[sf, Sequence @@ list];


SuperfieldDeclaration[sf_, OptionsPattern[]] := Block[{classname = OptionValue[ClassName]},

(* Update the lists *)
   SFExists[sf];
   AppendTo[M$SuperfieldClasses, sf];

(* Unit testing *)
   SFClassNameTest[classname];
   SFClassNameExists[classname];
   AppendTo[M$SuperfieldNames, classname];
   SuperfieldQ[classname] = True;
   SuperfieldQ[HC[classname]]=True;
   SuperfieldQ[ToExpression[ToString[classname]<>"bar"]]=True;
   
   SFGaugeBosonTest[OptionValue[GaugeBoson]];
   SFWeylTest[OptionValue[Weyl]];
   SFGauginoTest[OptionValue[Gaugino]];
   SFScalarTest[OptionValue[Scalar]];
   SFAuxiliaryTest[OptionValue[Auxiliary]];

(* Branching into vector and chiral fields *)
   Which[Head[sf] === CSF, DeclareChiralSuperfield[sf, Sequence@@(#->OptionValue[#]&/@Options[SuperfieldDeclaration][[All,1]])],
         Head[sf] === VSF, DeclareVectorSuperfield[sf, Sequence@@(#->OptionValue[#]&/@Options[SuperfieldDeclaration][[All,1]])],
         True, Message[SF::Type]
        ];
];


(* ::Subsection::Closed:: *)
(*Chiral superfields*)


(* ::Text:: *)
(*DeclareChiralSuperfield{sf , options] declares a chiral superfield coming from the model file.*)


DeclareChiralSuperfield[sf_, OptionsPattern[SuperfieldDeclaration]] := Block[{
   (* options *)
   classname = OptionValue[ClassName],
   weyl = OptionValue[Weyl],
   scalar = OptionValue[Scalar],
   auxiliary = OptionValue[Auxiliary],
   auxnum = Max[#[[1]]&/@Cases[MR$ClassesList,S[_]]]+1,
   indices = OptionValue[Indices],
   unphysical = OptionValue[Unphysical],
   chirality = OptionValue[Chirality],
   qnum = OptionValue[QuantumNumbers],
   myrule
},

(* Update the lists *)
   AppendTo[M$ChiralSuperfieldClasses, sf];
   AppendTo[M$ChiralSuperfieldNames, classname];

(* Declare the auxiliary field *)   
   If[auxiliary===MR$Null, 
      auxiliary = Symbol["FTerm" <> ToString[SF$AuxTermCounter++]];
      MR$ClassesRules[S[auxnum]] = { ClassName->auxiliary, SelfConjugate->False, Indices->indices, Unphysical->True};
      AppendTo[M$ClassesDescription,S[auxnum] == MR$ClassesRules[S[auxnum]]];
      AppendTo[MR$ClassesDescription,Rule[S[auxnum],MR$ClassesRules[S[auxnum]]]];
      AppendTo[MR$ClassesList, S[auxnum]];
      UnphysicalQ[auxiliary] = True;
      SelfConjugateQ[auxiliary] = False;      
      AddParticlesToClass[{auxiliary}, S[auxnum], 
        Sequence@@((#->OptionValue[#]&/@Options[SuperfieldDeclaration][[All,1]])/.Rule[ClassName,_]->Rule[ClassName,auxiliary])];
      DeclareQuantumNumbers[auxiliary, {} (*class members *), qnum];
      MR$Class[auxiliary]=S[auxnum]];

(* Unit testing *)
   CSFWeylTest[weyl];
   CSFChiralityTest[chirality, weyl];
   CSFScalarTest[scalar];
   CSFAuxiliaryTest[auxiliary];
   CSFIndicesTest[weyl, scalar, auxiliary, indices];

(* Name to classname *)
   SF2ClassName[classname] = sf;

(* Superfield to components *)
   SF2Aux[classname] = auxiliary;
   SF2Scalar[classname] = scalar;
   SF2Weyl[classname] = weyl;
   Scalar2SF[scalar] = classname;

(* Get the Superfield indices *)
   SF2Indices[classname]=indices;

(* antifields *)
   anti[classname] = ToExpression[ToString[classname]<>"bar"];
   anti[ToExpression[ToString[classname]<>"bar"]] = classname;
   HC[classname] = anti[classname];
   HC[ToExpression[ToString[classname]<>"bar"]] = classname;

(* Properties *)
   ChiralSuperfieldQ[classname] = True;
   ChiralSuperfieldQ[HC[classname]]=True;
   ChiralSuperfieldQ[ToExpression[ToString[classname]<>"bar"]]=True;
   $IndList[classname] = indices;
   Chirality[classname] = chirality;

(* Quantum numbers *)
   DeclareQuantumNumbers[classname, {} (*class members *), qnum];
   qnum=Union[qnum,myrule[#,#[classname]]&/@DeleteCases[Charge/.MR$GaugeGroupRules[#]&/@MR$GaugeGroupList,Charge]/.func_[classname]->0/.myrule->Rule];
   DeclareQuantumNumbers[classname, {} (*class members *), qnum];
   CSFQNumbersTest[classname,weyl,scalar,auxiliary];
   SF2QNumbers[classname]=qnum;
];


(* ::Subsection::Closed:: *)
(*Vector superfield*)


(* ::Text:: *)
(*DeclareVectorSuperfield{sf , options] declares a vector superfield coming from the model file.*)


DeclareVectorSuperfield[sf_, OptionsPattern[SuperfieldDeclaration]] := Block[{
   (* options *)
   classname = OptionValue[ClassName],
   gaugino = OptionValue[Gaugino],
   gaugeboson = OptionValue[GaugeBoson],
   auxiliary = OptionValue[Auxiliary],
   auxnum = Max[#[[1]]&/@Cases[MR$ClassesList,S[_]]]+1,
   indices = OptionValue[Indices],
   unphysical = OptionValue[Unphysical]
},

(* Update the lists *)
   AppendTo[M$VectorSuperfieldClasses, sf];
   AppendTo[M$VectorSuperfieldNames, classname];

(* Declare the auxiliary field *)   
   If[auxiliary===MR$Null, 
      auxiliary = Symbol["DTerm" <> ToString[SF$AuxTermCounter++]];
      MR$ClassesRules[S[auxnum]] = { ClassName->auxiliary, SelfConjugate->True, Indices->indices, Unphysical->True};
      AppendTo[M$ClassesDescription,S[auxnum] == MR$ClassesRules[S[auxnum]]];
      AppendTo[MR$ClassesDescription,Rule[S[auxnum],MR$ClassesRules[S[auxnum]]]];
      AppendTo[MR$ClassesList, S[auxnum]];
      UnphysicalQ[auxiliary] = True;
      SelfConjugateQ[auxiliary] = True;      
      AddParticlesToClass[{auxiliary}, S[auxnum], 
        Sequence@@((#->OptionValue[#]&/@Options[SuperfieldDeclaration][[All,1]])/.Rule[ClassName,_]->Rule[ClassName,auxiliary])];  
      MR$Class[auxiliary]=S[auxnum]];

(* Unit testing *)
   VSFGauginoTest[gaugino];
   VSFGaugeBosonTest[gaugeboson];
   VSFAuxiliaryTest[auxiliary];
   VSFIndicesTest[gaugino, gaugeboson, auxiliary, indices];

(* antifields *)
   anti[classname] = classname;
   HC[classname] = classname;

(* Name to classname *)
   SF2ClassName[classname] := sf;

(* Superfield to components *)
   SF2Aux[classname] = auxiliary;
   SF2Boson[classname] = gaugeboson;
   SF2Ino[classname] = gaugino;

(* Get the Superfield indices *)
   SF2Indices[classname]=indices;

(* Test the associated gauge group, if relevant *)
   VSFGroup[classname,gaugeboson];

(* Properties *)
   VectorSuperfieldQ[classname] = True;
   $IndList[classname] = indices;
];


(* ::Section:: *)
(*Non commutative environments*)


(* ::Subsubsection:: *)
(*Definition*)


(* ::Text:: *)
(*nc [... ] represents an ordered sequence of fermionic Weyl fermions. *)
(*  * The theta fields appear first*)
(*  * The thetabar fields appear next*)
(*  * The other fermionic fields appear at the end*)
(*  * All the spin indices are down indices*)


(* ::Subsubsection::Closed:: *)
(*Basic operations*)


nc[___,0,___]=0;

nc[]=1;\:ffff

nc[a___,Plus[b_,c__],d___] := nc[a,b,d]+nc[a,Plus[c],d];

nc[a___,Times[b_,c__],d___] := nc[a,b,c,d];

nc[a___,nc[b__],c___] := nc[a,b,c];

nc[a___] nc[b___] ^:= nc[a,b];


(* ::Subsubsection::Closed:: *)
(*Commuting stuff*)


nc[a___,b_?(numQ[#]===True || (BosonQ[#]===True && Not[SuperfieldQ[#] === True])&),c___] := b*nc[a,c];

nc[a___,f_[b_?(numQ[#]===True || (BosonQ[#]===True && Not[SuperfieldQ[#] === True]) &)],c___] := f[b]*nc[a,c];

nc[a___,Power[b_?(numQ[#]===True || (BosonQ[#]===True && Not[SuperfieldQ[#] === True]) &),i_],c___] := Power[b,i]*nc[a,c];

nc[a___,t:(Ueps|Deps|EDelta)[b__],c___] := t nc[a,c];

nc[a___,IndexDelta[b_,c_],d___] := IndexDelta[b,c] nc[a,d];


(* ::Subsubsection::Closed:: *)
(*Hermitian conjugation*)


HC[nc[a__]] ^:= nc[Sequence@@Reverse[(HC[#]&@List[a])]];


(* ::Subsubsection::Closed:: *)
(*Back to the nc environment*)


(* ::Text:: *)
(*Tonc[ expression ] restore the nc environment (and remove all Dot and TensDot2).*)


Tonc[exp_]:=Module[{tmp,rules,swap,savedel},

(* Chirality swapping function *)
   swap[Left]:=Right;
   swap[Right]:=Left;

(*Treat the derivatives*)
   tmp=exp//.{del[Except[del,x_][aa__],mu_]:>x[aa,mu,savedel]};
   tmp=tmp//.{del[Except[del,x_],mu_]:>x[mu,savedel]};

(* Add a nc environment *)
   tmp=nc[tmp];

(* Breaking of the tensdot structures *)
   rules:={
      TensDot2[f_?(FieldQ[#]===True&)[sp_,inds___]][down,chir_,a_]->nc[f[sp,inds]],
      TensDot2[si[mu_,a_,b_],ff__][down,chir_,a_]->si[mu,a,b] TensDot2[ff][up,swap[chir],b],
      TensDot2[ff__,si[mu_,a_,b_]][down,Right,b_]->si[mu,a,b] TensDot2[ff][up,Left,a],
      TensDot2[ff__,sibar[mu_,a_,b_]][up,Left,b_]->sibar[mu,a,b] TensDot2[ff][down,Right,a],
      TensDot2[sibar[mu_,a_,b_],ff__][up,chir_,a_]->sibar[mu,a,b] TensDot2[ff][down,swap[chir],b],
      TensDot2[f_?(FieldQ[#]===True&)[sp_,inds___]][up,chir_,sp_]:> Module[{sp1},nc[f[sp1,inds]] Ueps[sp,sp1]],
      Dot[a_?(Chirality[#]===Left&)[sp1_,inds1___],b_?(Chirality[#]===Left&)[sp11_,inds2___]]:>Module[{sp2},nc[a[sp2,inds1],b[sp11,inds2]] Ueps[sp1,sp2]],
      Dot[a_?(Chirality[#]===Right&)[sp11_,inds1___],b_?(Chirality[#]===Right&)[sp1_,inds2___]]:>Module[{spdot},nc[a[sp11,inds1],b[spdot,inds2]]*
        Ueps[sp1,spdot]],
      Dot[a_?(Chirality[#]===Left&)[sp1_,inds1___],b_?(Chirality[#]===Right&)[sp2_,inds2___]]:>
        Module[{sp3,sp4},nc[a[sp3,inds1],b[sp4,inds2]] Ueps[sp1,sp3] Ueps[sp2,sp4]]  
   };
   tmp=ReplaceRepeated[tmp,rules];

(* Restore the derivatives *)
   tmp=tmp//.{x_[aa__,mu_,savedel]:>del[x[aa],mu],x_[mu_,savedel]:>del[x,mu]};

tmp];


(* ::Section:: *)
(*Expansion of superfields to components*)


(* ::Subsubsection:: *)
(*Main routine: SF2Components[  ]*)


(* ::Text:: *)
(*SF2Components [ expression ] expand the superfields appearing in expression in terms of their components. *)
(*The function returns a list:*)
(*  * The first element is the expanded superfields*)
(*  * The second element is the list of the coefficients various possible combinations of Grassmann variables: *)
(*      { scalar,  theta,  thetabar,  theta si thetabar,  theta^2,  thetabar^2,  theta^2 thetabar,  thetabar^2 theta,  theta^2 thetabar^2  }*)
(* Let us note that:*)
(*   *  alpha and alphadot are the uncontracted spin indices (where relevant).*)
(*   * mu is the Lorent index of the vectorial component of the superfield (if relevant).*)
(*   * the fermion flux is always (theta . something) or (thetabar . something)*)


SF2Components[Plus[a_,b__]]:=Module[{mu,alpha,alphadot},SF2Components[a,mu,alpha,alphadot] + SF2Components[Plus[b],mu,alpha,alphadot]];


SF2Components[Plus[a_,b__],mu_,alpha_,alphadot_]:=SF2Components[a,mu,alpha,alphadot] + SF2Components[Plus[b],mu,alpha,alphadot];


SF2Components[exp_?(Head[#]=!=Plus&)]:=Module[{mu,alpha,alphadot},SF2Components[exp,mu,alpha,alphadot]];


SF2Components[exp_?(Head[#]=!=Plus&),mu_,alpha_,alphadot_] := Module[{tmp, resu, ll}, 
   (* Apply the definitions of the superfields *)
   tmp=ApplySuperfieldDefinitions[exp];

   (* Usefull pre-simplifications improving the speed *)   
   tmp = Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
   tmp = ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0];

   (* Simplify the number of used Grassmann variable combinations *)
   $OptIndex=1;
   If[MatchQ[tmp,Plus[_,__]],tmp=Expand[tmp]/.Plus[a_,b__]->List[a,b]; tmp=Plus@@(ToGrassmannBasis/@tmp),tmp=ToGrassmannBasis[tmp]];

(* Simplify Lie algebra *)
   If[FR$Sugra=!=-1,Do[
     If[!(Abelian/.MR$GaugeGroupRules[MR$GaugeGroupList[[ll]]]),tmp=SimplifyLieAlgebra[Expand[tmp],MR$GaugeGroupList[[ll]]]],
  {ll,1,Length[MR$GaugeGroupList]}];Clear[ll];];

   (* Collect the components of the expanded superfield *)
   resu = List[tmp,ComponentList[tmp,mu,alpha,alphadot]];

   (* Possible index optimization on the theta.sigma.thetbar term due to the cut spin indices*)
   resu[[2,4]]=OptimizeIndex[resu[[2,4]]]/.{Index[Spin1,b_]->b,Index[Spin2,b_]->b, Index[Lorentz,b_]->b};
resu];


(* ::Subsubsection:: *)
(*Superfield definitions*)


(* ::Text:: *)
(*ApplySuperfieldDefinitions [ expression ] applies the definition of the superfields.*)


ApplySuperfieldDefinitions[exp_] := Module[{tmp,dum,dumdum},
(* Kinetic terms *)
  tmp=exp/.CSFKineticTerms[sf_?(ChiralSuperfieldQ[#]===True&)]:>CSFKineticTerms[ToString[sf]];

(* Add a non commutative environment before breaking all the dot products *)
   tmp=nc[Expand[tmp]];

(* Expanding the superfield strengths *)
   tmp= Expand[tmp /. VSFKineticTerms[SF_,True]:>Module[{sp3,sp4,sp3d,sp4d},
     nc[SuperfieldStrengthL[SF,sp4],SuperfieldStrengthL[SF,sp3]] Ueps[sp3,sp4]+nc[SuperfieldStrengthR[SF,sp4d],SuperfieldStrengthR[SF,sp3d]] Ueps[sp4d,sp3d]]];
   tmp= Expand[tmp /. VSFKineticTerms[SF_,False]:>Module[{sp3,sp4,sp3d,sp4d,Gi},
     nc[SuperfieldStrengthL[SF,sp4,Gi],SuperfieldStrengthL[SF,sp3,Gi]] Ueps[sp3,sp4]+
     nc[SuperfieldStrengthR[SF,sp4d,Gi],SuperfieldStrengthR[SF,sp3d,Gi]] Ueps[sp4d,sp3d]]];

(* Check the spinorial superfields arguments and call the correction functions *)
   tmp=tmp/.sc2_?((#===SuperfieldStrengthL) || (#===SuperfieldStrengthR) &)[SF_?(VectorSuperfieldQ[#]=!=True &),inds__] :> Module[{},Message[SuperfieldStrength::Arguments]; Abort[]];
   tmp=tmp/.sc2_?((#===SuperfieldStrengthL) || (#===SuperfieldStrengthR) &)[SF_?(!StringQ[#] && VectorSuperfieldQ[#] &),inds__] :> sc2[dumdum,ToString[SF],inds];

(* Powers of superfields must be treated in a special way in order to create enough different indices *)
   tmp = tmp /. Power[a_?(SuperfieldQ[#]===True&),n_]:>(dum[Table[a,#]&@{n}]/.dum[List[aa__]]:>dum[aa]);
   tmp = tmp /. Power[a_?(SuperfieldQ[#]===True&)[inds__],n_]:>(dum[Table[a[inds],#]&@{n}]/.dum[List[aa__]]:>dum[aa]);

(* Expansion of the chiral superfields *)
   tmp = ReplaceRepeated[tmp,
      {b_?(ChiralSuperfieldQ[#]===True && Chirality[#]===Left &)[inds__]:> ExpandLCSF[SF2Scalar[b],SF2Weyl[b],SF2Aux[b],List[inds]],
       b_?(ChiralSuperfieldQ[anti[#]]===True && Chirality[anti[#]]===Left &)[inds__]:> 
            ExpandRCSF[HC[SF2Scalar[anti[b]]],HC[SF2Weyl[anti[b]]],HC[SF2Aux[anti[b]]],List[inds]],
       b_?(ChiralSuperfieldQ[#]===True && Chirality[#]===Right &)[inds__]:> ExpandRCSF[SF2Scalar[b],SF2Weyl[b],SF2Aux[b],List[inds]],
       b_?(ChiralSuperfieldQ[anti[#]]===True&&Chirality[anti[#]]===Right&)[inds__] :>
               ExpandLCSF[HC[SF2Scalar[anti[b]]],HC[SF2Weyl[anti[b]]],HC[SF2Aux[anti[b]]],List[inds]],
       b_?(ChiralSuperfieldQ[#]===True && Chirality[#]===Left &):> ExpandLCSF[SF2Scalar[b],SF2Weyl[b],SF2Aux[b],{}],
       b_?(ChiralSuperfieldQ[anti[#]]===True && Chirality[anti[#]]===Left &):> ExpandRCSF[HC[SF2Scalar[anti[b]]],HC[SF2Weyl[anti[b]]],HC[SF2Aux[anti[b]]],{}],
       b_?(ChiralSuperfieldQ[#]===True && Chirality[#]===Right &):> ExpandRCSF[SF2Scalar[b],SF2Weyl[b],SF2Aux[b],{}],
       b_?(ChiralSuperfieldQ[anti[#]]===True && Chirality[anti[#]]===Right &):> ExpandLCSF[HC[SF2Scalar[anti[b]]],HC[SF2Weyl[anti[b]]],HC[SF2Aux[anti[b]]],{}]
      }];

(* Expansion of the vector superfields *)
   tmp = ReplaceRepeated[Expand[tmp],
      {b_?(VectorSuperfieldQ[#]===True &)[inds__]:> ExpandVSF[SF2Ino[b],SF2Boson[b],SF2Aux[b],List[inds]],
       b_?(VectorSuperfieldQ[#]===True &):> ExpandVSF[SF2Ino[b],SF2Boson[b],SF2Aux[b],{}]
      }];

(* Expansion of the y variable *)
   tmp = ReplaceRepeated[tmp,b_?(FieldQ[#]===True&)[inds___,t:(y|yc)]:> X2Y[b,List[inds],t]];
   
(* Removal of the dummy variable introduced to treat the powers of superfields and the protection*)
   tmp = tmp/.{dum->Times,sc_[dumdum,argx__]->sc[argx]};

tmp];


(* ::Text:: *)
(*ExpandCSF [ chiral superfield components, indices ] expands a chiral superfield in terms of its components.  Let us remind that y^mu is defined by x^mu - I theta sigma^mu thetabar*)


ExpandRCSF[sp0_,sp12_,aux_,inds_List] := Module[{sp1dot,sp2dot,indsF,str=yc},
   indsF :=Sequence@@DeleteCases[Join[inds,{str}],Null | {}];
   sp0[indsF] + 
   Sqrt[2] Ueps[sp1dot,sp2dot] nc[thetabar[sp1dot],sp12[sp2dot,indsF]] - 
   Ueps[sp1dot,sp2dot] nc[thetabar[sp1dot],thetabar[sp2dot]] aux[indsF]/.a_[]:>a
];


ExpandLCSF[sp0_,sp12_,aux_,inds_List] := Module[{sp1,sp2,indsF,str=y},
   indsF :=Sequence@@DeleteCases[Join[inds,{str}],Null | {}];
   sp0[indsF] + 
   Sqrt[2] Ueps[sp2,sp1] nc[theta[sp1],sp12[sp2,indsF]] - 
   Ueps[sp2,sp1] nc[theta[sp1],theta[sp2]] aux[indsF]/.a_[]:>a
];


(* ::Text:: *)
(*ExpandVSF [ vector superfield components, indices ] expands a vector superfield in terms of its components*)


ExpandVSF[spin12_,spin1_,aux_,inds_List] := Module[{sp1,sp2dot,sp3,sp4dot,mu,indsD},
   indsD :=Sequence@@DeleteCases[inds,Null | {}];
   nc[theta[sp1],thetabar[sp2dot]] si[mu,sp3,sp4dot] Ueps[sp3,sp1]Ueps[sp4dot,sp2dot] spin1[mu,indsD] + 
   FR$Sugra*I Ueps[sp4dot,sp2dot] Ueps[sp3,sp1] nc[theta[sp1],theta[sp3],thetabar[sp4dot],HC[spin12][sp2dot,indsD]]-
   FR$Sugra*I Ueps[sp1,sp3] Ueps[sp2dot,sp4dot] nc[thetabar[sp2dot],thetabar[sp4dot],theta[sp3],spin12[sp1,indsD]]+
   1/2 Ueps[sp2dot,sp4dot] Ueps[sp3,sp1] nc[theta[sp1],theta[sp3],thetabar[sp2dot],thetabar[sp4dot]] aux[indsD] /.a_[]:>a
];


(* ::Text:: *)
(*X2Y[ expression ] expands the y^\mu variable in terms of the x^\mu and Grassmann variables.*)


X2Y[field_,inds_List, t_] := Module[{mu,sp1,sp2,sp3dot,sp4dot,tmp},
   tmp = Which[t===y, 
                 field[Sequence@@inds]-
                 FR$Sugra*I si[mu,sp2,sp3dot] nc[theta[sp1],thetabar[sp4dot],del[field[Sequence@@inds],mu]]*Ueps[sp2,sp1]*Ueps[sp3dot,sp4dot]-
                 1/4 nc[theta[sp1],theta[sp2],thetabar[sp3dot],thetabar[sp4dot],del[del[field[Sequence@@inds],mu],mu]]*Ueps[sp2,sp1]*Ueps[sp3dot,sp4dot],
               t===yc,
                 field[Sequence@@inds]+
                 FR$Sugra*I si[mu,sp2,sp3dot] nc[theta[sp1],thetabar[sp4dot],del[field[Sequence@@inds],mu]]*Ueps[sp2,sp1]*Ueps[sp3dot,sp4dot]-
                 1/4 nc[theta[sp1],theta[sp2],thetabar[sp3dot],thetabar[sp4dot],del[del[field[Sequence@@inds],mu],mu]]*Ueps[sp2,sp1]*Ueps[sp3dot,sp4dot]
   ];
   tmp/.{field[Sequence[]]->field,field[{}]->field}
];


(* ::Subsubsection:: *)
(*Simplification of the Grassmann variables*)


(* ::Text:: *)
(*ToGrassmannBasis [ expression ] is minimizing the number of independent Grassmann variable combinations.*)


ToGrassmannBasis[exp_]:=Block[{savedel,tmp},
(* Simplification of the Grassmann algebra: each fermion can only appear at most twice in a term *)
   tmp = ReplaceRepeated[Expand[exp],nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0];

(* Treat the derivatives *)
   tmp=tmp//.{del[Except[del,x_][aa__],mu_]:>x[aa,mu,savedel]};
   tmp=tmp//.{del[Except[del,x_],mu_]:>x[mu,savedel]};

(* Form dot products and Simplification of the Grassmann variable combinations *)
   tmp=GrassmannSimplify[tmp];

(* Derivatives are back *)
  tmp=tmp//.{x_[aa__,mu_,savedel]:>del[x[aa],mu],x_[mu_,savedel]:>del[x,mu]};

(* Make the printing more compact *)
   $OptIndex+=100;
   tmp = OptimizeIndex[Expand[tmp]]/.{Index[Spin1,b_]->b,Index[Spin2,b_]->b, Index[Lorentz,b_]->b};
   $OptIndex-=100; 
   tmp = OptimizeIndex[Expand[tmp]]/.{Index[Spin1,b_]->b,Index[Spin2,b_]->b, Index[Lorentz,b_]->b};

tmp];


(* ::Subsubsection::Closed:: *)
(*Grassmann variables simplifications*)


(* ::Text:: *)
(*GrassmannSimplify[ expression ] transform an expression in the basis of Grassmann variables { scalar,  theta,  thetabar,  theta si thetabar,  theta^2,  thetabar^2,  theta^2 thetabar,  thetabar^2 theta,  theta^2 thetabar^2  }.*)


GrassmannSimplify[exp_]:=Module[{tmp=Expand[exp],rules},
   rules:={
   (*Introduction of the upper indices for the fields and the TensDot2 environment*)
     nc[ff___,field_?(FieldQ[#]===True&)[a_,inds___],gg___] Ueps[a_,b_]:>-nc[ff,TensDot2[field[b,inds]][up,Chirality[field],b],gg],
     nc[ff___,field_?(FieldQ[#]===True&)[a_,inds___],gg___] Ueps[b_,a_]:> nc[ff,TensDot2[field[b,inds]][up,Chirality[field],b],gg],
     nc[ff___,TensDot2[hh__][up,Left,b_],gg___] si[mu_,b_,c_]:>nc[ff,TensDot2[hh,si[mu,b,c]][down,Right,c],gg],
     nc[ff___,TensDot2[hh__][up,Right,b_],gg___] si[mu_,c_,b_]:>nc[ff,TensDot2[si[mu,c,b],hh][down,Left,c],gg],
     nc[ff___,field_?(FieldQ[#]===True&)[b_,inds___],gg___] sibar[mu_,a_,b_]:>nc[ff,TensDot2[sibar[mu,a,b],field[b,inds]][up,Right,a],gg],
     nc[ff___,field_?(FieldQ[#]===True&)[a_,inds___],gg___] sibar[mu_,a_,b_]:>nc[ff,TensDot2[field[a,inds],sibar[mu,a,b]][up,Left,b],gg],
     nc[ff___,TensDot2[hh__][down,Left,b_],gg___] sibar[mu_,a_,b_]:>nc[ff,TensDot2[sibar[mu,a,b],hh][up,Right,a],gg],

   (*Merge left-handed structures*)
     nc[ff___,TensDot2[hh__][up,Left,b_],gg___,field_?(FieldQ[#]===True&)[b_,inds___],jj___]:> Dot[hh,field[b,inds]] nc[ff,gg,jj]*(-1)^Length[{gg}],
     nc[ff___,field_?(FieldQ[#]===True&)[b_,inds___],gg___,TensDot2[ii__][up,Left,b_],jj___]:>-Dot[ii,field[b,inds]] nc[ff,gg,jj]*(-1)^Length[{gg}],
     nc[ff___,TensDot2[hh__][up,Left,b_],gg___,TensDot2[ii__][down,Left,b_],jj___]:>Dot[hh,ii] nc[ff,gg,jj]*(-1)^Length[{gg}],
     nc[ff___,TensDot2[hh__][down,Left,b_],gg___,TensDot2[ii__][up,Left,b_],jj___]:>-Dot[ii,hh] nc[ff,gg,jj]*(-1)^Length[{gg}],  
     nc[ff___,TensDot2[hh__,fi1_?(FieldQ[#]===True &)[inds1__]][down,Left,b_],gg___,TensDot2[ii__,fi2_?(AntiFieldQ[#]===True&)[inds2__]][down,Left,a_],jj___]*Ueps[b_,a_]:> Dot[fi2[inds2],
         Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],a->b}],hh,fi1[inds1]] nc[ff,gg,jj](-1)^Length[{gg}],
       nc[ff___,TensDot2[hh__,fi1_?(FieldQ[#]===True &)[inds1__]][down,Left,b_],gg___,TensDot2[ii__,fi2_?(AntiFieldQ[#]===True&)[inds2__]][down,Left,a_],jj___]*Ueps[a_,b_]:> -Dot[fi2[inds2],
         Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],a->b}],hh,fi1[inds1]] nc[ff,gg,jj](-1)^Length[{gg}],

   (*Merge right-handed structures*)
     nc[ff___,TensDot2[hh__][up,Right,b_],gg___,field_?(FieldQ[#]===True&)[b_,inds___],jj___]:>-Dot[field[b,inds],hh] nc[ff,gg,jj]*(-1)^Length[{gg}],
     nc[ff___,field_?(FieldQ[#]===True&)[b_,inds___],gg___,TensDot2[ii__][up,Right,b_],jj___]:> Dot[field[b,inds],ii] nc[ff,gg,jj]*(-1)^Length[{gg}],
     nc[ff___,TensDot2[fi1_?(FieldQ[#]===True &)[inds1__],hh__][down,Right,b_],gg___,TensDot2[fi2_?(FieldQ[#]===True && Not[AntiFieldQ[#]===True]&)[inds2__],
       ii__][down,Right,a_],jj___]*Ueps[b_,a_]:> -Dot[fi1[inds1],hh,
       Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],a->b}],fi2[inds2]] nc[ff,gg,jj],
     nc[ff___,TensDot2[fi1_?(FieldQ[#]===True &)[inds1__],hh__][down,Right,b_],gg___,TensDot2[fi2_?(FieldQ[#]===True && Not[AntiFieldQ[#]===True]&)[inds2__],
       ii__][up,Right,a_],jj___]*Ueps[a_,b_]:> Dot[fi1[inds1],hh,
       Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],a->b}],fi2[inds2]] nc[ff,gg,jj],
     nc[ff___,TensDot2[hh__][up,Right,b_],gg___,TensDot2[ii__][down,Right,b_],jj___]:>-Dot[ii,hh] nc[ff,gg,jj]*(-1)^Length[{gg}],
     nc[ff___,TensDot2[hh__][down,Right,b_],gg___,TensDot2[ii__][up,Right,b_],jj___]:>  Dot[hh,ii] nc[ff,gg,jj]*(-1)^Length[{gg}],
     nc[ff___,TensDot2[fi1_?(AntiFieldQ[#]=!=True &)[inds1__],hh__][down,Right,b_],gg___,TensDot2[fi2_?(AntiFieldQ[#]=!=True&)[inds2__],ii__][down,Right,a_],jj___]*Ueps[a_,b_]:> 
         Dot[fi1[inds1],hh, Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],a->b}],fi2[inds2]] *
         nc[ff,gg,jj](-1)^Length[{gg}],

(* Merge left-right structures *)
     nc[ff___,TensDot2[theta[sp1_],hh__][down,Right,b_],gg___,TensDot2[ii__,theta[sp2_]][down,Left,c_],jj___]:> Module[{sp3},
       1/2*Dot[theta[sp3],theta[sp3]] nc[ff,gg,jj]*ReplaceRepeated[Times[hh],sp1->sp2] Times[ii]*(-1)^Length[{gg}]],

  (* epsilon tensor with upper indices *)   
     nc[ff___,TensDot2[ii__,theta[sp1_]][down,Left,b_],jj___]Ueps[a_,b_]:> 
        nc[ff,TensDot2[theta[sp1],
            Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],b->a}]][up,Left,a],jj],
     nc[ff___,TensDot2[ii__,theta[sp1_]][down,Left,b_],jj___]Ueps[b_,a_]:> 
       -nc[ff,TensDot2[theta[sp1],
            Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],b->a}]][up,Left,a],jj],
     nc[ff___,TensDot2[ii__,thetabar[sp1dot_]][down,Left,b_],jj___]Ueps[a_,b_]:> 
       -nc[ff,TensDot2[thetabar[sp1dot],
            Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],b->a}]][up,Left,a],jj],
     nc[ff___,TensDot2[ii__,thetabar[sp1dot_]][down,Left,b_],jj___]Ueps[b_,a_]:> 
        nc[ff,TensDot2[thetabar[sp1dot],
            Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],b->a}]][up,Left,a],jj],
     nc[ff___,TensDot2[theta[sp1_],ii__][down,Right,b_],jj___]Ueps[a_,b_]:> 
       -nc[ff,TensDot2[Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],b->a}],
            theta[sp1]][up,Right,a],jj],
     nc[ff___,TensDot2[theta[sp1_],ii__][down,Right,b_],jj___]Ueps[b_,a_]:> 
        nc[ff,TensDot2[Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],b->a}],
            theta[sp1]][up,Right,a],jj],
     nc[ff___,TensDot2[thetabar[sp1dot_],ii__][down,Right,b_],jj___]Ueps[a_,b_]:> 
        nc[ff,TensDot2[Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],b->a}],
            thetabar[sp1dot]][up,Right,a],jj],
     nc[ff___,TensDot2[thetabar[sp1dot_],ii__][down,Right,b_],jj___]Ueps[b_,a_]:> 
       -nc[ff,TensDot2[Sequence@@ReplaceRepeated[Reverse[{ii}],{si[mu_,sp_,sp2_]->Tsibar[mu,sp2,sp],sibar[mu_,sp_,sp2_]->Tsi[mu,sp2,sp],b->a}],
            thetabar[sp1dot]][up,Right,a],jj],
    Tsi->si, Tsibar->sibar,

   (* EDelta tensor *)
     EDelta[sp1_,sp2_] nc[ff___,field_?(FieldQ[#]===True&)[sp2_,inds___],gg___]:> nc[ff,field[sp1,inds],gg],
     EDelta[sp1_,sp2_]si[mu_,sp2_,sp3_]:> si[mu,sp1,sp3],
     EDelta[a_,b_] EDelta[b_,c_]:>EDelta[a,c],
     EDelta[a_,a_]->2,
     EDelta[a_,b_] nc[ff___,TensDot2[hh__][opt___,b_],gg___]:> nc[ff,TensDot2[Sequence@@ReplaceRepeated[{hh},b->a]][opt,a],gg],
     EDelta[b_,a_] nc[ff___,TensDot2[hh__][opt___,b_],gg___]:> nc[ff,TensDot2[Sequence@@ReplaceRepeated[{hh},b->a]][opt,a],gg],

   (*Remove Pauli matrices from the TensDot environment and order the dot products*)
     Dot[aa__,si[mu_,b_,c_],bb__]->si[mu,b,c] Dot[aa,bb],
     Dot[aa__,sibar[mu_,b_,c_],bb__]->sibar[mu,b,c] Dot[aa,bb],
     sibar[mu_,b_,c_] Dot[f1_[b_,inds1___],f2_[c_,inds2___]]->-si[mu,c,b] Dot[f2[c,inds2],f1[b,inds1]],
  
   (* Write theta first and thetabar second *)
     Dot[aa_?(!MatchQ[#,theta|thetabar]&)[sp_,inds___],theta[sp_]]:>Dot[theta[sp],aa[sp,inds]],
     Dot[thetabar[sp_],aa_?(!MatchQ[#,theta|thetabar]&)[sp_,inds___]]:>Dot[aa[sp,inds],thetabar[sp]],
     Dot[theta[sp1_],f_?(!MatchQ[#,theta|thetabar]&)[sp2_]] si[mu_,sp2_,sp3_]sibar[nu_,sp3_,sp1_]:> 
         Dot[theta[sp1],f[sp2]] si[nu,sp1,sp3]sibar[mu,sp3,sp2],
     Dot[f_?(!MatchQ[#,theta|thetabar]&)[sp2_,inds___],theta[sp1_]] si[mu_,sp2_,sp3_]sibar[nu_,sp3_,sp1_]:> 
         Dot[theta[sp1],f[sp2,inds]] si[nu,sp1,sp3]sibar[mu,sp3,sp2],
     Dot[thetabar[sp1_],f_?(!MatchQ[#,theta|thetabar]&)[sp2_,inds___]] sibar[mu_,sp1_,sp3_]si[nu_,sp3_,sp2_]:> 
         Dot[f[sp2,inds],thetabar[sp1]] si[mu,sp3,sp1]sibar[nu,sp2,sp3],

   (* theta.psi1 theta.(...).psi2 -> -1/2 theta.theta psi1.(...).psi2*)
     Dot[theta[sp1_],f1_[sp1_,ind1___]] Dot[theta[sp3_],f2_[sp2_,ind2___]] :> -1/2 Dot[theta[sp1],theta[sp1]] Dot[f1[sp3,ind1],f2[sp2,ind2]],
   (* theta.si[mu].psibar1 theta.(...).psi2 -> 1/2 theta.theta psibar1.sibar[mu].(...).psi2*)
     Dot[theta[sp1_],f1_[sp4_,ind1___]] si[mu_,sp1_,sp4_] Dot[theta[sp3_],f2_[sp2_,ind2___]] :> 
      1/2 Dot[theta[sp1],theta[sp1]] Dot[f1[sp4,ind1],f2[sp2,ind2]] sibar[mu,sp4,sp3],
   (* theta.psi1 psi2bar.thetabar -> 1/2 theta.si[mu].thetabar psi1.si[mu].psi2bar  *)
     Dot[theta[sp1_],f1_?(!MatchQ[#,theta|thetabar]&)[sp1_,ind1___]] Dot[f2_?(!MatchQ[#,theta|thetabar]&)[sp2_,ind2___],thetabar[sp2_]] :> 
         Module[{mu,sp3,sp4dot},1/2 Dot[theta[sp1],thetabar[sp2]] si[mu, sp1,sp2] si[mu, sp3,sp4dot] Dot[f1[sp3,ind1],f2[sp4dot,ind2]]],
   (* theta.psi1 psi2.si[mu].thetabar -> -1/2 theta.si[mu].thetabar psi1.si[mu].sibar[nu].psi2bar  *)
     Dot[theta[sp1_],f1_?(!MatchQ[#,theta|thetabar]&)[sp1_,ind1___]] Dot[f2_?(!MatchQ[#,theta|thetabar]&)[sp2_,ind2___],thetabar[sp5dot_]]*
      si[nu_,sp2_,sp5dot_] :> Module[{mu,sp3,sp4dot},
          -1/2 Dot[theta[sp1],thetabar[sp5dot]] si[mu,sp1,sp5dot] si[nu,sp3,sp4dot] sibar[mu, sp4dot,sp2] Dot[f2[sp3,ind2],f1[sp2,ind1]]],

   (* thetabar_alpdot psi2.si[mu].thetabar -> -1/2 psi2.si[mu].thetabar.thetabar *)
     nc[thetabar[sp1dot_]] Dot[f1_?(!MatchQ[#,thetabar]&)[sp2_,ind1___],thetabar[sp3dot_]]*si[mu_,sp2_,sp3dot_] :> 
          -1/2 Dot[thetabar[sp3dot],thetabar[sp3dot]] nc[TensDot2[f1[sp2,ind1],si[mu,sp2,sp1dot]][down,Right, sp1dot]],

   (* thetabar.psi1bar theta.si[mu].psi2bar -> -1/2 theta.si[mu].thetabar psi1bar.sibar[nu].si[mu].psi2bar  *)
     Dot[f1_?(!MatchQ[#,theta|thetabar]&)[sp1dot_,ind1___],thetabar[sp1dot_]] Dot[theta[sp2_],f2_?(!MatchQ[#,theta|thetabar]&)[sp5dot_,ind2___]]*
      si[nu_,sp2_,sp5dot_] :> Module[{mu,sp3dot,sp4},
          -1/2 Dot[theta[sp2],thetabar[sp1dot]] si[mu, sp2,sp1dot] sibar[nu,sp3dot,sp4] si[mu, sp4,sp5dot] Dot[f1[sp3dot,ind1],f2[sp5dot,ind2]]],

   (* psi1bar.(...).thetabar psi2bar.thetabar -> -1/2 thetabar.thetabar psi1bar.(...).psi2bar *)
     Dot[f1_[sp1_,ind1___],thetabar[sp3_]] Dot[f2_[sp2_,ind2___],thetabar[sp2_]] :> 
         -1/2 Dot[thetabar[sp2],thetabar[sp2]] Dot[f1[sp1,ind1],f2[sp3,ind2]],
   (* psi1bar.(...).thetabar psi.si[mu].thetabar -> 1/2 thetabar.thetabar psi1bar.(...).sibar[mu].psi *)
     Dot[f1_[sp1_,ind1___],thetabar[sp3_]] Dot[f2_[sp2_,ind2___],thetabar[sp4_]] si[mu_,sp2_,sp4_]:> 
         1/2 Dot[thetabar[sp4],thetabar[sp4]] Dot[f1[sp1,ind1],f2[sp2,ind2]] sibar[mu,sp3,sp2],
   
   (* psi1.si[mu].thetabar psi2bar.thetabar -> -1/2 thetabar.thetabar psi2.si[mu].psi1bar*)   
     Dot[f1_[sp2_,ind1___],thetabar[sp1_]] si[mu_,sp2_,sp1_] Dot[f2_[sp3_,ind2___],thetabar[sp3_]] :> 
         -1/2 Dot[thetabar[sp1],thetabar[sp1]] si[mu,sp2,sp3] Dot[f1[sp2,ind1],f2[sp3,ind2]],
   (* theta.si[mu].psi1bar theta.psi2 -> -1/2 theta.theta psi2.si[mu].psi1bar*)   
     Dot[theta[sp1_],f1_[sp2dot_,ind1___]] si[mu_,sp1_,sp2dot_] Dot[theta[sp3_],f2_[sp3_,ind2___]] :> 
         -1/2 Dot[theta[sp1],theta[sp1]] si[mu,sp3,sp2dot] Dot[f2[sp3,ind2],f1[sp2dot,ind1]],
   (* theta.si[mu].thetabar thetabar.psibar -> -1/2 theta.theta psi.si[mu].thetabar*)   
     Dot[theta[sp1_],thetabar[sp2dot_]] si[mu_,sp1_,sp2dot_] Dot[f2_[sp3dot_,ind2___],thetabar[sp3dot_]] :> 
         -1/2 Dot[thetabar[sp2dot],thetabar[sp2dot]] si[mu,sp1,sp3dot] Dot[theta[sp1],f2[sp3dot,ind2]],
     Dot[theta[sp1_],thetabar[sp2dot_]] si[mu_,sp1_,sp2dot_] Dot[theta[sp4_],f1_?(!MatchQ[#,theta|thetabar]&)[sp3dot_,ind1___]]si[nu_,sp4_,sp3dot_] :> 
           1/2 Dot[theta[sp1],theta[sp1]] Dot[f1[sp3dot,ind1],thetabar[sp2dot]] sibar[nu,sp3dot,sp4] si[mu,sp4,sp2dot],
   (* theta.si[mu].thetabar (blablabla thetabar)_alpha -> 1/2 thetabar.thetabar(blablabla.sibar[mu].theta)_alpha *)   
     Dot[theta[sp1_],thetabar[sp2dot_]] si[mu_,sp1_,sp2dot_] nc[TensDot2[ff__,thetabar[sp3dot_]][down,Left,sp4_]] :> 
          1/2 Dot[thetabar[sp2dot],thetabar[sp2dot]] nc[TensDot2[ff,sibar[mu,sp3dot,sp1],theta[sp1]][down,Left,sp4]],
     Dot[theta[sp1_],thetabar[sp2dot_]] si[mu_,sp1_,sp2dot_] Dot[f1_?(!MatchQ[#,theta|thetabar]&)[sp3_,ind1___],thetabar[sp4dot_]]si[nu_,sp3_,sp4dot_] :> 
           1/2 Dot[thetabar[sp2dot],thetabar[sp2dot]] Dot[f1[sp3,ind1],theta[sp1]] si[nu,sp3,sp4dot] sibar[mu,sp4dot,sp1],

   (* theta.si[mu].thetabar  theta.si[nu].thetabar -> 1/2 theta.theta thetabar.thetabar eta[mu,nu] *)
     Dot[theta[sp1_],thetabar[sp2dot_]] si[mu_,sp1_,sp2dot_] Dot[theta[sp3_],thetabar[sp4dot_]] si[nu_,sp3_,sp4dot_] :> 
          1/2 Dot[theta[sp1],theta[sp1]] Dot[thetabar[sp2dot], thetabar[sp2dot]]ME[mu,nu],

   (* Generalization of (si[mu].thetabar)_alpha psibar.thetabar -> -1/2 thetabar.thetabar (si[mu] psibar)_alpha *)
     nc[TensDot2[ff__,thetabar[sp2_]][down,Left,sp1_]] Dot[f1_?(!MatchQ[#,theta|thetabar]&)[sp3_,ind1___],thetabar[sp3_]]:> 
         -1/2 Dot[thetabar[sp3],thetabar[sp3]] nc[TensDot2[ff,f1[sp2,ind1]][down,Left,sp1]],
   (* Generalization of (si[mu].thetabar)_alpha theta.psi -> -1/2 thetabar.si[nu].thetabar (si[mu] sibar[nu] psi)_alpha *)
     nc[TensDot2[ff__,thetabar[sp2dot_]][down,Left,sp1_]] Dot[theta[sp4_],f1_?(!MatchQ[#,theta|thetabar]&)[sp4_,ind1___]]:> Module[{nu,sp5,sp6},         
         -1/2 Dot[theta[sp5],thetabar[sp2dot]]*si[nu,sp5,sp2dot] * nc[TensDot2[ff,sibar[nu,sp2dot,sp6],f1[sp6,ind1]][down,Left,sp1]]],
   (* Generalization of (si[mu].sibar[nu].theta)_alphadot theta.si[ro].psibar -> -1/2 theta.theta (si[mu] sibar[nu] si[ro] psibar)_alpha *)
     nc[TensDot2[ff__,theta[sp2_]][down,Left,sp1_]] si[mu_,sp3_,sp4dot_] Dot[theta[sp3_],f1_?(!MatchQ[#,theta]&)[sp4dot_,ind1___]]:>
         -1/2 Dot[theta[sp3],theta[sp3]] * nc[TensDot2[ff,si[mu,sp2,sp4dot],f1[sp4dot,ind1]][down,Left,sp1]],
   (* Generalization of (theta.si[mu])_alphadot theta.si[ro].psibar -> 1/2 theta.theta (psi.sibar[ro].sibar[mu])_alphadot *)
     nc[TensDot2[theta[sp1_],ff__][down,Right,sp1dot_]] si[mu_,sp2_,sp2dot_] Dot[theta[sp2_],f1_?(!MatchQ[#,theta]&)[sp2d0t_,ind1___]]:>
         1/2 Dot[theta[sp2],theta[sp2]] * nc[TensDot2[f1[sp2dot,ind1],sibar[mu,sp2dot,sp1],ff][down,Right,sp1dot]],
   (* Generalization of (thetabar.sibar[mu].si[nu])_alphadot psi.si[ro].thetabar -> -1/2 thetabar.thetabar (psi.si[ro].sibar[mu] si[nu])_alpha *)
     nc[TensDot2[thetabar[sp2dot_],ff__][down,Right,sp1dot_]] si[mu_,sp4_,sp3dot_] Dot[f1_?(!MatchQ[#,thetabar]&)[sp4_,ind1___],thetabar[sp3dot_]]:>
         -1/2 Dot[thetabar[sp3dot],thetabar[sp3dot]] * nc[TensDot2[f1[sp4,ind1],si[mu,sp4,sp2dot],ff][down,Right,sp1dot]],

   (* (sibar[nu].theta)^alphadot theta_alpha -> -1/2 theta.theta (si[mu] sibar[nu] si[ro] psibar)_alpha *)
     nc[TensDot2[sibar[mu_,sp1dot_,sp2_],theta[sp2_]][up,Right,sp1dot_],gg___,theta[sp3_]]:> Module[{sp2dot},
         -1/2 Dot[theta[sp2],theta[sp2]] * si[mu,sp3,sp2dot] (-1)^Length[{gg}] nc[gg] Ueps[sp1dot,sp2dot]],

   (* theta.si[mu].psibar  theta_alpha ->  -1/2 theta.theta (si[mu].psibar)_alpha *)
      nc[theta[sp2_]] Dot[theta[sp4_],f1_[sp3dot_,inds___]] si[nu_,sp4_,sp3dot_]:> 
          -1/2 Dot[theta[sp4],theta[sp4]] nc[TensDot2[si[nu,sp2,sp3dot], f1[sp3dot,inds]][down,Left,sp2]], 

   (* theta.si[mu].sibar[nu].theta  :> ME[mu,nu] theta.theta *)
     si[mu_,a_,b_] sibar[nu_,b_,c_] Dot[theta[a_],theta[c_]] :> ME[mu,nu] Dot[theta[a],theta[a]],
   (* thetabar.sibar[mu].si[nu].thetabar  :> ME[mu,nu] thetabar.thetabar *)
     sibar[mu_,a_,b_] si[nu_,b_,c_] Dot[thetabar[a_],thetabar[c_]] :> ME[mu,nu] Dot[thetabar[a],thetabar[a]],

   (* (si[mu] sibar[nu] theta) . (si[ro] sibar[sg] theta) :> theta.theta (ME[mu,nu] ME[ro,sg]+ME[ro,nu] ME[mu,sg] - ME[mu,ro] ME[nu,sg] - I Eps[mu,nu,ro,sg]*)
     nc[theta[sp1_],theta[sp2_]]*si[mu_,spa_,sp3_]*si[ro_,spb_,sp4_]*sibar[nu_,sp3_,sp1_]*sibar[sg_,sp4_,sp2_]*Ueps[spb_,spa_]:> 
       Dot[theta[sp1],theta[sp1]] (ME[mu,nu] ME[ro,sg]+ME[ro,nu] ME[mu,sg] - ME[mu,ro] ME[nu,sg] - I Eps[mu,nu,ro,sg]),

   (* 3 sigma matrices in a TensDot2 environment*)
     TensDot2[ff___,si[mu_,a_,b_],sibar[nu_,b_,c_],si[ro_,c_,d_],gg___][inds__] :> Module[{sg}, 
       ME[mu,nu] TensDot2[ff,si[ro,a,d],gg][inds] + ME[ro,nu] TensDot2[ff,si[mu,a,d],gg][inds] - ME[mu,ro] TensDot2[ff,si[nu,a,d],gg][inds] - 
       I Eps[mu,nu,ro,sg] TensDot2[ff,si[sg,a,d],gg][inds] ],

     TensDot2[ff___,sibar[mu_,a_,b_] si[nu_,b_,c_] sibar[ro_,c_,d_],gg___] :> Module[{sg}, 
      ME[mu,nu] TensDot2[ff,sibar[ro,a,d],gg][inds] + ME[ro,nu] TensDot2[ff,sibar[mu,a,d],gg][inds] - ME[mu,ro] TensDot2[ff,sibar[nu,a,d],gg][inds] + 
      I Eps[mu,nu,ro,sg] TensDot2[ff,sibar[sg,a,d],gg][inds] ]
   };
   tmp=Expand[tmp//.rules]//.rules;
   tmp];


(* ::Subsubsection:: *)
(*Extraction of the coefficients of the Grassmann variables combinations*)


(* ::Text:: *)
(*ComponentList [ expression ] extract the components of an expression depending on Grassmann variables. *)
(*The function returns the list of  the coefficients of the various possible combinations of Grassmann variables:  *)
(*   { scalar,  theta,  thetabar,  theta si thetabar,  theta^2,  thetabar^2,  theta^2 thetabar,  thetabar^2 theta,  theta^2 thetabar^2  }.*)
(* Let us note that: *)
(*    * sp1 and sp1dot are the uncontracted spin indices of the Grassmann variables (where relevant).*)
(*    * mu is the Lorentz index of the vectorial component of the superfield.*)


ComponentList[exp_,mu_,alpha_,alphadot_] := Module[{tmp,resu=Array[MR$Null,9],dum,dumdum,savedel,tmp2},

(*Treat the derivatives*)
   tmp2=exp//.{del[Except[del,x_][aa__],mmu_]:>x[aa,mmu,savedel]};
   tmp2=tmp2//.{del[Except[del,x_],mmu_]:>x[mmu,savedel]};

(* Scalar part *)  
   tmp=ReplaceRepeated[tmp2,t:(theta|thetabar)[_]->0];
   resu[[1]]=tmp;
   tmp=Expand[tmp2-tmp];

(* theta_alpha coefficient (alpha is the spin index of theta*)
   resu[[2]]=ReplaceRepeated[tmp//.{
      Dot[theta[sp_],f1_?(!MatchQ[#,(theta|thetabar)]&)]:>dumdum[sp] f1,
      nc[TensDot2[bla___,theta[sp_]][indx__]]:> dumdum[sp] nc[TensDot2[bla][{indx},{up,Left,sp}]],
      nc[theta[sp_],bla___]:> dumdum[sp] nc[bla]}, t:(theta|thetabar)[_]->0];
   resu[[2]]=resu[[2]]//.{Times[dumdum[sp_],gg__]:>ReplaceRepeated[Times[gg],sp->alpha]};

(* thetabar_alphadot coefficient (alpha_dot is the free index of thetabar *)
   resu[[3]] = Expand[Tonc[tmp]]//.{
     theta[_]->0,
     nc[aaa___,thetabar[sp1_],ccc___,thetabar[sp2_],bbb___]->0,
     nc[aaa___,fi_[indis__], thetabar[spd_],bbb___]->-nc[aaa,thetabar[spd],fi[indis],bbb]};
   resu[[3]]=resu[[3]]//.{x_[aa__,mmu_,savedel]:>del[x[aa],mmu],x_[mmu_,savedel]:>del[x,mmu]};
   resu[[3]] = ToGrassmannBasis[resu[[3]]/.expr_ nc[thetabar[spd_],argx___]:>ReplaceAll[expr nc[argx],spd->alphadot]];

(* theta sigma^mu thetabar coefficient*)
   resu[[4]]=ReplaceRepeated[tmp//.Dot[theta[a_],thetabar[b_]]si[nu_,a_,b_]->dumdum[nu],t:(theta|thetabar)[_]->0];
   resu[[4]]=resu[[4]]/.Times[dumdum[sp_],gg__]:>ReplaceRepeated[Times[gg],sp->mu];

(* theta^2 coefficient *)
   resu[[5]]=ReplaceRepeated[tmp//.Dot[theta[_],theta[_]]->1,t:(theta|thetabar)[_]->0];

(* thetabar^2 coefficient *)
   resu[[6]]=ReplaceRepeated[tmp//.Dot[thetabar[_],thetabar[_]]->1,t:(theta|thetabar)[_]->0];

(* theta^2 thetabar coefficient*)
  resu[[7]]=ReplaceRepeated[tmp//.{
      Dot[theta[_],theta[_]] Dot[f1_?((!MatchQ[#,(theta|thetabar)])&&(Chirality[#]===Right)&)[sp1_,inds___],thetabar[sp_]]:>dumdum[sp] f1[sp1,inds],
      Dot[theta[_],theta[_]] Dot[f1_?((!MatchQ[#,(theta|thetabar)])&&(Chirality[#]===Left)&)[sp1_,inds___],thetabar[sp_]]:>
        dumdum[sp] nc[TensDot2[f1[sp1,inds]][up,Left,sp1]],
      Dot[theta[_],theta[_]] nc[TensDot2[bla___,thetabar[sp_]][indx__]]:> dumdum[sp] nc[TensDot2[bla][{indx},{up,Right,sp}]]},t:(theta|thetabar)[_]->0];
   resu[[7]]=resu[[7]]/.Times[dumdum[sp_],gg__]:>ReplaceRepeated[Times[gg],sp->alphadot];

(* thetabar^2 theta coefficient*)
   resu[[8]]=ReplaceRepeated[tmp//.{
      Dot[thetabar[_],thetabar[_]] Dot[theta[sp_],f1_?((!MatchQ[#,(theta|thetabar)]) && (Chirality[#]===Left)&)[sp1_,inds___]]:>dumdum[sp] f1[sp1,inds],
      Dot[thetabar[_],thetabar[_]] Dot[theta[sp_],f1_?((!MatchQ[#,(theta|thetabar)]) && (Chirality[#]===Right)&)[sp1_,inds___]]:>
       dumdum[sp] nc[TensDot2[f1[sp1,inds]][up,Right,sp1]],
      Dot[thetabar[_],thetabar[_]] nc[TensDot2[theta[sp_],bla___][indx__]]:> dumdum[sp] nc[TensDot2[bla][{up,Left,sp},{indx}]]},t:(theta|thetabar)[_]->0];
   resu[[8]]=resu[[8]]/.Times[dumdum[sp_],gg__]:>ReplaceRepeated[Times[gg],sp->alpha];

(* theta^2 thetabar coefficient*)
   resu[[9]]=ReplaceRepeated[tmp//.Dot[theta[_],theta[_]] Dot[thetabar[_],thetabar[_]]:>1,t:(theta|thetabar)[_]->0];

(* Restore the derivatives *)
   resu=resu//.{x_[aa__,mmu_,savedel]:>del[x[aa],mmu],x_[mmu_,savedel]:>del[x,mmu]};

resu];


(* ::Subsubsection::Closed:: *)
(*Shortcut functions*)


GrassmannExpand[exp_]:=SF2Components[exp][[1]];


ScalarComponent[exp_]:= SF2Components[exp][[2,1]];
ThetaComponent[exp_]:= SF2Components[exp][[2,2]];
ThetaComponent[exp_,alpha_]:= Module[{mu,alphad},SF2Components[exp,mu,alpha,alphad][[2,2]]];
ThetabarComponent[exp_]:= SF2Components[exp][[2,3]];
ThetabarComponent[exp_,alphadot_]:= Module[{mu,alpha},SF2Components[exp,mu,alpha,alphadot][[2,3]]];
ThetaThetabarComponent[exp_]:= SF2Components[exp][[2,4]];
ThetaThetabarComponent[exp_,lorentz_]:= Module[{alphadot,alpha},SF2Components[exp,lorentz,alpha,alphadot][[2,4]]];
Theta2Component[exp_]:= SF2Components[exp][[2,5]];
Thetabar2Component[exp_]:= SF2Components[exp][[2,6]];
Theta2ThetabarComponent[exp_]:= SF2Components[exp][[2,7]];
Theta2ThetabarComponent[exp_,alphadot_]:= Module[{mu,alpha},SF2Components[exp,mu,alpha,alphadot][[2,7]]];
Thetabar2ThetaComponent[exp_]:= SF2Components[exp][[2,8]];
Thetabar2ThetaComponent[exp_,alpha_]:= Module[{mu,alphadot},SF2Components[exp,mu,alpha,alphadot][[2,8]]];
Theta2Thetabar2Component[exp_]:= SF2Components[exp][[2,9]];


(* ::Section:: *)
(*Superfield strength tensors*)


(* ::Subsubsection::Closed:: *)
(*SuperfieldStrengthL[ superfield, lower spin index ]*)


(* ::Text:: *)
(*SuperfieldStrengthL [ superfield, lower spin index ] transform a (scalar) vectorial superfield to the associated left-handed spinorial superfield in the case of an abelian gauge group.*)


SuperfieldStrengthL[SFstr_String,alpha_]:=Module[{Sfield, tmp,sp1dot,sp2dot},
  Sfield=Symbol[SFstr];

(* Check the abelianity *)
  If[(Indices/.M$SuperfieldRules[SF2ClassName[Sfield]])=!=Indices, Message[SuperfieldStrength::Abelian]; Abort[]];

(* Pre-simplification *)
   tmp=ApplySuperfieldDefinitions[Sfield];
   tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];

(* Covariant derivative with respect to the Grassmann variable theta_sp1*)
   tmp=DSUSY[tmp,alpha];
   tmp=Tonc[ToGrassmannBasis[tmp]];

(* Covariant derivatives with respect to the Grassmann variable thetabar_sp1dot*)
   tmp=DSUSYBar[tmp,sp1dot];
   tmp=Tonc[ToGrassmannBasis[tmp]];

(* Covariant derivatives with respect to the Grassmann variable thetabar_sp2dot*)
   tmp=DSUSYBar[tmp,sp2dot]*Ueps[sp2dot,sp1dot];

(* Final simplifications *)
   $OptIndex+=100;
   tmp=Tonc[ToGrassmannBasis[tmp]];
   tmp = Expand[-tmp/4];
tmp];


(* ::Subsubsection::Closed:: *)
(*SuperfieldStrengthL[ superfield, spin index, adjoint gauge index ]*)


SuperfieldStrengthL[SFstr_String,alpha_,AdjI_]:=Module[{Sfield=Symbol[SFstr],iii,jjj, GaugeOper, GaugeInd},
(* Get the adjoint operator checks the non-abelianity *)
  Do[
    Module[{GrName=MR$GaugeGroupList[[ll]],GrRep},
      If[(Superfield/.MR$GaugeGroupRules[GrName])===Sfield, 
          If[Abelian/.MR$GaugeGroupRules[GrName], Message[SuperfieldStrength::NonAbelian]; Abort[]]; 
        GrRep=GroupToReps[GrName];
        GaugeInd=GroupToAdj[GrName];
        GaugeOper=GrRep[[Position[GrRep[[All,2]],GaugeInd][[1,1]],1]]]],
  {ll,1,Length[MR$GaugeGroupList]}]; Clear[ll];
(* Calculate the Waij *)
  Expand[SuperfieldStrengthL[SFstr,alpha,iii,jjj]*GaugeOper[Index[GaugeInd,AdjI],Index[GaugeInd,jjj],Index[GaugeInd,iii]]]/.{
    fff_ GaugeOper[Index[GaugeInd,indx_],kk_,ll_] GaugeOper[Index[GaugeInd,AdjI],ll_,kk_]:>ReplaceAll[fff,indx->AdjI]}];


(* ::Subsubsection::Closed:: *)
(*SuperfieldStrengthL[ superfield, spin index, gauge index, gauge index ]*)


(* ::Text:: *)
(*SuperfieldStrengthL [ superfield, lower spin index, gauge index, gauge index] transform a (scalar) vectorial superfield to the associated left-handed spinorial superfield for a non-abelian gauge group. *)


SuperfieldStrengthL[SFstr_String,alpha_,GaugeI_,GaugeJ_]:=Module[{Sfield, coupl,GaugeInd,GaugeOper,Fstr,Dstr,dname, tmp,tmp2, GaugeK,dum,sp1dot,sp2dot,ll},
  Sfield=Symbol[SFstr];
(* Get the coupling constant and checking the non-abelianity *)
  Do[
    Module[{GrName=MR$GaugeGroupList[[ll]],GrRep},
      If[(Superfield/.MR$GaugeGroupRules[GrName])===Sfield, 
          If[Abelian/.MR$GaugeGroupRules[GrName], Message[SuperfieldStrength::NonAbelian]; Abort[]]; 
        dname=GrName;
        coupl=GroupToCoup[GrName];
        GrRep=GroupToReps[GrName];If[VectorQ[GrRep],GrRep=List[GrRep]];
        GaugeInd=GroupToAdj[GrName];
        GaugeOper=GrRep[[Position[GrRep[[All,2]],GaugeInd][[1,1]],1]]]],
  {ll,1,Length[MR$GaugeGroupList]}]; Clear[ll];

(*Exponential Exp[-2 g V] _ {kj} including the gauge indices *)
  tmp=nc[SFExp[-2*coupl*Sfield]];
  tmp=(tmp-1+IndexDelta[Index[GaugeInd,GaugeK],Index[GaugeInd,GaugeJ]])//.nc[Power[sf_?(VectorSuperfieldQ[#]===True&),n_]]->nc[sf,Power[sf,n-1]];
  tmp=ReplaceAll[tmp,{
    nc[ff___,Sfield,Sfield,hh___]:>Module[{GaugeL},nc[ff,Sfield[GaugeK,GaugeL],Sfield[GaugeL,GaugeJ],hh]],
    nc[ff___,Sfield,gg___] :> nc[ff,Sfield[GaugeK,GaugeJ],gg]}];
  tmp=ReplaceRepeated[tmp,{
    Sfield[a_,b_]:>Module[{GaugeM},Sfield[Index[GaugeInd,GaugeM]]*GaugeOper[Index[GaugeInd,GaugeM],Index[GaugeInd,a],Index[GaugeInd,b]]],
    nc[ff___,GaugeOper[inds__],gg___]->GaugeOper[inds] nc[ff,gg]}];

(* Superfield definition + simplification of the Grassmann variables combinations *)
   tmp=ApplySuperfieldDefinitions[tmp];
   tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
   $OptIndex+=1000;
   tmp=Tonc[ToGrassmannBasis[tmp]];

(*Covariant derivative with respect to the Grassmann variable theta_alpha*)
   tmp=DSUSY[tmp,alpha];
   tmp=Tonc[ToGrassmannBasis[tmp]];

(*Exponential Exp[+2 g V]_ {ik}*)
  tmp2=nc[SFExp[2*coupl*Sfield]];
  tmp2=(tmp2-1+IndexDelta[Index[GaugeInd,GaugeI],Index[GaugeInd,GaugeK]])//.nc[Power[sf_?(VectorSuperfieldQ[#]===True&),n_]]->nc[sf,Power[sf,n-1]];
  tmp2=ReplaceAll[tmp2,{
    nc[ff___,Sfield,Sfield,hh___]:>Module[{GaugeL},nc[ff,Sfield[GaugeI,GaugeL],Sfield[GaugeL,GaugeK],hh]],
    nc[ff___,Sfield,gg___] :> nc[ff,Sfield[GaugeI,GaugeK],gg]}];
  tmp2=ReplaceRepeated[tmp2,{
    Sfield[a_,b_]:>Module[{GaugeM},Sfield[Index[GaugeInd,GaugeM]]*GaugeOper[Index[GaugeInd,GaugeM],Index[GaugeInd,a],Index[GaugeInd,b]]],
    nc[ff___,GaugeOper[inds__],gg___]->GaugeOper[inds] nc[ff,gg]}];

(*Apply the superfield definitions to the exponential and simplification*)
  tmp2=ApplySuperfieldDefinitions[tmp2];
  tmp2=Expand[ReplaceRepeated[tmp2,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
  $OptIndex+=1000;
  tmp2=Tonc[ToGrassmannBasis[tmp2]];

(* (Exp[2 g V] D_alpha Exp[-2g V])_ {ij}*)
  tmp=nc[tmp2,tmp];
  tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
  $OptIndex-=2000;
  tmp=Tonc[ToGrassmannBasis[tmp]];
  tmp=Expand[tmp]/.{
    IndexDelta[ii_,jj_] GaugeOper[cc_,jj_,kk_]:> GaugeOper[cc,ii,kk],
    IndexDelta[ii_,jj_] GaugeOper[cc_,ii_,kk_]:> GaugeOper[cc,jj,kk],
    IndexDelta[ii_,jj_] GaugeOper[cc_,kk_,jj_]:> GaugeOper[cc,kk,ii],
    IndexDelta[ii_,jj_] GaugeOper[cc_,kk_,ii_]:> GaugeOper[cc,kk,jj]};

(*Covariant derivatives with respect to the Grassmann variable thetabar_sp1dot *)
  tmp=DSUSYBar[tmp,sp1dot];
  tmp=Tonc[ToGrassmannBasis[tmp]];

(*Covariant derivatives with respect to the Grassmann variable thetabar_sp1dot *)
  tmp=DSUSYBar[tmp,sp2dot]*Ueps[sp2dot,sp1dot];

(* Final simplifications *)
  $OptIndex+=100;
  tmp=ToGrassmannBasis[tmp];
  tmp=Expand[-tmp/4];

(* Lie algebra *)
  tmp=Expand[tmp/.{GaugeOper[aa_,b_,c_]GaugeOper[bb_,c_,d_]:> Module[{eee}, 
    I/2 Fstr[aa,bb,Index[GaugeInd,eee]] GaugeOper[Index[GaugeInd,eee],b,d] +
    1/2 Dstr[aa,bb,Index[GaugeInd,eee]] GaugeOper[Index[GaugeInd,eee],b,d] + 
    IndexDelta[aa,bb] IndexDelta[b,d]/(2*Sqrt[Length[IndexRange[Index[GaugeInd]]/.{NoUnfold[ar_]->ar,Unfold[ar_]->ar}]+1])]}];
  tmp=OptimizeIndex[tmp,{Fstr,Dstr}]/.{Index[Spin1,b_]->b,Index[Spin2,b_]->b, Index[Lorentz,b_]->b};
  SetAttributes[Dstr,Orderless];
  tmp=ReorderIndices[tmp,{Fstr,Dstr},GaugeInd]/.{Eps[a_,b_,c_,d_]:>(Signature[{a,b,c,d}] Eps@@Sort[{a,b,c,d}])};
  tmp=tmp/.Dstr[a_,b_,c_]*nc[TensDot2[si[mu_,sp1_,spd_],sibar[nu_,spd_,sp2_],we_[sp2_,ind3___]][down,Left,sp1_]]*fi_[mu_,b_]*fi_[nu_,c_]->
    Dstr[a,b,c]*nc[we[sp1,ind3]]*fi[mu,b]*fi[mu,c];
  tmp=tmp/.nc[TensDot2[si[mu_,sp1_,spd_],sibar[nu_,spd_,sp2_],we_[sp2_,ind3___]][down,Left,sp1_]]*fi_[mu_,b_]*fi_[nu_,b_]->nc[we[sp1,ind3]]*fi[mu,b]*fi[mu,b];
  tmp=tmp/.Fstr[a_,b_,c_] fi_[mu_,a_] fi_[mu_,b_]->0;
Tonc[tmp]/.Fstr->(StructureConstant/.MR$GaugeGroupRules[dname])];


(* ::Subsubsection::Closed:: *)
(*SuperfieldStrengthR[ superfield , lower spin index  ]*)


(* ::Text:: *)
(*SuperfieldStrengthR [ superfield,  lower spin index ] transform a (scalar) vectorial superfield to the associated right-handed spinorial superfield for an abelian group*)


SuperfieldStrengthR[SFstr_String,alphadot_]:=Module[{Sfield, tmp,sp1,sp2},
  Sfield=Symbol[SFstr];

(* Check the abelianity *)
  If[(Indices/.M$SuperfieldRules[SF2ClassName[Sfield]])=!=Indices, Message[SuperfieldStrength::Abelian]; Abort[]];

(* Pre-simplification *)
   tmp=ApplySuperfieldDefinitions[Sfield];
   tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];

(* Covariant derivative with respect to the Grassmann variable theta_sp1*)
   tmp=DSUSYBar[tmp,alphadot];
   tmp=Tonc[ToGrassmannBasis[tmp]];

(* Covariant derivatives with respect to the Grassmann variable theta_sp1*)
   tmp=DSUSY[tmp,sp1];
   tmp=Tonc[ToGrassmannBasis[tmp]];

(* Covariant derivatives with respect to the Grassmann variable theta_sp2*)
   tmp=DSUSY[tmp,sp2]*Ueps[sp1,sp2];

(* Final simplifications *)
   $OptIndex+=100;
   tmp=Tonc[ToGrassmannBasis[tmp]];
   tmp = Expand[FR$Sugra*tmp/4];
tmp];


(* ::Subsubsection::Closed:: *)
(*SuperfieldStrengthR[ superfield, spin index, adjoint gauge index ]*)


SuperfieldStrengthR[SFstr_String,alpha_,AdjI_]:=Module[{Sfield=Symbol[SFstr],iii,jjj, GaugeOper, GaugeInd},
(* Get the adjoint operator checks the non-abelianity *)
  Do[
    Module[{GrName=MR$GaugeGroupList[[ll]],GrRep},
      If[(Superfield/.MR$GaugeGroupRules[GrName])===Sfield, 
          If[Abelian/.MR$GaugeGroupRules[GrName], Message[SuperfieldStrength::NonAbelian]; Abort[]]; 
        GrRep=GroupToReps[GrName];
        GaugeInd=GroupToAdj[GrName];
        GaugeOper=GrRep[[Position[GrRep[[All,2]],GaugeInd][[1,1]],1]]]],
  {ll,1,Length[MR$GaugeGroupList]}]; Clear[ll];
(* Calculate the Waij *)
  Expand[SuperfieldStrengthR[SFstr,alpha,iii,jjj]*GaugeOper[Index[GaugeInd,AdjI],Index[GaugeInd,jjj],Index[GaugeInd,iii]]]/.{
    fff_ GaugeOper[Index[GaugeInd,indx_],kk_,ll_] GaugeOper[Index[GaugeInd,AdjI],ll_,kk_]:>ReplaceAll[fff,indx->AdjI]}];


(* ::Subsubsection::Closed:: *)
(*SuperfieldStrengthR[superfield , spin index, gauge index, gauge index]*)


(* ::Text:: *)
(*SuperfieldStrengthR[ superfield, lower spin index, gauge index, gauge index] transform a (scalar) vectorial superfield to the associated right - handed spinorial superfield for a non - abelian gauge group.*)


SuperfieldStrengthR[SFstr_String,alphadot_,GaugeI_,GaugeJ_]:=Module[{Sfield=Symbol[SFstr], coupl,GaugeInd,GaugeOper,Fstr,Dstr,dname, tmp,tmp2, GaugeK,dum, sp1,sp2,ll},
(* Get the coupling constant and checking the non-abelianity *)
  Do[
    Module[{GrName=MR$GaugeGroupList[[ll]],GrRep},
      If[(Superfield/.MR$GaugeGroupRules[GrName])===Sfield, 
          If[Abelian/.MR$GaugeGroupRules[GrName], Message[SuperfieldStrength::NonAbelian]; Abort[]]; 
        dname=GrName;
        coupl=CouplingConstant/.MR$GaugeGroupRules[GrName];
        GrRep=Representations/.MR$GaugeGroupRules[GrName];If[VectorQ[GrRep],GrRep=List[GrRep]];
        GaugeInd=(Indices/.M$SuperfieldRules[SF2ClassName[Sfield]])[[1,1]];
        GaugeOper=GrRep[[Position[GrRep[[All,2]],GaugeInd][[1,1]],1]]]],
  {ll,1,Length[MR$GaugeGroupList]}]; Clear[ll];

(*Exponential Exp[2 g V] _ {kj} including the gauge indices *)
  tmp=nc[SFExp[2*coupl*Sfield]];
  tmp=(tmp-1+IndexDelta[Index[GaugeInd,GaugeK],Index[GaugeInd,GaugeJ]])//.nc[Power[sf_?(VectorSuperfieldQ[#]===True&),n_]]->nc[sf,Power[sf,n-1]];
  tmp=ReplaceAll[tmp,{
    nc[ff___,Sfield,Sfield,hh___]:>Module[{GaugeL},nc[ff,Sfield[GaugeK,GaugeL],Sfield[GaugeL,GaugeJ],hh]],
    nc[ff___,Sfield,gg___] :> nc[ff,Sfield[GaugeK,GaugeJ],gg]}];
  tmp=ReplaceRepeated[tmp,{
    Sfield[a_,b_]:>Module[{GaugeM},Sfield[Index[GaugeInd,GaugeM]]*GaugeOper[Index[GaugeInd,GaugeM],Index[GaugeInd,a],Index[GaugeInd,b]]],
    nc[ff___,GaugeOper[inds__],gg___]->GaugeOper[inds] nc[ff,gg]}];

(* Superfield definitions + simplification of the Grassmann variables combinations *)
   tmp=ApplySuperfieldDefinitions[tmp];
   tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
   $OptIndex+=1000;
   tmp=Tonc[ToGrassmannBasis[tmp]];

(*Covariant derivative with respect to the Grassmann variable thetabar_alphadot*)
   tmp=DSUSYBar[tmp,alphadot];
   tmp=Tonc[ToGrassmannBasis[tmp]];

(*Exponential Exp[-2 g V]_ {ik} *)
  tmp2=nc[SFExp[-2*coupl*Sfield]];
  tmp2=(tmp2-1+IndexDelta[Index[GaugeInd,GaugeI],Index[GaugeInd,GaugeK]])//.nc[Power[sf_?(VectorSuperfieldQ[#]===True&),n_]]->nc[sf,Power[sf,n-1]];
  tmp2=ReplaceAll[tmp2,{
    nc[ff___,Sfield,Sfield,hh___]:>Module[{GaugeL},nc[ff,Sfield[GaugeI,GaugeL],Sfield[GaugeL,GaugeK],hh]],
    nc[ff___,Sfield,gg___] :> nc[ff,Sfield[GaugeI,GaugeK],gg]}];
  tmp2=ReplaceRepeated[tmp2,{
    Sfield[a_,b_]:>Module[{GaugeM},Sfield[Index[GaugeInd,GaugeM]]*GaugeOper[Index[GaugeInd,GaugeM],Index[GaugeInd,a],Index[GaugeInd,b]]],
    nc[ff___,GaugeOper[inds__],gg___]->GaugeOper[inds] nc[ff,gg]}];

(*Apply the superfield definitions to the exponential and simplification*)
  tmp2=ApplySuperfieldDefinitions[tmp2];
  tmp2=Expand[ReplaceRepeated[tmp2,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
  $OptIndex+=1000;
  tmp2=Tonc[ToGrassmannBasis[tmp2]];

(*Exp[2 g v] D_alpha Exp[-2g V] _ {ij}*)
  tmp=nc[tmp2,tmp];
  tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
  $OptIndex-=2000;
  tmp=Tonc[ToGrassmannBasis[tmp]];
  tmp=Expand[tmp]/.{
    IndexDelta[ii_,jj_] GaugeOper[cc_,jj_,kk_]:> GaugeOper[cc,ii,kk],
    IndexDelta[ii_,jj_] GaugeOper[cc_,ii_,kk_]:> GaugeOper[cc,jj,kk],
    IndexDelta[ii_,jj_] GaugeOper[cc_,kk_,jj_]:> GaugeOper[cc,kk,ii],
    IndexDelta[ii_,jj_] GaugeOper[cc_,kk_,ii_]:> GaugeOper[cc,kk,jj]};

(*Covariant derivatives with respect to the Grassmann variable theta_sp1 + optimization*)
  tmp=DSUSY[tmp,sp1];
  tmp=Tonc[ToGrassmannBasis[tmp]];

(*Covariant derivatives with respect to the Grassmann variable theta_sp2 *)
  tmp=DSUSY[tmp,sp2]*Ueps[sp1,sp2];

(* Final simplifications *)
   $OptIndex+=100;
   tmp=ToGrassmannBasis[tmp];
   tmp=Expand[-tmp/4];

(* Lie algebra *)
  tmp=Expand[FR$Sugra*tmp/.{GaugeOper[aa_,b_,c_]GaugeOper[bb_,c_,d_]:> Module[{eee}, 
    I/2 Fstr[aa,bb,Index[GaugeInd,eee]] GaugeOper[Index[GaugeInd,eee],b,d] +
    1/2 Dstr[aa,bb,Index[GaugeInd,eee]] GaugeOper[Index[GaugeInd,eee],b,d] + 
    IndexDelta[aa,bb] IndexDelta[b,d]/(2*Sqrt[Length[IndexRange[Index[GaugeInd]]/.{NoUnfold[ar_]->ar,Unfold[ar_]->ar}]+1])]}];
  tmp=OptimizeIndex[tmp,{Fstr,Dstr}]/.{Index[Spin1,b_]->b,Index[Spin2,b_]->b, Index[Lorentz,b_]->b};
  SetAttributes[Dstr,Orderless];
  tmp=ReorderIndices[tmp,{Fstr,Dstr},GaugeInd]/.{Eps[a_,b_,c_,d_]:>(Signature[{a,b,c,d}] Eps@@Sort[{a,b,c,d}])};
  tmp=tmp/.Dstr[a_,b_,c_]*nc[TensDot2[we_[sp1_,ind3___],sibar[mu_,sp1_,spd_],si[nu_,spd_,sp2_]][down,Right,sp2_]]*fi_[mu_,b_]*fi_[nu_,c_]->
    Dstr[a,b,c]*nc[TensDot2[we[sp2,ind3]][down,Right,sp2]]*fi[mu,b]*fi[mu,c];
  tmp=tmp/.nc[TensDot2[we_[sp1_,ind3___],sibar[mu_,sp1_,spd_],si[nu_,spd_,sp2_]][down,Right,sp2_]]*fi_[mu_,b_]*fi_[nu_,b_]->
    nc[TensDot2[we[sp2,ind3]][down,Right,sp2]]*fi[mu,b]*fi[mu,b];
  tmp=tmp/.Fstr[a_,b_,c_] fi_[mu_,a_] fi_[mu_,b_]->0;
Tonc[tmp]/.Fstr->(StructureConstant/.MR$GaugeGroupRules[dname])];


(* ::Section:: *)
(*SUSY transformations and superderivatives*)


(* ::Subsubsection::Closed:: *)
(*Superderivatives *)


(* ::Text:: *)
(*Derivatives with respect to the Grassmann variable theta and thetabar*)


Dth[thetabar[_]]=0;
Dth[f_?(!MatchQ[#,theta]&)[__]]:=0;
Dth[f_?(!(FieldQ[#]===True)&)]:=0;

Dthbar[theta[_]]=0;
Dthbar[f_?(!MatchQ[#,thetabar]&)[__]]:=0;
Dthbar[f_?(!(FieldQ[#]===True)&)]:=0;

del[theta[_],_] = 0;
del[thetabar[_],_] = 0;
del[nc[ff__],mu_]:=Module[{ii},Plus@@(Table[nc[Sequence@@(MapAt[del[#,mu]&,{ff},ii])],{ii,Length[{ff}]}])];

ddel[nc[ff__],mu_]:=Module[{ii},Plus@@(Table[nc[Sequence@@(MapAt[ddel[#,mu]&,{ff},ii])],{ii,Length[{ff}]}])];
ddel[Plus[a_,b__],mu_]:=ddel[a,mu] + ddel[Plus[b],mu];
ddel[Times[ff___,si[mu_,a_,b_],gg___],nu_]:=si[mu,a,b] ddel[Times[ff,gg],nu];
ddel[Times[ff___,Ueps[a_,b_],gg___],nu_]:=Ueps[a,b] ddel[Times[ff,gg],nu];
ddel[Times[ff___,nc[gg__],hh___,mu_]] := ddel[Times[ff,hh],mu] + Times[ff,hh] ddel[nc[gg],mu];


(* ::Text:: *)
(*Left-handed covariant derivative*)


DSUSY[exp_,alpha_]:=Module[{tmp, tmp1, tmp2, mu, sp1dot,sp2dot,dd},
(* Derivative with respect to the Grassmann variable *)
   tmp1=dd*nc[exp]/.nc[ff__]:>Module[{ii},Plus@@((Table[nc[Sequence@@(MapAt[(-1)^(ii+1) Dth,{ff},ii])],{ii,Length[{ff}]}])/.(-Dth)[i_]:>-Dth[i])]/dd;
   tmp1=Expand[tmp1]//.Plus[ff__]:>Plus@@((ReplaceRepeated[#,Dth[theta[sp_]]:>Deps[sp,alpha]])&/@{ff})/.dd->0;

(* I sigma thetbar term *)
   tmp2=-I si[mu,alpha,sp1dot]Ueps[sp1dot,sp2dot] nc[thetabar[sp2dot],ddel[exp,mu]];
   tmp2 = tmp2//.ddel->del;

   tmp=Expand[tmp1+FR$Sugra*tmp2];

(* Simplifications *)
   tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
   tmp=ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0];
   tmp=Expand[tmp]/.EDelta[a_,b_] nc[ff___,field_[b_,inds___],gg___] -> nc[ff,field[a,inds],gg];

tmp];


(* ::Text:: *)
(*Right-handed covariant derivative*)


DSUSYBar[exp_,alphadot_]:=Module[{tmp, tmp1, tmp2, mu, sp1,sp2,dd},

(* Derivative with respect to the Grassmann variable *)
   tmp1=dd*nc[exp] /.nc[ff__]:>Module[{ii},Plus@@((Table[nc[Sequence@@(MapAt[(-1)^(ii+1) Dthbar,{ff},ii])],{ii,Length[{ff}]}])/.(-Dthbar)[i_]:>-Dthbar[i])]/dd;
   tmp1=Expand[tmp1]//.Plus[ff__]:>Plus@@((ReplaceRepeated[#,Dthbar[thetabar[sp_]]:>FR$Sugra*Deps[sp,alphadot]])&/@{ff})/.dd->0;

(* I theta sigma term *)
   tmp2=-I si[mu,sp1,alphadot]Ueps[sp1,sp2] nc[theta[sp2],ddel[exp,mu]];
   tmp2=tmp2//.ddel->del;

   tmp=Expand[tmp1+tmp2];

(* Simplifcations *)
   tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
   tmp=ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0];
   tmp=Expand[tmp]/.EDelta[a_,b_] nc[ff___,field_[b_,inds___],gg___] -> nc[ff,field[a,inds],gg];

tmp];


(* ::Subsubsection::Closed:: *)
(*Supersymmetric transformations*)


QSUSY[exp_,alpha_]:=Module[{tmp,tmp1,tmp2,mu,sp1dot,sp2dot,dd},

(*-I*Derivative with respect to the Grassmann variable*)
  tmp1=-I*dd*nc[exp]/.nc[ff__]:>Module[{ii},Plus@@((Table[nc[Sequence@@(MapAt[(-1)^(ii+1) Dth,{ff},ii])],{ii,Length[{ff}]}])/.(-Dth)[i_]:>-Dth[i])]/dd;
  tmp1=Expand[tmp1]//.Plus[ff__]:>Plus@@((ReplaceRepeated[#,Dth[theta[sp_]]:>Deps[sp,alpha]])&/@{ff})/.dd->0;

(*sigma thetbar term*)
  tmp2= si[mu,alpha,sp1dot]Ueps[sp1dot,sp2dot] nc[thetabar[sp2dot],ddel[exp,mu]];
  tmp2=tmp2//.ddel->del;

  tmp=Expand[tmp1+FR$Sugra*tmp2];

(* Simplifcations *)
  tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
  tmp=ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0];
  tmp=Expand[tmp]/.EDelta[a_,b_] nc[ff___,field_[b_,inds___],gg___] -> nc[ff,field[a,inds],gg];

(* sugra case *)
  If[FR$Sugra===-1,tmp = Expand[I*tmp]];

tmp];


QSUSYBar[exp_,alphadot_]:=Module[{tmp,tmp1,tmp2,mu,sp1,sp2,dd},

(*I*Derivative with respect to the Grassmann variable*)
  tmp1=I*dd*nc[exp]/.nc[ff__]:>Module[{ii},Plus@@((Table[nc[Sequence@@(MapAt[(-1)^(ii+1) Dthbar,{ff},ii])],{ii,Length[{ff}]}])/.(-Dthbar)[i_]:>-Dthbar[i])]/dd;
  tmp1=Expand[tmp1]//.Plus[ff__]:>Plus@@((ReplaceRepeated[#,Dthbar[thetabar[sp_]]:>FR$Sugra*Deps[sp,alphadot]])&/@{ff})/.dd->0;

(*-theta sigma term*)
  tmp2=- si[mu,sp1,alphadot]Ueps[sp1,sp2] nc[theta[sp2],ddel[exp,mu]];
  tmp2=tmp2//.ddel->del;

  tmp=Expand[tmp1+tmp2];

(* Simplifcations *)
  tmp=Expand[ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0]];
  tmp=ReplaceRepeated[tmp,nc[___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___,a_?(FermionQ[#]===True&)[__],___]->0];
  tmp=Expand[tmp]/.EDelta[a_,b_] nc[ff___,field_[b_,inds___],gg___] -> nc[ff,field[a,inds],gg];

(* sugra case *)
  If[FR$Sugra===-1,tmp = Expand[-I*tmp]];

tmp];


DeltaSUSY [exp_, epsi_] := Module[{tmp,sp1,sp2},
(* tests *)
  If[Not[WeylFieldQ[epsi]] || MatchQ[epsi,_[__]] || Chirality[epsi]=!=Left, Message[DeltaSUSY::Arguments]; Abort[]];
(* if successfull -> calculate the SUSY transform *)
  tmp = Tonc[ GrassmannExpand[ Expand[exp] ] ];
  tmp=I nc[QSUSY[tmp,sp1],epsi[sp2]] Ueps[sp2,sp1]+I nc[HC[epsi[sp2]],QSUSYBar[tmp,sp1]] Ueps[sp2,sp1];
ToGrassmannBasis[Expand[tmp]]];


(* ::Section:: *)
(*Chiral superfield kinetic terms (Kaehler potential)*)


(* ::Subsubsection::Closed:: *)
(*Main routine : loop over chiral superfields*)


(* ::Text:: *)
(*Kins[ ] writes down automatically the simplest Kaehler potential for all the chiral superfield of the model*)


CSFKineticTerms[]:=Plus@@(CSFKineticTerms[#]&/@ M$ChiralSuperfieldNames);


(* ::Subsubsection::Closed:: *)
(*Main routine : get automatically the simplest Kaehler potential for one chiralfield*)


(* ::Text:: *)
(*CSFKineticTerms[ superfield ] gets automatically the kinetic term HC[Phi] Exp[-2 g V] Phi (summing over all gauge groups)*)


CSFKineticTerms[SFS_String]:=Module[{iSF,IndTypesf, tmp, dum, sf},
(* useful stuff *)
  sf=Symbol[SFS];
  IndTypesf=$IndList[sf]/.Index[a_]->a;

(* Generate the exponential for each gauge group *)
  tmp=SFExp/@(-2 dum[#](CouplingConstant/.MR$GaugeGroupRules[#])*(Superfield/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList);
  tmp=ReplaceAll[tmp,exp_?(VectorSuperfieldQ[#]===True&):>exp[Sequence@@(Indices/.M$SuperfieldRules[exp])]];
  tmp=tmp//.{
    a_[Indices]:>a,
    a_[Index[xx_]]dum[name_]:>Module[{ii},a[ii]*dum[ii,name]],
    a_[Index[xx_]]^2dum[name_]^2:>Module[{ii,jj},a[ii]*dum[ii,name]a[jj]*dum[jj,name]]};

(* Get the right charge for the Abelian groups *)
  tmp=tmp/.{
    dum[name_?(Abelian/.MR$GaugeGroupRules[#]&)]^2:>(Charge/.MR$GaugeGroupRules[name]) @sf^2,
    dum[name_?(Abelian/.MR$GaugeGroupRules[#]&)]:>(Charge/.MR$GaugeGroupRules[name]) @sf};

(* Get the right operator for the non-abelian groups *)
  tmp=tmp/.dum[indx_,name_?(!Abelian/.MR$GaugeGroupRules[#]&)]:> Module[{indrepr,repr,  oper},
    repr=Representations/.MR$GaugeGroupRules[name];
    If[VectorQ[repr],indrepr={repr[[2]]};repr={repr[[1]]},indrepr=repr[[All,2]];repr=repr[[All,1]]];
    oper=If[Intersection[IndTypesf,indrepr]==={},0,repr[[Position[indrepr,Sequence@@Intersection[IndTypesf,indrepr]][[1,1]]]]];
  (dum[indx,Intersection[IndTypesf,indrepr]] oper[indx])/.{0[_]->0,dum[indxx_,{bla__}]->dum[indxx,bla]}];

(* Insert indices *)
  iSF=sf[$IndList[sf]]/.{Index[a_]:>Module[{iii},Index[a,iii]],ff_[{}]->ff,List->Sequence};
  tmp=Expand[HC[iSF] iSF Times@@tmp];
  tmp=tmp//.dum[ii_,indx_] op_?(Not[SuperfieldQ[#]===True]&)[ii_] sf[indb___,Index[indx_,iii_],inda___]:>
    Module[{kkk},op[ii,iii,kkk] sf[indb,Index[indx,kkk],inda]];

(* Simplifications: removal of the terms with 3 or more Vector superfields *)
  tmp=Tonc[tmp]//.{
    vsf_?(VectorSuperfieldQ[#]===True&)^2->Sequence[vsf,vsf],
    nc[___,vsf1_?(VectorSuperfieldQ[#]===True&),___,vsf2_?(VectorSuperfieldQ[#]===True&),___,vsf3_?(VectorSuperfieldQ[#]===True&),___]->0,
    nc[___,vsf1_?(VectorSuperfieldQ[#]===True&),___,vsf2_?(VectorSuperfieldQ[#]===True&)[__],___,vsf3_?(VectorSuperfieldQ[#]===True&)[__],___]->0,
    nc[___,vsf1_?(VectorSuperfieldQ[#]===True&)[__],___,vsf2_?(VectorSuperfieldQ[#]===True&),___,vsf3_?(VectorSuperfieldQ[#]===True&)[__],___]->0,
    nc[___,vsf1_?(VectorSuperfieldQ[#]===True&)[__],___,vsf2_?(VectorSuperfieldQ[#]===True&)[__],___,vsf3_?(VectorSuperfieldQ[#]===True&),___]->0,
    nc[___,vsf1_?(VectorSuperfieldQ[#]===True&)[__],___,vsf2_?(VectorSuperfieldQ[#]===True&),___,vsf3_?(VectorSuperfieldQ[#]===True&),___]->0,
    nc[___,vsf1_?(VectorSuperfieldQ[#]===True&),___,vsf2_?(VectorSuperfieldQ[#]===True&)[__],___,vsf3_?(VectorSuperfieldQ[#]===True&),___]->0,
    nc[___,vsf1_?(VectorSuperfieldQ[#]===True&),___,vsf2_?(VectorSuperfieldQ[#]===True&),___,vsf3_?(VectorSuperfieldQ[#]===True&)[__],___]->0,
    Index[Type_,b_]->b};
tmp];


(* ::Section:: *)
(*Vector superfield kinetic terms*)


(* ::Subsubsection::Closed:: *)
(*Shortcut function*)


VSFKineticTerms[sf_]:=Module[{tmpgrp},
   tmpgrp=Flatten[DeleteCases[((List[Superfield,Abelian,CouplingConstant,#]/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList), List[sff_?(#=!=sf&),__]]];
   If[tmpgrp[[2]], VSFKineticTerms[sf,True]/4, Module[{ii},VSFKineticTerms[sf,False]]/(16 tmpgrp[[3]]^2)]
];


(* ::Subsubsection::Closed:: *)
(*Generation of the contracted superfield strength tensors*)


VSFKineticTerms[]:=Plus@@(VSFKineticTerms/@((Superfield/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList));


(* ::Subsubsection:: *)
(*Lie algebra simplifications*)


(* ::Text:: *)
(*SimplifyLieAgebra [ expression, group name ] uses the properties of the Lie algebra to simplify the product of the representation matrices.*)


SimplifyLieAlgebra[exp_, GrName_] := Module[{tmp, AdjOper,GrRep,ID,struc,t1,t2},
(* Structure constants and adjoint representations *)
  GrRep=GroupToReps[GrName];If[VectorQ[GrRep],GrRep=List[GrRep]];
  AdjOper=GrRep[[Position[GrRep[[All,2]],GroupToAdj[GrName]][[1,1]],1]];
  struc=GroupToStrucConst[GrName];

(* Collect traces of adjoint representation matrices  *)
  tmp=exp//.{AdjOper[aa_,ii_,jj_]AdjOper[bb_,jj_,ii_]:> (GroupToAdj[GrName]/.GroupToDynkins[GrName]ID[aa,bb]), AdjOper[_,ii_,ii_]->0};
  tmp=tmp/.ID[a_,b_] sub_ :> ReplaceAll[sub,a->b] /;a=!=b;
  tmp=ReorderIndices[tmp,{struc,Null},GroupToAdj[GrName]]/.{
    Eps[a_,b_,c_,d_]:>(Signature[{a,b,c,d}] Eps@@Sort[{a,b,c,d}])};

(* Further simplifcations,if required *)
   t1=Expand[tmp-(tmp/.struc[__]->0)];
   t2=Expand[tmp-t1];
   t1=t1/.struc[a_,b_,c_] fi_[mu_,a_] fi_[mu_,b_]->0;
   t1=t1/.struc[a_,b_,c_]:>Signature[{a,b,c}] struc@@Sort[{a,b,c}]/;Not[OrderedQ[{a,b,c}]];
   t1=t1/.{
     struc[a_,b_,e_]struc[c_,d_,e_]Eps[_,_,_,_] fi_[mu_,a_] fi_[nu_,b_]fi_[ro_,c_] fi_[si_,d_]->0,
     struc[e_,a_,b_]struc[e_,c_,d_]Eps[_,_,_,_] fi_[mu_,a_] fi_[nu_,b_]fi_[ro_,c_] fi_[si_,d_]->0};
OptimizeIndex[t1+t2]/.Index[Type_,a_]->a];


(* ::Subsubsection:: *)
(*Get the fields present in an expression*)


ToFieldIndexList[exp_]:=Module[{res=Expand[exp],ll},
(* Save the ordering of the expression and remove objects without indices *)
  res=If[(Length[res]=!=1) && (Length[res]=!=0),(List@@res)//.{nc->Sequence, Dot->Sequence, TensDot2[ff__][opt__]->Sequence[ff]}, List[res]];
  res=res/.Power[a_,n_? (IntegerQ[#]&)] :> Sequence@@Table[a,{Abs[n]}];
  res=res//.{HC[aa_]->aa,Conjugate[aa_]->aa,del[x_,mu_]->Sequence[Index[Lorentz,mu],x]};
  res=DeleteCases[If[MatchQ[#,_[__]]&&Head[#]=!= Complex,#,{}]&/@res,{}];
  res=DeleteCases[If[NumericQ[#],{},#]&/@res,{}];

(* Restore index types *)
  res = res/. {
    f_?(Spin32WeylFieldQ[#]===True &)[sp_,muu_]:> {},
    f_?(Spin32WeylFieldQ[#]===True &)[sp_,muu_,inds__]:> Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]],
    f_?(FermionQ[#]===True &)[sp_]->{},
    f_?(FermionQ[#]===True &)[sp_,inds__]:> Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]],
    f_?(VectorFieldQ[#]===True&)[mu_]->Index[Lorentz,mu],
    f_?(VectorFieldQ[#]===True&)[mu_,inds__]:> Sequence[Index[Lorentz,mu],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(ScalarFieldQ[#]===True&)[inds__]:>Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]],
    f_?(ScalarFieldQ[#]===True&)->{},
    si[mu_,sp_,spd_]-> {},sibar[mu_,spd_,sp_]-> {}, Ueps[a_,b_]->{}, Deps[a_,b_]->{}, ME[mu_,nu_]->{}, Eps[argx__]->{},
    f_?(MemberQ[MR$ParameterList,#]===True&)[inds__]:>{}
 };
  Do[
    Module[{GrName, GrRepList, GrIndList,GrAdj,GrRep,GrStr},
      GrName=MR$GaugeGroupList[[ll]];
      If[!(Abelian/.MR$GaugeGroupRules[GrName]),
        GrRep=Representations/.MR$GaugeGroupRules[GrName];
        GrStr=StructureConstant/.MR$GaugeGroupRules[GrName];
        If[VectorQ[GrRep],GrIndList={GrRep[[2]]};GrRepList={GrRep[[1]]},GrIndList=GrRep[[All,2]];GrRepList=GrRep[[All,1]]];
        res=res/.ff_?(MemberQ[GrRepList,#] &)[inda_,indx_,indy_]-> {};
        res=res/.GrStr[inda_,indx_,indy_]->{}]],
  {ll,1,Length[MR$GaugeGroupList]}]; Clear[ll];

(* result *)  
DeleteCases[res/.Index[type2_,Index[type2_,bb_]]->Index[type2,bb],{}]];


(* ::Subsubsection::Closed:: *)
(*Canonical field ordering*)


ReorderIndices[Plus[a_,b__],{Fstr_,Dstr_},AdjIndex_]:= Plus[ReorderIndices[a,{Fstr,Dstr},AdjIndex],ReorderIndices[Plus[b],{Fstr,Dstr},AdjIndex]];


ReorderIndices[exp_?(Head[#]=!=Plus&),{Fstr_,Dstr_},AdjIndex_]:=Module[{tmp,LorI,GauI,cnt=$OptIndex+1000,EX,myRule},
    tmp=DeleteCases[ToFieldIndexList[exp],Index[a_?((#=!=Lorentz) && (#=!=AdjIndex)&),_]];
    tmp=DeleteCases[tmp/.{Dstr[___]->List[],Fstr[___]->List[], Index[_,b_?(NumericQ[#]&)]->List[]},{}];

    EX=DeleteCases[(ToIndexList[exp]/.{Fstr[ind__]->ind,Dstr[ind__]->ind,Index[a_?((#=!=Lorentz) && (#=!=AdjIndex)&),_]->List[]}),{}]/.Index[_,b_]->b;
    EX=DeleteCases[Tally[EX],List[_,a_?(#=!=1&)]]/. List[a_,1]->a;

    LorI=Tally[DeleteCases[tmp,Index[AdjIndex,_]]/.Index[_,b_]->b]/.List[aa_,n_?(IntegerQ[#]&)]->aa;
    LorI=DeleteCases[(myRule[#,Symbol["mu$"<>ToString[cnt++]]]&/@ LorI),myRule[a_?(MemberQ[EX,#]&),_]]/.myRule:>Rule; cnt=$OptIndex+1000;

    GauI=Tally[DeleteCases[tmp,Index[Lorentz ,_]]/.Index[_,b_]->b]/.List[aa_,n_?(IntegerQ[#]&)]->aa;
    GauI=DeleteCases[(myRule[#,Symbol[ToString[AdjIndex]<>"$"<>ToString[cnt++]]]&/@GauI),myRule[a_?(MemberQ[EX,#]&),_]]/.myRule:>Rule;

    exp/.LorI/.GauI];


(* ::Section:: *)
(*Index naming optimization*)


(* ::Subsubsection:: *)
(*Main routine for index naming optimization*)


(* ::Text:: *)
(*OptimizeIndex [ expression, list ] is a function renaming the indices created in the various modules in order to have more compact expressions at the end of the game (collecting terms with the same structure).*)
(*The index numering is starting at the value of $OptIndex.*)
(*list is a list of temporary parameters which carry indices. *)


(*OptimizeIndex[Plus[exp1_,exp__],Param_List:{}]:=Plus[OptimizeIndex[exp1,Param],OptimizeIndex[Plus[exp],Param]];*)
(* CD: 07.02.14 : Need map in order to avoid recursion limit for large expressions *)
OptimizeIndex[sum_Plus,Param_List:{}]:=OptimizeIndex[#,Param]& /@ sum;


OptimizeIndex[exp_?(Head[#]=!=Plus&),Param_List:{}]:=Block[{tmpexp},
  tmpexp=If[FR$FExpand,Expand[exp],Expand[exp,Indices]]; (*indices added by celine*)
  If[Head[tmpexp]===Plus,OptimizeIndex[tmpexp,Param],OptimizeIndex2[tmpexp,Param]]];



OptimizeIndex2[exp_?(Head[#]=!=Plus&),Param_List:{}]:=Block[{bckp,OptRules,flavs,ll,cnt,tmp,II,EE,ff},
(*Initialization *)
  flavs=DeleteCases[MR$IndexList,MR$Null]/.Index[a_]->a;
  cnt=$OptIndex;
(* Get a list of indices *)
  tmp=PrePutIndices[exp]/.{Index[type_,I,num_]->Index[type,II,num],Index[type_,E,num_]->Index[type,EE,num],Index[type_,f,num_]->Index[type,ff,num]};
  tmp=tmp/.Index[type_,muf_,u_?IntegerQ]:>Index[type, Symbol[ToString[muf]<>ToString[u]]];
  bckp=ToIndexList[tmp];
  If[Param=!=Null, bckp=bckp/.(Rule[#[inds__],inds]&/@Param)];

(* Remove the non-summed indices *)
  bckp=DeleteCases[bckp/.(Rule[#,{}]&/@ReplaceAll[DeleteCases[Tally[bckp],{_,2}],{a_,1}->a]),{}];

(* Create the rules for index renaming *)
  bckp=ReplaceAll[DeleteCases[Tally[bckp],{_,1}],{a_,2}->a];
  OptRules={};
  OptRules=Append[OptRules,Rule[#,Symbol["sp$"<>ToString[cnt++]]]&/@ (Cases[bckp,Index[Spin1,_]])];cnt=$OptIndex;
  OptRules=Append[OptRules,Rule[#,Symbol["sp$"<>ToString[cnt++]<>"dot"]]&/@ (Cases[bckp,Index[Spin2,_]])];cnt=$OptIndex;
  OptRules=Append[OptRules,Rule[#,Symbol["SP$"<>ToString[cnt++]]]&/@ (Cases[bckp,Index[Spin,_]])];cnt=$OptIndex;
  OptRules=Append[OptRules,Rule[#,Symbol["mu$"<>ToString[cnt++]]]&/@ (Cases[bckp,Index[Lorentz,_]])];cnt=$OptIndex;
  Do[OptRules=Append[OptRules,Rule[#,Symbol[ToString[flavs[[ll]]]<>"$"<>ToString[cnt++]]]&/@(Cases[bckp,Index[flavs[[ll]],_]])];cnt=$OptIndex,{ll,Length[flavs]}];
(* CD: Bug fix 15.02.2013: 
   Old piece of code was:
  OptRules=Flatten[DeleteCases[OptRules,{}]]/.Index[_,aa_]->aa;
   The rule need sot be more general, to cover the cases where Index[ ] has 3 entries. 
   So, I change it to:

  OptRules=Flatten[DeleteCases[OptRules,{}]]/.Index[_,aa_,___]:>aa;*)
  OptRules=Flatten[DeleteCases[OptRules,{}]]/.Index[_,aa_]->aa;
(* End bug fix 15.02.2013 *)

  (* final simplications *)
  tmp = TreatUDeps[tmp];
  tmp = tmp/.Index[inds__]:>Index[Sequence@@ReplaceAll[{inds},OptRules]];
  tmp = tmp/.TensDot2[ff__][inds__]:>TensDot2[ff][Sequence@@ReplaceAll[{inds},OptRules]];
  tmp=tmp/.{Index[type_,Index[type_,bb__]]->Index[type,bb]};
  Return[tmp];
];


(* ::Subsection:: *)
(*Special routine for the Ueps and Eeps to get the nature of the spin indices*)


TreatUDeps[exp_]:=Block[{indices,rules},
  indices=Cases[ToIndexList[exp],Index[Spin1|Spin2,_]];
  indices=ReplaceAll[DeleteCases[Tally[indices],{_,1}],{a_,2}->a];
  rules=(MyRule[#[[2]],#]&/@indices)/.MyRule->Rule;
  Return[exp/.{Ueps[argx__]:>ReplaceAll[Ueps[argx],rules],Deps[argx__]:>ReplaceAll[Deps[argx],rules]}];
];


(* ::Subsubsection:: *)
(*Transform an expression in a list of the indices appearing inside, keeping the same order*)


(* ::Text:: *)
(*ToIndexList[ expression ] transforms an expression, keeping only the indices appearing inside*)


ToIndexList[exp_]:=Block[{res,MyTens},
(* pre-cleaning *)
  res=exp//.{Cos[x_]->x, Sin[x_]->x, Tan[x_]->x};
  res=res/.{Index[type_,Index[type_,bb_]]->Index[type,bb],Index[Spin1,b_]->b,Index[Spin2,b_]->b,Index[Lorentz,b_]->b};  

(* Save the ordering of the expression and remove objects without indices *)
  res=If[(Length[res]=!=1) && (Length[res]=!=0),(List@@res), List[res]]//.{nc->Sequence, Dot->Sequence, GammaLine->Sequence,TensDot2[ff__][opt__]->Sequence[ff]};
  res=res/.{HC[aa_]->aa,Conjugate[aa_]->aa,CC[aa_]->aa,IndexDelta[aa__]->Sequence[aa], PauliSigma[aa__]->Sequence[aa], K6bar[aa__]->Sequence[aa],K6[aa__]->Sequence[aa]};
  res=res/.Power[a_,n_? (IntegerQ[#]&)] :> Sequence@@Table[a,{Abs[n]}];
  res=res//.del[x_,mu_]->Sequence[x, Index[Lorentz,mu]];
  res=res/.FV[sa_, mu_]->Index[Lorentz, mu];
  res=res/.eeps[il_]:>Sequence@@il;
  res=res/.TensDot->MyTens/.MyTens[argx__][inds__]:>Sequence[argx,Sequence@@(Index[Spin,#]&/@{inds})];
  res=DeleteCases[If[MatchQ[#,_[__]]&&Head[#]=!= Complex,#,{}]&/@res,{}];
  res=DeleteCases[If[NumericQ[#],{},#]&/@res,{}];

(* Restore index types *)
  res = res/. {
    f_?(CSpin32FieldQ[#]===True &)[sp_,muu_,inds__]:> Sequence[Index[Spin,sp],Index[Lorentz,muu],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(RSpin32FieldQ[#]===True &)[sp_,muu_,inds__]:> Sequence[Index[Spin,sp],Index[Lorentz,muu],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(CSpin32FieldQ[#]===True &)[sp_,muu_]:> Sequence[Index[Spin,sp],Index[Lorentz,muu]],
    f_?(RSpin32FieldQ[#]===True &)[sp_,muu_]:> Sequence[Index[Spin,sp],Index[Lorentz,muu]],
    f_?(Chirality[#]===Left && Spin32WeylFieldQ[#]===True &)[sp_,muu_,inds___]:> Sequence[Index[Spin1,sp],Index[Lorentz,muu],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(Chirality[#]===Right && Spin32WeylFieldQ[#]===True &)[sp_,muu_,inds___]:> Sequence[Index[Spin2,sp],Index[Lorentz,muu],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(Chirality[#]===Left && WeylFieldQ[#]===True &)[sp_]->Index[Spin1,sp],
    f_?(Chirality[#]===Left && WeylFieldQ[#]===True &)[sp_,inds__]:> Sequence[Index[Spin1,sp],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(Chirality[#]===Right&& WeylFieldQ[#]===True &)[sp_]->Index[Spin2,sp],
    f_?(Chirality[#]===Right&& WeylFieldQ[#]===True &)[sp_,inds__]:> Sequence[Index[Spin2,sp],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(DiracFieldQ[#]===True &)[sp_,inds__]:> Sequence[Index[Spin,sp],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(MajoranaFieldQ[#]===True &)[sp_,inds__]:> Sequence[Index[Spin,sp],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(DiracFieldQ[#]===True &)[sp_]-> Index[Spin,sp],
    f_?(MajoranaFieldQ[#]===True &)[sp_]-> Index[Spin,sp],
    f_?(Spin2FieldQ[#]===True&)[mu_,nu_]:>Sequence[Index[Lorentz,mu],Index[Lorentz,nu]],
    f_?(Spin2FieldQ[#]===True&)[mu_,nu_,inds__]:>Sequence[Index[Lorentz,mu],Index[Lorentz,nu],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(VectorFieldQ[#]===True&)[mu_]->Index[Lorentz,mu],
    f_?(VectorFieldQ[#]===True&)[mu_,inds__]:> Sequence[Index[Lorentz,mu],Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]],
    f_?(ScalarFieldQ[#]===True&)[inds__]:> Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]],
    si[mu_,sp_,spd_]-> Sequence[Index[Lorentz,mu], Index[Spin1,sp],Index[Spin2,spd]],
    sibar[mu_,spd_,sp_]-> Sequence[Index[Lorentz,mu], Index[Spin2,spd],Index[Spin1,sp]],
    Ga[mu_]->Index[Lorentz,mu],
    Ga[mu_,sp1_,sp2_]->Sequence[Index[Lorentz,mu],Index[Spin,sp1],Index[Spin,sp2]],
    Sig[mu_,nu_,sp1_,sp2_]->Sequence[Index[Lorentz,mu],Index[Lorentz,nu],Index[Spin,sp1],Index[Spin,sp2]],
    Sig[mu_,nu_]->Sequence[Index[Lorentz,mu],Index[Lorentz,nu]],
    ProjP[sp1_,sp2_]->Sequence[Index[Spin,sp1],Index[Spin,sp2]],
    ProjM[sp1_,sp2_]->Sequence[Index[Spin,sp1],Index[Spin,sp2]],
    Ueps[a_,b_]->Sequence[Index[Spin1,a],Index[Spin1,b],Index[Spin2,a],Index[Spin2,b]],
    Deps[a_,b_]->Sequence[Index[Spin1,a],Index[Spin1,b],Index[Spin2,a],Index[Spin2,b]],
    ME[mu_,nu_]->Sequence[Index[Lorentz,mu],Index[Lorentz,nu]],
    Eps[mu_,nu_,ro_,si_]->Sequence[Index[Lorentz,mu],Index[Lorentz,nu],Index[Lorentz,ro],Index[Lorentz,si]],
    FR$deltaZ[fi_List, inds_, op_String]:>Sequence@@Flatten[inds],FR$deltaZ[fi_List, inds_]:>Sequence@@Flatten[inds],
    f_?(MemberQ[M$IndParam,#]===True&)[inds__]:>Sequence[Inner[Index,Flatten[IndexType[f]],{inds},Sequence]]    
 };

  Do[
    Module[{GrName, GrRepList, GrIndList,GrAdj,GrRep,GrStr,GrSym},
      GrName=MR$GaugeGroupList[[ll]];
      If[!(Abelian/.MR$GaugeGroupRules[GrName]),
        GrRep=Representations/.MR$GaugeGroupRules[GrName];
        GrStr=StructureConstant/.MR$GaugeGroupRules[GrName];
        GrSym=SymmetricTensor/.MR$GaugeGroupRules[GrName];
        If[VectorQ[GrRep],GrIndList={GrRep[[2]]};GrRepList={GrRep[[1]]},GrIndList=GrRep[[All,2]];GrRepList=GrRep[[All,1]]];
        GrAdj=GroupToAdj[GrName];
        res=ReplaceAll[res,{
         ff_?(MemberQ[GrRepList,#] &)[inda_,indx_,indy_]:>
           Sequence[Index[GrAdj,inda],Index[GrIndList[[Position[GrRepList,ff][[1,1]]]],indx],Index[GrIndList[[Position[GrRepList,ff][[1,1]]]],indy]]}];
        res=ReplaceAll[res,{ff_?(MemberQ[GrRepList,#] &)[inda_]:>Index[GrAdj,inda]}];
        res=res/.GrStr[inda_,indx_,indy_]->Sequence[Index[GrAdj,inda],Index[GrAdj,indx],Index[GrAdj,indy]];
        res=res/.GrSym[inda_,indx_,indy_]->Sequence[Index[GrAdj,inda],Index[GrAdj,indx],Index[GrAdj,indy]]  ]],
  {ll,1,Length[MR$GaugeGroupList]}]; Clear[ll];

  res = DeleteCases[res//.{
     List[if1___,Eps[id1___,jj_],if2___, Index[type_,jj_],if3___]:>List[if1,Index[type,jj],Eps[id1],if2, Index[type,jj],if3],
     List[if1___,Index[type_,jj_],if2___,Eps[id1___,jj_],if3___]:>List[if1,Index[type,jj],if2,Eps[id1],Index[type,jj],if3]},Eps[]];

(* Cleaning *)
  res=DeleteCases[res/.Index[_,Index[type_,bb_,___]]->Index[type,bb],Index[_,Ext[_]]];
  res=If[NumericQ[#]||MatchQ[#,Index[type_,_?NumericQ]]||MatchQ[#,Index[type_,Int[_?NumericQ]]],{},#]&/@res;
  res=DeleteCases[res,{}];

(* result *) 
res];


(* ::Subsubsection:: *)
(*Get the type of the indices of a given field or a given parameter*)


IndexType[field_?(FieldQ[#]===True&)]:={DeleteCases[$IndList[field]/.Index[a_]->a,Spin|Spin1|Spin2|Lorentz]};

(*IndexType[field_?((FieldQ[#]===True) && Not[AntiFieldQ[#]===True] &)]:={Indices/.MR$ClassesRules[MR$Class[field]]/.Index[a_]->a};

IndexType[field_?((FieldQ[#]===True) && (AntiFieldQ[#]===True) &)]:={Indices/.MR$ClassesRules[MR$Class[anti[field]]]/.Index[a_]->a};*)


IndexType[param_?(TensQ[#]===True&)]:= (Indices/.(MR$ParameterRules[param]))/.Index[a_]->a;

IndexType[param_?(NoTensQ[#]===True&)]:= (Indices/.(MR$ParameterRules[param]))/.Index[a_]->a;


(* ::Section:: *)
(*Solving the equation of motion for the auxiliary fields*)


(* ::Subsubsection::Closed:: *)
(*Wrapper*)


SolveEqMotionFD[exp_]:=SolveEqMotionF[SolveEqMotionD[exp]];


(* ::Subsubsection::Closed:: *)
(*D-terms: SolveEqMotionD[ expression ]*)


(* ::Text:: *)
(*SolveEqMotionD[ expression ] solve the equations of motion for the auxiliary D-fields of the theory*)


SolveEqMotionD[exp_] :=Module[{tmp, Der, PowTimes, MakePattern, MyModule, MyRuleDelayed, Modulify},
(* Breaks the powers *)
  tmp=exp/. Power[a_,n_? (IntegerQ[#]&&(#>0)&)] :> PowTimes@@Table[a,{n}];

(* Modulification *)
  Modulify[Plus[a_,b__]]:=Modulify[a] + Modulify[Plus[b]];
  Modulify[aa_?(Head[#]=!=Plus&)]:=Module[{IdxLst},IdxLst=ReplaceAll[DeleteCases[Tally[ToIndexList[aa]/.Index[_,a_]->a],{_,1}],{a_,2}->a];MyModule[IdxLst,aa]];

(* Definition of the derivative with respect to a field *)
  Der[L_,phi_[ind__]]:=Module[{dum,dum2},
    Coefficient[Expand[dum2*L]/.{
      sub_*phi[ind2__]:>sub*dum*Times@@(IndexDelta@@@Transpose[{{ind},{ind2}}])+phi[ind2]*Der[sub,phi[ind]],
      sub_*Power[phi[ind2__],2]:>2*dum*phi[ind]*sub+Power[phi[ind2],2]*Der[sub,phi[ind]]},dum]/.dum2->1];

(* Function to create a replacement rule *)
    MakePattern[rule_Rule] := MapAt[MapAt[Pattern[#,Blank[]]&, #, 1]&, rule, 1];

(* Solution of the eq. of motion *)
  FR$DTerms=Flatten@( Module[{DT, res},
  (* Get the auxiliary field with the proper indices *)
    DT=(SF2Aux[Superfield/.MR$GaugeGroupRules[#]][$IndList[Superfield/.MR$GaugeGroupRules[#]]])/.{
      dt_[{}]->dt, Index[name_]:>Module[{ii},ii],List->Sequence};

  (* Derivate the Lagrangian and create the replacement rule *)
    If[MatchQ[DT,_[__]], 
      If[Der[exp,DT]===0,res={}, res=Flatten[Solve[Der[exp,DT]==0,DT]]; If[res=!={},res=MakePattern @@ res,Print["No solution."]; Abort[]]], 
      If[D[exp,DT]===0,res={},res=Flatten[Solve[D[exp,DT]==0,DT]]; If[res==={},Print["No solution."]; Abort[]]]];

  (* Add the modules *)
    res=res/.Rule[a_,b_]:>MyRuleDelayed[a,Modulify[Expand[b]]]; 
  res/.{MyRuleDelayed->RuleDelayed,MyModule->Module}]&/@MR$GaugeGroupList);

(* Replace the DTerms by the solution of the eq. of motion *)
tmp/.FR$DTerms/.PowTimes->Times];


(* ::Subsubsection::Closed:: *)
(*F-terms: SolveEqMotionF[ expression ]*)


(* ::Text:: *)
(*SolveEqMotionF[ expression ] solve the equations of motion for the auxiliary F-fields of the theory*)


SolveEqMotionF[exp_] :=Module[{tmp, Der, PowTimes, MakePattern, MyModule, MyRuleDelayed, Modulify,IndexDelta2,MyPattern},
(* Breaks the powers *)
  tmp=exp/. Power[a_,n_? (IntegerQ[#]&&(#>0)&)] :> PowTimes@@Table[a,{n}];

(* Modulification *)
  Modulify[Plus[a_,b__]]:=Modulify[a] + Modulify[Plus[b]];
  Modulify[aa_?(Head[#]=!=Plus&)]:=Module[{IdxLst},IdxLst=ReplaceAll[DeleteCases[Tally[ToIndexList[aa]/.Index[_,a_]->a],{_,1}],{a_,2}->a];MyModule[IdxLst,aa]];

(* Definition of the derivative with respect to a field *)
  Der[L_,phi_[ind__]]:=Module[{dum,dum2},
    Coefficient[Expand[dum2*L]/.{
      sub_*phi[ind2__]:>sub*dum*Times@@(IndexDelta2@@@Transpose[{{ind},{ind2}}])+phi[ind2]*Der[sub,phi[ind]],
      sub_*Power[phi[ind2__],2]:>2*dum*phi[ind]*sub+Power[phi[ind2],2]*Der[sub,phi[ind]]},dum]/.dum2->1];

(* Function to create a replacement rule *)
    MakePattern[rule_Rule] := MapAt[#/.f_?(FieldQ[#]===True&)[inds__] :> 
         f[Sequence@@(Map[If[MatchQ[#,Index[_,_]],#/.Index[name_,gr_]:>MyPattern[gr,Blank[]],MyPattern[#,Blank[]]]&, {inds}])]&, rule, 1];

(* Solution of the eq. of motion *)
  FR$FTerms=Flatten@( Module[{FT, res,sys},
  (* Get the auxiliary field with the proper indices *)
  FT=SF2Aux[#][$IndList[#]]/.{dt_[{}]->dt, Index[name_]:>Module[{ii},Index[name,ii]],List->Sequence}; 
  (* Derivate the Lagrangian and create the replacement rule *)
    If[MatchQ[FT,_[__]], 
      If[(Der[exp,FT]===0)&&(Der[exp,HC[FT]]===0), 
        res={}, 
        sys={Der[exp,FT]==0,Der[exp,HC[FT]]==0}//.{
          f_?(FieldQ[#]===True&)[ind1___,ind_?(Not[NumericQ[#]]&),ind2___] IndexDelta2[ind_,Index[name_,muf_]]:>f[ind1,Index[name,muf],ind2],
          f_?(FieldQ[#]===True&)[ind1___,ind_?(Not[NumericQ[#]]&),ind2___] IndexDelta2[Index[name_,muf_],ind_]:>f[ind1,Index[name,muf],ind2]};
        res=Flatten[Solve[sys,{FT,HC[FT]}]]; If[res=!={},res=MakePattern /@ res,Print["No solution."]; Abort[]]], 
      If[D[exp,FT]===0,
        res={},
        sys={D[exp,FT]==0,D[exp,HC[FT]]==0};
        res=Flatten[Solve[sys,{FT,HC[FT]}]];If[res==={},Print["No solution."]; Abort[]]]
       ];
   res=res/.{IndexDelta2->IndexDelta};

  (* Add the modules *)
    res=res/.Rule[a_,b_]:>MyRuleDelayed[a,Modulify[Expand[b]]]/.MyModule[List[],bla_]:>bla;
  res/.{MyRuleDelayed->RuleDelayed,MyModule->Module,MyPattern->Pattern,Index[_,a_]->a}]&/@M$ChiralSuperfieldNames);

(* Replace the FTerms by the solution of the eq. of motion *)
Plus@@((Expand[#/.FR$FTerms/.PowTimes->Times])&/@(List@@tmp))];


(* ::Section:: *)
(*NCC environment*)


(* ::Subsection::Closed:: *)
(*nccExtractNum*)


(* ::Text:: *)
(*Extracts numerical constants form an ncc environment*)


nccExtractNum[x1___, a_?(numQ[#]===True&),x2___]:=a nccExtractNum[x1, x2];


(* ::Subsection::Closed:: *)
(*IndexShifter*)


(* ::Text:: *)
(*Takes as an arguments a field or a sigma matrix, and returns the object with all indices lowered.*)
(*Note that a sibar is transformed into si.*)


IndexShifter[sibar[mu_,ad_,a_]]:=Block[{index1=Unique["spd"],index2=Unique["sp"]},Return[{Ueps[ad,index2] Ueps[a,index1],si[mu,index1,index2]}]];

IndexShifter[si[inds___]]:={1,si[inds]};

IndexShifter[wey_?(WeylFieldQ[#]===True&)[a_,inds___]]:=Block[{index},
   If[Chirality[wey]===Left,
      index=Unique["sp"];
      Return[{Ueps[a,index],wey[index,inds]}],
      index=Unique["spd"];
      Return[{Ueps[a,index],wey[index,inds]}]
     ];
];


(* ::Subsection::Closed:: *)
(*nccPutIndices*)


(* ::Text:: *)
(*Main routine, that restores the Spin1 and Spin2 indices.*)


nccPutIndices[x1_?(WeylFieldQ[#]===True&),matrices___,x2_?(WeylFieldQ[#]===True&)]:=Block[{
  makebracket,
  chain = {x1,matrices,x2},
  Nind1,Nind2,
  weyl1,weyl2,
  spindices,spindices2,
  IndexAppender,
  newmatrices,
  uepslist,newindices
  },

  (* Internal function, that turn x into x[]*)
  makebracket=If[MatchQ[#,_[___]],#,#[]]&;
  chain=makebracket/@chain;

  (* Internal function:
     IndexAppender[mat[x,y...],{s,t}]
     returns
     mat[x,z,...,s,t]
   *)
  IndexAppender[mat_[inds___],newindices[s_,t_]]:=mat[inds,s,t];

  (* Check of the conditions  are right *)
  Nind1=Length[chain[[1]]];
  Nind2=Length[chain[[-1]]];
  weyl1=chain[[1]];
  weyl2=chain[[-1]];

  If[(Nind1!=Length[$IndList[weyl1[[0]]]]-1)||(Nind2!=Length[$IndList[weyl2[[0]]]]-1),
     Message[ncc::weyls]
    ];

  (* We now proceed in two steps:
     step1: introduce contracted indices *without* Ueps
     step2: introduce Ueps
   *)
  (*Step1*)
  (*create the indices we need*)
  spindices=Table[Unique["sp"],{Length[{matrices}]+1}];
  (*add them*)
  weyl1=Prepend[weyl1,spindices[[1]]];
  weyl2=Prepend[weyl2,spindices[[-1]]];
  spindices2=Rest[spindices];
  spindices=Most[spindices];
  spindices=newindices@@@Transpose[{spindices,spindices2}];
  newmatrices =Inner[IndexAppender,{matrices},spindices,List];

  (* We now need to
   * lower the indices of sibar
   * If weyl1 one is left, lower
   * If weyl2 is right, lower
   *)
  (* For the matrices *)
  newmatrices=IndexShifter/@newmatrices;
  weyl1=If[Chirality[weyl1[[0]]]===Left,
  IndexShifter[weyl1],{1,weyl1}];
  weyl2=If[Chirality[weyl2[[0]]]===Right,
  IndexShifter[weyl2],{1,weyl2}];

  (* Let's put everything together *)
  newmatrices=Append[Prepend[newmatrices,weyl1],weyl2];
  (* This object is now a matrix, each entry being a pair {Ueps,  weyl/Sigma}
  *)
  newmatrices=Transpose[newmatrices];
  uepslist=Times@@newmatrices[[1]];
  newmatrices=newmatrices[[2]];

  (* Return and exit *)
  Return[uepslist*(nc@@newmatrices)];
];




(* ::Subsection::Closed:: *)
(*ncc*)


ncc[something___]:=Block[{temp},
  temp=nccExtractNum[something];
  temp=temp/.nccExtractNum :> nccPutIndices;
  Return[temp];
];
