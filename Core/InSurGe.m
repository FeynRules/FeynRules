(* ::Package:: *)

(* ::Text:: *)
(*Comments:*)
(*  07.06.2013: Removed all OptimizeIndex calls because the latter calls now ExpandIndices which is not wanted here.*)


(* ::Section::Closed:: *)
(*Format*)


Format[FR$D[aa_],StandardForm] := ("\[PartialD]"/"\[PartialD]t")[aa];
Format[FR$D[aa_],TraditionalForm] := ("\[PartialD]"/"\[PartialD]t")[aa];


(* ::Section:: *)
(*Useful Functions*)


(* ::Subsection::Closed:: *)
(*Get the dimension associated to a given index*)


IndexDim[ind_] := Length[IndexRange[Index[ind/.Index[aa__]->aa]]/.{NoUnfold[bb_]->bb,Unfold[bb_]->bb}];


(* ::Subsection:: *)
(*SU(N) invariant products*)


(* ::Text:: *)
(*The synthax for a SU(N) invariant product is SUDot[ SF, SF, SF, ..., index], where index corresponds to the contracted SU(N) index and is contained into the explicit indices of each SF.*)
(*SUEps could also be used explicitely. Sometimes there is no choice.*)
(*It works also for list of contracted indices.*)
(*A single index is always transformed to a list.*)
(*If a contracted index is on the form EpsUnfold[name], it can be explicitely expanded by the function ExpandSUDot.*)


SUDot[argx__,a_?(Head[#]=!=List&)]:=SUDot[argx,{a}];


HC[SUDot[argx__]]:=SUDot[Sequence@@(If[Head[#]=!=List,HC[#],#]&/@{argx})];


Conjugate[SUEps[argx__]]^:=SUEps[argx];
HC[SUEps[argx__]]^:=SUEps[argx];


(* ::Text:: *)
(*The routine ExpandSUDot expands the SU (N). By default, a rank - N epsilon tensor is introduced. If the index is stored on the form EpsUnfold[index], then the expansion is explicitely performed.*)


ExpandSUDot[expr_]:=expr/.SUDot->TreatSUDot;


TreatSUDot[argx__,index_List]:=Module[{SUEps,tmp,SUDotb,luf,MyTable,MySignature},
  (* Creating the epsilons *)
  tmp=SUDotb[argx] Times@@ (SUEps[#,List[]]&/@index)/.EpsUnfold->Sequence;
  tmp=tmp//.SUDotb[kkk___,fi_[arg0___,idx_,arg1___],lll___]SUEps[idx_,{in___}]:>
     Module[{nid=Unique["idx"]},SUDotb[kkk,fi[arg0,nid,arg1],lll] SUEps[idx,Append[{in},nid]]];

  (* Unfolding, if necessary *)
  luf=Cases[index,EpsUnfold[_]]/.EpsUnfold->Sequence;
  tmp=tmp//.fact_ SUEps[idx_?(MemberQ[luf,#]&),argu_]:>
    Plus@@Flatten[ MyTable[fact  MySignature[argu],Sequence@@(List[#,1,Length[argu]]&/@argu)]/.MyTable->Table/.MySignature->Signature];

tmp//.{SUEps[_,muf_List]:>Eps[Sequence@@muf],SUDotb->Times}];


(* ::Subsection::Closed:: *)
(*Add the types of the indices of a given superfield (if the indices are not there, i.e., we have only the classname, they are created automatically)*)


Indexify[sf_?(SuperfieldQ[#]===True&)[inds__]]:=sf[Inner[Index,($IndList[sf]/.Index[a__]->a),{inds},Sequence]];
Indexify[sf_?($IndList[#]=!=List[] && SuperfieldQ[#]===True&)]:=sf[Sequence@@($IndList[sf]/.Index[bla_]:>Index[bla,Unique["index"]])];
Indexify[sf_?($IndList[#]===List[] && SuperfieldQ[#]===True&)]:=sf;


(* ::Subsection::Closed:: *)
(*Add the types of the indices of a given field (if the indices are not there, i.e., we have only the classname, they are create automatically)*)


Indexify[sf_?(FieldQ[#]===True&)[inds__]]:=sf[Inner[Index,($IndList[sf]/.Index[a__]->a),{inds},Sequence]];
Indexify[sf_?($IndList[#]=!=List[] && FieldQ[#]===True&)]:=sf[Sequence@@($IndList[sf]/.Index[bla_]:>Index[bla,Unique["index"]])];
Indexify[sf_?($IndList[#]===List[] && FieldQ[#]===True&)]:=sf;


(* ::Subsection:: *)
(*Get the dynking index summed over all chiral multiplets*)


GetDynkin[group_]:= Module[{indxSF,multi,charge,casi,indxgr},
  indxSF=(SF2Indices[#]&/@M$ChiralSuperfieldNames)/.Index[aa__]->aa;
   If[ AbelianQ[group],
    multi = Times@@(IndexDim/@# )&/@indxSF; charge = (GroupToCharge[group]/@M$ChiralSuperfieldNames)^2; multi.charge,
    indxgr = GroupToReps[group][[All,2]]; multi = Times@@((IndexDim/@Complement[#,indxgr])/.List[]->{1})&/@ indxSF; 
    casi = Flatten[(Intersection[#,indxgr] &/@ indxSF)/.GroupToDynkins[group]/.List[]->0]; multi.casi ]];


(* ::Subsection:: *)
(*Gives the casimir of a field with respect to the considered gauge group *)


Options[FCasimir]={power->2};


FCasimir[field_, gauge_,OptionsPattern[]]:= Module[{n},
  n=OptionValue[power];
  If[AbelianQ[gauge],
    GroupToNorm[gauge]*(GroupToCharge[gauge][field])^n, 
    (Intersection[GroupToReps[gauge][[All,2]],($IndList[field/.a_[ind__]->a] /.Index[a_]->a)]/. GroupToCasimirs[gauge] /. List[]->{0})[[1]]]];


(* ::Subsection::Closed:: *)
(*Reduces MyEps and MyDelta products*)


FormattingExpr[expr_]:= Module[{tmp},
tmp = expr//. { MyDelta[Index[type_,ind1_],Index[type_,ind2_]]^2:> IndexDim[type], MyEps[arg1_,arg2_]^2:> Factorial[Length[{arg1,arg2}]] ,
                MyDelta[arg1_, arg2_] MyDelta[arg2_,arg3_]:> MyDelta[arg1,arg3], MyEps[arg1_,arg2_]MyEps[arg2_,arg3_]:>Inner[ MyDelta, {arg1},{arg3}, Times]
                };
tmp = tmp/.{MyDelta[a__]:> IndexDelta[a],MyEps->SUEps}
];


(* ::Subsection:: *)
(*Common part to 2 loop beta functions involving a sum over 3 yukawas*)


GetCommonPart[]:= Module[{sf1,sf3,sf2,sf4,sf5,sf6,nfields},
(* I chose FR$Temp instead of FR$SuperW to keep the order of the fields. It is important.*)
  sf1=Indexify/@M$ChiralSuperfieldNames;sf2=Indexify/@M$ChiralSuperfieldNames;sf3=Indexify/@M$ChiralSuperfieldNames;
  sf4=Indexify/@M$ChiralSuperfieldNames;sf5=Indexify/@M$ChiralSuperfieldNames;sf6=Indexify/@M$ChiralSuperfieldNames;
  nfields=Length[M$ChiralSuperfieldNames];
(* Creating the list *)
  FR$Common=Flatten[Table[KeepOrdered[FR$Temp[sf1[[a]],sf2[[b]],sf3[[c]]],FR$Temp[sf3[[c]],sf4[[d]],sf5[[e]]],FR$Temp[sf4[[d]],sf5[[e]],sf6[[f]]]],
  {a,1,nfields},{b,1,nfields},{c,1,nfields},{d,1,nfields},{e,1,nfields},{f,1,nfields}]];
(* Deleting any null element *)
  FR$Common=DeleteCases[If[MatchQ[((#/.FR$Temp[a__]:>FR$SuperW[a])/.FR$SuperWRules ),KeepOrdered[___,0,___]],0,#]&/@FR$Common,0];];


(* ::Section:: *)
(*Functions for preprocessing the superpotential and the soft SUSY-breaking Lagrangian*)


(* ::Subsection::Closed:: *)
(*Tests*)


(* ::Text:: *)
(*This function tests the restrictions*)


TestSuperW[SuperW_]:=Module[{},
  (* Term for a given parameter == 1 *)
  If[DeleteCases[Tally[SuperW[[All,1]]//.{
     SUEps[__]->1, 
     IndexDelta[__]->1, 
     - exp_->exp,
     (a_?(MemberQ[MR$Parameters[[All,1]],#]&)[inds__])?(numQ[#]===True&)->a}],{_,1}]=!={},
    Message[RGE::SuperWparams]; Abort[]];

  (* Check renormalizability *)
  If[DeleteCases[Length/@SuperW[[All,2]],1|2|3]=!={}, Message[RGE::SuperWrenorm]];
];


(* ::Text:: *)
(*This function checks if the gaugino mass terms are diagonal *)


(* ::Text:: *)
(*Changed on 07.06.2013 because OptimizeIndex does not work as expected anymore.*)


(*CheckLinoMass[Lmass_,LagSoft_]:=Module[{},  
  If[OptimizeIndex[Expand[LagSoft-Lmass/.{
    a_?(ScalarFieldQ[#]===True&)[__]->0,
    a_?(ScalarFieldQ[#]===True&)->0,
    a_?(VectorFieldQ[#]===True&)[__]->0,
    SUDot[__]->0}]]=!=0,Message[RGE::InoMassLag]; Abort[]];
Lmass];
*)

CheckLinoMass[Lmass_,LagSoft_]:=Block[{index,tmpp},
  tmpp=Expand[Lmass-LagSoft]/.{ a_?(ScalarFieldQ[#]===True&)[__]->0, a_?(ScalarFieldQ[#]===True&)->0, a_?(VectorFieldQ[#]===True&)[__]->0, SUDot[__]->0};
  tmpp=tmpp/.Dot->Times/.a_?(FieldQ[#]&)[ind__]^2:>a[index];
  If[tmpp==0, Return[Lmass],Message[RGE::InoMassLag]; Abort[]]];


(* ::Subsection:: *)
(*Derivation of the gaugino mass terms*)


(* ::Text:: *)
(*This function derive the list of the gaugino masses from LSoft*)


GauginoMasses[LagSoft_]:=Module[{Lmass=0,Der}, 
  (* Definition of the derivative with respect to a fermionic field (cf. Dot products) *)
   Der[L_,phi_[ind__]]:=Module[{dum,dum2},
    Coefficient[Expand[dum2*L]/.{
      sub_*phi[ind2__]:>sub*dum*Times@@(IndexDelta@@@Transpose[{{ind},{ind2}}])+phi[ind2]*Der[sub,phi[ind]],
      sub_*Power[phi[ind2__],2]:>2*dum*phi[ind]*sub+Power[phi[ind2],2]*Der[sub,phi[ind]],
      sub_*Dot[a_,b_]:>dum*(Der[a,phi[ind]]*b+a*Der[b,phi[ind]])*sub+Der[sub,phi[ind]] Dot[a,b]},dum]/.dum2->1]//.{
      Dot[a___,IndexDelta[inds__],b___]->Dot[a,b] IndexDelta[inds]};

  (* Derivation of the mass terms *)
  {Module[{ino,spin,gauge,massterm, masstermbar},  
   ino=If[AbelianQ[#]===True, (SF2Ino[Superfield]/.MR$GaugeGroupRules[#])[spin], (SF2Ino[Superfield]/.MR$GaugeGroupRules[#])[spin,gauge]];
   massterm=-Der[Der[Expand[LagSoft],ino],ino]/.{IndexDelta[spin,spin]->1,IndexDelta[gauge,gauge]->1}; 
   masstermbar=-Der[Der[Expand[LagSoft],HC[ino]],HC[ino]]/.{IndexDelta[spin,spin]->1,IndexDelta[gauge,gauge]->1};

   (* Checks *)
   If[massterm=!=HC[masstermbar],Message[RGE::InoMass];Abort[]];

   (* Mass Lagrangian *)
   Lmass+=-1/2 massterm ino.ino -1/2 masstermbar HC[ino].HC[ino];
 
   massterm]&/@MR$GaugeGroupList, CheckLinoMass[Lmass,LagSoft]}];


(* ::Subsection:: *)
(*Creation of lists from bilinear and trilinear couplings from LSoft and SuperW*)


(* ::Text:: *)
(*This function produces a {param, {superfield1, superfield2, ...} } list from the superpotential. It also create the replacement rules FR$SuperW[superfields] -> ...*)


ExtractSuperWTerms[superpot_,flag_]:=Module[{tmp,tmpdelta},

  (* Map colours and anticolours + initialization *)
  Global`Colourb=Colour;
  SetAttributes[FR$SuperW,Orderless];
  SetAttributes[FR$Soft,Orderless];

  (* Step 1: create a list with elements {parameter, superfields} *)

  tmp=If[Head[superpot]===Plus,List@@superpot,{superpot}];
  tmp=Module[{prm=#/.{SUDot[__]->1,a_?(flag[#]===True&)[__]->1,a_?(flag[#]===True&)->1}},List[prm,#/prm]]&/@tmp;
  (* Benj: commenting the conjugate part as probably not necessary *)
  If[flag===FieldQ, 
    tmp=(Sequence@@#)&/@(If[Length[#]===2,
      DeleteCases[#,(*{___ Conjugate[_],_} | {Conjugate[_],_} |*) {_,_? (Cases[List@@#,_?(AntiFieldQ[#]===True&)]=!={} || AntiFieldQ[#]===True&)}],
      #]&/@
      (GatherBy[tmp,ReplaceRepeated[(#/.{a_,b_}-> a),{Conjugate[a__]->a,a_?(# =!= Rational && numQ[#[Sequence@@$IndList[#]]]===True&)[inds__]->a}]&]))
  ];
  tmp=tmp//.{
    {expr_,bla__ SUDot[argx__,{aa__}]}:>{expr*Times@@(Table[SUEps[{{aa}[[nn]]}],{nn,1,Length[{aa}]}]),ReplaceAll[List[bla, argx],Rule[#,SUDot[#]]&/@{aa}]},
    {expr_,SUDot[argx__,{aa__}]}:>{expr*(Times@@Table[SUEps[{{aa}[[nn]]}],{nn,1,Length[{aa}]}]),ReplaceAll[List[argx],Rule[#,SUDot[#]]&/@{aa}]}};
(*  tmp=tmp/.Power[exp_,n_]:>Table[exp,{n}];*)
(*Mod by A.A. 03.20.2013: Added test on exp so that nothing is done when exp is a number*)
  tmp=tmp/.Power[exp_?(!NumericQ[#]&),n_]:>Table[exp,{n}];


  tmp=If[Head[#[[2]]]=!=List,{#[[1]],DeleteCases[tmpdelta*#[[2]]/.Times->List,tmpdelta]},#]&/@tmp;

  (* Step 2: insert explicit Epsilon tensors instead of the SU (N) dot-products *)
  tmpdelta=tmp/.sf_?(flag[#]===True&)[inds__]:> Indexify[sf[inds]];
  tmpdelta=tmpdelta//.$TensIndRules;
  tmpdelta=ReplaceRepeated[#,{fact__ SUEps[{ata_,inds___}],List[jjj___,sf1_[aar___,Index[type_,SUDot[ata_]],bb___] ,kkk___]}:>
  Module[{id=Unique["idx"]},{fact SUEps[Append[{ata,inds},Index[type,id]]], List[jjj,sf1[aar,Index[type,id],bb],kkk]}]]&/@tmpdelta;
  tmpdelta=tmpdelta/.SUEps[{_,inds__}]:>SUEps[inds];

  (* Step 3: insert explicit IndexDelta instead of contracted indices *)
  tmpdelta=ReplaceRepeated[#,{fact_,List[jjj___,sf1_[aar___,Index[type_,ind_],bb___] ,kkk___,sf2_[aaa___,Index[type_,ind_],bbb___],lll___]}:>
    Module[{id=Unique["idx"]},{fact IndexDelta[Index[type,id],Index[type,ind]], List[jjj,sf1[aar,Index[type,ind],bb],kkk,sf2[aaa,Index[type,id],bbb],lll]}]
  ]&/@tmpdelta;

  (* Step 4: Multiplicity *)
   tmpdelta=Replace[#,List[aaa_,bbb_]:>List[aaa*Times@@(Factorial/@Tally[bbb][[All,2]]),bbb]]&/@tmpdelta;

  (* Creating the replacement list for the Yukawas *)
  If[flag===SuperfieldQ,
    FR$SuperWRules=Rule[(FR$SuperW[Sequence@@(#/.Index[type_,cc_]:>Index[type, MyPattern[cc,Blank[]]]&/@(#[[2]]))]),(#[[1]]) ]&/@tmpdelta;
(*AA 06.02.13: Added this line so that indices that only appear in the right-hand side be unique everytime the rule is applied*)
    FR$SuperWRules=ArrangeIndices[FR$SuperWRules];
    FR$SuperWRules=Join[FR$SuperWRules/.MyPattern->Pattern,List[FR$SuperW[_,_,_]->0,FR$SuperW[_,_]->0,FR$SuperW[_]->0]],
    FR$SoftRules=Rule[(FR$Soft[Sequence@@(#/.Index[type_,cc_]:>Index[type, MyPattern[cc,Blank[]]]&/@(#[[2]]))]),(#[[1]]) ]&/@tmpdelta;
    FR$SoftRules=Join[FR$SoftRules/.MyPattern->Pattern,List[FR$Soft[_,_,_]->0,FR$Soft[_,_]->0,FR$Soft[_]->0]];
    FR$SoftRules=ArrangeIndices[FR$SoftRules];];

tmpdelta];


(* ::Subsection:: *)
(*Make sure an index is only repeated twice*)


(* ::Text:: *)
(*Added this function so that:*)
(*    1) an index does not appear more than twice in each side*)
(*    2) if it appears twice in a given side, it should not appear in the other side*)
(*    3) If it appears twice in the right-hand side, than <right-hand-side>  should be *)
(*              Module[{index-repeated-twice}, <right-hand-side> ]*)
(*         so that whenever we apply the rule, a new set of indices in generated and an index does not appear more than twice*)
(*  This function does not look for indices appearing twice (or more) in the left-hand side*)


ArrangeIndices[list_]:=Block[{MyPauli,MyDelta,MyEps,tmplist,myresu,tmpresu,lefthandind,righthandind,MModule,MRuleDelayed},
  (*making things easier*)
  TensQ[MyPauli]=True;TensQ[MyPauli[ind__]]=True;
  TensQ[MyDelta]=True;TensQ[MyDelta[ind__]]=True;
  TensQ[MyEps]=True;TensQ[MyEps[ind__]]=True;
  tmplist=list/.{PauliSigma->MyPauli,IndexDelta->MyDelta,SUEps->MyEps};

  myresu=(
    (*list of indices in the right-hand side and in the left-hand side*)
   lefthandind=DeleteCases[#[[1]]/.{FR$SuperW->List,FR$Soft->List}/.a_?(SuperfieldQ[#]||FieldQ[#]&)[ind__]:>ind,a_?(SuperfieldQ[#]||FieldQ[#]&)]/.Pattern->MyPattern/.MyPattern[a_,_]:>a;
   righthandind=DeleteCases[If[Head[#[[2]]]===Times||Head[#[[2]]]===Plus,List@@#[[2]],{#[[2]]}] /.a_?(#===MyDelta||#===MyEps||#===MyPauli||TensQ[#]&)[ind__]:>Ind[ind],_?(!MatchQ[#,Ind[__]]&)]/.Ind[ind__]:>ind;

   (*An index should be repeated only twice!!!*)
   If[Cases[Tally[righthandind],List[ind_,n_?(#>2&)]]=!={},
     Print[Style[{"There seems to be an error in the indices. There is at least one that is repeated more than twice in coupling ",#},Red]];Abort[]];

   (*If an index is repeated twice in the right-hand side then it should NOT appear in the left-hand side*)
   If[Intersection[{#/.Index[_,ind_]:>ind},(lefthandind/.Index[_,ind_]:>ind)]=!={},
     Print[Style[{"There seems to be an error in the indices. There is at least one that is repeated more than twice in the couplings "},Red]];Abort[]]&/@Cases[Tally[righthandind],List[ind_,2]][[All,1]];

   (*Now everything should be ok, indices that only appear in the right-hand side should have a module*)
   tmpresu=MRuleDelayed[#[[1]],MModule[(Cases[Tally[righthandind],List[_,2]][[All,1]]/.Index[_,ind_]->ind),#[[2]]]];
   tmpresu//.{MRuleDelayed->RuleDelayed,MModule->Module,MyPauli->PauliSigma,MyDelta->IndexDelta,MyEps->SUEps}
 )&/@tmplist;

  Return[myresu]
];


(* ::Section:: *)
(*RGEs for the gauge coupling constant*)


(* ::Subsection:: *)
(*Calculation of the first coefficients of the beta function*)


ComputeBetag1[]:=Module[{indxSF=(SF2Indices[#]&/@M$ChiralSuperfieldNames)/.Index[aa__]->aa},
  FR$betag1 = GroupToCoup[#]^3*If[Not[AbelianQ[#]],

   (* Non-abelian case: sum_superfields[multiplicity * Dinkyn] ] *)
     Module[{indxGrp=GroupToReps[#][[All,2]],t1,t2},
       t1=Times@@((IndexDim/@(Complement[#,indxGrp]))/.List[]->{1})&/@indxSF;
       t2=Times@@@(Intersection[#,indxGrp]&/@indxSF/.GroupToDynkins[#]/.List[]->{0});
       t1.t2-3 GroupToAdj[#]/.GroupToCasimirs[#]],

   (* Abelian case: sum_superfields[multiplicity * Dinkyn] = normalization * sum_superfields[multiplicity * charge^2] *)
     GroupToNorm[#]*Module[{multiplicity, sumch},
       multiplicity=(Times@@(IndexDim/@#))&/@indxSF;
       sumch=GroupToCharge[#]^2/.(SF2QNumbers[#]&/@M$ChiralSuperfieldNames)/.GroupToCharge[#]->0;
       Inner[Times,multiplicity,sumch,Plus]]] &/@MR$GaugeGroupList]


(* ::Subsection::Closed:: *)
(*Calculation of the second coefficients of the beta function*)


ComputeBetag2[superpot_]:=Module[{cr, cg, yuka,tsf1,indxSF,reps,coup},
(* We need the FR$SuperWRules variable *)
   If[Head[superpot]===List,superpot,ExtractSuperWTerms[superpot,SuperfieldQ]];
(* indices of all superfields *)
   indxSF=(SF2Indices[#]&/@M$ChiralSuperfieldNames)/.Index[aa__]->aa;
(* list of all existing chiral superfields with their indices *)
   tsf1= Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
(* cr = Sum[ g_ {b}^2 s_ {a}(R) c_b (R), b ] *)
   cr = GroupToCoup[#]^2*If[ AbelianQ[#],
    GroupToNorm[#]* (GroupToCharge[#]/@M$ChiralSuperfieldNames )^2,
    reps = GroupToReps[#][[All,2]]; ((Intersection[#,reps]&/@ indxSF)/.GroupToCasimirs[#]) /.{} -> 0/.{a_}->a ] &/@ MR$GaugeGroupList;

  FR$betag2 = If[Not[AbelianQ[#]],
   coup=GroupToCoup[#];
   (* Non-abelian case *)
   Module[{indxGrp=GroupToReps[#][[All,2]],t1,t2},
    cg = GroupToAdj[#]/.GroupToCasimirs[#];
    t1=Times@@((IndexDim/@(Complement[#,indxGrp]))/.List[]->{1})&/@indxSF;
    t2=Times@@@(Intersection[#,indxGrp]&/@indxSF/.GroupToDynkins[#]/.List[]->{0});
   (* yukawa part *)
   yuka=Plus@@Flatten[Table[ 
      FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ] Conjugate[FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ]]*((Intersection[(SF2Indices[tsf1[[ll]] /.a_[__]->a]/.Index[a_]->a), GroupToReps[#][[All,2]]]/.GroupToCasimirs[#] )/IndexDim[GroupToAdj[#]] )/.FR$SuperWRules, {ll,1,Length[M$ChiralSuperfieldNames]},{nn,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]}]];
   (*Removed the OptimizeIndex on july 6th. See reason at the top of the package.*)
   (*yuka=OptimizeIndex[yuka/.IndexDelta[Index[type_,ind_],Index[type_,ind_]]:>IndexDim[type]];*)
   yuka=yuka/.IndexDelta[Index[type_,ind_],Index[type_,ind_]]:>IndexDim[type];

   (coup^5*(-6*cg^2+ 2*cg*GetDynkin[#])+ (coup^3*Plus@@Flatten[(4*t1*t2*#)&/@ cr]) - coup^3*yuka)/(16*Pi^2)],

  (* Abelian case *)
   Module[{multiplicity,sr},
    coup=GroupToCoup[#];
    multiplicity=(Times@@(IndexDim/@#))&/@indxSF;
    sr = GroupToNorm[#]* (GroupToCharge[#]/@M$ChiralSuperfieldNames)^2;
  (* Yukawa part *)
   yuka=Plus@@Flatten[Table[FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ] Conjugate[FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ]]*(GroupToCharge[#][tsf1[[ll]]/.a_[___]->a ])^2/.FR$SuperWRules,{ll,1,Length[M$ChiralSuperfieldNames]},{nn,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]}]];
   (*yuka=OptimizeIndex[yuka/.IndexDelta[Index[type_,ind_],Index[type_,ind_]]:>IndexDim[type]];*)
   yuka=yuka/.IndexDelta[Index[type_,ind_],Index[type_,ind_]]:>IndexDim[type];
   (coup^3*Plus@@Flatten[Plus@@(4*multiplicity*#*sr) &/@ cr] - coup^3*(GroupToNorm[#])*yuka)/(16*Pi^2)]]&/@MR$GaugeGroupList;
  FR$betag2=FR$betag2/.SUEps[arg1_,arg2_]^2:> Factorial[Length[{arg1,arg2}]]];


(* ::Subsection:: *)
(*Computation of the RGEs*)


(* ::Text:: *)
(*This function compute the beta function for each gauge group and returns the RGE associated to the evolution of the coupling constant.*)


Options[GaugeCouplingsRGE]={NLoop -> 2};


GaugeCouplingsRGE[superpot_,OptionsPattern[]]:=Module[{DeriveRGE,DeriveRGE2},
  (* Two loop only if nloop ==2 *)
  Which[OptionValue[NLoop]==1,FR$betag2=0, OptionValue[NLoop]==2, ComputeBetag2[superpot], True, Print["Only 2 possible choices for loop leve, 1 or 2"]; Abort[]];
  (* Compute beta function coefficients if necessary *)
  ComputeBetag1[];
  (* The RGE itself *)
  DeriveRGE[coup_,beta1_]:=FormattingRGE[FR$D[coup] == Expand[beta1/(16 Pi^2)]];
  Inner[DeriveRGE,GroupToCoup/@MR$GaugeGroupList,FR$betag1+FR$betag2,List]];


(* ::Section:: *)
(*RGE for the gaugino mass terms*)


(* ::Text:: *)
(*This function extract the gaugino mass terms from LSoft, test that they are diagonal and hermitian, and returns the associated RGEs.*)
(*LagSoft must be given in terms of component fields (Weyl fermion, not Dirac).*)
(*Note that the ino masses ordering corresponds to the one of the gauge groups.*)


(* ::Subsection::Closed:: *)
(*2 Loop beta function for gaugino mass terms*)


ComputeBetaMi2[superpot_,LagSoft_,inomasses_]:= Module[{M,tsf1,tsf2,indxSF,reps,yuka, cr, indx1,indx2,type},
(* Check whether FR$SuperWRules and FR$SoftRules are defined *)
   If[ FR$SuperWRules =={}, ExtractSuperWTerms[superpot,SuperfieldQ]];
   If[ FR$SoftRules =={}, SoftParameters[LagSoft]];
(* indices of all superfields *)
   indxSF=( SF2Indices[#]&/@M$ChiralSuperfieldNames)/.Index[aa__]->aa;
(* list of all existing chiral superfields with their indices *)
   tsf1=Indexify/@ M$ChiralSuperfieldNames;
   tsf2=#/.a_[ind__]:>SF2Scalar[a][ind] &/@tsf1;
(* cr = Sum[ g_ {b}^2 s_ {a}(R) c_b (R)*(Mxa + Mxb), b ] *)
   cr=GroupToCoup[MR$GaugeGroupList[[#]]]^2*(inomasses[[#]] + M )*If[ AbelianQ[MR$GaugeGroupList[[#]]],
     GroupToNorm[MR$GaugeGroupList[[#]]]*( GroupToCharge[MR$GaugeGroupList[[#]]]/@M$ChiralSuperfieldNames )^2,
    reps= GroupToReps[MR$GaugeGroupList[[#]]][[All,2]]; ((Intersection[#,reps]&/@ indxSF)/. GroupToCasimirs[MR$GaugeGroupList[[#]]]) /.{} -> 0/.{a_}->a ] &/@Range[Length[ MR$GaugeGroupList]];
   FR$betaMi2 = If[Not[ AbelianQ[MR$GaugeGroupList[[#]] ]],
(* Non-abelian case *)
    Module[{indxGrp= GroupToReps[MR$GaugeGroupList[[#]] ][[All,2]],t1,t2,cg},
     cg =  GroupToAdj[MR$GaugeGroupList[[#]] ]/. GroupToCasimirs[MR$GaugeGroupList[[#]] ];
      t1=Times@@(( IndexDim/@(Complement[#,indxGrp]))/.List[]->{1})&/@indxSF;
      t2=Times@@@(Intersection[#,indxGrp]&/@indxSF/. GroupToDynkins[MR$GaugeGroupList[[#]] ]/.List[]->{0});
(* yukawa part *)
     yuka=Flatten[Table[(FR$Soft[tsf2[[mm]],tsf2[[nn]],tsf2[[ll]]]*Conjugate[FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ]]- inomasses[[#]]* FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ]*Conjugate[FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ]]) *((Intersection[( SF2Indices[tsf1[[ll]] /.a_[__]->a]/.Index[a_]->a),  GroupToReps[MR$GaugeGroupList[[#]]][[All,2]]]/. GroupToCasimirs[MR$GaugeGroupList[[#]]] )/ IndexDim[ GroupToAdj[MR$GaugeGroupList[[#]]]] )/.FR$SoftRules/.FR$SuperWRules,
       {ll,1,Length[M$ChiralSuperfieldNames]},{nn,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]}]];
     yuka=Expand[Plus@@(yuka)];
      GroupToCoup[MR$GaugeGroupList[[#]] ]^2 *  (-24*cg^2+ 8*cg*GetDynkin[MR$GaugeGroupList[[#]] ])*inomasses[[#]]+ Plus@@(Flatten[(8*t1*t2*#)&/@ cr]/.M -> inomasses[[ # ]] ) + 2*yuka],
(* Abelian case *)
    Module[{multiplicity,sr},
     multiplicity=(Times@@(IndexDim/@#))&/@indxSF;
     sr =  GroupToNorm[MR$GaugeGroupList[[#]]]* ( GroupToCharge[MR$GaugeGroupList[[#]]]/@M$ChiralSuperfieldNames)^2;
(* Yukawa part *)
     yuka=Flatten[Table[(FR$Soft[tsf2[[mm]],tsf2[[nn]],tsf2[[ll]]] *Conjugate[FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ]] - inomasses[[#]]* FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ] *Conjugate[FR$SuperW[tsf1[[mm]],tsf1[[nn]],tsf1[[ll]] ]]) *( GroupToCharge[MR$GaugeGroupList[[#]]][tsf1[[ll]]/.a_[___]->a ])^2/.FR$SuperWRules/.FR$SoftRules,
       {ll,1,Length[M$ChiralSuperfieldNames]},{nn,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]}]];
     yuka=Plus@@(yuka);
     Plus@@(Flatten[Plus@@(8*multiplicity*#*sr) &/@ cr]/.M -> inomasses[[ # ]]) + 2*(GroupToNorm[MR$GaugeGroupList[[#]]])*yuka]] &/@Range[Length[MR$GaugeGroupList]];
   FR$betaMi2=FR$betaMi2//.{SUEps[arg1_,arg2_]^2:> Factorial[Length[{arg1,arg2}]], SUEps[arg1_,arg2_]SUEps[arg2_,arg1_]:>Factorial[Length[{arg1,arg2}]],IndexDelta[Index[type_,ind_],Index[type_,ind_]]:>IndexDim[type]} ];


(* ::Subsection::Closed:: *)
(*Computing the Gaugino RGEs*)


Options[GauginoMassesRGE]= { NLoop->2};


GauginoMassesRGE[superpot_,LagSoft_,OptionsPattern[]]:= Module[{inomasses, Lmass, b1g2, indxSF,loop,DeriveRGE},
    (* Get the gaugino mass parameters *)     
  inomasses=GauginoMasses[LagSoft][[1]];
 
  (* Calculation of the first beta function times g^2 *)
  If[FR$betag1=={}, ComputeBetag1[]];
  FR$betaMi1=Inner[Times,2/ GroupToCoup/@MR$GaugeGroupList,FR$betag1,List];
  FR$betaMi1=Inner[Times,FR$betaMi1,inomasses,List];
   (* Calculation of the second beta function *)
  Which[OptionValue[NLoop]==1, FR$betaMi2=0,OptionValue[NLoop]==2,FR$betaMi2=Inner[Times,(GroupToCoup/@MR$GaugeGroupList)^2/(16*Pi^2),ComputeBetaMi2[superpot,LagSoft,inomasses],List],True,Print["Only 2 possible choices for loop leve, 1 or 2"]; Abort[]];

  (* Collect all the pieces together *)
  DeriveRGE[inomass_,beta_]:= FormattingRGE[FR$D[inomass] == Expand[beta/(16 Pi^2) ]];
  Inner[DeriveRGE,inomasses,FR$betaMi1 + FR$betaMi2,List]];


(* ::Section::Closed:: *)
(*RGEs for the superpotential parameters*)


(* ::Text:: *)
(*The superpotential must be given in terms of gauge-eigenstate superfields. *)
(*The code supports only linear, bilinear and trilinear interactions.*)
(*All the indices MUST be contracted. Therefore, SU(N) invariant product must be included within the SUDot environment without any unfolding.*)
(*One term is then associated to each parameter.*)
(*Colour and Colourb are reserved names for the color indices.*)


(* ::Subsection:: *)
(*One-Loop Anomalous dimension matrices*)


(* ::Text:: *)
(*This function calculate the anomalous dimension matrices associated with the different chiral superfields*)


AnomDim[sf1_,sf2_]:=Module[{gauge,yuka,sfname1,sfname2,tsf1,tsf2,ind1,ind2},
  (*Initialization*)
  sfname1=sf1/.a_[__]->a;
  sfname2=sf2/.a_[__]->a;
  ind1=sf1/.a_?(#=!=Index&)[ii__]->{ii}; If[ind1===sf1,ind1={}];
  ind2=sf2/.a_?(#=!=Index&)[ii__]->{ii}; If[ind2===sf2,ind2={}];
  
  (* Gauge pieces *)
  gauge=If[sfname1===sfname2,
    Plus@@(
      Module[{casi}, 
        If[AbelianQ[#]===True,
         casi=(GUTNormalization Charge^2/.MR$GaugeGroupRules[#])/.(QuantumNumbers/.M$SuperfieldRules[sfname1]/.QuantumNumbers->{})/.GroupToCharge[#]->0,
         casi=Plus@@(Intersection[$IndList[sfname1]/.Index[a_]->a,GroupToReps[#][[All,2]]]/.GroupToCasimirs[#])];
        -2 casi GroupToCoup[#]^2]&/@ MR$GaugeGroupList),0] Inner[IndexDelta,ind1,ind2,Times];

  (* Yukawa pieces *)
  tsf1= Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  tsf2= Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  yuka=Plus@@Flatten[Table[
    FR$SuperW[sf1 ,tsf1[[mm]],tsf2[[nn]]] Conjugate[FR$SuperW[sf2,tsf1[[mm]],tsf2[[nn]]]]/.FR$SuperWRules//.{
      SUEps[argx__]^2:>Factorial[Length[{argx}]],
      SUEps[arg0___,arg1_,arg2___]SUEps[arg0p___,arg1_,arg2p___]:>
         Inner[IndexDelta,{arg0},{arg0p},Times]*Inner[IndexDelta,{arg2},{arg2p},Times]Factorial[Length[{arg0,arg2}]]},
    {nn,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]}]];
  yuka=yuka/.IndexDelta[Index[type_,ind_],Index[type_,ind_]]:>IndexDim[type];

  (* everything *)
(*  OptimizeIndex[Expand[(gauge+yuka/2)]]*)

Expand[(gauge+yuka/2)]
];


(* ::Subsection::Closed:: *)
(*Gauge Piece for the 2loop anomalous dimension*)


(* ::Text:: *)
(*This function calculates the term g^4 [C(i) S(R) + 2*C(i)^2 - 3*C*(G) C(i)] that is found in the anom dim of yukawas and soft breaking terms*)


Options[GaugePiece]={GauginoMass->2,NoCasimir->False}


GaugePiece[sfname_,OptionsPattern[]]:= Module[{ci,sr,cg,multiplicity,coup,gauginomass,nocasimir},
  ci = If[AbelianQ[#],
  (* Abelian case *)
    List[GroupToCoup[#]^2*FCasimir[sfname,#],GetDynkin[#],0,Times@@(IndexDim/@($IndList[sfname]/.Index[a_]->a)),GroupToNorm[#]*GroupToCoup[#]^2],
  (* Non abelian case *)
    List[GroupToCoup[#]^2*FCasimir[sfname,#],GetDynkin[#],GroupToAdj[#]/.GroupToCasimirs[#],
      Times@@IndexDim/@(Complement[$IndList[sfname]/.Index[a_]->a,GroupToReps[#][[All,2]]]),GroupToCoup[#]^2]]&/@MR$GaugeGroupList;
  gauginomass=OptionValue[GauginoMass];
  nocasimir=If[OptionValue[NoCasimir],0,1];
  sr = Flatten[ci[[All,2]]]; cg = Flatten[ci[[All,3]]]; multiplicity = ci[[All,4]]; coup = ci[[All,5]]; ci = Flatten[ci[[All,1]]];
  Expand[Plus@@(ci*sr*gauginomass*coup + 2*Plus@@((nocasimir*gauginomass*ci*#)&/@ci) - 3*(cg*ci)*gauginomass*coup)]];


(* ::Subsection:: *)
(*Two-loop Anomalous dimension matrices*)


(* ::Text:: *)
(*This function calculate the anomalous dimension matrices associated with the different chiral superfields*)


ComputeBeta2sup[sf1_,sf2_]:= Module[{sfname1, sfname2, ind1,ind2,gauge, yuka,MyRule},
(*Initialization*)
  sfname1=sf1/.a_[__]->a;
  sfname2=sf2/.a_[__]->a;
  ind1=sf1/.a_?(#=!=Index&)[ii__]->{ii}; If[ind1===sf1,ind1={}];
  ind2=sf2/.a_?(#=!=Index&)[ii__]->{ii}; If[ind2===sf2,ind2={}];
  If[FR$Common=={}, GetCommonPart[]];
  MyRule=FR$SuperWRules/.{IndexDelta[a__]:>MyDelta[a], SUEps[a__]:> MyEps[a]};

(*gauge piece *)
  gauge = If[sfname1===sfname2,Inner[IndexDelta,ind1,ind2,Times]*GaugePiece[sfname1],0];

(* Yukawa part *)
  yuka=Module[{temp},
    temp=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sfname1&)[ind__],f2_,f3_],FR$Temp[f3_,f4_,f5_],FR$Temp[f4_,f5_,f6_]]:>Conjugate[FR$SuperW[sf1,f2,f3]] FR$SuperW[f3,f4,f5]Conjugate[FR$SuperW[f4,f5,f6]]FR$SuperW[f2,f6,sf2];
    temp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[fields__],FR$Temp[f3_?(#===sfname1&)[ind__],f4_,f5_],FR$Temp[f4_,f5_,f6_?(#===sfname2&)[ind2__]]]:>Conjugate[FR$SuperW[sf1,f4,f5]]FR$SuperW[sf2,f4,f5]*GroupToCoup[#]^2*(2*FCasimir[f4,#]-FCasimir[sf1,#])&/@MR$GaugeGroupList;
    temp=(temp/.KeepOrdered[__]->0)/.MyRule];
(*  The everything *)
(*  FormattingExpr[OptimizeIndex[Expand[2*gauge + yuka]]]*)
    Expand[2*gauge + yuka]//.{MyEps->SUEps,MyDelta->IndexDelta}
];


(* ::Subsection::Closed:: *)
(*Computation of the beta function*)


(* ::Subsubsection::Closed:: *)
(*Summing over all 1 loop beta functions*)


ComputeBetaSuperW11[superw1_]:=Module[{sfs},
  sfs=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];

  FR$betaSW11=betaSuperW11[Sequence@@#[[2]],sfs]&/@ superw1;
];


ComputeBetaSuperW21[superw2_]:=Module[{sfs},
  sfs=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];

  FR$betaSW21=betaSuperW21[Sequence@@#[[2]],sfs]&/@ superw2;
];


ComputeBetaSuperW31[superw3_]:=Module[{sfs},
  sfs=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];

  FR$betaSW31=betaSuperW31[Sequence@@#[[2]],sfs]&/@ superw3;
];


(* ::Subsubsection::Closed:: *)
(*Summing over all 2 loop beta functions*)


ComputeBetaSuperW12[superw1_]:=Module[{sfs},
  sfs=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];

  FR$betaSW12=betaSuperW12[Sequence@@#[[2]],sfs]&/@ superw1;
];


ComputeBetaSuperW22[superw2_]:=Module[{sfs},
  sfs=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];

  FR$betaSW22=betaSuperW22[Sequence@@#[[2]],sfs]&/@ superw2;
];


ComputeBetaSuperW32[superw3_]:=Module[{sfs},
  sfs=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];

  FR$betaSW32=betaSuperW32[Sequence@@#[[2]],sfs]&/@ superw3;
];


(* ::Subsubsection::Closed:: *)
(*One specific 1 loop beta function*)


betaSuperW31[field1_,field2_,field3_,sfs_]:= Plus@@Table[
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]], AnomDim[field1,sfs[[nn]]] FR$SuperW[sfs[[nn]],field2,field3],0]+
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field2/.sff_[inds__]->sff]], AnomDim[field2,sfs[[nn]]] FR$SuperW[field1,sfs[[nn]],field3],0]+
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field3/.sff_[inds__]->sff]], AnomDim[field3,sfs[[nn]]] FR$SuperW[field1,field2,sfs[[nn]]],0],
{nn,1,Length[M$ChiralSuperfieldNames]}]//.FR$SuperWRules;


betaSuperW21[field1_,field2_,sfs_]:= Plus@@Table[
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]], AnomDim[field1,sfs[[nn]]] FR$SuperW[sfs[[nn]],field2],0]+
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field2/.sff_[inds__]->sff]], AnomDim[field2,sfs[[nn]]] FR$SuperW[field1,sfs[[nn]]],0],
{nn,1,Length[M$ChiralSuperfieldNames]}]//.FR$SuperWRules;


betaSuperW11[field1_,sfs_]:=Plus@@Table[
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]], AnomDim[field1,sfs[[nn]]] FR$SuperW[sfs[[nn]]],0],
{nn,1,Length[M$ChiralSuperfieldNames]}]//.FR$SuperWRules;


(* ::Subsubsection::Closed:: *)
(*One specific 2 loop beta function*)


betaSuperW32[field1_,field2_,field3_,sfs_]:= Plus@@Table[
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]], ComputeBeta2sup[field1,sfs[[nn]]] FR$SuperW[sfs[[nn]],field2,field3],0]+
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field2/.sff_[inds__]->sff]], ComputeBeta2sup[field2,sfs[[nn]]] FR$SuperW[field1,sfs[[nn]],field3],0]+
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field3/.sff_[inds__]->sff]], ComputeBeta2sup[field3,sfs[[nn]]] FR$SuperW[field1,field2,sfs[[nn]]],0],
{nn,1,Length[M$ChiralSuperfieldNames]}]//.FR$SuperWRules;


betaSuperW22[field1_,field2_,sfs_]:= Plus@@Table[
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]], ComputeBeta2sup[field1,sfs[[nn]]] FR$SuperW[sfs[[nn]],field2],0]+
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field2/.sff_[inds__]->sff]], ComputeBeta2sup[field2,sfs[[nn]]] FR$SuperW[field1,sfs[[nn]]],0],
{nn,1,Length[M$ChiralSuperfieldNames]}]//.FR$SuperWRules;


betaSuperW12[field1_,sfs_]:=Plus@@Table[
  If[Sort[$IndList[sfs[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]], ComputeBeta2sup[field1,sfs[[nn]]] FR$SuperW[sfs[[nn]]],0],
{nn,1,Length[M$ChiralSuperfieldNames]}]//.FR$SuperWRules;


(* ::Subsection:: *)
(*Formatting the RGE*)


(* ::Text:: *)
(*Old version --- keep it as backup -- 07.06.2013*)


(*
FormattingRGE[expr_]:=Block[{tmp=Expand[expr],MyIndexDelta,MySUEps,MyEps},
  (*Some properties*)
  MyIndexDelta[Index[typ_,ind_],Index[typ_,ind_]]:=PRIVATE`IndexDim[typ];
  MyIndexDelta/:MyIndexDelta[ind1_,ind2_]MyIndexDelta[ind2_,ind3_]=MyIndexDelta[ind1,ind3];

  MyEps/:MyEps[argx__]^2:=Factorial[Length[{argx}]];
  MyEps/:MyEps[argx__]^4:=(Factorial[Length[{argx}]])^2;
  MyEps/:MyEps[arg0___,arg1_,arg2___]MyEps[arg0p___,arg1_,arg2p___]:=If[Length[{arg0p}]===Length[{arg0}],
        Inner[IndexDelta,{arg0},{arg0p},Times]*Inner[IndexDelta,{arg2},{arg2p},Times]Factorial[Length[{arg0,arg2}]], 
       (-1)^(Length[{arg2p}]+(Length[{arg2p}]+1)*Length[{arg0p}])*Inner[IndexDelta,{arg0},{arg2p},Times]*Inner[IndexDelta,{arg2},{arg0p},Times]Factorial[Length[{arg0,arg2}]]];

  (*Removing Deltas and SUEps from right-hand side*)
  tmp=tmp//.{
    (*Removing IndexDelta*)
    Equal[FR$D[IndexDelta[ind1__]para__],beta__]:>Equal[FR$D[MyIndexDelta[Sequence@@Reverse[{ind1}]]MyIndexDelta[ind1]para],Expand[MyIndexDelta[Sequence@@Reverse[{ind1}]]beta]],
    (*Removing SUEps*)
    Equal[FR$D[SUEps[ind1__]para__],beta__]:>Equal[FR$D[MySUEps[Sequence@@Reverse[{ind1}]]MySUEps[ind1]para],Expand[SUEps[Sequence@@Reverse[{ind1}]]beta]]
  }//.{MySUEps->SUEps};

  (*Contracting SUEps*)
  tmp=tmp//.{
      SUEps[argx__]^2:>Factorial[Length[{argx}]],
      SUEps[argx__]^4:>(Factorial[Length[{argx}]])^2,
      SUEps[arg0___,arg1_,arg2___]SUEps[arg0p___,arg1_,arg2p___]:>If[Length[{arg0p}]===Length[{arg0}],
        Inner[IndexDelta,{arg0},{arg0p},Times]*Inner[IndexDelta,{arg2},{arg2p},Times]Factorial[Length[{arg0,arg2}]], 
       (-1)^(Length[{arg2p}]+(Length[{arg2p}]+1)*Length[{arg0p}])*Inner[IndexDelta,{arg0},{arg2p},Times]*Inner[IndexDelta,{arg2},{arg0p},Times]Factorial[Length[{arg0,arg2}]]]
   };

  (*Deltas that appear now are from the SUEps contraction and should have value 1 in the case IndexDelta[ind,ind]*)
  tmp=tmp//.Equal[FR$D[IndexDelta[ind__]para_],beta__]:>Equal[FR$D[para],Expand[beta/IndexDelta[ind]]]//.IndexDelta[ind_,ind_]:>1;

  (*Removing numbers from left-hand side*)
  tmp=tmp//.{Equal[FR$D[a_?(NumericQ[#]&)para_],beta__]:>Equal[FR$D[para],Expand[beta/a]],Equal[FR$D[-a_],expre_]:>Equal[FR$D[a],Expand[-expre]]};

  (*Only deltas contracting PauliSigma should remain -> it should be safe to remove the type of the index*)
  tmp=tmp//.{IndexDelta[Index[_,a_],ind___]->IndexDelta[a,ind],IndexDelta[ind___,Index[_,a_]]->IndexDelta[ind,a]};

  (*The complex conjugate of PauliSigma is its hermitian conjugate. Here I only put the transverse*)
  tmp=tmp//.Conjugate[PauliSigma[ind1_,ind2_,ind3_]fac_]:>PauliSigma[ind1,ind3,ind2]Conjugate[fac];

  (*Contract Deltas first*)
  tmp=tmp//.{IndexDelta[ind1_,ind2_]fac_[ind3___,ind1_,ind4___]:>fac[ind3,ind2,ind4],IndexDelta[ind1_,ind2_]fac_[ind3___,ind2_,ind4___]:>fac[ind3,ind1,ind4]};

  (*If a PauliSigma is in the left-hand side of the equation, bring it to the right side*)
  tmp=tmp//.Equal[FR$D[PauliSigma[ind1_,ind2_,ind3_]para_],beta__]:>Equal[FR$D[para],Expand[PauliSigma[ind1,ind3,ind2]/2*beta]];

  (*Contract Sigmas*)
  tmp=tmp//.{PauliSigma[ind1_,ind2_,ind3_]PauliSigma[ind1_,ind3_,ind5_]para___:>3*IndexDelta[ind2,ind5]para/;(ind2=!=ind5),
             PauliSigma[ind1_,ind2_,ind3_]PauliSigma[ind1_,ind3_,ind2_]para___:>6*para,
             PauliSigma[ind1_,ind2_,ind3_]PauliSigma[ind1_,ind4_,ind5_]:>(2*IndexDelta[ind2,ind5]IndexDelta[ind3,ind4]-IndexDelta[ind2,ind3]IndexDelta[ind4,ind5])/;(ind2=!=ind4&&ind3=!=ind5)};

  (*Optimize Index*)
  tmp=tmp/.Equal[a_,b_]:>Equal[a,OptimizeIndex[Expand[b]]];

  (*Return result*)
  Return[tmp]
];
*)


FormattingRGE[rge_]:=Block[{MyIndexDelta,MyEps,MyPauli,tmp},
  (*Restoring Index types*)
  (* This is to convert a[ind1,ind2] b[Index[typ,ind1],ind3] to a[Index[typ,ind1],ind2] b[Index[typ,ind1],ind3] *)
  tmp=rge//.{a_[ind___,ind1_?(Head[#]=!=Index&),ind2_]b_[Index[typ_,ind1_],ind3_]:>a[ind,Index[typ,ind1],ind2]b[Index[typ,ind1],ind3],
             a_[ind___,ind1_?(Head[#]=!=Index&),ind2_]b_[ind3_,Index[typ_,ind1_]]:>a[ind,Index[typ,ind1],ind2]b[ind3,Index[typ,ind1]]};
  (*Some definitions*)
  MyIndexDelta[Index[typ_,ind_],Index[typ_,ind_]]:=IndexDim[typ];
  MyIndexDelta/:MyIndexDelta[ind1_,ind2_]fac_[ind3___,ind1_,ind4___]:=fac[ind3,ind2,ind4];
  MyIndexDelta/:MyIndexDelta[ind1_,ind2_]fac_[ind3___,ind2_,ind4___]:=fac[ind3,ind1,ind4];
  MyIndexDelta/:MyIndexDelta[Index[typ_,ind1_],ind2_]fac_[ind3___,ind1_,ind4___]:=fac[ind3,ind2,ind4];
  MyIndexDelta/:MyIndexDelta[ind1_,Index[typ_,ind2_]]fac_[ind3___,ind2_,ind4___]:=fac[ind3,ind1,ind4];
  MyIndexDelta/:(MyIndexDelta[ind1_,ind2_])^2:=If[ind1=!=ind2,1];
  MyIndexDelta/:(MyIndexDelta[ind1_,ind1_]):=2;
  MyIndexDelta/:MyIndexDelta[ind_?(Head[#]=!=Index&),Index[typ_,ind2_]]:=MyIndexDelta[Index[typ,ind],Index[typ,ind2]];
  MyIndexDelta/:MyIndexDelta[Index[typ_,ind2_],ind_?(Head[#]=!=Index&)]:=MyIndexDelta[Index[typ,ind2],Index[typ,ind]];
  MyIndexDelta/:(MyIndexDelta[Index[typ_,ind1_],Index[typ_,ind2_]])^2:=If[ind1=!=ind2,1];
  MyIndexDelta/:(MyIndexDelta[Index[typ_,ind1_],Index[typ_,ind1_]]):=2;



  MyEps/:MyEps[argx__]^2:=Factorial[Length[{argx}]];
  MyEps/:MyEps[argx__]^-2:=Factorial[Length[{argx}]];
  MyEps/:MyEps[arg0___,arg1_,arg2___]MyEps[arg0p___,arg1_,arg2p___]:=If[Length[{arg0p}]===Length[{arg0}],
        Inner[MyIndexDelta,{arg0},{arg0p},Times]*Inner[MyIndexDelta,{arg2},{arg2p},Times]Factorial[Length[{arg0,arg2}]], 
       (-1)^(Length[{arg2p}]+(Length[{arg2p}]+1)*Length[{arg0p}])*Inner[MyIndexDelta,{arg0},{arg2p},Times]*Inner[MyIndexDelta,{arg2},{arg0p},Times]Factorial[Length[{arg0,arg2}]]];
  MyEps/:MyEps[ind1_,ind2_]fac_?(#=!=MyIndexDelta||#=!=MyEps&)[ind___,ind1_,ind3___]:=-fac[ind,ind2,ind3];
  MyEps/:MyEps[ind2_,ind1_]fac_?(#=!=MyIndexDelta||#=!=MyEps&)[ind___,ind1_,ind3___]:=fac[ind,ind2,ind3];
  MyEps/:MyEps[ind1_,ind2_]Conjugate[fac_?(#=!=MyIndexDelta||#=!=MyEps&)[ind___,ind1_,ind3___]]:=-Conjugate[fac[ind,ind2,ind3]];
  MyEps/:MyEps[ind2_,ind1_]Conjugate[fac_?(#=!=MyIndexDelta||#=!=MyEps&)[ind___,ind1_,ind3___]]:=Conjugate[fac[ind,ind2,ind3]];
  MyEps/:MyEps[ind_?(Head[#]=!=Index&),Index[typ_,ind2_]]:=MyEps[Index[typ,ind],Index[typ,ind2]];
  MyEps/:MyEps[Index[typ_,ind2_],ind_?(Head[#]=!=Index&)]:=MyEps[Index[typ,ind2],Index[typ,ind]];
  MyEps/:MyEps[ind1_,ind2_]:=0/;(ind1===ind2);

  (*Simplify expressions: 1st round*)
  tmp=Expand[tmp]//.Conjugate[Times[a_,b_]]:>Times[Conjugate[a]Conjugate[b]];
  (*Use my spacename*)
  tmp=tmp//.{SUEps->MyEps,IndexDelta->MyIndexDelta,PauliSigma->MyPauli};

(* *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  ***** *)
(*  tmp=tmp//.{                                                                                                                                                       *
 *    (*Removing IndexDelta*)                                                                                                                                         * 
 *   Equal[FR$D[MyIndexDelta[ind1__]para_],beta__]:>Equal[FR$D[para],Expand[beta/MyIndexDelta[ind1]]],                                                                 *                                            
 *   (*Removing SUEps*)                                                                                                                                               *
 *   Equal[FR$D[MyEps[ind1__]para_],beta__]:>Equal[FR$D[para],Expand[beta/MyEps[ind1]]],                                                                              *                                                       
 *    (*Pauli sigmas *)                                                                                                                                               *
 *   Equal[FR$D[MyPauli[ind1_,ind2_,ind3_]para_],beta__]:>Equal[FR$D[para],Expand[beta/MyPauli[ind1,ind3,ind2]]],                                                     *                                                                                                          
 *   Equal[FR$D[a_?(NumericQ[#]&)para_],beta__]:>Equal[FR$D[para],Expand[beta/a]],Equal[FR$D[-a_],expre_]:>Equal[FR$D[a],Expand[-expre]],                             *
 *   MyPauli[Index[_,ind_],ind2__]:>MyPauli[ind,ind2]                                                                                                                 * 
 *}                                                                                                                                                                  * 
 * *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  *****  ***** *)

  (*Removing Deltas and SUEps from right-hand side*)
  tmp=tmp//.{
    (*Deltas*)
    Equal[FR$D[MyIndexDelta[ind1__]para__],beta__]:>Equal[FR$D[MyIndexDelta[ind1]MyIndexDelta[ind1]para],Expand[MyIndexDelta[ind1]beta]],
    (*Epsilon*)
    Equal[FR$D[MyEps[ind1__]para__],beta__]:>Equal[FR$D[MyEps[ind1]MyEps[ind1]para],Expand[MyEps[ind1]beta]],
    (*Pauli matrices*)
    Equal[FR$D[MyPauli[ind1_,ind2_,ind3_]para_],beta__]:>Equal[FR$D[para],Expand[MyPauli[ind1,ind3,ind2]/2*beta]],
      (*remove index type from Pauli matrices*)
    MyPauli[Index[_,ind_],ind2__]:>MyPauli[ind,ind2],
    (*Numbers*)
    Equal[FR$D[a_?(NumericQ[#]&)para_],beta__]:>Equal[FR$D[para],Expand[beta/a]],Equal[FR$D[-a_],expre_]:>Equal[FR$D[a],Expand[-expre]]
  };


  tmp=Expand[tmp]//.{Conjugate[MyPauli[ind1_,ind2_,ind3_]]*MyPauli[ind1_,ind4_,ind5_]:>3MyIndexDelta[ind2,ind5]MyIndexDelta[ind3,ind4]};

  tmp=Expand[tmp]//.{
    (*Not a matrix product *)
    Conjugate[MyPauli[a_,i_,j_]]MyPauli[a_,i_,j_]:>6,
    (*Completeness relation*)
    MyPauli[ind1_,ind2_,ind3_]MyPauli[ind1_,ind4_,ind5_]:>Expand[(2*MyIndexDelta[ind2,ind5]MyIndexDelta[ind3,ind4]-MyIndexDelta[ind2,ind3]MyIndexDelta[ind4,ind5])],
    Conjugate[MyPauli[ind1_,ind2_,ind3_]]MyPauli[ind1_,ind4_,ind5_]:>Expand[(2*MyIndexDelta[ind2,ind5]MyIndexDelta[ind3,ind4]-MyIndexDelta[ind2,ind3]MyIndexDelta[ind4,ind5])]};

  (*re-apply the same rules*)
  tmp=Expand[tmp]//.{
    Conjugate[MyPauli[a_,i_,j_]]MyPauli[a_,i_,j_]:>6,
    MyPauli[ind1_,ind2_,ind3_]MyPauli[ind1_,ind4_,ind5_]:>Expand[(2*MyIndexDelta[ind2,ind5]MyIndexDelta[ind3,ind4]-MyIndexDelta[ind2,ind3]MyIndexDelta[ind4,ind5])],
    Conjugate[MyPauli[ind1_,ind2_,ind3_]]MyPauli[ind1_,ind4_,ind5_]:>Expand[(2*MyIndexDelta[ind2,ind5]MyIndexDelta[ind3,ind4]-MyIndexDelta[ind2,ind3]MyIndexDelta[ind4,ind5])]};

  tmp=Expand[tmp]//.MyIndexDelta[__]^2->2//.MyIndexDelta[ind1_,ind1_]:>2;
  (*simplification of terms like (A[a,i,j] A[b,j,k]) / (A[a,ip,jp] A[b,ip,jp]) *)
  tmp=tmp//.{
    (MyPauli[a_,ind1_,ind2_]MyPauli[b_,ind1_,ind2_])/(MyPauli[a_,ind__]MyPauli[b_,ind__]):>1,
    (MyPauli[a_,ind2_,ind1_]MyPauli[b_,ind1_,ind2_])/(MyPauli[a_,ind__]MyPauli[b_,ind__]):>1,
    (Conjugate[MyPauli[a_,ind1_,ind2_]]MyPauli[b_,ind1_,ind2_])/(MyPauli[a_,ind__]MyPauli[b_,ind__]):>1,
    (Conjugate[MyPauli[a_,ind2_,ind1_]]MyPauli[b_,ind1_,ind2_])/(MyPauli[a_,ind__]MyPauli[b_,ind__]):>1,
    MyPauli[a_,i_,i_]:>0,MyPauli[__]^2->2};
(*Line added on 6 july 2013 in order for the scalar fields' masses to display IndexDelta instead of PRIVATE`MyIndexDelta*)
  tmp=Expand[tmp]/.MyIndexDelta->IndexDelta;
  Return[tmp] ];



(* ::Subsection::Closed:: *)
(*Yukawas RGEs*)


YukawaRGEs[superpot_,nloop_]:=Module[{tmpp,tmpsup},
(*Allow for a direct usage without calling ExtractSuperWTerms before*)
  tmpsup=If[Head[superpot]===Plus,ExtractSuperWTerms[superpot,SuperfieldQ],superpot];
  ComputeBetaSuperW31[Cases[tmpsup,{_,a_?(Length[#]===3&)}]];
  If[nloop==2,ComputeBetaSuperW32[Cases[tmpsup,{_,a_?(Length[#]===3&)}]],FR$betaSW32=func/@Cases[tmpsup,{_,a_?(Length[#]===3&)}]/.func[__]->0 ];  
  tmpp=FR$betaSW31/(16*Pi^2)+FR$betaSW32/(16*Pi^2)^2;
  FormattingRGE[FormattingRGE[Inner[Equal,FR$D/@Cases[tmpsup,{_,a_?(Length[#]===3&)}][[All,1]],tmpp,List]]]];


(* ::Subsection::Closed:: *)
(*Bilinear RGEs*)


BilinearRGEs[superpot_,nloop_]:=Module[{tmpp,tmpsup},
(*Allow for a direct usage without calling ExtractSuperWTerms before*)
  tmpsup=If[Head[superpot]===Plus,ExtractSuperWTerms[superpot,SuperfieldQ],superpot];
  ComputeBetaSuperW21[Cases[tmpsup,{_,a_?(Length[#]===2&)}]];
  If[nloop==2,ComputeBetaSuperW22[Cases[tmpsup,{_,a_?(Length[#]===2&)}]],FR$betaSW22=func/@Cases[tmpsup,{_,a_?(Length[#]===2&)}]/.func[__]->0];
  tmpp=FR$betaSW21/(16*Pi^2)+FR$betaSW22/(16*Pi^2)^2;
  FormattingRGE[Inner[Equal,FR$D/@Cases[tmpsup,{_,a_?(Length[#]===2&)}][[All,1]],tmpp,List]]];


(* ::Subsection::Closed:: *)
(*Linear RGEs*)


LinearRGEs[superpot_,nloop_]:=Module[{tmpp,tmpsup},
(*Allow for a direct usage without calling ExtractSuperWTerms before*)
  tmpsup=If[Head[superpot]===Plus,ExtractSuperWTerms[superpot,SuperfieldQ],superpot];
  ComputeBetaSuperW11[Cases[tmpsup,{_,a_?(Length[#]===1&)}]];
  If[nloop==2,ComputeBetaSuperW12[Cases[tmpsup,{_,a_?(Length[#]===1&)}]],FR$betaSW12=func/@Cases[tmpsup,{_,a_?(Length[#]===1&)}]/.func[__]->0];
  tmpp=FR$betaSW11/(16*Pi^2)+FR$betaSW12/(16*Pi^2)^2;
  FormattingRGE[Inner[Equal,FR$D/@Cases[tmpsup,{_,a_?(Length[#]===1&)}][[All,1]],tmpp,List]]];


(* ::Subsection::Closed:: *)
(*Main routine for the RGEs*)


Options[SuperpotentialRGE]={NLoop->2};


SuperpotentialRGE[superpot_,OptionsPattern[]]:=Module[{tmp,tmp2,rge,func},

  (* Pre-processing of the Yukawa couplings *)
  tmp=ExtractSuperWTerms[superpot,SuperfieldQ];
  TestSuperW[tmp];

  (* beta functions and rges *)
  rge=YukawaRGEs[tmp,OptionValue[NLoop]];
  rge=Join[rge,BilinearRGEs[tmp,OptionValue[NLoop]]];
  rge=Join[rge,LinearRGEs[tmp,OptionValue[NLoop]]];
(*Printing RGEs*)
  rge];


(* ::Section:: *)
(*RGEs for the SUSY breaking parameters*)


(* ::Subsection::Closed:: *)
(*Extracting soft parameters from LSoft*)


(* ::Text:: *)
(*This functions return the three set of rules: ino masses, soft masses and soft interactions and prepares the list for the suspect interface.*)


SoftParameters[lsoft_]:=Module[{GetDim,tmp,Lino,MyTable,softmasses,softinteractions,inomasses},
  GetDim[exp_]:=Length[List@@(exp//.{SUDot[fields__,{__}]:>Times@@{fields},SUEps[___]->1,a_?(numQ[#]===True&)->1,a_?(FieldQ[#]===True&)[inds__]->a})];

  (* Extracting the various terms: ino masses, soft masses and interaction terms *)
  tmp=Expand[(-lsoft/.a_?(FermionQ[#]===True&)->0)];
  Lino=Expand[lsoft+tmp];
  tmp=If[Head[tmp]===Plus,List@@tmp,{tmp}];
  tmp=Plus@@DeleteCases[If[GetDim[#]>3,0,#]&/@tmp,0];
  tmp=ExtractSuperWTerms[tmp,FieldQ];
  softmasses=Cases[tmp,
    _?(MatchQ[#,List[_,List[_?(FieldQ[#]===True && AntiFieldQ[#]=!=True&),_?(AntiFieldQ[#]===True&)]]] || 
    MatchQ[#,List[_,List[_?(AntiFieldQ[#]===True&),_?(FieldQ[#]===True && AntiFieldQ[#]=!=True&)]]]&)];
  softinteractions=DeleteCases[tmp,
    _?(MatchQ[#,List[_,List[_?(FieldQ[#]===True && AntiFieldQ[#]=!=True&),_?(AntiFieldQ[#]===True&)]]] || 
    MatchQ[#,List[_,List[_?(AntiFieldQ[#]===True&),_?(FieldQ[#]===True && AntiFieldQ[#]=!=True&)]]]&)];
  inomasses=GauginoMasses[Lino][[1]];

  (* Get the scalar soft masses rules *)
  FR$ScalarMassRules=Select[FR$SoftRules,
    MatchQ[#,Rule[FR$Soft[_?(FieldQ[#]===True && AntiFieldQ[#]=!=True &),_?(AntiFieldQ[#]===True&)],_]  ] || 
    MatchQ[#,Rule[FR$Soft[_?(AntiFieldQ[#]===True&),_?(FieldQ[#]===True && AntiFieldQ[#]=!=True &)],_]  ]&];
  FR$SoftRules=DeleteCases[FR$SoftRules, _?(MemberQ[FR$ScalarMassRules,#]&)];

  (* Output *)
{softmasses, inomasses,softinteractions}];


(* ::Subsection::Closed:: *)
(*Calculation of the beta functions for the soft parameters*)


(* ::Subsubsection::Closed:: *)
(*Main function computing the different 1 loop beta functions*)


ComputeBetaMs1[Lsca_, ino_]:=Module[{sfs1,sfs2,sfs3,sfsbar},
  (* Generic list of superfields *)
  sfs1=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfs2=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfs3=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfsbar=HC[#]&/@sfs1;
  (* The computation itself *)
  FR$betaMs1=betaMS1[Sequence@@#[[2]],ino,sfs1,sfs2,sfs3,sfsbar]&/@Lsca;
];


ComputeBetaSoftInt11[lsoft1_]:=Module[{sfs1,sfs2,sfs3,sfsbar},
  (* Generic list of superfields *)
  sfs1=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfs2=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfs3=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfsbar=HC[#]&/@sfs1;

  (* The computation itself *)
  FR$betaSI11=betaSoftInt11[Sequence@@#[[2]],sfs1,sfs2,sfs3,sfsbar]&/@ lsoft1;
];


ComputeBetaSoftInt21[lsoft2_,ino_]:=Module[{sfs1,sfs2,sfs3},
  (* Generic list of superfields *)
  sfs1=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfs2=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfs3=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];

  (* The computation itself *)
  FR$betaSI21=betaSoftInt21[Sequence@@#[[2]],ino,sfs1,sfs2,sfs3]&/@ lsoft2;
];


ComputeBetaSoftInt31[lsoft3_,ino_]:=Module[{sfs1,sfs2,sfs3},
  (* Generic list of superfields *)
  sfs1=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfs2=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  sfs3=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];

  (* The computation itself *)
  FR$betaSI31=betaSoftInt31[Sequence@@#[[2]],ino,sfs1,sfs2,sfs3]&/@ lsoft3;
];


(* ::Subsubsection:: *)
(*Main function computing the different 2 loop beta functions*)


ComputeBetaMs2[Lsca_, ino_]:=Module[{sfs1,sfs2,sfs3,sfsbar},
  FR$betaMs2=betaMS2[Sequence@@#[[2]],ino]&/@Lsca;
];


ComputeBetaSoftInt12[lsoft1_]:=Module[{sfs1,sfs2,sfs3,sfsbar},
  (* The computation itself *)
  FR$betaSI12=betaSoftInt12[];
];


ComputeBetaSoftInt22[lsoft2_,ino_]:=Module[{sfs1,sfs2,sfs3},
  (* The computation itself *)
  FR$betaSI22=betaSoftInt22[Sequence@@#[[2]],ino]&/@lsoft2;
];


ComputeBetaSoftInt32[lsoft3_,ino_]:=Module[{sfs1,sfs2,sfs3},
  (* The computation itself *)
  FR$betaSI32=betaSoftInt32[Sequence@@#[[2]],ino]&/@lsoft3;
];


(* ::Subsubsection::Closed:: *)
(*Calculation of the beta function for one single mass term: 1 Loop*)


betaMS1[field1_,field2_,inomasses_,sfs1_,sfs2_,sfs3_,sfsbar_]:=Module[{fi,afi,indfi,indafi,resu},
  (* Getting field and antifield, as well as indices *)
    If[AntiFieldQ[field1]===True, afi=field1; fi=field2, fi=field1; afi=field2];
    indfi=fi/.a_?(#=!=Index&)[ii__]->{ii}; If[indfi===fi,indfi={}];
    indafi=afi/.a_?(#=!=Index&)[ii__]->{ii}; If[indafi===afi,indafi={}];
  
(* Results *)  
  resu=(Plus@@Flatten[Table[
    If[Sort[$IndList[sfs1[[ll]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[anti[afi]/.sff_[inds__]->sff]],
      1/2 Conjugate[FR$SuperW[anti[afi],sfs3[[nn]],sfs2[[mm]]]] FR$SuperW[sfs3[[nn]],sfs2[[mm]],sfs1[[ll]]] FR$Soft[sfsbar[[ll]],fi],0]+
    If[Sort[$IndList[sfs3[[nn]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[fi/.sff_[inds__]->sff]],
      1/2 FR$SuperW[fi,sfs1[[ll]],sfs2[[mm]]] Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[nn]]] FR$Soft[afi,sfs3[[nn]]],0]+
    If[Sort[$IndList[sfs2[[mm]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[sfs1[[ll]]/.sff_[inds__]->sff]],
     2 Conjugate[FR$SuperW[anti[afi],sfs3[[nn]],sfs2[[mm]]]] FR$SuperW[fi,sfs3[[nn]],sfs1[[ll]]] FR$Soft[sfsbar[[ll]],sfs2[[mm]]],0],
  {ll,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]},{nn,1,Length[M$ChiralSuperfieldNames]}]])+
  (Plus@@Flatten[Table[
     Conjugate[FR$Soft[anti[afi],sfs1[[ll]],sfs2[[mm]]]] FR$Soft[fi,sfs1[[ll]],sfs2[[mm]]],
  {ll,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]}]])-
  8*If[(fi/.aaa_[inds__]->aaa)===(anti[afi]/.aaa_[inds__]->aaa), Dot[GFactor[fi],(# Conjugate[#] &/@inomasses)]*Inner[IndexDelta,indfi,indafi,Times],0]+
  2*TraceTerms[afi,fi];

  (* Restoring fields and superfields in the superpotential and soft terms *)
  resu=RestoreFields[resu];

  (* Simplifications *)
  resu=ReplaceSuperAndSoftTerms[resu];

(*Output *)
resu];


(* ::Subsubsection:: *)
(*Calculation of the beta function for one single mass term: 2 Loop*)


(* ::Text:: *)
(*The following two functions are specific to the scalar masses 2 loop beta function*)


(*This term is a trace over dynkin indices associated to the scalar fields of the theory weighted by the mass of each field*)
GetDynkinTrace[field_,inomasses_]:= Module[{field2,indxSF,multi,charge,casi,indxgr,sfs,tmpp},
  field2=If[MemberQ[M$ChiralSuperfieldNames,(field/.a_[__]->a)],field,HC[field]];
  sfs=Indexify/@M$ChiralSuperfieldNames;
  indxSF=($IndList/@M$ChiralSuperfieldNames)/.Index[a_]->a;
  tmpp=FCasimir[field2,MR$GaugeGroupList[[#]]]*If[AbelianQ[MR$GaugeGroupList[[#]]],
    multi=Times@@(IndexDim/@# )&/@indxSF;
    charge=GroupToCoup[MR$GaugeGroupList[[#]]]^4*GroupToNorm[MR$GaugeGroupList[[#]]]^2(GroupToCharge[MR$GaugeGroupList[[#]]]/@sfs)^2 (FR$Soft[#,HC[#]]&/@sfs) ;
    multi.charge,
    indxgr=GroupToReps[MR$GaugeGroupList[[#]]][[All,2]];
    multi=Times@@((IndexDim/@Complement[#,indxgr])/.List[]->{1})&/@ indxSF;
    casi=GroupToCoup[MR$GaugeGroupList[[#]]]^4*(FR$Soft[#,HC[#]]&/@sfs)*Flatten[(Intersection[#,indxgr] &/@ indxSF)/.GroupToDynkins[MR$GaugeGroupList[[#]]]/.List[]->0];
    multi.casi+((GroupToAdj[MR$GaugeGroupList[[#]]]/.GroupToCasimirs[MR$GaugeGroupList[[#]]])*inomasses[[#]]Conjugate[inomasses[[#]]])
  ]&/@Range[Length[MR$GaugeGroupList]];
  Plus@@Flatten[tmpp]];


(* ::Text:: *)
(*The beta function itself*)


betaMS2[field1_,field2_,inomasses_]:=Module[{sf,sf1name,sf2name,tmpp,sf1,sf2,sf1ind,sf2ind},
(*We'll only be working with superfields. Scalar components will be restored at the end of the calculations*)
  {sf1,sf2}={field1,field2}/.{a_?(AntiFieldQ[#]===True&)[ind__]:>Scalar2SF[HC[a]][ind], a_?(AntiFieldQ[#]===False&)[ind__]:> Scalar2SF[a][ind]};
  {sf1name,sf2name}={sf1/.a_[__]->a,sf2/.a_[__]->a};
  sf1ind=sf1/.a_?(#=!=Index&)[ii__]->{ii}; If[sf1ind===sf1name,sf1ind={}];
  sf2ind=sf2/.a_?(#=!=Index&)[ii__]->{ii}; If[sf2ind===sf2name,sf2ind={}];
  sf=(Indexify/@M$ChiralSuperfieldNames);
(*If the common part was not initialized, it is done here*)
  If[FR$Common=={}, GetCommonPart[]];
(*The beta function*)
  tmpp=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:> -1/2FR$Soft[sf1,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$SuperW[f2,f6,sf2]Conjugate[FR$SuperW[f4,f5,f6]]FR$SuperW[fields];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:> -1/2 FR$Soft[sf2,f1]FR$SuperW[f1,f2,f3] Conjugate[FR$SuperW[f2,f6,sf1]]FR$SuperW[f4,f5,f6]Conjugate[FR$SuperW[fields]];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf2name&)[ind__],f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:>-Plus@@Table[Conjugate[FR$SuperW[sf1,sf[[ll]],f2]]FR$SuperW[sf2,f2,f3]FR$Soft[sf[[ll]],f6]Conjugate[FR$SuperW[fields]]FR$SuperW[f4,f5,f6],{ll,Length[M$ChiralSuperfieldNames]}];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf1name&)[ind__],f2_,f3_],FR$Temp[f3_,f4_,f5_],FR$Temp[f4_,f5_,f6_]]:> -Plus@@Table[FR$SuperW[sf2,sf[[ll]],f2] FR$Soft[f6,sf[[ll]] ] Conjugate[FR$SuperW[sf1,f2,f3]]FR$SuperW[f3,f4,f5]Conjugate[FR$SuperW[f4,f5,f6]],{ll,Length[M$ChiralSuperfieldNames]}];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf1name&)[ind__],f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:> -Plus@@Table[FR$SuperW[sf2,sf[[ll]],f6] FR$Soft[f2,sf[[ll]]]Conjugate[FR$SuperW[sf1,f2,f3]]Conjugate[FR$SuperW[f4,f5,f6]]FR$SuperW[fields],{ll,Length[M$ChiralSuperfieldNames]}];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf2name&)[ind__],f2_,f3_],FR$Temp[f3_,f4_,f5_],FR$Temp[f4_,f5_,f6_]]:>-2*Plus@@Table[Conjugate[FR$SuperW[sf1,f2,sf[[ll]]]]FR$SuperW[sf2,f2,f3]Conjugate[FR$SuperW[f3,f4,f5]] FR$SuperW[f4,sf[[ll]],f6]FR$Soft[f5,f6],{ll,Length[M$ChiralSuperfieldNames]}];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf2name&)[ind__],f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:> -Conjugate[FR$SuperW[sf1,f2,f6]]FR$SuperW[sf2,f2,f3]Conjugate[FR$Soft[fields]]FR$Soft[f6,f4,f5];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf2name&)[ind__],f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:> -Conjugate[FR$Soft[sf1,f2,f6]]FR$Soft[sf2,f2,f3]Conjugate[FR$SuperW[fields]]FR$SuperW[f6,f4,f5];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf2name&)[ind__],f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:> -Conjugate[FR$Soft[sf1,f2,f6]]FR$SuperW[sf2,f2,f3]Conjugate[FR$SuperW[fields]]FR$Soft[f6,f4,f5];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf2name&)[ind__],f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:> -Conjugate[FR$SuperW[sf1,f2,f6]]FR$Soft[sf2,f2,f3]Conjugate[FR$Soft[fields]]FR$SuperW[f6,f4,f5];
  tmpp+=Plus@@Flatten[FR$Common/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_?(#===sf2name&)[ind__]]]:> (FR$Soft[sf1,f3]Conjugate[FR$SuperW[fields]]FR$SuperW[sf2,f4,f5] + 2*Conjugate[FR$Soft[sf1,f4,f5]]FR$Soft[sf2,f4,f5] - 2*Conjugate[FR$Soft[sf1,f4,f5]]FR$SuperW[sf2,f4,f5]inomasses[[#]] - 2*Conjugate[FR$SuperW[sf1,f4,f5]]FR$Soft[sf2,f4,f5]Conjugate[inomasses[[#]]] + 4*Conjugate[FR$SuperW[sf1,f4,f5]]FR$SuperW[sf2,f4,f5]inomasses[[#]]Conjugate[inomasses[[#]]])GroupToCoup[MR$GaugeGroupList[[#]]]^2 (FCasimir[f4,MR$GaugeGroupList[[#]]] + FCasimir[f5,MR$GaugeGroupList[[#]]] -FCasimir[sf1,MR$GaugeGroupList[[#]]] )&/@Range[Length[MR$GaugeGroupList]]];
  tmpp+=Plus@@Flatten[FR$Common/.KeepOrdered[FR$Temp[fields__],FR$Temp[f3_?(#===sf1name&)[ind__],f4_,f5_],FR$Temp[f4_,f5_,f6_]]:> Conjugate[FR$SuperW[sf1,f4,f5]]FR$SuperW[f4,f5,f6]FR$Soft[sf2,f6]GroupToCoup[MR$GaugeGroupList[[#]]]^2 (FCasimir[f4,MR$GaugeGroupList[[#]]] + FCasimir[f5,MR$GaugeGroupList[[#]]] -FCasimir[sf1,MR$GaugeGroupList[[#]]] )&/@Range[Length[MR$GaugeGroupList]]];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_?(#===sf1name&)[ind__],f2_,f3_],FR$Temp[f3_,f4_?(#===sf2name&)[ind__],f5_],FR$Temp[fields__]]:>4Conjugate[FR$SuperW[sf1,f2,f3]]FR$SuperW[f3,sf2,f5]FR$Soft[f2,f5]GroupToCoup[MR$GaugeGroupList[[#]]]^2 ( FCasimir[f2,MR$GaugeGroupList[[#]]] + FCasimir[f3,MR$GaugeGroupList[[#]]] -FCasimir[sf1,MR$GaugeGroupList[[#]]] )&/@Range[Length[MR$GaugeGroupList]];
  tmpp+=If[sf1name===sf2name,Inner[IndexDelta,sf1ind,sf2ind,Times](24*GaugePiece[sf1name,GauginoMass->inomasses*Conjugate/@inomasses,NoCasimir->True]+Plus@@Flatten[Table[GroupToCoup[MR$GaugeGroupList[[ii]]]^2*FCasimir[sf1,MR$GaugeGroupList[[ii]]] FCasimir[sf1,MR$GaugeGroupList[[jj]]]*GroupToCoup[MR$GaugeGroupList[[jj]]]^2(32 inomasses[[ii]]Conjugate[inomasses[[ii]]] + 8*inomasses[[ii]]Conjugate[inomasses[[jj]]] + inomasses[[jj]]Conjugate[inomasses[[ii]]]),{ii,Length[MR$GaugeGroupList]},{jj,Length[MR$GaugeGroupList]}]]),0];
  tmpp+=If[sf1name===sf2name,(Plus@@FR$Common)/.KeepOrdered[FR$Temp[fields__],FR$Temp[f3_,f4_,f5_],FR$Temp[f4_,f5_,f6_]]:> Inner[IndexDelta,sf1ind,sf2ind,Times]If[AbelianQ[#],-2*GroupToCoup[#]^2 FCasimir[sf1,#,power->1] FR$Soft[f3,f6]Conjugate[FR$SuperW[f3,f4,f5]],0]FR$SuperW[f4,f5,f6]&/@MR$GaugeGroupList,0];
  tmpp+=If[sf1name===sf2name,Inner[IndexDelta,sf1ind,sf2ind,Times]*8*GetDynkinTrace[sf1,inomasses],0];
  tmpp+=8*TraceTerm2Loop[field2,field1];
(*Deleting useless terms*)
  tmpp=tmpp/.KeepOrdered[__]->0;
(*Restoring scalar fields in FR$Soft[]*)
  tmpp=RestoreFields[Plus@@tmpp];
(*FR$Soft works only with the combination (field, antifield). We make sure this combination appears*)
  tmpp=tmpp/.{FR$Soft[a_?(AntiFieldQ[#]===False&)[ind__],b_?(AntiFieldQ[#]===False&)[ind2__]]:>FR$Soft[HC[a][ind],b[ind2]],FR$Soft[a_?(AntiFieldQ[#]===True&)[ind__],b_?(AntiFieldQ[#]===True&)[ind2__]]:>FR$Soft[HC[a][ind],b[ind2]]};
(*Replacing and simplifying*)  
  tmpp=ReplaceSuperAndSoftTerms[tmpp];
  tmpp];


(* ::Subsubsection::Closed:: *)
(*Computation of the beta function assiociated to one single soft interaction: 1loop*)


betaSoftInt11[field1_,sfs1_,sfs2_,sfs3_,sfsbar_]:=Module[{resu},
(* Result *)
  resu=(Plus@@Flatten[Table[
      If[Sort[$IndList[sfs1[[ll]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]],
       1/2 Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[sfs2[[mm]],sfs3[[nn]],field1] FR$Soft[sfs1[[ll]]]+
           Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[sfs1[[ll]]] FR$Soft[sfs2[[mm]],sfs3[[nn]],field1]+
           Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]]  FR$SuperW[field1,sfs1[[ll]]] FR$Soft[sfs2[[mm]],sfs3[[nn]]],0]+
       2 FR$SuperW[field1,sfs2[[mm]],sfs3[[nn]]] FR$Soft[sfsbar[[ll]],sfs3[[nn]]] Conjugate[FR$SuperW[sfs2[[mm]],sfs1[[ll]]]],
    {ll,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]},{nn,1,Length[M$ChiralSuperfieldNames]}]])+
    (Plus@@Flatten[Table[Conjugate[FR$Soft[sfs1[[ll]],sfs2[[mm]]]] FR$Soft[field1,sfs1[[ll]],sfs2[[mm]]],
      {ll,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]}]]);

  (* Restoring fields and superfields in the superpotential and soft terms *)
  resu=RestoreFields[resu];

  (* Simplifications *)
  resu=ReplaceSuperAndSoftTerms[resu];

(*Output *)
resu];


betaSoftInt21[field1_,field2_,inomasses_,sfs1_,sfs2_,sfs3_]:=Module[{resu},
(* Result *)
  resu=(Plus@@Flatten[Table[
      If[Sort[$IndList[sfs1[[ll]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field2/.sff_[inds__]->sff]],
       1/2 Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[sfs2[[mm]],sfs3[[nn]],field2] FR$Soft[field1,sfs1[[ll]]]+
           Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[field1,sfs1[[ll]]] FR$Soft[sfs2[[mm]],sfs3[[nn]],field2],0]+
      If[Sort[$IndList[sfs1[[ll]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]],
       1/2 Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[sfs2[[mm]],sfs3[[nn]],field1] FR$Soft[field2,sfs1[[ll]]]+
           Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[field2,sfs1[[ll]]] FR$Soft[sfs2[[mm]],sfs3[[nn]],field1],0]+
       1/2 Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$Soft[sfs2[[mm]],sfs3[[nn]]] FR$SuperW[field2,field1,sfs1[[ll]]]+
       1/2 Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$Soft[sfs2[[mm]],sfs3[[nn]]] FR$SuperW[field1,field2,sfs1[[ll]]],
    {ll,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]},{nn,1,Length[M$ChiralSuperfieldNames]}]])-
    2 FR$Soft[field1,field2](Plus@@(GFactor[field1]+GFactor[field2]))+
     4 FR$SuperW[field1,field2]Dot[GFactor[field1]+GFactor[field2],inomasses];

  (* Restoring fields and superfields in the superpotential and soft terms *)
  resu=RestoreFields[resu];

  (* Simplifications *)
  resu=ReplaceSuperAndSoftTerms[resu];

(*Output *)
resu];


betaSoftInt31[field1_,field2_,field3_,inomasses_,sfs1_,sfs2_,sfs3_]:=Module[{resu},
(* Result *)
  resu=(Plus@@Flatten[Table[
      If[Sort[$IndList[sfs1[[ll]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field3/.sff_[inds__]->sff]],
        1/2Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[sfs2[[mm]],sfs3[[nn]],field3] FR$Soft[field1,field2,sfs1[[ll]]]+
       Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$Soft[sfs2[[mm]],sfs3[[nn]],field3] FR$SuperW[field1,field2,sfs1[[ll]]],0]+
      If[Sort[$IndList[sfs1[[ll]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field1/.sff_[inds__]->sff]],
       1/2Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[sfs2[[mm]],sfs3[[nn]],field1] FR$Soft[field3,field2,sfs1[[ll]]]+
      Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$Soft[sfs2[[mm]],sfs3[[nn]],field1] FR$SuperW[field3,field2,sfs1[[ll]]],0]+
      If[Sort[$IndList[sfs1[[ll]]/.sfff_[indsp__]->sfff]]===Sort[$IndList[field2/.sff_[inds__]->sff]],
       1/2Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$SuperW[sfs2[[mm]],sfs3[[nn]],field2] FR$Soft[field1,field3,sfs1[[ll]]]+
      Conjugate[FR$SuperW[sfs1[[ll]],sfs2[[mm]],sfs3[[nn]]]] FR$Soft[sfs2[[mm]],sfs3[[nn]],field2] FR$SuperW[field1,field3,sfs1[[ll]]],0],
    {ll,1,Length[M$ChiralSuperfieldNames]},{mm,1,Length[M$ChiralSuperfieldNames]},{nn,1,Length[M$ChiralSuperfieldNames]}]])-
    2 FR$Soft[field1,field2,field3](Plus@@(GFactor[field1]+GFactor[field2]+GFactor[field3]))+
     4 FR$SuperW[field1,field2,field3]Dot[GFactor[field1]+GFactor[field2]+GFactor[field3],inomasses];

  (* Restoring fields and superfields in the superpotential and soft terms *)
  resu=RestoreFields[resu];

  (* Simplifications *)
  resu=ReplaceSuperAndSoftTerms[resu];

(*Output *)
resu];


(* ::Subsubsection:: *)
(*Computation of the beta function assiociated to one single soft interaction: 2loop*)


betaSoftInt12[ ]:=Module[{resu},
0];


betaSoftInt22[sf1_,sf2_,inomasses_]:=Module[{tmpp,sf2name},
  If[FR$Common=={}, GetCommonPart[]];
  sf2name=sf2/.a_[ind__]->a;
(*Calculating the beta function itself*)
  tmpp=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:>-1/2FR$Soft[sf1,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$SuperW[fields]Conjugate[FR$SuperW[f4,f5,f6]]FR$SuperW[f2,f6,sf2];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:>-1/2FR$SuperW[sf1,sf2,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$Soft[f2,f6]FR$SuperW[fields]Conjugate[FR$SuperW[f4,f5,f6]];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:>-1/2FR$SuperW[sf1,sf2,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$SuperW[f2,f6]FR$Soft[fields]Conjugate[FR$SuperW[f4,f5,f6]];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:> - FR$SuperW[sf1,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$Soft[fields]Conjugate[FR$SuperW[f4,f5,f6]]FR$SuperW[f2,f6,sf2];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[fields__],FR$Temp[f4_,f5_,f6_]]:>-FR$SuperW[sf1,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$SuperW[fields]Conjugate[FR$SuperW[f4,f5,f6]]FR$Soft[f2,f6,sf2];
  tmpp+=(Plus@@FR$Common)/.KeepOrdered[FR$Temp[f1_,f2_,f3_],argx__]:>2*FR$SuperW[sf1,sf2,f1]Conjugate[FR$SuperW[f1,f2,f3]]*(FR$Soft[f2,f3] - FR$SuperW[f2,f3]inomasses[[#]])GroupToCoup[MR$GaugeGroupList[[#]]]^2 FCasimir[f2,MR$GaugeGroupList[[#]]] &/@Range[Length[MR$GaugeGroupList]];
  tmpp+=Plus@@Flatten[(FR$Common/.KeepOrdered[arg__,FR$Temp[f1_,f2_,f3_],FR$Temp[f2_,f3_,f4_?(#===sf2name&)[ind__]]]:>(FR$Soft[sf1,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$SuperW[f2,f3,sf2] + 2*FR$SuperW[sf1,f1] Conjugate[FR$SuperW[f1,f2,f3]]FR$Soft[f2,f3,sf2] - 2*FR$SuperW[sf1,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$SuperW[f2,f3,sf2]inomasses[[#]])GroupToCoup[MR$GaugeGroupList[[#]]]^2*(2FCasimir[f1,MR$GaugeGroupList[[#]]] - FCasimir[sf1,MR$GaugeGroupList[[#]]])&/@Range[Length[MR$GaugeGroupList]])/.KeepOrdered[__]->{}];
  tmpp+=2*FR$Soft[sf1,sf2]*GaugePiece[sf1/.a_[__]->a] - 8*FR$SuperW[sf1,sf2]GaugePiece[sf1/.a_[__]->a,GauginoMass->inomasses];
(*Restoring proper fields in FR$SuperW and FR$Soft and applying different rules*)  
  tmpp=RestoreFields[Plus@@tmpp];
  tmpp=ReplaceSuperAndSoftTerms[tmpp];
  tmpp];


betaSoftInt32[sf1_,sf2_,sf3_,inomasses_]:=Module[{tmpp},
(*If the common part was not calculated.*)
  If[ FR$Common=={}, GetCommonPart[]];
(*The beta functin itself*)
    tmpp= Plus@@FR$Common/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[field__],FR$Temp[f4_,f5_,f6_]]:> -1/2FR$Soft[sf1,sf2,f1]Conjugate[ FR$SuperW[f1,f2,f3]] FR$SuperW[field] Conjugate[FR$SuperW[f4,f5,f6]] FR$SuperW[f2,f6,sf3]/.FR$SuperWRules;
    tmpp-=Plus@@FR$Common/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[field__],FR$Temp[f4_,f5_,f6_]]:>FR$SuperW[sf1,sf2,f1] Conjugate[ FR$SuperW[f1,f2,f3]] FR$SuperW[field] Conjugate[FR$SuperW[f4,f5,f6]] FR$Soft[f2,f6,sf3]/.FR$SuperWRules;
    tmpp-=Plus@@FR$Common/.KeepOrdered[FR$Temp[f1_,f2_,f3_],FR$Temp[field__],FR$Temp[f4_,f5_,f6_]]:>FR$SuperW[sf1,sf2,f1] Conjugate[ FR$SuperW[f1,f2,f3]] FR$Soft[field] Conjugate[FR$SuperW[f4,f5,f6]] FR$SuperW[f2,f6,sf3]/.FR$SuperWRules;
    tmpp+=Plus@@Flatten[(FR$Common/.KeepOrdered[arg__,FR$Temp[f1_,f2_,f3_],FR$Temp[f2_,f3_,f4_?(#===sf3name&)[ind__]]]:>(FR$Soft[sf1,sf2,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$SuperW[f2,f3,sf3] + 2* FR$SuperW[sf1,sf2,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$Soft[f2,f3,sf3]- 2*inomasses[[#]]FR$SuperW[sf1,sf2,f1]Conjugate[FR$SuperW[f1,f2,f3]]FR$SuperW[f2,f3,sf3])*GroupToCoup[MR$GaugeGroupList[[#]]]^2*(2FCasimir[f2,MR$GaugeGroupList[[#]]] - FCasimir[sf3,MR$GaugeGroupList[[#]]])&/@Range[Length[MR$GaugeGroupList]])/.KeepOrdered[__]->{}];
    tmpp+=2*FR$Soft[sf1,sf2,sf3]GaugePiece[sf3/.a_[__]->a] - 8FR$SuperW[sf1,sf2,sf3]GaugePiece[sf3/.a_[__]->a,GauginoMass->inomasses];
(*Putting scalar fields in FR$Soft*)
    tmpp=RestoreFields[Plus@@tmpp];
(*Replacing FR$Soft and FR$SuperW AND contracting SUEps and IndexDelta*)
    tmpp=ReplaceSuperAndSoftTerms[tmpp];
  tmpp];


(* ::Subsubsection::Closed:: *)
(*Restoring proper objects in the soft and superpotential functions in order to have the function FR$SuperW and FR$Soft working*)


RestoreFields[expr_]:=Module[{resu},
  resu = expr//.FR$Soft[aaa___,sf_?(SuperfieldQ[#]===True&)[inds__],bbb___]:>FR$Soft[aaa,SF2Scalar[sf][inds],bbb];
  resu = resu//.FR$Soft[aaa___,sf_?(SuperfieldQ[#]===True&),bbb___]:>FR$Soft[aaa,SF2Scalar[sf],bbb];
  resu = resu//.FR$SuperW[aaa___,sf_?(ScalarFieldQ[#]===True&)[inds__],bbb___]:>FR$SuperW[aaa,Scalar2SF[sf][inds],bbb];
  resu = resu//.FR$SuperW[aaa___,sf_?(ScalarFieldQ[#]===True&),bbb___]:>FR$SuperW[aaa,Scalar2SF[sf],bbb];
  resu = resu//.SF2Scalar[sf_]->anti[SF2Scalar[anti[sf]]];
resu];


(* ::Subsubsection:: *)
(*Replacing FR$SuperW and FR$Soft functions by their values and contracting deltas and epsilons*)


ReplaceSuperAndSoftTerms[expr_]:=Module[{resu},
  resu=Expand[expr]//.FR$ScalarMassRules//.FR$SuperWRules//.FR$SoftRules//.{
      SUEps[argx__]^2:>Factorial[Length[{argx}]],
      SUEps[arg0___,arg1_,arg2___]SUEps[arg0p___,arg1_,arg2p___]:>If[Length[{arg0p}]===Length[{arg0}],
        Inner[IndexDelta,{arg0},{arg0p},Times]*Inner[IndexDelta,{arg2},{arg2p},Times]Factorial[Length[{arg0,arg2}]], 
       (-1)^(Length[{arg2p}]+(Length[{arg2p}]+1)*Length[{arg0p}])*
        Inner[IndexDelta,{arg0},{arg2p},Times]*Inner[IndexDelta,{arg2},{arg0p},Times]Factorial[Length[{arg0,arg2}]]]};
  resu=resu/.IndexDelta[Index[type_,ind_],Index[type_,ind_]]:>IndexDim[type];
resu];


(* ::Subsubsection::Closed:: *)
(*Gauge group factors appearing in the 1 loop soft rges*)


GFactor[field_]:=Module[{ffii=field/.a_[inds__]->a},
  If[AbelianQ[#],
    GroupToNorm[#] GroupToCoup[#]^2 GroupToCharge[#]^2/.(SF2QNumbers[Scalar2SF[ffii]])/.GroupToCharge[#]->0,
    GroupToCoup[#]^2 Plus@@(Intersection[$IndList[ffii]/.Index[a_]->a,GroupToReps[#][[All,2]]]/.GroupToCasimirs[#])
]]&/@MR$GaugeGroupList;


(* ::Subsubsection::Closed:: *)
(*Calculation of the 'Trace terms' appearing in the soft mass rge*)


TraceTerms[antifield_,field_]:=Plus@@(Module[{sfs,antisfs,indfl,indafl},
(* Associated superfields and carried indices *)
  sfs=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  antisfs=HC[#]&/@sfs;
  indfl=field/.a_?(#=!=Index&)[ii__]->{ii}; If[indfl===field,indfl={}];
  indafl=antifield/.a_?(#=!=Index&)[ii__]->{ii}; If[indafl===antifield,indafl={}];

(* Computing the trace terms *)
  If[AbelianQ[#],
    If[(field/.a_[inds__]->a)===anti[(antifield/.a_[inds__]->a)], 
      GroupToNorm[#] GroupToCoup[#]^2 (GroupToCharge[#]/.(SF2QNumbers[Scalar2SF[field/.a_[inds__]->a]])/.GroupToCharge[#]->0)*
       Inner[IndexDelta,indfl,indafl,Times]*(Plus@@Flatten[Table[
       (GroupToCharge[#]/.(SF2QNumbers[sfs[[ll]]/.a_[inds__]->a])/.GroupToCharge[#]->0) FR$Soft[sfs[[ll]],antisfs[[ll]]],
       {ll,1,Length[M$ChiralSuperfieldNames]}]]),
      0],
    0]]&/@MR$GaugeGroupList);


(*This is a trace over the generators of the different representations weighted by quadratic casimir invariants for the irreducible representations of the chiral superfields.*)
TraceTerm2Loop[antifield_,field_]:=Module[{sfs,antisfs,indfl,indafl,tmpp},
(* Associated superfields and carried indices *)
  sfs=Table[Indexify[M$ChiralSuperfieldNames[[nn]]],{nn,1,Length[M$ChiralSuperfieldNames]}];
  antisfs=HC[#]&/@sfs;
  indfl=field/.a_?(#=!=Index&)[ii__]->{ii}; If[indfl===field,indfl={}];
  indafl=antifield/.a_?(#=!=Index&)[ii__]->{ii}; If[indafl===antifield,indafl={}];
  tmpp=Plus@@Flatten[Table[If[(field/.a_[inds__]->a)===anti[(antifield/.a_[inds__]->a)]&&AbelianQ[MR$GaugeGroupList[[ii]]],
    GroupToCoup[MR$GaugeGroupList[[ii]]]^2*GroupToCoup[MR$GaugeGroupList[[jj]]]^2*FCasimir[field,MR$GaugeGroupList[[ii]],power->1]*Inner[IndexDelta,indfl,indafl,Times]*(Plus@@Table[FCasimir[sfs[[ll]],MR$GaugeGroupList[[ii]],power->1]*FCasimir[sfs[[ll]],MR$GaugeGroupList[[jj]]]FR$Soft[sfs[[ll]],antisfs[[ll]]],{ll,Length[M$ChiralSuperfieldNames]}]),
    0],{ii,Length[MR$GaugeGroupList]},{jj,Length[MR$GaugeGroupList]}]];
  tmpp];


(* ::Subsection::Closed:: *)
(*Computing the RGEs*)


Options[ScaSoftRGE]={NLoop->2};


ScaSoftRGE[lsoft_,superpot_,OptionsPattern[]]:=Module[{tmp, rge,rge1,rge2, InoMasses, Lino, LScaMass},
    (* Get the Yukawa couplings *)  
  tmp=ExtractSuperWTerms[superpot,SuperfieldQ]; 
  TestSuperW[tmp];

  (* Get the soft terms as three independent lists *)
  {LScaMass,InoMasses,tmp}=SoftParameters[lsoft];

  (* 1 Loop beta functions and rges *)
  ComputeBetaSoftInt31[Cases[tmp,{_,a_?(Length[#]===3&)}],InoMasses];
    rge1=Inner[Equal,FR$D/@Cases[tmp,{_,a_?(Length[#]===3&)}][[All,1]],FR$betaSI31/(16 Pi^2),List]; 
  ComputeBetaSoftInt21[Cases[tmp,{_,a_?(Length[#]===2&)}],InoMasses];
    rge1=Join[rge1,Inner[Equal,FR$D/@Cases[tmp,{_,a_?(Length[#]===2&)}][[All,1]],FR$betaSI21/(16 Pi^2),List]];
  ComputeBetaSoftInt11[Cases[tmp,{_,a_?(Length[#]===1&)}]];
    rge1=Join[rge1,Inner[Equal,FR$D/@Cases[tmp,{_,a_?(Length[#]===1&)}][[All,1]],FR$betaSI11/(16 Pi^2),List]];
  ComputeBetaMs1[LScaMass,InoMasses];
    rge1=Join[rge1,Inner[Equal,FR$D/@LScaMass[[All,1]],FR$betaMs1/(16 Pi^2),List]]; 
(* 2 Loop beta functions and rges *)
  If[OptionValue[NLoop]==2,
  ComputeBetaSoftInt32[Cases[tmp,{_,a_?(Length[#]===3&)}],InoMasses];
    rge2=Inner[Equal,FR$D/@Cases[tmp,{_,a_?(Length[#]===3&)}][[All,1]],FR$betaSI32/(16 Pi^2)^2,List]; 
  ComputeBetaSoftInt22[Cases[tmp,{_,a_?(Length[#]===2&)}],InoMasses];
    rge2=Join[rge2,Inner[Equal,FR$D/@Cases[tmp,{_,a_?(Length[#]===2&)}][[All,1]],FR$betaSI22/(16 Pi^2)^2,List]];
  ComputeBetaSoftInt12[Cases[tmp,{_,a_?(Length[#]===1&)}]];
  FR$betaSI12=If[Head[FR$betaSI12]=!=List&&Length[FR$betaSI12]===0,{FR$betaSI12},FR$betaSI12];
    rge2=Join[rge2,Inner[Equal,FR$D/@Cases[tmp,{_,a_?(Length[#]===1&)}][[All,1]],FR$betaSI12/(16 Pi^2)^2,List]];
  ComputeBetaMs2[LScaMass,InoMasses];
    rge2=Join[rge2,Inner[Equal,FR$D/@LScaMass[[All,1]],FR$betaMs2/(16 Pi^2)^2,List]],
    rge2={0,0}&/@Range[Length[LScaMass]+Length[tmp]]];
  (* Formatting *)
  rge1=FormattingRGE[rge1];rge2=FormattingRGE[rge2];
  rge=(rge1[[#,1]]==rge1[[#,2]]+rge2[[#,2]])&/@Range[Length[rge2]];

rge];


(* ::Section::Closed:: *)
(*Get Renormalization Group Equations*)


Options[RGE] = {NLoop->2};


RGE[lsoft_,superpot_,OptionsPattern[]]:=Module[{nloop,rge1,rge2,rge3,rge4},
  nloop=OptionValue[NLoop]; 
  rge1=GaugeCouplingsRGE[superpot,NLoop->nloop];
  rge2=GauginoMassesRGE[superpot,lsoft,NLoop->nloop];
  rge3=SuperpotentialRGE[superpot,NLoop->nloop];
  rge4=ScaSoftRGE[lsoft,superpot,NLoop->nloop];
  Join[rge1,rge2,rge3,rge4]
];
