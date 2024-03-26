(* ::Package:: *)

(* ::Title:: *)
(*Automated derivation of the Goldstino/Gravitino interactions*)


(* ::Section::Closed:: *)
(*Useful functions*)


(* ::Subsection:: *)
(*Converting derivatives to indices*)


ConvDer[exp_]:=exp//.{del[Except[del,x_][aa__],mu_]:>x[aa,mu,FR$del]}//.{del[Except[del,x_],mu_]:>x[mu,FR$del]}/.a_[aa__,FR$del][bb_,FR$del]->a[aa,FR$del,bb,FR$del];


(* ::Section:: *)
(*Supersymmetric transformation of the Lagrangian*)


(* ::Subsection:: *)
(*SUSY transformation of the chiral Lagrangian*)


DeltaSUSYChiral[lc_,spin_, lor_] :=Module[{start,resu,resudbl,resusgl,spt},
  (* We first compute del_mu J^mu *)
  resu=If[Head[lc]===Plus, List@@lc, {lc}];
  Print[Style["Supersymmetric transformation of the chiral Lagrangian...",Orange,Bold]];
  resu=Plus@@(Module[{lagg}, lagg=Theta2Thetabar2Component[Tonc[DeltaSUSY[#,eps1]]]; lagg]&/@resu);

  (* We then derive the supercurrent from the previous result \[Rule] we use the fermions to extract the total derivative *)
  resu=Expand[resu/.eps1bar[_]->0];
  start=resu;
  (* Terms with a double femrionic derivatives *)
  resu=If[Head[resu]===Plus, List@@resu,{resu}];
  resudbl = Tonc/@ Cases[resu,_?(Cases[#,Dot[___,del[del[fer_?(FermionQ[#]===True&)[__],_],_],___]]=!={}&)];
  resudbl=resudbl//.nc[aaa___,bbb_,eps1[sp_],ccc___]->-nc[aaa,eps1[sp],bbb,ccc];
  resudbl= resudbl/.{del[del[fil_[inds__],mu_],nu_]:>del[fil[inds],mu] IndexDelta[lor,nu],eps1[sp_]:>Deps[sp,spin]};
  resu=OptimizeIndex[start-Expand[del[GrassmannExpand[Expand[Ueps[spin,spt]nc[eps1[spt],(Plus@@resudbl)]]],lor]]]/.del[eps1[_],_]->0;

  (* Terms with a simple fermionic derivative *)
  resu=If[Head[resu]===Plus, List@@resu,{resu}];
  resusgl = Tonc/@ Cases[resu,_?(Cases[#,Dot[___,del[fer_?(FermionQ[#]===True&)[__],_],___]]=!={}&)];
  resusgl=resusgl//.nc[aaa___,bbb_,eps1[sp_],ccc___]->-nc[aaa,eps1[sp],bbb,ccc];
  resusgl=resusgl/.{del[fil_?(FermionQ[#]===True&)[inds__],mu_]:>fil[inds] IndexDelta[lor,mu],eps1[sp_]:>Deps[sp,spin]};

  (* Check *)
  resu=OptimizeIndex[(Plus@@resu)-Expand[del[GrassmannExpand[Expand[Ueps[spin,spt]nc[eps1[spt],(Plus@@resusgl)]]],lor]]]/.del[eps1[_],_]->0;
  If[resu=!=0, Print[Style["Problen with the extraction of the supercurrent in LChiral",Bold,Red]];Abort[]];

  (* Output *)
  Return[OptimizeIndex[Plus@@resudbl+Plus@@resusgl]];
];


(* ::Subsection:: *)
(*SUSY transformation of the vector Lagrangian*)


DeltaSUSYVector[lv_,spin_,lorentz_] :=Module[{start,check,resu,spt,inoList=SF2Ino/@M$VectorSuperfieldNames,
   cstlist=DeleteCases[StructureConstant/.MR$GaugeGroups[[All,2]],StructureConstant]},

  (* We first compute del_mu J^mu *)
  resu=If[Head[lv]===Plus, List@@lv, {lv}];
  Print[Style["Supersymmetric transformation of the vector Lagrangian... ",Orange,Bold]];
  resu=Plus@@(Module[{lagg},lagg=SF2Components[Tonc[DeltaSUSY[#,eps1]]]; lagg[[2,5]]+lagg[[2,6]]]&/@resu);

  (* We then derive the supercurrent from the previous result *)
  resu=Expand[resu/.eps1bar[_]->0/. del[del[ff_,Global`mu$1],Global`mu$2]->del[del[ff,Global`mu$2],Global`mu$1]];
  resu=resu/.Eps[argx__]:>Signature[{argx}] Eps[Sequence@@Sort[{argx}]]/.Eps[___,a_,___,b_,___]del[del[_,a_],b_]->0;
  start=resu; 
  resu=If[Head[resu]===Plus, List@@resu, {resu}];

  (* we now that at this stage, we can construct the supercurrent on the gaugino bar and remove the total derivative (we also remove eps1 with an upper index*)
  resu = Tonc/@ Cases[resu,_?(Cases[#,Dot[_,del[ino_?(MemberQ[anti/@inoList,#]&)[__],_]] ]=!={}&)];
  resu=resu//.nc[aaa___,bbb_,eps1[sp_],ccc___]->-nc[aaa,eps1[sp],bbb,ccc];
  resu= resu/.{del[ino_?(MemberQ[HC/@inoList,#]&)[inds__],mu_]:>ino[inds]IndexDelta[lorentz,mu],eps1[sp_]:>Deps[sp,spin]};

  (* Check that everything worked fine *)
  check = start-Expand[del[GrassmannExpand[Expand[Ueps[spin,spt]nc[eps1[spt],(Plus@@resu)]]],lorentz]];
  check = OptimizeIndex[check/.del[eps1[_],_]->0]/.{ff_?(MemberQ[cstlist,#]&)[inds__]:>ff[Sequence@@Sort[{inds}]] Signature[{inds}],
   Eps[argx__]:>Signature[{argx}] Eps[Sequence@@Sort[{argx}]], Eps[___,a_,___,b_,___]del[del[_,a_],b_]->0};
  If[check=!=0, Print[Style["Problen with the extraction of the supercurrent in LVector",Bold,Red]];Abort[]];

  (* output *)
  Return[Plus@@resu];
];


(* ::Subsection::Closed:: *)
(*SUSY transformation of the superpotential Lagrangian*)


DeltaSUSYSuperW[lw_,spin_,lorentz_]:=Module[{resu,start,check,spt},
  (* We first compute del_mu J^mu *)
  resu=If[Head[lw]===Plus,List@@lw,{lw}];
  Print[Style["Supersymmetric transformation of the superpotential Lagrangian...",Orange,Bold]];
  resu=Plus@@(Module[{lagg},lagg=SF2Components[Tonc[DeltaSUSY[#,eps1]]]; lagg[[2,5]]+lagg[[2,6]]]&/@resu);

  (* We then derive the supercurrent from the previous result (we use the fermionic derivative *)
  resu=Expand[resu/.eps1bar[_]->0];
  start =resu;
  resu=If[Head[resu]===Plus, List@@resu,{resu}];
  resu = Tonc/@ Cases[resu,_?(Cases[#,Dot[___,del[fer_?(FermionQ[#]===True&)[__],_],___]]=!={}&)];
  resu=resu//.nc[aaa___,bbb_,eps1[sp_],ccc___]->-nc[aaa,eps1[sp],bbb,ccc];
  resu=resu/.{del[fil_?(FermionQ[#]===True&)[inds__],mu_]:>fil[inds] IndexDelta[lorentz,mu],eps1[sp_]:>Deps[sp,spin]};

  (* Check that everything worked fine *)
  check = start-Expand[del[GrassmannExpand[Expand[Ueps[spin,spt]nc[eps1[spt],(Plus@@resu)]]],lorentz]];
  check = OptimizeIndex[check/.del[eps1[_],_]->0];
  If[check=!=0, Print[Style["Problen with the extraction of the supercurrent in LVector",Bold,Red]];Abort[]];

  (* output *)
  Return[Plus@@resu];
];


(* ::Section:: *)
(*Variation laws deduced from Euler-Lagrange equations*)


(* ::Subsection:: *)
(*Creation of the replacement rules with the transformation laws for the component fields*)


(* ::Subsubsection:: *)
(*CSF: lower indices for all Weyls*)


CreateChiralDeltaSUSY[] := Module[{MyPat},
  FR$DPhiRules= Module[{sca=SF2Scalar[#],sfidx,inds,out},
    inds=If[$IndList[#]=!={},Indexify[sca]/.Index[_,a_]->a,{}];
    sfidx=#[Sequence@@inds]/.fld_[]->fld;
    out=MyRule[FR$DPhi[sca[Sequence@@(MyPat[#,Blank[]]&/@inds)]/.fld_?(FieldQ[#]===True&)[]->fld],Tonc[ScalarComponent[Tonc[DeltaSUSY[sfidx,eps1]]]]];
    $OptIndex=999;out=out/.MyRule[a_,b_]:>MyRule[a,OptimizeIndex[Expand[b]]];
    out/.MyPat->Pattern/.MyRule->Rule] &/@M$ChiralSuperfieldNames;
  
   FR$DPsiRules = Module[{sfidx,wey=SF2Weyl[#],inds,out,spi},
    inds=If[$IndList[#]=!={},Indexify[#]/.Index[_,a_]->a,{}];
    sfidx=#[Sequence@@inds]/.fld_[]->fld;
    out=MyRule[FR$DPsi[wey[MyPat[spi,Blank[]],Sequence@@(MyPat[#,Blank[]]&/@inds)]],Tonc[ThetaComponent[Tonc[DeltaSUSY[sfidx,eps1]/.eps1bar[_]->0],spi]]/Sqrt[2]];
    $OptIndex=999;out=out/.MyRule[a_,b_]:>MyRule[a,OptimizeIndex[Expand[b]]];
    out/.MyPat->Pattern/.MyRule->Rule]&/@M$ChiralSuperfieldNames;

  FR$DPsibarRules = Module[{wey=SF2Weyl[#],sfidx,inds,spi,spi2,out},
   inds=If[$IndList[#]=!={},Indexify[#]/.Index[_,a_]->a,{}];
   sfidx=anti[#][Sequence@@inds]/.fld_[]->fld;
   out=MyRule[FR$DPsi[anti[wey][MyPat[spi,Blank[]],Sequence@@(MyPat[#,Blank[]]&/@inds)]],
      Tonc[ThetabarComponent[Deps[spi,spi2]Tonc[DeltaSUSY[sfidx,eps1]/.eps1bar[_]->0],spi2]/Sqrt[2]]];
    out=out/.del[argx__]->DC[argx];
    $OptIndex=999;out=out/.MyRule[a_,b_]:>MyRule[a,OptimizeIndex[Expand[b]]];
    out/.MyPat->Pattern/.MyRule->Rule]&/@M$ChiralSuperfieldNames;

  $OptIndex=1;];


(* ::Subsubsection::Closed:: *)
(*VSF: with lower spin indices for the ino*)


CreateVectorDeltaSUSY[]:=Module[{tmpidx},
  FR$DInoRules=Module[{name=Indexify[#],newinds,myrule,mypattern},
    newinds=name/.#[muf__]->{muf}/.#->{};
    myrule[
      FR$DPsi[SF2Ino[#][Pattern[tmpidx,Blank[]],Sequence@@(newinds/.Index[type_,muff_]:>mypattern[muff,Blank[]])]], 
      Thetabar2ThetaComponent[Tonc[I DeltaSUSY[name,eps1]],tmpidx]/.{del[SF2Boson[#][mu_,a___],nu_]->FS[SF2Boson[#],nu,mu,a]}
    ]//.{mypattern->Pattern,myrule->Rule}]&/@M$VectorSuperfieldNames;

  FR$DVRules=Module[{name=Indexify[#],newinds,myrule,mypattern},
    newinds=name/.#[muf__]->{muf}/.#->{};
    myrule[
      FR$DV[SF2Boson[#][Pattern[tmpidx,Blank[]],Sequence@@(newinds/.Index[type_,muff_]:>mypattern[muff,Blank[]])]], 
      Tonc[ThetaThetabarComponent[Tonc[DeltaSUSY[name,eps1]],tmpidx]]
    ]/.{eps1bar[_]->0}//.{mypattern->Pattern,myrule->Rule}
  ]&/@M$VectorSuperfieldNames;

  $OptIndex=666;
  FR$DInoRules=Rule[#[[1]],Tonc[OptimizeIndex[#[[2]]]]]&/@FR$DInoRules;
  FR$DVRules=Rule[#[[1]],OptimizeIndex[#[[2]]]]&/@FR$DVRules;
  $OptIndex=1;];


(* ::Subsection:: *)
(*Derivatives with respect to fields and adding the variations*)


(* ::Subsubsection::Closed:: *)
(*Scalar field*)


DerS[L_?(Head[#]===Plus&),fi_,spin_]:=Plus@@ (DerS[#,fi,spin]&/@L);


DerS[L_ ,fi_,spin_]:=Module[{tp,DER},
  tp=DER[#,fi] L/# &/@(List@@L);
  tp=tp/.DER[Power[a_,2],b_]->2 a DER[a, b];
  tp=tp/.sub_ DER[a_[inds1__,mu2_,FR$del,mu2_,FR$del],fi]:> del[DerS[sub a[inds1,mu2,FR$del],fi,spin],mu2]-DerS[ConvDer[del[sub,mu2]] a[inds1,mu2,FR$del],fi,spin];
  tp=tp/.sub_ DER[a_[mu2_,FR$del,mu2_,FR$del],fi]:> del[DerS[sub a[mu2,FR$del],fi,spin],mu2]-DerS[ConvDer[del[sub,mu2]] a[mu2,FR$del],fi,spin];  
  tp=tp/.DER[a_,b_]:>0/; Head[a]=!=Head[b];
  tp=tp/.DER[a_[indices1__],b_[indices2__]]:>0/;Length[{indices1}]=!=Length[{indices2}] && Length[Cases[{indices1},FR$del]]=!=2;
  tp=tp/.DER[a_[inds1__,mu2_,FR$del],a_[inds2__,mu_,FR$del]]:>FR$DPhi[a[inds1]] IndexDelta[mu2,mu];
  tp=tp/.DER[a_[mu2_,FR$del],a_[mu_,FR$del]]:>FR$DPhi[a] IndexDelta[mu2,mu];
  tp=Expand[tp/.FR$DPhiRules];
  tp=tp//.nc[aaa___,bbb_,eps1[sp_],ccc___]->-nc[aaa,eps1[sp],bbb,ccc]/.eps1[sp_]:>Deps[sp,spin];
  tp = ConvDer[tp];
  tp=tp/.EDelta->IndexDelta;
  Return[Plus@@tp];
];


(* ::Subsubsection::Closed:: *)
(*Fermionic Weyl field*)


DerF[L_,fi_,spin_]:=Module[{tp,DER},
  tp = DER[#,fi] L/# &/@(List@@L);
  tp = Expand[tp//.DER[nc[a1_,a2___],fi]:>nc[DER[a1,fi],a2]+nc[a1,DER[nc[a2],fi]]];
  tp = tp/.DER[a_,b_]:>0/;Head[a]=!=Head[b];
  tp = tp/.DER[a_[indices1__],b_[indices2__]]:>0/;Length[{indices1}]=!=Length[{indices2}];
  tp = tp/.DER[a_[inds1__,mu2_,FR$del],a_[inds2__,mu_,FR$del]]:>FR$DPsi[a[inds1]] IndexDelta[mu2,mu];
  tp = Expand[tp/.FR$DPsiRules/.FR$DPsibarRules/.FR$DInoRules];
  tp=tp//.nc[aaa___,bbb_,eps1[sp_],ccc___]->-nc[aaa,eps1[sp],bbb,ccc]/.eps1[sp_]:>Deps[sp,spin];
  Return[Plus@@tp];
];


(* ::Subsubsection::Closed:: *)
(*Vector field*)


DerV[L_ ,fi_,spin_]:=Module[{tp,DER},
  tp=DER[#,fi] L/# &/@(List@@L);
  tp=tp/.DER[Power[a_,2],b_]->2 a DER[a, b];
  tp=tp/.DER[a_,b_]:>0/;Head[a]=!=Head[b];
  tp=tp/.DER[a_[indices1__],b_[indices2__]]:>0/;Length[{indices1}]=!=Length[{indices2}];
  tp=tp/.DER[a_[inds1__,mu2_,FR$del],a_[inds2__,mu_,FR$del]]:>FR$DV[a[inds1]] IndexDelta[mu2,mu];
  tp=Expand[tp/.FR$DVRules];
  tp=tp//.nc[aaa___,bbb_,eps1[sp_],ccc___]->-nc[aaa,eps1[sp],bbb,ccc]/.eps1[sp_]:>Deps[sp,spin];
  Return[Plus@@tp];
];


(* ::Subsection::Closed:: *)
(*Main routine for the chiral Lagrangian*)


DLC[lag_,spin_,lorentz_]:=Module[{tmplag=Expand[Theta2Thetabar2Component[Tonc[lag]]],scalist,ferlist,antiferlist,result},
(* Replacement rules for the SUSY transformation of the component fields *)
  CreateChiralDeltaSUSY[];FR$DInoRules={};

(* Preparing the Lagrangian *)
  tmplag=ConvDer[tmplag];
  tmplag=If[Head[tmplag]===Plus, List@@tmplag, {tmplag}];
  tmplag=Expand[Tonc[#]&/@tmplag];

(* Lists of field *)
  scalist=ConvDer[del[Indexify[SF2Scalar[#]],Index[Lorentz,lorentz]]&/@M$ChiralSuperfieldNames];
  ferlist=ConvDer[del[Indexify[SF2Weyl[#]],Index[Lorentz,lorentz]]&/@M$ChiralSuperfieldNames];
  antiferlist=ConvDer[del[anti[Indexify[SF2Weyl[#]]],Index[Lorentz,lorentz]]&/@M$ChiralSuperfieldNames];

(* Computation of the derivatives with respect to the fields *)
  Print[Style["Variations of the chiral Lagrangian from the derivation of Euler-Lagrange equations...",Orange,Bold]];
  result=Join[
    Table[Plus@@(DerS[#,scalist[[iii]],spin]&/@tmplag),{iii,1,Length[scalist]}],
    Table[Plus@@(DerF[#,ferlist[[iii]],spin] &/@tmplag),{iii,1,Length[ferlist]}],
    Table[Plus@@(DerF[#,antiferlist[[iii]],spin] &/@tmplag),{iii,1,Length[antiferlist]}]];

(* Restoring spacetime derivatives *)
  result=result//.{x_[aa__,mu_,FR$del]:>del[x[aa],mu],x_[mu_,FR$del]:>del[x,mu]};

OptimizeIndex[Plus@@result]];


(* ::Subsection::Closed:: *)
(*Main routine for the vector Lagrangian*)


DLV[lag_,spin_,lorentz_]:=Module[{tmplag,inolist,gaugelist,result},
(* Replacement rules for the SUSY transformation of the component fields *)
  CreateVectorDeltaSUSY[];FR$DPsiRules={};FR$DPsibarRules={};

(* Preparing the Lagrangian \[Rule] transforming the derivatives as an additional index *)
  tmplag=ConvDer[Theta2Component[lag]+Thetabar2Component[lag]];
  tmplag=If[Head[tmplag]===Plus, List@@tmplag, {tmplag}];
  tmplag=Expand[Tonc[#]&/@tmplag];

(* Lists of field *)
  inolist=ConvDer[del[Indexify[SF2Ino[#]],Index[Lorentz,lorentz]]&/@M$VectorSuperfieldNames];
  gaugelist=ConvDer[del[Indexify[SF2Boson[#]],Index[Lorentz,lorentz]]&/@M$VectorSuperfieldNames];

(* Computation of the derivatives with respect to the fields *)
  Print[Style["Variations of the vector Lagrangian from the derivation of Euler-Lagrange equations...",Orange,Bold]];
  result= Join[
    Table[Plus@@(DerF[#,inolist[[iii]],spin] &/@tmplag),{iii,1,Length[inolist]}],
    Table[Plus@@(DerV[#,gaugelist[[iii]],spin] &/@tmplag),{iii,1,Length[gaugelist]}]];

(* Restoring spacetime derivatives *)
  result=result//.{x_[aa__,mu_,FR$del]:>del[x[aa],mu],x_[mu_,FR$del]:>del[x,mu]}/.Index[_,lorentz]->lorentz;
OptimizeIndex[Plus@@result]];


(* ::Section:: *)
(*Shifting the current with respect to a constant*)


(* ::Text:: *)
(*The supercurrent is defined up to a total derivative -> need to be subtracted for correct non-derivative couplings to goldstino and gravitino*)


SupercurrentShift[sf_,lor_,sp_]:=Block[{fer=SF2Weyl[sf],sca=SF2Scalar[sf],isp=Unique["sp"],isp2=Unique["sp2"],ispd=Unique["spd"],idx,ilor=Unique[lor]},
  idx=$IndList[sf]/.Index[a_]:>Unique["ids"];
  fer=fer[isp,Sequence@@idx];
  sca=anti[sca[Sequence@@idx]/.fi_[]->fi];

  Return[Expand[1/(Sqrt[2]2)del[sca nc[fer]*si[lor,sp,ispd]*sibar[ilor,ispd,isp],ilor]-1/(2 Sqrt[2]) del[sca  nc[fer/.isp->sp],lor]]];
];


(* ::Section:: *)
(*Core routine to compute the supercurrent*)


SuperCurrent[lv_,lc_,lw_,spin_,lorentz_]:=Module[{JC,JW,JV,JDLC,JDLV, lagC,lagV,lagW,shi},
  (* Welcome message *)
  Print[Style["Computation of the supercurrent.",Green,Bold]];

  (* Pre-processing the Lagrangians *)
  lagC = OptimizeIndex[Expand[lc]];
  lagV = OptimizeIndex[Expand[lv]];
  lagW = OptimizeIndex[Expand[lw]];

  (* Supersymmetric transformation of the SUSY Lagrangian *)
  JC = If[lagC=!=0, DeltaSUSYChiral[lagC,spin,lorentz],0];
  JV = If[lagV=!=0, OptimizeIndex[DeltaSUSYVector[lagV,spin,lorentz]],0];
  JW = If[lagW=!=0, OptimizeIndex[DeltaSUSYSuperW[lagW,spin,lorentz]],0];

  (* Variations of the Lagrangian from the derivation of Euler-Lagrange equations *)
  JDLC = If[lagC=!=0, DLC[lagC,spin,lorentz],0];
  JDLV = If[lagV=!=0, DLV[lagV,spin,lorentz],0];

  (* Shifts *)
   shi = If[lagC=!=0,Plus@@ (OptimizeIndex[SupercurrentShift[#,lorentz,spin]]&/@M$ChiralSuperfieldNames),0];

  (* Footer *)
  Print[Style["Done.",Green,Bold]];

(* Output *)
(-JC-JV-JW+JDLC+JDLV-shi)/.Index[Spin1|Spin2,a_]->a];
