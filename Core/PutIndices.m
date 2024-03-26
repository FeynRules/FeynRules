(* ::Package:: *)

(* ::Title:: *)
(*Index restore and flavor expansion routines*)


(* ::Section:: *)
(*Useful functions*)


PutIndexName[f_?FieldQ[ind__]] := If[Length[{ind}] == Length[$IndList[f]],
                                     (done[f] @@ Table[Append[$IndList[f][[kk]], {ind}[[kk]]], {kk, Length[{ind}]}]) //. Index[name_, Index[name_, kk__]] -> Index[name,kk],
                                     Message[PutIndexName::NoIndexMatch]];

PPInd[f_?(FieldQ), ind___] := Block[{tempind = List[ind], temp1 = {}, indlist = $IndList[f], output},
    indlist = indlist /. Index[name_] -> name;
    If[Length[tempind] == Length[indlist],
    Do[temp1 = If[FreeQ[tempind[[ll]], Index], Append[temp1, Index[indlist[[ll]], tempind[[ll]]]], Append[temp1, tempind[[ll]]]],
       {ll, Length[tempind]}];
    output = Done[f] @@ temp1,
    output = Done[f] @@ tempind];
    output];

PrePutIndices[expr_] := Block[{BlockIt},
   expr //. {f_?(FieldQ)[ind___] :> PPInd[f, ind]}//.TensDot[u__,v_?(Head[#]=!=HCPI && Head[#]=!=HC&)][rr_, ss_] -> BlockIt[TensDot][u, v[rr,ss]]//.{TensDot[u__,HCPI[v_]][rr_, ss_] -> BlockIt[TensDot][u, HCPI[v],v[rr,ss]],TensDot[u__,HCPI[v_]][rr_, ss_] -> BlockIt[TensDot][u, HC[v],v[rr,ss]]} //. $TensIndRules//.{BlockIt[TensDot][u__, HCPI[v_],v_[rr_, ss_]] -> TensDot[u, HCPI[v]][rr, ss],BlockIt[TensDot][u__, HC[v_],v_[rr_, ss_]] -> TensDot[u, HC[v]][rr, ss],BlockIt[TensDot][u__, v_[ind__, rr_, ss_]] -> TensDot[u, v[ind]][rr, ss], BlockIt[TensDot][u__, v_[rr_, ss_]] -> TensDot[u, v][rr, ss]} //. {del[fff_, Except[Index[___], mu_]] -> del[fff, Index[Lorentz, mu]]} /. Done -> Identity]; 

(*********************************************************************)
NoTensPutInd[f_, ind_] := Block[{indl = $IndList[f], indi, output, mergeindname},
    mergeindname[Index[name_], i_] := Index[name, i];
    indi = Table[mergeindname[indl[[rr]], ind[[rr]]], {rr, Length[ind]}] //. Index -> NTIndex;
    output = f @@ indi];


TreatAllowSummation[expr_] := Block[{temp},
    temp = expr /. {yy_?(NoTensQ[#] === True &)[ff_?(FreeQ[#, Index|NTIndex]&)] :> 
          Module[{i}, ((ASTensor[yy][Append[$IndList[yy][[1]], ff],Append[$IndList[yy][[1]], i]])/. Index -> ASIndex)
              *IndexDelta[Append[$IndList[yy][[1]], ff],Append[$IndList[yy][[1]], i]]],
          yy_?(NoTensQ[#] === True &)[Index[name_, ff_]] :> 
              Module[{i}, ASTensor[yy][ASIndex[name, ff],ASIndex[name, i]] *IndexDelta[Append[$IndList[yy][[1]], ff],Append[$IndList[yy][[1]], i]]],
          yy_?(NoTensQ[#] === True &)[NTIndex[name_, ff_]] :> 
              Module[{i}, ASTensor[yy][ASIndex[name, ff],ASIndex[name, i]] *IndexDelta[Append[$IndList[yy][[1]], ff],Append[$IndList[yy][[1]], i]]]}];
 







IdentifyChains[chain__]:=Block[{tmpchain={chain},IdIndex,curch,newtmpchain,reslist={},IL},
   IL[gg_?(GammaMatrixQ[#]===True&)]:={Spin,Spin};
   IL[gg_?(GammaMatrixQ[#]=!=True&)]:=$IndList[gg];
   tmpchain=IdIndex[#,Take[IL[#/.HCPI -> Identity/.tt_[__]->tt],-2]]&/@tmpchain;
   While[tmpchain=!={},
         newtmpchain={};
         curch={tmpchain[[1]]};
         Do[If[tmpchain[[kk,2,1]]===Last[curch][[2,2]],
               curch=Append[curch,tmpchain[[kk]]],
               newtmpchain=Append[newtmpchain,tmpchain[[kk]]]],
         {kk,2,Length[tmpchain]}];
   curch=curch/.IdIndex[tt_,_]->tt;
   reslist=Append[reslist,TensDot@@curch];
   tmpchain=newtmpchain];
   reslist]


(* ::Section:: *)
(*Flavor expansion routine*)


(* ::Text:: *)
(*BF note: we can remove the power of anything. Indeed, if anything with an index is inside, it can only be a parameter with the allowsummation flag set to true (otherwise the expression is ill-defined). *)
(*The only exception is the square.*)


GetInds[name_][xx___,aa_+bb_,yy___]:=GetInds[name][xx,aa,yy]+GetInds[name][xx,bb,yy];
GetInds[name_][xx___,Power[a_,b_],yy___]:=GetInds[name][xx,yy]/; (b=!=2);
GetInds[name_][xx___,Power[a_,b_],yy___]:=GetInds[name][xx,a,a,yy]/; (b===2);
GetInds[name_][xx___,ff_[facs__],yy___]:=GetInds[name][xx,ff,facs,yy]/;(ff=!=Plus)&&(ff=!=Index)&&(ff=!=Power)&&(ff=!=NTIndex);
GetInds[name_][xx___,tt_?((FreeQ[#,Index]&&FreeQ[#,NTIndex])&),yy___]:=GetInds[name][xx,yy];
GetInds[name_][xx___,Index[n__],yy___,Index[n__],zz___]:=GetInds[name][xx,Index[n],yy,zz];
GetInds[name_][xx___, Index[_, _?NumericQ], yy___] := GetInds[name][xx, yy];
GetInds[name_][xx___, Index, yy___] := GetInds[name][xx, yy];
GetInds[name_][xx___,NTIndex[inds__],yy___] := GetInds[name][xx, Index[inds],yy]
GetInds[namelist_][xx___,Index[name_,n_],yy___]:=GetInds[namelist][xx,yy] /; Not[MemberQ[namelist,name]];


MakeIndexSumPI[lisi_List] := MakeIndexSumPI /@ lisi;

MakeIndexSumPI[expr_?(Head[#]=!=List &),name_]:=Block[{tmpexprlist,getindlist,joingetindlist,sumindlist,tenslist,idSum,indlist},
   tmpexprlist=Expand[expr];
   tmpexprlist=If[Head[tmpexprlist]===Plus,List@@tmpexprlist,{tmpexprlist}];
   getindlist=GetInds[name][#]&/@tmpexprlist;
   getindlist=getindlist//.GetInds[_]:>GI //. GI[exp___]:>Sort[List[exp]]/.{}->1;
   joingetindlist = Union[getindlist];
   sumindlist = joingetindlist/. Index[nam_,k_]:>{k,First[MRIndexRange[Index[nam]]],Last[MRIndexRange[Index[nam]]]};
   (*Expand the indices of the tensors and remove the zero entries first one (tensor) by one *)
   tenslist=Cases[tmpexprlist/.Conjugate->Identity,_?TensQ,\[Infinity]];
   For[kk=1,kk<=Length[tenslist],kk++,
     indlist=Cases[sumindlist,_?(Not[FreeQ[tenslist[[kk]],#[[1]]]]&),{2}];
     sumindlist=DeleteCases[sumindlist,_?(Not[FreeQ[tenslist[[kk]],#[[1]]]]&),{2}];
     If[Length[indlist]>0,
       tmpexprlist=idSum[tmpexprlist,Sequence@@indlist]/.idSum->Sum;
       tmpexprlist=ApplyDefinitions[tmpexprlist/.{Conjugate[a_?TensQ][x__]->Conjugate[a[x]]}];
     ];
   ];
   tmpexprlist=tmpexprlist//.{Conjugate[tt_?(TensQ)[ind__]] -> Conjugate[tt][ind]};

   tmpexprlist = Plus @@ Table[Plus @@ Extract[tmpexprlist, Position[getindlist, joingetindlist[[kk]]], IndSum[#, Sequence @@ sumindlist[[kk]]]&], {kk, Length[joingetindlist]}];
   tmpexprlist=tmpexprlist /. IndSum -> IndSumOpt1;
   tmpexprlist=tmpexprlist /. IndSumOpt1 -> IndSumOpt2;
   tmpexprlist=tmpexprlist /. IndSumCombine -> IndSum;
   tmpexprlist = tmpexprlist /. IndSum :> Sum;
(* Benj: Added the case where we have a single term here *)
   If[Head[tmpexprlist]===Plus,Plus@@(Times@@@tmpexprlist),Plus@(Times@@tmpexprlist)]];

MR$IndexExpand[expr_, indname_] := MakeIndexSumPI[expr, indname] /. IndexSum -> Sum;

IndSumOpt1[termexpr_, xx___, {name1_, x1_, x2_}, {name2_, y1_, y2_}, yy___] := IndSumOpt1[termexpr, xx, {name2, y1, y2}, {name1, x1,x2}, yy] /; (y1>x1);
IndSumOpt1[termexpr_, xx___, {name1_, x1_, x2_}, {name2_, x1_, y2_}, yy___] := IndSumOpt1[termexpr, xx, {name2, x1, y2}, {name1, x1,x2}, yy] /; (y2 > x2);

IndSumOpt1[termexpr_, xx___, 1, yy___] := IndSumOpt1[termexpr,xx,yy];

IndSumOpt1[termexpr_,1] := termexpr;

IndSumOpt2[tempexpr_, inds__] := IndSumOpt3[tempexpr][inds][Rule@@@(Transpose[{List[inds][[All,1]], Table[Symbol["ISind"<>ToString[ii]], {ii, Length[{inds}]}]}])];

IndSumOpt3[tempexpr_][inds__][rulelist_] := IndSumCombine[tempexpr ,inds] /. rulelist;

IndSumCombine /: aa_ *IndSumCombine[tempexpr1_, inds___] := IndSumCombine[aa*tempexpr1, inds];
IndSumCombine /: IndSumCombine[tempexpr1_, inds___] + IndSumCombine[tempexpr2_, inds___] := IndSumCombine[tempexpr1+tempexpr2, inds];




(* ::Section:: *)
(*Index restoring*)


TensQ[AdjointRepTemp[group_]] := TensQ[AdjointRep[group]];

$IndList[AdjointRepTemp[group_]] := $IndList[AdjointRep[group]];

AdjointRepTemp[group_][aa___, i_?(FreeQ[#, Index]&), bb___] := AdjointRepTemp[group][aa, Index[$IndList[AdjointRepTemp[group]][[1]], i], bb];


RemoveTensor[psi1_[inds1___],tt1_,tt2___,psi2_[inds2___]]:=Module[{i,j,temptt1,tind1,tind2,MyConjugate,MyTranspose, MyAdjointRep, MyTP},

   temptt1=PrePutIndices[If[MatchQ[tt1, _[__]], Append[Append[tt1 /. {AdjointRep -> AdjointRepTemp, Conjugate -> MyConjugate, Transpose -> MyTranspose, TP -> MyTP},i],j] /. {TensDot[t1_, tt__, ii_, jj_]:> TensDot[t1,tt][i,j], 
        HCPI[tt__, ii_,jj_]:> HCPI[tt][ii,jj], 
        MyTP[t_][iis__] :> MyTP[t[iis]],
        MyTranspose[tt__, ii_,jj_]:> MyTranspose[tt][ii,jj], 
        MyConjugate[tt__, ii_, jj_] :> MyConjugate[tt][ii,jj]}, tt1[i,j]]] /. Conjugate -> MyConjugate /. {MyConjugate[tt_[ii_,jj_]]:>MyConjugate[tt][jj,ii],MyTranspose[tt_][ii_,jj_]:>MyTranspose[tt][jj,ii], MyTP[t_[iis__]] :> TP[t][iis]};

   If[temptt1 ===0,
      Return[0];
      ];

   tind1=temptt1[[-2]];
   tind2=temptt1[[-1]];

   temptt1=temptt1/.{MyConjugate[tt_][ii_,jj_]:>Conjugate[tt[jj,ii]],MyTranspose[tt_][ii_,jj_]:>tt[jj,ii], AdjointRepTemp -> AdjointRep};

   temptt1 * RemoveTensor[psi1[inds1,tind1],tt2,psi2[inds2,tind2]]];


SaturateChain[psi1_[inds1___],psi2_[inds2___]]:=Module[{indnames1=$IndList[psi1],ninds1=Length[$IndList[psi1]],indnames2=$IndList[psi2],ninds2=Length[$IndList[psi2]],indtab1, indtab2,missing},

   If[Length[{inds1}]!= Length[{inds2}],Message[PutIndices::NoIndexMath]];

   If[Length[{inds1}]<ninds1,
      missing = Complement[indnames1,{inds1}/.Index[name_,_]:>Index[name]];
      indtab1  = Table[Append[missing[[kk]],Symbol["i"<>ToString[kk]<>"$"<>ToString[$ModuleNumber]]],{kk,ninds1-Length[{inds1}]}],
      indtab1={}];

   If[Length[{inds2}]<ninds2,
      missing = Complement[indnames2,{inds2}/.Index[name_,_]:>Index[name]];
      indtab2  = Table[Append[missing[[kk]],Symbol["i"<>ToString[kk]<>"$"<>ToString[$ModuleNumber]]],{kk,ninds2-Length[{inds2}]}],
      indtab2={}];


   ReorderIndices[psi1[inds1,Sequence@@indtab1],psi2[inds2,Sequence@@indtab2]]];




ReorderIndices[psi1_[inds1___],psi2_[inds2___]]:=Block[{indnames1=$IndList[psi1], indnames2=$IndList[psi2], list1 = {inds1}/.Index[name_,_]:>Index[name],list2 = {inds2}/.Index[name_,_]:>Index[name]},

   list1 = Table[Rule[list1[[kk]],{inds1}[[kk]]],{kk,Length[list1]}];
   list2 = Table[Rule[list2[[kk]],{inds2}[[kk]]],{kk,Length[list2]}];

   Dot[psi1[Sequence@@(indnames1/.list1)],psi2[Sequence@@(indnames2/.list2)]]];

RestoreFermionIndices[psi1_,gg___,del[psi2_,mu_]]:=del2[RestoreFermionIndices[psi1,gg,psi2],mu];
RestoreFermionIndices[del[psi1_,mu_],gg___,psi2_]:=del1[RestoreFermionIndices[psi1,gg,psi2],mu];


RestoreFermionIndices[Except[_?(#=!=CC&)[__],psi1_?(FermionQ[#] === True &)],Except[_?(#=!=CC&)[__],psi2_?(FermionQ[#]===True&)]]:=Module[{indnames=$IndList[psi2]},

(*Mod. 01.25.2013. AA: If one of the psi is a ghost then return Dot[psi1,psi2] instead of Dot[psi1[], psi2[]] *)
   If[GhostFieldQ[psi1]===True||GhostFieldQ[psi2]===True,Return[Dot[psi1,psi2]]];

   If[Length[$IndList[psi1]]!= Length[$IndList[psi2]],Message[PutIndices::NoIndexMath]];
   indnames = Table[Append[indnames[[kk]],Symbol["i" <> ToString[kk]<>"$"<>ToString[$ModuleNumber]]],{kk,Length[indnames]}];
   Dot[psi1@@indnames,psi2@@indnames]];


RestoreFermionIndices[Except[_?(#=!=CC&)[__],psi1_?(FermionQ[#] === True &)],gg__,Except[_?(#=!=CC&)[__],psi2_?(FermionQ[#]===True&)]]:=Block[{temp},
   temp={psi1[],Sequence@@(IdentifyChains@@({gg} /. HC -> HCPI)),psi2[]};

   temp = (RemoveTensor@@temp)/.RemoveTensor->SaturateChain;

   Return[temp];
];

del1[a_?(numQ) f_,mu_] := a * del1[f,mu];
del2[a_?(numQ) f_,mu_] := a * del2[f,mu];
del2[0, _] := 0;



ReleaseASIndex[temp_] := temp //. {ASTensor[yy_][ASIndex[name_, i1_], ASIndex[name_, i2_]] * aa_ :> yy[NTIndex[name, i1]] ReplaceIndex[aa,i2,i1],
                    g_[ind1___, ASTensor[yy_][ASIndex[name_, i1_], ASIndex[name_, i2_]], ind2___] * aa_ :> ReplaceIndex[g[ind1, yy[NTIndex[name, i1]], ind2] * aa,i2,i1],
                    h_[ind11___,g_[ind1___, ASTensor[yy_][ASIndex[name_, i1_], ASIndex[name_, i2_]], ind2___], ind22___] * aa_ :> ReplaceIndex[h[ind11,g[ind1, yy[NTIndex[name, i1]], ind2],ind22] * aa,i2,i1]};


PutIndices[expr_, flavorexpandQ_, flavorexpandlist_,nodef___] := 
  Module[{temp, output, ExpandIndices, Dotdone, StartPutIndices, StartPutIndices2, SaturateIndices, AddIndexToField, SatInd, ContinuePutIndices, PutInd, 
          ReorderIndicesNames, SatIndName, flavexp, HCPutIndices, PutIndName, SaturateIndexNames, PutFieldIndices, DiagProd2, tempflavexp, tempnoflavexp, 
          lab999, FRFuncQ, flavexplabel, flavexpblanklist, $AppliedRules = False},   

(* Preparation *)     
    temp = expr /.{Dot[x___,Times[s___,Dot[y___],t___],z___] :> Times[s,t, Dot[x,y,z]] /; FreeQ[{s},Dot] && FreeQ[{t},Dot]};
    temp = temp /. {Power[Dot[xx_, yy___],n_] -> Power[ExpandIndices[Dot[xx, yy]], n]};
    Power[ExpandIndices[xx_],n_] ^:= ExpandIndices @@ Table[xx, {n}];

    temp = temp /. Dot -> FR$Dot /. FR$Dot -> Dot;

(* Index restoration *)
    temp = temp /. Dot -> RestoreFermionIndices /. RestoreFermionIndices -> Dot /. HCPI -> HC /. TP[t_][as__, i_, j_] :> t[as,j,i]; 

    temp = temp //. {del1[Dot[psi1_,psi2_],mu_] :> Dot[del[psi1,mu],psi2], del2[Dot[psi1_,psi2_],mu_] :> Dot[psi1, del[psi2, mu]]};

    temp = temp //. {Conjugate[tt_?(TensQ)[ind__]] -> Conjugate[tt][ind]};

(* Modifications by A. Alloul 12/11/2012:
   1) Modified the  line below
   2) Modifications bring a considerable speed gain: Applying  Expand on the whole lagrangian is much slower
      than applying it on every term.
*)
(*    temp = Expand[temp];*)
    temp = If[Head[temp]===Plus,FieldExpand/@temp,FieldExpand[temp]];


(* Modifications by A. Alloul 12/11/2012:
   1) Modified the  line below
   2) Modifications bring a considerable speed gain: Applying  ApplyDefinitions on the whole lagrangian is much slower
      than applying it on every term.
*)
(*    temp = ApplyDefinitions[temp];*)

    If[nodef=!=True,temp = If[Head[temp]===Plus,ApplyDefinitions/@temp,ApplyDefinitions[temp]]];

    temp = temp //. {HC -> HCPutIndices} //. {HCPutIndices[t_?(TensQ)][ind___] -> HCPutIndices[t[ind]]};
    temp = PrePutIndices[temp];

(* Modifications by A. Alloul 12/11/2012:
   1) Modified the  line below
   2) Modifications bring a considerable speed gain: Applying  Expand on the whole lagrangian is much slower
      than applying it on every term.
*)
(*    temp = Expand[temp]; *)
    temp = If[Head[temp]===Plus,FieldExpand/@temp,FieldExpand[temp]]; 

(* Index restoration for Eps and co. *)
    FRFuncQ[xx_] := (TensQ[xx] === True_) || (FieldQ[xx] === True) || (xx === del) || (xx ===ME) || (xx === IndexDelta) || (xx == FV) || (xx === Eps);
    temp = temp //. Dispatch[{Except[Index, f_][xx___, Index[name_, a_], yy___] Except[Index, g_?FRFuncQ][zz___, a_, tt___] :> f[xx, Index[name, a], yy] g[zz, Index[name, a], tt],
                     Except[Index, h_][bb___, Except[Index, f_][xx___, Index[name_, a_], yy___], cc___] Except[Index, g_?FRFuncQ][zz___, a_, tt___] :> h[bb, f[xx, Index[name, a], yy], cc] g[zz, Index[name, a], tt],
                     Except[Index, f_][xx___, Index[name_, a_], yy___] Except[Index, h_][bb___, Except[Index, g_?FRFuncQ][zz___, a_, tt___], cc___] :> f[xx, Index[name, a], yy] h[bb, g[zz, Index[name, a], tt], cc],
                     Except[Index, h_][bb___, Except[Index, f_][xx___, Index[name_, a_], yy___], cc___] Except[Index, j_][dd___, Except[Index, g_?FRFuncQ][zz___, a_, tt___], ee___] :> h[bb, f[xx, Index[name, a], yy], cc] j[dd, g[zz, Index[name, a], tt], ee]}];
    temp = temp //. HCPutIndices -> HC;

(* Modifications by A. Alloul 12/11/2012:
   1) Modified the  line below
   2) Modifications bring a considerable speed gain: Applying  Expand on the whole lagrangian is much slower
      than applying it on every term.
*)
(*    temp = Expand[temp]; *)
    temp =If[Head[temp]===Plus,FieldExpand/@temp,FieldExpand[temp]];

(* Release AllowSummation *)

    temp = ReleaseASIndex[temp];

  (*  temp = temp //. ASNT[Index] -> Index;*)
    temp = temp /. DiagProd2 -> DiagProd;


(* Modifications by A. Alloul 12/11/2012:
   1) Modified the  line below
   2) Modifications bring a considerable speed gain: Applying  ApplyDefinitions on the whole lagrangian is much slower
      than applying it on every term.
*)
(*    temp = ApplyDefinitions[temp];*)
    If[nodef=!=True,temp =If[Head[temp]===Plus,ApplyDefinitions/@temp,ApplyDefinitions[temp]]];

    If[flavorexpandQ, (*Print["Expanding flavors..."];*) 
       flavexpblanklist = Index[#,__]& /@ flavorexpandlist;
       If[Not[And@@ (FreeQ[temp, #]& /@ flavexpblanklist)],
             Label[flavexplabel];
             If[Head[temp] === Plus, tempflavexp = Select[Expand[temp], Not[ListFreeQ[#, flavexpblanklist]]&];
                                     tempnoflavexp = Expand[temp - tempflavexp],
                                     tempflavexp = temp; tempnoflavexp = 0];

             tempflavexp = tempflavexp //. Index[name_, xx_?NumericQ] :> Done[Index][name, xx] /; MemberQ[flavorexpandlist, name];
             tempflavexp = If[Head[tempflavexp]===Plus,(MR$IndexExpand[#, flavorexpandlist]&)/@tempflavexp,MR$IndexExpand[tempflavexp, flavorexpandlist]];

             tempflavexp = tempflavexp /. Done[Index] :> Index;


(* Modifications by A. Alloul 12/11/2012:
   1) Modified the  line below
   2) Modifications bring a considerable speed gain: Applying  ApplyDefinitions on the whole lagrangian is much slower
      than applying it on every term.
*)
(*    temp = ApplyDefinitions[tempflavexp];*)
             tempflavexp = If[Head[tempflavexp]===Plus,ApplyDefinitions/@tempflavexp,ApplyDefinitions[tempflavexp]];

(* Modifications by A. Alloul 12/11/2012: Same reasons as above. Original version commented *)
             (*If[Not[$AppliedRules], tempnoflavexp = ApplyDefinitions[tempnoflavexp]; $AppliedRules = True];*)
               If[Not[$AppliedRules], tempnoflavexp = If[Head[tempnoflavexp]===Plus,ApplyDefinitions/@tempnoflavexp,ApplyDefinitions[tempnoflavexp]]; $AppliedRules = True];
             temp = Expand[tempflavexp + tempnoflavexp];
             If[Not[And@@(FreeQ[temp, Index[#, _?(Not[NumericQ[#]]&)]]& /@ flavorexpandlist)], Goto[flavexplabel]]
       ](*endn if Not...*)
    ];(*end if flavorexpandQ*)
    temp = temp //. {Conjugate[tt_?(TensQ)][ind__] -> Conjugate[tt[ind]]};

    temp  = PrePutIndices[temp //. f_?(FieldQ)[] -> f];

    temp  = temp /. ExpandIndices -> Times;

    Return[temp];

];


ListFreeQ[expr_,lisi_List]:= And @@ (FreeQ[expr,#]& /@ lisi);
