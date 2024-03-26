(* ::Package:: *)

(* ::Section:: *)
(*Useful tools*)


(* ::Subsection::Closed:: *)
(*Indexize: add the type of an index for a field*)


Indexize[item_]:=Block[{resu},
  resu=item/.fi_?(FieldQ[#]===True&)[inds__]:>fi[Sequence@@Inner[Index,$IndList[fi],{inds},List]]/.Index[Index[type_],val_]->Index[type,val];
  resu=resu/.para_?(MemberQ[M$IndParam,#]&)[inds__]:>para[Sequence@@Inner[Index,$IndList[para],{inds},List]]/.Index[Index[type_],val_]->Index[type,val];
  Return[resu];
];


(* ::Section:: *)
(*Settings*)


(* ::Subsection::Closed:: *)
(*Replacement rules for the group generators*)


FR$GenRules={};


(* ::Subsection::Closed:: *)
(*Rotations*)


FR$ToGaugeBasis={};
FR$UnitarySimplifications={Power->Pow, Pow[a_,n_] Pow[b_,n_]:>Pow[a b,n]};


(* ::Subsection::Closed:: *)
(*Mass matrices*)


FR$MassMatrices={};
FR$MassMatricesToCalculate={};


(* ::Subsection::Closed:: *)
(*SelfConjugateQ*)


SelfConjugateQ[fi_?(FieldQ[#]===True&)[inds__]]:=SelfConjugateQ[fi];


(* ::Section:: *)
(*Initialization of the mixing relations*)


(* ::Subsection:: *)
(*Testing unit for the mixing declaration + declaration of the mixing parameters*)


(* ::Subsubsection::Closed:: *)
(*Core testing function*)


TestingMixingDeclaration[mbasis_,gbasis_,Val_,matname_,bname_]:=Block[{nd,goodvalue=Val,goodblock=bname,mname,TheVal=Val},
  (* Initialization *)
  If[Val =!=MR$Null && ListQ[Val] && And@@(MatchQ[#,Rule[_,_]]&/@Val), TheVal ={Val}]; 

  (* Bases consistencies *)
  nd=CheckFieldBases[mbasis,gbasis];

  (* If the value is given *)
  If[matname=!={MR$Null}, mname=matname/.Blank->MR$Null/.MR$Null[]->MR$Null; 
    Block[{num}, 
      num=Position[mname,#][[1,1]];
      (* Checks: one value and one set of indices (if any) for each mixing matrix *)     
      If[TheVal=!=MR$Null, goodvalue=TheVal[[num]]];
      If[bname=!={MR$Null}, If[Dimensions[#]=!=Dimensions[bname[[num]]], Message[MassDiag::BlockName]; Abort[],goodblock=bname[[num]]]];
      goodvalue=goodvalue/.Blank->MR$Null/.MR$Null[]->MR$Null;      
      goodblock=goodblock/.Blank->MR$Null/.MR$Null[]->MR$Null;
      If[#=!=MR$Null, CheckValue[#,goodvalue,goodblock,Dimensions[gbasis[[1]]][[-1]]], CheckNumerical[goodvalue]];
    ]&/@mname,
    CheckNumerical[goodvalue];
  ];
  Return[];
];


(* ::Subsubsection::Closed:: *)
(*Checking the consistency of the bases*)


(* ::Text:: *)
(*The function CheckFieldBases checks *)
(*   1. if the number of elements of the gauge and mass bases are the same *)
(*   2. if the number of mass basis extracted from a gauge basis is equal to two 9for the scalar-pseudoscalar mixing*)
(*   3. that the number of understood indices ( _ ) is consistent*)


CheckFieldBases[mbasis_,gbasis_]:=Block[{nd=Dimensions[gbasis][[1]]},
  If[nd=!=1 && nd=!=2, Message[MassDiag::BasisInconsistency]; Abort[]];
  CheckBasis[mbasis,#]&/@gbasis;
  Return[nd];
];


CheckBasis[mbasis_,gbasis_]:=Block[{lg,lm,maprules},
  maprules = myrule[Sequence@@Reverse[#]]&/@FR$ReprMap/.myrule->Rule;
  If[Not[And@@(Length[#]===Length[gbasis]&/@mbasis)],Message[MassDiag::BasisInconsistency]; Abort[]];
  If[Length[mbasis]=!=1 && Length[mbasis]=!=2, Message[MassDiag::MassBasisInconsistency]; Abort[]];
  lm=Tally[Count[{#/.{fi_?(FieldQ[#]===True&)[ids__]->ids/.maprules}},myBlank[]]&/@Flatten[mbasis/.Blank->myBlank]];
  lg=Tally[Count[{#/.{fi_?(FieldQ[#]===True&)[ids__]->ids/.maprules}},myBlank[]]&/@Flatten[gbasis/.Blank->myBlank]];
  If[Length[lm]=!=1 || Length[lg]=!=1, Message[MassDiag::BasisInconsistencyBlank]; Abort[]];
  lm=lm[[1,1]];lg=lg[[1,1]];
  If[lg=!=lm,Message[MassDiag::BasisInconsistencyBlankRot]; Abort[]];
];


(* ::Subsubsection:: *)
(*When a symbol is given: checking the sattelite rules + declaration of the parameter if relevant*)


(* ::Text:: *)
(*A symbols can be attached to values, LH blocknames, indices, etc... the parameters can be declared or not, etc... all of this is checked here*)


DeclareFullNewPrm[Thebname_,matname_,len_]:=Block[{tmpdecl,index={},desc,bname=Thebname},
  (* Initialization *)
  If[ListQ[$IndList[matname]], index=$IndList[matname]];

  (* If the block name is not provided *)
  If[Thebname==={MR$Null}, bname=ToString[matname]<>"BLOCK"];
  BlockToParam[matname]=bname;

  (* Externals: declaring the real part of the parameter *)
  tmpdecl=List[bname, 
    Table[{List[i,j],List[Symbol["R"<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]],NoValue[1], False,
      ToString[matname]<>ToString[i]<>"x"<>ToString[j]<>" (parameter to be computed by the MD routine)"]},{i,len},{j,len}
    ]
  ];
  tmpdecl = tmpdecl/.{bname,aaA_}:>{bname,Flatten[aaA,1]}; 
  EParamList = Append[EParamList,tmpdecl];
  tmpdecl = tmpdecl[[2,All,2]];
  tmpdecl = Insert[#,Ext,2]&/@tmpdecl;
  tmpdecl = Insert[#,NoValue[1],3]&/@tmpdecl;
  tmpdecl = Insert[#,bname,4]&/@tmpdecl;
  ParamList = Join[ParamList,tmpdecl];

  (* Externals: declaring the Imaginary part of the parameter *)
  tmpdecl=List[Symbol["IM"<>ToString[bname]], 
    Table[{List[i,j],List[Symbol["I"<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]],NoValue[1], False,
      "IM"<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]<> "(parameter to be computed by the MD routine)"]},{i,len},{j,len}
    ]
  ];
  tmpdecl = tmpdecl/.{Symbol["IM"<>ToString[bname]],aaA_}:>{Symbol["IM"<>ToString[bname]],Flatten[aaA,1]};
  EParamList = Append[EParamList,tmpdecl];       
  tmpdecl = tmpdecl[[2,All,2]];
  tmpdecl = Insert[#,Ext,2]&/@tmpdecl;
  tmpdecl = Insert[#,NoValue[1],3]&/@tmpdecl;
  tmpdecl = Insert[#,Symbol["IM"<>ToString[bname]],4]&/@tmpdecl;
  ParamList = Join[ParamList,tmpdecl];

  (* The internal parameters *)
  tmpdecl = Flatten[Table[{Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]],
    Symbol["R"<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]]+ I*Symbol["I"<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]],
    True,ToString[bname]<>" ("<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]<>")"}, {i,len},{j,len}],1];
  IParamList=Join[IParamList, tmpdecl];
  tmpdecl = Insert[#,Int,2]&/@tmpdecl;
  ParamList = Join[ParamList,tmpdecl];

  ParamRules=Flatten[Join[ParamRules,
    Table[MyRule[Symbol["R"<>ToString[matname]][i,j],Symbol["R"<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]]],{i,len},{j,len}]]/.MyRule->Rule];
  ParamRules=Flatten[Join[ParamRules,
    Table[MyRule[Symbol["I"<>ToString[matname]][i,j],Symbol["I"<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]]],{i,len},{j,len}]]/.MyRule->Rule];
  ParamRules=Flatten[Join[ParamRules,
    Table[MyRule[matname[i,j],Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]]],{i,len},{j,len}]]/.MyRule->Rule];

(*Mod. 01.25.2013 AA: Added this rule so that we can link MatNxM to Mat[Index[typ,N],Index[typ,M]]*)
  ToIndexRules=(Cases[ParamRules/.Rule->myRule,myRule[_[__],_]]/.myRule[a_,b_]->myRule[b,a]/.myRule->Rule);
(*Add-On end*)
  desc = {ParameterType->External,ComplexParameter->False,BlockName->bname,Indices->index};
  DeclareTensor[Symbol["R"<>ToString[matname]],index,Sequence@@desc];
  M$Parameters=Join[M$Parameters,{Equal[Symbol["R"<>ToString[matname]],desc]}]; 
  MR$ParameterRules[Symbol["R"<>ToString[matname]]]=desc;
  desc = {ParameterType->External,ComplexParameter->False,BlockName->Symbol["IM"<>ToString[bname]],Indices->index};
  DeclareTensor[Symbol["I"<>ToString[matname]],index,Sequence@@desc];
  M$Parameters=Join[M$Parameters,{Equal[Symbol["I"<>ToString[matname]],desc]}]; 
  MR$ParameterRules[Symbol["I"<>ToString[matname]]]=desc;
  desc = {ParameterType->Internal,ComplexParameter->True, Value->{matname[i_,j_]:>Symbol["R"<>ToString[matname]][i,j]+I*Symbol["I"<>ToString[matname]][i,j]},
    Indices->index};
  DeclareTensor[matname,index,Sequence@@desc];
  M$Parameters=Join[M$Parameters,{Equal[matname,desc]}]; 
  MR$ParameterRules[matname]=desc;

  M$IndParam=Join[M$IndParam,{matname,Symbol["I"<>ToString[matname]],Symbol["R"<>ToString[matname]]}];
  MR$ParameterList=Join[MR$ParameterList,{matname,Symbol["I"<>ToString[matname]],Symbol["R"<>ToString[matname]]}];
  CnumQ[matname[ii__]]^:=True;
];


DeclareExternalPrm[Thebname_,matname_,len_,Val_]:=Block[{tmpdecl,index={},desc,bname=Thebname},
  (* Initialization *)
  If[ListQ[$IndList[matname]], index=$IndList[matname]];

  (* If the block name is not provided *)
  If[Thebname==={MR$Null}, bname=ToString[matname]<>"BLOCK"];
  BlockToParam[matname]=bname;

  (* Declaration *)
  tmpdecl=List[bname, 
    Table[{List[i,j],List[Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]],
      Cases[Val/.Rule->myrule,myrule[matname[i,j],_]][[1,2]], False,
      ToString[bname]<>" ("<>ToString[matname]<>ToString[i]<>"x"<>ToString[j]<>") )"]},{i,len},{j,len}
    ]
  ];
  tmpdecl = tmpdecl/.{bname,aaA_}:>{bname,Flatten[aaA,1]};
  EParamList = Append[EParamList,tmpdecl];       
  tmpdecl = tmpdecl[[2,All,2]];
  tmpdecl = Insert[#,Ext,2]&/@tmpdecl;
  tmpdecl = Insert[#,NoValue[1],3]&/@tmpdecl;
  tmpdecl = Insert[#,bname,4]&/@tmpdecl;
  ParamList = Join[ParamList,tmpdecl];

  ParamRules=Flatten[Join[ParamRules,
    Table[MyRule[matname[i,j],Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]]],{i,len},{j,len}]]/.MyRule->Rule];
  desc = {ParameterType->External,ComplexParameter->False,BlockName->bname,Indices->index, Value -> Val};
  DeclareTensor[matname,index,Sequence@@desc];
  M$Parameters=Join[M$Parameters,{Equal[matname,desc]}]; 
  MR$ParameterRules[matname]=desc;

  M$IndParam=Join[M$IndParam,{matname}];
  MR$ParameterList=Append[MR$ParameterList,matname];
];


DeclareInternalPrm[Thebname_,matname_,len_,Val_]:=Block[{tmpdecl, index={}, desc,bname},
  (* Initialization *)
  If[ListQ[$IndList[matname]], index=$IndList[matname]];

  (* Declaration *)
  tmpdecl = Flatten[Table[{Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]],Cases[Val/.Rule->myrule,myrule[matname[i,j],_]][[1,2]],
         True, ToString[matname]<>ToString[i]<>"x"<>ToString[j]}, {i,len},{j,len}],1];
  IParamList=Join[IParamList, tmpdecl];
  tmpdecl = Insert[#,Int,2]&/@tmpdecl;
  ParamList = Join[ParamList,tmpdecl];

  ParamRules=Flatten[Join[ParamRules,
    Table[MyRule[matname[i,j],Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]]],{i,len},{j,len}]]/.MyRule->Rule];
  desc = {ParameterType->Internal,ComplexParameter->True, Value->Val,Indices->index};
  DeclareTensor[matname,index,Sequence@@desc];
  M$Parameters=Join[M$Parameters,{Equal[matname,desc]}]; 
  MR$ParameterRules[matname]=desc;

  M$IndParam=Join[M$IndParam,{matname}];
  MR$ParameterList=Append[MR$ParameterList,matname];

  CnumQ[matname[ii__]]^:=True;
];


CheckValue[matname_,Val_,blockname_,len_]:=Block[{bname=blockname,tmpdecl,IsInternal},
(*Mod. 01.29.2013 AA: Added numQ[mixing matrix element] = True*)
  Table[numQ[ToExpression[ToString[matname]<>ToString[i]<>"x"<>ToString[j]]]=True,{i,len},{j,len}];
(*Add-On end*)
  (* Checking if the parameter has already been declared; if yes: no computation by the code *)
  If[MemberQ[M$Parameters[[All,1]],matname],
    If[NoCompute[matname]=!=False, 
      NoCompute[matname]=True;
      Table[FrV[matname[i,j]]=Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]],{i,len},{j,len}];
      Table[CnumQ[Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]]]^:=True,{i,len},{j,len}];
    ],
    (* NO: we need to declare the parameter *) 
    NoCompute[matname]=False;
    (* Value: if formula -> internal parameter; if numerical value -> external parameters; it not existing -> external *)
    If[Val=!=MR$Null, 
      If[And@@(MatchQ[#,Rule[_,_]]&/@ Val), 
        If[Not[And@@(NumericQ/@Val[[All,2]])], IsInternal=True, IsInternal=False;If[blockname==={MR$Null},bname=ToString[matname]<>"BLOCK"]],
        Message[MassDiag::value]; Abort[]
      ];
      (NumericalValue[#[[1]]]=NumericalValue[#[[2]]])&/@Val,
      (* else value === MR$Null *)
      FR$MassMatricesToCalculate=Append[FR$MassMatricesToCalculate,matname]; IsInternal=0;
    ];
(*Mod 01.25.2013 see PutMatrixIndex below*)
    $IndList[matname]={Index[DUMMY],Index[DUMMY]};


    Table[FrV[matname[i,j]]=Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]],{i,len},{j,len}];
    Table[CnumQ[Symbol[ToString[matname]<>ToString[i]<>"x"<>ToString[j]]]^:=True,{i,len},{j,len}];

    If[IsInternal===0, DeclareFullNewPrm[bname,matname,len]];
    If[IsInternal===False, DeclareExternalPrm[bname,matname,len,Val]];
    If[IsInternal===True, DeclareInternalPrm[bname,matname,len,Val]];
  ];
  Return[];
];


(* ::Text:: *)
(*Added by AA on 01.25.2013: Adds an index type to the mass matrices that need one*)
(*  Input -> matrixname*)
(*  output -> List[Index[type1],Index[type2]]*)
(*  Disabled for the moment*)


PutMatrixIndex[id_]:=Block[{gaugeindlist,massindlist,ind,list},
  (*Initialization*)
  list=Which[
    PRIVATE`Type[id]==="FLR",List[{GaugeBasis[id,"L"],MassBasis[id],MatrixSymbol[id,"L"]},{GaugeBasis[id,"R"],MassBasis[id],MatrixSymbol[id,"R"]}],
    PRIVATE`Type[id]==="CWeyl",List[{GaugeBasis[id][[1]],MassBasis[id][[1]],MatrixSymbol[id][[1]]},{GaugeBasis[id][[2]],MassBasis[id][[2]],MatrixSymbol[id][[2]]}],
    PRIVATE`Type[id]==="SPS",List[{GaugeBasis[id],MassBasis[id,"S"],MatrixSymbol[id,"S"]},{GaugeBasis[id],MassBasis[id,"PS"],MatrixSymbol[id,"PS"]}],
    True,List[{GaugeBasis[id],MassBasis[id],MatrixSymbol[id]}]];
  (If[#[[3]]==="No matrix symbol found.",Return[]];
   gaugeindlist=$IndList/@(#[[1]]/.a_?(FieldQ[#]&)[___]:>a);
   massindlist=$IndList/@(#[[2]]/.a_?(FieldQ[#]&)[___]:>a);
   (*If fields in the gauge basis don't have the same indices -> DUMMY *)
   If[Equal@@gaugeindlist=!=True,$IndList[#[[3]]]={Index[DUMMY],Index[DUMMY]};IndexType[#[[3]]]={Index[DUMMY],Index[DUMMY]};];

  (*Otherwise we remove the colour and spin ?*)
  gaugeindlist=Flatten[DeleteDuplicates[gaugeindlist]];
  massindlist=Flatten[DeleteDuplicates[massindlist]];
  ind=DeleteCases[Intersection[gaugeindlist,massindlist],a_?(MatchQ[IndexRange[#],NoUnfold[_]]&)|Index[Spin]|Index[Lorentz]];
  If[ind=!={},
  $IndList[#[[3]]]=Sequence@@@{ind,ind};IndexType[#[[3]]]=Sequence@@@{ind,ind}, 
  $IndList[#[[3]]]={Index[DUMMY],Index[DUMMY]};IndexType[#[[3]]]={Index[DUMMY],Index[DUMMY]}];
)&/@list
 ];


(* ::Subsubsection::Closed:: *)
(*Checking if a matrix is numerical*)


CheckNumerical[matr_]:= If[matr===MR$Null || Not[And@@(NumericQ/@Flatten[matr])], Message[MassDiag::MissingNumericalValue]; Abort[]];


(* ::Subsection::Closed:: *)
(*Patternize and removing patterns*)


(* ::Text:: *)
(*This function replaces an expression with indices by the associated expressions with a pattern.*)


Patternize[basis_]:= basis/.Index[type_,ia_?(Not[NumericQ[#]]&)]:>Index[type,MyPattern[ia,Blank[]]]/.MyPattern->Pattern;


(* ::Text:: *)
(*This function replaces the generic patterns _ by unique names given as arguments for a given field*)


TrackBlank[field_,inds_]:=Block[{fie = field/.Blank->myBlank, idd, ie=inds, ili=$IndList[Head[field]], ct=0, cnt, cnttot,maprules},
  (* Possible representation mapping *)
  maprules = myrule[Sequence@@Reverse[#]]&/@FR$ReprMap/.myrule->Rule;
  ili=ili/.maprules;

  (* If no indices, nothing to do *)
  If[Not[MatchQ[field,_[__]]], Return[field]];

  (* Checking the number of indices and killing spin and lorentz if necessary *)
  If[Length[List@@fie]=!=Length[ili],Message[MassDiag::NumberofBlank];Abort[]];
  cnttot=Length[ili];

  (* Replacing the blanks *)
  Return[
    If[#===myBlank[],
    (* If we have a blank index *)
      cnt=0; ct++; 
      (* This checks if the new index  has already been indentified *)
      While[IDType[First[ie]]=!=MR$Null && ili[[ct]]=!=IDType[First[ie]]&& cnt <cnttot,cnt++; ie=Append[Rest[ie],First[ie]]]; 
      If[cnt===cnttot&&ili[[ct]]=!=IDType[First[ie]],Message[MassDiag::MassBasisIndexInconsistency];Abort[]];
      (* This performs the replacement and update the IDType rule *)
      idd=First[ie]; IDType[idd]=(ili[[ct]]/.maprules); ie=Rest[ie];idd,
    (* Normal index -> nothing to do *)
      ct++;#
    ] &/@fie
  ];
];


(* ::Text:: *)
(*This function creates the number of required indices and apply the TrackBlank function to the indices and initialize them to a Null type*)


PutIndex[field_]:=Block[{ids,cids},
  ids = If[Head[field]=!=Symbol,$IndList[Head[field]],$IndList[field]];
  If[ids==={},Return[field]]; 
  cids = If[MatchQ[field,_[__]],field/.fi_?(FieldQ[#]===True&)[idx__]->{idx},{}];
  While[Length[ids]=!=Length[cids], cids=Prepend[cids,myBlank[]]];
  If[MatchQ[field,_[__]],Return[Head[field][Sequence@@cids]/.myBlank->Blank],Return[field[Sequence@@cids]/.myBlank->Blank]];
];


RemovePatterns[mygbasis_,mymbasis_]:=Block[{gbasis,mbasis,nblank, newindx,newgbasis,newmbasis},
  (* Adding blanks for spin and Lorentz *)
  gbasis=PutIndex/@mygbasis;
  mbasis=PutIndex/@mymbasis;

  (* Counting the number of blank indices and cerating the associated number of symbols *)
  nblank = Tally[Count[{#/.{fi_?(FieldQ[#]===True&)[ids__]->ids}},myBlank[]]&/@(gbasis/.Blank->myBlank)][[1,1]];
  newindx=Table[Unique[IND],{nblank}];

  (* Initializing the IDType rules *)
  (IDType[#]=MR$Null)&/@newindx;

  (* Replacing the blank indices by the fresh indices *)
  newgbasis=TrackBlank[#,newindx]&/@gbasis;
  newmbasis=TrackBlank[#,newindx]&/@mbasis;

  (* output *)
  Return[{newgbasis,newmbasis}];
];


(* ::Subsection::Closed:: *)
(*Update the rotation rules to simplify products of mixing matrices*)


CFrV[x_]:=Conjugate[FrV[x]];


UpdateRotationRules[mat_,NN_]:=Block[{tmprul},
  tmprul = Flatten[{
    Table[MyRule[FrV[mat[1,jj]] CFrV[mat[1,ll]],IndexDelta[jj,ll] - Plus@@(Table[FrV[mat[i,jj]]CFrV[mat[i,ll]],{i,2,NN}])],{jj,1,NN},{ll,1,NN}],
    Table[If[ll=!=1&&jj=!=1,
      MyRule[FrV[mat[jj,1]] CFrV[mat[ll,1]],IndexDelta[jj,ll] - Plus@@(Table[FrV[mat[jj,i]]CFrV[mat[ll,i]],{i,2,NN}])],{}],{jj,1,NN},{ll,1,NN}],
    Table[MyRule[Power[FrV[mat[1,jj]],n_] CFrV[mat[1,ll]],
       Power[FrV[mat[1,jj]],n-1]*IndexDelta[jj,ll]-Plus@@(Table[Power[FrV[mat[1,jj]],n-1]*FrV[mat[i,jj]]CFrV[mat[i,ll]],{i,2,NN}])],{jj,1,NN},{ll,1,NN}],
    Table[MyRule[FrV[mat[1,jj]] Power[CFrV[mat[1,ll]],n_],
       Power[CFrV[mat[1,ll]],n-1]*IndexDelta[jj,ll]-Plus@@(Table[Power[CFrV[mat[1,ll]],n-1]*FrV[mat[i,jj]]CFrV[mat[i,ll]],{i,2,NN}])],{jj,1,NN},{ll,1,NN}],
    Table[If[ll=!=1&&jj=!=1,MyRule[Power[FrV[mat[jj,1]],n_] CFrV[mat[ll,1]],
       Power[FrV[mat[jj,1]],n-1]*IndexDelta[jj,ll]-Plus@@(Table[Power[FrV[mat[jj,1]],n-1]*FrV[mat[jj,i]]CFrV[mat[ll,i]],{i,2,NN}])],{}],{jj,1,NN},{ll,1,NN}],
    Table[If[ll=!=1&&jj=!=1,MyRule[FrV[mat[jj,1]] Power[CFrV[mat[ll,1]],n_],
       Power[CFrV[mat[ll,1]],n-1]*IndexDelta[jj,ll]-Plus@@(Table[Power[CFrV[mat[ll,1]],n-1]FrV[mat[jj,i]]CFrV[mat[ll,i]],{i,2,NN}])],{}],{jj,1,NN},{ll,1,NN}]}];
  FR$UnitarySimplifications=Union[FR$UnitarySimplifications,tmprul]/.MyRule->Rule;
  Return[];
];


(* ::Subsection::Closed:: *)
(*Rotations for vector and Weyl fields*)


CreateHCRule[rule_]:=Block[{newrule},
  newrule=rule/.{Rule->MyRule,Module->MyMod,RuleDelayed->MyRuleDelayed};
  newrule = newrule//.{ MyRule[a_,b_]:>Rule[anti[a],anti[b]], MyRuleDelayed[a_,MyMod[{idx__},b_]]:>RuleDelayed[anti[a],MyMod[{idx},anti[b]]]};
  newrule = newrule/.{ProjP[aa_,bb_]->ProjM[bb,aa],ProjM[aa_,bb_]->ProjP[bb,aa]};
  newrule=newrule/.MyMod->Module;
  newrule=Union[rule,newrule];
(*Mod. 24.01.2013 AA. A rule for charge conjugation is created for every particle -> Useful in the LRSM case*)
  newrule=CreateCCRule[newrule];
  Return[newrule];
];


(* ::Text:: *)
(*Special treatment for Charge Conjugation*)


CreateCCRule[rule_]:=Block[{newrule,MyCC,MyMod,MyRuleDelayed,MyRule},
  newrule=rule/.{Rule->MyRule,Module->MyMod,RuleDelayed->MyRuleDelayed};
  newrule=newrule//.CC[f_][ind__]:>MyCC[f[ind]]/.CC[f_]:>MyCC[f];
  newrule=newrule/.{MyRule[f1_,f2_]:>Rule[MyCC[f1],MyCC[f2]],MyRuleDelayed[f1_,f2_]:>RuleDelayed[MyCC[f1],MyCC[f2]]}//.MyCC[MyMod[a_,b_]]:>MyMod[a,MyCC[b]];
(*Distribute the MyCC function*)
  newrule=newrule//.{MyCC[a_*b_]:>MyCC[a]*MyCC[b],MyCC[a_ + b_]:>MyCC[a]+MyCC[b],MyCC[MyCC[a_]]:>a};
(*Charge conjugation on a scalar/vector field and on a parameter is just the hermitian conjugate *)
  newrule=newrule//.MyCC[a_?(ScalarFieldQ[#]||VectorFieldQ[#]||FieldQ[#]=!=True&)]:>HC[a]//.MyCC->CC;
(*The hermitian conjugate of a projector is the projector....*)
  newrule=newrule//.HC[a_?(#===ProjP||#===ProjM&)[ind__]]:>a[ind];
(*....but ProjP becomes ProjM and ProjM becomes ProjP*)
  newrule=newrule/.{ProjP->ProjM,ProjM->ProjP};
(*come back to the module and print*)
  newrule=newrule/.MyMod->Module;
  Return[Union[rule,newrule]];
];


RotateVector[mbasis_,gbasis_,Val_]:=Block[{rule,MyModule},
  (* From mass to gauge basis *)
  rule=Inner[MyRule,Patternize[mbasis],PrePutIndices[Val.gbasis],List];
  rule=rule/.MyRule[a_,b_]:>MyRule[a,ReplaceAll[b,fiel_?(FieldQ[#]===True && $IndList[#]==={}&):>fiel-(fiel/.FR$vevRules/.fiel->0)]]/.
    MyRule[a_,b_]:>MyRule[a,ReplaceAll[b,fiel_?(FieldQ[#]===True&)[argus__]:>fiel[argus]- (fiel[argus]/.FR$vevRules/.fiel[argus]->0)]];
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@mbasis)], rule=CreateHCRule[rule]];
  FR$ToGaugeBasis=DeleteDuplicates[Join[FR$ToGaugeBasis,rule]];

  (* From gauge to mass basis *)
  rule=Inner[MyRule,Patternize[gbasis],PrePutIndices[ConjugateTranspose[Val].mbasis],List];
  rule=rule/.{MyRule[a_?(Head[#]===Symbol&),b_]:>MyRule[a,(a/.FR$vevRules/.a->0)+b],MyRule[a_[indx__],b_]:>MyRule[a[indx],(a[indx]/.FR$vevRules/.a[indx]->0)+b]};
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@gbasis)], rule=CreateHCRule[rule]];
  MR$Definitions=DeleteDuplicates[Join[MR$Definitions,rule]];

  (* exit *)
  Return[];
];


(* ::Subsection::Closed:: *)
(*Rotation of scalar fields*)


RotateScalar[mbasis_,gbasis_,Val_]:=Block[{rule,MyModule},
(* From mass to gauge basis *)
  rule=Inner[MyRule,Patternize[mbasis],PrePutIndices[Val.gbasis],List];
  rule=rule/.MyRule[a_,b_]:>MyRule[a,ReplaceAll[b,fiel_?(FieldQ[#]===True && $IndList[#]==={}&):>(fiel+anti[fiel]- 2 (fiel/.FR$vevRules/.fiel->0))/Sqrt[2]]]/.
       MyRule[a_,b_]:>MyRule[a,ReplaceAll[b,fiel_?(FieldQ[#]===True&)[argus__]:>(fiel[argus]+anti[fiel][argus]-2(fiel[argus]/.FR$vevRules/.fiel[argus]->0))/Sqrt[2]]];
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@mbasis)], rule=CreateHCRule[rule]];
  FR$ToGaugeBasis=DeleteDuplicates[Join[FR$ToGaugeBasis,rule]];

  (* From gauge to mass basis *)
  rule=Inner[MyRule,Patternize[gbasis],PrePutIndices[ConjugateTranspose[Val].mbasis],List];
  rule=rule/.{MyRule[a_?(Head[#]===Symbol&),b_]:>MyRule[a[Scalar],b/Sqrt[2]],MyRule[a_[indx__],b_]:>MyRule[a[indx,Scalar],b/Sqrt[2]]};
  rule=Join[rule/.MyRule[a_[indx___,Scalar],_]:>MyRule[a[indx],(a[indx]/.a[]->a/.FR$vevRules/.{a[indx]->0,a->0})+a[indx,Scalar]+I a[indx,Pseudoscalar]],rule];
  rule=Join[rule/.MyRule[a_[indx___,Scalar],_]:>MyRule[HC[a[indx]],(a[indx]/.a[]->a/.FR$vevRules/.{a[indx]->0,a->0})+a[indx,Scalar]-I a[indx,Pseudoscalar]],rule];
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module/.fi_?(FieldQ[#]===True&)[]->fi;  
  MR$Definitions=DeleteDuplicates[Join[MR$Definitions,rule]];

  (* exit *)
  Return[];
];


(* ::Subsection::Closed:: *)
(*Rotation of pseudoscalar fields*)


RotatePseudoscalar[mbasis_,gbasis_,Val_]:=Block[{rule, MyModule},
(* From mass to gauge basis *)
  rule=Inner[MyRule,Patternize[mbasis],PrePutIndices[Val.gbasis],List];
  rule=rule/.MyRule[a_,b_]:>MyRule[a,ReplaceAll[b,fiel_?(FieldQ[#]===True && $IndList[#]==={}&):>(fiel-anti[fiel])/(Sqrt[2] I)]]/.
       MyRule[a_,b_]:>MyRule[a,ReplaceAll[b,fiel_?(FieldQ[#]===True&)[argus__]:>(fiel[argus]-anti[fiel][argus])/(Sqrt[2] I)]];
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@mbasis)], rule=CreateHCRule[rule]];
  FR$ToGaugeBasis=DeleteDuplicates[Join[FR$ToGaugeBasis,rule]];

  (* From gauge to mass basis *)
  rule=Inner[MyRule,Patternize[gbasis],PrePutIndices[ConjugateTranspose[Val].mbasis],List];
  rule=rule/.{MyRule[a_?(Head[#]===Symbol&),b_]:>MyRule[a[Pseudoscalar],b/Sqrt[2]],MyRule[a_[indx__],b_]:>MyRule[a[indx,Pseudoscalar],b/Sqrt[2]]};
  rule=Join[rule/.MyRule[a_[indx___,Pseudoscalar],_]:>MyRule[a[indx],(a[indx]/.a[]->a/.FR$vevRules/.{a[indx]->0,a->0})+a[indx,Scalar]+I a[indx,Pseudoscalar]],rule];
  rule=Join[rule/.MyRule[a_[indx___,Pseudoscalar],_]:>MyRule[HC[a[indx]],(a[indx]/.a[]->a/.FR$vevRules/.{a[indx]->0,a->0})+a[indx,Scalar]-I a[indx,Pseudoscalar]],rule];
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module/.fi_?(FieldQ[#]===True&)[]->fi;
  MR$Definitions=DeleteDuplicates[Join[MR$Definitions,rule]];

  (* exit *)
  Return[];
];


(* ::Subsection:: *)
(*Rotation of four-component fermionic fields*)


Listize[exp_]:=  DeleteCases[If[Head[exp]===Plus, List@@exp,{exp}],0|0.];


Modulize[MyRule[a_,b_],MyModule_]:=Block[{idx},
  idx =Flatten[(Cases[Tally[ ToIndexList[#]],{_,2}]/.{aa_,2}->aa)&/@(Listize[b]/.ff_[ab___,left|right|Scalar|Pseudoscalar]:>ff[ab])/.Index[_,aa_]->aa];  
  Return[MyRule[a,MyModule[idx,b]]];
];


RotateFermion[mbasis_,gbasis_,Val_,chir_]:=Block[{rule,rule2,MyModule},
  (* From mass to gauge basis *)
  rule2=Inner[MyRule,Patternize[mbasis],
   ((PrePutIndices[left[#]]&/@mbasis)/.a_?(FieldQ[#]===True&)[indx__]->a[indx,left])+((PrePutIndices[right[#]]&/@mbasis)/.a_?(FieldQ[#]===True&)[indx__]->a[indx,right]),List];
  rule2=(Modulize[#,MyModule]&/@rule2)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule2=rule2/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@mbasis)], rule2=CreateHCRule[rule2]];  
  rule=Inner[MyRule,Patternize[mbasis],PrePutIndices[Val.gbasis],List];
  rule=rule/.MyRule[a_[indx__],b_]:>MyRule[a[indx,chir],b];
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@mbasis)], rule=CreateHCRule[rule]];  
  FR$ToGaugeBasis=DeleteDuplicates[Join[FR$ToGaugeBasis,rule2]];
  FR$ToGaugeBasis=DeleteDuplicates[Join[FR$ToGaugeBasis,rule]];

  (* From gauge to mass basis *)
  rule=Inner[MyRule,Patternize[gbasis],PrePutIndices[ConjugateTranspose[Val].mbasis],List];
  rule=rule/.MyRule[a_,b_]:>MyRule[a,ReplaceAll[b,fiel_?(FieldQ[#]===True&)[argx__]:>PrePutIndices[chir[fiel[argx]]]]];  
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@gbasis)], rule=CreateHCRule[rule]];
  MR$Definitions=DeleteDuplicates[Join[MR$Definitions,rule]];

  (* exit *)
  Return[];
];


RotateFermion4[mbasis_,gbasis_,Val_,chir_]:=Block[{rule,rule2,MyModule},
  (* From mass to gauge basis *)
  rule2=Inner[MyRule,Patternize[mbasis],
   ((PrePutIndices[left[#]]&/@mbasis)/.a_?(FieldQ[#]===True&)[indx__]->a[indx,left])+((PrePutIndices[right[#]]&/@mbasis)/.a_?(FieldQ[#]===True&)[indx__]->a[indx,right]),List];
  rule2=(Modulize[#,MyModule]&/@rule2)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule2=rule2/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@mbasis)], rule2=CreateHCRule[rule2]];  
  rule=Inner[MyRule,Patternize[mbasis],PrePutIndices[Val.gbasis],List];
  rule=rule/.MyRule[a_[indx__],b_]:>MyRule[a[indx,chir],PrePutIndices[chir[b]]/.chir[0]->0];
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@mbasis)], rule=CreateHCRule[rule]];  
  If[chir=!=right,FR$ToGaugeBasis=DeleteDuplicates[Join[FR$ToGaugeBasis,rule2]]];
  FR$ToGaugeBasis=DeleteDuplicates[Join[FR$ToGaugeBasis,rule]];

  (* From gauge to mass basis *)
  rule2=Inner[MyRule,Patternize[gbasis],
   ((PrePutIndices[left[#]]&/@gbasis)/.a_?(FieldQ[#]===True&)[indx__]->a[indx,left])+((PrePutIndices[right[#]]&/@gbasis)/.a_?(FieldQ[#]===True&)[indx__]->a[indx,right]),List];
  rule2=(Modulize[#,MyModule]&/@rule2)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule2=rule2/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@gbasis)], rule2=CreateHCRule[rule2]];  
  rule=Inner[MyRule,Patternize[gbasis],PrePutIndices[ConjugateTranspose[Val].mbasis],List];
  rule=rule/.MyRule[a_[indx__],b_]:>MyRule[a[indx,chir],PrePutIndices[chir[b]]/.chir[0]->0];  
  rule=(Modulize[#,MyModule]&/@rule)/.MyModule[{},expr_]->expr/.MyRule[a_,MyModule[{idx__},res_]]:>RuleDelayed[a,MyModule[{idx},res]];
  rule=rule/.MyRule->Rule/.MyModule->Module;
  If[Not[And@@(SelfConjugateQ[#]&/@gbasis)], rule=CreateHCRule[rule]];
  If[chir=!=right,MR$Definitions=DeleteDuplicates[Join[MR$Definitions,rule2]]];
  MR$Definitions=DeleteDuplicates[Join[MR$Definitions,rule]];

  (* exit *)
  Return[];
];


(* ::Subsection::Closed:: *)
(*CreateMixingDefinitions*)


(* ::Text:: *)
(*This function updates all the list with the replacement rules for a given mixing.*)
(*  2. treat the case of a new mixing matrix parameter not given by the user ->  parameter rules to update + special ToValue[ ] function returning the numerical value of the matrix if provided*)
(*  3. myval contains the matrix element (parameter names or values)*)
(*  4. The rotation rules are created*)


CreateMixingDefinitions[gbasis_,mbasis_,mat_,value_,bname_,chir_,inve_]:=Block[{newgbasis,newmbasis,description,myval=value,matr,inv},
  (*Putting indices *)
  {newgbasis,newmbasis} = RemovePatterns[gbasis, mbasis];
  newgbasis=newgbasis/.fi_?(FieldQ[#]===True&)[inds__]:>fi[Sequence@@Inner[Index,$IndList[fi],{inds},List]]/.Index[Index[type_],val_]->Index[type,val];
  newmbasis=newmbasis/.fi_?(FieldQ[#]===True&)[inds__]:>fi[Sequence@@Inner[Index,$IndList[fi],{inds},List]]/.Index[Index[type_],val_]->Index[type,val];

  (* Removing blanks *)
  matr=mat/.Blank->MR$Null/.MR$Null[]->MR$Null;
  inv=inve/.Blank->MR$Null/.MR$Null[]->False;

   (* Creating rotation + mapping rules *)
  If[matr=!=MR$Null,    
    If[value=!=MR$Null,
       Table[NumericalValue[Symbol[ToString[matr]<>ToString[i]<>"x"<>ToString[j]]]=NumericalValue[matr[i,j]],{i,Length[newgbasis]},{j,Length[newgbasis]}]];
    UpdateRotationRules[matr,Length[newgbasis]];
    myval=Array[matr,{Length[newmbasis],Length[newgbasis]}]/.matr[i_,j_]:>FrV[matr[i,j]];
  ];  
  If[inv,myval=ConjugateTranspose[myval]];

  (* Create rotation rules *)
  If[chir===Vector, RotateVector[newmbasis,newgbasis,myval]; Return[]];
  If[chir===Scalar, RotateScalar[newmbasis,newgbasis,myval]; Return[]];
  If[chir===Pseudoscalar, RotatePseudoscalar[newmbasis,newgbasis,myval]; Return[]];
  If[chir===Left, RotateFermion[newmbasis,newgbasis,myval,left]; Return[]];
  If[chir===Right, RotateFermion[newmbasis,newgbasis,myval,right]; Return[]];
  If[chir===Left4, RotateFermion4[newmbasis,newgbasis,myval,left]; Return[]];
  If[chir===Right4, RotateFermion4[newmbasis,newgbasis,myval,right]; Return[]];
  If[chir===Weyl, RotateVector[newmbasis,newgbasis,myval]; Return[]];

  (* Mixing declaration: error *)
  Message[MassDiag::MixDeclUnknown]; Abort[]
];


(* ::Subsection::Closed:: *)
(*Initialization of the mixings *)


(* ::Text:: *)
(*This is the main function, called at the time of the load of the FeynRules model*)


InitializeMixings[] :=Block[{},  
  FR$vevRules = { Rule[Indexize[#[[1]]],#[[2]]/Sqrt[2]], Rule[HC[Indexize[#[[1]]]],HC[#[[2]]]/Sqrt[2]]}&/@M$vevs;
  FR$vevRules=Flatten[FR$vevRules];
  LoadMixing[(#[[1]]/.Mix[id_]->id),#[[2]]]&/@M$MixingsDescription;
  (FR$MixingRules[#[[1]]]=#[[2]])&/@M$MixingsDescription;
(*Mod 01.27.2013 AA: Disabled for the moment because not necessary!*)
  (*PutMatrixIndex[#]&/@(M$MixingsDescription/.Equal[Mix[a_],__]:>a);*)
];


(* ::Subsection:: *)
(*Core function: LoadMixing*)


(* ::Text:: *)
(*Options of the LoadMixing function.*)


Options[LoadMixing]={
  MassBasis->{{}},
  GaugeBasis->{},
  MixingMatrix->MR$Null,
  BlockName->MR$Null,
  Chirality->None,
  Value->MR$Null,
  Inverse->False
};


(* ::Text:: *)
(*Loading multiple mixing relations -> one mixing at a time*)


LoadMixing[id_,mixrules_List]:=LoadMixing[id,Sequence@@mixrules];


(* ::Text:: *)
(*The LoadMixing function load one single mixing relation at a time.*)


LoadMixing[id_,OptionsPattern[]]:=Block[{tmpbasis,
  (* Initialization *)
    gbasis=OptionValue[GaugeBasis],
    mbasis=OptionValue[MassBasis],
    bname=OptionValue[BlockName],
    MixMat=OptionValue[MixingMatrix],
    MatVal=OptionValue[Value],
    inv = OptionValue[Inverse]
  },

  (* Check id *)
   If[Not[StringQ[id]], Message[MassDiag::IdMix]; Abort[]];

  (* Formatting the bases and checking if they are compliant *)  
  If[mbasis==={},mbasis={{}},If[Not[ListQ[mbasis[[1]]]],mbasis={mbasis}]];
  If[Not[MatrixQ[gbasis]],gbasis={gbasis}];
  If[Not[ListQ[MixMat]], MixMat={MixMat}]; 
  If[Not[ListQ[bname]], bname={bname}];
  TestingMixingDeclaration[mbasis, gbasis, MatVal, MixMat, (*indices,*) bname];

  (* When one gauge-eigenstate basis gives rise to two mass-eigenstate bases: scalar fields -> scalar vs. pseudoscalar mixing. *)
  If[Length[mbasis]==2, 
    If[Dimensions[gbasis][[1]]===1,gbasis=Flatten[gbasis]];
    If[And@@(ScalarFieldQ[#]===True&/@gbasis),
      Type[id]="SPS";
      If[bname==={MR$Null},bname={MR$Null,MR$Null}];
      If[MixMat==={MR$Null},MixMat={MR$Null,MR$Null}];
      If[MatVal===MR$Null,MatVal={MR$Null,MR$Null}];
      If[inv===False,inv={False,False}];
      CreateMixingDefinitions[gbasis,mbasis[[1]],MixMat[[1]], MatVal[[1]],bname[[1]],Scalar,inv[[1]]];
      CreateMixingDefinitions[gbasis,mbasis[[2]],MixMat[[2]], MatVal[[2]],bname[[2]],Pseudoscalar,inv[[2]]]; 
      Return[];
    ];
    If[And@@((FermionQ[#]===True && WeylFieldQ[#] === True) &/@Flatten[gbasis]),
      Type[id]="CWeyl";
      If[bname==={MR$Null},bname={MR$Null,MR$Null}];
      If[MixMat==={MR$Null},MixMat={MR$Null,MR$Null}];
      If[MatVal===MR$Null,MatVal={MR$Null,MR$Null}];
      If[inv===False,inv={False,False}];
      CreateMixingDefinitions[gbasis[[1]],mbasis[[1]],MixMat[[1]], MatVal[[1]],bname[[1]],Weyl,inv[[1]]];
      CreateMixingDefinitions[gbasis[[2]],mbasis[[2]],MixMat[[2]], MatVal[[2]],bname[[2]],Weyl,inv[[2]]];
      Return[];
    ];
    Message[MassDiag::TwoMassBases]; Abort[]
  ];

  (* One single gauge basis + one single mass basis: vectors, charged scalars + fermions *)
  If[Length[mbasis]===1 && Dimensions[gbasis][[1]]===1,
    gbasis=Flatten[gbasis];
    If[And@@(VectorFieldQ[#]===True&/@gbasis), CreateMixingDefinitions[gbasis,mbasis[[1]],MixMat[[1]],MatVal, bname, Vector, inv]; Return[]];
    If[And@@(ScalarFieldQ[#]===True&/@gbasis), CreateMixingDefinitions[gbasis,mbasis[[1]],MixMat[[1]],MatVal, bname, Vector, inv]; Return[]];
    If[And@@(FermionQ[#]===True && WeylFieldQ[#] =!= True &/@gbasis),
      Type[id]="F4";
      If[bname==={MR$Null},bname={MR$Null,MR$Null}];
      If[MixMat==={MR$Null},MixMat={MR$Null,MR$Null}];
      If[MatVal===MR$Null,MatVal={MR$Null,MR$Null}];
      If[Dimensions[MatVal][[1]]=!=2, 
        If[And@@(SelfConjugateQ[#]===True &/@gbasis), Message[MassDiag::MajoranaBases]; Abort[], Message[MassDiag::FermionValBases]; Abort[]]];
      If[inv===False,inv={False,False}];
      CreateMixingDefinitions[gbasis,mbasis[[1]],MixMat[[1]],MatVal[[1]], bname[[1]], Left4, inv[[1]]];
      CreateMixingDefinitions[gbasis,mbasis[[1]],MixMat[[2]],MatVal[[2]], bname[[2]], Right4, inv[[2]]];
      Return[];
    ];
    If[And@@(FermionQ[#]===True && WeylFieldQ[#] === True &/@gbasis),
      CreateMixingDefinitions[gbasis,mbasis[[1]],MixMat[[1]],MatVal, bname[[1]],Weyl, inv];
      Return[];
    ];
    Message[MassDiag::OneMassOneGaugeBases]; Abort[]
  ];

  (* Fermions with two bases (one for left and one for right) *)
  If[Length[mbasis]===1 && Dimensions[gbasis][[1]]===2,
    If[And@@(FermionQ[#]===True && WeylFieldQ[#] =!= True &/@Flatten[gbasis]),
      Type[id]="FLR";
      If[bname==={MR$Null},bname={MR$Null,MR$Null}];
      If[MixMat==={MR$Null},MixMat={MR$Null,MR$Null}];
      If[MatVal===MR$Null,MatVal={MR$Null,MR$Null}];
      If[inv===False,inv={False,False}];
      CreateMixingDefinitions[gbasis[[1]],mbasis[[1]],MixMat[[1]],MatVal[[1]], bname[[1]], Left, inv[[1]]];
      CreateMixingDefinitions[gbasis[[2]],mbasis[[1]],MixMat[[2]],MatVal[[2]], bname[[2]], Right, inv[[2]]];
      Return[]
    ];
    Message[MassDiag::OneMassTwoGaugeBases]; Abort[]
  ];
  Message[MassDiag::Bases]; Abort[]
];


(* ::Section:: *)
(*Flavor expansion*)


(* ::Subsection::Closed:: *)
(*Flavor Expansion over the unfolded indices*)


(* ::Text:: *)
(*This function do a numerical flavor expansion of the indices of the type "unfold". It takes a single lagrangian term as an argument.*)


ExpandUnfoldIndices[exp_]:=Block[{exprules,MyTable,expandedterm},
  (* Get the list of the indices to be expanded, with their range *) 
   exprules=Cases[Tally[ToIndexList[exp]],{_,2}][[All,1]]/.Index->List;
   exprules=Cases[exprules,_?(MemberQ[MR$FlavorList,#[[1]]]&)];
   exprules={#[[2]],1,IndexDim[Index[#[[1]]]]}&/@exprules;

  (* Do the expansion *)
  expandedterm=If[exprules=!={}, Plus@@(Flatten[MyTable[exp,Sequence@@exprules]/.MyTable->Table]),exp];  

  (* Replacement of the generators *)
  Return[expandedterm/.FR$GenRules/.DeleteCases[MR$Definitions,Rule[_?(FieldQ[#]===True&),_]|RuleDelayed[_?(FieldQ[#]===True&),_]]];
];


(* ::Subsection::Closed:: *)
(*Field counting*)


(* ::Text:: *)
(*This functions returns the power in field of a Lagrangian term*)


Dim[Times[a_,b__]]:=Dim[a]+Dim[Times[b]];
Dim[Dot[a_,b_]]:=Dim[a]+Dim[b];
Dim[Power[a_,b_?NumericQ]]:=Plus@@Table[Dim[a],{b}];
Dim[fi_?(FieldQ[#]===True&)]:=1;
Dim[fi_?(FieldQ[#]===True&)[__]]:=1;
Dim[fi_?(Not[FieldQ[#]===True]&)]:=0;


(* ::Subsection::Closed:: *)
(*Core routine*)


(* ::Text:: *)
(*This functions*)
(*   1. Reject the derivative terms*)
(*   2. Do a flavor expansion over the unfolded indices*)
(*   3. shift all the scalar fields by their vev*)
(*   4. reject all the non-bilinear terms + derivative terms*)


SpecialExpandIndices[lag_]:=Block[{tmplag=Expand[lag]/.del[__]->0,vevrules},
  (* Initialization *)
  FR$GenRules=Flatten[DeleteCases[Definitions/.MR$GaugeGroupRules[#]&/@MR$GaugeGroupList,Definitions],1];

  (* Stores the lagrangian as a list + suppression of the derivatives *)
  tmplag=If[Head[tmplag]===Plus, List@@tmplag,{tmplag}];

  (* flavor expansion over the unfold indices + adding the index types*)
  tmplag=ExpandUnfoldIndices/@tmplag;
  tmplag=(#/.fi_?(FieldQ[#]===True&)[inds__]:>Indexize[fi[inds]])&/@tmplag;
  tmplag=(#/.fi_?(MemberQ[M$IndParam,#]&)[inds__]:>Indexize[fi[inds]])&/@tmplag;

  (* Perform, for safety reasons, rotations to the gauge basis (the vevs are well treated here) + achieve the listification*)
  tmplag=Expand/@(tmplag/.FR$ToGaugeBasis);
  tmplag=Flatten[tmplag/.Plus->List]; 

  (* Shift the vevs for the scalar fields *)
  vevrules = FR$vevRules/.Rule[a_,b_]:>Rule[a,a+b];
  tmplag=Flatten[Expand[#/.vevrules]&/@tmplag/.Plus->List]; 

  (* Delete the non-bilinear terms *)  
  tmplag=DeleteCases[tmplag,_?(Dim[#]=!=2&)];

  (* Formatting the indices and rotations to the gauge basis*)  
  Return[Plus@@tmplag];
];


(* ::Subsection:: *)
(*ExpandIndices2*)


IDXFuncQ[xx_] := (TensQ[xx] === True_) || (FieldQ[xx] === True) || (xx === del) || (xx ===ME) || (xx === IndexDelta) || (xx == FV) || (xx === Eps);


ExpandIndices2[lag_]:=Block[{tmplag=Expand[lag]/.Dot->FR$Dot/.FR$Dot->Dot, mufmuf, blkflav,grules, g0rules,gnzrules,unirules},
  (* Initialization *)
  blkflav = Index[#,_]& /@ MR$FlavorList;  
  FR$GenRules=Flatten[DeleteCases[Definitions/.MR$GaugeGroupRules[#]&/@MR$GaugeGroupList,Definitions],1];  
  grules=DeleteCases[MR$Definitions,Rule[_?(FieldQ[#]===True&),_]|RuleDelayed[_?(FieldQ[#]===True&),_]];
  g0rules = Cases[MR$Definitions/.{Rule->MyRule,RuleDelayed->MyRuleDelayed,ReleaseHold->rh},(MyRule|MyRuleDelayed)[_,0|rh[0]]];
  g0rules = g0rules/.{MyRule->Rule,MyRuleDelayed->RuleDelayed,rh->ReleaseHold};
  gnzrules = Complement[MR$Definitions,g0rules];  
  g0rules = g0rules/.ReleaseHold[0]:>0/.Rule|RuleDelayed->MyRule/.MyRule[a_,b_]:>MyRule[FrV[a],b]/.MyRule->Rule/.FrV[a_]:>a;
  gnzrules=gnzrules/.g0rules;
  unirules = DeleteCases[FR$UnitarySimplifications/.Rule->MyRule/.MyRule[a_,b_]:>MyRule[a,b/. g0rules]/.Power[0,_]->0/.MyRule->Rule,Rule[_,0]];
  tmplag=Listize[Expand[tmplag]];

  (* ExpandIndices *)
  FR$Exp=0; If[FR$Debug,Print["ExpandIndices: ", Dynamic[FR$Exp]," / ",Length[tmplag]]];
  mufmuf=Timing[tmplag=Block[{tpm,exprules},FR$Exp++; 
    tpm=#/.Dot->RestoreFermionIndices/.RestoreFermionIndices->Dot/.HCPI->HC/.TP[t_][as__, i_, j_] :> t[as,j,i];
    tpm=tpm//.{del1[Dot[psi1_,psi2_],mu_] :> Dot[del[psi1,mu],psi2], del2[Dot[psi1_,psi2_],mu_] :> Dot[psi1, del[psi2, mu]]};
    tpm=tpm//.Conjugate[tt_?(TensQ)[ind__]] -> Conjugate[tt][ind];
    tpm = tpm //. {HC -> HCPutIndices} //. {HCPutIndices[t_?(TensQ)][ind___] -> HCPutIndices[t[ind]]}; 
    tpm = PrePutIndices[tpm];
    tpm = tpm //. Dispatch[{Except[Index, f_][xx___, Index[name_, a_], yy___] Except[Index, g_?IDXFuncQ][zz___, a_, tt___] :> 
         f[xx, Index[name, a], yy] g[zz, Index[name, a], tt],
       Except[Index, h_][bb___, Except[Index, f_][xx___, Index[name_, a_], yy___], cc___] Except[Index, g_?IDXFuncQ][zz___, a_, tt___] :> 
         h[bb, f[xx, Index[name, a], yy], cc] g[zz, Index[name, a], tt],
       Except[Index, f_][xx___, Index[name_, a_], yy___] Except[Index, h_][bb___, Except[Index, g_?IDXFuncQ][zz___, a_, tt___], cc___] :>
         f[xx, Index[name, a], yy] h[bb, g[zz, Index[name, a], tt], cc],
       Except[Index, h_][bb___,Except[Index,f_][xx___,Index[name_,a_],yy___],cc___] Except[Index,j_][dd___,Except[Index,g_?IDXFuncQ][zz___,a_,tt___],ee___] :>
          h[bb, f[xx, Index[name, a], yy], cc] j[dd, g[zz, Index[name, a], tt], ee]}];
    tpm = tpm //. HCPutIndices -> HC;
    tpm = ReleaseASIndex[tpm];
    tpm = tpm /. DiagProd2 -> DiagProd;
    exprules=Cases[DeleteDuplicates[ToIndexList[tpm]],Alternatives@@blkflav];
    If[exprules=!={},
      exprules = {#[[2]],1,IndexDim[Index[#[[1]]]]}&/@exprules;
      tpm = Plus@@(Flatten[MyTable[tpm,Sequence@@exprules]/.MyTable->Table]);  
      tpm=tpm/.FR$GenRules/.grules;
    ];
    tpm]&/@tmplag];
  If[FR$Debug,Print["  => Done in ",mufmuf[[1]], " seconds."]];
  tmplag=Listize[Plus@@tmplag];

  (* Definitions *)
  FR$Def=0; If[FR$Debug,Print["MR$Definitions: ", Dynamic[FR$Def]," / ",Length[tmplag]]];
  mufmuf=Timing[tmplag=Block[{},FR$Def++; #//.g0rules//.gnzrules/.Dot->FR$Dot/.FR$Dot->Dot]&/@tmplag];
  If[FR$Debug,Print["  => Done in ",mufmuf[[1]], " seconds."]];

  FR$ExpDef=0; If[FR$Debug,Print["Expansion of the terms: ", Dynamic[FR$ExpDef]," / ",Length[tmplag]]];
  mufmuf=Timing[tmplag=Block[{},FR$ExpDef++; Expand[#]]&/@tmplag];
  If[FR$Debug,Print["  => Done in ",mufmuf[[1]], " seconds."]];
  tmplag=Listize[Plus@@tmplag];

  (* Unitarity Simplifications *)
  FR$Uni1=0; If[FR$Debug,Print["Unitarity Simplifications 1: ", Dynamic[FR$Uni1]," / ",Length[tmplag]]];
  mufmuf=Timing[tmplag = Block[{}, FR$Uni1++;Expand[#//.unirules/.Pow->Power]]&/@tmplag];
  If[FR$Debug,Print["  => Done in ",mufmuf[[1]], " seconds."]];
  tmplag=Listize[Plus@@tmplag];
  FR$Uni2=0; If[FR$Debug,Print["Unitarity Simplifications 2: ", Dynamic[FR$Uni2]," / ",Length[tmplag]]];
  mufmuf=Timing[tmplag = Block[{}, FR$Uni2++;Expand[#//.unirules/.Pow->Power]]&/@tmplag];
  If[FR$Debug,Print["  => Done in ",mufmuf[[1]], " seconds."]];
  FR$ExpUni2=0; If[FR$Debug,Print["Expansion of the terms: ", Dynamic[FR$ExpUni2]," / ",Length[tmplag]]];
  mufmuf=Timing[tmplag=Block[{},FR$ExpUni2++; Expand[#]]&/@tmplag];
  If[FR$Debug,Print["  => Done in ",mufmuf[[1]], " seconds."]];
  tmplag=Plus@@tmplag;
(*Mod 01.25.2013 AA a tensor in the form param[ind1,ind2] is transformed to paramind1xind2 to avoid problems with FeynmanRules. Thanks Benj!!!
*)

   tmplag=(ApplyDefinitions/@(tmplag/.Conjugate[a_][ind__]:>MyConj[a[ind]]))//.a_?(MemberQ[M$Parameters[[All,1]],#]&)[ind1___,Index[_,b_],ind2___]:>a[ind1,b,ind2];
   tmplag=tmplag/.a_?(MemberQ[FR$MassMatricesToCalculate,#]&)[ind1_,ind2_]:>ToExpression[ToString[a]<>ToString[ind1]<>"x"<>ToString[ind2]]/.a_?(MemberQ[FR$MassMatricesToCalculate,#]&)[ind1_,ind2_,ind3_]:>ToExpression[ToString[a]<>ToString[ind1]<>"x"<>ToString[ind2]<>"x"<>ToString[ind3]];
   tmplag=tmplag/.MyConj->Conjugate;
  (*End of the add-on*)
  tmplag=Expand/@tmplag;

  (* output *)
  Return[tmplag];
];


(* ::Section:: *)
(*Tree-level mass matrices*)


(* ::Subsection:: *)
(*Derivative with respect to a field*)


Derf[Plus[a_,b__],fi_]:=Plus@@(Derf[#,fi]&/@{a,b});
Derf[a_?(numQ[#]===True&) rest_,muf_]:=a Derf[rest,muf];
Derf[a_?(CnumQ[#]===True&) rest_,muf_]:=a Derf[rest,muf];
Derf[a_?(numQ[#]===True&) ,_]:=0;
Derf[a_?(CnumQ[#]===True&) ,_]:=0;
Derf[IndexDelta[__] ,_]:=0;

Derf[Times[a_,b__],fi_]:=Times[Derf[a,fi],b]+a*Derf[Times[b],fi];
Derf[Power[a_,n_],fi_]:=n*Power[a,n-1] *Derf[a,fi];
Derf[Dot[a_,b_],fi_]:=Derf[a,fi]*b + a*Derf[b,fi]; 

Derf[HC[a_][inds__],fi_]:=HC[Derf[a[inds],fi]];
Derf[HC[a_],fi_]:=HC[Derf[a,fi]];

Derf[fi_?(Head[#]===Symbol&),fi2_]:=Derf[fi[],fi2];
Derf[fi_,fi2_?(Head[#]===Symbol&)]:=Derf[fi,fi2[]];

Derf[fi1_?(FieldQ[#]===True&)[inds1___],fi2_?(FieldQ[#]===True&)[inds2___]]:=If[fi1=!=fi2,0,Inner[IndexDelta2,{inds2},{inds1},Times]];


Derf[0,_]=0;


(* ::Subsection:: *)
(*Core function*)


ComputeMassMatrix2[lagr_,b1_,b2_]:=Block[{mat,nsize = Length[b1],bases, tmplag},
  bases=DeleteDuplicates[Join[b1,b2]/.fld_?(FieldQ[#]===True&)[__]->fld];  
  tmplag = lagr/.{_?(FieldQ[#]===True && Not[MemberQ[bases,#]]&)[__]->0,_?(FieldQ[#]===True && $IndList[#]==={} && Not[MemberQ[bases,#]]&)->0};
  tmplag = tmplag/.aa_?(MemberQ[MR$ParameterList,#]&)[ids__]:>FrV[ReplaceAll[aa[ids],Index[_,ii_?(NumericQ)]:>ii]]/.FrV->Plus;
  tmplag = Expand[tmplag//.FR$UnitarySimplifications/.Pow->Power];
  tmplag = Expand[tmplag//.FR$UnitarySimplifications/.Pow->Power];
  mat = Table[Derf[Derf[tmplag,b1[[iii]]],b2[[jjj]]],{iii,nsize},{jjj,nsize}];
  mat = mat/.Index[type_,Index[type_,a_]]->Index[type,a]/.IndexDelta2->IndexDelta;
  mat = Expand[mat]//.{ME[__]->1,
     IndexDelta[b_?(!NumericQ[#]&),a_?(!NumericQ[#]&)] IndexDelta[a_?(!NumericQ[#]&),c_?(!NumericQ[#]&)]:> IndexDelta[b,c],
     IndexDelta[a_?(!NumericQ[#]&),b_?(!NumericQ[#]&)] IndexDelta[a_?(!NumericQ[#]&),c_?(!NumericQ[#]&)]:> IndexDelta[b,c]};
  mat = mat/.IndexDelta[a_?(!NumericQ[#]&),b_?(!NumericQ[#]&)]->1;
  mat=ApplyDefinitions/@(ApplyDefinitions/@mat);
  Return[mat];
];


(* ::Subsection:: *)
(*Wrapper:  TreeLevelMassMatrix*)


(* ::Text:: *)
(*Prerotation in the case of chain mixing*)


PreRotate[Field_]:=Block[{mbases,tmpfld={}, tmpdef={},bbas,bbas2},
  (* Computing all mass bases *)
  mbases=({MassBasis,GaugeBasis}/.FR$MixingRules[#])&/@M$MixingsDescription[[All,1]];

  (* Checking basis by basis *)
  If[Length[Dimensions[#[[1]]]]===1,
    (* only one mass basis *) 
    bbas=And@@((Derf[#,Field]===0 && Derf[#,anti[Field]]===0)&/@(PrepareBasis[#[[1]]]));
    If[Not[bbas], tmpfld=Append[tmpfld,PrepareBasis[#[[2]]]]],
    (* two mass bases *)
    bbas=And@@((Derf[#,Field]===0 && Derf[#,anti[Field]]===0)&/@(PrepareBasis[#[[1,1]]]));
    bbas2=And@@((Derf[#,Field]===0 && Derf[#,anti[Field]]===0)&/@(PrepareBasis[#[[1,2]]]));
    If[Not[bbas] || Not[bbas2], tmpfld=Append[tmpfld,PrepareBasis[#[[2]]]]]
  ]&/@mbases;

  (* Adding the hermitian conjugate fields *)
   tmpfld = Flatten[tmpfld];
   tmpfld=DeleteDuplicates[Join[tmpfld,anti/@tmpfld]];

  (* Extracting the rotations from MR$Definitions *) 
 tmpdef= Flatten[Block[{curr=#}, DeleteCases[If[(curr/.#)=!=curr,#,{}]&/@MR$Definitions,{}]] &/@tmpfld];

  (* Output *)
  Return[tmpdef];
];


PrepareBasis[bas_]:=RemovePatterns[bas,{}][[1]]/. fi_?(FieldQ[#]===True&)[inds__]:>fi[Sequence@@Inner[Index,$IndList[fi],{inds},List]]/.{
   Index[Index[type_],val_]->Index[type,val]};


(* ::Text:: *)
(*This extract the mass matrix from a Lagrangian*)


Options[TreeLevelMassMatrix] = {
 Basis1->{},
 Basis2->{}
}


TreeLevelMassMatrix[Mix[id_],lagr_,OptionsPattern[]]:=Block[{gbasis,gbasis2,tag=False,tmpmat1,tmpmat2,tmpmat3,basis,rotlagr,tprl},
  (* Initialization *)
  If[id==="MUF",
    gbasis = PrepareBasis[OptionValue[Basis1]];
    gbasis2 = PrepareBasis[OptionValue[Basis2]],
    FR$MassMatrices = DeleteDuplicates[Append[FR$MassMatrices,id]];
    basis = GaugeBasis/.FR$MixingRules[Mix[id]];
    If[Not[MatrixQ[basis]],basis={basis}];
    If[Dimensions[basis][[1]]===2, 
      gbasis=PrepareBasis[basis[[1]]]; gbasis2=PrepareBasis[basis[[2]]], 
      gbasis=PrepareBasis[basis[[1]]]; gbasis2=PrepareBasis[basis[[1]]]
    ];
  ];
  gbasis2=anti/@gbasis2;
  tprl = Cases[MR$Definitions/.{Rule->MyRule,RuleDelayed->MyRuleDelayed},(MyRule|MyRuleDelayed)[_[___,Scalar|Pseudoscalar],_]];
  tprl = tprl/.{MyRule->Rule,MyRuleDelayed->RuleDelayed};

  (* Prerotate the Lagrangian if necessary *) 
  tmpmat1 = Timing[Block[{prerot={}},
    prerot=Append[prerot,PreRotate[#]]&/@gbasis;
    prerot=DeleteDuplicates[Flatten[Append[prerot,PreRotate[#]]&/@gbasis2]];  
    prerot=prerot/.aa_?(MemberQ[MR$ParameterList,#]&)[ids__]:>FrV[ReplaceAll[aa[ids],Index[_,ii_?(NumericQ)]:>ii]]/.FrV->Plus;
    prerot = prerot//.tprl;
    rotlagr = Expand[lagr/.prerot/.Dot->FR$Dot/.FR$Dot->Dot];
  ]];

  (* MUF case *)
  If[id==="MUF",
    tmpmat1=ComputeMassMatrix2[rotlagr,gbasis,gbasis2];
    If[And@@((FermionQ[#]===True&)/@gbasis), Return[-tmpmat1]];
    If[And@@((VectorFieldQ[#]===True&)/@gbasis), Return[tmpmat1]];
    If[And@@((ScalarFieldQ[#]===True&)/@gbasis), Return[-tmpmat1]];
  ]; 
 
  (* Vector masses *)
  If[And@@(VectorFieldQ[#]===True&/@gbasis), tag=True; ReturnMat[ToString[id]]=ComputeMassMatrix2[rotlagr,gbasis,gbasis2]; Return[]];

  (* Scalar masses *)
  If[And@@(ScalarFieldQ[#]===True&/@gbasis), tag=True; 
    If[Type[id]==="SPS",
      (* need to split pseudosclar-scalar *)
      tmpmat1=ComputeMassMatrix2[rotlagr,gbasis,gbasis]/2;
      tmpmat2=ComputeMassMatrix2[rotlagr,gbasis,gbasis2]/2;
      tmpmat3=ComputeMassMatrix2[rotlagr,gbasis2,gbasis2]/2;
      ReturnMat[ToString[id]<>"S"] = - tmpmat1 - 2 tmpmat2 - tmpmat3;
      ReturnMat[ToString[id]<>"PS"] =  tmpmat1 - 2 tmpmat2 + tmpmat3; Return[],
      (* only one basis here -> normal way *)     
      ReturnMat[ToString[id]]=-ComputeMassMatrix2[rotlagr,gbasis,gbasis2]; Return[]; 
    ];
  ];  

  (* Four-component fermion masses *)  
  If[And@@((FermionQ[#]===True && WeylFieldQ[#]=!=True&)/@gbasis), tag=True; 
    ReturnMat[ToString[id]]=-ComputeMassMatrix2[rotlagr,gbasis,gbasis2]/.{ProjM[__]->1, ProjP[__]->0}; Return[]
  ];

  (* Two-component fermion masses *)  
  If[And@@((FermionQ[#]===True && WeylFieldQ[#]===True&)/@Flatten[gbasis]),  
    If[Type[id]=!="CWeyl", 
      (* neutral fields, one basis *)  
      tag=True; ReturnMat[ToString[id]]=-ComputeMassMatrix2[rotlagr,gbasis,gbasis]; Return[],
      (* charged fields, two bases *)
      tag=True; ReturnMat[ToString[id]]=-ComputeMassMatrix2[rotlagr,gbasis,anti/@gbasis2]; Return[]
    ];
  ];

  (* Check *)
  If[Not[tag], Print["Error in the extraction of the mass matrices"]; Abort[]];
];


(* ::Section:: *)
(*User's functions*)


(* ::Subsection:: *)
(*Computation of the tree-level mass matrices*)


(* ::Text:: *)
(*This function extract the mass matrices associated to the mixing declarations*)


Options[ComputeTreeLevelMassMatrix] = {
 Mix -> MR$Null,
 ScreenOutput->True,
 Basis1->{},
 Basis2->{}
}


ComputeTreeLevelMassMatrix[lagr_,OptionsPattern[]]:=Block[{tmplag, Mixings, mix,out,b1,b2},
  b1 = OptionValue[Basis1];b2 = OptionValue[Basis2];
  If[b1==={} && b2=!={}, Message[MassDiag::Basisprov]; Abort[]];
  If[b1=!={} && b2==={}, Message[MassDiag::Basisprov]; Abort[]];

  (* Initialization *)
  mix = OptionValue[Mix]; out = OptionValue[ScreenOutput];
  If[mix=!=MR$Null && Head[mix]=!=List,mix=List[mix]];
  If[mix=!=MR$Null && Not[MemberQ[M$MixingsDescription[[All,1]]/.Mix[id_]->id,#]],Message[MassDiag::NonExistingMix]; Abort[]]&/@mix;

  (* Preparing the Lagrangian *)
  If[out, Print["Index expansion ..."]];
  tmplag = Timing[SpecialExpandIndices[lagr]];
  If[out, Print["  => done in ", tmplag[[1]], " seconds."]]; tmplag=tmplag[[2]];

  (* All mass matrices *)
  If[mix===MR$Null && b1==={},
    FR$MassMatrices={};
    (* Getting the list of mixing matrices and calling the core function *)
    Mixings = ({#[[1]],{MixingMatrix,Value,GhostFieldQ[MassBasis]}/.#[[2]]}&/@M$MixingsDescription)/.GhostFieldQ[fields_]:>And@@(GhostFieldQ[#]===True&/@fields);
    Mixings = Cases[DeleteCases[Mixings,{_,{MixingMatrix,_,_}}|{_,{_,_,True}}],{_,{_,Value,_}}];
    Mixings = DeleteCases[If[NoCompute[#[[2,1]]]===True,{},#]&/@Mixings,{}];
    TreeLevelMassMatrix[#[[1]],tmplag]&/@Mixings;
    (* Printout *)
    If[out,
      Print["Tree level mass matrix computations achieved. Only the mass matrices necessary for the C++ code have been computed."];
      Print["Check FR$MassMatrices for the list of the MixingIDs."];
      Print["Use MassMatrix[ MixingID ], GaugeBasis[ MixingID ], MassBasis[ MixingID ], BlockName[ MixingID ] and MatrixSymbol[ MixingID ] "<>
          "for more information on the results."];
    ];
    Return[]
  ];

  (*else: one single mass matrix*)
  If[mix=!=MR$Null && b1==={},
    TreeLevelMassMatrix[Mix[#],tmplag]&/@mix;
    (* Printout *)
    (If[out, 
      Print["Computation of the tree level mass matrix for the mixing id "<> ToString[#] <>" achieved."];
      Print["For the results, use MassMatrix[ " <> ToString[#]<>" ], GaugeBasis[ " <> ToString[#]<>" ], MassBasis[ " <> ToString[#]<>" ], "<>
            "BlockName [ " <> ToString[#]<>" ] and MatrixSymbol [ "<>ToString[#]<>" ]."]
    ];
    If[Type[#]==="SPS",Print[ { MatrixForm[MassMatrix[#,"S"]], MatrixForm[MassMatrix[#, "PS"]]} ], Print[MatrixForm[MassMatrix[#]]]])&/@mix;
    Return[]
  ];

  (* Case "MUF": this computes a mass matrix associated to no particular basis *)
  Return[TreeLevelMassMatrix[Mix["MUF"], tmplag, Basis1->b1, Basis2->b2]];
];


(* ::Subsection::Closed:: *)
(*Getting back a gauge basis*)


GaugeBasis[id_,S]:=GaugeBasis[id,"S"];
GaugeBasis[id_,PS]:=GaugeBasis[id,"PS"];

GaugeBasis[id_,L]:=GaugeBasis[id,"L"];
GaugeBasis[id_,R]:=GaugeBasis[id,"R"];


GaugeBasis[id_,"S"]:=GaugeBasis[id];

GaugeBasis[id_,"PS"]:=GaugeBasis[id];


GaugeBasis[id_,letter_?(#==="L" || #==="R"&)] := Block[{resu,num},
  If[letter==="L", num=1, num=2];
  resu = GaugeBasis/.FR$MixingRules[Mix[id]];
  Return[resu[[num]]];
]; 


GaugeBasis[id_] := If[Type[id]==="FLR", Message[MassDiag::GBFLR]; Abort[],GaugeBasis/.FR$MixingRules[Mix[id]]];


(* ::Subsection::Closed:: *)
(*Getting back a mass basis*)


MassBasis[id_,S]:=MassBasis[id,"S"];
MassBasis[id_,PS]:=MassBasis[id,"PS"];


MassBasis[id_,"S"] := (MassBasis/.FR$MixingRules[Mix[id]])[[1]];

MassBasis[id_,"PS"] := (MassBasis/.FR$MixingRules[Mix[id]])[[2]];


MassBasis[id_] := If[Type[id]==="SPS", Message[MassDiag::GBSPS]; Abort[], MassBasis/.FR$MixingRules[Mix[id]]];


(* ::Subsection::Closed:: *)
(*Getting back a block name*)


BlockName[id_,S]:=BlockName[id,"S"];
BlockName[id_,PS]:=BlockName[id,"PS"];

BlockName[id_,L]:=BlockName[id,"L"];
BlockName[id_,R]:=BlockName[id,"R"];


BlockName[id_,"S"]:=Block[{resu,mmix},
  resu=BlockName/.FR$MixingRules[Mix[id]]; 
  If[resu===BlockName,
   mmix = MixingMatrix/.FR$MixingRules[Mix[id]];
   If[mmix=!=MixingMatrix, resu[[1]]=BlockToParam[mmix[[1]]]/.BlockToParam[_]->BlockName]
  ];
  Return[(resu/.BlockName->{"No BlockName found.","No BlockName found."})[[1]]];
];

BlockName[id_, "PS"]:=Block[{resu,mmix},
  resu=BlockName/.FR$MixingRules[Mix[id]]; 
  If[resu===BlockName, 
   mmix = MixingMatrix/.FR$MixingRules[Mix[id]];
   If[mmix=!=MixingMatrix, resu={"No BlockName found.",BlockToParam[mmix[[2]]]/.BlockToParam[_]->BlockName}]
  ];
  Return[(resu/.BlockName->{"No BlockName found.","No BlockName found."})[[2]]];
];


BlockName[id_,letter_?(#==="L" || #==="R"&)] := Block[{resu,num,mmix},
  If[letter==="L", num=1, num=2];
  resu = BlockName/.FR$MixingRules[Mix[id]]/.Blank->MR$Null/.MR$Null[]->MR$Null;
  If[resu===BlockName,
   mmix = MixingMatrix/.FR$MixingRules[Mix[id]];
   If[mmix=!=MixingMatrix, If[mmix[[num]]=!=Blank[],resu=BlockToParam[mmix[[num]]]/.BlockToParam[_]->BlockName]]
  ];
  If[resu===BlockName, Return["No BlockName found."]];
  If[resu[[num]]===MR$Null,
   mmix = MixingMatrix/.FR$MixingRules[Mix[id]];
   If[mmix=!=MixingMatrix, If[mmix[[num]]=!=Blank[],Print[FullForm[mmix]];resu[[num]]=BlockToParam[mmix[[num]]]/.BlockToParam[_]->BlockName]]
  ];
  resu=resu/.MR$Null->"No BlockName found."/.BlockName->"No BlockName found.";
  Return[resu[[num]]];
]; 


BlockName[id_] := Block[{resu,mmix},
  If[Type[id]==="SPS", Message[MassDiag::GBSPS]; Abort[]];
  If[Type[id]==="FLR", Message[MassDiag::GBFLR]; Abort[]];
  If[Type[id]==="F4", Message[MassDiag::GBFLR]; Abort[]];
  resu=BlockName/.FR$MixingRules[Mix[id]];
  If[resu===BlockName, 
   mmix = MixingMatrix/.FR$MixingRules[Mix[id]];
   If[mmix=!=MixingMatrix, resu=BlockToParam[mmix]/.BlockToParam[_]->BlockName]
  ];
  Return[resu/.BlockName->"No BlockName found."];
];


(* ::Subsection::Closed:: *)
(*Getting back a matrix symbol*)


MatrixSymbol[id_,S]:=MatrixSymbol[id,"S"];
MatrixSymbol[id_,PS]:=MatrixSymbol[id,"PS"];

MatrixSymbol[id_,L]:=MatrixSymbol[id,"L"];
MatrixSymbol[id_,R]:=MatrixSymbol[id,"R"];


MatrixSymbol[id_,"S"] := ((MixingMatrix/.FR$MixingRules[Mix[id]])/.MixingMatrix->{"No matrix symbol found.","No matrix symbol found."})[[1]];

MatrixSymbol[id_,"PS"] := ((MixingMatrix/.FR$MixingRules[Mix[id]])/.MixingMatrix->{"No matrix symbol found.","No matrix symbol found."})[[2]];


MatrixSymbol[id_,letter_?(#==="L" || #==="R"&)] := Block[{resu,num},
  If[letter==="L", num=1, num=2];
  resu = MixingMatrix/.FR$MixingRules[Mix[id]]/.Blank->MR$Null/.MR$Null[]->"No matrix symbol found.";
  If[resu===MixingMatrix, Return["No matrix symbol found."], Return[resu[[num]]]];
]; 


MatrixSymbol[id_] := Block[{resu},
  If[Type[id]==="SPS", Message[MassDiag::GBSPS]; Abort[]];
  If[Type[id]==="FLR", Message[MassDiag::GBFLR]; Abort[]];
  If[Type[id]==="F4", Message[MassDiag::GBFLR]; Abort[]];
  Return[MixingMatrix/.FR$MixingRules[Mix[id]]/.MixingMatrix->"No matrix symbol found."];
];


(* ::Subsection::Closed:: *)
(*Getting back a mass matrix*)


MassMatrix[id_,S]:=MassMatrix[id,"S"];
MassMatrix[id_,PS]:=MassMatrix[id,"PS"];

MassMatrix[id_,L]:=MassMatrix[id,"L"];
MassMatrix[id_,R]:=MassMatrix[id,"R"];


MassMatrix[id_,"S"]:=Block[{sym=ToString[id]<>"S"},If[MemberQ[FR$MassMatrices,id], Simplify[ReturnMat[sym]], Print["Matrix not computed."]; Return[];]];

MassMatrix[id_,"PS"]:=Block[{sym=ToString[id]<>"PS"},If[MemberQ[FR$MassMatrices,id],Simplify[ReturnMat[sym]],Print["Matrix not computed."]; Return[];]];


MassMatrix[id_,"L"]:=MassMatrix[id];
MassMatrix[id_,"R"]:=MassMatrix[id];


MassMatrix[id_]:=If[Type[id]==="SPS", Message[MassDiag::GBSPS]; Abort[],If[MemberQ[FR$MassMatrices,id], Simplify[ReturnMat[ToString[id]]],"Matrix not computed."]];


(* ::Subsection::Closed:: *)
(*Getting back the value of a mixing matrix*)


MixMatrix[id_,S]:=MixMatrix[id,"S"];
MixMatrix[id_,PS]:=MixMatrix[id,"PS"];

MixMatrix[id_,L]:=MixMatrix[id,"L"];
MixMatrix[id_,R]:=MixMatrix[id,"R"];


MixMatrix[id_,"S"] := Block[{val},
  val=(Value/.FR$MixingRules[Mix[id]]);
  If[NoCompute[MatrixSymbol[id,"S"]]===True, Return["This matrix must be given as an external parameter by the user."]];
  If[val===Value, Return["Please use the numerical code."], Return[val[[1]]/.Value->"Please use the numerical code."]];
];

MixMatrix[id_, "PS"] := Block[{val},
  val=(Value/.FR$MixingRules[Mix[id]]);
  If[NoCompute[MatrixSymbol[id,"PS"]]===True, Return["This matrix must be given as an external parameter by the user."]];
  If[val===Value, Return["Please use the numerical code."], Return[val[[2]]/.Value->"Please use the numerical code."]];
];


MixMatrix[id_,letter_?(#==="L" || #==="R"&)] := Block[{resu,num},
  If[letter==="L", num=1, num=2];
  resu = Value/.FR$MixingRules[Mix[id]];
  If[NoCompute[MatrixSymbol[id,letter]]===True, Return["This matrix must be given as an external parameter by the user."]];
  If[resu===Value, Return["Please use the numerical code."],resu=resu[[num]]];    
  If[And@@(MatchQ[#,Rule[_,_]]&/@ resu),
    Return[Table[MatrixSymbol[id,letter][iii,jjj],{iii,Length[MassBasis[id]]},{jjj,Length[MassBasis[id]]}]/.resu],
    Return[resu]
  ];
]; 


MixMatrix[id_] := Block[{resu},
  If[Type[id]==="SPS", Message[MassDiag::GBSPS]; Abort[]];
  If[Type[id]==="FLR", Message[MassDiag::GBFLR]; Abort[]];
  If[Type[id]==="F4", Message[MassDiag::GBFLR]; Abort[]];
  resu = Value/.FR$MixingRules[Mix[id]];
  If[NoCompute[MatrixSymbol[id]]===True, Return["This matrix must be given as an external parameter by the user."]];
  If[resu===Value, Return["Please use the numerical code."]];  
  If[And@@(MatchQ[#,Rule[_,_]]&/@ resu),
    Return[Table[MatrixSymbol[id][iii,jjj],{iii,Length[MassBasis[id]]},{jjj,Length[MassBasis[id]]}]/.resu],
    Return[resu]
  ];
];  


(* ::Subsection:: *)
(*Summary function*)


MixingSummary[sbl_?(Not[StringQ[#]]&)]:= Block[{},Message[MassDiag::MixingSummaryArgs]; Abort[]];


MixingSummary[sbl_String]:=Block[{}, 
  If[Type[sbl]==="SPS", 
    Print["Gauge basis = ",GaugeBasis[sbl]];
    Print["Scalar sector"];
    Print["***************************************************"];
    Print["  Mass basis = ", MassBasis[sbl,"S"], "\n  Block = ", BlockName[sbl,"S"], "\n  Symbol = ",MatrixSymbol[sbl,"S"]];
    Print["  Squared mass matrix = ", MatrixForm[MassMatrix[sbl, "S"]]];
    Print["  Mixing matrix = ", MatrixForm[MixMatrix[sbl, "S"]]];
    Print["Pseudoscalar sector"];
    Print["***************************************************"];
    Print["  Mass basis = ", MassBasis[sbl,"PS"], "\n  Block = ", BlockName[sbl,"PS"], "\n  Symbol = ",MatrixSymbol[sbl,"PS"]];
    Print["  Squared mass matrix = ", MatrixForm[MassMatrix[sbl, "PS"]]];
    Print["  Mixing matrix = ", MatrixForm[MixMatrix[sbl, "PS"]]];
    Return[];
  ];
  If[Type[sbl]==="FLR", 
    Print["Mass basis = ", MassBasis[sbl]];
    Print["Mass matrix = ", MatrixForm[MassMatrix[sbl]]];
    Print["Left-handed fermions"];
    Print["****************************************************"];    
    Print["  Gauge basis = ",GaugeBasis[sbl, "L"]];
    Print["  Block = ", BlockName[sbl,"L"], "\n  Symbol = ",MatrixSymbol[sbl,"L"]];    
    Print["  Mixing matrix = ", MatrixForm[MixMatrix[sbl, "L"]]];    
    Print["Right-handed fermions"];
    Print["****************************************************"];
    Print["  Gauge basis = ",GaugeBasis[sbl, "R"]];
    Print["  Block = ", BlockName[sbl,"R"], "\n  Symbol = ",MatrixSymbol[sbl,"R"]];
    Print["  Mixing matrix = ", MatrixForm[MixMatrix[sbl, "R"]]];
    Return[];
  ];
  If[Type[sbl]==="F4", 
    Print["Mass basis = ", MassBasis[sbl]];
    Print["  Gauge basis = ",GaugeBasis[sbl, "L"]];
    Print["Mass matrix = ", MatrixForm[MassMatrix[sbl]]];
    Print["Left-handed fermions"];
    Print["****************************************************"];
    Print["  Block = ", BlockName[sbl,"L"], "\n  Symbol = ",MatrixSymbol[sbl,"L"]];    
    Print["  Mixing matrix = ", MatrixForm[MixMatrix[sbl, "L"]]];
    Print["Right-handed fermions"];
    Print["****************************************************"];
    Print["  Block = ", BlockName[sbl,"R"], "\n  Symbol = ",MatrixSymbol[sbl,"R"]];
    Print["  Mixing matrix = ", MatrixForm[MixMatrix[sbl, "R"]]];
    Return[];
  ];
  If[Type[sbl]==="CWeyl", 
    Print["Mass matrix = ", MatrixForm[MassMatrix[sbl]]];
    Print["First basis of charged Weyl fermions"];
    Print["****************************************************"];    
    Print["  Mass basis = ", MassBasis[sbl][[1]]];    
    Print["  Gauge basis = ",GaugeBasis[sbl][[1]]];
    Print["  Block = ", BlockName[sbl][[1]], "\n  Symbol = ",MatrixSymbol[sbl][[1]]];
    Print["  Mixing matrix = ", MatrixForm[MixMatrix[sbl, "L"]]];
    Print["Second basis of charged Weyl fermions"];
    Print["****************************************************"];    
    Print["  Mass basis = ", MassBasis[sbl][[2]]];    
    Print["  Gauge basis = ",GaugeBasis[sbl][[2]]];
    Print["  Block = ", BlockName[sbl][[2]], "\n  Symbol = ",MatrixSymbol[sbl][[2]]];
    Print["  Mixing matrix = ", MatrixForm[MixMatrix[sbl, "R"]]];    
    Return[];
  ];
  Print["Gauge basis = ",GaugeBasis[sbl]];
  Print["Mass basis = ", MassBasis[sbl], "\nBlock = ", BlockName[sbl], "\nSymbol = ",MatrixSymbol[sbl]];
  Print["Squared mass matrix = ", MatrixForm[MassMatrix[sbl]]];
  Print["The mixing matrix = ", MatrixForm[MixMatrix[sbl]]];
  Return[];
];


(* ::Section:: *)
(*One loop mass matrices computation*)


(* ::Section:: *)
(*ComputeMassMatrix*)


Options[ComputeMassMatrix]={ Mix -> MR$Null, ScreenOutput->True, Basis1->{}, Basis2->{}};


ComputeMassMatrix[lagr_,OptionsPattern[]]:=Block[{},
    ComputeTreeLevelMassMatrix[lagr,Mix->OptionValue[Mix],ScreenOutput->OptionValue[ScreenOutput],Basis1->OptionValue[Basis1],Basis2->OptionValue[Basis2]]];
