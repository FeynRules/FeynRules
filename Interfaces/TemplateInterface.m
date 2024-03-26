(* ::Package:: *)

(* ::Title:: *)
(*Template Interface*)


(* ::Text:: *)
(*Authors:  Dr.X, Dr. Y*)


(* ::Subsection:: *)
(*Helpful Definitions and Functions*)


(* ::Subsubsection::Closed:: *)
(*TemplateCFunctionReplacements*)


TemplateCFunctionReplacements={
Cos->cos,Sin->sin,Tan->tan,
Sec->sec,Csc->csc,Cot->cot,
Cosh->cosh,Sinh->sinh,Tanh->tanh,
Sech->sech,Csch->csch,Coth->coth,
ArcCos->acos,ArcSin->asin,ArcTan->atan,
ArcSec->asec,ArcCsc->acsc,ArcCot->acot,
ArcCosh->acosh,ArcSinh->asinh,ArcTanh->atanh,
ArcSech->asech,ArcCsch->acsch,ArcCoth->acoth,
Sqrt->sqrt,Power->pow,Exp->exp,Log->log,
Abs->fabs,Ceiling->ceil,Floor->floor,Max->fmax,Min->fmin
};


(* ::Subsection:: *)
(*WriteTemplateOutput*)


(* ::Subsubsection::Closed:: *)
(*WriteTemplateOutput[lags, options]*)


WriteTemplateOutput[lags__, options___] := WriteTemplateOutput[{lags}, options] /; (And @@ ((Head[#] =!= Rule &) /@ {lags})) && (And @@ ((Head[#] === Rule &) /@ {options})) && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteTemplateOutput[lags__]:=WriteTemplateOutput[{lags},dummyOption->False]/;(And @@ ((Head[#] =!= Rule &) /@ {lags})) && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteTemplateOutput[lags_List,options___]:=Module[{olddirName,dirName},
(*Print message to the screen.*)
Print[" - - - FeynRules interface to Template - - -"];
Print[" - - -    Authors: Dr. X and Dr. Y     - - -"];
Print[" - - -   Please cite arXiv:0000.0000   - - -"];

(*Create directory if not present.*)
olddirName=Directory[];
If[$OperatingSystem==="Windows",fileSlash="\\",fileSlash="/"]; 
dirName=Output/.{options}/.Output:>olddirName<>fileSlash<>StringReplace[M$ModelName, " " -> "-"]<>"-Template";

CreateDirectory[dirName];
SetDirectory[dirName];
Print["Writing files to ",dirName," directory.\n"];

(*Write particles, parameters and vertices.*)
WriteTemplateParticles[];
WriteTemplateExtParams[];
WriteTemplateIntParams[];
WriteTemplateVertices[lags, Sequence @@ DeleteCases[{options}, Rule[Output,_] | RuleDelayed[Output,_]]];

(*Reset directory.*)
SetDirectory[olddirName];

(*Print Done!*)
Print["Done!"];
];


(* ::Subsection::Closed:: *)
(*Particles*)


(* ::Subsubsection::Closed:: *)
(*WriteTemplateParticles[options]*)


WriteTemplateParticles[]:=WriteTemplateParticles[Output->"TemplateParticles.txt"];

WriteTemplateParticles[options___]:=Module[{fileName,fileHandle,j,k},
(*Create particle file.*)
fileName=Output/.options/.Output->"TemplateParticles.txt";
fileHandle=OpenWrite[fileName];
Print["Writing particles to ",fileName,".\n"];

(*Write comments at the beginning.*)
WriteString[fileHandle,"# This file generated automatically by Template Interface.\n\n"];

(*Loop through particles.*)
WriteString[fileHandle,"#Part\tAnti\tSpin\tMass\tWidth\tColor\tPDG\n"];
Do[
WriteString[fileHandle,ToString[PartList[[j,2,k,1]]]<>"\t"<>ToString[PartList[[j,2,k,2]]]<>"\t"<>ToString[PartList[[j,2,k,3]]]<>"\t"<>ToString[PartList[[j,2,k,5]]/.ZERO->0]<>"\t"<>ToString[PartList[[j,2,k,6]]/.ZERO->0]<>"\t"<>ToString[PartList[[j,2,k,7]]]<>"\t"<>ToString[PartList[[j,2,k,9]]/.NoPDG[a_]->a]<>"\n"];
,{j,1,Length[PartList]},{k,1,Length[PartList[[j,2]]]}];

Close[fileHandle];
];


(*WriteTemplateParticles[Output->"~/tmp/Particles.tmp"]*)


(* ::Subsection::Closed:: *)
(*Parameters*)


(* ::Subsubsection:: *)
(*WriteTemplateExtParams[options]*)


WriteTemplateExtParams[]:=WriteTemplateExtParams[Output->"TemplateExtParams.txt"];

WriteTemplateExtParams[options__]:=Module[{fileName,fileHandle,j, TemplateEParamList, TemplateMassList, TemplateWidthList},
(*Create external parameter file.*)
fileName=Output/.options/.Output->"TemplateExtParams.txt";
fileHandle=OpenWrite[fileName];
Print["Writing external parameters to ",fileName,".\n"];

(*Write comments at the beginning.*)
WriteString[fileHandle,"# This file generated automatically by Template Interface.\n\n"];

(* Check for external parameters without a value *)
If[Not[FreeQ[EParamList, NoValue] && FreeQ[MassList, NoValue] && FreeQ[WidthList, NoValue]],
   Print["Warning: All external parameters which have not been assigned a numerical value will be set to 1."]];
TemplateEParamList = EParamList /. NoValue -> Identity /. NoBlockName -> Identity;
TemplateMassList = MassList /. NoValue -> Identity;
TemplateWidthList = WidthList /. NoValue -> Identity;

(*Loop through external parameters.*)
Do[
WriteString[fileHandle,"Block "<>ToString[TemplateEParamList[[j,1]] /. NoBlockName -> Identity]<>"\n"];
(*Loop through parameters in this block.*)
Do[
WriteString[fileHandle,"\t"<>ToString[TemplateEParamList[[j,2,k,1,1]]]<>"\t"<>ToString[TemplateEParamList[[j,2,k,2,1]]]<>"\t"<>ToString[TemplateEParamList[[j,2,k,2,-3]]]<>"\t\t#"<>ToString[TemplateEParamList[[j,2,k,2,-1]]]<>"\n"];
,{k,1,Length[TemplateEParamList[[j,2]]]}];
,{j,1,Length[TemplateEParamList]}];

(*Loop through masses.*)
WriteString[fileHandle,"Block Masses\n"];
Do[
If[TemplateMassList[[2,j,3]]=!=Internal,WriteString[fileHandle,"\t"<>ToString[j]<>"\t"<>ToString[TemplateMassList[[2,j,2]]]<>"\t"<>ToString[TemplateMassList[[2,j,3]]]<>"\n"];];
,{j,1,Length[TemplateMassList[[2]]]}];

(*Loop through widths.*)
WriteString[fileHandle,"Block Widths\n"];
Do[
If[TemplateWidthList[[2,j,3]]=!=Internal,WriteString[fileHandle,"\t"<>ToString[j]<>"\t"<>ToString[TemplateWidthList[[2,j,2]]]<>"\t"<>ToString[TemplateWidthList[[2,j,3]]]<>"\n"];];
,{j,1,Length[TemplateWidthList[[2]]]}];

(*Close parameter file.*)
Close[fileHandle];
];


(* ::Input:: *)
(*(*WriteTemplateExtParams[Output->"~/tmp/Ext.tmp"]*)*)


(* ::Subsubsection::Closed:: *)
(*ReadTemplateExtParams[options]*)


ReadTemplateExtParams[]:=ReadTemplateExtParams[Input->"TemplateExtParams.txt"];

ReadTemplateExtParams[options__]:=Module[{fileName,fileHandle,j=0,line,parameter,replaceList={}},
(*Create external parameter file.*)
fileName=Input/.options/.Input->"TemplateExtParams.txt";
fileHandle=OpenRead[fileName];
Print["Reading external parameters from ",fileName,"."];

(*Loop through the parameters in the file.*)
line=Read[fileHandle,String];
While[line=!=EndOfFile,
If[StringTake[line,{1}]=!="#",
parameter=StringSplit[line,{" ","\t"}];
If[parameter[[1]]=!="Block"&&parameter[[1]]=!="",
AppendTo[replaceList,ToExpression[parameter[[2]]]->ToExpression[parameter[[3]]]];
];
];
line=Read[fileHandle,String];
];

(*Update the parameters.*)
UpdateParameters[replaceList/.List[a__]->a];

(*Close parameter file.*)
Close[fileHandle];
];


(*ReadTemplateExtParams[Input->"~/tmp/Ext.tmp"]*)


(* ::Subsubsection::Closed:: *)
(*WriteTemplateIntParams[options]*)


WriteTemplateIntParams[]:=WriteTemplateIntParams[Output->"TemplateIntParams.txt"];

WriteTemplateIntParams[options__]:=Module[{fileName,fileHandle,j, TemplateIParamList},
(*Create external parameter file.*)
fileName=Output/.options/.Output->"TemplateIntParams.txt";
fileHandle=OpenWrite[fileName];
Print["Writing internal parameters to ",fileName,".\n"];

(*Write comments at the beginning.*)
WriteString[fileHandle,"# This file generated automatically by Template Interface.\n\n"];

(* Check for internal parameters without a value *)
If[Not[FreeQ[IParamList, NoValue]],
   Print["Warning: All internal parameters which have not been assigned a numerical value will be set to 1."]];
TemplateIParamList = IParamList /. NoValue -> Identity;

(*Loop through internal parameters.*)
Do[
WriteString[fileHandle,ToString[TemplateIParamList[[j,1]]]<>"\t"<>ToString[CForm[TemplateIParamList[[j,2]]]/.TemplateCFunctionReplacements]<>"\t\t\t#"<>ToString[TemplateIParamList[[j,-1]]]<>"\n"];
,{j,1,Length[TemplateIParamList]}];

(*Close parameter file.*)
Close[fileHandle];
];


(* ::Input:: *)
(*(*WriteTemplateIntParams[Output->"~/tmp/Int.tmp"]*)*)


(* ::Subsection:: *)
(*Vertices*)


(* ::Subsubsection::Closed:: *)
(*TemplateVReplaceList' s*)


(*Make a list of replacement rules that take the Mathematica form of various things to the Template form.*)
TemplateVReplaceList1={
Index[type_,Ext[a_]]:>Index[type,a],
Index[type_,a_,b_]:>Index[type,ToExpression[ToString[a]<>ToString[b]]]
};
TemplateVReplaceList2={
f[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]->SU3f[a,b,c],
T[Index[Gluon,a_],Index[Colour,b_],Index[Colour,c_]]->SU3T[a,b,c],
ME[Index[Lorentz,a_],Index[Lorentz,b_]]:>me[a,b],
FV[a_,Index[Lorentz,b_]]:>p[a,b],
Ga[Index[Lorentz,a_]]->ga[a],
TensDot[a___,ga[b_],c___][Index[Spin,d_],Index[Spin,e_]]->gaP[a,ga[b],c,d,e],
Ga[Index[Lorentz,a_],Index[Spin,b_],Index[Spin,c_]]->gaP[ga[a],b,c],
Ga[5,Index[Spin,a_],Index[Spin,b_]]->gaP[ga[5],a,b],
ProjP[Index[Spin,a_],Index[Spin,b_]]->gaP[ProjP,a,b],
ProjM[Index[Spin,a_],Index[Spin,b_]]->gaP[ProjM,a,b],
IndexDelta[Index[Spin,a_],Index[Spin,b_]]->gaP[a,b]
};


(* ::Subsubsection::Closed:: *)
(*Reordering of the fields and fermion flow*)


Reordering[ll_List, vert_] := Module[{Reorder, temp, output,vt, AntiToEnd, ffv, ParticleType, tvert, shuffle},
        ParticleType[_?(((DiracFieldQ[#] === True) || (MajoranaFieldQ[#] === True))&)] := "F";
        ParticleType[_?(VectorFieldQ[#] === True &)] := "V";
        ParticleType[_?(GhostFieldQ[#] === True &)] := "U";        
        ParticleType[_?(((ScalarFieldQ[#] === True) && Not[GhostFieldQ[#] === True]) &)] := "S";
        ParticleType[_?(Spin2FieldQ[#] === True &)] := "T";
        shuffle[1] = 3;
        shuffle[2] = 2;
        shuffle[3] = 1;
        Reorder[x___, f1_?(Not[FermionQ[#] === True]&), f2_?(FermionQ[#] === True &), y___] := Reorder[x, f2, f1, y];
        Reorder[x___, f1_?(Not[FermionQ[#] === True] && Not[VectorFieldQ[#] === True]&), f2_?(VectorFieldQ[#] === True &), y___] := ReOrderFunc[x, f2, f1, y];
        AntiToEnd[x___, f1_?(AntiFieldQ[#] === True &), f2_?(Not[AntiFieldQ[#] === True] &), y___] := AntiToEnd[x, f2, f1, y];
        temp = Reorder @@ ll;
        temp = List @@ temp;
        vt = StringJoin @@ (ParticleType /@ temp);
        output = Which[(vt === "FFV") || (vt === "FFS"),
              ffv = {ll[[3]], anti[ll[[2]]], anti[ll[[1]]]};
              ffv = ffv //. CC -> anti //. field_?(((MajoranaFieldQ[#] === True) && (AntiFieldQ[#] === True)) &) :> anti[field];
              tvert = vert /. {Ext[k_] :>  Ext[shuffle[k]], FV[k_, mu_] :> FV[shuffle[k], mu], SP[k1_, k2_] :> SP[shuffle[k1], shuffle[k2]]};
              {ffv, tvert},
           True, {ll, vert}]];


(* ::Subsubsection::Closed:: *)
(*WriteTemplateVertices[options]*)


WriteTemplateVertices[lags__, options__] := WriteTemplateVertices[{lags}, options] /; (And @@ ((Head[#] =!= Rule &) /@ {lags})) && (And @@ ((Head[#] === Rule &) /@ {options})) && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteTemplateVertices[lags__]:=WriteTemplateVertices[{lags},Output->"TemplateVertices.txt"]/;(And @@ ((Head[#] =!= Rule &) /@ {lags})) && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteTemplateVertices[{lags__}]:=WriteTemplateVertices[{lags},Output->"TemplateVertices.txt"]/;(And @@ ((Head[#] =!= Rule &) /@ {lags}));

WriteTemplateVertices[lags_List, options___]:=Module[{fileName,fileHandle,vertices={},verts},
(*Loop through lagrangians and generate Feynman rules.*)
Do[
Print["Generating vertices for L",j];
verts=FeynmanRules[lags[[j]],ScreenOutput->False,MaxCanonicalDimension->4,FlavorExpand->True];
If[Length[vertices]>0,vertices=MergeVertices[vertices,verts],vertices=verts];
,{j,1,Length[lags]}];

(*Create vertices file.*)
fileName=Output/.options/.Output->"TemplateVertices.txt";
fileHandle=OpenWrite[fileName];
Print["\nWriting vertices to ",fileName,".\n"];

(*Write comments at the beginning.*)
WriteString[fileHandle,"# This file was generated automatically by Template Interface.\n\n"];




(* The fermion flow *)
vertices = vertices /. {field_?(FieldQ[#] === True &), _?NumericQ} -> field;
vertices = Reordering @@@ vertices;

(*Loop through vertices.*)
WriteString[fileHandle,"#Part1\tPart2\tPart3\tPart4\tVertex\n"];

Do[
WriteString[fileHandle,PartName[vertices[[j,1,1]]]<>"\t"<>PartName[vertices[[j,1,2]]]<>"\t"<>PartName[vertices[[j,1,3]]]];
If[Length[vertices[[j,1]]]===4,WriteString[fileHandle,"\t"<>PartName[vertices[[j,1,4]]]],WriteString[fileHandle,"\t"]];
WriteString[fileHandle,"\t"<>ToString[CForm[vertices[[j,2]]//.ParamRules/.TemplateVReplaceList1//.TemplateVReplaceList2]//.TemplateCFunctionReplacements]<>"\n\n"];
,{j,1,Length[vertices]}];

(*Close parameter file.*)
Close[fileHandle];
];
