(* ::Package:: *)

(* ::Title:: *)
(*CalcHep Interface*)


(* ::Text:: *)
(*N. Christensen, C. Duhr*)


(* ::Subsection::Closed:: *)
(*Some Useful Functions*)


(* ::Subsubsection::Closed:: *)
(*stringResize[string,length]*)


stringResize[string_,length_]:=Module[{newstring,ilength,blankstring},
blankstring="                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ";
ilength=StringLength[string];
(*Print[CH$BlankString];*)
Which[
ilength>length,newstring=StringTake[string,length],
ilength==length,newstring=string,
ilength<length,newstring=string<>StringJoin[Table[" ",{length-ilength}]];
(*StringTake[CH$BlankString,length-ilength];*)
];
newstring];


(* ::Subsubsection::Closed:: *)
(*resizeName[name]*)


CHName[name_]:=resizeName[name];

resizeName[name_]:=Module[{newName,PartListTmp,j,piece1,piece2,result},
PartListTmp=Flatten[PartList[[All,2]],1];
(*If it is a GB or Ghost, split it into the piece before the . and the piece after.*)
result=StringSplit[name,"."];
(*Now resize.*)
Do[
Which[
result[[1]]===PartListTmp[[j,1]]&&StringTake[result[[1]],1]==="~",newName=stringResize[result[[1]],CH$ParticleNameLength+1],
result[[1]]===PartListTmp[[j,1]]&&StringTake[result[[1]],1]=!="~",newName=stringResize[result[[1]],CH$ParticleNameLength]<>" ",
result[[1]]===PartListTmp[[j,2]]&&StringTake[result[[1]],1]==="~"&&StringLength[result[[1]]]>CH$ParticleNameLength+1,newName=(stringResize[result[[1]],CH$ParticleNameLength+1]),
result[[1]]===PartListTmp[[j,2]]&&StringTake[result[[1]],1]=!="~"&&StringLength[result[[1]]]>CH$ParticleNameLength,newName=("~"<>stringResize[result[[1]],CH$ParticleNameLength]),
result[[1]]===PartListTmp[[j,2]],newName=stringResize[result[[1]],CH$ParticleNameLength+1]
];
,{j,1,Length[PartListTmp]}];
(*If it is a GB or Ghost, add the .f, .c, or .C back on.*)
If[Length[result]>1,newName=StringReplace[newName<>"."<>result[[2]]," "->""]];
newName
];


(* ::Subsubsection::Closed:: *)
(*ParticleColor[particle]*)


ParticleColor[particle_]:=Module[{PartListTmp,color,j},
PartListTmp=Flatten[PartList[[All,2]],1];
color=1;
Do[
If[particle===PartListTmp[[j,1]]||particle===PartListTmp[[j,2]],color=PartListTmp[[j,7]]];
,{j,1,Length[PartListTmp]}];
color
];


(* ::Subsubsection::Closed:: *)
(*ParticleSpin[particle]*)


ParticleSpin[particle_]:=Module[{PartListTmp,spin,j},
PartListTmp=Flatten[PartList[[All,2]],1];
spin=1;
Do[
If[particle===PartListTmp[[j,1]]||particle===PartListTmp[[j,2]],spin=PartListTmp[[j,3]]];
,{j,1,Length[PartListTmp]}];
spin
];


(* ::Subsubsection::Closed:: *)
(*PartOrAntiPart[particle]*)


PartOrAntiPart[part_]:=Module[{PartListTmp,partorantipart,j},
PartListTmp=Flatten[PartList[[All,2]],1];
partorantipart=0;
Do[
Which[part===PartListTmp[[j,1]],partorantipart="part",part===PartListTmp[[j,2]],partorantipart="antipart"];
,{j,1,Length[PartListTmp]}];
partorantipart
];


(* ::Subsubsection::Closed:: *)
(*CheckSizeOfExpression[name,expression,size]*)


CH$LongestSoFar=1;
CheckSizeOfExpression[name_,expression_,size_]:=If[StringLength[expression]>size&&StringLength[expression]>CH$LongestSoFar,
CH$LongestSoFar=StringLength[expression];
If[CH$PrintWarnings&&CH$CompHEP,
Print[Style["\tWarning!",Red]," Expression for ",name," is longer than I was prepared for."];
Print["\t\tPlease increase the max size to atleast ",StringLength[expression],"."];
Print["\t\te.g. WriteCHOutput[Lagrangians,MaxExpressionLength->",StringLength[expression],"];"]; 
];
];


(* ::Subsubsection::Closed:: *)
(*CommonLorentzFactor[vertex]*)


CommonLorentzFactor[vertex_]:=Module[{j,commonDen,vertexTerms={},leftOver,commonTerm=1},
If[Head[vertex]===Plus,
commonDen=Denominator[Together[vertex]];
(*Print[commonDen];*)
Do[AppendTo[vertexTerms,vertex[[j]]/.{CHLoChain[__]->1,CHGaChain[___][__]->1}],{j,1,Length[vertex]}];

vertexTerms=commonDen vertexTerms;
(*Print[vertexTerms];*)
If[Length[vertexTerms]>1,
leftOver=Simplify[vertexTerms[[1]]/vertexTerms[[2]]];
commonTerm=vertexTerms[[1]]/Numerator[leftOver];
(*Print[commonTerm];*)
Do[
leftOver=Simplify[commonTerm/vertexTerms[[j]]];
commonTerm=commonTerm/Numerator[leftOver];
(*Print[commonTerm];*)
,{j,3,Length[vertexTerms]}];
];
commonTerm=Simplify[commonTerm/commonDen];
,
commonTerm=vertex/.{CHLoChain[__]->1,CHGaChain[___][__]->1};
(*Print[{1}];*)
];
commonTerm
];


(* ::Subsubsection::Closed:: *)
(*CFunctionReplacements0*)


CFunctionReplacements0={Sec[a_]->HoldForm[Cos[a]^-1],Csc[a_]->HoldForm[Sin[a]^-1],Cot[a_]->HoldForm[Tan[a]^-1]};


(* ::Subsection::Closed:: *)
(*WriteCHOutput[lags]*)


(*Initialize numbering for constants.*)
CH$CouplingN=0;
(*Numbering for auxiliary fields.*)
CH$AuxiliaryN=0;
(*Initialize numbering for model.*)
CH$ModelNumber=1;
(*Setup options for WriteCHOutput*)
CH$CompHEP=False;
(*Max Expression Length*)
CH$MaxExpressionLength=100;
(*The new version of CH allows any length particle names.*)
CH$ParticleNameLength=10;
(*The new version of CH allows parameters of any length.*)
CH$ParameterNameLength=15;
(*Exclude 4 Scalar Interactions *)
CH$Exclude4Scalars=False;
(*Whether to simplify vertices*)
CH$Simplify=True;
(*Whether to set the widths to automatic.*)
CH$AutoWidths=True;
(*Whether to print warnings.*)
CH$PrintWarnings=True;
(*Alternate verices*)
CH$Vertices=Automatic;
(*MaxCanonicalDimension*)
CH$MaxCanonicalDimension=5;
(*LHA support*)
CH$LHASupport=False;
(*Whether to use Dialogs to ask the user for input.*)
CH$DialogBox=On;
Options[WriteCHOutput]={MaxExpressionLength->100,CompHEP->False,ModelNumber->1, Output -> Automatic, CHParticleNameLength->10, ParameterNameLength->15, Exclude4Scalars->False,CHSimplify->True,CHAutoWidths->True,PrintWarnings->True,CHVertices->Automatic,MaxCanonicalDimension->5,MaxParticles->4,LHASupport->False,DialogBox->On, ConservedQuantumNumbers :> MR$QuantumNumbers};


WriteCHOutput[lags__, options__] := WriteCHOutput[{lags}, options] /; (And @@ ((Head[#] =!= Rule &) /@ {lags})) && (And @@ ((Head[#] === Rule &) /@ {options})) && (And @@ ((Head[#]=!=List&) /@ {lags}));

WriteCHOutput[lags__] := WriteCHOutput[{lags}] /; (And @@ ((Head[#] =!= Rule &) /@ {lags}))  && (And @@ ((Head[#]=!=List&) /@ {lags}));

(*OptionsPattern[] only works in 6.0.  
WriteCHOutput[lags__,OptionsPattern[]]:=Block[{dirname,olddir,options={}},*)
WriteCHOutput[{lags__}, options___]:=Block[{i,dirname,olddir,tempoutdir},
(*Time how long the interface takes.*)
CH$time0=AbsoluteTime[];
Print[" - - - FeynRules interface to CalcHep/CompHEP"];
Print[" - - - ",Style["Authors:",Blue]," N. Christensen, C. Duhr"];
Print[" - - - ",Style["Please cite:",Blue]," arXiv:0906.2474"];
olddir=Directory[];

(*Begin numbering for auxiliary fields.*)
CH$AuxiliaryN=0;
(*Begin numbering for constants.*)
CH$CouplingN=0;

(* Set Options *)
CH$PrintWarnings=PrintWarnings/.{options}/.Options[WriteCHOutput];
CH$ModelNumber = ModelNumber /. {options} /. Options[WriteCHOutput];
CH$CompHEP = CompHEP /. {options} /. Options[WriteCHOutput];
CH$MaxExpressionLength = MaxExpressionLength /. {options} /. Options[WriteCHOutput];
CH$LongestSoFar=CH$MaxExpressionLength;
If[CH$PrintWarnings&&CH$MaxExpressionLength>1900,Print[Style["\tWarning!",Red],"  MaxExpressionLength is longer than the CalcHEP gui can handle.  This can be fixed by opening the file c_source/chep_crt/include/syst.h and change '#define STRSIZ 2048' to '#define STRSIZ "<>ToString[CH$MaxExpressionLength+500]<>"' and recompile CH (make clean and then make)."]];
CH$ParticleNameLength = CHParticleNameLength/.{options}/.Options[WriteCHOutput];
CH$ParameterNameLength = ParameterNameLength/.{options}/.Options[WriteCHOutput];
CH$Exclude4Scalars = Exclude4Scalars /.{options}/.Options[WriteCHOutput];
CH$Simplify=CHSimplify/.{options}/.Options[WriteCHOutput];
CH$AutoWidths=CHAutoWidths/.{options}/.Options[WriteCHOutput];
CH$LHASupport=LHASupport/.{options}/.Options[WriteCHOutput];
If[CH$CompHEP,
CH$ParticleNameLength=3;
CH$AutoWidths=False;
CH$LHASuppport=False;
];
CH$Vertices=CHVertices/.{options}/.Options[WriteCHOutput];
CH$MaxCanonicalDimension=MaxCanonicalDimension/.{options}/.Options[WriteCHOutput];
CH$DialogBox=DialogBox/.{options}/.Options[WriteCHOutput];

(* Setting the output directory *)
tempoutdir = Output /. {options} /. Options[WriteCHOutput];
If[tempoutdir =!= Automatic,
   dirname = If[Not[StringQ[tempoutdir]], tempoutdir = ToString[tempoutdir], tempoutdir],
   (*Default value *)
   dirname = StringJoin[StringReplace[M$ModelName, " " -> "-"], "-CH"]
];
If[Not[MemberQ[FileNames[],dirname]],(*Print["Creating Directory ",dirname];*)
CreateDirectory[dirname]];
If[$OperatingSystem==="Windows",SetDirectory[StringJoin[Directory[],"\\",dirname]],SetDirectory[StringJoin[Directory[],"/",dirname]]];
Print[Style["\nWriting",Blue]," files to "<>ToString[Directory[]]<>"."];

WriteCHParticles[];
(*WriteCHVars[CH$MaxExpressionLength];*)
WriteCHExtVars[options];
WriteCHIntVars[options];
WriteCHExtLibs[];
WriteCHVertices[{lags},CH$MaxExpressionLength,options];
FinishCHIntVars[options];

SetDirectory[olddir];

CH$timef=AbsoluteTime[];
Print[Style["\nDone",Blue]," in "<>ToString[Round[(CH$timef-CH$time0)/60,0.01]]<>"min!"];
];


(* ::Subsection::Closed:: *)
(*WriteCHParticles[]*)


PartNameCH[CC[a_]]:=PartName[anti[a]];


WriteCHParticles[]:=Module[{parttemplist,file,j,nColorOctets=0,progress=0,renamedParticles={},dialogString="",aux=""},
Print[Style["\nWriting",Blue]," particles file 'prtcls"<>ToString[CH$ModelNumber]<>".mdl'."];

(*Extract the particles list.*)
parttemplist={};
Do[AppendTo[parttemplist,Flatten[{PartList[[j,2,i]],PartList[[j,1]]}]],{j,1,Length[PartList]},{i,1,Length[PartList[[j,2]]]}];
(*If[$VersionNumber>=6,Print[ProgressIndicator[Dynamic[progress]]]];*)

(*Check to see if there is a PDG code.*)
(*If not, create one.*)
If[CH$PrintWarnings,Do[If[Head[Last[parttemplist[[partkk]]]]===NoPDG,Print[Style["\tWarning:",Purple],"  no PDG code found for ",parttemplist[[partkk,-2]],". Assigned PDG code: ",Identity@@Last[parttemplist[[partkk]]]]],{partkk,Length[parttemplist]}]];

(*Open the particle file and write the initial line.*)
file=OpenWrite["prtcls"<>ToString[CH$ModelNumber]<>".mdl"];
WriteString[file,M$ModelName<>"\n"];
WriteString[file," Particles\n"];
If[CH$CompHEP,
WriteString[file,
" Full  name  | P "<>stringResize["",CH$ParticleNameLength-2]<>"| aP"<>stringResize["",CH$ParticleNameLength-2]<>"|2*spin|"<>stringResize[" mass ",CH$ParameterNameLength]<>"|"<>stringResize["width ",CH$ParameterNameLength]<>"|color|aux|>LaTex(A)<|>LaTeX(A+)   <|\n"],
WriteString[file,
" Full  name  |A  "<>stringResize["",CH$ParticleNameLength-2]<>"|A+ "<>stringResize["",CH$ParticleNameLength-2]<>"| number |2*spin|"<>stringResize[" mass ",CH$ParameterNameLength]<>"|"<>stringResize["width ",CH$ParameterNameLength]<>"|color|aux|>LaTex(A)<|>LaTeX(A+)   <|\n"]
];

(*Cycle through each particle and write it to file.*)
Do[
(*If the particle is a Goldstone Boson or Ghost, skip it.*)
If[parttemplist[[j,13]]===NoGS&&parttemplist[[j,3]]=!=U,

(*Full Name*)
WriteString[file,stringResize[parttemplist[[j,10]],13]];
(*Particle Name*)
(*CH only allows particles with 3 characters (It used to be 2.)*)
(*or 5 characters with a mandatory '~' as the first character.*)
If[(StringLength[parttemplist[[j,1]]]==(CH$ParticleNameLength+1)&&StringTake[parttemplist[[j,1]],1]=!="~")||StringLength[parttemplist[[j,1]]]>CH$ParticleNameLength,
Print["\tRenamed "<>parttemplist[[j,1]]<>" to "<>resizeName[parttemplist[[j,1]]]<>"."];
AppendTo[renamedParticles,{parttemplist[[j,1]],resizeName[parttemplist[[j,1]]]}];
];
WriteString[file,"|"<>resizeName[parttemplist[[j,1]]]];
(*Antiparticle Name*)
(*CH only allows particles with 4 characters (It used to be 2.)*)
(*or 5 characters with a mandatory '~' as the first character.*)
If[(StringLength[parttemplist[[j,1]]]==(CH$ParticleNameLength+1)&&StringTake[parttemplist[[j,1]],1]=!="~")||StringLength[parttemplist[[j,1]]]>CH$ParticleNameLength,
Print["\tRenamed "<>parttemplist[[j,2]]<>" to "<>resizeName[parttemplist[[j,2]]]<>"."];
AppendTo[renamedParticles,{parttemplist[[j,2]],resizeName[parttemplist[[j,2]]]}];
];
WriteString[file,"|"<>resizeName[parttemplist[[j,2]]]];

(*PDG*)
If[!CH$CompHEP,WriteString[file,"|"<>stringResize[ToString[parttemplist[[j,9]]/.NoPDG[pdg_]->pdg],8]]];
(*2x Spin*)
WriteString[file,"|"<>stringResize[parttemplist[[j,3]]/.{S->"0",F->"1",V->"2",R->"3",T->"4"},6]];
(*Mass*)
WriteString[file,"|"<>stringResize[ToString[parttemplist[[j,5]]/.ZERO->0],CH$ParameterNameLength]];
(*Width*)
If[(parttemplist[[j,6]]/.ZERO->0)===0,
	WriteString[file,"|"<>stringResize["0",CH$ParameterNameLength]];,
	If[CH$AutoWidths,
		WriteString[file,"|"<>stringResize["!"<>ToString[parttemplist[[j,6]]],CH$ParameterNameLength]],
		WriteString[file,"|"<>stringResize[ToString[parttemplist[[j,6]]/.ZERO->0],CH$ParameterNameLength]],
		WriteString[file,"|"<>stringResize[ToString[parttemplist[[j,6]]/.ZERO->0],CH$ParameterNameLength]]
	];
];
(*Color*)
WriteString[file,"|"<>stringResize[ToString[parttemplist[[j,7]]/.{S->"1",T->"3",O->"8"}],5]];
If[parttemplist[[j,7]]==O&&parttemplist[[j,3]]=!=F,nColorOctets++];
(*Auxilary*)
WriteString[file,"|"];
aux="";(*Need to add charge and G when Goldstones are present.  Not done yet.*)
aux=ToString[3Q[parttemplist[[j,15]]]];
If[parttemplist[[j,3]]==V&&MemberQ[(parttemplist[[j,15]]===#)&/@parttemplist[[All,13]],True],aux="G"];
If[parttemplist[[j,3]]==V&&(parttemplist[[j,5]]/.ZERO->0)==0,aux="G"];
WriteString[file,stringResize[aux,3]];
(*Particle Latex*)
WriteString[file,"|"<>stringResize[parttemplist[[j,1]],10]];
(*Antiparticle Latex*)
WriteString[file,"|"<>stringResize[parttemplist[[j,2]],14]];
WriteString[file,"|\n"];
];
progress=j/Length[parttemplist];
,{j,1,Length[parttemplist]}];

(*If there are more than one color octets, warn the user.*)
If[CH$PrintWarnings&&nColorOctets>1,
Print[Style["\tWarning!",Red],"  I notice you have more than one color octet."];
Print["\t\tIn CH, 4-point color octet vertices are nontrivial,\n\t\t and must be added with an auxilliary field."];
Print["\t\tPlease refer to the CH manual for these vertices."];
];
Close[file];

(*Ask the user if the particle renamings are ok.*)
If[CH$PrintWarnings&&Length[renamedParticles]>0,
dialogString="CH only allows particle names of up to 2 characters.\n";
dialogString=dialogString<>"Additionally, a '~' can be added as a 3rd character at the beginning of a particle name.\n";
dialogString=dialogString<>"The following renamings were done on the fly:\n";
Do[
dialogString=dialogString<>renamedParticles[[j,1]]<>" \[Rule] "<>renamedParticles[[j,2]]<>"\n";
,{j,1,Length[renamedParticles]}];
dialogString=dialogString<>"Please, make sure that these renamings don't conflict with any other names.\n\n";
dialogString=dialogString<>"Is it ok to make these renamings?";
If[DialogBox===On,If[!ChoiceDialog[dialogString,{"Ok"->True, "Abort"->False}],Abort[]]];
];
];


(* ::Subsection::Closed:: *)
(*CHVars*)


(* ::Subsubsection::Closed:: *)
(*WriteCHExtVars[options]*)


WriteCHExtVars[options___]:=Module[{ParamListTmp,internalFile,externalFile,j,stringTmp},
(* Set Options *)
CH$ModelNumber = ModelNumber /. {options} /. Options[WriteCHOutput];
CH$CompHEP = CompHEP /. {options} /. Options[WriteCHOutput];
CH$MaxExpressionLength = MaxExpressionLength /. {options} /. Options[WriteCHOutput];
CH$LHASupport=LHASupport/.{options}/.Options[WriteCHOutput];

CH$NNoValues=0;
CH$NComplex=0;
If[CH$PrintWarnings,Do[
If[Not[FreeQ[ParamList[[kparam]],NoValue]]&&CH$NNoValues<3,CH$NNoValues++;Print[Style["\tWarning:",Purple]," no value found for parameter ",ToString[ParamList[[kparam,1]]],". Default value 1 assigned."]];
If[CH$NNoValues==3,CH$NNoValues++;Print[Style["\tWarning:",Purple],"  Default value of 1 assigned when no value found.\n\t\tFurther printing suppressed."]];
If[Not[FreeQ[ParamList[[kparam]],Complex]]&&CH$NComplex<3,CH$NComplex++;Print[Style["\tWarning:",Purple]," Complex piece found in parameter ",ToString[ParamList[[kparam,1]]],".  Setting it equal to its real part."]];
If[CH$NComplex==3,CH$NComplex++;Print[Style["\tWarning:",Purple],"  Complex parts of parameters are set to their real part.\n\t\tFurther printing suppressed."]];
,{kparam,Length[ParamList]}]];
ParamListTmp=ParamList//.NoValue[_]->1.0//.Complex[a__,b__]->a;

(**************)
(*Open file.*)
(**************)
Print[Style["\nWriting",Blue]," external parameter file 'vars"<>ToString[CH$ModelNumber]<>".mdl'."];
externalFile=OpenWrite["vars"<>ToString[CH$ModelNumber]<>".mdl"];
WriteString[externalFile,M$ModelName<>"\n"];
If[CH$CompHEP,
WriteString[externalFile," Variables\n"],
WriteString[externalFile," Parameters\n"]
];
WriteString[externalFile,stringResize["  Name ",CH$ParameterNameLength]];
WriteString[externalFile,"| Value       |"<>stringResize["> Comment",73]<>"<|\n"];


(************)
(*ParamList*)
(************)
Do[
(*Strong coupling*)
If[ParamListTmp[[j,2]]==Int&&NameToParameter[ParamListTmp[[j,1]]]===gs,
WriteString[externalFile,stringResize["GG",CH$ParameterNameLength]];
WriteString[externalFile,"|"<>stringResize[ToString[CForm[NumericalValue[gs]]],13]];
WriteString[externalFile,stringResize["|"<>(ParamListTmp[[j,-1]]/.Description->"Description")<>"   This value will be ignored and calculated from alfSMZ as a running parameter.",75]<>"|\n"];
];

If[ParamListTmp[[j,2]]==Ext,
(*Strong coupling*)
If[NameToParameter[ParamListTmp[[j,1]]]===gs,
WriteString[externalFile,stringResize["GG",CH$ParameterNameLength]];
WriteString[externalFile,"|"<>stringResize[ToString[CForm[ParamListTmp[[j,3]]]],13]];
WriteString[externalFile,stringResize["|"<>(ParamListTmp[[j,-1]]/.Description->"Description")<>"   This value will be ignored and calculated from alfSMZ as a running parameter.",75]<>"|\n"];
If[CH$LHASupport===True,
	WriteString[externalFile,stringResize["alfSMZDef",CH$ParameterNameLength]],
	WriteString[externalFile,stringResize["alfSMZ",CH$ParameterNameLength]]
];
WriteString[externalFile,"|"<>stringResize[ToString[CForm[N[ParamListTmp[[j,3]]^2/4/3.14159]]],13]];
WriteString[externalFile,stringResize["|Strong alpha(MZ) used for running.",75]<>"|\n"];
,
(*Not Strong coupling*)
If[CH$PrintWarnings&&StringLength[ToString[ParamListTmp[[j,1]]]]>CH$ParameterNameLength,
Print[Style["\tWarning! ",Red],ToString[ParamListTmp[[j,1]]]," is too long for CalcHep."];
Print["\t\tCutting it down to ",stringResize[ToString[ParamListTmp[[j,1]]],CH$ParameterNameLength],"."];
Print["\t\tPlease note that it will not be cut down in the func.mdl or lgrng.mdl files."];
Print["\t\tThis will break the model.  You should shorten the name in ",MR$currentmodel];
];
If[CH$PrintWarnings&&ToString[ParamListTmp[[j,1]]]=="E",
Print[Style["\tWarning!",Red],"  Parameter 'E' conflicts with the base of the natural logarithm."];
];
If[CH$LHASupport===True,
	WriteString[externalFile,stringResize[ToString[ParamListTmp[[j,1]]/.ee->EE]<>"Def",CH$ParameterNameLength]],
	WriteString[externalFile,stringResize[ToString[ParamListTmp[[j,1]]],CH$ParameterNameLength]]
];
WriteString[externalFile,"|"<>stringResize[ToString[CForm[ParamListTmp[[j,3]]/.ee->EE]],13]];
WriteString[externalFile,stringResize["|"<>ParamListTmp[[j,-1]]/.Description->"Description",75]<>"|\n"];
];
];

,{j,1,Length[ParamListTmp]}];

(**************************************************************************)
(* Remove Goldstone Bosons and Ghosts from Mass and Widths Lists *)
(**************************************************************************)
GPDGList={};Do[
If[PartList[[j,2,k,-1]]=!=NoGS||Head[PartList[[j,1,1]]]===U,AppendTo[GPDGList,PartList[[j,2,k,9]]]];
,{j,1,Length[PartList]},{k,1,Length[PartList[[j,2]]]}];
CHMassList=MassList;
Do[CHMassList=DeleteCases[CHMassList,{{GPDGList[[j]]},__},2],{j,1,Length[GPDGList]}];
CHWidthList=WidthList;
Do[CHWidthList=DeleteCases[CHWidthList,{{GPDGList[[j]]},__},2],{j,1,Length[GPDGList]}];
CHMassList=({#1,KillDoubles[Rest/@#2]}&)@@CHMassList;
CHWidthList=({#1,KillDoubles[Rest/@#2]}&)@@CHWidthList;

(*********************)
(*    Masses       *)
(*********************)
Do[
If[CHMassList[[2,j,2]]=!=Internal,
If[CH$LHASupport===True,
	WriteString[externalFile,stringResize[ToString[CHMassList[[2,j,1]]]<>"Def",CH$ParameterNameLength]],
	WriteString[externalFile,stringResize[ToString[CHMassList[[2,j,1]]],CH$ParameterNameLength]]
];
WriteString[externalFile,stringResize["|"<>ToString[CForm[CHMassList[[2,j,2]]/.NoValue[1]->0]],14]];
WriteString[externalFile,stringResize["|Mass of "<>ToString[MassToPart[CHMassList[[2,j,1]]]]<>".",75]<>"|\n"];
];
,{j,1,Length[CHMassList[[2]]]}];
(*********************)
(*    Widths       *)
(*********************)
Do[
If[CHWidthList[[2,j,2]]=!=Internal,
Which[
CH$LHASupport&&CH$AutoWidths,
	WriteString[externalFile,stringResize["%"<>ToString[CHWidthList[[2,j,1]]]<>"Def",CH$ParameterNameLength]],
CH$LHASupport,
	WriteString[externalFile,stringResize[ToString[CHWidthList[[2,j,1]]]<>"Def",CH$ParameterNameLength]],
CH$AutoWidths,
	WriteString[externalFile,stringResize["%"<>ToString[CHWidthList[[2,j,1]]],CH$ParameterNameLength]],
1==1,
	WriteString[externalFile,stringResize[ToString[CHWidthList[[2,j,1]]],CH$ParameterNameLength]]
];
WriteString[externalFile,stringResize["|"<>ToString[CForm[CHWidthList[[2,j,2]]/.NoValue[1]->0]],14]];
WriteString[externalFile,stringResize["|Width of "<>ToString[WidthToPart[CHWidthList[[2,j,1]]]]<>".",75]<>"|\n"];
];
,{j,1,Length[CHWidthList[[2]]]}];
(*E*)
WriteString[externalFile,stringResize["E",CH$ParameterNameLength]];
WriteString[externalFile,stringResize["|2.718281828459045",14]];
WriteString[externalFile,stringResize["|The base of the natural logarithm.",75]<>"|\n"];
(*Pi*)
WriteString[externalFile,stringResize["Pi",CH$ParameterNameLength]];
WriteString[externalFile,stringResize["|3.141592653589793",14]];
WriteString[externalFile,stringResize["|The circumference of a circle divided by the diameter.",75]<>"|\n"];

(**************)
(*Close file.*)
(**************)
Close[externalFile];


(**************)
(*LHA file    *)
(**************)
If[CH$LHASupport===True,
	Print[Style["\nWriting",Blue]," external LHA parameter file 'vars"<>ToString[CH$ModelNumber]<>".lha'.\n"];
	WriteLHAFile[Output->"vars"<>ToString[CH$ModelNumber]<>".lha"];
];
];


(* ::Subsubsection::Closed:: *)
(*ReadCHExtVars[options]*)


ReadCHExtVars[options___]:=Module[{file,fileName,var,value,replList={}},
CH$ModelNumber = ModelNumber /. {options} /. Options[WriteCHOutput];
CH$CompHEP = CompHEP /. {options} /. Options[WriteCHOutput];
CH$MaxExpressionLength = MaxExpressionLength /. {options} /. Options[WriteCHOutput];
CH$LHASupport=LHASupport/.{options}/.Options[WriteCHOutput];

If[CH$LHASupport,Print[Style["LHA support does not work for ReadCHExtVars.  Please use default LHA reeader.",Red]]];

(*Open file*)
fileName=Input/.options/.Input->"vars"<>ToString[CH$ModelNumber]<>".mdl";
Print["Reading external parameter file '"<>fileName<>"'."];
file=OpenRead[fileName];
Read[file,String];
Read[file,String];
Read[file,String];
var=Read[file,Word,WordSeparators->{" ","\[Backslash]t","|"}];
While[var=!=EndOfFile,
value=Read[file,Word,WordSeparators->{" ","\[Backslash]t","|"}];
AppendTo[replList,ToExpression[var]->ToExpression[value]];
Read[file,String];
var=Read[file,Word,WordSeparators->{" ","\[Backslash]t","|"}];
];
Close[file];

(*Update parameters*)
(*Print[replList];*)
Print["Updating parameters."];
UpdateParameters[replList/.List[a__]->a];
Print["Done"];
];


(* ::Subsubsection::Closed:: *)
(*WriteCHIntVars[options]*)


WriteCHExtLHAVar[var_,file_,LHASupport_,blk_]:=Module[{stringTmp},
	If[LHASupport===True,
		WriteString[file,stringResize[ToString[var[[2,1]]],CH$ParameterNameLength]<>"|"],
		WriteString[file,stringResize["%"<>ToString[var[[2,1]]],CH$ParameterNameLength]<>"|"]
	];
	stringTmp="if(slhaFound,if(slhaValExists(\""<>ToString[blk/.NoBlockName[FRBlock_]->FRBlock]<>"\","<>ToString[Length[var[[1]]]]<>","<>Apply[StringJoin,Riffle[ToString[#]&/@var[[1]],","]]<>"),slhaVal(\""<>ToString[blk/.NoBlockName[FRBlock_]->FRBlock]<>"\",0,"<>ToString[Length[var[[1]]]]<>","<>Apply[StringJoin,Riffle[ToString[#]&/@var[[1]],","]]<>"),"<>ToString[var[[2,1]]]<>"Def),"<>ToString[var[[2,1]]]<>"Def)";
	CheckSizeOfExpression["       ",stringTmp,CH$MaxExpressionLength];
	WriteString[file,stringResize[stringTmp,CH$LongestSoFar+10]];
	If[CH$CompHEP,WriteString[file," |"],WriteString[file," %"]];
	WriteString[file,stringResize[var[[2,-1]],50]<>"\n"];
];


WriteCHLHAMass[m_,file_,LHASupport_]:=Module[{stringTmp},
	If[m[[3]]=!=Internal,
		If[LHASupport===True,
			WriteString[file,stringResize[ToString[m[[2]]],CH$ParameterNameLength]<>"|"],
			WriteString[file,stringResize["%"<>ToString[m[[2]]],CH$ParameterNameLength]<>"|"]
		];
		stringTmp="if(slhaFound,if(slhaValExists(\"MASS\",1,"<>ToString[Abs[m[[1,1]]/.NoPDG[a_]->a]]<>"),slhaVal(\"MASS\",0,1,"<>ToString[Abs[m[[1,1]]/.NoPDG[a_]->a]]<>"),"<>ToString[m[[2]]]<>"Def),"<>ToString[m[[2]]]<>"Def)";
		CheckSizeOfExpression["       ",stringTmp,CH$MaxExpressionLength];
		WriteString[file,stringResize[stringTmp,CH$LongestSoFar+10]];
		If[CH$CompHEP,WriteString[file," |"],WriteString[file," %"]];
		WriteString[file,stringResize["Mass of "<>ToString[MassToPart[m[[2]]]]<>".",50]<>"\n"];		
	];
];


WriteCHLHAWidth[m_,file_,LHASupport_]:=Module[{stringTmp},
	If[m[[3]]=!=Internal,
		If[CH$AutoWidths||LHASupport=!=True,
			WriteString[file,stringResize["%"<>ToString[m[[2]]],CH$ParameterNameLength]<>"|"],
			WriteString[file,stringResize[ToString[m[[2]]],CH$ParameterNameLength]<>"|"]
		];
		stringTmp="if(slhaFound,if(1+slhaDecayExists("<>ToString[Abs[m[[1,1]]/.NoPDG[a_]->a]]<>"),slhaWidth("<>ToString[Abs[m[[1,1]]/.NoPDG[a_]->a]]<>"),"<>ToString[m[[2]]]<>"Def),"<>ToString[m[[2]]]<>"Def)";
		CheckSizeOfExpression["       ",stringTmp,CH$MaxExpressionLength];
		WriteString[file,stringResize[stringTmp,CH$LongestSoFar+10]];
		If[CH$CompHEP,WriteString[file," |"],WriteString[file," %"]];
		WriteString[file,stringResize["Width of "<>ToString[WidthToPart[m[[2]]]]<>".",50]<>"\n"];		
	];
];


WriteCHIntVars[options___]:=Module[{ParamListTmp,internalFile,externalFile,j,stringTmp,GPDGList,CHMassList,CHWidthList},
	CH$ModelNumber = ModelNumber /. {options} /. Options[WriteCHOutput];
	CH$CompHEP = CompHEP /. {options} /. Options[WriteCHOutput];
	CH$MaxExpressionLength = MaxExpressionLength /. {options} /. Options[WriteCHOutput];
	CH$LHASupport=LHASupport/.{options}/.Options[WriteCHOutput];
	CH$LongestSoFar=CH$MaxExpressionLength;

	(*Set Complex parameters equal to real part.*)
	(*This should be improved.*)
	ParamListTmp=ParamList//.NoValue[_]->1.0//.Complex[a__,b__]->a;
	
	(*Print message to user.*)
	Print[Style["\nWriting",Blue]," internal parameter file 'func"<>ToString[CH$ModelNumber]<>".mdl'."];
	internalFile=OpenWrite["func"<>ToString[CH$ModelNumber]<>".mdl2"];
	
	(**********************************************)
	(*   Write external parameters to func.mdl2   *)
	(**********************************************)
	If[CH$LHASupport===True,
		WriteString[internalFile,stringResize["*slhaFound",CH$ParameterNameLength]<>"|"],
		WriteString[internalFile,stringResize["%*slhaFound",CH$ParameterNameLength]<>"|"]
	];
	WriteString[internalFile,stringResize["1+access(\"vars"<>ToString[CH$ModelNumber]<>".lha\", 0)",CH$LongestSoFar+10]];
	If[CH$CompHEP,WriteString[internalFile," |"],WriteString[internalFile," %"]];
	WriteString[internalFile,"Check whether LHA file is present.\n"];
	If[CH$LHASupport===True,
		WriteString[internalFile,stringResize["*rdSLHA",CH$ParameterNameLength]<>"|"],
		WriteString[internalFile,stringResize["%*rdSLHA",CH$ParameterNameLength]<>"|"]
	];
	WriteString[internalFile,stringResize["if(slhaFound,slhaRead(\"vars"<>ToString[CH$ModelNumber]<>".lha\", 0),-1)",CH$LongestSoFar+10]];
	If[CH$CompHEP,WriteString[internalFile," |"],WriteString[internalFile," %"]];
	WriteString[internalFile,"Read LHA file.\n"];
	Do[
		WriteCHExtLHAVar[#,internalFile,CH$LHASupport,EParamList[[j,1]]]&/@(EParamList[[j,2]]/.ee->EE);
	,{j,1,Length[EParamList]}];

	(**************************************************************************)
	(* Remove Goldstone Bosons and Ghosts from Mass and Widths Lists          *)
    (* Write to LHA reader section.                                           *)
	(**************************************************************************)
	GPDGList={};
	Do[
		If[PartList[[j,2,k,-1]]=!=NoGS||Head[PartList[[j,1,1]]]===U,AppendTo[GPDGList,PartList[[j,2,k,9]]]];
	,{j,1,Length[PartList]},{k,1,Length[PartList[[j,2]]]}];
	CHMassList=MassList;
	Do[CHMassList=DeleteCases[CHMassList,{{GPDGList[[j]]},__},2],{j,1,Length[GPDGList]}];
	CHWidthList=WidthList;
	Do[CHWidthList=DeleteCases[CHWidthList,{{GPDGList[[j]]},__},2],{j,1,Length[GPDGList]}];
	WriteCHLHAMass[#,internalFile,CH$LHASupport]&/@CHMassList[[2]];
	WriteCHLHAWidth[#,internalFile,CH$LHASupport]&/@CHWidthList[[2]];

	(**********************************************)
	(*   Write internal parameters to func.mdl2  *)
	(**********************************************)
	Do[
		If[ParamListTmp[[j,2]]==Int&&NameToParameter[ParamListTmp[[j,1]]]=!=gs,
			WriteString[internalFile,stringResize[ToString[ParamListTmp[[j,1]]/.ee->"EE"],CH$ParameterNameLength]<>"|"];
			stringTmp=StringReplace[ToString[CForm[ParamListTmp[[j,3]]/.ee->"EE"//.CFunctionReplacements0]//.CFunctionReplacements],{" "->"","\""->""}];
			CheckSizeOfExpression[ToString[ParamListTmp[[j,1]]],stringTmp,CH$MaxExpressionLength];
			WriteString[internalFile,stringResize[stringTmp,CH$LongestSoFar+10]];
			If[CH$CompHEP,WriteString[internalFile," |"],WriteString[internalFile," % "]];
			WriteString[internalFile,stringResize[ParamListTmp[[j,-1]]/.Description->"Description",50]<>"\n"];
		];
	,{j,1,Length[ParamListTmp]}];
	Close[internalFile];
    (*Run["cp func"<>ToString[CH$ModelNumber]<>".mdl2 func"<>ToString[CH$ModelNumber]<>".mdl3"];*)
		
];





WriteCHIndexSumVars[options___]:=Module[{ParamListTmp,internalFile,j,stringTmp},
	CH$ModelNumber = ModelNumber /. {options} /. Options[WriteCHOutput];
	CH$CompHEP = CompHEP /. {options} /. Options[WriteCHOutput];

	(*Set Complex parameters equal to real part.*)
	(*This should be improved.*)
	ParamListTmp=FR$AbbIndexSumExpanded//.Complex[a__,b__]->a;
	
	(*Print message to user.*)
	(*Print[Style["\nAppending",Blue]," internal parameter file 'func"<>ToString[CH$ModelNumber]<>".mdl'."];*)
	internalFile=OpenAppend["func"<>ToString[CH$ModelNumber]<>".mdl2"];
	
	(**********************************************)
	(*   Write ISUM parameters to func.mdl2       *)
	(**********************************************)
	Do[
		WriteString[internalFile,stringResize[ToString[ParamListTmp[[j,1]]],CH$ParameterNameLength]<>"|"];
		stringTmp=StringReplace[ToString[CForm[ParamListTmp[[j,2]]//.CFunctionReplacements0]//.CFunctionReplacements],{" "->"","\""->""}];
		CheckSizeOfExpression[ToString[ParamListTmp[[j,1]]],stringTmp,CH$MaxExpressionLength];
		WriteString[internalFile,stringResize[stringTmp,CH$LongestSoFar+10]];
		If[CH$CompHEP,WriteString[internalFile," |"],WriteString[internalFile," % "]];
		WriteString[internalFile,stringResize["Abbreviation used in optimizatized FeynRules output.",50]<>"\n"];
	,{j,1,Length[ParamListTmp]}];

	Close[internalFile];
];


FinishCHIntVars[options___]:=Module[{internalFile},
	CH$ModelNumber = ModelNumber /. {options} /. Options[WriteCHOutput];
	CH$CompHEP = CompHEP /. {options} /. Options[WriteCHOutput];
	CH$MaxExpressionLength = MaxExpressionLength /. {options} /. Options[WriteCHOutput];

	(*Print message to user.*)
	Print[Style["Finishing",Blue]," internal parameter file 'func"<>ToString[CH$ModelNumber]<>".mdl'."];
	If[CH$PrintWarnings&&CH$LongestSoFar>1900,Print[Style["\tWarning!",Red],"  MaxExpressionLength is longer than the CalcHEP gui can handle.  This can be fixed by opening the file c_source/chep_crt/include/syst.h and change '#define STRSIZ 2048' to '#define STRSIZ "<>ToString[CH$LongestSoFar+500]<>"' and recompile CH (make clean and then make)."]];

	(**************************************************)
	(*   Write first lines of func.mdl in func.mdl1   *)
	(**************************************************)
	internalFile=OpenWrite["func"<>ToString[CH$ModelNumber]<>".mdl1"];
	WriteString[internalFile,M$ModelName<>"\n"];
	WriteString[internalFile," Constraints\n"];
	If[CH$CompHEP,
		WriteString[internalFile,stringResize["  Name ",CH$ParameterNameLength]<>"|"<>stringResize["> Expression",CH$LongestSoFar]<>"<|> Comment"<>stringResize["",50]<>"<|\n"],
		WriteString[internalFile,stringResize["  Name ",CH$ParameterNameLength]<>"|> Expression"<>stringResize["",CH$LongestSoFar-3]<>"<|\n"];
	];
	Close[internalFile];
	(****************************)
	(*   Cat the two together  *)
	(****************************)
	Run["cat func"<>ToString[CH$ModelNumber]<>".mdl1 func"<>ToString[CH$ModelNumber]<>".mdl2 > func"<>ToString[CH$ModelNumber]<>".mdl"];
	Run["rm func"<>ToString[CH$ModelNumber]<>".mdl1 func"<>ToString[CH$ModelNumber]<>".mdl2"];
];


(* ::Subsection::Closed:: *)
(*WriteCHExtLibs[]*)


WriteCHExtLibs[]:=Module[{file,j},
Print[Style["\nWriting",Blue]," external library file 'extlib"<>ToString[CH$ModelNumber]<>".mdl'."];

(*Open the extlib file and write the initial line.*)
file=OpenWrite["extlib"<>ToString[CH$ModelNumber]<>".mdl"];
WriteString[file,M$ModelName<>"\n"];
WriteString[file,"Libraries\n"];
WriteString[file,"External libraries  and citation                                      <|\n"];

(*Write FR info*)
WriteString[file,"%This model file was generated by FeynRules version "<>FR$VersionNumber<>".\n"];
WriteString[file,"%Please cite:\n%   arXiv:0806.4194\n%   arXiv:0906.2474\n"];
WriteString[file,"%Further information can be found at:\n%   http://feynrules.phys.ucl.ac.be\n%\n"];

(*Write model info*)
(*Authors*)
If[Length[MR$Authors]>=1&&MR$Authors[[1]]=!=MR$Null,
	WriteString[file,"%This model implementation was created by:\n"];
	Do[WriteString[file,"%   "<>MR$Authors[[j]]<>"\n"],{j,1,Length[MR$Authors]}];
];
(*Emails*)
If[Length[MR$Emails]>=1&&MR$Emails[[1]]=!=MR$Null,
	WriteString[file,"%Emails: \n"];
	Do[WriteString[file,"%   "<>MR$Emails[[j]]<>"\n"],{j,1,Length[MR$Emails]}];
];
(*Version*)
WriteString[file,"%Model version: "<>MR$Version<>"\n"];
(*Date*)
WriteString[file,"%Date: "<>MR$Date<>"\n"];
(*References*)
If[Length[MR$References]>=1&&MR$References[[1]]=!=MR$Null,
	WriteString[file,"%Please cite: \n"];
	Do[WriteString[file,"%   "<>MR$References[[j]]<>"\n"],{j,1,Length[MR$References]}];
];
(*URLs*)
If[Length[MR$URLs]>=1&&MR$URLs[[1]]=!=MR$Null,
	WriteString[file,"%Further information can be found at: \n"];
	Do[WriteString[file,"%   "<>MR$URLs[[j]]<>"\n"],{j,1,Length[MR$URLs]}];
];

(*access*)
WriteString[file,"\n%This prototype is used when LHA support is turned on.\n"];
WriteString[file,"extern int access(const char *pathname, int mode);\n"];

Close[file];
];


(* ::Subsection::Closed:: *)
(*WriteCHVertices[lags,maxLength,options]*)


(* ::Subsubsection::Closed:: *)
(*WriteCHVertices[lags,maxLength,options]*)


WriteCHVertices[{lags__},maxLength_,options___]:=Module[{couplings={},verts,vertListList={},CHVertexArray,PartListTmp,colorOctets,ClassNameToNameReplacements,lgrngFile,funcFile,partFile,varFile,j,k,defname,progress=0,MaxLength,conservedqns,CHMaxParticles,vrtStringList,vrtParallel,vrtLength},
Print[""];
ClassNameToNameReplacements=CreateClassNameToParticleNameReplacementRule[];

(*Generate Feynman rules*)
Vertices["CHLtot"]={};
Which[CH$Vertices===Automatic,
FR$AbbIndexSumExpanded={};
CHMaxParticles=MaxParticles/.{options}/.Options[WriteCHOutput];
Vertices["CHLtot"] = FlavorExpansion[FeynmanRules[{lags},FlavorExpand -> FR$AutoFlavorExpand, ConservedQuantumNumbers -> False, ScreenOutput -> False,MaxParticles->CHMaxParticles,Exclude4Scalars->CH$Exclude4Scalars]];

conservedqns = ConservedQuantumNumbers /. {options} /. Options[WriteCHOutput];
If[(conservedqns =!= {}) && (conservedqns =!= False),
   ConserveQN[#, conservedqns]& /@ (((#1&)@@@#&)/@ Vertices["CHLtot"][[All,1]])];
,1==1,
Vertices["CHLtot"]=CH$Vertices;
];
WriteCHIndexSumVars[options];
verts=VerticesReplaceNames[Vertices["CHLtot"]];

(*Flatten particle list*)
PartListTmp=Flatten[PartList[[All,2]],1];


Print[Style["\nWriting",Blue]," vertices file 'lgrng"<>ToString[CH$ModelNumber]<>".mdl'\nand appending variable file 'func"<>ToString[CH$ModelNumber]<>".mdl', particle file 'prtcls"<>ToString[CH$ModelNumber]<>".mdl' and variable file 'vars"<>ToString[CH$ModelNumber]<>".mdl'."];
(*If[$VersionNumber>=6,Print[ProgressIndicator[Dynamic[progress]]]];*)
(*  Generate vertex strings  *)
If[Global`FR$Parallelize,
	DistributeDefinitions[CH$LongestSoFar];
	vrtParallel=ParallelSubmit[GetCHVertexString[#,maxLength]]&/@ MapIndexed[{#2[[1]],#1}&,verts];
	vrtStringList=WaitAll[vrtParallel];
,
	vrtStringList=GetCHVertexString[#,maxLength]&/@ MapIndexed[{#2[[1]],#1}&,verts];
];
(*  Wrirte vertices to file.   *)
lgrngFile=OpenWrite["lgrng"<>ToString[CH$ModelNumber]<>".mdl2"];
funcFile=OpenAppend["func"<>ToString[CH$ModelNumber]<>".mdl2"];
partFile=OpenAppend["prtcls"<>ToString[CH$ModelNumber]<>".mdl"];
varFile=OpenAppend["vars"<>ToString[CH$ModelNumber]<>".mdl"];
MaxLength=1;
Do[
	MaxLength=Max[MaxLength,StringLength[vrtStringList[[j,1]]]];
	WriteString[lgrngFile,vrtStringList[[j,1]]];
	Do[
		vrtLength=StringLength[vrtStringList[[j,2,k]]];
		If[vrtLength>CH$LongestSoFar,CH$LongestSoFar=vrtLength];
		WriteString[funcFile,vrtStringList[[j,2,k]]];
	,{k,1,Length[vrtStringList[[j,2]]]}];
	If[Length[vrtStringList[[j]]]==4,
		WriteString[partFile,vrtStringList[[j,3]]];
		WriteString[varFile,vrtStringList[[j,4]]];
	];
	progress=j/Length[vrtStringList];
,{j,1,Length[vrtStringList]}];
	(*Do[
		CHVertexArray=CHVertex[verts[[j]],j,maxLength];
		MaxLength=Max[MaxLength,StringLength[CHVertexArray[[1]]]];
		WriteString[lgrngFile,CHVertexArray[[1]]];
		Do[WriteString[funcFile,CHVertexArray[[2,k]]],{k,1,Length[CHVertexArray[[2]]]}];
		If[Length[CHVertexArray]==4,
			WriteString[partFile,CHVertexArray[[3]]];
			WriteString[varFile,CHVertexArray[[4]]];
		];
		progress=j/Length[verts];
	,{j,1,Length[verts]}];*)
Close[lgrngFile];
Close[funcFile];
Close[partFile];
Close[varFile];

(*Write first lines of lgrng.mdl*)
file=OpenWrite["lgrng"<>ToString[CH$ModelNumber]<>".mdl1"];
WriteString[file,M$ModelName<>"\n"];
If[CH$CompHEP,
WriteString[file," Lagrangian\n"];
WriteString[file,stringResize["P1   "<>stringResize["",CH$ParticleNameLength-2]<>"|P2   "<>stringResize["",CH$ParticleNameLength-2]<>"|P3   "<>stringResize["",CH$ParticleNameLength-2]<>"|P4   "<>stringResize["",CH$ParticleNameLength-2]<>"|>         Factor              <|>   dLagrangian/ dA(p1) dA(p2) dA(p3)",MaxLength]<>"<|\n"];
,
WriteString[file," Vertices\n"];
WriteString[file,stringResize["A1   "<>stringResize["",CH$ParticleNameLength-2]<>"|A2   "<>stringResize["",CH$ParticleNameLength-2]<>"|A3   "<>stringResize["",CH$ParticleNameLength-2]<>"|A4   "<>stringResize["",CH$ParticleNameLength-2]<>"|>         Factor              <|>  Lorentz part",MaxLength]<>"<|\n"];
];
Close[file];

(*Cat the two together*)
Print[Style["\nFinishing",Blue]," vertices file 'lgrng"<>ToString[CH$ModelNumber]<>".mdl'."];
Run["cat lgrng"<>ToString[CH$ModelNumber]<>".mdl1 lgrng"<>ToString[CH$ModelNumber]<>".mdl2 > lgrng"<>ToString[CH$ModelNumber]<>".mdl"];
Run["rm lgrng"<>ToString[CH$ModelNumber]<>".mdl1 lgrng"<>ToString[CH$ModelNumber]<>".mdl2"];

];


(* ::Subsubsection::Closed:: *)
(*GetCHVertexString[vert, maxLength]*)


GetCHVertexString[vert_,maxLength_]:=Module[{strList={}},
	CHVertex[vert[[2]],vert[[1]],maxLength]
];


(* ::Subsubsection::Closed:: *)
(*VerticesReplaceNames[vertices]*)
(*Replaces the Class names with particle/antiparticle names.*)


VerticesReplaceNames[vertices_]:=Module[{newVertices,j,k},
newVertices=vertices;
Do[
newVertices[[j,1,k,1]]=PartNameCH[vertices[[j,1,k,1]]];
,{j,1,Length[vertices]},{k,1,Length[vertices[[j,1]]]}];

newVertices
];


(* ::Subsubsection::Closed:: *)
(*CHVertex[vertex, n, maxLength]*)


CHVertex[vertex_,n_,maxLength_]:=Module[{vertArray={"",{""}},vertString="",vertType="",colorType=S^4,vertexTmp=0,implemented=False,fermionPower=0},
(*Find out what type of vertex this is.*)
Which[
Length[vertex[[1]]]===3,
vertType=ParticleSpin[vertex[[1,1,1]]]ParticleSpin[vertex[[1,2,1]]]ParticleSpin[vertex[[1,3,1]]],
Length[vertex[[1]]]===4,
vertType=ParticleSpin[vertex[[1,1,1]]]ParticleSpin[vertex[[1,2,1]]]ParticleSpin[vertex[[1,3,1]]]ParticleSpin[vertex[[1,4,1]]]
];
(*Print[vertType];*)
Which[
Length[vertex[[1]]]===3,
colorType=ParticleColor[vertex[[1,1,1]]]ParticleColor[vertex[[1,2,1]]]ParticleColor[vertex[[1,3,1]]],
Length[vertex[[1]]]===4,
colorType=ParticleColor[vertex[[1,1,1]]]ParticleColor[vertex[[1,2,1]]]ParticleColor[vertex[[1,3,1]]]ParticleColor[vertex[[1,4,1]]]
];
(*Print[colorType];*)



(*Count the number of fermions.*)
(*If not 0 or 2, warn the user.*)
(*If 2, reorder the fermions.*)
fermionPower=vertType/.{a___*F^p_*R^q_->p+q,a___*F*R^q_->1+q,a___*F^p_*R->p+1,a___*F*R->2,F^p_*R^q_->p+q,F*R^q_->1+q,F^p_*R->p+1,F*R->2,a___*F^p_->p,F^p_->p,a___*F->1,F->1,a___*R^p_->p,R^p_->p,a___*R->1,R->1,a__->0};
(*Print[fermionPower];*)
If[
CH$PrintWarnings&&fermionPower==1||fermionPower==3||fermionPower==5,
Print[Style["\tWarning!",Red],"  The following vertex has an odd number of fermions:"];
Print["\t\t",vertex];
];
If[
CH$PrintWarnings&&fermionPower==4,
Print[Style["\tWarning!",Red],"  4 fermion vertices are not allowed in CH."];
Print["\t\tThey must be implemented via an auxilliary field."];
Print["\t\tYou can add this vertex by hand."];
Print["\t\t",vertex];
];


(*Multiply vertex by -i*)
(*CalcHEP's definition of f has an extra factor of i.*)
(*Trade gs for GG*)
vertexTmp=vertex;(*Print[vertexTmp];*)
vertexTmp[[2]]=-I*vertexTmp[[2]]//.{gs->GG,G->GG,ee->EE}/.{f[Index[Gluon,a_],Index[Gluon,b_],Index[Gluon,c_]]->I f[Index[Gluon,a],Index[Gluon,b],Index[Gluon,c]]};
(*Print[vertexTmp];*)

(*Change to CH specific Lorentz Chains.*)
Which[
fermionPower==2,
(*Print[vertexTmp];*)
vertexTmp=ReorderFermions[vertexTmp];
(*Print[vertexTmp];*),
fermionPower==0,
(*Print[vertexTmp];*)
vertexTmp[[2]]=CHLoChain[1]vertexTmp[[2]];(*Added to minimize the size of vertices with no Lorentz structure 12/2/09.*)
vertexTmp=TensToCHLoChain[vertexTmp];
];

(*Print[vertex,"\nvertType=",vertType,"\tcolorType=",colorType];*)
(*Begin the vertex string.*)
Which[
(*glue-glue-squark-squark*)
Length[vertex[[1]]]===4&&fermionPower===0&&colorType===O^2T^2&&vertType===V^2S^2&&
((vertex[[1,1,1]]==vertex[[1,2,1]]&&vertex[[1,3,1]]==PartName[anti[PartSymbol[vertex[[1,4,1]]]]])||
(vertex[[1,1,1]]==vertex[[1,3,1]]&&vertex[[1,2,1]]==PartName[anti[PartSymbol[vertex[[1,4,1]]]]])||
(vertex[[1,1,1]]==vertex[[1,4,1]]&&vertex[[1,2,1]]==PartName[anti[PartSymbol[vertex[[1,3,1]]]]])||
(vertex[[1,1,1]]==PartName[anti[PartSymbol[vertex[[1,2,1]]]]]&&vertex[[1,3,1]]==vertex[[1,4,1]])||
(vertex[[1,1,1]]==PartName[anti[PartSymbol[vertex[[1,3,1]]]]]&&vertex[[1,2,1]]==vertex[[1,4,1]])||
(vertex[[1,1,1]]==PartName[anti[PartSymbol[vertex[[1,4,1]]]]]&&vertex[[1,2,1]]==vertex[[1,3,1]])),
vertString="";,
Length[vertex[[1]]]===3,
vertString=stringResize[resizeName[vertexTmp[[1,1,1]]],CH$ParticleNameLength+3]<>
stringResize["|"<>resizeName[vertexTmp[[1,2,1]]],CH$ParticleNameLength+4]<>
stringResize["|"<>resizeName[vertexTmp[[1,3,1]]],CH$ParticleNameLength+4]<>
stringResize["|",CH$ParticleNameLength+4]<>"|";,
Length[vertex[[1]]]===4&&colorType=!=O^4,
vertString=stringResize[resizeName[vertexTmp[[1,1,1]]],CH$ParticleNameLength+3]<>
stringResize["|"<>resizeName[vertexTmp[[1,2,1]]],CH$ParticleNameLength+4]<>
stringResize["|"<>resizeName[vertexTmp[[1,3,1]]],CH$ParticleNameLength+4]<>
stringResize["|"<>resizeName[vertexTmp[[1,4,1]]],CH$ParticleNameLength+4]<>"|";,
(*4 Gluon Vertex*)
Length[vertex[[1]]]===4&&colorType==O^4&&vertType==V^4&&vertex[[1,1,1]]==vertex[[1,2,1]]==vertex[[1,3,1]]==vertex[[1,4,1]],
vertString=stringResize[resizeName[vertexTmp[[1,1,1]]],CH$ParticleNameLength+3]<>
stringResize["|"<>resizeName[vertexTmp[[1,2,1]]],CH$ParticleNameLength+4]<>
stringResize["|"<>StringReplace[resizeName[vertexTmp[[1,3,1]]]," "->""]<>".t",CH$ParticleNameLength+4]<>
stringResize["|",CH$ParticleNameLength+4]<>"|";
];
(*Print[Length[vertex[[1]]]];
Print[colorType];
Print[vertString];*)


(*Write the rest of the vertex string.*)(*Print["CH$CouplingN=",CH$CouplingN];*)
Which[
(*Too many particles in vertex.*)
Length[vertex[[1]]]>4,
implemented=False;,
(*Fermion Vertex*)
fermionPower==2,
implemented=True;
vertArray=CHFermionVertex[vertexTmp,n,maxLength];,
(*Other Vertices*)
fermionPower==0&&colorType=!=O^4&&colorType=!=O^3 T&&colorType=!=O^2 T^2&&colorType=!=O T^3&&colorType=!=T^4,
implemented=True;
vertArray=CHNonFermionVertex[vertexTmp,n,maxLength];,
(*4 Gluon Vertex*)
Length[vertex[[1]]]===4&&colorType==O^4&&vertType==V^4&&vertex[[1,1,1]]==vertex[[1,2,1]]==vertex[[1,3,1]]==vertex[[1,4,1]],
implemented=True;
vertArray=CH4GluonVertex[vertexTmp,n,maxLength];,
(*glue-glue-squark-squark*)
Length[vertex[[1]]]===4&&fermionPower==0&&colorType==O^2T^2&&vertType==V^2S^2&&
((vertex[[1,1,1]]==vertex[[1,2,1]]&&vertex[[1,3,1]]==PartName[anti[PartSymbol[vertex[[1,4,1]]]]])||
(vertex[[1,1,1]]==vertex[[1,3,1]]&&vertex[[1,2,1]]==PartName[anti[PartSymbol[vertex[[1,4,1]]]]])||
(vertex[[1,1,1]]==vertex[[1,4,1]]&&vertex[[1,2,1]]==PartName[anti[PartSymbol[vertex[[1,3,1]]]]])||
(vertex[[1,1,1]]==PartName[anti[PartSymbol[vertex[[1,2,1]]]]]&&vertex[[1,3,1]]==vertex[[1,4,1]])||
(vertex[[1,1,1]]==PartName[anti[PartSymbol[vertex[[1,3,1]]]]]&&vertex[[1,2,1]]==vertex[[1,4,1]])||
(vertex[[1,1,1]]==PartName[anti[PartSymbol[vertex[[1,4,1]]]]]&&vertex[[1,2,1]]==vertex[[1,3,1]])),
vertArray=CHGlueGlueSquarkSquark[vertexTmp,n,maxLength];
implemented=True;
];(*Print["\tCH$CouplingN=",CH$CouplingN];*)
vertArray[[1]]=vertString<>vertArray[[1]];
(*Print[vertArray];
Print[];Print[];Print[];Print[];*)


(*Warn the user if the vertex is not implemented.*)
If[!implemented,
If[CH$PrintWarnings,
Print[Style["\tWarning!",Red],"  The following vertex is not implemented in FeynRules->CH yet."];
Print["\t\t",vertex[[1]]];
Print["\t\tYou can add this vertex by hand after importing into CalcHEP."];
Print["\t\tYou may also wish to help implement it in FeynRules->CH."];
(*Print["fermionPower=",fermionPower,"\tcolorType=",colorType,"\tvertType=",vertType];*)
];
vertArray={"",{""}}
];


(*Print[vertArray];*)
vertArray
];


(* ::Subsubsection::Closed:: *)
(*CH4GluonVertex[vertex, n, maxLength]*)


CH4GluonVertex[vertex_,n_,maxLength_]:=Module[{term1423,constants,vertexStuff,vertString="",couplString=""},
(*Determine the overall consant in front of the g14g23 term.*)
term1423=Expand[ vertex[[2]]]/.ParamRules/.{
	b__+a_*CHLoChain[___,ME[Index[Lorentz,Ext[1]],Index[Lorentz,Ext[4]]],___,ME[Index[Lorentz,Ext[2]],Index[Lorentz,Ext[3]]],___]->a,
	b__+a_*CHLoChain[___,ME[Index[Lorentz,Ext[2]],Index[Lorentz,Ext[3]]],___,ME[Index[Lorentz,Ext[1]],Index[Lorentz,Ext[4]]],___]->a
};
(*Remove the color structure.*)
constants=term1423//.{
f_[Index[Gluon,_],__]->1,
f_[Index[Colour,_],__]->1,
CHLoChain[GG]->1
};
(*Create the constant.*)
(*CH$CouplingN++;*)(*Print["\t\tCH$CouplingN=",CH$CouplingN];*)
couplingName="x"<>ToString[n]<>"x";(*<>ToString[CH$CouplingN];*)
(*couplingName="x"<>ToString[n];*)
constantsTmp=Abs[constants]/.Abs[a_^2]->a^2;
constants=constants/constantsTmp Sqrt[constantsTmp/2]/.Sqrt[a_^2]->a;
(*Write the coupling constant string.*)
couplString=StringReplace[ToString[CForm[constants//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];
CheckSizeOfExpression[couplingName,couplString,maxLength];
couplString=stringResize[couplingName,CH$ParameterNameLength]<>"|"<>
stringResize[couplString,Max[CH$LongestSoFar,StringLength[couplString]]+10]<>If[CH$CompHEP," |"," % "]<>
stringResize[resizeName[vertex[[1,1,1]]],5]<>
stringResize[resizeName[vertex[[1,2,1]]],5]<>
stringResize[resizeName[vertex[[1,3,1]]]<>".t",8]<>" coupling.\n";

(*Write the Lorentz part of the vertex string.*)
(*If[MemberQ[term1423,CHLoChain[GG]^2],*)
vertString=stringResize["GG*"<>couplingName,31]<>"|m1.M3*m2.m3-m1.m3*m2.M3\n";(*,
vertString=stringResize[couplingName,31]<>"|m1.M3*m2.m3-m1.m3*m2.M3\n";,
vertString=stringResize[couplingName,31]<>"|m1.M3*m2.m3-m1.m3*m2.M3\n";
];*)
{vertString,{couplString}}
];


(* ::Subsubsection::Closed:: *)
(*CHGlueGlueSquarkSquark[vertex, n, maxLength]*)


CHGlueGlueSquarkSquark[vertex_,n_,maxLength_]:=Module[{constants,constantsTmp,constantSign,couplingName,couplString,partLine,varLine,vertLine,i,iglue,isquark},
(*Print["Working on :",vertex];*)

(*Create the constant.*)
(*CH$CouplingN++;*)
couplingName="x"<>ToString[n]<>"x";(*<>ToString[CH$CouplingN];*)
constantsTmp=-vertex[[2]]/2/.ParamRules//.{CHLoChain[__]->1,
	T[Index[Gluon,Ext[a_]],Index[Colour,b1_,b2_],Index[Colour,Ext[c_]]]->1,
	T[Index[Gluon,Ext[a_]],Index[Colour,Ext[c_]],Index[Colour,b1_,b2_]]->-1,
	T[Index[Gluon,Ext[a_]],Index[Colour,b1_],Index[Colour,Ext[c_]]]->1,
	T[Index[Gluon,Ext[a_]],Index[Colour,Ext[c_]],Index[Colour,b1_]]->-1
	};
If[CH$Simplify,constantsTmp=Simplify[constantsTmp]];
constantSign=-Re[NumericalValue[constantsTmp]];
constantsTmp=ToExpression["Mox"<>ToString[n]<>"x"<>ToString[CH$AuxiliaryN]]^2constantsTmp;
constants=Sqrt[constantsTmp]/.Sqrt[a_^2]->a;
(*Write the coupling constant string.*)
couplString=StringReplace[ToString[CForm[constants//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];
CheckSizeOfExpression[couplingName,couplString,maxLength];
couplString=stringResize[couplingName,CH$ParameterNameLength]<>"|"<>
stringResize[couplString,Max[CH$LongestSoFar,StringLength[couplString]]+10]<>If[CH$CompHEP," |"," % "]<>
stringResize[resizeName[vertex[[1,1,1]]],5]<>
stringResize[resizeName[vertex[[1,2,1]]],5]<>
stringResize[resizeName[vertex[[1,3,1]]],5]<>
stringResize[resizeName[vertex[[1,4,1]]],5]<>" coupling.\n";
(*Print["Coupling Line : ",couplString];*)

(*Create the auxiliary field*)
AuxName="ox"<>ToString[n]<>"x"<>ToString[CH$AuxiliaryN];
AuxAntiName="Ox"<>ToString[n]<>"x"<>ToString[CH$AuxiliaryN];
(*Create the particle table line.*)
partLine=stringResize["Auxiliary "<>ToString[n]<>"x"<>ToString[CH$AuxiliaryN],13];
partLine=partLine<>"|"<>stringResize[AuxName,CH$ParticleNameLength+1]<>"|"<>stringResize[AuxAntiName,CH$ParticleNameLength+1];
If[!CH$CompHEP,partLine=partLine<>"|"<>stringResize[ToString[9999900+CH$AuxiliaryN],8]];
partLine=partLine<>"|"<>stringResize["2",6];
partLine=partLine<>"|"<>stringResize["Mox"<>ToString[n]<>"x"<>ToString[CH$AuxiliaryN],CH$ParameterNameLength]<>"|"<>stringResize["0",CH$ParameterNameLength];
partLine=partLine<>"|"<>stringResize["3",5]<>"|"<>stringResize["*",3];
partLine=partLine<>"|"<>stringResize["aux_{"<>ToString[CH$AuxiliaryN]<>"}",10];
partLine=partLine<>"|"<>stringResize["Aux_{"<>ToString[CH$AuxiliaryN]<>"}",14];
partLine=partLine<>"|\n";
(*Print["Auxiliary Particle Line : ",partLine];*)

(*Add mass to vars.*)
varLine=stringResize["Mox"<>ToString[n]<>"x"<>ToString[CH$AuxiliaryN],CH$ParameterNameLength]<>stringResize["|100000",14]<>stringResize["|Mass of Auxiliary "<>ToString[n]<>"x"<>ToString[CH$AuxiliaryN]<>".",75]<>"|\n";
(*Print["Mass Line : ",varLine];*)

(*Create vertex lines*)
(*Find the 1st glue and the squark.*)
iglue=0;isquark=0;
Do[
If[ParticleColor[vertex[[1,i,1]]]==O,iglue=i];
If[ParticleColor[vertex[[1,i,1]]]==T&&AntiFieldQ[PartSymbol[vertex[[1,i,1]]]]===False,isquark=i];
,{i,1,4}];
vertLine=stringResize[resizeName[vertex[[1,iglue,1]]],CH$ParticleNameLength+3];
vertLine=vertLine<>stringResize["|"<>resizeName[vertex[[1,isquark,1]]],CH$ParticleNameLength+4];
vertLine=vertLine<>stringResize["|"<>AuxAntiName,CH$ParticleNameLength+4];
vertLine=vertLine<>stringResize["|",CH$ParticleNameLength+4]<>"|";
(*If[constantSign<0,Print[ToString[constantSign,InputForm]<>"<0"],Print[ToString[constantSign,InputForm]<>">=0"],Print[FullForm[constantSign],"<?>0"]];*)
vertLine=vertLine<>stringResize[If[constantSign<0,"i*","",""]<>"GG*"<>couplingName,31]<>"|m1.m3\n";
(*Now for the second pair.*)
Do[
If[ParticleColor[vertex[[1,i,1]]]==O,iglue=i];
If[ParticleColor[vertex[[1,i,1]]]==T&&AntiFieldQ[PartSymbol[vertex[[1,i,1]]]]===True,isquark=i];
,{i,4,1,-1}];
vertLine=vertLine<>stringResize[resizeName[vertex[[1,iglue,1]]],CH$ParticleNameLength+3];
vertLine=vertLine<>stringResize["|"<>resizeName[vertex[[1,isquark,1]]],CH$ParticleNameLength+4];
vertLine=vertLine<>stringResize["|"<>AuxName,CH$ParticleNameLength+4];
vertLine=vertLine<>stringResize["|",CH$ParticleNameLength+4]<>"|";
vertLine=vertLine<>stringResize[If[constantSign<0,"i*","",""]<>"GG*"<>couplingName,31]<>"|m1.m3\n";
(*Print["vertLine : ",vertLine,"\n\n"];*)


CH$AuxiliaryN++;
{vertLine,{couplString},partLine,varLine}];


(* ::Subsubsection::Closed:: *)
(*CHNonFermionVertex[vertex, n, maxLength]*)


CHNonFermionVertex[vertex_,n_,maxLength_]:=Module[{vertString="",vertLorentzString="",couplString=
"",couplStringTmp="",couplingN=0,couplingName,vertexStuff,couplingConstants={},constants,lorentzStuff,j,commonFactor,nGG=0,nEE=0,nGGn=0,nEEn=0,GGEEString=""},


(*Remove color indices.  They are implicit in CH.*)
vertexStuff=vertex[[2]]/.ParamRules//.{
IndexDelta[Index[Colour,_],Index[__]]->1,
IndexDelta[Index[Gluon,_],Index[__]]->1,
T[Index[Colour,_],__]->1,
T[Index[Gluon,_],__]->1,
f[Index[Gluon,_],Index[Gluon,_],Index[Gluon,__]]->1
(*f_[Index[Gluon,__],__]->1,
f_[Index[Colour,__],__]->1*)};

(*Write common factor string*)

commonFactor=CommonLorentzFactor[vertexStuff];
(*Print["Common=",commonFactor];*)
(*CH$CouplingN++;*)
couplingName="x"<>ToString[n]<>"x"<>ToString[couplingN++];
(*couplingName="x"<>ToString[n];*)
vertString=stringResize[couplingName,31];

If[CH$Simplify,
couplString=StringReplace[ToString[CForm[Simplify[commonFactor]//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
couplString=StringReplace[ToString[CForm[commonFactor//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
couplString=StringReplace[ToString[CForm[commonFactor//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];
];(*Print["couplString=",couplString];*)
CheckSizeOfExpression[couplingName,couplString,maxLength];
couplString=stringResize[couplingName,CH$ParameterNameLength]<>"|"<>
stringResize[couplString,Max[CH$LongestSoFar,StringLength[couplString]]+10]<>If[CH$CompHEP," |"," % "]<>
stringResize[resizeName[vertex[[1,1,1]]],5]<>
stringResize[resizeName[vertex[[1,2,1]]],5]<>
stringResize[resizeName[vertex[[1,3,1]]],5];
If[Length[vertex[[1]]]===4,couplString=couplString<>stringResize[resizeName[vertex[[1,4,1]]],5]];
couplString=couplString<>" coupling.\n";

(*Check if the vertex is the sum of several terms.*)
(*If it is, sum over them.*)
Which[Head[vertexStuff]===Plus,
	Do[
		CH$CouplingN++;
		couplingName="x"<>ToString[n]<>"x"<>ToString[couplingN++];
		(*couplingName="x"<>ToString[n]<>"s"<>ToString[j];*)
		If[CH$Simplify||1==1,
			constants=Simplify[(vertexStuff[[j]]/.{CHLoChain[__]->1})/commonFactor];
		If[constants===0,LoChain=1,LoChain= Simplify[vertexStuff[[j]]/constants/commonFactor]];,
			constants=Simplify[(vertexStuff[[j]]/.{CHLoChain[__]->1})/commonFactor];
		If[constants===0,LoChain=1,LoChain= Simplify[vertexStuff[[j]]/constants/commonFactor]];,
			constants=Simplify[(vertexStuff[[j]]/.{CHLoChain[__]->1})/commonFactor];
		If[constants===0,LoChain=1,LoChain= Simplify[vertexStuff[[j]]/constants/commonFactor]];
		];
	
		(*Write the coupling constant string.*)(*Print[Count[LoChain,GG],":",Count[LoChain,EE],": ",LoChain];*)
		If[j===1,
			nGG=Count[LoChain,GG,Infinity,Heads->True];
			nEE=Count[LoChain,EE,Infinity,Heads->True];
		,
			nGGn=Count[LoChain,GG,Infinity,Heads->True];
			nEEn=Count[LoChain,EE,Infinity,Heads->True];
			If[nGGn!=nGG,
				Print["Error: Vertices with different powers of GG in each term is not supported."];
				Print[vertex];
			];
			If[nEEn!=nEE,nEE=0];
		];
		LoChain=LoChain//.{CHLoChain[a___,GG,b___]->CHLoChain[a,b]};
		If[nEE>0,LoChain=LoChain//.{CHLoChain[a___,EE,b___]->CHLoChain[a,b]};];
		(*If a term is numeric, then just include it in the Lorentz part.*)
		If[NumericQ[constants],
		(*Write the Lorentz part of the vertex string.*)
		Which[constants===1,
			vertLorentzString=vertLorentzString<>LoChainToString[LoChain]<>"+",
		constants==-1&&j==1,
			vertLorentzString=vertLorentzString<>"-"<>LoChainToString[LoChain]<>"+",
		constants==-1&&j>1,
			vertLorentzString=StringDrop[vertLorentzString,-1];
			vertLorentzString=vertLorentzString<>"-"<>LoChainToString[LoChain]<>"+",
		constants<0&&j==1,
			vertLorentzString=vertLorentzString<>ToString[constants]<>"*"<>LoChainToString[LoChain]<>"+",
		constants<0&&j>1,
			vertLorentzString=StringDrop[vertLorentzString,-1];
			vertLorentzString=vertLorentzString<>ToString[constants]<>"*"<>LoChainToString[LoChain]<>"+",
		constants>0,
			vertLorentzString=vertLorentzString<>ToString[constants]<>"*"<>LoChainToString[LoChain]<>"+",
		1==1,
			vertLorentzString=vertLorentzString<>"("<>ToString[constants]<>")*"<>LoChainToString[LoChain]<>"+"
		];
		,
		(*Write couplString*)
		If[CH$Simplify,
			couplStringTmp=StringReplace[ToString[CForm[Simplify[constants]//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
			couplStringTmp=StringReplace[ToString[CForm[constants//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
			couplStringTmp=StringReplace[ToString[CForm[constants//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];
		];
		CheckSizeOfExpression[couplingName,couplStringTmp,maxLength];
		couplString=couplString<>
		stringResize[couplingName,CH$ParameterNameLength]<>"|"<>
		stringResize[couplStringTmp,Max[CH$LongestSoFar,StringLength[couplStringTmp]]+10]<>" \n";
	
		(*Write the Lorentz part of the vertex string.*)
		vertLorentzString=vertLorentzString<>couplingName<>"*"<>
		LoChainToString[LoChain]<>"+";
		];
	,{j,1,Length[vertexStuff]}];
	vertLorentzString=StringDrop[vertLorentzString,-1];
,
(*If the vertex only has one term, do not sum.*)
1==1,
	CH$CouplingN++;
	couplingName="x"<>ToString[n]<>"x"<>ToString[couplingN++];
	(*couplingName="x"<>ToString[n];*)
	constants= vertexStuff/.{CHLoChain[__]->1};
	If[constants===0,LoChain=1,LoChain=Simplify[vertexStuff/constants]];
	
	(*Write the Lorentz part of the vertex string.*)(*Print[Count[LoChain,GG],":",Count[LoChain,EE],": ",LoChain];*)
	nGG=Count[LoChain,GG,Infinity,Heads->True];
	nEE=Count[LoChain,EE,Infinity,Heads->True];
	LoChain=LoChain//.{CHLoChain[a___,GG,b___]->CHLoChain[a,b],CHLoChain[a___,EE,b___]->CHLoChain[a,b]};
	vertLorentzString=vertLorentzString<>LoChainToString[LoChain];
];

Which[
nGG>1&&nEE>1,
	GGEEString="GG^"<>ToString[nGG]<>"*EE^"<>ToString[nEE],
nGG==1&&nEE>1,
	GGEEString="GG*EE^"<>ToString[nEE],
nGG>1&&nEE==1,
	GGEEString="GG^"<>ToString[nGG]<>"*EE",
nGG==1&&nEE==1,
	GGEEString="GG*EE",
nGG==0&&nEE>1,
	GGEEString="EE^"<>ToString[nEE],
nGG>1&&nEE==0,
	GGEEString="GG^"<>ToString[nGG],
nGG==0&&nEE==1,
	GGEEString="EE",
nGG==1&&nEE==0,
	GGEEString="GG",
1==1,
	GGEEString=""
];
If[GGEEString=!="",vertString=GGEEString<>"*"<>vertString];
vertString=stringResize[vertString,31]<>"|"<>vertLorentzString<>"\n";

{vertString,{couplString}}
];


(* ::Subsubsection::Closed:: *)
(*CHLoReplacementRules*)


(* ::Input:: *)
(*(*The purpose of this is to combine all the Lorentz structure into one construct.*)*)
(*(*Then we can simplify the CH structure.*)*)


CHLoReplacementRules={
FV[a_,Index[b__]]->CHLoChain[FV[a,Index[b]]],
FV[a_,Ext[b__]]->CHLoChain[FV[a,Index[Lorentz,Ext[b]]]],
SP[a__]->CHLoChain[SP[a]],
ME[a__]->CHLoChain[ME[a]],
Eps[Index[Lorentz,a1__],Index[Lorentz,a2__],Index[Lorentz,a3__],Index[Lorentz,a4__]]->CHLoChain[CHEps[Index[Lorentz,a1],Index[Lorentz,a2],Index[Lorentz,a3],Index[Lorentz,a4]]],
(*i has to be in the Vertices table.  It cannot be in the Constraints table.*)
Complex[r_,im_]->r*CHLoChain[1]+im*CHLoChain[i],
GG->CHLoChain[GG],
EE->CHLoChain[EE]
};


(* ::Subsubsection::Closed:: *)
(*CHLoSimplificationRules*)


CHLoSimplificationRules={
(*Combine Eps[] and FV[] where possible.*)
CHLoChain[f___,FV[a_,Index[Lorentz,b_,c_]],g___,CHEps[d___,Index[Lorentz,b_,c_],e___],h___]->CHLoChain[f,CHEps[d,FV[a],e],g,h],
CHLoChain[f___,CHEps[d___,Index[Lorentz,b_,c_],e___],g___,FV[a_,Index[Lorentz,b_,c_]],h___]->CHLoChain[f,CHEps[d,FV[a],e],g,h],
CHLoChain[f___,FV[a_,Index[Lorentz,b_]],g___,CHEps[d___,Index[Lorentz,b_],e___],h___]->CHLoChain[f,CHEps[d,FV[a],e],g,h],
CHLoChain[f___,CHEps[d___,Index[Lorentz,b_],e___],g___,FV[a_,Index[Lorentz,b_]],h___]->CHLoChain[f,CHEps[d,FV[a],e],g,h],
(*Move FV[_] to the front of Eps[__].*)
CHLoChain[e___,CHEps[a___,Index[b__],FV[c_],d___],f___]->-CHLoChain[e,CHEps[a,FV[c],Index[b],d],f],
(*Remove 1 from CHLoChain : Added 12/2/09 so that all scalar vertices do not grow too large.*)
CHLoChain[e___,1,f__]->CHLoChain[e,f],CHLoChain[e__,1,f___]->CHLoChain[e,f],
(*Replace i^2 with -1*)
CHLoChain[e___,i,f___,i,g___]->-CHLoChain[1,e,f,g]};


(* ::Subsubsection::Closed:: *)
(*TensToCHLoChain[vertex]*)


TensToCHLoChain[vertex_]:=If[CH$Simplify,
Collect[Expand[vertex/.CHLoReplacementRules]//.
(*Combine products of CHLoChains.*)
{CHLoChain[a__]CHLoChain[b__]->CHLoChain[a,b],Power[CHLoChain[a__],b_Integer]:>Apply[CHLoChain,Table[a,{b}]]},CHLoChain[__],Simplify]//.CHLoSimplificationRules,
Collect[Expand[vertex/.CHLoReplacementRules]//.
(*Combine products of CHLoChains.*)
{CHLoChain[a__]CHLoChain[b__]->CHLoChain[a,b],Power[CHLoChain[a__],b_Integer]:>Apply[CHLoChain,Table[a,{b}]]},CHLoChain[__]]//.CHLoSimplificationRules,
Collect[Expand[vertex/.CHLoReplacementRules]//.
(*Combine products of CHLoChains.*)
{CHLoChain[a__]CHLoChain[b__]->CHLoChain[a,b],Power[CHLoChain[a__],b_Integer]:>Apply[CHLoChain,Table[a,{b}]]},CHLoChain[__]]//.CHLoSimplificationRules
]


(* ::Subsubsection::Closed:: *)
(*LoChainToString[LoChain]*)


LoChainToString[LoChain_]:=Module[{string="",chain,arguments,j},
(*Print[LoChain];*)
(*Replace the elements of the Lorentz chain with their CH form.*)
chain=LoChain//.{
Index[Lorentz,Ext[a_]]:>"m"<>ToString[a],
Index[Lorentz,Ext[a_,1]]:>"m"<>ToString[a],
Index[Lorentz,Ext[a_,2]]:>"M"<>ToString[a]
}//.
(*Index[Lorentz,Ext[a_,b_]]->Index[Lorentz,Ext[a]]//.*){
(*GG*)
CHLoChain[a___,GG,b___]->CHLoChain["GG",a,b],
(*EE*)
CHLoChain[a___,EE,b___]->CHLoChain["EE",a,b],
(*i*)
CHLoChain[a___,i,b___]->CHLoChain["i",a,b],
(*1*)
CHLoChain[a___,1,b___]->CHLoChain["1",a,b],
(*Scalar Product*)
SP[a_,b_]:>"p"<>ToString[a]<>".p"<>ToString[b],
(*Four Vector*)
FV[n_,a_]:>"p"<>ToString[n]<>"."<>a,
FV[n_]:>"p"<>ToString[n],
(*FV[n_,Index[Lorentz,Ext[a_]]]:>"p"<>ToString[n]<>".m"<>ToString[a],*)
(*Metric*)
ME[a_,b_]:>a<>"."<>b
(*ME[Index[Lorentz,Ext[a_]],Index[Lorentz,Ext[b_]]]:>"m"<>ToString[a]<>".m"<>ToString[b],*)}//.{
(*Eps[a,b,c,d]*)
CHEps[a_,b_,c_,d_]:>"eps("<>a<>","<>b<>","<>c<>","<>d<>")"
(*Eps[Index[Lorentz,Ext[a_]],Index[Lorentz,Ext[b_]],Index[Lorentz,Ext[c_]],Index[Lorentz,Ext[d_]]]:>"eps(m"<>ToString[a]<>",m"<>ToString[b]<>",m"<>ToString[c]<>",m"<>ToString[d]<>")",
Eps[FV[a_],Index[Lorentz,Ext[b_]],Index[Lorentz,Ext[c_]],Index[Lorentz,Ext[d_]]]:>"eps(p"<>ToString[a]<>",m"<>ToString[b]<>",m"<>ToString[c]<>",m"<>ToString[d]<>")",
Eps[FV[a_],FV[b_],Index[Lorentz,Ext[c_]],Index[Lorentz,Ext[d_]]]:>"eps(p"<>ToString[a]<>",p"<>ToString[b]<>",m"<>ToString[c]<>",m"<>ToString[d]<>")"*)
}/.GreekToSpelledOut;
(*Print[chain];*)

(*Now turn it into a string.*)
Which[
FreeQ[chain,CHLoChain],
string="1";,
1==1,
arguments=chain/.CHLoChain[a__]->{a};
If[Length[arguments]>0,string=ToString[arguments[[1]]],string="1"];
Do[string=string<>"*"<>ToString[arguments[[j]]];
,{j,2,Length[arguments]}];
];
(*Print[string];*)
string
];


(*Internal indices are greek letters.  They need to replaced with their English equivalents.*)
GreekToSpelledOut={\[Alpha]->alpha,\[Beta]->beta,\[Gamma]->gamma,\[Delta]->delta,\[Epsilon]->epsilon,\[Zeta]->zeta,\[Eta]->eta,\[Theta]->theta,\[Iota]->iota,\[Kappa]->kappa,\[Lambda]->lambda,\[Mu]->mu,\[Nu]->nu,\[Xi]->xi,\[Omicron]->omicron,\[Pi]->pi,\[Rho]->rho,\[Sigma]->sigma,\[Tau]->tau,\[Upsilon]->upsilon,\[Phi]->phi,\[Chi]->chi,\[Psi]->psi,\[Omega]->omega};


(* ::Subsubsection::Closed:: *)
(*CHFermionVertex[vertex, n,  maxLength]*)


CHFermionVertex[vertex_,n_,maxLength_]:=Module[{vertString="",couplString=
"",couplStringTmp="",couplingN=0,couplingName,couplingNameTmp,vertexStuff,couplingConstants={},constants,gammaChain,j,vertLorentzString="",nGG=0,nEE=0,nGGn=0,nEEn=0,GGEEString="",commonFactor},
(*Print[vertex];*)

(*Remove color indices.  They are implicit in CH.*)
vertexStuff=vertex[[2]]/.ParamRules//.{
IndexDelta[Index[Colour,_],Index[Colour,_]]->1,
IndexDelta[Index[Gluon,_],Index[Gluon,_]]->1,
T[Index[Gluon,_],Index[Colour,_],Index[Colour,_]]->1,
f[Index[Gluon,_],Index[Gluon,_],Index[Gluon,_]]->1};

(*Write common factor string*)
commonFactor=CommonLorentzFactor[vertexStuff];
(*Print["Common=",commonFactor];*)
CH$CouplingN++;
couplingName="x"<>ToString[n]<>"x"<>ToString[couplingN++];
(*couplingName="x"<>ToString[n];*)
vertString=stringResize[couplingName,31];

If[CH$Simplify,
couplString=StringReplace[ToString[CForm[Simplify[commonFactor]//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
couplString=StringReplace[ToString[CForm[commonFactor//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
couplString=StringReplace[ToString[CForm[commonFactor//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];
];(*Print["couplString=",couplString];*)
CheckSizeOfExpression[couplingName,couplString,maxLength];
couplString=stringResize[couplingName,CH$ParameterNameLength]<>"|"<>
stringResize[couplString,Max[CH$LongestSoFar,StringLength[couplString]]+10]<>If[CH$CompHEP," |"," % "]<>
stringResize[resizeName[vertex[[1,1,1]]],5]<>
stringResize[resizeName[vertex[[1,2,1]]],5]<>
stringResize[resizeName[vertex[[1,3,1]]],5];
If[Length[vertex[[1]]]===4,couplString=couplString<>stringResize[resizeName[vertex[[1,4,1]]],5]];
couplString=couplString<>" coupling.\n";

(*Check if the vertex is the sum of several terms.*)
(*If it is, sum over them.*)
Which[Head[vertexStuff]===Plus,
	Do[
		constants= (vertexStuff[[j]]/commonFactor)/.{
		CHGaChain[__][__]->1,
		FV[__]->1};
		If[CH$Simplify||1==1,
			gammaChain= Simplify[vertexStuff[[j]]/commonFactor/constants];,
			gammaChain= vertexStuff[[j]]/commonFactor/constants;,
			gammaChain= vertexStuff[[j]]/commonFactor/constants;
		];
		(*If not an integer, write the coupling constant string.*)
		If[!IntegerQ[constants],
			CH$CouplingN++;
			couplingNameTmp="x"<>ToString[n]<>"x"<>ToString[couplingN++];
			(*couplingNameTmp="x"<>ToString[n]<>"s"<>ToString[j];*)
			If[CH$Simplify,
				couplStringTmp=StringReplace[ToString[CForm[Simplify[constants]//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
				couplStringTmp=StringReplace[ToString[CForm[constants//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
				couplStringTmp=StringReplace[ToString[CForm[constants//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];
			];
			CheckSizeOfExpression[couplingNameTmp,couplStringTmp,maxLength];
			couplString=couplString<>
			stringResize[couplingNameTmp,CH$ParameterNameLength]<>"|"<>
			stringResize[couplStringTmp,Max[CH$LongestSoFar,StringLength[couplStringTmp]]+10]<>If[CH$CompHEP," |"," % "]<>
			stringResize[resizeName[vertex[[1,1,1]]],5]<>
			stringResize[resizeName[vertex[[1,2,1]]],5]<>
			stringResize[resizeName[vertex[[1,3,1]]],8]<>" coupling.\n";
		];
		
		(*Write the Lorentz part of the vertex string.*)
		(*Print[Count[gammaChain,GG,Infinity,Heads->True],":",Count[gammaChain,EE,Infinity,Heads->True],": ",gammaChain];*)
		If[j===1,
			nGG=Count[gammaChain,GG,Infinity,Heads->True];
			nEE=Count[gammaChain,EE,Infinity,Heads->True];
		,
			nGGn=Count[gammaChain,GG,Infinity,Heads->True];
			nEEn=Count[gammaChain,EE,Infinity,Heads->True];
			If[nGGn!=nGG,
				Print["Error: Vertices with different powers of GG in each term are not supported."];
				Print[vertex];
			];
			If[nEEn!=nEE,nEE=0];
		];
		gammaChain=gammaChain//.{CHGaChain[a___,GG,b___][c__]:>CHGaChain[a,b][c]};
		If[nEE>0,gammaChain=gammaChain//.{CHGaChain[a___,EE,b___][c__]:>CHGaChain[a,b][c]};];
		vertLorentzString=vertLorentzString<>
		Which[
			j==1&&IntegerQ[constants]&&constants>0,ToString[constants],
			IntegerQ[constants]&&constants>0,"+"<>ToString[constants],
			IntegerQ[constants],ToString[constants],
			j==1,couplingNameTmp,
			1==1,"+"<>couplingNameTmp
		]<>
		"*"<>GammaChainToString[gammaChain];
	,{j,1,Length[vertexStuff]}];
	couplingNameTmp="";
	(*vertLorentzString=StringDrop[vertLorentzString,-1];*)
,
(*If the vertex only has one term, do not sum.*)
1==1,
	constants= (vertexStuff/commonFactor)/.{
	CHGaChain[__][__]->1,
	FV[__]->1};
	If[CH$Simplify||1==1,
		gammaChain= Simplify[vertexStuff/commonFactor/constants];,
		gammaChain= vertexStuff/commonFactor/constants;,
		gammaChain= vertexStuff/commonFactor/constants;
	];
	(*If not an integer, write the coupling constant string.*)
	If[!IntegerQ[constants],
		CH$CouplingN++;
		couplingNameTmp="x"<>ToString[n]<>"x"<>ToString[couplingN++];
		(*couplingNameTmp="x"<>ToString[n];*)
		If[CH$Simplify,
			couplString=StringReplace[ToString[CForm[Simplify[constants]//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
			couplString=StringReplace[ToString[CForm[constants//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];,
			couplString=StringReplace[ToString[CForm[constants//.CFunctionReplacements0]//.CFunctionReplacements],{" "->""}];
		];
		CheckSizeOfExpression[couplingNameTmp,couplString,maxLength];
		couplString=stringResize[couplingNameTmp,CH$ParameterNameLength]<>"|"<>
		stringResize[couplString,Max[CH$LongestSoFar,StringLength[couplString]]+10]<>If[CH$CompHEP," |"," % "]<>
		stringResize[resizeName[vertex[[1,1,1]]],5]<>
		stringResize[resizeName[vertex[[1,2,1]]],5]<>
		stringResize[resizeName[vertex[[1,3,1]]],8]<>" coupling.\n";
	];
	
	(*Write the Lorentz part of the vertex string.*)
	nGG=Count[gammaChain,GG,Infinity,Heads->True];
	nEE=Count[gammaChain,EE,Infinity,Heads->True];
	
	gammaChain=gammaChain//.{CHGaChain[a___,GG,b___][c__]:>CHGaChain[a,b][c],CHGaChain[a___,EE,b___][c__]:>CHGaChain[a,b][c]};
	vertLorentzString=GammaChainToString[gammaChain];
];

Which[
nGG>1&&nEE>1,
	GGEEString="GG^"<>ToString[nGG]<>"*EE^"<>ToString[nEE],
nGG==1&&nEE>1,
	GGEEString="GG*EE^"<>ToString[nEE],
nGG>1&&nEE==1,
	GGEEString="GG^"<>ToString[nGG]<>"*EE",
nGG==1&&nEE==1,
	GGEEString="GG*EE",
nGG==0&&nEE>1,
	GGEEString="EE^"<>ToString[nEE],
nGG>1&&nEE==0,
	GGEEString="GG^"<>ToString[nGG],
nGG==0&&nEE==1,
	GGEEString="EE",
nGG==1&&nEE==0,
	GGEEString="GG",
1==1,
	GGEEString=""
];
Which[
GGEEString===""&&couplingName==="",couplingName="1",
couplingName==="",couplingName=GGEEString,
GGEEString==="",couplingName=couplingName,
1==1,couplingName=GGEEString<>"*"<>couplingName;
];
vertString=stringResize[couplingName,31]<>"|"<>vertLorentzString<>"\n";


{vertString,{couplString}}
];


(* ::Subsubsection::Closed:: *)
(*GammaChainToString[GaChain]*)


GammaChainToString[GaChain_]:=Module[{string="",chain,arguments,j},
(*Replace the elements of the gamma chain with their CH form.*)
(*Print[InputForm[GaChain]];
Print[GaChain];*)
chain=GaChain//.{
Index[Lorentz,Ext[a_]]:>"m"<>ToString[a],
Index[Lorentz,Ext[a_,1]]:>"m"<>ToString[a],
Index[Lorentz,Ext[a_,2]]:>"M"<>ToString[a]
}//.{
Ga[5]->"G5",
(*FV[n_,Index[Lorentz,a_,m_]]*CHGaChain[b___,Ga[Index[Lorentz,a_,m_]],c___][d__]:>CHGaChain[b,"G(p"<>ToString[n]<>")",c][d],*)
CHGaChain[a___,Ga[FV[b_]],c___][d__]:>CHGaChain[a,"G(p"<>ToString[b]<>")",c][d],
CHGaChain[a___,SlashedP[b_],c___][d__]:>CHGaChain[a,"G(p"<>ToString[b]<>")",c][d],
(*Lorentz index.*)
CHGaChain[b___,Ga[a_String],c___][d__]:>CHGaChain[b,"G("<>a<>")",c][d],
CHGaChain[b___,Ga[Index[Lorentz,Ext[a_]]],c___][d__]:>CHGaChain[b,"G(m"<>ToString[a]<>")",c][d],
CHGaChain[b___,Ga[Index[Lorentz,Ext[a_,1]]],c___][d__]:>CHGaChain[b,"G(m"<>ToString[a]<>")",c][d],
CHGaChain[b___,Ga[Index[Lorentz,Ext[a_,2]]],c___][d__]:>CHGaChain[b,"G(M"<>ToString[a]<>")",c][d],
(*Contracted gamma matrices.*)
CHGaChain[a___,Ga[Index[Lorentz,b_]],c___,Ga[Index[Lorentz,b_]],d___][e__]:>CHGaChain[a,"G("<>ToString[b/.GreekToSpelledOut]<>")",c,"G("<>ToString[b/.GreekToSpelledOut]<>")",d][e],
CHGaChain[a___,Ga[Index[Lorentz,b_,f_]],c___,Ga[Index[Lorentz,b_,f_]],d___][e__]:>CHGaChain[a,"G("<>ToString[b/.GreekToSpelledOut]<>ToString[f]<>")",c,"G("<>ToString[b/.GreekToSpelledOut]<>ToString[f]<>")",d][e],

CHGaChain[a___,Delta,b___]->CHGaChain[a,"1",b],
CHGaChain[a___,1,b___]->CHGaChain[a,"1",b],
CHGaChain[a___,1+Ga[5],b___]->CHGaChain[a,"(1+G5)",b],
CHGaChain[a___,1-Ga[5],b___]->CHGaChain[a,"(1-G5)",b],
CHGaChain[a___,GG,g___]->CHGaChain["GG",a,g],
CHGaChain[a___,EE,g___]->CHGaChain["EE",a,g],
CHGaChain[a___,i,g___]->CHGaChain["i",a,g],

(*Scalar Product*)
SP[a_,b_]:>"p"<>ToString[a]<>".p"<>ToString[b],
(*Four Vector*)
FV[n_,a_]:>"p"<>ToString[n]<>"."<>a,
FV[n_]:>"p"<>ToString[n],
(*FV[n_,Index[Lorentz,Ext[a_]]]:>"p"<>ToString[n]<>".m"<>ToString[a],*)
(*Metric*)
ME[a_,b_]:>a<>"."<>b,

CHGaChain[]->CHGaChain["1"]
(*Eps[a,b,c,d]*)
(*CHGaChain[aa___,CHEps[Index[Lorentz,Ext[a_]],Index[Lorentz,Ext[b_]],Index[Lorentz,Ext[c_]],Index[Lorentz,Ext[d_]]],cc___]:>CHGaChain[aa,"eps(m"<>ToString[a]<>",m"<>ToString[b]<>",m"<>ToString[c]<>",m"<>ToString[d]<>")",cc]*)
};
(*Print[chain];*)
(*Now turn it into a string.*)
arguments=chain/.CHGaChain[a__][b__]->{a};
string=arguments[[1]];
Do[string=string<>"*"<>arguments[[j]];
,{j,2,Length[arguments]}];
(*Print[InputForm[string]];*)
string
];


(*Internal indices are greek letters.  They need to replaced with their English equivalents.*)
GreekToSpelledOut={\[Alpha]->alpha,\[Beta]->beta,\[Gamma]->gamma,\[Delta]->delta,\[Epsilon]->epsilon,\[Zeta]->zeta,\[Eta]->eta,\[Theta]->theta,\[Iota]->iota,\[Kappa]->kappa,\[Lambda]->lambda,\[Mu]->mu,\[Nu]->nu,\[Xi]->xi,\[Omicron]->omicron,\[Pi]->pi,\[Rho]->rho,\[Sigma]->sigma,\[Tau]->tau,\[Upsilon]->upsilon,\[Phi]->phi,\[Chi]->chi,\[Psi]->psi,\[Omega]->omega};


(* ::Subsubsection::Closed:: *)
(*CHGaRules*)


CHGaRules={
g_?((GammaMatrixQ[#] && (Head[#]=!=TensDot))&)[ind__, Index[Spin,s_], Index[Spin,r_]]:>CHGaChain[g[ind]][Index[Spin,s],Index[Spin,r]],
g_?((GammaMatrixQ[#] && (Head[#]=!=TensDot))&)[Index[Spin,s_], Index[Spin,r_]]:>CHGaChain[g][Index[Spin,s],Index[Spin,r]],
TensDot[g1__,g2_?(GammaMatrixQ)][ Index[Spin,s_], Index[Spin,r_]]->CHGaChain[g1,g2][Index[Spin,s],Index[Spin,r]],
IndexDelta[Index[Spin,r_],Index[Spin,s_]]->CHGaChain[Delta][Index[Spin,r],Index[Spin,s]],
SlashedP[a_,Index[Spin,r_],Index[Spin,s_]]->CHGaChain[Ga[FV[a]]][Index[Spin,r],Index[Spin,s]],
TensDot[g1__,SlashedP[ndx_]][ Index[Spin,s_], Index[Spin,r_]]->CHGaChain[g1,Ga[FV[ndx]]][Index[Spin,s],Index[Spin,r]]
};


CHGaSimplificationRules={
(*GG*)
GG CHGaChain[g__][a__]->CHGaChain[GG,g][a],
CHGaChain[a___,Ga[b_],GG,c___][d__]->CHGaChain[a,GG,Ga[b],c][d],
(*EE*)
EE CHGaChain[g__][a__]->CHGaChain[EE,g][a],
CHGaChain[a___,Ga[b_],EE,c___][d__]->CHGaChain[a,EE,Ga[b],c][d],
(*i has to be in the Vertices table.  It cannot be in the Constraints table.*)
Complex[r_,im_]CHGaChain[g__][a__]->r*CHGaChain[g][a]+im*CHGaChain[i,g][a],
(*Replace scalar products with gamma matrix structure for CH.*)
(*SP[a_,b_]CHGaChain[g__][ind__]->1/2(CHGaChain[Ga[FV[a]],Ga[FV[b]],g][ind]+CHGaChain[Ga[FV[b]],Ga[FV[a]],g][ind]),*)
SP[a_,b_]CHGaChain[g___][ind__]->CHGaChain[SP[a,b],g][ind],
(*Replace metric with gamma matrices.*)
(*ME[Index[Lorentz,a__],Index[Lorentz,b__]]CHGaChain[g__][ind__]->1/2 (CHGaChain[Ga[Index[Lorentz,a]],Ga[Index[Lorentz,b]],g][ind]+CHGaChain[Ga[Index[Lorentz,b]],Ga[Index[Lorentz,a]],g][ind]),*)
ME[a_,b_]CHGaChain[g1___,Ga[b_],g2___][ind__]->CHGaChain[g1,Ga[a],g2][ind],
ME[a__]CHGaChain[g___][ind__]->CHGaChain[ME[a],g][ind],
(*Projection operators need to be explicit.*)
CHGaChain[g1___,ProjP,g2___][a__]->CHGaChain[g1,g2][a]/2+CHGaChain[g1,Ga[5],g2][a]/2,
CHGaChain[g1___,ProjM,g2___][a__]->CHGaChain[g1,g2][a]/2-CHGaChain[g1,Ga[5],g2][a]/2,
CHGaChain[][a__]->CHGaChain[1][a],
(*Replace SlashedP with Ga[FV[]]*)
CHGaChain[g1___,SlashedP[ndx_],g2___][ind__]->CHGaChain[g1,Ga[FV[ndx]],g2][ind],
(*Move Four Vectors inside gamma chains.*)
FV[n_,Index[Lorentz,a_,m_]]*CHGaChain[b___,Ga[Index[Lorentz,a_,m_]],c___][d__]->CHGaChain[b,Ga[FV[n]],c][d],
FV[n_,Index[Lorentz,a_]]*CHGaChain[b___,Ga[Index[Lorentz,a_]],c___][d__]->CHGaChain[b,Ga[FV[n]],c][d],
(*LC Eps tensor*)
Eps[Index[Lorentz,a1__],Index[Lorentz,a2__],Index[Lorentz,a3__],Index[Lorentz,a4__]]CHGaChain[a5___][a6__]->CHGaChain[CHEps[Index[Lorentz,a1],Index[Lorentz,a2],Index[Lorentz,a3],Index[Lorentz,a4]],a5][a6],
(*FV[n_,Index[Lorentz,a__]]CHGaChain[b___,CHEps[c___,Index[Lorentz,a__],d___],e___]->CHGaChain[b,CHEps[c,FV[n],d],e],*)
(*Explicit Eps rules for spin 3/2 fermion*)
CHGaChain[g1___,CHEps[b_,Index[Lorentz,a__],c_,d_],g2___,Ga[Index[Lorentz,a__]],Ga[e__],Ga[5],g3___][ind__]->-CHGaChain[g1,g2,CHEps[b,Index[Lorentz,a],c,d]Ga[Index[Lorentz,a]],Ga[5],Ga[e],g3],
CHGaChain[g1___,CHEps[b_,Index[Lorentz,a__],c_,d_],g2___,Ga[Index[Lorentz,a__]],g3___][ind__]->-CHGaChain[g1,CHEps[Index[Lorentz,a],b,c,d],g2,Ga[Index[Lorentz,a]],g3][ind],
CHGaChain[g1___,CHEps[b_,c_,Index[Lorentz,a__],d_],g2___,Ga[Index[Lorentz,a__]],g3___][ind__]->CHGaChain[g1,CHEps[Index[Lorentz,a],b,c,d],g2,Ga[Index[Lorentz,a]],g3][ind],
CHGaChain[g1___,CHEps[b_,c_,d_,Index[Lorentz,a__]],g2___,Ga[Index[Lorentz,a__]],g3___][ind__]->-CHGaChain[g1,CHEps[Index[Lorentz,a],b,c,d],g2,Ga[Index[Lorentz,a]],g3][ind],
CHGaChain[g1___,Ga[Index[Lorentz,a__]],Ga[5],g2___,CHEps[Index[Lorentz,a__],b___],g3___][ind__]->CHGaChain[g1,CHEps[Index[Lorentz,a],b],Ga[Index[Lorentz,a]],Ga[5],g2,g3][ind],
CHGaChain[g1___,CHEps[Index[Lorentz,a__],b_,c_,d_],g2___,Ga[5],Ga[Index[Lorentz,a__]],g3___][ind__]->-CHGaChain[g1,CHEps[Index[Lorentz,a],b,c,d],g2,Ga[Index[Lorentz,a]],Ga[5],g3][ind],
CHGaChain[g1___,CHEps[Index[Lorentz,a__],b_,c_,d_],g2___,Ga[Index[Lorentz,a__]],Ga[5],g3___][ind__]->-(I/6)(
CHGaChain[g1,g2,Ga[b],Ga[c],Ga[d],g3][ind]+CHGaChain[g1,g2,Ga[c],Ga[d],Ga[b],g3][ind]+
CHGaChain[g1,g2,Ga[d],Ga[b],Ga[c],g3][ind]-CHGaChain[g1,g2,Ga[b],Ga[d],Ga[c],g3][ind]-
CHGaChain[g1,g2,Ga[d],Ga[c],Ga[b],g3][ind]-CHGaChain[g1,g2,Ga[c],Ga[b],Ga[d],g3][ind]
),
(*Full Eps rule if nothing else works.*)
CHGaChain[g1___,CHEps[a_,b_,c_,d_],g2___][ind__]->I/24 (
(*abcd*)
CHGaChain[Ga[a],Ga[b],Ga[c],Ga[d],Ga[5],g1,g2][ind]-CHGaChain[Ga[a],Ga[b],Ga[d],Ga[c],Ga[5],g1,g2][ind]+
CHGaChain[Ga[a],Ga[c],Ga[d],Ga[b],Ga[5],g1,g2][ind]-CHGaChain[Ga[a],Ga[c],Ga[b],Ga[d],Ga[5],g1,g2][ind]+
CHGaChain[Ga[a],Ga[d],Ga[b],Ga[c],Ga[5],g1,g2][ind]-CHGaChain[Ga[a],Ga[d],Ga[c],Ga[b],Ga[5],g1,g2][ind]+
(*bcad*)
CHGaChain[Ga[b],Ga[c],Ga[a],Ga[d],Ga[5],g1,g2][ind]-CHGaChain[Ga[b],Ga[c],Ga[d],Ga[a],Ga[5],g1,g2][ind]+
CHGaChain[Ga[b],Ga[a],Ga[d],Ga[c],Ga[5],g1,g2][ind]-CHGaChain[Ga[b],Ga[a],Ga[c],Ga[d],Ga[5],g1,g2][ind]+
CHGaChain[Ga[b],Ga[d],Ga[c],Ga[a],Ga[5],g1,g2][ind]-CHGaChain[Ga[b],Ga[d],Ga[a],Ga[c],Ga[5],g1,g2][ind]+
(*cabd*)
CHGaChain[Ga[c],Ga[a],Ga[b],Ga[d],Ga[5],g1,g2][ind]-CHGaChain[Ga[c],Ga[a],Ga[d],Ga[b],Ga[5],g1,g2][ind]+
CHGaChain[Ga[c],Ga[b],Ga[d],Ga[a],Ga[5],g1,g2][ind]-CHGaChain[Ga[c],Ga[b],Ga[a],Ga[d],Ga[5],g1,g2][ind]+
CHGaChain[Ga[c],Ga[d],Ga[a],Ga[b],Ga[5],g1,g2][ind]-CHGaChain[Ga[c],Ga[d],Ga[b],Ga[a],Ga[5],g1,g2][ind]+
(*dacb*)
CHGaChain[Ga[d],Ga[a],Ga[c],Ga[b],Ga[5],g1,g2][ind]-CHGaChain[Ga[d],Ga[a],Ga[b],Ga[c],Ga[5],g1,g2][ind]+
CHGaChain[Ga[d],Ga[c],Ga[b],Ga[a],Ga[5],g1,g2][ind]-CHGaChain[Ga[d],Ga[c],Ga[a],Ga[b],Ga[5],g1,g2][ind]+
CHGaChain[Ga[d],Ga[b],Ga[a],Ga[c],Ga[5],g1,g2][ind]-CHGaChain[Ga[d],Ga[b],Ga[c],Ga[a],Ga[5],g1,g2][ind]
),
(*Ga[a]..Ga[a]*)
CHGaChain[g1___,Ga[5],Ga[5],g2___][ind__]->CHGaChain[g1,g2][ind],
CHGaChain[g1___,Ga[5],Ga[a_],Ga[5],g2___][ind__]->-CHGaChain[g1,Ga[a],g2][ind],
CHGaChain[g1___,Ga[5],FV[a_],Ga[5],g2___][ind__]->-CHGaChain[g1,FV[a],g2][ind],
CHGaChain[g1___,Ga[5],Ga[a_],Ga[b_],g2___][ind__]->CHGaChain[g1,Ga[a],Ga[b],g2][ind],
CHGaChain[g1___,Ga[5],Ga[a_],FV[b_],g2___][ind__]->CHGaChain[g1,Ga[a],FV[b],g2][ind],
CHGaChain[g1___,Ga[5],FV[a_],Ga[b_],g2___][ind__]->CHGaChain[g1,FV[a],Ga[b],g2][ind],
CHGaChain[g1___,Ga[5],FV[a_],FV[b_],g2___][ind__]->CHGaChain[g1,FV[a],FV[b],g2][ind],
CHGaChain[g1___,Ga[Index[Lorentz,a__]],Ga[Index[Lorentz,a__]],g2___][ind__]->4CHGaChain[g1,g2][ind],
CHGaChain[g1___,Ga[Index[Lorentz,a__]],Ga[b_],Ga[Index[Lorentz,a__]],g2___][ind__]->-2CHGaChain[g1,Ga[b],g2][ind],
CHGaChain[g1___,Ga[Index[Lorentz,a__]],Ga[b_],Ga[c_],Ga[Index[Lorentz,a__]],g2___][ind__]->2CHGaChain[g1,Ga[b],Ga[c],g2][ind]+2CHGaChain[g1,Ga[c],Ga[b],g2][ind],
CHGaChain[g1___,Ga[Index[Lorentz,a__]],Ga[b_],Ga[c_],Ga[d_],Ga[Index[Lorentz,a__]],g2___][ind__]->-2CHGaChain[g1,Ga[d],Ga[c],Ga[b],g2][ind],
CHGaChain[g1___,Ga[Index[Lorentz,a__]],Ga[b_],Ga[c_],Ga[d_],Ga[e_],Ga[Index[Lorentz,a__]],g2___][ind__]->2(CHGaChain[g1,Ga[c],Ga[d],Ga[e],Ga[b],g2][ind]+CHGaChain[g1,Ga[b],Ga[e],Ga[d],Ga[c],g2][ind]),
(*Multiple factors of i*)
CHGaChain[g1___,i,g2___,i,g3___][ind__]->-CHGaChain[g1,g2,g3][ind],
CHGaChain[g1__,i,g2___][ind__]->CHGaChain[i,g1,g2][ind],
(*Replace Four Vectors with gamma matrix structures for CH.*)
(*FV[a_,Index[b__]]CHGaChain[g__][ind__]->1/2 (CHGaChain[Ga[FV[a]],Ga[Index[b]],g][ind]+CHGaChain[Ga[Index[b]],Ga[FV[a]],g][ind])*)
FV[a__]CHGaChain[g___][ind__]->CHGaChain[FV[a],g][ind],
CHGaChain[g1___,FV[n_,a_],g2___,Ga[a_],g3___][ind__]->CHGaChain[g1,g2,Ga[FV[n]],g3][ind],
CHGaChain[g1___,Ga[a_],g2___,FV[n_,a_],g3___][ind__]->CHGaChain[g1,Ga[FV[n]],g2,g3][ind]
};


(* ::Subsubsection::Closed:: *)
(*TensDotToCHGaRules[vertex]*)


TensDotToCHGaRules[vertex_]:=If[CH$Simplify,
Collect[Expand[Expand[Expand[vertex]/.CHGaRules//.CHGaSimplificationRules]//.CHGaSimplificationRules]//.CHGaSimplificationRules,CHGaChain[__][__],Simplify],
Collect[Expand[Expand[Expand[vertex]/.CHGaRules//.CHGaSimplificationRules]//.CHGaSimplificationRules]//.CHGaSimplificationRules,CHGaChain[__][__]],
Collect[Expand[Expand[Expand[vertex]/.CHGaRules//.CHGaSimplificationRules]//.CHGaSimplificationRules]//.CHGaSimplificationRules,CHGaChain[__][__]]
]


(* ::Subsubsection::Closed:: *)
(*ReorderFermions[vertex]*)


ReorderFermions[vertex_]:=Module[{vert,vertexTmp,indices,indicesTmp,j},
vertexTmp=TensDotToCHGaRules[vertex];
(*Print[vertexTmp];Print[InputForm[vertexTmp]];*)

(*If it is a sum of terms, make sure the spinor index ordering is the same for each.*)
(*If it isn't, warn the user that this is not supported.*)
(*Either way, pull out the ordering of the first term.*)
Which[Head[vertexTmp[[2]]]===Plus,
indices=vertexTmp[[2,1]]/.{a___*CHGaChain[__][b_,c_]->{b,c},CHGaChain[__][b_,c_]->{b,c}};
Do[
indicesTmp=vertexTmp[[2,j]]/.{a___*CHGaChain[__][b_,c_]->{b,c},CHGaChain[__][b_,c_]->{b,c}};
If[CH$PrintWarnings&&indicesTmp=!=indices,
Print[Style["\tWarning!",Red],"  The spinor indices in the following vertex have more than one order:"];
Print["\t\t",vertex];
Print["\t\tThis is not supported."];
Print["\t\tYou will have to fix this by hand."];
];
,{j,2,Length[vertexTmp[[2]]]}];
,
1==1,
indices=vertexTmp[[2]]/.{a___*CHGaChain[__][b_,c_]->{b,c},CHGaChain[__][b_,c_]->{b,c}};
];


(*Check to make sure that the indices are external indices.*)
indicesTmp=indices/.{Index[Spin,Ext[a_]],Index[Spin,Ext[b_]]}->{a,b};
If[CH$PrintWarnings&&indicesTmp===indices,
Print[Style["\tWarning!",Red],"  Internal index at the end of a gamma chain."];
Print["\t\tThis is a bug in FeynRules->CH."];
Print["\t\tPlease send the authors a short notebook whith just enough information to reproduce this vertex:"];
Print["\t\t",vertex];
Print["\t\t",indices,"  :  ",indicesTmp,"  :  ",InputForm[indices]];
];


(*Now, use the extracted indices to reorder the fermions.*)
(*There is no minus sign here, because (Claude tells me) the order  *)
(*of the fields here does not determine their order in the vertex.  *)
(*He tells me that it determined by the indices.  He tells me that  *)
(*the signs in the ordering have already been taken care of.        *)
vert=vertexTmp;
If[indicesTmp[[2]]<indicesTmp[[1]],
vert[[1,indicesTmp[[2]]]]=vertexTmp[[1,indicesTmp[[1]]]];
vert[[1,indicesTmp[[1]]]]=vertexTmp[[1,indicesTmp[[2]]]];
(*Print[vert];*)
];
If[CH$Simplify,
vert[[2]]=Collect[vertexTmp[[2]],CHGaChain[__][__],Simplify];,
vert[[2]]=Collect[vertexTmp[[2]],CHGaChain[__][__]];,
vert[[2]]=Collect[vertexTmp[[2]],CHGaChain[__][__]];
];

vert
];
