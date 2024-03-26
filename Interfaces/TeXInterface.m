(* ::Package:: *)

(* ::Title:: *)
(*TEX Interface*)


(* ::Text:: *)
(*Authors:  Neil Christensen, Claude Duhr*)


(* ::Subsection::Closed:: *)
(*Helpful Definitions and Functions*)


(* ::Subsubsection::Closed:: *)
(*FRTeXFormula*)


FRTeXFormula[form_]:=Module[{nFrm=form},
(*Remove Pattern*)
nFrm=nFrm/.Pattern->TeX$Pattern/.{TeX$Pattern[a_,Blank[]]->a,TeX$Pattern[a_,BlankSequence[]]->a,TeX$Pattern[a_,BlankNullSequence[]]->a}/.TeX$Pattern->Pattern;
(*nFrm=nFrm/.Module->TeX$Module/.TeX$Module[{a___},b___]->b;*)
(*Final textual replacements.*)
nFrm=StringReplace[
	StringReplace[ToString[StandardForm[nFrm],TeXForm],{
		"\\text{Superscript}["~~ShortestMatch[a__]~~","~~ShortestMatch[b__]~~"]":>ToString[a]<>"^{"<>ToString[b]<>"}",
		(*"[" -> "(", "]" -> ")",*)
		"\\unicode{f3c8}"->"\\dagger "
		"^"->"{}^",
		"_"->"{}_"
	}]
,
	{"^"->"{}^",
	"{}{}"->"{}"
	}];
nFrm
];


(* ::Subsubsection::Closed:: *)
(*DescriptionSplit*)


DescriptionSplit[des_,lngth_]:=Module[{j,lst,tmp="",desList={}},
lst=StringSplit[StringReplace[des,{"^"->"\^"}]];
Do[
	If[StringLength[tmp]+StringLength[lst[[j]]]+1>lngth,
		AppendTo[desList,tmp];tmp=lst[[j]];,
		tmp=tmp<>" "<>lst[[j]]
	];
,{j,1,Length[lst]}];
AppendTo[desList,tmp];
desList
];



(*DescriptionSplit["Now is the time for all good men to come to the aid of their country.  Now is the time for all good men to come to the aid of their country.",30]*)


(* ::Subsubsection::Closed:: *)
(*ExtOrInt*)


ExtOrInt[param_]:=Module[{def},
If[Length[Indices/.param[[2]]/.Indices->{}]>0,def=Internal,def=External,def=External];
ParameterType/.param[[2]]/.ParameterType->def
];


(* ::Subsubsection::Closed:: *)
(*TeXLagrangianQ*)


TeXLagrangianQ[f_]:=Module[{LagQ=True},
If[!FreeQ[f,Rule],LagQ=False];
If[Head[f]===List,LagQ=False];
LagQ
];


(* ::Subsubsection::Closed:: *)
(*TeXVertexListQ*)


TeXVertexListQ[f_]:=Module[{vrtQ=False},
If[Head[f]===List&&Head[f[[1]]]===List&&Head[f[[1,1]]]===List,vrtQ=True];	
If[!FreeQ[f,Rule],vrtQ=False];
vrtQ
];


(* ::Subsubsection::Closed:: *)
(*TeXRuleQ*)


TeXRuleQ[f_]:=If[!FreeQ[f,Rule]||!FreeQ[f,RuleDelayed],True,False,False];


(* ::Subsection::Closed:: *)
(*WriteLaTeXOutput*)


(* ::Subsubsection::Closed:: *)
(*Options*)


(*LaTeX$Output=Automatic;
LaTeX$Overwrite=Automatic;
LaTeX$Paper="letterpaper";
LaTeX$Compile=True;*)
Options[WriteLaTeXOutput]={Output->Automatic,FileName->Automatic,Overwrite->True,Paper->"letterpaper",Compile->False};


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXOutput*)


WriteLaTeXOutput[ff___]:=Module[{j,f={ff},lgs={},lj=1,vrts={},vj=1,opts={}},
Do[
	Which[
		Head[f[[j]]]===List&&TeXLagrangianQ[f[[j,2]]],AppendTo[lgs,f[[j]]];lj++;,
		Head[f[[j]]]=!=List&&TeXLagrangianQ[f[[j]]],AppendTo[lgs,{ToExpression["Subscript[L,"<>ToString[lj]<>"]"],f[[j]]}];lj++;,
		Head[f[[j]]]===List&&TeXVertexListQ[f[[j,2]]],AppendTo[vrts,f[[j]]];vj++;,
		Head[f[[j]]]=!=List&&TeXVertexListQ[f[[j,2]]],AppendTo[vrts,{ToExpression["Subscript[V,"<>ToString[vj]<>"]"],f[[j]]}];vj++;,
		TeXRuleQ[f[[j]]],AppendTo[opts,f[[j]]]
	]
,{j,1,Length[f]}];
WriteLaTeXOutputPrivate[lgs,vrts,opts];
(*Print[lgs];
Print[vrts];
Print[opts];*)
];


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXOutputPrivate[lags, verts, options]*)


WriteLaTeXOutputPrivate[lags_,verts_,options_]:=Module[{olddirName,dirName,time0,timef},
(*Time how long the interface takes.*)
time0=AbsoluteTime[];
(*Print message to the screen.*)
Print[" - - - FeynRules interface to LaTeX"];
Print[" - - - ",Style["Authors:",Blue]," N. Christensen and C. Duhr"];
Print[" - - - ",Style["Please cite:",Blue]," arXiv:0906.2474"];

(*Set Options*)
(*LaTeX$Overwrite=Overwrite/.options/.Options[WriteLaTeXOutput];
LaTeX$Paper=Paper/.options/.Options[WriteLaTeXOutput];*)

(*Create directory if not present.*)
LaTeX$Output=Output/.options/.Options[WriteLaTeXOutput];
olddirName=Directory[];
If[$OperatingSystem==="Windows",fileSlash="\\",fileSlash="/"]; 
If[LaTeX$Output===Automatic,
	dirName=olddirName<>fileSlash<>StringReplace[M$ModelName, " " -> "-"]<>"-TeX",
	dirName=LaTeX$Output
];
If[Not[MemberQ[FileNames[],dirName]],
	CreateDirectory[dirName];
];
Print[Style["\nWriting:",Blue]," files to ",dirName,"."];
SetDirectory[dirName];

(*Write particles, parameters and vertices.*)
WriteLaTeXGeneral[options];
WriteLaTeXTitle[options];
WriteLaTeXAbstract[options];
WriteLaTeXIntroduction[options];
WriteLaTeXSymmetries[options];
WriteLaTeXFields[options];
WriteLaTeXLagrangians[lags,options];
WriteLaTeXParameters[options];
WriteLaTeXVertices[verts,options];
WriteLaTeXBibliography[options];

(*Compile*)
LaTeX$Compile=Compile/.options/.Options[WriteLaTeXOutput];
If[LaTeX$Compile===True,CompileLaTeX[options]];

(*Reset directory.*)
SetDirectory[olddirName];

(*Print Done!*)
timef=AbsoluteTime[];
Print[Style["\nDone",Blue]," in "<>ToString[Round[timef-time0,1]]<>"s."];
];


(* ::Subsection::Closed:: *)
(*OpenLaTeXFile[filename,options]*)


OpenLaTeXFile[fileName_,options_]:=Module[{j,fileHandle},

(*Open file*)
If[(Overwrite/.options/.Options[WriteLaTeXOutput])=!=True&&MemberQ[FileNames[],fileName],
	If[(Overwrite/.options/.Options[WriteLaTeXOutput])===False||!ChoiceDialog["Do you want to overwrite "<>fileName<>"?",{"Ok"->True, "Skip"->False}],
		Print[Style["Skipping: ",RGBColor[0,0.75,0.75]],fileName<>"."];
		Return[False];
	];
];
fileHandle=OpenWrite[fileName];

(*Write preamble*)
Print[Style["Writing: ",Blue],fileName<>"."];
WriteString[fileHandle, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"];
WriteString[fileHandle, "% LaTeX file for "<>M$ModelName<>".\n"];
WriteString[fileHandle, "% written by\n"];
Do[
	WriteString[fileHandle, "%\t"<>MR$Authors[[j]]<>"\n"];
,{j,1,Length[MR$Authors]}];
WriteString[fileHandle, "% Generated by the FeynRules LaTeX interface.\n"];
WriteString[fileHandle, "% written by Neil Christensen and Claude Duhr, 2009\n"];
WriteString[fileHandle, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"];
fileHandle
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXGeneral*)


WriteLaTeXGeneral[]:=WriteLaTeXGeneral[Overwrite->Automatic];

WriteLaTeXGeneral[options___]:=Module[{fileName,fileHandle,paper,lngth=7.5},
(*Open file.*)
fileName=FileName/.options/.Options[WriteLaTeXOutput];
If[fileName===Automatic,fileName=StringReplace[M$ModelName," "->"-"]<>".tex"];
fileHandle=OpenLaTeXFile[fileName,options];
If[fileHandle===False,Return[]];

(*Paper Type*)
LaTeX$Paper=ToLowerCase[Paper/.options/.Options[WriteLaTeXOutput]];
Which[
	LaTeX$Paper==="a4paper"||LaTeX$Paper==="a4",paper="a4paper";lngth=10.25;,
	LaTeX$Paper==="a3paper"||LaTeX$Paper==="a3",paper="a3paper";lngth=8.5;,
	LaTeX$Paper==="legalpaper"||LaTeX$Paper==="legal",paper="legalpaper";lngth=13;,
	1==1,paper="letterpaper";lngth=9.75;
];	

(*Write general information to file.*)
WriteString[fileHandle, "\\documentclass["<>paper<>"]{article}\n"];
WriteString[fileHandle, "\n"];
WriteString[fileHandle, "\\usepackage{amsfonts}\n"];
WriteString[fileHandle, "\\usepackage{amsmath}\n"];
WriteString[fileHandle, "\\usepackage{graphicx}\n"];
WriteString[fileHandle, "\n"];
WriteString[fileHandle, "\\newenvironment{respr}[0]{\\sloppy\\begin{flushleft}\\hspace*{0.75cm}\\(}{\\)\\end{flushleft}\\fussy}\n"];
WriteString[fileHandle, "\n"];
WriteString[fileHandle, "\\setlength{\\evensidemargin}{-0.5 in}\n"];
WriteString[fileHandle, "\\setlength{\\oddsidemargin}{-0.5 in}\n"];
WriteString[fileHandle, "\\setlength{\\textwidth}{7.5 in}\n"];
WriteString[fileHandle, "\\setlength{\\topmargin}{-0.5 in}\n"];
WriteString[fileHandle, "\\setlength{\\headheight}{0 in}\n"];
WriteString[fileHandle, "\\setlength{\\headsep}{0 in}\n"];
WriteString[fileHandle, "\\setlength{\\textheight}{"<>ToString[lngth]<>" in}\n"];
WriteString[fileHandle, "\\setlength{\\footskip}{0.25 in}\n"];
WriteString[fileHandle, "\\renewcommand{\\arraystretch}{1.2}\n"];
WriteString[fileHandle, "\n"];
WriteString[fileHandle, "\n"];

WriteString[fileHandle, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"];
WriteString[fileHandle, "% Document begins here.\n"];
WriteString[fileHandle, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"];
WriteString[fileHandle, "\\input{title.tex}\n"];
WriteString[fileHandle, "\\begin{document}\n"];
WriteString[fileHandle, "\\maketitle\n"];
WriteString[fileHandle, "\\input{abstract.tex}\n"];
WriteString[fileHandle, "\\tableofcontents\n"];
WriteString[fileHandle, "\\listoftables\n"];
WriteString[fileHandle, "\\input{introduction.tex}\n"];
WriteString[fileHandle, "\\input{symmetries.tex}\n"];
WriteString[fileHandle, "\\input{fields.tex}\n"];
WriteString[fileHandle, "\\input{lagrangians.tex}\n"];
WriteString[fileHandle, "\\input{parameters.tex}\n"];
WriteString[fileHandle, "\\input{vertices.tex}\n"];
WriteString[fileHandle, "\\input{bibliography.tex}\n"];
WriteString[fileHandle, "\\end{document}\n"];

Close[fileHandle];
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXTitle*)


WriteLaTeXTitle[]:=WriteLaTeXTitle[Overwrite->Automatic];

WriteLaTeXTitle[options___]:=Module[{j,fileHandle},
(*Temporary*)
MR$Emails=PRIVATE`MR$Emails;
MR$Institutions=PRIVATE`MR$Institutions;

(*Open file.*)
fileHandle=OpenLaTeXFile["title.tex",options];
If[fileHandle===False,Return[]];

(*Write title*)
WriteString[fileHandle, "\\title{FeynRules Implementation of "<>StringReplace[M$ModelName,"_"->"\_"]<>"}\n"];

(*Write author information.*)
WriteString[fileHandle,"\\author{\n"];
If[Length[MR$Authors]>0,
	WriteString[fileHandle,"\t"<>MR$Authors[[1]]<>"\n"];
	If[Length[MR$Authors]===Length[MR$Emails],
		WriteString[fileHandle,"\t\t\\thanks{email: "<>MR$Emails[[1]]<>"}\n"];
	];
	If[Length[MR$Authors]===Length[MR$Institutions],
		WriteString[fileHandle,"\t\t\\\\"<>MR$Institutions[[1]]<>"\n"];
	];
];
Do[
	WriteString[fileHandle,"\t\\and\n"];
	WriteString[fileHandle,"\t"<>MR$Authors[[j]]<>"\n"];
	If[Length[MR$Authors]===Length[MR$Emails],
		WriteString[fileHandle,"\t\t\\thanks{email: "<>MR$Emails[[j]]<>"}\n"];
	];
	If[Length[MR$Authors]===Length[MR$Institutions],
		WriteString[fileHandle,"\t\t\\\\"<>MR$Institutions[[j]]<>"\n"];
	];
,{j,2,Length[MR$Authors]}];
If[Length[MR$Authors]=!=Length[MR$Institutions],
	Do[
		WriteString[fileHandle,"\t\t\\\\"<>MR$Institutions[[j]]<>"\n"];
	,{j,1,Length[MR$Institutions]}];
];
WriteString[fileHandle,"}\n"];

(*Write date*)
WriteString[fileHandle,"\\date{\\today}\n"];



Close[fileHandle];
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXAbstract*)


WriteLaTeXAbstract[]:=WriteLaTeXAbstract[Overwrite->Automatic];

WriteLaTeXAbstract[options___]:=Module[{fileName,fileHandle},
(*Open file.*)
fileHandle=OpenLaTeXFile["abstract.tex",options];
If[fileHandle===False,Return[]];

(*Write preamble.*)
WriteLaTeXPreamble[fileHandle];

(*Write abstract*)
WriteString[fileHandle, "\\begin{abstract}\n"];
WriteString[fileHandle, "We describe the implementation of the "<>StringReplace[M$ModelName,"_"->"\_"]<>" model using the FeynRules package.\n"];
WriteString[fileHandle, "\\end{abstract}\n"];


Close[fileHandle];
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXIntroduction*)


WriteLaTeXIntroduction[]:=WriteLaTeXIntroduction[Overwrite->Automatic];

WriteLaTeXIntroduction[options___]:=Module[{fileName,fileHandle},
(*Open file.*)
fileHandle=OpenLaTeXFile["introduction.tex",options];
If[fileHandle===False,Return[]];


(*Write introduction*)
WriteString[fileHandle, "\\section{\\label{introduction}Introduction}\n"];
WriteString[fileHandle, "We describe the implementation of the "<>StringReplace[M$ModelName,"_"->"\_"]<>" model using the FeynRules \\cite{Christensen:2008py} package.\n"];
If[Length[MR$References]>0&&MR$References[[1]]=!=MR$Null,WriteString[fileHandle, "More information about this model can be found in \\cite{1}"]];
If[Length[MR$References]>1,WriteString[fileHandle, "-\\cite{"<>ToString[Length[MR$References]]<>"}"]];
If[Length[MR$References]>0&&MR$References[[1]]=!=MR$Null,WriteString[fileHandle,".\n"]];

Close[fileHandle];
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXSymmetries*)


WriteLaTeXSymmetries[]:=WriteLaTeXSymmetries[Overwrite->Automatic];

WriteLaTeXSymmetries[options___]:=Module[{fileName,fileHandle,reps,repLength,defs,minLength,indices,j,k},
(*Open file.*)
fileHandle=OpenLaTeXFile["symmetries.tex",options];
If[fileHandle===False,Return[]];


(*Write introduction*)
WriteString[fileHandle, "\\section{\\label{symmetries}Gauge Symmetries}\n"];
If[Length[M$GaugeGroups]>0,
	WriteString[fileHandle, "The gauge group of this model is\n"];
	WriteString[fileHandle, "\\begin{equation}\n"];
	WriteString[fileHandle, ToString[M$GaugeGroups[[1,1]]]];
	Do[
		WriteString[fileHandle, "\\ \\times\\ "<>ToString[M$GaugeGroups[[j,1]]]];
	,{j,2,Length[M$GaugeGroups]}];
	WriteString[fileHandle, ".\n\\end{equation}\n"];
	WriteString[fileHandle, "Details of these gauge groups can be found in Table \\ref{tab:symmetries}.\n"];

	(*Write table with symmetry group details.*)
	WriteLaTeXTableBegin[fileHandle,{"Group","Abelian",{"Gauge","Boson"},{"Coupling","Constant"},"Charge",{"Structure","Constant"},{"Symmetric","Tensor"},"Reps","Defs"}];
	Do[
		WriteString[fileHandle, ToString[M$GaugeGroups[[j,1]]]];
		WriteString[fileHandle, " & "<>ToString[Abelian/.M$GaugeGroups[[j,2]]/.{True->"T",False->"F"}]];
		WriteString[fileHandle, " & "<>ToString[GaugeBoson/.M$GaugeGroups[[j,2]]]];
		WriteString[fileHandle, " & "<>ToString[CouplingConstant/.M$GaugeGroups[[j,2]]]];
		WriteString[fileHandle, " & "<>ToString[Charge/.M$GaugeGroups[[j,2]]/.Charge->""]];
		WriteString[fileHandle, " & "<>StringReplace[ToString[StructureConstant/.M$GaugeGroups[[j,2]]/.StructureConstant->""],"$"->"\$"]];
		WriteString[fileHandle, " & "<>StringReplace[ToString[SymmetricTensor/.M$GaugeGroups[[j,2]]/.SymmetricTensor->""],"$"->"\$"]];
		reps=Representations/.M$GaugeGroups[[j,2]]/.Representations->{};
		Which[Length[reps]===0,repLength=0,Length[reps]>0&&Head[reps[[1]]]===List,repLength=Length[reps],Length[reps]>0,repLength=1];
		defs=Definitions/.M$GaugeGroups[[j,2]]/.Definitions->{};
		minLength=Min[repLength,Length[defs]];
		Which[
			Length[reps]===0&&Length[defs]===0,WriteString[fileHandle, " & & \\\\\n"],
			Length[reps]===0&&Length[defs]>0,WriteString[fileHandle, " & & "<>StringReplace[ToString[defs[[1]]],{"__"->"\_\_","$_"->"\$\_","$"->"\$","->"->"$\\rightarrow$"}]<>"\\\\\n"],
			Length[reps]===1&&Length[defs]===0,WriteString[fileHandle, " & $"<>ToString[reps[[1,1]]]<>"_{"<>ToString[MR$IndForm[reps[[1,2]]]]<>","<>ToString[MR$IndForm[reps[[1,2]]]]<>"}$ & \\\\\n"],
			Length[reps]===1&&Length[defs]>0,WriteString[fileHandle, " & $"<>ToString[reps[[1,1]]]<>"_{"<>ToString[MR$IndForm[reps[[1,2]]]]<>","<>ToString[MR$IndForm[reps[[1,2]]]]<>"}$ & "<>StringReplace[ToString[defs[[1]]],{"__"->"\_\_","$_"->"\$\_","$"->"\$","->"->"$\\rightarrow$"}]<>"\\\\\n"],
			Length[reps]>1&&repLength===1&&Length[defs]===0,WriteString[fileHandle, " & $"<>ToString[reps[[1]]]<>"_{"<>ToString[MR$IndForm[reps[[2]]]]<>","<>ToString[MR$IndForm[reps[[2]]]]<>"}$ & \\\\\n"],
			Length[reps]>1&&repLength===1&&Length[defs]>0,WriteString[fileHandle, " & $"<>ToString[reps[[1]]]<>"_{"<>ToString[MR$IndForm[reps[[2]]]]<>","<>ToString[MR$IndForm[reps[[2]]]]<>"}$ & "<>StringReplace[ToString[defs[[1]]],{"__"->"\_\_","$_"->"\$\_","$"->"\$","->"->"$\\rightarrow$"}]<>"\\\\\n"],
			repLength>=1&&Length[defs]===0,WriteString[fileHandle, " & $"<>ToString[reps[[1,1]]]<>"_{"<>ToString[MR$IndForm[reps[[1,2]]]]<>","<>ToString[MR$IndForm[reps[[1,2]]]]<>"}$ & \\\\\n"],
			repLength>=1&&Length[defs]>0,WriteString[fileHandle, " & $"<>ToString[reps[[1,1]]]<>"_{"<>ToString[MR$IndForm[reps[[1,2]]]]<>","<>ToString[MR$IndForm[reps[[1,2]]]]<>"}$ & "<>StringReplace[ToString[defs[[1]]],{"__"->"\_\_","$_"->"\$\_","$"->"\$","->"->"$\\rightarrow$"}]<>"\\\\\n"]
		];
		Do[
			WriteString[fileHandle, " &&&&&& & $"<>ToString[reps[[k,1]]]<>"_{"<>ToString[MR$IndForm[reps[[k,2]]]]<>","<>ToString[MR$IndForm[reps[[k,2]]]]<>"}$ & "<>StringReplace[ToString[defs[[k]]],{"__"->"\_\_","$_"->"\$\_","$"->"\$","->"->"$\\rightarrow$"}]<>"\\\\\n"];
		,{k,2,minLength}];
		If[minLength<2,minLength=2];
		Do[
			WriteString[fileHandle, " &&&&&& & $"<>ToString[reps[[k,1]]]<>"_{"<>ToString[MR$IndForm[reps[[k,2]]]]<>","<>ToString[MR$IndForm[reps[[k,2]]]]<>"}$ & \\\\\n"];
		,{k,minLength,repLength}];
		Do[
			WriteString[fileHandle, " &&&&&& & & "<>StringReplace[ToString[defs[[k]]],{"__"->"\_\_","$_"->"\$\_","$"->"\$","->"->"$\\rightarrow$"}]<>"\\\\\n"];
		,{k,minLength,Length[defs]}]
		
	,{j,1,Length[M$GaugeGroups]}];
	WriteLaTeXTableEnd[fileHandle,"Gauge Groups","tab:symmetries","Details of gauge groups."];
];

indices=PRIVATE`MR$IndexList/.Index->Identity;
If[Length[indices]>0,
	WriteString[fileHandle, "\nThe definitions of the indices can be found in Table \\ref{tab:indices}.\n"];
	(*Write table with symmetry group details.*)
	WriteLaTeXTableBegin[fileHandle,{"Index","Symbol","Range"}];
	Do[
		WriteString[fileHandle, ToString[indices[[j]]]];
		WriteString[fileHandle, " & "<>ToString[MR$IndForm[indices[[j]]]/.MR$IndForm[a_]->a]];
		WriteString[fileHandle, " & 1-"<>ToString[Length[MRIndexRange[Index[indices[[j]]]]]]<> "\\\\\n"];
	,{j,1,Length[indices]}];
	WriteLaTeXTableEnd[fileHandle,"Indices","tab:indices","Definition of the indices."];

];

WriteString[fileHandle,"\\clearpage\n"];
Close[fileHandle];
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXFields*)


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXFields*)


WriteLaTeXFields[]:=WriteLaTeXFields[Overwrite->Automatic];

WriteLaTeXFields[options___]:=Module[{fileName,fileHandle,j,k},
(*Open file.*)
fileHandle=OpenLaTeXFile["fields.tex",options];
If[fileHandle===False,Return[]];
WriteString[fileHandle, "\\section{\\label{fields}Fields}\n"];
WriteString[fileHandle, "In this section, we describe the field content of our model implementation.\n"];

WriteLaTeXFieldTable[fileHandle,"scalar",options];
WriteLaTeXFieldTable[fileHandle,"fermion",options];
WriteLaTeXFieldTable[fileHandle,"vector",options];
WriteLaTeXFieldTable[fileHandle,"spin 3/2",options];
WriteLaTeXFieldTable[fileHandle,"spin 2",options];
WriteLaTeXFieldTable[fileHandle,"ghost",options];

WriteString[fileHandle,"\\clearpage\n"];
Close[fileHandle];
];


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXFieldTable*)


WriteLaTeXFieldTable[fileHandle_,fld_,options__]:=Module[{j,fldRepl,Nfld=0,Nphy=0,Nunphy=0,tblN=1,lineN=0,mxLns},
(*Paper Type*)
LaTeX$Paper=ToLowerCase[Paper/.options/.Options[WriteLaTeXOutput]];
Which[
	LaTeX$Paper==="a4paper"||LaTeX$Paper==="a4",mxLns=38,
	LaTeX$Paper==="a3paper"||LaTeX$Paper==="a3",mxLns=33,
	LaTeX$Paper==="legalpaper"||LaTeX$Paper==="legal",mxLns=49,
	1==1,mxLns=34
];
(*Field Type*)
Which[
	fld==="spin 2",fldRepl={T[_]->True},
	fld==="spin 3/2",fldRepl={R[_]->True},
	fld==="vector",fldRepl={V[_]->True},
	fld==="fermion",fldRepl={F[_]->True},
	fld==="scalar",fldRepl={S[_]->True},
	fld==="ghost",fldRepl={U[_]->True}
];
(*Determine if there are any fields of this type.*)
Do[
	If[(M$ClassesDescription[[j,1]]/.fldRepl),
		Nfld++;
		Which[
			!(Unphysical/.M$ClassesDescription[[j,2]]/.Unphysical->False),Nphy++,
			(Unphysical/.M$ClassesDescription[[j,2]]/.Unphysical->False),Nunphy++
		];
	];
,{j,1,Length[M$ClassesDescription]}];

If[Nfld>0,
	WriteString[fileHandle,"\\subsection{\\label{"<>fld<>"s}"<>StringReplace[fld,{"sc"->"Sc","fe"->"Fe","ve"->"Ve","sp"->"Sp","gh"->"Gh"}]<>" Fields}\n"];
	WriteString[fileHandle,"In this subsection, we describe the "<>fld<>" fields of our model.  "];

	(*Physical*)
	If[Nphy>0,
		lineN=0;tblN=1;
		WriteLaTeXTableBegin[fileHandle,{"Class","SC","I","FI","QN","Mem","M","W","PDG"}];
		Do[If[lineN>mxLns,
				WriteLaTeXTableEnd[fileHandle,"Physical "<>StringReplace[fld,{"sc"->"Sc","fe"->"Fe","ve"->"Ve","sp"->"Sp","gh"->"Gh"}]<>" Fields","tab:"<>fld<>"s:physical:"<>ToString[tblN],"Details of physical "<>fld<>" fields."<>
					"  The headers are as follows:  SC = self conjugate, I = indices, FI = flavor index, QN = quantum numbers, Mem = members, M = mass, W = width, and PDG = particle data group number."];
				WriteLaTeXTableBegin[fileHandle,{"Class","SC","I","FI","QN","Mem","M","W","PDG"}];
				tblN++;lineN=0;
			];
			If[(M$ClassesDescription[[j,1]]/.fldRepl)&&!(Unphysical/.M$ClassesDescription[[j,2]]/.Unphysical->False),{lineN,tblN}=WriteLaTeXField[lineN,tblN,fld,fileHandle,M$ClassesDescription[[j,2]],options]];
		,{j,1,Length[M$ClassesDescription]}];
		WriteLaTeXTableEnd[fileHandle,"Physical "<>StringReplace[fld,{"sc"->"Sc","fe"->"Fe","ve"->"Ve","sp"->"Sp","gh"->"Gh"}]<>" Fields","tab:"<>fld<>"s:physical:"<>ToString[tblN],"Details of physical "<>fld<>" fields."<>
			"  The headers are as follows:  SC = self conjugate, I = indices, FI = flavor index, QN = quantum numbers, Mem = members, M = mass, W = width, and PDG = particle data group number."];
		WriteString[fileHandle,"The details of the physical "<>fld<>"s can be found in Table"];
		If[tblN===1,
			WriteString[fileHandle," \\ref{tab:"<>fld<>"s:physical:1}.  "],
			WriteString[fileHandle,"s \\ref{tab:"<>fld<>"s:physical:1}"];
			Do[WriteString[fileHandle,", \\ref{tab:"<>fld<>"s:physical:"<>ToString[j]<>"}"],{j,2,tblN}];
			WriteString[fileHandle,".  "];
		];
	];

	(*UnPhysical*)
	If[Nunphy>0,
		lineN=0;tblN=1;
		WriteLaTeXTableBegin[fileHandle,{"Class","SC","I","FI","QN","Mem","Definitions"}];
		Do[
			If[lineN>mxLns,
				WriteLaTeXTableEnd[fileHandle,"Unphysical "<>StringReplace[fld,{"sc"->"Sc","fe"->"Fe","ve"->"Ve","sp"->"Sp","gh"->"Gh"}]<>" Fields","tab:"<>fld<>"s:unphysical:"<>ToString[tblN],"Details of unphysical "<>fld<>" fields."<>
					"  The headers are as follows:  SC = self conjugate, I = indices, FI = flavor index, QN = quantum numbers, and Mem = members."];
				WriteLaTeXTableBegin[fileHandle,{"Class","SC","I","FI","QN","Mem","Definitions"}];
				tblN++;lineN=0;
			];
			If[(M$ClassesDescription[[j,1]]/.fldRepl)&&(Unphysical/.M$ClassesDescription[[j,2]]/.Unphysical->False),{lineN,tblN}=WriteLaTeXField[lineN,tblN,fld,fileHandle,M$ClassesDescription[[j,2]],options]];
		,{j,1,Length[M$ClassesDescription]}];
		WriteLaTeXTableEnd[fileHandle,"Unphysical "<>StringReplace[fld,{"sc"->"Sc","fe"->"Fe","ve"->"Ve","sp"->"Sp","gh"->"Gh"}]<>" Fields","tab:"<>fld<>"s:unphysical:"<>ToString[tblN],"Details of unphysical "<>fld<>" fields."<>
			"  The headers are as follows:  SC = self conjugate, I = indices, FI = flavor index, QN = quantum numbers, and Mem = members."];
		WriteString[fileHandle,"The details of the unphysical "<>fld<>"s can be found in Table"];
		If[tblN===1,
			WriteString[fileHandle," \\ref{tab:"<>fld<>"s:unphysical:1}.  "],
			WriteString[fileHandle,"s \\ref{tab:"<>fld<>"s:unphysical:1}"];
			Do[WriteString[fileHandle,", \\ref{tab:"<>fld<>"s:unphysical:"<>ToString[j]<>"}"],{j,2,tblN}];
			WriteString[fileHandle,".  "];
		];
	];

];
];


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXField*)


WriteLaTeXField[lN_,tN_,fld_,fileHandle_,class_,options__]:=Module[{tmp,members,mrLns=False,lineN=lN,mxLns,tblN=tN,j=1,m,w,masses,widths},
(*Paper Type*)
LaTeX$Paper=ToLowerCase[Paper/.options/.Options[WriteLaTeXOutput]];
Which[
	LaTeX$Paper==="a4paper"||LaTeX$Paper==="a4",mxLns=43,
	LaTeX$Paper==="a3paper"||LaTeX$Paper==="a3",mxLns=38,
	LaTeX$Paper==="legalpaper"||LaTeX$Paper==="legal",mxLns=54,
	1==1,mxLns=39
];
(*Members*)
members=ClassMembers/.class/.ClassMembers->{};If[Head[members]=!=List,members={members}];
(*Mass and Width*)
tmp=Mass/.class/.Mass->{};
Which[
	Length[members]===0,m=tmp;masses={};,
	Length[tmp]===Length[members],m="";masses=tmp;,
	Length[tmp]>Length[members],m=tmp[[1]];masses=Drop[tmp,1];,
	Length[tmp]==2,m=tmp;masses={};,
	Length[tmp]==1,m=tmp;masses={};,
	1===1,m="";masses=tmp;
];
tmp=Width/.class/.Width->{};
Which[
	Length[members]===0,w=tmp;widths={};,
	Length[tmp]===Length[members],w="";widths=tmp;,
	Length[tmp]>Length[members],w=tmp[[1]];widths=Drop[tmp,1];,
	Length[tmp]==2,w=tmp;widths={};,
	Length[tmp]==1,w=tmp;widths={};,
	1===1,w="";widths=tmp;
];

(*Add empty line for readability.*)
(*Which[
	!(Unphysical/.class/.Unphysical->False),
	WriteString[fileHandle," &&&&&&&& \\\\\n"],
	(Unphysical/.class/.Unphysical->False),
	WriteString[fileHandle," &&&&&& \\\\\n"]
];*)
(*Or try a line.*)
(*WriteString[fileHandle,"\hline\n"];*)
(*I don't like either!*)

(*First Line*)
mrLns=False;
WriteString[fileHandle,ToString[ClassName/.class]];
WriteString[fileHandle," & "<>ToString[SelfConjugate/.class/.SelfConjugate->False/.{True->"T",False->"F"}]];
tmp=Indices/.class/.Indices->{}/.Index->Identity;If[Head[tmp]=!=List,tmp={tmp}];
WriteString[fileHandle," & "<>ToString[Map[MR$IndForm,tmp]/.MR$IndForm[a_]->a]];
tmp=FlavorIndex/.class/.FlavorIndex->{}/.Index->Identity;If[Head[tmp]=!=List,tmp={tmp}];
WriteString[fileHandle," & "<>ToString[Map[MR$IndForm,tmp]/.MR$IndForm[a_]->a]];
tmp=QuantumNumbers/.class/.QuantumNumbers->{};If[Head[tmp]=!=List,tmp={tmp}];
If[Length[tmp]>1,mrLns=True];
If[Length[tmp]>0,WriteString[fileHandle," & $"<>StringReplace[ToString[InputForm[tmp[[1]]]],{"->"->"="}]<>"$"],WriteString[fileHandle," &"]];
If[Length[members]>0,mrLns=True];
If[Length[members]===0,WriteString[fileHandle," & "<>ToString[ClassName/.class]],WriteString[fileHandle," & "]];
Which[!(Unphysical/.class/.Unphysical->False),
	If[Length[masses]>0,mrLns=True];
	WriteString[fileHandle," & "<>StringReplace[ToString[m],","->"="]];
	If[Length[widths]>0,mrLns=True];
	WriteString[fileHandle," & "<>StringReplace[ToString[w],","->"="]];
	tmp=PDG/.class/.PDG->{};If[Head[tmp]===List&&Length[tmp]>0,mrLns=True];
	Which[
		Head[tmp]=!=List,WriteString[fileHandle," & "<>ToString[tmp]<>"\\\\\n"],
		1==1,WriteString[fileHandle," & \\\\\n"]
	];,
	(Unphysical/.class/.Unphysical->False),
	tmp=Definitions/.class/.Definitions->{}/.
		Pattern->TeX$Pattern/.{TeX$Pattern[a_,Blank[]]->a,TeX$Pattern[a_,BlankSequence[]]->a,TeX$Pattern[a_,BlankNullSequence[]]->a}/.TeX$Pattern->Pattern;
	Which[
		Length[members]===0&&Length[tmp]>0,
		If[Length[tmp]>1,mrLns=True];
		WriteString[fileHandle," & $"<>FRTeXFormula[tmp[[1]]]<>"$\\\\\n"],
		Length[members]>0&&Length[tmp]>0,
		mrLns=True;
		WriteString[fileHandle," & \\\\\n"];,
		1==1,
		WriteString[fileHandle," & \\\\\n"]
	];
];
lineN++;

(*Successive Lines*)
While[mrLns,
	Which[
		!(Unphysical/.class/.Unphysical->False)&&lineN>mxLns,
		WriteLaTeXTableEnd[fileHandle,"Physical "<>StringReplace[fld,{"sc"->"Sc","fe"->"Fe","ve"->"Ve","sp"->"Sp","gh"->"Gh"}]<>" Fields","tab:"<>fld<>"s:physical:"<>ToString[tblN],"Details of physical "<>fld<>" fields."<>
			"  The headers are as follows:  SC = self conjugate, I = indices, FI = flavor index, QN = quantum numbers, Mem = members, M = mass, W = width, and PDG = particle data group number."];
		WriteLaTeXTableBegin[fileHandle,{"Class","SC","I","FI","QN","Mem","M","W","PDG"}];
		tblN++;lineN=0;,
		(Unphysical/.class/.Unphysical->False)&&lineN>mxLns,
		WriteLaTeXTableEnd[fileHandle,"Unphysical "<>StringReplace[fld,{"sc"->"Sc","fe"->"Fe","ve"->"Ve","sp"->"Sp","gh"->"Gh"}]<>" Fields","tab:"<>fld<>"s:unphysical:"<>ToString[tblN],"Details of unphysical "<>fld<>" fields."<>
			"  The headers are as follows:  SC = self conjugate, I = indices, FI = flavor index, QN = quantum numbers, and Mem = members."];
		WriteLaTeXTableBegin[fileHandle,{"Class","SC","I","FI","QN","Mem","Definitions"}];
		tblN++;lineN=0;
	];
	mrLns=False;
	WriteString[fileHandle," &  & & "];
	tmp=QuantumNumbers/.class/.QuantumNumbers->{};If[Head[tmp]=!=List,tmp={tmp}];
	If[Length[tmp]>j+1,mrLns=True];
	If[Length[tmp]>j,WriteString[fileHandle," & $"<>StringReplace[ToString[InputForm[tmp[[j+1]]]],{"->"->"="}]<>"$"],WriteString[fileHandle," &"]];
	If[Length[members]>j,mrLns=True];
	If[Length[members]>=j,WriteString[fileHandle," & "<>ToString[members[[j]]]],WriteString[fileHandle," & "]];
	Which[!(Unphysical/.class/.Unphysical->False),
		If[Length[masses]>j,mrLns=True];
		If[Length[masses]>=j,WriteString[fileHandle," & "<>StringReplace[ToString[masses[[j]]],","->"="]],WriteString[fileHandle," & "]];
		If[Length[widths]>j,mrLns=True];
		If[Length[widths]>=j,WriteString[fileHandle," & "<>StringReplace[ToString[widths[[j]]],","->"="]],WriteString[fileHandle," & "]];
		tmp=PDG/.class/.PDG->{};If[Head[tmp]=!=List,tmp={tmp}];If[Length[tmp]>j,mrLns=True];
		If[Length[tmp]>=j,WriteString[fileHandle," & "<>ToString[tmp[[j]]]],WriteString[fileHandle," & "]];
		WriteString[fileHandle,"\\\\\n"];,
		(Unphysical/.class/.Unphysical->False),
		tmp=Definitions/.class/.Definitions->{}/.
			Pattern->TeX$Pattern/.{TeX$Pattern[a_,Blank[]]->a,TeX$Pattern[a_,BlankSequence[]]->a,TeX$Pattern[a_,BlankNullSequence[]]->a}/.TeX$Pattern->Pattern;
		Which[
			Length[members]===0&&Length[tmp]>j,
			If[Length[tmp]>j+1,mrLns=True];
			WriteString[fileHandle," & $"<>FRTeXFormula[tmp[[j+1]]]<>"$\\\\\n"];,
			Length[members]>0&&Length[tmp]>=j,
			If[Length[tmp]>j,mrLns=True];
			WriteString[fileHandle," & $"<>FRTeXFormula[tmp[[j]]]<>"$\\\\\n"];,
			1==1,
			WriteString[fileHandle," & \\\\\n"]
		];
	];
	j=j+1;
	lineN++;
];
 {lineN,tblN}
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXLagrangians*)


WriteLaTeXLagrangians[lags_,options___]:=Module[{j,fileHandle},
(*Open file.*)
fileHandle=OpenLaTeXFile["lagrangians.tex",options];
If[fileHandle===False,Return[]];
If[Length[lags]>0,
	WriteString[fileHandle, "\\section{\\label{lagrangians}Lagrangian}\n"];
	WriteString[fileHandle, "In this section, we describe the Lagrangian of our model implementation.\n"];
	
	Do[
		WriteString[fileHandle, "\\subsection{\\label{L"<>ToString[j]<>"}"];
		If[Head[lags[[j]]]=!=List,
			WriteString[fileHandle, "$L_{"<>ToString[j]<>"}$}\n"],
			WriteString[fileHandle, "$"<>FRTeXFormula[lags[[j,1]]]<>"$}"]
		];
		WriteString[fileHandle, "\n\\begin{sloppypar}\\begin{flushleft}$\n"];
		WriteString[fileHandle, FRTeXFormula[lags[[j,2]]]<>"\n"];
		WriteString[fileHandle, "$\\end{flushleft}\\end{sloppypar}\n"];
	,{j,1,Length[lags]}];
];

WriteString[fileHandle,"\\clearpage\n"];
Close[fileHandle];
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXParameters*)


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXParameters*)


WriteLaTeXParameters[]:=WriteLaTeXParameters[Overwrite->Automatic];

WriteLaTeXParameters[options___]:=Module[{fileName,fileHandle,j,k},
(*Open file.*)
fileHandle=OpenLaTeXFile["parameters.tex",options];
If[fileHandle===False,Return[]];
WriteString[fileHandle, "\\section{\\label{parameters}Parameters}\n"];
WriteString[fileHandle, "In this section, we describe the parameters of our model implementation.\n"];

WriteLaTeXExternalParameters[fileHandle,options];
WriteLaTeXInternalParameters[fileHandle,options];

Close[fileHandle];
];


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXExternalParameters*)


WriteLaTeXExternalParameters[fileHandle_,options__]:=Module[{j,tmp,tblN=1,lineN=0,mxLns},
(*Paper Type*)
LaTeX$Paper=ToLowerCase[Paper/.options/.Options[WriteLaTeXOutput]];
Which[
	LaTeX$Paper==="a4paper"||LaTeX$Paper==="a4",mxLns=38,
	LaTeX$Paper==="a3paper"||LaTeX$Paper==="a3",mxLns=33,
	LaTeX$Paper==="legalpaper"||LaTeX$Paper==="legal",mxLns=49,
	1==1,mxLns=35
];

WriteString[fileHandle, "\\subsection{\\label{external parameters}External Parameters}\n"];
WriteString[fileHandle, "In this subsection, we describe the external parameters of our model.  "];
(*Write table with external parameter details.*)
tblN=1;lineN=0;
WriteLaTeXTableBegin[fileHandle,{"P","C","I","V","D","PN","BN","OB","IO","Description"}];
Do[
	If[lineN>mxLns,
		WriteLaTeXTableEnd[fileHandle,"External Parameters","tab:parameters:external:"<>ToString[tblN],"Details of external parameters."<>
			"  The headers are as follows:  P = parameter, C = complex, I = indices, V = value, D = definition, PN = parameter name, BN = block name, OB = order block, and IO = interaction order."];
		WriteLaTeXTableBegin[fileHandle,{"P","C","I","V","D","PN","BN","OB","IO","Description"}];
		lineN=0;tblN++;
	];
	If[ExtOrInt[M$Parameters[[j]]]===External,
	{lineN,tblN}=WriteLaTeXExternalParameter[lineN,tblN,fileHandle,M$Parameters[[j]],options];
	];
,{j,1,Length[M$Parameters]}];
WriteLaTeXTableEnd[fileHandle,"External Parameters","tab:parameters:external:"<>ToString[tblN],"Details of external parameters."<>
	"  The headers are as follows:  P = parameter, C = complex, I = indices, V = value, D = definition, PN = parameter name, BN = block name, OB = order block, and IO = interaction order."];
WriteString[fileHandle, "  The details of the external parameters can be found in Table"];
If[tblN===1,
	WriteString[fileHandle," \\ref{tab:parameters:external:1}.  "],
	WriteString[fileHandle,"s \\ref{tab:parameters:external:1}"];
	Do[WriteString[fileHandle,", \\ref{tab:parameters:external:"<>ToString[j]<>"}"],{j,2,tblN}];
	WriteString[fileHandle,".  "];
];

WriteString[fileHandle,"\\clearpage\n"];
];


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXExternalParameter*)


WriteLaTeXExternalParameter[lN_,tN_,fileHandle_,par_,options__]:=Module[{tmp,$j=1,des,vl,df,pn,mrLns=False,mxLns,lineN=lN,tblN=tN},
(*Paper Type*)
LaTeX$Paper=ToLowerCase[Paper/.options/.Options[WriteLaTeXOutput]];
Which[
	LaTeX$Paper==="a4paper"||LaTeX$Paper==="a4",mxLns=43,
	LaTeX$Paper==="a3paper"||LaTeX$Paper==="a3",mxLns=38,
	LaTeX$Paper==="legalpaper"||LaTeX$Paper==="legal",mxLns=54,
	1==1,mxLns=40
];
des=DescriptionSplit[ToString[Description/.par[[2]]/.Description->""],30];
vl=TeXExpandValues[par];
df=Definitions/.par[[2]]/.Definitions->{}/.
	Pattern->TeX$Pattern/.{TeX$Pattern[a_,Blank[]]->a,TeX$Pattern[a_,BlankSequence[]]->a,TeX$Pattern[a_,BlankNullSequence[]]->a}/.TeX$Pattern->Pattern;
If[Head[df]=!=List,df={df}];
pn=ParameterName/.par[[2]]/.ParameterName->{};If[Head[pn]=!=List,pn={pn}];
WriteString[fileHandle, "$"<>FRTeXFormula[par[[1]]]<>"$"];
WriteString[fileHandle, " & "<>ToString[ComplexParameter/.par[[2]]/.ComplexParameter->False/.{True->"T",False->"F"}]];
tmp=Indices/.par[[2]]/.Indices->{}/.Index->Identity;If[Head[tmp]=!=List,tmp={tmp}];
WriteString[fileHandle," & "<>ToString[Map[MR$IndForm,tmp]/.MR$IndForm[a_]->a]];
WriteString[fileHandle," & $"];
(*If[Length[vl]>0,WriteString[fileHandle,FRTeXFormula[vl[[1]]]]];
If[Length[vl]>1,mrLns=True];*)
Which[
	Length[vl]==1,WriteString[fileHandle, ToString[NumericalValue[ApplyDefinitions[vl[[1]]]]]],
	Length[vl]>1,mrLns=True;WriteString[fileHandle,FRTeXFormula[vl[[1,1]]]<>" \\rightarrow "<>ToString[NumericalValue[ApplyDefinitions[vl[[1,2]]]]/.NoValue[1]->1]];
];
WriteString[fileHandle,"$ & $"];
If[Length[df]>0,WriteString[fileHandle,FRTeXFormula[df[[1]]]]];
If[Length[df]>1,mrLns=True];
WriteString[fileHandle, "$ & $"];
If[Length[pn]>0,WriteString[fileHandle,FRTeXFormula[pn[[1]]]]];
If[Length[pn]>1,mrLns=True];
WriteString[fileHandle, "$ & "<>ToString[BlockName/.par[[2]]/.BlockName->""]];
WriteString[fileHandle, " & "<>ToString[OrderBlock/.par[[2]]/.OrderBlock->""]];
WriteString[fileHandle, " & "<>ToString[InteractionOrder/.par[[2]]/.InteractionOrder->""]];
WriteString[fileHandle, " & "];
If[Length[des]>0,WriteString[fileHandle,des[[1]]]];
If[Length[des]>1,mrLns=True];
WriteString[fileHandle,"\\\\\n"];
lineN++;

While[mrLns,
	If[lineN>mxLns,
		WriteLaTeXTableEnd[fileHandle,"External Parameters","tab:parameters:external:"<>ToString[tblN],"Details of external parameters."<>
			"  The headers are as follows:  P = parameter, C = complex, I = indices, V = value, D = definition, PN = parameter name, BN = block name, OB = order block, and IO = interaction order."];
		WriteLaTeXTableBegin[fileHandle,{"P","C","I","V","D","PN","BN","OB","IO","Description"}];
		lineN=0;tblN++;
	];
	mrLns=False;
	$j++;lineN++;
	WriteString[fileHandle," & & & $"];
	If[Length[vl]>=$j,WriteString[fileHandle,FRTeXFormula[vl[[$j]]]]];
	If[Length[vl]>$j,mrLns=True];
	WriteString[fileHandle,"$ & $"];
	If[Length[df]>=$j,WriteString[fileHandle,FRTeXFormula[df[[$j]]]]];
	If[Length[df]>$j,mrLns=True];
	WriteString[fileHandle,"$ & $"];
	If[Length[pn]>=$j,WriteString[fileHandle,FRTeXFormula[pn[[$j]]]]];
	If[Length[pn]>$j,mrLns=True];
	WriteString[fileHandle, "$ & & & &"];
	If[Length[des]>=$j,WriteString[fileHandle,des[[$j]]]];
	If[Length[des]>$j,mrLns=True];
	WriteString[fileHandle, " \\\\\n"];
];
{lineN,tblN}
];


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXInternalParameters*)


WriteLaTeXInternalParameters[fileHandle_,options__]:=Module[{tmp,tblN=1,lineN=0,mxLns,j,k},
(*Paper Type*)
LaTeX$Paper=ToLowerCase[Paper/.options/.Options[WriteLaTeXOutput]];
Which[
	LaTeX$Paper==="a4paper"||LaTeX$Paper==="a4",mxLns=32,
	LaTeX$Paper==="a3paper"||LaTeX$Paper==="a3",mxLns=27,
	LaTeX$Paper==="legalpaper"||LaTeX$Paper==="legal",mxLns=42,
	1==1,mxLns=27
];

WriteString[fileHandle, "\\subsection{\\label{internal parameters}Internal Parameters}\n"];
WriteString[fileHandle, "In this subsection, we describe the internal parameters of our model.  "];

(*Write table with internal parameter details.*)
tblN=1;lineN=0;
WriteLaTeXTableBegin[fileHandle,{"P","C","I","V","NV","D","PN","IO","Description"}];
Do[
	If[lineN>mxLns,
		WriteLaTeXTableEnd[fileHandle,"Internal Parameters","tab:parameters:internal:"<>ToString[tblN],"Details of internal parameters."<>
			"  The headers are as follows:  P = parameter, C = complex, I = Indices, V = value, NV = numerical value, D = definition, PN = parameter name, and IO = interaction order."];
		WriteLaTeXTableBegin[fileHandle,{"P","C","I","V","NV","D","PN","IO","Description"}];
		lineN=0;tblN++;
	];
	If[ExtOrInt[M$Parameters[[j]]]===Internal,
		{lineN,tblN}=WriteLaTeXInternalParameter[lineN,tblN,fileHandle,M$Parameters[[j]],j,options];
	];
,{j,1,Length[M$Parameters]}];
WriteLaTeXTableEnd[fileHandle,"Internal Parameters","tab:parameters:internal:"<>ToString[tblN],"Details of internal parameters."<>
	"  The headers are as follows:  P = parameter, C = complex, I = Indices, V = value, NV = numerical value, D = definition, PN = parameter name, and IO = interaction order."];
WriteString[fileHandle, "  The details of the internal parameters can be found in Table"];
If[tblN===1,
	WriteString[fileHandle," \\ref{tab:parameters:internal:1}.  "],
	WriteString[fileHandle,"s \\ref{tab:parameters:internal:1}"];
	Do[WriteString[fileHandle,", \\ref{tab:parameters:internal:"<>ToString[j]<>"}"],{j,2,tblN}];
	WriteString[fileHandle,".  "];
];


(*Write the equations for the internal parameter values and definitions.*)
WriteString[fileHandle, "  The values and definitions of the internal parameters will be written below."];
Do[If[ExtOrInt[M$Parameters[[j]]]===Internal,
	(*Value*)
	tmp=Value/.M$Parameters[[j,2]]/.Value->{};
	Which[Head[tmp]=!=List,
		WriteString[fileHandle, "\n\\begin{equation}\n"];
		WriteString[fileHandle, "\\label{value:"<>ToString[j]<>"}\n"];
		WriteString[fileHandle, FRTeXFormula[M$Parameters[[j,1]]]];
		WriteString[fileHandle, " = \n"];
		WriteString[fileHandle, FRTeXFormula[tmp]];
		WriteString[fileHandle, "\\end{equation}\n"];,
		Length[tmp]>0,
		WriteString[fileHandle, "\n\\begin{equation}\n"];
		WriteString[fileHandle, "\\label{value:"<>ToString[j]<>"}\n"];
		WriteString[fileHandle, "\\begin{array}{rcl}\n"];
		Do[
			WriteString[fileHandle, FRTeXFormula[tmp[[k,1]]]];
			WriteString[fileHandle, " & = & \n"];
			WriteString[fileHandle, FRTeXFormula[tmp[[k,2]]]<>"\\\\\n"];
		,{k,1,Length[tmp]}];
		WriteString[fileHandle, "\\end{array}\n"];
		WriteString[fileHandle, "\\end{equation}\n"];
	];
];,{j,1,Length[M$Parameters]}];

WriteString[fileHandle,"\\clearpage\n"];
];


(* ::Subsubsection::Closed:: *)
(* WriteLaTeXInternalParameter*)


WriteLaTeXInternalParameter[lN_,tN_,fileHandle_,par_,j_,options__]:=Module[{tmp,k=1,des,nvl,df,pn,mrLns=False,mxLns,lineN=lN,tblN=tN},
(*Paper Type*)
LaTeX$Paper=ToLowerCase[Paper/.options/.Options[WriteLaTeXOutput]];
Which[
	LaTeX$Paper==="a4paper"||LaTeX$Paper==="a4",mxLns=37,
	LaTeX$Paper==="a3paper"||LaTeX$Paper==="a3",mxLns=32,
	LaTeX$Paper==="legalpaper"||LaTeX$Paper==="legal",mxLns=47,
	1==1,mxLns=32
];
des=DescriptionSplit[ToString[Description/.par[[2]]/.Description->""],30];
nvl=TeXExpandValues[par];
df=Definitions/.par[[2]]/.Definitions->{}/.
	Pattern->TeX$Pattern/.{TeX$Pattern[a_,Blank[]]->a,TeX$Pattern[a_,BlankSequence[]]->a,TeX$Pattern[a_,BlankNullSequence[]]->a}/.TeX$Pattern->Pattern;
If[Head[df]=!=List,df={df}];
pn=ParameterName/.par[[2]]/.ParameterName->{};If[Head[pn]=!=List,pn={pn}];
WriteString[fileHandle, "$"<>FRTeXFormula[par[[1]]]<>"$"];
WriteString[fileHandle, " & "<>ToString[ComplexParameter/.par[[2]]/.ComplexParameter->False/.{True->"T",False->"F"}]];
tmp=Indices/.par[[2]]/.Indices->{}/.Index->Identity;If[Head[tmp]=!=List,tmp={tmp}];
WriteString[fileHandle," & "<>ToString[Map[MR$IndForm,tmp]/.MR$IndForm[a_]->a]];
WriteString[fileHandle," & "];
If[Length[Value/.par[[2]]/.Value->{}]>0,WriteString[fileHandle, "Eq. \\ref{value:"<>ToString[j]<>"}"]];
WriteString[fileHandle," & $"];
Which[
	Length[nvl]==1,WriteString[fileHandle, ToString[NumericalValue[ApplyDefinitions[nvl[[1]]]]]],
	Length[nvl]>1,mrLns=True;WriteString[fileHandle,FRTeXFormula[nvl[[1,1]]]<>" \\rightarrow "<>ToString[NumericalValue[ApplyDefinitions[nvl[[1,2]]]]/.NoValue[1]->1]];
];
WriteString[fileHandle,"$  & $"];
If[Length[df]>0,WriteString[fileHandle,FRTeXFormula[df[[1]]]]];
If[Length[df]>1,mrLns=True];
WriteString[fileHandle, "$ & $"];
If[Length[pn]>0,WriteString[fileHandle,FRTeXFormula[pn[[1]]]]];
If[Length[pn]>1,mrLns=True];
WriteString[fileHandle, "$ & "<>ToString[InteractionOrder/.par[[2]]/.InteractionOrder->""]];
WriteString[fileHandle, " & "];
If[Length[des]>0,WriteString[fileHandle,des[[1]]]];
If[Length[des]>1,mrLns=True];
WriteString[fileHandle,"\\\\\n"];
lineN++;

While[mrLns,
	If[lineN>mxLns,
		WriteLaTeXTableEnd[fileHandle,"Internal Parameters","tab:parameters:internal:"<>ToString[tblN],"Details of internal parameters."<>
			"  The headers are as follows:  P = parameter, C = complex, I = Indices, V = value, NV = numerical value, D = definition, PN = parameter name, and IO = interaction order."];
		WriteLaTeXTableBegin[fileHandle,{"P","C","I","V","NV","D","PN","IO","Description"}];
		lineN=0;tblN++;
	];
	mrLns=False;
	k++;lineN++;
	WriteString[fileHandle," & & & & $"];
	If[Length[nvl]>=k,WriteString[fileHandle,FRTeXFormula[nvl[[k,1]]]<>" \\rightarrow "<>ToString[NumericalValue[ApplyDefinitions[nvl[[k,2]]]]/.NoValue[1]->1]]];
	If[Length[nvl]>k,mrLns=True];
	WriteString[fileHandle,"$ & $"];
	If[Length[df]>=k,WriteString[fileHandle,FRTeXFormula[df[[k]]]]];
	If[Length[df]>k,mrLns=True];
	WriteString[fileHandle,"$ & $"];
	If[Length[pn]>=k,WriteString[fileHandle,FRTeXFormula[pn[[k]]]]];
	If[Length[pn]>k,mrLns=True];
	WriteString[fileHandle, "$ & & "];
	If[Length[des]>=k,WriteString[fileHandle,des[[k]]]];
	If[Length[des]>k,mrLns=True];
	WriteString[fileHandle, " \\\\\n"];
];
{lineN,tblN}
];


(* ::Subsubsection::Closed:: *)
(*TeXExpandValues*)


TeXExpandValues[par_]:=Module[{indxJ=1,indcs,indexRanges={},nm,cmpList={},cmpOld,nvls={},vls={},dfs,j},
(*Determine the range of the indices.*)
indcs=Indices/.par[[2]]/.Indices->{};
Do[
	AppendTo[indexRanges,MRIndexRange[indcs[[j]]]];
,{j,1,Length[indcs]}];
(*Determine the component names.*)
nm=par[[1]];
If[Length[indexRanges]>0,
	Do[
		AppendTo[cmpList,ToString[nm]<>"["<>ToString[indexRanges[[1,j]]]]
	,{j,1,Length[indexRanges[[1]]]}];
	indexJ=2;
	While[indexJ<=Length[indexRanges],
		cmpOld=cmpList;cmpList={};
		Do[
			AppendTo[cmpList,cmpOld[[k]]<>","<>ToString[indexRanges[[indexJ,j]]]]
		,{k,1,Length[cmpOld]},{j,1,Length[indexRanges[[indexJ]]]}];
		indexJ++;
	];
	cmpOld=cmpList;cmpList={};
	Do[
		AppendTo[cmpList,ToExpression[cmpOld[[k]]<>"]"]];
	,{k,1,Length[cmpOld]}];
];
(*Determine the replacement lists.*)
(*vls=Value/.par[[2]]/.Value->{};
If[Head[vls]=!=List,vls={vls}];
dfs=Definitions/.par[[2]]/.Definitions->{};
If[Head[dfs]=!=List,dfs={dfs}];*)
Do[
	AppendTo[nvls,cmpList[[k]]->NumericalValue[ApplyDefinitions[cmpList[[k]]](*/.vls/.dfs*)]/.NoValue[1]->1];
,{k,1,Length[cmpList]}];
(*Which[
	Length[nvls]===0&&Length[vls]===1&&TeXRuleQ[vls[[1]]],
	nvls={vls[[1,1]]->NumericalValue[ApplyDefinitions[vls[[1,2]]]/.dfs]/.NoValue[1]->1},
	Length[nvls]===0&&Length[vls]===1,
	nvls={NumericalValue[ApplyDefinitions[vls[[1]]]/.dfs]/.NoValue[1]->1}
];*)
If[Length[nvls]===0,AppendTo[nvls,NumericalValue[ApplyDefinitions[nm]]/.NoValue[1]->1]];
nvls
]


(* ::Subsection::Closed:: *)
(*WriteLaTeXVertices*)


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXVertices*)


WriteLaTeXVertices[vrts_,options___]:=Module[{fileHandle,vrtList,tmp,j,k},
(*Open file.*)
fileHandle=OpenLaTeXFile["vertices.tex",options];
If[fileHandle===False,Return[]];
If[Length[vrts]>0,
	WriteString[fileHandle, "\\section{\\label{vertices}Vertices}\n"];
	WriteString[fileHandle, "In this section, we describe the vertices of our model implementation.\n"];
	
	Do[
		WriteString[fileHandle, "\\subsection{\\label{V"<>ToString[j]<>"}\n"];
		If[TeXVertexListQ[vrts[[j]]],
			vrtList=vrts[[j]];
			WriteString[fileHandle, "$V_{"<>ToString[j]<>"}$}\n"];,
			vrtList=vrts[[j,2]];
			WriteString[fileHandle, "$"<>FRTeXFormula[vrts[[j,1]]]<>"$}"];
		];
		Do[
			WriteString[fileHandle,"\\begin{minipage}{1.5in}\n"];
			WriteString[fileHandle,"\\begin{displaymath}\n"];
			WriteString[fileHandle,ToString[vrtList[[k,1]],TeXForm]<>"\n"];
			WriteString[fileHandle,"\\end{displaymath}\n"];
			WriteString[fileHandle,"\\end{minipage}\n"];
			WriteString[fileHandle,"\\begin{minipage}{6in}\n"];
			WriteString[fileHandle, "\\begin{sloppypar}\\begin{flushleft}$\n"];
			WriteString[fileHandle, FRTeXFormula[Expand[vrtList[[k,2]]]]<>"\n"];
			WriteString[fileHandle, "$\\end{flushleft}\\end{sloppypar}\n"];
			WriteString[fileHandle,"\\end{minipage}\n"];
		,{k,1,Length[vrtList]}];
	,{j,1,Length[vrts]}];
];

WriteString[fileHandle,"\\clearpage\n"];
Close[fileHandle];
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXBibliography*)


WriteLaTeXBibliography[]:=WriteLaTeXBibliography[Overwrite->Automatic];

WriteLaTeXBibliography[options___]:=Module[{fileName,fileHandle,j},
(*Open file.*)
fileHandle=OpenLaTeXFile["bibliography.tex",options];
If[fileHandle===False,Return[]];

(*Write bibliography*)
WriteString[fileHandle, "\\begin{thebibliography}{99}\n"];

Do[
	If[MR$References[[j]]=!=MR$Null,
		WriteString[fileHandle, "%\\cite{"<>ToString[j]<>"}\n"];
		WriteString[fileHandle, "\\bibitem{"<>ToString[j]<>"}\n"];
		WriteString[fileHandle, MR$References[[j]]<>"\n"];
	];
,{j,1,Length[MR$References]}];

WriteString[fileHandle, "%\\cite{Christensen:2008py}\n"];
WriteString[fileHandle, "\\bibitem{Christensen:2008py}\n"];
WriteString[fileHandle, "  N.~D.~Christensen and C.~Duhr,\n"];
WriteString[fileHandle, "  %``FeynRules - Feynman rules made easy,''\n"];
WriteString[fileHandle, "  arXiv:0806.4194 [hep-ph].\n"];
WriteString[fileHandle, "  %%CITATION = ARXIV:0806.4194;%%\n"];

WriteString[fileHandle, "\\end{thebibliography}\n"];


Close[fileHandle];
];


(* ::Subsection::Closed:: *)
(*Compile*)


CompileLaTeX[]:=CompileLaTeX[Paper->"letterpaper"];

CompileLaTeX[options___]:=Module[{fileName,fileHandle,paper,dirName},
Print[Style["\nCompiling.",Blue]];
(*File Name*)
fileName=FileName/.options/.Options[WriteLaTeXOutput];
If[fileName===Automatic,fileName=StringReplace[M$ModelName," "->"-"]<>".tex"];
(*LaTeX*)
dirName=Directory[];
(*Print["cd "<>dirName<>" && pdflatex -interaction batchmode "<>fileName];*)
Run["cd "<>dirName<>" && pdflatex -interaction batchmode "<>fileName];
Run["cd "<>dirName<>" && pdflatex -interaction batchmode "<>fileName];
Run["cd "<>dirName<>" && pdflatex -interaction batchmode "<>fileName];
(*dvips*)
(*LaTeX$Paper=ToLowerCase[Paper/.options/.Options[WriteLaTeXOutput]];
Which[
	LaTeX$Paper==="a4paper"||LaTeX$Paper==="a4",paper="a4",
	LaTeX$Paper==="a3paper"||LaTeX$Paper==="a3",paper="a3",
	LaTeX$Paper==="legalpaper"||LaTeX$Paper==="legal",paper="legal",
	1==1,paper="letter"
];
Run["dvips -t "<>paper<>" -o "<>fileName<>".ps "<>fileName<>".dvi"];*)
(*ps2pdf*)
(*Run["ps2pdf ",fileName<>".ps"];*)
(*Acroread*)
(*Run["acroread ",fileName<>".pdf &"];*)
];


(* ::Subsection::Closed:: *)
(*WriteLaTeXTables*)


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXTableEnd*)


WriteLaTeXTableEnd[fileHandle_,short_,label_,caption_]:=Module[{tmp},
WriteString[fileHandle, "\\hline\n"];
WriteString[fileHandle, "\\end{tabular}\n"];
WriteString[fileHandle, "\\caption["<>short<>"]{\\label{"<>label<>"}"<>caption<>"}\n"];
WriteString[fileHandle, "\\end{center}\n"];
WriteString[fileHandle, "\\end{table}\n"];
];


(* ::Subsubsection::Closed:: *)
(*WriteLaTeXTableBegin*)


WriteLaTeXTableBegin[fileHandle_,headList_]:=Module[{tmp,j},
WriteString[fileHandle, "\\begin{table}[!h]\n"];
WriteString[fileHandle, "\\begin{center}\n"];
WriteString[fileHandle, "\\begin{tabular}{|c|"];
Do[
	If[headList[[j]]==="Description",
		WriteString[fileHandle,"l|"],
		WriteString[fileHandle,"c|"]
	];
,{j,2,Length[headList]}];
WriteString[fileHandle,"}\n"];
WriteString[fileHandle, "\\hline\n"];
If[Head[headList[[1]]]===List,
	WriteString[fileHandle,headList[[1,1]]]
];
Do[
	If[Head[headList[[j]]]===List,
	WriteString[fileHandle," & "<>headList[[j,1]]],
	WriteString[fileHandle," & "]
	];
,{j,2,Length[headList]}];
WriteString[fileHandle, "\\\\\n"];
If[Head[headList[[1]]]===List,
	WriteString[fileHandle,headList[[1,2]]],
	WriteString[fileHandle,"\\raisebox{1.5ex}[0pt]{"<>headList[[1]]<>"}"];
];
Do[
	If[Head[headList[[j]]]===List,
	WriteString[fileHandle," & "<>headList[[j,2]]],
	WriteString[fileHandle," & \\raisebox{1.5ex}[0pt]{"<>headList[[j]]<>"}"]
	];
,{j,2,Length[headList]}];
WriteString[fileHandle, "\\\\\n"];
WriteString[fileHandle, "\\hline\\hline\n"];
];
