(* ::Package:: *)

(* ::Section:: *)
(*Initialisation and management of the SuSpect$ lists*)


(* ::Subsection::Closed:: *)
(*Version of the SuSpect interface*)


SuSpect$Version="0.2";


(* ::Subsection::Closed:: *)
(*All the lists needed by the interface*)


(* ::Subsubsection::Closed:: *)
(*List of files created by the SuSpect interface*)


SuSpect$Files = {"inc/DerivativeExternal.h",
                 "src/DerivativeExternal.cxx"};


(* ::Subsubsection::Closed:: *)
(*List of the parameters to be evolved*)


SuSpect$Coup = {};


SuSpect$ScalarSoftMasses={};

SuSpect$InoSoftMasses={}; SuSpect$lnInoSoftMasses={};

SuSpect$SoftInteractions1={}; SuSpect$SoftInteractions2={}; SuSpect$SoftInteractions3={};


SuSpect$SuperW1={}; SuSpect$SuperW2={}; SuSpect$SuperlnW2={}; SuSpect$SuperW3={};


(* ::Subsubsection::Closed:: *)
(*List of Abbreviations*)


SuSpect$Abbreviations={};
SuSpect$ToAbbreviations={};


(* ::Subsubsection::Closed:: *)
(*Mapping of the FR names to the SuSpect names*)


SuSpect$Convertion={Conjugate[a_]->a};
SuSpect$ToyVector={};


SuSpect$Dictionary:={};


(* ::Subsection:: *)
(*Initialization of all the lists required by the SuSpect interface*)


(* ::Subsubsection:: *)
(*This function initialize a given set of parameters*)


(* ::Text:: *)
(*It returns one initialiazed list, plus append to the conversion lists the required stuff *)


Options[ListInit] = { yVector -> True };


ListInit[coups_,OptionsPattern[]]:=Module[{MyTable, list,yv},
  yv = OptionValue[yVector];

  (*Pre-process the list *)
  list=Flatten[coups[[All,1]]//.{IndexDelta[__]->1,SUEps[__]->1,-expr_->expr,a_?(NumericQ[#]&)->1}];
  list=Flatten[(ReplaceRepeated[#,{
    ff_[a___,Index[type_,name_],b___]:>MyTable[ff[a,name,b],{name,1,IndexDim[Index[type]]}], 
    MyTable[MyTable[func__],func2_]:>MyTable[func,func2]}]&/@list)/.MyTable->Table];

  (* Add the corresponding entries to SuSpect$Convertion *)
  SuSpect$Convertion=Join[SuSpect$Convertion,(If[MatchQ[#,_[__]],Rule[#,#/.he_[argx__]:>Symbol[StringJoin[ToString[he],Sequence[ToString/@{argx}]]]]]&/@list)];

  (* Add the corresponding entries to SuSpect$ToyVector *)
  list=If[MatchQ[#,_[__]],#/.he_[argx__]:>StringJoin[ToString[he],Sequence[ToString/@{argx}]],ToString[#]]&/@list;
  If[yv,SuSpect$ToyVector=Join[SuSpect$ToyVector,(Rule[#,"yVector[m_"<>#<>"]"]&/@ list)]];

  (* Output *)
list];


(* ::Subsubsection::Closed:: *)
(*This initializes all the lists*)


SuSpectListInit[Sca_,Ino_,Soft_,superW_]:=Module[{},
  (* Coupling constants *)
  SuSpect$Convertion=Join[SuSpect$Convertion,Inner[Rule,GroupToCoup[#]^2&/@MR$GaugeGroupList,Symbol[#]&/@SuSpect$Coup,List]];
  SuSpect$Convertion=Join[SuSpect$Convertion,Inner[Rule,GroupToCoup[#]^4&/@MR$GaugeGroupList,(Symbol[#]^2)&/@SuSpect$Coup,List]];
  SuSpect$ToyVector=Join[SuSpect$ToyVector,Inner[Rule,SuSpect$Coup,"yVector[m_"<>#<>"]"&/@SuSpect$Coup,List]];

  (* Soft susy-breaking scalar parameters *)
  SuSpect$ScalarSoftMasses=ListInit[Sca];
  SuSpect$SoftInteractions3=ListInit[Cases[Soft,{_,a_?(Length[#]===3&)}]];
  SuSpect$SoftInteractions2=ListInit[Cases[Soft,{_,a_?(Length[#]===2&)}]];
  SuSpect$SoftInteractions1=ListInit[Cases[Soft,{_,a_?(Length[#]===1&)}]];

  (* Ino masses *)
  SuSpect$lnInoSoftMasses="ln"<>ToString[#]&/@ Ino;
  SuSpect$InoSoftMasses=ToString[#]&/@ Ino;
  SuSpect$ToyVector=Join[SuSpect$ToyVector,(Rule[#,"yVector[m_"<>#<>"]"]&/@SuSpect$lnInoSoftMasses)];

  (* Superpotential *)
  SuSpect$SuperW1=ListInit[Cases[superW,List[_,a_?(Length[#]===1&)]]];
  SuSpect$SuperW2=ListInit[Cases[superW,List[_,a_?(Length[#]===2&)]],yVector->False];
  SuSpect$SuperlnW2="ln"<>#&/@SuSpect$SuperW2;
  SuSpect$ToyVector=Join[SuSpect$ToyVector,(Rule[#,"yVector[m_"<>#<>"]"]&/@SuSpect$SuperlnW2)];
  SuSpect$SuperW3=ListInit[Cases[superW,List[_,a_?(Length[#]===3&)]]];

  (* Cleaning *)
  SuSpect$Convertion=DeleteCases[SuSpect$Convertion,Null];
];


(* ::Section::Closed:: *)
(*SuSpect output format*)


(* ::Subsection::Closed:: *)
(*C++ format*)


SuSpect$ToCppRules={"Power"->"pow", "Pi"->"m_cpi", "Conjugate"->"conj", "FRExp"->"exp"};


(* ::Subsection::Closed:: *)
(*WriteSuSpectCommentLine*)


(* ::Text:: *)
(*This function writes a comment line in C++, starting with //*)


WriteSuSpectCommentLine[file_, string_String] := WriteString[file, "// " <> string <> "\n"];


(* ::Subsection::Closed:: *)
(*WriteSuSpectLine*)


(* ::Text:: *)
(*This function write a blank line, sdtarting with //*)


WriteSuSpectLine[file_] :=WriteString[file,"//  -------------------------------------------"<>"\n"];


(* ::Subsection::Closed:: *)
(*WriteSuSpectFRHeader*)


(* ::Text:: *)
(*This function writes the header of all produced files.*)


WriteSuSpectFRHeader[file_] := Block[{},
  WriteSuSpectCommentLine[file, "This file was automatically created by FeynRules "<> FR$VersionNumber];
  WriteSuSpectCommentLine[file, "Mathematica version: "<> $Version];
  WriteSuSpectCommentLine[file, "Date: "<> DateString[]];
  WriteString[file, "\n\n"];
];


(* ::Subsection::Closed:: *)
(*Create dictionary between FeynRules and SuSpect for the MSSM*)


CreateFeynRulesToSuSpectDictionary[]:=Module[{tmpnames},
  tmpnames=Table["RGE::g"<>ToString[ii]<>"sq",{ii,1,3}];
  SuSpect$Dictionary = Inner[List,SuSpect$Coup,tmpnames,List];
  SuSpect$Dictionary = Join[SuSpect$Dictionary,Inner[List,SuSpect$SuperlnW2,{"RGE::lnmu"},List]];
  tmpnames=Flatten[Table["RGE::Yd"<>ToString[jj]<>ToString[ii],{ii,1,3},{jj,1,3}]];
  tmpnames=Append[Drop[tmpnames,-1],"RGE::Yb"];
  tmpnames=Flatten[Append[tmpnames,Table["RGE::Ye"<>ToString[jj]<>ToString[ii],{ii,1,3},{jj,1,3}]]];
  tmpnames=Append[Drop[tmpnames,-1],"RGE::Ytau"];
  tmpnames=Flatten[Append[tmpnames,Flatten[Table["RGE::Yu"<>ToString[jj]<>ToString[ii],{ii,1,3},{jj,1,3}]]]];
  tmpnames=Append[Drop[tmpnames,-1],"RGE::Yt"];
  SuSpect$Dictionary = Join[SuSpect$Dictionary,Inner[List,SuSpect$SuperW3,tmpnames,List]];
  tmpnames=Table["RGE::lnM"<>ToString[ii],{ii,1,3}];
  SuSpect$Dictionary = Join[SuSpect$Dictionary,Inner[List,SuSpect$lnInoSoftMasses,tmpnames,List]];
  tmpnames={"RGE::msqHd", "RGE::msqHu","RGE::msqSdownR","RGE::msqSdownRstrangeR", "RGE::msqSdownRbotR", "RGE::msqSstrangeRdownR",
    "RGE::msqSstrangeR", "RGE::msqSstrangeRbotR",  "RGE::msqSbotRdownR", "RGE::msqSbotRstrangeR", "RGE::msqSbotR", "RGE::msqSelR", "RGE::msqSelRmuR", 
    "RGE::msqSelRtauR","RGE::msqSmuRelR", "RGE::msqSmuonR", "RGE::msqSmuRtauR", "RGE::msqStauRelR","RGE::msqStauRmuR", "RGE::msqStauR", "RGE::msqSlepton1L",
    "RGE::msqSlepton12L", "RGE::msqSlepton13L", "RGE::msqSlepton21L", "RGE::msqSlepton2L", "RGE::msqSlepton23L", "RGE::msqSlepton31L", "RGE::msqSlepton32L", 
	"RGE::msqSlepton3L", "RGE::msqSquark1L", "RGE::msqSquark12L", "RGE::msqSquark13L", "RGE::msqSquark21L", "RGE::msqSquark2L", "RGE::msqSquark23L",
    "RGE::msqSquark31L","RGE::msqSquark32L","RGE::msqSquark3L", "RGE::msqSupR","RGE::msqSupRcharmR", "RGE::msqSupRtopR", "RGE::msqScharmRupR", "RGE::msqScharmR",
    "RGE::msqScharmRtopR", "RGE::msqStopRupR", "RGE::msqStopRcharmR","RGE::msqStopR"};  
  SuSpect$Dictionary = Join[SuSpect$Dictionary,Inner[List,SuSpect$ScalarSoftMasses,tmpnames,List]];
  SuSpect$Dictionary = Join[SuSpect$Dictionary,Inner[List,SuSpect$SoftInteractions2,{"RGE::B"},List]];
  tmpnames=Flatten[Table["RGE::Ae"<>ToString[jj]<>ToString[ii],{ii,1,3},{jj,1,3}]];
  tmpnames=Append[Drop[tmpnames,-1],"RGE::Atau"];
  tmpnames=Flatten[Append[tmpnames,Flatten[Table["RGE::Ad"<>ToString[jj]<>ToString[ii],{ii,1,3},{jj,1,3}]]]];
  tmpnames=Append[Drop[tmpnames,-1],"RGE::Ab"];
  tmpnames=Flatten[Append[tmpnames,Flatten[Table["RGE::Au"<>ToString[jj]<>ToString[ii],{ii,1,3},{jj,1,3}]]]];
  tmpnames=Append[Drop[tmpnames,-1],"RGE::At"];
  SuSpect$Dictionary = Join[SuSpect$Dictionary,Inner[List,SuSpect$SoftInteractions3,tmpnames,List]];
];


(* ::Section:: *)
(*DerivativeExternal*)


(* ::Subsection::Closed:: *)
(*Core function for the header file*)


WriteDerivativeExternalHeader[]:=Module[{outfile=OpenWrite["inc/DerivativeExternal.h"]},
  (* Writing header *)
  WriteSuSpectFRHeader[outfile];

  (* Includes *)
  WriteDerivativeExternalIncludes[outfile];

  (* Class definition *)
  WriteDerivativeExternalClassHeader[outfile];

  (* Footer *)
  WriteDerivativeExternalFooterHeader[outfile];

  (* Closing the output file *)
  Close[outfile];
];


(* ::Subsection::Closed:: *)
(*Core function for the cxx file*)


WriteDerivativeExternalCXX[]:=Module[{outfile=OpenWrite["src/DerivativeExternal.cxx"]},
  (* Writing header *)
  WriteSuSpectFRHeader[outfile];

  (* Includes *)
  WriteDerivativeExternalIncludesCXX[outfile];

  (* Constant parameters *)
  WriteDerivativeExternalConstants[outfile];

  (* Destrcutor method *)
  WriteDerivativeExternalDestructor[outfile];

  (* Method associated to the RGE *)
  WriteDerivativeExternalRGE[outfile];

  (* Closing the output files *)
  Close[outfile];
];


(* ::Subsection::Closed:: *)
(*Title*)


WriteDerivativeExternalTitle[file_,text_]:=Module[{},
  WriteString[file,"\n\n"];
  WriteSuSpectLine[file];
  WriteSuSpectCommentLine[file," "<>text];
  WriteSuSpectLine[file];
];


(* ::Subsection::Closed:: *)
(*Includes*)


WriteDerivativeExternalIncludes[file_]:=Module[{},
 WriteString[file,"#ifndef DERIVATIVEEXTERNAL_H\n"];
 WriteString[file,"#define DERIVATIVEEXTERNAL_H\n\n"];
 WriteString[file,"#include <vector>\n"];
 WriteString[file,"#include \"stdlib.h\"\n"];
 WriteString[file,"#include \"DerivativeBase.h\"\n"];
 WriteString[file,"#include \"RgeEvolution_enum.h\"\n\n"];
];


WriteDerivativeExternalIncludesCXX[file_]:=Module[{},
 WriteString[file,"#include <cmath>\n"];
 WriteString[file,"#include \"DerivativeExternal.h\"\n\n"];
];


(* ::Subsection:: *)
(*Defining the class DerivativeExternal*)


WriteDerivativeExternalClassHeader[file_]:=Module[{},
 WriteString[file,"namespace SUSPECT{\n\n"];
 WriteString[file,"  class DerivativeExternal :public DerivativeBase {\n"];

 (* public *)
 WriteString[file,"\n   public:\n"];
 WriteString[file,"     DerivativeExternal(SLHA4suspect *);\n"];
 WriteString[file,"     ~DerivativeExternal();\n"];
 WriteString[file,"     std::vector<double> Execute(double , std::vector<double>, std::vector<bool>);\n"];

 (* Private *)
 WriteString[file,"\n   private:\n"];
 WriteString[file,"      unsigned int m_"<>#[[1]]<>";\n"]&/@SuSpect$Dictionary;

 (* Closing *)
 WriteString[file,"\n  };\n\n"];
 WriteString[file,"}\n\n"];
];


(* ::Subsection:: *)
(*Constant parameters in the cxx file (including the dictionary)*)


WriteDerivativeExternalConstants[file_]:=Module[{},
  WriteString[file, "\nSUSPECT::DerivativeExternal::DerivativeExternal(SLHA4suspect *theSLHAblock): DerivativeBase(theSLHAblock){\n\n"];
  WriteString[file, "  m_", #[[1]]," = ", #[[2]]<>";\n"] &/@SuSpect$Dictionary;
  WriteString[file,"\n}\n\n"];
];


(* ::Subsection::Closed:: *)
(*Destructor method in the cxx file*)


WriteDerivativeExternalDestructor[file_]:=Module[{},
  WriteString[file, "\nSUSPECT::DerivativeExternal::~DerivativeExternal(){}\n\n"];
];


(* ::Subsection:: *)
(*RGE routine in the cxx file*)


(* ::Subsubsection::Closed:: *)
(*Core function*)


WriteDerivativeExternalRGE[file_]:=Module[{},
  WriteString[file, "\nstd::vector<double> SUSPECT::DerivativeExternal::Execute(double x, std::vector<double> yVector, std::vector<bool> RGEon){\n\n"];

  (* Declarations *)
  WriteDerivativeExternalTitle[file, "Declarations"];
  WriteString[file, "  std::vector<double> dydx;\n"];
  WriteString[file, "  dydx.resize(yVector.size());\n"];
  WriteDerivativeExternalParams[file];

  (* Coefficients of the beta functions *)
  WriteDerivativeExternalTitle[file,"Coefficients of the beta functions"];
  WriteDerivativeExternalBeta[file];

  (* RGEs *)
  WriteDerivativeExternalTitle[file,"List of the RGEs"];
  WriteDerivativeExternalRGEMain[file];

  (* Footer *)
  WriteDerivativeExternalTitle[file," Returning the result"];
  WriteString[file, "  return dydx;\n"];
  WriteString[file,"}\n\n"];
];


(* ::Subsubsection::Closed:: *)
(*Parameters and abbreviations*)


WriteDerivativeExternalParams[file_]:=Module[{rules=Join[SuSpect$ToCppRules,SuSpect$ToyVector]},
  (* Linking parameters and their logs *)
  WriteSuSpectCommentLine[file, "Linking parameters and their logs"];
  Logify[SuSpect$InoSoftMasses,SuSpect$lnInoSoftMasses,file];
  Logify[SuSpect$SuperW2,SuSpect$SuperlnW2,file];
  
  (* Abbreviations *)
  WriteString[file,"\n"];
  WriteSuSpectCommentLine[file,"Declaration of the abbreviations"];
  WriteString[file,"  double ",ToString[#[[1]]]," = ",
    StringReplace[ToString[CForm[#[[2]]/.{Index[_,a_?(NumericQ[#]&)]->a}//.SuSpect$Convertion]],rules],";\n"]&/@SuSpect$Abbreviations;
  WriteString[file,"\n"];
];


Logify[masses_,lnmasses_,file_]:=Module[{tmplist,rules=Join[SuSpect$ToCppRules,SuSpect$ToyVector]},
  tmplist=(FRExp[Symbol[#]]&/@lnmasses)//.SuSpect$Convertion;
  tmplist=Inner[StringJoin,"  double "<>#<>"="&/@masses,StringReplace[ToString/@(CForm[#]&/@tmplist),rules],List];
  WriteString[file, #,";\n"]&/@tmplist;
];


(* ::Subsubsection:: *)
(*Coefficients of the beta functions*)


WriteDerivativeExternalBeta[file_]:=Module[{},
(* Declaration *)
  WriteString[file, "  std::vector<double> beta0;\n  beta0.resize((unsigned int)RGE::last,0.);\n\n"];

(* Gauge sector *)
  WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the gauge couplings",SuSpect$Coup,FR$betag1];
  WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the gaugino masses",SuSpect$lnInoSoftMasses,FR$betaMi1];

(* Scalar soft SUSY breaking sector *)
  WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the scalar soft mass terms",SuSpect$ScalarSoftMasses,FR$betaMs1];
  If[Length[SuSpect$SoftInteractions3]=!=0,
    WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the trilinear soft interaction terms",SuSpect$SoftInteractions3,FR$betaSI31]];
  If[Length[SuSpect$SoftInteractions2]=!=0,
    WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the bilinear soft interaction terms",SuSpect$SoftInteractions2,FR$betaSI21]];
  If[Length[SuSpect$SoftInteractions1]=!=0,
    WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the linear soft interaction terms",SuSpect$SoftInteractions1,FR$betaSI11]];

(* Superpotential sector *)
  If[Length[SuSpect$SuperW3]=!=0,
    WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the trilinear superpotential terms",SuSpect$SuperW3,FR$betaSW31]];
  If[Length[SuSpect$SuperlnW2]=!=0,
    WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the bilinear superpotential terms",SuSpect$SuperlnW2,FR$betaSW21]];
  If[Length[SuSpect$SuperW1]=!=0,
    WriteDerivativeExternalBeta1[file,"First coefficient of the beta functions of the linear superpotential terms",SuSpect$SuperW1,FR$betaSW11]];
];


WriteDerivativeExternalBeta1[file_,title_,prm_,beta1s_]:=Module[{tmplist,tmpbeta},
  (* Preparing the beta functions *)
  tmpbeta=beta1s/.{Index[_,a_?(NumericQ[#]&)]->a}//.SuSpect$Convertion;
  (* Outputting *)
  WriteSuSpectCommentLine[file," "<>title];
  tmplist=Inner[StringJoin,"  beta0[m_"<>#<>"]="&/@prm, StringReplace[ToString/@(CForm[#]&/@tmpbeta),Join[SuSpect$ToCppRules,SuSpect$ToyVector]],List];
  WriteString[file,#,";\n"]&/@tmplist;
  WriteString[file,"\n"];
];


(* ::Subsubsection:: *)
(*RGEs*)


WriteDerivativeExternalRGEMain[file_]:=Module[{},
(* Gauge sector *)
  WriteDerivativeExternalRGEs[file,"RGEs of the gauge coupling constants",SuSpect$Coup];
  WriteDerivativeExternalRGEs[file,"RGEs of the gaugino masses",SuSpect$lnInoSoftMasses];

(* Scalar soft terms *)
  WriteDerivativeExternalRGEs[file,"RGEs of the scalar soft mass terms", SuSpect$ScalarSoftMasses];
  If[Length[SuSpect$SoftInteractions3]=!=0,WriteDerivativeExternalRGEs[file,"RGEs of the trilinear soft interaction terms",SuSpect$SoftInteractions3]];
  If[Length[SuSpect$SoftInteractions2]=!=0,WriteDerivativeExternalRGEs[file,"RGEs of the bilinear soft interaction terms",SuSpect$SoftInteractions2]];
  If[Length[SuSpect$SoftInteractions1]=!=0,WriteDerivativeExternalRGEs[file,"RGEs of the linear soft interaction terms",SuSpect$SoftInteractions1]];

(* Superpotential *)
  If[Length[SuSpect$SuperW3]=!=0,WriteDerivativeExternalRGEs[file,"RGEs of the trilinear superpotential terms",SuSpect$SuperW3]];
  If[Length[SuSpect$SuperlnW2]=!=0,WriteDerivativeExternalRGEs[file,"RGEs of the bilinear superpotential terms",SuSpect$SuperlnW2]];
  If[Length[SuSpect$SuperW1]=!=0,WriteDerivativeExternalRGEs[file,"RGEs of the linear superpotential terms",SuSpect$SuperW1]];
];


WriteDerivativeExternalRGEs[file_,title_,prm_]:=Module[{},
  WriteSuSpectCommentLine[file," "<>title];
  WriteString[file,"  if (RGEon[m_"<>#<>"]) dydx[m_"<>#<>"] = beta0[m_"<>#<>"]*m_cpi;\n"]&/@ prm;
  WriteString[file,"\n"];
];


(* ::Subsubsection::Closed:: *)
(*Footer*)


 WriteDerivativeExternalFooterHeader[file_]:=Module[{},
  WriteString[file,"#endif\n"];
];


(* ::Section:: *)
(*Calculation of the coefficients of the beta functions*)


(* ::Subsection::Closed:: *)
(*Main function*)


(* ::Text:: *)
(*This function computes all the beta functions in a form ready to be written to cxx files.*)


CalculateBetaFunctions[LScaMass_,InoMasses_,LSoft_,LSuperW_]:=Module[{},
(* I. Gauge coupling constants and gaugino masses *)
  GaugeBetaFunction[InoMasses];

(* II. Soft scalar mass beta functions *)    
  ScalarMassBetaFunction[LScaMass,InoMasses];

(* III. Soft interactions beta functions *)    
  SoftInteractionBetaFunction[LSoft,InoMasses];

(* IV. Superpotential beta functions *)    
  SuperWBetaFunction[LSuperW];
];


(* ::Subsection:: *)
(*The beta functions themselves*)


(* ::Subsubsection::Closed:: *)
(*Calculation of the gauge coupling beta functions and the gaugino mass terms beta functions (in the SuSpect conventions)*)


GaugeBetaFunction[inomasses_]:= Module[{}, 
  GaugeCouplingsRGE[];
  FR$betaMi1=Inner[Times,2/GroupToCoup/@MR$GaugeGroupList,FR$betag1,List];  
  FR$betag1=Inner[Times,2*GroupToCoup/@MR$GaugeGroupList,FR$betag1,List];
];


(* ::Subsubsection::Closed:: *)
(*Calculation of the scalar soft mass beta functions*)


ScalarMassBetaFunction[sca_,ino_]:=Module[{}, 
  (* Calculation of the beta functions *)
  ComputeBetaMs1[sca,ino];

 (* Processing the format so that it becomes compatible with suspect 3 *)
  FR$betaMs1=FormatizeBeta[sca,FR$betaMs1];
];


(* ::Subsubsection::Closed:: *)
(*Calculation of the soft interactions beta functions*)


SoftInteractionBetaFunction[soft_,ino_]:=Module[{},
  (* Calculation of the beta functions *)
  ComputeBetaSoftInt31[Cases[soft,{_,a_?(Length[#]===3&)}],ino];
  ComputeBetaSoftInt21[Cases[soft,{_,a_?(Length[#]===2&)}],ino];
  ComputeBetaSoftInt11[Cases[soft,{_,a_?(Length[#]===1&)}]];

 (* Processing the format so that it becomes compatible with suspect 3 *)
  FR$betaSI31=FormatizeBeta[Cases[soft,{_,a_?(Length[#]===3&)}],FR$betaSI31];  
  FR$betaSI21=FormatizeBeta[Cases[soft,{_,a_?(Length[#]===2&)}],FR$betaSI21];  
  FR$betaSI11=FormatizeBeta[Cases[soft,{_,a_?(Length[#]===1&)}],FR$betaSI11];
];


(* ::Subsubsection::Closed:: *)
(*Calculation of the superpotential beta functions*)


SuperWBetaFunction[superw_]:=Module[{prmlist,l1,l2,l3},
  (*Initialization *)
  l1=Cases[superw,{_,a_?(Length[#]===1&)}];
  l2=Cases[superw,{_,a_?(Length[#]===2&)}];
  l3=Cases[superw,{_,a_?(Length[#]===3&)}];
  prmlist=l2[[All,1]]/.{_?(NumericQ[#]&)->1, SUEps[__]->1,IndexDelta[__]->1};

  (* Calculation of the beta functions *)
  ComputeBetaSuperW31[l3];
  ComputeBetaSuperW21[l2];
  ComputeBetaSuperW11[l1];

  (* From the parameter RGE the its log RGE *)
  FR$betaSW21=Expand[#]&/@(Inner[Times,FR$betaSW21,1/#&/@prmlist,List]);

 (* Processing the format so that it becomes compatible with suspect 3 *)
  FR$betaSW31=FormatizeBeta[l3,FR$betaSW31];  
  FR$betaSW21=FormatizeBeta[l2,FR$betaSW21];  
  FR$betaSW11=FormatizeBeta[l1,FR$betaSW11];
];


(* ::Subsection:: *)
(*Formatting*)


(* ::Subsubsection:: *)
(*Core function*)


FormatizeBeta[soft_,expression_]:=Module[{tmpp},
  tmpp=Inner[Equal,FR$D/@soft[[All,1]],expression,List];
  tmpp=tmpp//.{
    Equal[FR$D[-a_],expre_]:>Equal[FR$D[a],Expand[-expre]],
    Equal[FR$D[muf_?(NumericQ[#]&)*a_],expre_]:>Equal[FR$D[a],Expand[expre/muf]],
    Equal[FR$D[func_?(#===IndexDelta||#===SUEps&)[argx__] expr_],expre_]:>Equal[FR$D[expr],Expand[expre/ func[argx]]]};
  tmpp=ReplaceAll[tmpp,SUEps[aaa__]:>Signature[{aaa}] SUEps[Sequence@@Sort[{aaa}]]];
  tmpp=(#/.Equal[FR$D[a_],b_]:> Equal[a,OptimizeIndex[b]])&/@tmpp;

  (* Expand all the indices and create abbreviations *)
  tmpp=Flatten[If[MatchQ[#[[1]],_[__]],NumerizeMatrixIndices[Sequence@@#],#[[2]]]&/@tmpp];
  tmpp=Abbreviate[#]&/@tmpp;
tmpp];


(* ::Subsubsection::Closed:: *)
(*Expansion of the matrix indices*)


(* ::Text:: *)
(*This function transform a RGE about a generic matrix element onto RGEs over all possible matrix elements*)


NumerizeMatrixIndices[quantity_,beta_]:=Module[{MyTable,indlist},
(* Gets the idices to be exapanded *)
  indlist=List@@quantity;
  indlist={#/.Index[_,bbb_]->bbb,1, IndexDim[#/.Index[bbb_,_]->bbb]}&/@indlist;
(* Do the expansion *)
Flatten[(MyTable[beta,Sequence@@indlist])/.MyTable->Table]];


(* ::Subsubsection::Closed:: *)
(*Management  of the abbreviations*)


Abbreviate[Plus[aa_,bbb__]]:=Abbreviate[aa]+Abbreviate[Plus[bbb]];


Abbreviate[exp_?(Head[#]=!=Plus&)]:=Module[{dum,rest},
  dum=exp/.Times[aaa___,aa_?(NumericQ[#]&),bbb___]->Times[aaa,bbb]; 
  rest=exp/dum;
rest*Abbreviationize[dum]];


Abbreviationize[exp_]:=Module[{abname,result,IndList,theab,MyTable},
  result=exp/.SuSpect$ToAbbreviations;
  (* Check if the abbreviation is not existing already *)
  If[ (Head[result]===Times) || (Head[result]=!=Times && result===exp),
    abname=Unique["SU"];
    SuSpect$ToAbbreviations=Append[SuSpect$ToAbbreviations,Rule[exp,abname]];
    IndList=KillDoubles[ToIndexList[result]];
    IndList=IndList/.Index[type_,bbb_]:> List[bbb,1,IndexDim[Index[type]]];
    theab=MyTable[result,IndList]/.{MyTable[aaa_,{}]->aaa,MyTable[aaa_,muf_]:>MyTable[aaa,Sequence@@muf]}/.MyTable->Table;
    If[Head[theab]===List,theab=Plus@@Flatten[theab]];
    SuSpect$Abbreviations=Append[SuSpect$Abbreviations,Rule[abname,theab]];
    result=result/.SuSpect$ToAbbreviations];
result];


(* ::Section::Closed:: *)
(*Main routine*)


Options[WriteSuSpectOutput] = {
  Output :> StringReplace[M$ModelName <> "_SuSpect", {" " -> "_"}]
};


WriteSuSpectOutput[lsoft_,superpot_,OptionsPattern[]]:=Module[{olddir=Directory[],LScaMass,InoMasses,SoftInt,superW,MyTable},
  (* Initialization *)
  SuSpect$Coup=ToString[GroupToCoup[#]]<>"sq"&/@MR$GaugeGroupList;
  superW=ExtractSuperWTerms[superpot,SuperfieldQ]; 
  TestSuperW[superW];
  {LScaMass,InoMasses,SoftInt}=SoftParameters[lsoft];
  SuSpectListInit[LScaMass,InoMasses,SoftInt,superW];
  CreateFeynRulesToSuSpectDictionary[];
  
  (* Priting header *)
  Print[" --- FeynRules interface to SuSpect 3 version " <> SuSpect$Version <> " ---"];
  Print["     A. Alloul, B. Fuks, M. Rausch de Traubenberg, 2011"];
  Print["     arXiv:yymm.nnnn"];

  (* Setting up the output directory *)
  If[Not[MemberQ[FileNames[], OptionValue[Output]]], CreateDirectory[OptionValue[Output]]];
  SetDirectory[OptionValue[Output]];
  If[Not[MemberQ[FileNames[], "inc"]], CreateDirectory["inc"]];
  If[Not[MemberQ[FileNames[], "src"]], CreateDirectory["src"]];
  DeleteFileIfExists /@ SuSpect$Files;

  (* Calculating the renormalization group equations and computing the beta functions *)
  Print[Style["Computing the coefficients of the beta functions...",Orange]];
  CalculateBetaFunctions[LScaMass,InoMasses,SoftInt,superW];

  (* Writing the list of quantities to be evolved *)
  Print[Style["Creating header files...",Orange]];
  WriteDerivativeExternalHeader[];

  Print[Style["Creating c++ files...",Orange]];  
  WriteDerivativeExternalCXX[];

  (* Getting back to the initial directory *)
  SetDirectory[olddir];
  Print["Done."];
];
