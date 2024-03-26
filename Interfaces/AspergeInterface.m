(* ::Package:: *)

M$InterfaceVersion=0.8.1;
$ASperGeDir = "";


(* ::Text:: *)
(*Changelog:*)
(*   22.07.2013: Fixed interface so that the C++ code returns both mixing matrices for right-handed and left-handed fermions*)
(*   08.05.2013: Adapted to the new format of ASperGe after incompatibility problems with recent versions of gcc*)
(*   27.08.2013: Fixed a rule in DefineParam routine.*)
(*                       CheckDefinitions rewritten in order to allow for partial declaration of mixings.*)
(*                       *)
(* *)


(* ::Section:: *)
(*Useful tools*)


(* ::Subsection::Closed:: *)
(*IndexifyParameter*)


(* ::Text:: *)
(*This function adds the necessary indices to every nude parameter*)


IndexifyParameter[param_?(ListQ[$IndList[#]]&)]:=param[Sequence@@($IndList[param]/.Index[type_]:>Index[type,Unique["indu"]])];
IndexifyParameter[param_?(ListQ[$IndList[#]]===False&)]:=param;


(* ::Subsection:: *)
(*ExpandAndDefine*)


(* ::Text:: *)
(*This function simply adds the corresponding indices to a given parameter and expands these indices. Takes as input a definition in the format*)
(*Equal[ parameter, declaration]*)


ExpandParam[def_]:=Block[{tmpp},
  (*Put and expand indices*)
  tmpp=def/.Equal[a_,b__]:>Equal[IndexifyParameter[a],b];
  tmpp=tmpp/.Equal[a_[indu__],b__]:>MyTable[Equal[a[indu],b],Sequence@@(List@(Sequence@indu)/.Index[typ_,name_]:>{name,Length[IndexRange[Index[typ]]]})];
  tmpp=Flatten[tmpp/.MyTable->Table]/.Index[typ_,name_]:>name;
  tmpp=If[tmpp===def,{tmpp},tmpp];
  Return[tmpp];
];


(* ::Text:: *)
(*This function processes the parameters' declarations and only keeps the relevant inforamtion for the definitions. *)
(*	Input: a definition with one of the following options {BlockName and Orderblock, BlockName and Indices, Values and/or Definitions}. As input there should not be definitions/values and  blocks for a parameter.*)
(*	Output: List[ par, value] Or List[par, blockname, {ind} ]*)


DefineParam[def_]:=Block[{tmpp}, 
  Which[
    (*Parameters with no indices and a block*)
    Position[def[[2]],BlockName]=!={}&&Position[def[[2]],OrderBlock]=!={}, 
      tmpp=List[def[[1]],OptionValue[def[[2]],BlockName], List[OptionValue[def[[2]],OrderBlock]]],
    (*Parameters with blocks and indices*)
    Position[def[[2]],BlockName]=!={}&&Position[def[[2]],Indices]=!={},
      tmpp=ExpandParam[def];
     tmpp=tmpp/.Equal[a_[indu__],list_]:>List[a[indu],OptionValue[list,BlockName],List[indu]],
    (*Definitions and/or Value no block  *)
    Position[def[[2]],BlockName]=={}&&Position[def[[2]],a_?(#==Value||#==Definitions&)]=!={} ,
      tmpp=Equal[def[[1]],DeleteCases[def[[2]],Rule[Indices,_]]];
(*AA: 08.27.13 > Modified this rule to have it working with more cases. *)
   tmpp=#//.{
     Equal[par_,list_]:>If[(ApplyDefinitions[par]/.Index[typ_,indu_]:>indu)===par,List[par,OptionValue[list,Value]],List[par,ApplyDefinitions[par]]],
     List[par_,list_?(ListQ[#]&)]:>List[par,ReplaceAll[par,list]]}&/@ExpandParam[tmpp];
  ];
  (*Convert integers to reals*)
  tmpp=Expand[tmpp]/.List[par_?(IntegerQ[#]=!=True&),num_Integer]:>List[par,N[num,30]];
  (*Check if the parameter is real or not*)
  tmpp=tmpp/.{List[a_?(CnumQ[#]&),b_?(ListQ[#]=!=True&),c___]:>List[a,b,c,Complex],List[a_?(CnumQ[#]=!=True&),b_?(ListQ[#]=!=True&),c___]:>List[a,b,c,Real]};
  Return[tmpp]];


(* ::Subsection:: *)
(*Check definitions*)


(* ::Text:: *)
(*AA. 08.27.13 > This function has been re-written to be more generic. Now it only tests if one of the masses to be computed in M$MixingsDescription appears in MR$Paremeters. If yes, calculations are aborted. This is more general than the previous version in the sense that it allows to have mixings defined in M$MixingsDescription and others in M$ClassesDescription.*)


CheckDefinitions[def_]:=Block[{masslist={},fieldmasses={}},
  (*fieldmasses contains the masses of the fields present in M$MixingsDescription*)
  fieldmasses=Mass/@Cases[Flatten[M$MixingsDescription[[All,2]]/.Rule[a_,b_]:>List[a,b]],_?FieldQ];
  (*We check that none of the masses in fieldmasses is considered as an input in MR$Parameters. *)
  If[MemberQ[fieldmasses,(#[[1]]/.a_[ind__]:>a)], AppendTo[masslist,#[[1]]/.a_[ind__]:>a]]&/@def;
  If[masslist=!={},
    Print["The mass parameters ",masslist," should not appear in M$Parameters. Calculations aborted"];Abort[];]
  ];



(* ::Subsection:: *)
(*DefinitionsToStrings*)


(* ::Text:: *)
(*Function to transform the definitions into strings*)
(*Input: one definition at a time. Can be *)
(*	1- Equal[ param , whatever] *)
(*	2- param*)
(*	3- whatever*)
(*Output: the string version of the format compatible with the c++ code*)


DefinitionsToStrings[def_]:=Block[{TransformParamsNames,tmpdef,tmpp},
  (*this rule is to transform something like yu[1,1] to yu1x1*)
  TransformParamsNames={"{"->"",","->"x"," "->"","}"->""};
  tmpdef=def/.par_?(MemberQ[M$Parameters[[All,1]],#]&)[ind__]:>ToExpression[ToString[par]<>StringReplace[ToString[List@ind],TransformParamsNames]];
  tmpdef=If[MatrixQ[def], FormatOutput[tmpdef], tmpp=tmpdef/.List[par_,value_,typ_?(#===Complex || #===Real&),des_]:>List[par,FormatOutput[value],typ,des]; #/.a_?(StringQ[#]=!=True&):>ToString[a] &/@tmpp];
  Return[tmpdef]];


(* ::Subsection::Closed:: *)
(*Get Parameter description*)


GetParamDescription[param_]:=Return[(Cases[M$Parameters,_?(MatchQ[#,Equal[param/.a_[ind__]:>a,rule__]]&)]/.Equal[_,b__]:>(Description/.b)/.Description->"No description found")[[1]] ];


(* ::Subsection::Closed:: *)
(*Format Output*)


(* ::Text:: *)
(*This function adds to every parameter in the right-hand side of the equality the strings ->value. Example*)
(*  yl1x1 = new Par((sqrt(2)*(yme -> value))/(vev -> value));*)
(*  	Input: whatever as long as it is a string. *)
(*  	Output: If "whatever" belongs to M$Parameters, than it adds to it a " -> getValue() " and the final output is a string in CForm*)


FormatOutput[def_]:=Block[{MyArrow,tmpp,CRules},
  tmpp=Expand[def]/.n_?(IntegerQ[#]||NumericQ[#]&)*pars__:>N[n,30]*pars;  
 (*some definitions for MyArrow function. This is used to add an " -> getValue()" to every parameter*)
  MyArrow=Unique["Arrow"];
  MyArrow[pars__?(Head[#]===Plus &),value]:=(MyArrow[#,value]&/@pars); MyArrow[pars__?(Head[#]===Times&),value]:=(MyArrow[#,value]&/@pars); MyArrow[par_?(NumericQ[#]&),value]:=par; MyArrow[a_?(ListQ[#]&),value]:=(MyArrow[#,value] &/@a);MyArrow[Power[par_,x_],value]:=Power[MyArrow[par,value],x];
  MyArrow[Conjugate[par_],value]:=Conjugate[MyArrow[par,value]];
  MyArrow[func_[par_?(MemberQ[MR$Parameters[[All,1]],#]&)],value]:=func[MyArrow[par,value]];
  (*CForm doesn't seem to work that well, so some extra replacement rules need to be provided*)
  CRules={"1/"->"1./","Power"->"pow","Pi"->"M_PI","Sqrt("~~n:DigitCharacter:>"sqrt("<>ToString[n]<>".","Sqrt"->"sqrt","Cos"->"cos","Sin"->"sin","Tan"->"tan","ArcTan"->"atan","Complex"->"complex<double>","Conjugate"->"conj"};
  (*Apply MyArrow to the def*)
  tmpp=MyArrow[tmpp,value];
  (*CForm doesn't seem to work that well, so some extra replacement rules need to be provided*)
  tmpp=If[MatrixQ[tmpp],
    StringReplace[StringReplace[ToString/@(CForm/@#),CRules],{ToString[MyArrow]<>"("->"",",value)"->"->getValue()"}]&/@tmpp,
    StringReplace[StringReplace[ToString[CForm[tmpp]],CRules],{ToString[MyArrow]<>"("->"",",value)"->"->getValue()"}]];
  Return[tmpp]];


(* ::Subsection::Closed:: *)
(*Define blocks*)


(* ::Text:: *)
(*takes in input the list of the parameters defined with a block and the name of the block to define it returns*)
(*{name, SLHABlockxxx, range of the orderblock, number of indices}*)
(*one remark concerning the range of the orderblock, it is calculted by looking at the parameter that has the largest orderblock. *)


DefineBlocks[blockdefined_,block_]:=Block[{tmpp},
  tmpp=Cases[blockdefined,List[par_,block,ind_,_,_]][[All,3]];
  tmpp=List[block,Unique["SLHABlock"],Max[tmpp],Length[tmpp[[1]]]];
  Return[tmpp]];


(* ::Subsection:: *)
(*Checking hermiticity*)


CheckMatrices[]:=Block[{matrix,checkhermiticity},
  checkhermiticity[matrix_,id_]:=Block[{},
    If[matrix[[1,2]]=!=Conjugate[matrix[[2,1]]]&&matrix[[1,2]]=!=Conjugate[matrix[[1,1]]],
       Print["The mixing ",id," leads to a non hermitian matrix. Please check it"];Abort[]]];
  Which[
    Type[#]==="FLR", checkhermiticity[MassMatrix[#,"L"].ConjugateTranspose[MassMatrix[#,"L"]],#],
    Type[#]==="CWeyl",checkhermiticity[MassMatrix[#].ConjugateTranspose[MassMatrix[#]],#],
    Type[#]==="SPS",checkhermiticity[MassMatrix[#,"PS"],#],
    True,
      If[And@@(WeylFieldQ/@MassBasis),
        checkhermiticity[MassMatrix[#].ConjugateTranspose[MassMatrix[#]],#],
        checkhermiticity[MassMatrix[#],#]]]&/@FR$MassMatrices;
  Return[]];


(* ::Section:: *)
(*Write the files*)


(* ::Subsection::Closed:: *)
(*WriteFiles function*)


(* ::Text:: *)
(*This function writes the headers of the different files.*)
(*If the option DiagMass is set to True, than the mass matrices are defined.*)


Options[WriteFiles]={DiagMass->True};


WriteFiles[OptionsPattern[]]:=Block[{cppstream,hppstream,stream,mainstream},
    (*Nice introduction in all files*)
  cppstream=OpenWrite["src/Parameters.cpp"];
  hppstream=OpenWrite["inc/Parameters.hpp"];
  If[OptionValue[DiagMass],mainstream=OpenWrite["main.cpp"]; 
  stream=List[cppstream,hppstream,mainstream],stream=List[cppstream,hppstream]];
  WriteString[#,"////////////////////////////////////////////////////////////////////////////////\n"]&/@stream;
  WriteString[#,"//                                                                            //\n"]&/@stream;
  WriteString[#,"// Automated mass matrix diagonalization                                      //\n"]&/@stream;
  WriteString[#,"// A. Alloul, K. De Causmaecker, B. Fuks                                      //\n"]&/@stream;
  WriteString[cppstream,"// Cpp source file for the parameters of the model "<>M$ModelName<>" //\n"];
  WriteString[hppstream,"// Cpp header file for the parameters of the model "<>M$ModelName<>" //\n"];
  If[OptionValue[DiagMass],WriteString[mainstream,"//  Main file for the model "<>M$ModelName<>" //\n"];];   
  WriteString[hppstream,"// File generated by FeynRules "<>FR$VersionNumber<>"                                          //\n"];
  WriteString[cppstream,"// File generated by FeynRules "<>FR$VersionNumber<>"                                         //\n"];
  If[OptionValue[DiagMass],WriteString[mainstream,"// File generated by FeynRules "<>FR$VersionNumber<>"                                         //\n"]];
  WriteString[#,"//                                                                            //\n"]&/@stream;
  WriteString[#,"////////////////////////////////////////////////////////////////////////////////\n\n"]&/@stream;
  Close[#]&/@stream; 
  If[OptionValue[DiagMass],DefineParameters[DiagMass->True];DefineMassMatrices[],DefineParameters[]];
];


(* ::Subsection:: *)
(*Define the parameters*)


(* ::Text:: *)
(*This functions is used to write in the files Parameters.cpp and Parameters.hpp all the needed informations to have every parameter defined. *)
(*If DiagMass is set to False, there will be no room for mass matrices definitions in the latter files*)


Options[DefineParameters]={DiagMass->False};


DefineParameters[OptionsPattern[]]:=Block[{def,blockdefined,blockrule,mixingslist,parameterslist,hppstream,cppstream,stream,blocklist},
  (*Initialization*)
  (*If both block and value are provided, only block is kept*)
  def=#/.Equal[par_,List[rule1___,Rule[BlockName,block_],rule3___,Rule[toto_?(#===Definitions||#===Value&),value_],rule5___]]:>Equal[par,List[rule1,Rule[BlockName,block],rule3,rule5]]&/@M$Parameters;
 (*Only keep relevant information*)
  def=#//.Equal[a__,List[opt1___,Rule[name_?(!MemberQ[{BlockName,OrderBlock,Definitions,Value,Indices},#]&),value_],opt3___]]:>Equal[a,List[opt1,opt3]] &/@def;
  (*Do not keep the mass matrices*)
  (*It is important to note here that a mixing matrix appearing in both M$MixingsDescription AND MR$Parameters is supposed
    to be known and provided by the user. It will be thus treated as a known parameter *)
  mixingslist=Flatten[FilterRules[M$MixingsDescription[[All,2]],MixingMatrix]/.{Rule[_,a_]:>a,a_?(#===_&):>{}}];
  mixingslist=DeleteCases[mixingslist,a_?(MemberQ[Intersection[mixingslist,MR$Parameters[[All,1]]],#]&)];
  mixingslist=Flatten[List[#,ToExpression["R"<>ToString[#]],ToExpression["I"<>ToString[#]]]&/@mixingslist];
  def=DeleteCases[def,Equal[a_?(MemberQ[mixingslist,#]&),def__]];
  def=#/.List[list_?(ListQ[#]&),list2___]:>Sequence@@List[list,list2]&/@(DefineParam/@def);
  (*Add parameters descriptions if any*)
  def=Join[#,{GetParamDescription[#[[1]]]}]&/@def;
  (*Checking definitions*)
  CheckDefinitions[def];

  (*for parameters defined in a paramcard.dat*)
  blockdefined=Union[Cases[def,List[_,_,_,_,_]]];
  blocklist=Union[blockdefined/.List[par_,blockname_,ind_,typ_,des_]:>blockname];
  blocklist=DefineBlocks[blockdefined,#]&/@blocklist;
  blockrule=Rule[WordBoundary~~ToString[#[[1]]]~~WordBoundary, ToString[#[[2]]]]&/@blocklist;
 (*Transform the definitions into strings*)
  def=DefinitionsToStrings[#]&/@def;
  (*Create and open files for writing*)
  hppstream=OpenAppend["inc/Parameters.hpp"];
  cppstream=OpenAppend["src/Parameters.cpp"];
  stream={hppstream,cppstream};
  (*The .hpp file*)
  WriteString[hppstream,"#ifndef PARAMETERS_HPP \n#define PARAMETERS_HPP \n\n"];
  WriteString[hppstream,"#include \"headers.hpp\" \n#include \"SLHABlock.hpp\" \n#include \"Par.hpp\"\n#include \"CPar.hpp\" \n#include \"RPar.hpp\"\n\n"];
  WriteString[hppstream, "///class storing and treating all the parameters of the model\n"];
  WriteString[hppstream,"class Parameters \n{ \n"];
  WriteString[hppstream,"    public:\n"];
  WriteString[hppstream,"        ///empty constructor\n        Parameters(){};\n        ///calling this functions will initialise the parameters\n        void init();\n\n"];
  WriteString[hppstream,"\n"];
  WriteString[hppstream,"        //Declaration of the SLHA Blocks\n"];
  (WriteString[hppstream,"        ///SLHA block "<>ToUpperCase[ToString[#[[1]]]]<>"\n"];
  WriteString[hppstream,"        SLHABlock * "<>ToString[#[[2]]]<>";\n"];)&/@blocklist;
  WriteString[hppstream,"\n"];
  WriteString[hppstream,"        ///vector storing the blocks that provide the output \n"];
  WriteString[hppstream,"        vector<SLHABlock*> OutBlocks;\n"];
  WriteString[hppstream,"\n"];
  WriteString[hppstream,"        ///flag for debugging\n        static bool debug;\n\n"];
  WriteString[hppstream,"        //Declaration of the parameters\n"];
  If[#[[-2]]==="Complex",
  WriteString[hppstream," /// "<>Last[#]<>"\n       CPar * "<> First[#] <>";  \n"],
  WriteString[hppstream,"  ///"<>Last[#]<>"\n      RPar * "<> First[#]<>";   \n"]]&/@ def;
  (*The .cpp file*)
  WriteString[cppstream,"#include \"Parameters.hpp\"\n\n"];
  WriteString[cppstream,"bool Parameters::debug = false;\n"];
  WriteString[cppstream,"void Parameters::init()\n{\n    if(debug)\n"];
  WriteString[cppstream,"        printLog(\"Constructing the parameters (Parameters.cpp line \", __LINE__, \")\\n\");\n\n"];
  WriteString[cppstream,"    //Construction of the block format: SLHABlocks(\"name\",number of indices, size)\n"];
  (WriteString[cppstream, "    "<>ToString[#[[2]]]<>" = new SLHABlock(\""<>ToUpperCase[ToString[#[[1]]]]<>"\","<>ToString[#[[4]]]<>","<>ToString[#[[3]]]<>");\n"];
  WriteString[cppstream,"     OutBlocks.push_back("<>ToString[#[[2]]]<>");\n"];)&/@blocklist;
  WriteString[cppstream,"      if(debug)\n        printLog(\"  * SLHABlocks constructed\\n  * intialize parameters\\n\");\n"]; 
  (*Declare every parameter*)
  WriteString[cppstream,"\n"];
  WriteString[cppstream,"    //Initialisation of the parameters format:
    // Par(block, index1, index2, index3) (index2 and 3 are optional) or Par(value)\n"];
 (*Karen 08/05/2013 changed the following line in order to make ASperGe compatible with gcc 4.8*)
  Which[Length[#]===5, WriteString[cppstream,"    "<>#[[1]]<>" = new RPar("<>StringReplace[#[[2]],blockrule]];
                       WriteString[cppstream,","<>StringReplace[#[[3]],{"{"->"","}"->""}]];
                       WriteString[cppstream,");\n"],
        Length[#]===4 && #[[-2]]=="Complex", WriteString[cppstream,"    "<>#[[1]]<>" = new CPar("<>#[[2]]<>");\n"],
        Length[#]===4 && #[[-2]]==="Real",  WriteString[cppstream,"    "<>#[[1]]<>" = new RPar("<>#[[2]]<>");\n"]]&/@def;
  WriteString[cppstream,"\n"];
  WriteString[cppstream,"    if(debug) printLog(\"\\n    ... initialization done\\n\");\n\n"];
  (*If these files are created for the mass matrices diagonalization routine, than output a list of the parameters*)
  If[OptionValue[DiagMass], 
    Close[#]&/@stream,
    WriteString[hppstream,"};\n"];WriteString[hppstream,"#endif"];Close[hppstream];
    WriteString[cppstream,"};\n"];Close[cppstream];];
  Print["Parameters.cpp and Parameters.hpp output is finished.\n They have been stored in " <> Directory[]];
];


(* ::Subsection:: *)
(*Define the mass matrices*)


(* ::Text:: *)
(*This function is only used when the mass diagonalisation is needed.*)
(*It appends to the files Parameters.cpp Parameters.hpp and main.cpp all the necessary declarations/initialisations and definitions needed to perform the diagonalisation.*)
(*First step in the calculation consists in storing for every mass matrix that has been calculated the following informations:*)
(*List[ MatrixSymbol, BlockName, Size of the matrix, the analytical expression of the matrix ]*)


CalculatePDGFLR[field_]:=Block[{resu},
  resu=PartPDG[field];
  If[Not[FreeQ[resu,PartPDG]], resu=PartPDG[ClassMemberList[field/.fi_[__]:>fi][[field/.fi_[inds1___,a_?NumericQ,inds2___] :>a]]]];
  If[Not[FreeQ[resu,PartPDG]], Print["Error with the PDG identification ("<>ToString[InputForm[field]]<>"). Please contact the FeynRules authors."]];
  Return[resu];
];


CalculatePDGWeyl[field_]:=Block[{Ferm4,inds},
  Ferm4=To4Components[field/.fi_[__]:>fi];
  inds=List@@field;
  Return[CalculatePDGFLR[Ferm4[Sequence@@inds]]];
];


DefineMassMatrices[]:=Block[{tmpdef,cppstream,hppstream,mainstream,matrixelement,matrixsymbol,blockname,massmatrix,pdgids,massbasis,liste},
 tmpdef=Which[
    (*Dirac Fermions*)
    Type[#]==="FLR",
      matrixsymbol=ToString[MatrixSymbol[#,"L"]];
      blockname=ToString/@List[BlockName[#,"L"],BlockName[#,"R"]];
      massmatrix=DefinitionsToStrings/@((Expand[List[MassMatrix[#].ConjugateTranspose[MassMatrix[#]], ConjugateTranspose[MassMatrix[#]].MassMatrix[#] ]]/.n_?(NumericQ[#]&)*pars__:>N[n,30]*pars)/.Index[typ_,num_]:>num);
      pdgids=ToString/@(Sort/@List[CalculatePDGFLR/@MassBasis[#],CalculatePDGFLR/@MassBasis[#]]); 
      massbasis=ToString/@(MassBasis[#]/.part_[n_Integer,_]:>part[n]);
      liste=List[matrixsymbol,blockname,ToString[Length[massbasis]],massmatrix,pdgids,massbasis],

(*Charged Weyls*)
   Type[#]==="CWeyl",
      matrixsymbol=ToString/@(List[MatrixSymbol[#][[1]],MatrixSymbol[#][[2]]]);
      blockname =ToString/@List[BlockName[#][[1]],BlockName[#][[2]]];
      massmatrix=DefinitionsToStrings/@((Expand[List[MassMatrix[#].ConjugateTranspose[MassMatrix[#]],ConjugateTranspose[MassMatrix[#]].MassMatrix[#]]]/.n_?(NumericQ[#]&)*pars__:>N[n,30]*pars)/.Index[typ_,num_]:>num);
      pdgids=ToString/@(List[Sort[CalculatePDGWeyl/@MassBasis[#][[1]]],-Sort[CalculatePDGWeyl/@MassBasis[#][[2]]]]);
      massbasis=List[ToString/@(MassBasis[#][[1]]/.par_[n_Integer,_]:>par[n]),ToString/@(MassBasis[#][[2]]/.par_[n_Integer,_]:>par[n])];
      liste=List[matrixsymbol,blockname,ToString/@(Length/@massbasis),massmatrix,pdgids,massbasis];
      Sequence@@List[liste[[All,1]],liste[[All,2]]],
    (*Scalars and pseudo scalars*)
    Type[#]==="SPS",
      matrixsymbol=ToString/@(List[MatrixSymbol[#,"S"],MatrixSymbol[#,"PS"]]);
      blockname=ToString/@List[BlockName[#,"S"],BlockName[#,"PS"]];
      massmatrix=DefinitionsToStrings/@((Expand[List[MassMatrix[#,"S"], MassMatrix[#,"PS"]]]/.n_?(NumericQ[#]&)*pars__:>N[n,30]*pars)/.Index[typ_,num_]:>num);
      pdgids=ToString/@(Sort/@List[CalculatePDGFLR/@MassBasis[#,"S"],CalculatePDGFLR/@MassBasis[#,"PS"]]);
      massbasis=List[ToString/@(MassBasis[#,"S"]/.par_[n_Integer,_]:>par[n]),ToString/@(MassBasis[#,"PS"]/.par_[n_Integer,_]:>par[n])];                 
      liste=List[matrixsymbol,blockname,ToString/@(Length/@massbasis),massmatrix,pdgids,massbasis];
      Sequence@@List[liste[[All,1]],liste[[All,2]]],
    (*Others*)
    True,
(*It could be a neutral Weyl*)
      If[WeylFieldQ[First[MassBasis[#]]]===True,
        matrixsymbol=ToString[MatrixSymbol[#]];
        blockname=ToString[BlockName[#]];
        massmatrix=DefinitionsToStrings[(Expand[MassMatrix[#]]/.n_?(NumericQ[#]&)*pars__:>N[n,30]*pars)/.Index[typ_,num_]:>num];
        pdgids=ToString[Sort[CalculatePDGWeyl/@MassBasis[#]]];
        massbasis=ToString/@(MassBasis[#]/.part_[n_Integer,_]:>part[n]);
        liste=List[matrixsymbol,blockname,ToString[Length[massbasis]],massmatrix,pdgids,massbasis,"nsq"],
(*or a vector field*)
        matrixsymbol=ToString[MatrixSymbol[#]];
        blockname=ToString[BlockName[#]];
        massmatrix=DefinitionsToStrings[Expand[MassMatrix[#]]/.Index[typ_,num_]:>num];
        pdgids=ToString[Sort[CalculatePDGFLR/@MassBasis[#]]];
        massbasis=ToString/@(MassBasis[#]/.part_[n_Integer,_]:>part[n]);
        liste=List[matrixsymbol,blockname,ToString[Length[massbasis]],massmatrix,pdgids,massbasis]]]&/@FR$MassMatrices;

  (*just making sure that string characters will have a double quote*)
  tmpdef[[All,6]]=StringReplace[#,"\"]"->"]\""]&/@(StringReplace[#,{WordBoundary~~a__~~WordBoundary:>"\""<>ToString[a]<>"\""}]&/@tmpdef[[All,6]]);
  (*Creating a unique name for mass matrices of Dirac fermions
  tmpdef=If[Length[#[[2]]]==2, Append[#[[2]],Unique[#[[2,1]]]],#[[2]]]&/@tmpdef;*)

  cppstream=OpenAppend["src/Parameters.cpp"];
  hppstream=OpenAppend["inc/Parameters.hpp"];
  mainstream=OpenAppend["main.cpp"] (*too much?*);
  (*First: the Parameters.hpp*)
  WriteString[hppstream,"\n    ///map to store the pointers to the functions initialising the mass matrices \n"];
  WriteString[hppstream,"    map <string,initFP> massInit;\n"];
  WriteString[hppstream,"    map <string,initFP2> massInit2;\n"];

  (WriteString[hppstream,"    ///function to initialise the "<>#[[1]]<>" mass matrix\n"];
    If[Length[#[[2]]]==0,
      WriteString[hppstream,"    void init"<>#[[1]]<>"(gsl_matrix_complex *);\n"],
      WriteString[hppstream,"    void init"<>#[[1]]<>"(gsl_matrix_complex *,gsl_matrix_complex *);\n"]]; )&/@tmpdef;

  WriteString[hppstream," \[NonBreakingSpace]  ///prints the parameters belonging to a block\n"];
 \[NonBreakingSpace]WriteString[hppstream,"   \[NonBreakingSpace]void printParameters(ofstream &); \n"];
  WriteString[hppstream,"};\n#endif"];
  Close[hppstream];
  (*Second: the Parameters.cpp*)
  WriteString[cppstream,"    //map string \"MassSymbol\" to the pointer to the initialising function\n"];
  If[Length[#[[2]]]==0,
    WriteString[cppstream,"    massInit[\""<>#[[1]]<>"\"] = &Parameters::init"<>#[[1]]<>";\n\n"],
    WriteString[cppstream,"    massInit2[\""<>#[[1]]<>"\"] = &Parameters::init"<>#[[1]]<>";\n\n"]]&/@tmpdef;
  WriteString[cppstream,"    if(debug) printLog(\"... parameter construction done\\n\\n\");\n"];
  WriteString[cppstream,"}\n"];

  (
  If[Length[#[[2]]]==0,
  (*Non Dirac matrices*)
    WriteString[cppstream,"//functions to initialise the "<>#[[1]]<>" mass matrix\n"];
    WriteString[cppstream,"void Parameters::init"<>#[[1]]<>"(gsl_matrix_complex *m)\n"];
    WriteString[cppstream,"{\n    gsl_matrix_complex_set_zero(m);\n"];
    WriteString[cppstream,"    gsl_complex comp;\n"];
    Table[
      matrixelement=If[StringMatchQ[#[[4,ii,jj]],"0"],"complex<double>(0.,0.)",#[[4,ii,jj]]];
      WriteString[cppstream,"    GSL_SET_COMPLEX(&comp,\n    realpart("<>matrixelement<>"),\n    imagpart("<>matrixelement<>"));\n"];
      WriteString[cppstream,"    gsl_matrix_complex_set(m,"<>ToString[ii-1]<>","<>ToString[jj-1]<>",comp);\n\n"],{ii,ToExpression[#[[3]]]},{jj,ToExpression[#[[3]]]}];
      WriteString[cppstream,"}\n"],

 (*Dirac matrices*)
    WriteString[cppstream,"//functions to initialise the "<>#[[1]]<>" mass matrix\n"];
  \[NonBreakingSpace] WriteString[cppstream,"void Parameters::init"<>#[[1]]<>"(gsl_matrix_complex *m,gsl_matrix_complex *m2)\n"];
    WriteString[cppstream,"{\n    gsl_matrix_complex_set_zero(m);gsl_matrix_complex_set_zero(m2);\n"];
    WriteString[cppstream,"    gsl_complex comp;\n"];
    WriteString[cppstream,"   //fill m;\n"];

    Table[
      matrixelement=If[StringMatchQ[#[[4,1,ii,jj]],"0"],"complex<double>(0.,0.)",#[[4,1,ii,jj]]];
      WriteString[cppstream,"    GSL_SET_COMPLEX(&comp,\n    realpart("<>matrixelement<>"),\n    imagpart("<>matrixelement<>"));\n"];
      WriteString[cppstream,"    gsl_matrix_complex_set(m,"<>ToString[ii-1]<>","<>ToString[jj-1]<>",comp);\n\n"],{ii,ToExpression[#[[3]]]},{jj,ToExpression[#[[3]]]}];
      WriteString[cppstream,"\n"];
      WriteString[cppstream,"   //fill m2;\n"];

    Table[
      matrixelement=If[StringMatchQ[#[[4,2,ii,jj]],"0"],"complex<double>(0.,0.)",#[[4,2,ii,jj]]];
      WriteString[cppstream,"    GSL_SET_COMPLEX(&comp,\n    realpart("<>matrixelement<>"),\n    imagpart("<>matrixelement<>"));\n"];
      WriteString[cppstream,"    gsl_matrix_complex_set(m2,"<>ToString[ii-1]<>","<>ToString[jj-1]<>",comp);\n\n"],{ii,ToExpression[#[[3]]]},{jj,ToExpression[#[[3]]]}];
      WriteString[cppstream,"}\n"]]; )&/@tmpdef;

  WriteString[cppstream,"\n\nvoid Parameters::printParameters(ofstream& out)\n{"];
  WriteString[cppstream,"    Par::updateOutBlocks();\n"];
  WriteString[cppstream,"    for( int i = 0; i < OutBlocks.size() ; i++)\n"];
  WriteString[cppstream,"    (OutBlocks.at(i))->printSLHABlock(out);\n}\n"];
  Close[cppstream];
  (*Last but not least: the main.cpp*)
  WriteString[mainstream,"#include \"headers.hpp\"\n"];
  WriteString[mainstream,"#include \"Parameters.hpp\"\n"];
  WriteString[mainstream,"#include \"tools.hpp\"\n"];
  WriteString[mainstream,"#include \"MassMatrix.hpp\"\n"];
  WriteString[mainstream,"#include \"MassMatrixTwoMix.hpp\"\n"];
  WriteString[mainstream,"#include \"CPar.hpp\"\n"];
  WriteString[mainstream,"#include \"RPar.hpp\"\n\n"];
  WriteString[mainstream,"#include \"MassMatrixNF.hpp\"\n\n"];
  WriteString[mainstream,"int main(int argc, char* argv[])\n{\n"];
  WriteString[mainstream,"    //remove the log file\n    removeLog();\n\n"];
  WriteString[mainstream,"    //debug prints debugging messages and\n    //prints the mass matrices in the output too\n"];
  WriteString[mainstream,"    MassMatrix::debug = true;\n    Parameters::debug = true;\n"];
  WriteString[mainstream,"    Par::debug = true;\n\n"];
  WriteString[mainstream,"    //check whether the arguments given by the user are correct\n    checkArguments(argc,argv);\n\n"];
  WriteString[mainstream,"    //set the input file\n    SLHABlock::inputfile = *(argv+1);\n\n"];
  WriteString[mainstream,"    //initialise the parameters\n    MassMatrix::par->init();\n\n"];
  WriteString[mainstream,"    //mass matrix constructor arguments:\n"];
  WriteString[mainstream,"    //Symbol, block, IMblock, matrix dimension, Pdg-Ids, particles' names.//\n"];
  (*Karen 08/05/2013 changed the following line in order to make ASperGe compatible with gcc 4.8*)
  ( If[Length[#[[2]]]==0,
      WriteString[mainstream,"    int pdgId"<>#[[1]]<>"[] = "<>#[[5]]<>";\n"];
      WriteString[mainstream,"    string pdgName"<>#[[1]]<>"[] = "<>ToString[#[[6]]]<>";\n"];
      If[Length[#]===6,
        WriteString[mainstream,"    new MassMatrix(\""<>#[[1]]<>"\",\""<>#[[2]]<>"\",\"IM"<>#[[2]]<>"\","<>#[[3]]<>",pdgId"<>#[[1]]<>",pdgName"<>#[[1]]<>");\n"],
        WriteString[mainstream,"    new MassMatrixNF(\""<>#[[1]]<>"\",\""<>#[[2]]<>"\",\"IM"<>#[[2]]<>"\","<>#[[3]]<>",pdgId"<>#[[1]]<>",pdgName"<>#[[1]]<>");\n"]],
      WriteString[mainstream,"    int pdgId"<>#[[1]]<>"[] = "<>#[[5,1]]<>";\n"];
      WriteString[mainstream,"    string pdgName"<>#[[1]]<>"[] = "<>ToString[#[[6]]]<>";\n"];
      WriteString[mainstream,"    new MassMatrixTwoMix(\""<>#[[1]]<>"\",\""<>#[[2,1]]<>"\",\"IM"<>#[[2,1]]<>"\",\""<>#[[2,2]]<>"\",\"IM"<>#[[2,2]]<>"\","<>#[[3]]<>",pdgId"<>#[[1]]<>",pdgName"<>#[[1]]<>");\n"]];
  )&/@tmpdef;
  WriteString[mainstream,"\n    //diagonalizes all the members of the class MassMatrix and prints the results//\n"];
  WriteString[mainstream,"    MassMatrix::generateAll(argc-3,argv+3,*(argv+2));\n"];
  WriteString[mainstream,"    return 0;\n}\n\n"];
  Close[mainstream];

];


(* ::Section:: *)
(*Main*)


(* ::Text:: *)
(*This is the main function from which everything starts.*)


Options[WriteASperGe]={Output->Default,Mix->MR$Null};


WriteASperGe[lag_,OptionsPattern[]]:=Block[{dirname,location,interfacedir,mix},
  mix=OptionValue[Mix];

  (*just to know where we are*)
  location=Directory[];

  (*name of the output dir*)
  dirname=If[OptionValue[Output]===Default,StringReplace[ToString[M$ModelName]<>"_MD"," "->"_"],ToString[OptionValue[Output]]];
  $ASperGeDir = Directory[] <> "/" <>dirname;
  (*Path to the mass diag interface folder*)
  interfacedir=StringReplace[Global`$FeynRulesPath<>"/Interfaces/MassDiag","//"->"/"];
  (*delete (if necessary) and create a new directory*)
  If[DirectoryQ[dirname], DeleteDirectory[dirname,DeleteContents->True]]; CopyDirectory[interfacedir,dirname]; 

  (*go to the output dir and play*)
  SetDirectory[dirname];
  Print[Style["* Computing Mass Matrices",Orange]]; 
  ComputeTreeLevelMassMatrix[lag,ScreenOutput->False,Mix->mix];
  CheckMatrices[];
  Print[Style["* Writing Files",Orange]];
  WriteFiles[];WriteLHAFile[Output->"input/externals.dat"];
  Print[Style["Done",Orange]];
  (*Come back to where we were*)
  SetDirectory[location];]


(* ::Section:: *)
(*Executing ASperGE*)


RunASperGe[] :=Block[{olddir=Directory[]},
  SetDirectory[$ASperGeDir];
  Print[Style[" * Compiling Asperge ...",Orange]];
  Run["make"];
  Print[Style[" * Running Asperge ...",Orange]];
  Print[Run["./ASperGe input/externals.dat output/out.dat"]];
  Print[Style[" * Updating parameters ...",Orange]];
  SetDirectory[$ASperGeDir<>"/output"];
  ReadLHAFile[Input->"out.dat"];
  Print[Style["Done",Orange]];
  SetDirectory[olddir];
  Return[];
];
