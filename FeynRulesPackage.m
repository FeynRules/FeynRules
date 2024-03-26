(* ::Package:: *)

(* ::Title:: *)
(*The FeynRules package*)


(* ::Section:: *)
(*Some printout*)


FR$Loaded = True;

BeginPackage["FeynRules`"];

FR$VersionNumber = "2.3.49";
FR$VersionDate = "29 September 2021";

Print[" - FeynRules - "];
Print["Version: ", FR$VersionNumber, " ("FR$VersionDate, ")."];
Print["Authors: A. Alloul, N. Christensen, C. Degrande, C. Duhr, B. Fuks"];
Print[" "];
Print["Please cite:"];
Print["    - Comput.Phys.Commun.185:2250-2300,2014 (arXiv:1310.1921);"];
Print["    - Comput.Phys.Commun.180:1614-1641,2009 (arXiv:0806.4194)."];
Print[" "];
Print["http://feynrules.phys.ucl.ac.be"];
Print[" "];
Print["The FeynRules palette can be opened using the command FRPalette[]."];


(* ::Section:: *)
(*FeynRules symbols definitions*)


(* ::Subsection:: *)
(*System variables*)


M$ModelName::usage = "Model file variable. Name given to the model. The default is the name of the model file.";

M$ClassesDescription::usage = "The list of all particle classes defined in the model file.";

M$Information::usage = "Model file variable. List containing information about the authors of the model file.";

MR$Authors::usage = "List containing the names of the authors of the model file (if specified in M$Information).";

MR$Date::usage = "List containing the date at which the model file was created (if specified in M$Information).";

MR$Institution::usage = "List containing the names of intstitutes the authors of the model file belong to (if specified in M$Information).";

MR$Email::usage = "List containing the email addresses of the authors of the model file (if specified in M$Information).";

MR$References::usage = "List containing the refenrences used to create the  model file (if specified in M$Information).";

MR$URLs::usage = "List containing the URLs the user wants to refer to (if specified in M$Information).";

MR$ClassesDescription::usage = "A modified version of M$ClassesDescriptions used by FeynRules.";

MR$ClassesList::usage = "A list containing all the names of the particles classes in M$ClassesDescription.";

MR$ClassesRules::usage = "MR$ClassesRules[c] is the list of options associated with the particle class c.";

M$GaugeGroups::usage = "The list of all gauge groups defined in the model file.";

MR$GaugeGroups::usage = "A modified version of M$GaugeGroups used by FeynRules.";

MR$GaugeGroupList::usage = "A list containing all the names of the gauge groups in M$GaugeGroups.";

MR$GaugeGroupRules::usage = "MR$GaugeGroupRules[G] is the list of options associated with the gauge group G.";

M$Parameters::usage = "The list of all parameters defined in the model file.";

MR$Parameters::usage = "A modified version of M$Parameters used by FeynRules.";

MR$ParameterList::usage = "A list containing all the names of the parameters in M$Parameters.";

MR$ParameterRules::usage = "MR$ParameterRules[p] is the list of options associated for the parameter p.";

MR$Definitions::usage = "The list of all definitions encountered in the model-file.";

MR$currentmodel::usage = "The model currently loaded into the kernel."

MR$ModelLoaded::usage = "True, if a model-fiel has been loaded, False otherwise.";

MR$FeynRulesLoaded::usage = "True, if FeynRules package has been loaded."

MR$QuantumNumbers::usage = "A list containing all quantum numbers of the model.";

$ConservedQN::usage = "If True, generates an warning if a vertex is encountered that does not conserve the quantum number 
specified in the model file. The default value is True.";

MR$Output::usage = "The current output mode.";

CH$MaxExpressionLength::usage = "Variable of the CalcHep interface, specifying the maximum number of characteres for each parameter on the function file. 
The default value is 50.";

CH$ModelNumber::usage = "Variable of the CalcHep interface, specifying the number of the output model. 
The default value is 1.";

CH$CompHEP::usage = "Variable of the CalcHep interface, specifying whether to write CalcHEP output (False) or CompHEP output (True). 
The default value is False.";

FR$SpaceTimeDimension::usage = "Variable defining the space-time dimension. The default value is 4.";

MR$Default::usage = "Internal FeynRules symbol to denote an default value.";

MR$Null::usage = "Internal FeynRules symbol to denote a null value of a variable.";

FR$MaxSimplify::usage ="bla";

FR$VertexNumber::usage = "The number used to denote the output of FeynmanRules.";

FR$OptimizeParams::usage = "bla";
GetOrder::usage = "bla";

FR$MakeWeylToDirac::usage = "Internal FR variable";


M$FormFactors::usage = "The list of all form factor classes defined in the model file.";


FR$FExpand::usage = "If true, the full expansion is perform to obtain vertices and so on"

If[Global`FR$FullExpand===False,FR$FExpand=False,FR$FExpand=True];


(* ::Subsection:: *)
(*Particle class properties*)


TeXParticleName::usage = "A string, the TeX code associated with the particle name.";

TeXAntiParticleName::usage = "A string, the TeX code associated with the antiparticle name.";

FullName::usage = "Property of the particle classes. An arbitrary string, indicating the name of the particle in full letters (e.g. \"Electron\").";

ClassName::usage = "Mandatory property of the particle classes, specifying the name of the particle class. Only those elements of 
M$ClassesDescription are read in where ClassName is given a value.";

TeXClassName::usage = "A string, given the TeX name of the class.";

ClassMembers::usage = "Property of the particle classes, listing the names of the different class members. If the class only 
contain a single particle, the default value is ClassName";

SelfConjugate::usage = "Mandatory property of the particle classes, specifying whether a field is selfconjugate (True) or not 
(False).";

FlavorIndex::usage = "Property of the particle classes, specifying the flavor index which should be expanded over if FlavorExpand 
is turned to True.";

QuantumNumbers::usage = "Property of the particle classes, listing the quantum numbers of the class (charge, lepton numbers,...). 
The default value is {}.";

(* MW edit: usage of the U1Charges option *)
U1Charges::usage = "Property of the particle classes, listing the particles charges under different U(1) symmetries. The values must
be give as a list of pairs. The first element of each pair is the symbol representing the U(1) charge, as defined with the property Charge
in the gauge group declaration or as given explicitely in the covariant derivative DC. The second element is the charge of the particle under
that symmetry."

Mass::usage = "Property of the particle classes, listing the masses of the different class members. The value of Mass for each 
class member can be\n
1. a symbol representing the mass in the lagrangian.\n
2. a list {symbol, value} (mandatory in the MG mode).";

MajoranaPhase::usage = "MajoranaPhase[\[Lambda]] represents the Majorana phase of the Majorana field \[Lambda].";

Width::usage = "Property of the particle classes, listing the decay rates of the different class members. The value of Width for 
each class member can be\n
1. a symbol representing the width in the lagrangian.\n
2. a list {symbol, value} (mandatory in the MG mode).";

PropagatorType::usage = "Property of the particle classes, specifying the linetype by which the propagator of the particle should be drawn. The allowed linetypes are\n
S / Straight : solid line.\n
W / Sine : wavy line.\n
C / Curly : curly line.\n
D / ScalarDash : dotted line.";

PropagatorLabel::usage = "Property of the particle classes, listing the labels by which the class members should be represented when 
the Feynman diagrams are drawn.";

PropagatorArrow::usage = "Property of the particle classes. This property belongs to FeynArts, and is ignored by FeynRules.";

MixingPartners::usage = "Property of the particle classes. This property belongs to FeynArts, and is ignored by FeynRules."

MatrixTraceFactor::usage = "Property of the particle classes. This property belongs to FeynArts, and is ignored by FeynRules."

InsertOnly::usage = "Property of the particle classes. This property belongs to FeynArts, and is ignored by FeynRules."

ParticleName::usage = "Property of the particle classes, listing the names given to the different class members for an MC output.";

AntiParticleName::usage = "Property of the particle classes, listing the names given to the antiparticles of the different class 
in an MC output.";

GaugeIndex::usage = "Property of the particle classes, specifying the index which should be used to infer the colour structure.";

S::usage = "1. Particle class representing the scalars.\n
2. Linetype repsresenting solid lines.\n
3. Colour structure (singlet).";

T::usage = "1. Particle class representing the spin2 particles.\n
2. Colour structure (triplet).\n
2. Default name of the fundamental SU(3) matrices. This name is mandatory for MC interfaces, in order to identify correctly the colour structures.";

O::usage = "Colour structure (octet).";

PDG::usage = "Property of the particle classes, listing the PDG codes of the different class members in the MAdGraph mode.";

Indices::usage = "Property of the particle and parameter classes, listing the indices carried by a particle or tensor parameter.";

WeylComponents::usage = "A two-component list, giving the Weyl components of a Dirac fermion.";

Unphysical::usage = "Property of the particle classes, specifying whether a field is unphysical. Unphysical fields are not used in the 
output. The relations of the unphysical fields to the physical ones can be obtained via the Definitions property.";

Definitions::usage = "Property commoun to all classes (particles, gauge groups, parameters), listing a set of replacement rules 
that should be applied by FeynRules before performing any calculation.";

SymmetricIndices::usage = "Property of the particle classes, specifying the indices that should be symmetrized.";

AntiSymmetricIndices::usage = "Property of the particle classes, specifying the indices that should be antisymmetrized.";

Symmetric::usage = "Property of the T particle class, specifying whether it is a symmetric spin 2 field (True) or not (False). 
The default value is False.";

AntiSymmetric::usage = "Property of the T particle class, specifying whether it is a antisymmetric spin 2 field (True) or not (False). 
The default value is False.";

C::usage = "Linetype repsresenting curly lines.";

W::usage = "Linetype repsresenting wavy lines.";

D::usage = "Linetype repsresenting dotted lines.";

Straight::usage = "Linetype repsresenting solid lines.";

ScalarDash::usage = "Linetype representing dashed lines.";

GhostDash::usage = "LineType representing dotted lines.";

Sine::usage = "Linetype repsresenting wavy lines.";

Cycles::usage = "Linetype repsresenting curly lines.";

F::usage = "Particle class representing the spin 1/2 fermions.";

W::usage = "Particle class representing Weyl fermions in the (1/2,0) representation."

V::usage = "Particle class representing the vectors.";

R::usage = "Particle class representing the spin 3/2 fermions.";

RW::usage = "Particle class representing the spin 3/2 fermions in 2-component notation.";

U::usage = "Particle class representing the ghost particles.";

Goldstone::usage = "Property for the scalar particle classes, specifying whether a particle is a Goldstone boson associated with a vector boson and a Higgs.";

Ghost::usage = "Property of the ghost particle class (U), specifying the gauge boson connected with this ghost field.";

NoGS::usage = "Default value of the Goldstone property.";

PartName::usage = "PartName[\[Psi]] returns the value of ParticleName / AntiParticleName of \[Psi].";

PartSymbol::usage = "PartSymbol is the inverse of PartName.";

Chirality::usage = "Property of the Weyl fermion classes. Left for (1/2, 0) and Right for (0,1/2) fermions. The default is Left.";


(* ::Subsection::Closed:: *)
(*Gauge group class properties*)


Abelian::usage = "Mandtory property of the gauge group classes, specifying whether a gauge group is abelian (True) or not (False).";

GaugeBoson::usage = "Property of the gauge group classes, specifying the name of the gauge boson connected to this gauge group. 
This gauge boson must be declared in M$ClassesDescription.";

CouplingConstant::usage = "Property of the gauge group classes, specifying the name of the coupling constant connected to this 
gauge group. This coupling constant must be declared in M$Parameters.";

Charge::usage = "Property of the gauge group classes, specifying the name of the charge connected with an abelian gauge group.";

Representations::usage = "Property of the gauge group classes, listing the representations of the gauge group that are used 
inside the lagrangian. E.g. if the gauge index of the quarks is Colour, and the fundamental representation matrices are denoted by 
T, then the representation is {T, Colour}.";

StructureConstant::usage = "Property of the gauge group classes, specifying the name of the structure constant connected to this 
gauge group.";

SymmetricTensor::usage = "Property of the gauge group classes, specifying the name of the completely symmetric tensor connected to this 
gauge group.";

Dynkin::usage="Dynkin index for representations non-abelian gauge groups.";

GUTNormalization::usage="Normalization factor at the GUT scale for abelian gauge groups.";

Casimir::usage="Quadratic Casimir group invariants";

f::usage = "Default name of the SU(3) structure constant. This name is mandatory for MC interfaces, in order to identify correctly the colour structures.";

SUNT::usage = "FormCalc name for the SU(3) color matrices.";

SUNF::usage = "FormCalc name for the SU(3) structure constants."

dSUN::usage = "Default name of the SU(3) d-term. This name is mandatory for MC interfaces, in order to identify correctly the colour structures."

AdjointIndex::usage = "Property of the gauge group classes, specifying the name of the name of the index of the adjoint 
representation of this gauge group.";

VeVs::usage = "1. Property of the scalar particles. The name of the parameter giving the vev associated to a scalar. Zero by default.\n
2. Property of the gauge group classes. A list of all scalars that acquire a vev breaking the gauge symmetry."




AddGaugeRepresentation::usage = "If added to a model file, AddGaugeRepresentation[ list ] merges the gauge representations in list with those in M$GaugeGroups. list is a list of the form, e.g. for QCd sextets,\n
{SU3C  ->  {T6, Sextet}}.";


FR$ReprMap::usage = "Mapping representations and antirepresentations (necessary for susy models.)";



(* ::Subsection::Closed:: *)
(*Parameter class properties*)


(* ::Subsubsection:: *)
(*Properties valid for both scalar and tensor parameters*)


Description::usage = "Property of the parameter classes. An arbitrary string, giving a description of the parameter (e.g. \"Electric charge\").";

ParameterType::usage = "Property of the parameter classes, specifying whether a parameter is Internal or External. 
The default value is External for scalar parameters, and Internal for tensor parameters.";

BlockName::usage = "Property of the parameter classes, specifying the name of the PDG block the parameter belongs to. 
If no BlockName is specified, the parameter gets assigned the default block FRBlock .";

ParameterName::usage = "Property of the parameter classes, specifying the name of the parameter used for the interface to an MC.";

InteractionOrder::usage = "Property of the parameter classes, specifying the interaction order of the parameter. 
E.g. the interaction order of \[Alpha]S is {QCD, 2}.";

Value::usage = "Property of the parameter classes.\n
1. For external parameters, the numerical value.\n
2. For internal parameters, the definition in terms of more fundamental parameters (external and/or internal).";

OrderBlock::usage = "Property of the parameter classes, specifying the PDG code of the parameter.
If no PDG code is assigned, the particle gets an automatically assigned PDG code (starting from 6000000.";

ComplexParameter::usage = "Property of the parameter classes, specifying whether a parameter is complex (True) or not (False).\n
1. For scalar parameters the default value is False.\n
2. For tensor parameters, the default value is True.";

TeX::usage = "Property of the parameter classes, specifying how the parameter should be printed in the TeX output.";

External::usage = "Value of the parameter class property ParameterType.";

Internal::usage = "Value of the parameter class property ParameterType.";


(* ::Subsubsection::Closed:: *)
(*Tensor parameter properties*)


TensorClass::usage = "Property of the parameter classes, specifying the class of tensors to whcih this tensor parameter belongs 
(e.g. Dirac matrices).";

AllowSummation::usage = "Property of the parameter classes.";

Unitary::usage = "Property of the parameter classes, specifying whether a tensor parameter is unitary (True) or not (False). Unitary tensor must 
be complex. the default value is False.";

Orthogonal::usage = "Property of the parameter classes, specifying whether a tensor parameter is orthogonal (True) or not (False). Unitary tensor must 
be real. the default value is False.";

Hermitian::usage = "Property of the parameter classes, specifying whether a tensor parameter is hermitian (True) or not (False). Unitary tensor must 
be complex. the default value is False.";


(* ::Subsection:: *)
(*Basic functions*)


FRPalette::usage = "Loads the FeynRules palette.";

LoadModel::usage = "LoadModel[file.fr] loads the model-file file.mod into to the kernel and initializes FeynRules 
to this specific model.";

Report::usage = "Option of LoadModel";

Restriction::usage = "Option of LoadModel. Restriction -> filename.rest loads the restriction file gievn as the argument of the option.";

AddDefinition::usage = "Adds a definition for a parameter to the kernel.";

LoadRestriction::usage = "Loads the restriction file given as the argument of the function.";

ModelInformation::usage = "ModelInformation[] reads out the information about the model authors given in the model file.";

Authors::usage = "Element of M$Information.";

Date::usage = "Element of M$Information.";

Institutions::usage = "Element of M$Information.";

Emails::usage = "Element of M$Information.";

References::usage = "Element of M$Information.";

URLs::usage = "Element of M$Information.";

IndexRange::usage = "IndexRange[Index[name]] is the routine used in the model-file to declare the index name, 
e.g. IndexRange[Index[name]] = Range[k] declares an index of type name, taking its values in the {1,...,k}.";

IndexStyle::usage = "IndexStyle[name, k] fixes the index name to print as k.";

Index::usage = "Index[name, i] represents an index of type name and value i.";

FeynmanRules::usage = "FeynmanRules[L] calculates the vertices associated with the lagrangian L.\n 
FeynmanRules returns an internal tag by which the vertices can be called using the Vertices function.";

(* MW edit *)
VertexHook::usage = "VertexHook is a user definable function which is applied to each vertex generated by FeynmanRules."

Vertices::usage = "Vertices[\"L\"] contains the vertices from the lagrangian named L.";

VerticesMG::usage = "VerticesMG[\"L\"] contains the MadGraph vertices from the lagrangian named L.";

VerticesSH::usage = "VerticesSH[\"L\"] contains the Sherpa vertices from the lagrangian named L.";

VerticesFA::usage = "VerticesFA[\"L\"] contains the FeynArts vertices from the lagrangian named L.";

FlavorExpand::usage = "Option of the function FeynmanRules, specifying the flavor indices FeynRules should expand over.\n
1. The indices that should be expanded can be given as a list.\n
2. FlavorExpand -> True expands over all flavor indices. The default value is False.";

IndexExpand::usage = "Option of the function FeynmanRules, IndexExpand -> {...} specifies the indices the lagrangian should be expanded over.";

ScreenOutput::usage = "Option for FeynmanRules. If True, the vertices are written on the screen. The default value is False.";

TeXOutput::usage = "Option for FeynmanRules. TexOutput -> \"file.tex\" writes the output in TeXForm in file.tex.  ";

Name::usage = "A String chosen by the User to name the vertices obtained.";

ConservedQuantumNumbers::usage = "Option of FeynRules, specifying whether a warning should be generated if a vertex is encountered 
that does not conserve the quantum number specified in the model file. The default value is $ConservedQN.";

MaxParticles::usage = "Option of the FeynmanRules function, specifying the maximum number of particles that should appear in the vertices."; 

MinParticles::usage = "Option of the FeynmanRules function, specifying the minimum number of particles that should appear in the vertices."; 

MaxCanonicalDimension::usage = "Option of the FeynmanRules function, specifying the maximal canonical dimension for which vertices should be calculated."; 

MinCanonicalDimension::usage = "Option of the FeynmanRules function, specifying the maximal canonical dimension for which vertices should be calculated."; 

SelectParticles::usage = "Option of FeynmanRules. SelectParticles -> {{\[Phi]1, \[Phi]2,...}, {...}}, only calculates the vertices with the specified field content 
{\[Phi]1, \[Phi]2,...}, etc.";

Exclude4Scalars::usage = "Options of FeynmanRules. If True, then no quartic scalar interactions are computed.";

ApplyMomCons::usage = "Options of FeynmanRules. If True, momentum conservation is used to simplify the vertices.";

PrintLagrangian::usage = "Option of WriteTeXOutput. If set to false, the lagrangian will not be exported into the TeX file. The default value is True.";

WriteMGOutput::usage = "Writes the MadGraph output files for the model.";

WriteMGParamCard::usage = "Writes the external variables to the MadGraph param_card.dat."

MGParticleName::usage = "Takes the particle/antiparticle name and gives the appropriate MG name.";

MGName::usage = "Internal MG interface function";

PartNameMG::usage= "Internal MG interface function";

DecomposeGluonVertex::usage = "Option of the MadGraph interface. When True, then the 4 gluon vertex is decomposed into three-point vertices connected by a tensor.
True by default.";

DialogBox::usage = "Option of the MC interfaces. If On, dialog boxes with warning messges can be produced. The default value is Off.";

MaxVertices::usage = "Option of WriteMGOutput. Fixes the maximal number of vertices to be written into one file. The default value is 50.";

WriteCHOutput::usage = "Writes the CalcHep/CompHEP output files for the model.";

CHName::usage = "Returns the CH name for the particle.";

CHParticleNameLength::usage = "Option for WriteCHOutput.  The default particle name length is 4 characters (plus an optional ~ at the beginning).  If you are using an older version or are using CompHEP, you will want to set this to 2.";

CHSimplify::usage = "Options of the CalcHep interface. If True, the vertices are simplified. The default is true.";

LHASupport::usage = "Option of the CalHEP interface.  If True, LHA parameter support is output.";

PrintWarnings::usage = "Option of the CalcHEP interface.  If True, warnings are printed.";

CHAutoWidths::usage = "Option of the CalcHEP interface.  If True, the widths are set to be calculated by CalcHEP on the fly.  Default is true.";

CHVertices::usage = "Option of the CalcHEP interface.  If set to a vertex list, this will be implemented in place of the Lagrangian.";

WriteCHExtVars::usage = "Writes the external variables to CH format.";

ReadCHExtVars::usage = "Reads external variables from CH format.";

WriteLaTeXOutput::usage = "Writes LaTeX output.";

Overwrite::usage = "Option for WriteLaTeXOutput.  Determines whether the LaTeX files should be overwritten.  Automatic by default.";

Paper::usage = "Option for WriteLaTeXOutput.  Determines the paper type that should be used.  Choices are letter, legal and a4.  Default is letter.";

WriteSHOutput::usage = "Writes the Sherpa output files for the model.";

WriteFeynArtsOutput::usage = "Writes the FeynArts output files for the model.";

CouplingRename::usage = "Determines whether to rename the couplings in FA.";

GenericFile::usage = "Option of WriteFeynArtsOutput, generate a generic file with the same name as the model file if True. 
If False the generated FA model file should be used with lorentz.gen. Default is True";

DiracIndices::usage = "Option of WriteFeynArtsOutput, determine if the generic file should contrain dirac indices. 
If True dirac indices are added even if the maximal number of fermions per the vertices is at most two.
If Automatic, put dirac indices only if the maximal number of fermions per the vertices is more than 2.
Default is Automatic.";

WriteTeXOutput::usage = "WriteTeXOutput[file.tex, list] writes the TeX output for the vertices specified in list into file.tex.";

MergeVertices::usage = "MergeVertexLists[list1, list2,...] combines the vertex lists list1, list2,... into a single list.";

RemoveZeroVertices::usage = "RemoveVertices[vertex list] returns a new vertex list with the numerically zero vertices removed.";

MaxExpressionLength::usage = "Option of WriteCHOutput, specifying the maximum number of characteres for each parameter on the function file. 
The default value is CH$MaxExpresionLength.";

ModelNumber::usage = "Option of the CalcHep interface, specifying the number of the output model. 
The default value is CH$ModelNumber.";

CompHEP::usage = "Option of the CalcHep interface, specifying whether to write CalcHEP output (False) or CompHEP output (True). 
The default value is CH$CompHEP.";

NewFeynArtsClasses::usage = "NewFeynArtsClasses[] returns a list with the particle classes used in the FeynArts interfaces, if the particle classes have been redefined.";

UpdateParameters::usage = "UpdateParameters[param1->value,param2->value,...] updates the numerical values of external parameters.";

WriteParameters::usage = "WriteParameters[] writes a file with the parameters that can be shared.";

ReadParameters::usage = "ReadParameters[] reads a file with the parameters.";

WriteRestrictionFile::usage = "WriteRestrictionFile[] writes out a restriction file which puts to zero all external parameters having zero numerical value.";

GaugeXi::usage = "1. GaugeXi[s] is a gauge parameter with index s.\n
Property of the gauge group classes, giving the value of the \[Xi] parameter.";

WriteLHAFile::usage = "Write a LHA parameter file.";

ConvertLHAFile::usage = "Convert a LHA parameter file into a FeynRules parameter file.";

ReadLHAFile::usage = "Reads a LHA parameter file, and updates the parameters correspondingly.\n
Optionnally, a FeynRules parameter file may be created.";

WeylToDirac::usage = "Converts a Lagrangian written in terms of Weyl fermions to Dirac fermions.";

SymmetryFactor::usage = "If True, a symmetry factor is included in the fermion flow algorithm. The default is False.";



WriteUFO::usage = "Writes the Python output files for the model.";

NegativeInteractionOrder::usage = "Option of WriteUFO. Specifies whether negative interaction orders are allowed for couplings.\n
* Automatic: Allowed, but warning printed if non positve interaction order is encountered.\n
* True: Allowed, no warning.\n
* False: Not allowed, they are put to zero by the UFO.\n
The default is Automatic.";

Optimization::usage = "Option of WriteUFO. If True, then all numerically zero couplings are removed from the output.\n
The default is False.";

RemoveGhosts::usage = "Option of WriteUFO. Remove all ghosts and Goldstoen bosons from the output.";

Restrictions::usage = "Option of WriteUFO. A list of strings, which are names of restriction files (.rst), whose contents will be passed to the UFO output.";


NumericalOnly::usage = "Option of WriteParameters. If True, NoValue[1] and Internal are ignored when writing out the parameters. The default is False.";


DeclareNewDefinition::usage = "DeclareNewDefinition[rule, list] will add rule to list and return the ne list."


FRPi::usage = "Internal representation of Pi in some of the interfaces.";


\!\(TraditionalForm\`PYReorderParticles\)::usage = "Internal FR function.";
\!\(TraditionalForm\`PYOrderFermions\)::usage = "Internal FR function.";


(* ::Subsection::Closed:: *)
(*Usefull symbol to write down the lagrangian*)


gs::usage = "Mandatory name for the strong coupling for interfaces.";

ee::usage = "Mandatory name for the electromagnetic coupling for interfaces.";

\[Alpha]S::usage = "Mandatory name for the strong coupling for interfaces.";

aS::usage = "Mandatory ParameterName for the strong coupling for interfaces.";

\[Alpha]EW::usage = "Mandatory name for the electromagnetic coupling for interfaces.";

aEW::usage = "Mandatory ParameterName for the electromagnetic coupling for interfaces.";

\[Alpha]EWM1::usage = "Mandatory name for the inverse electromagnetic coupling for interfaces.";

aEWM1::usage = "Mandatory ParameterName for the inverse electromagnetic coupling for interfaces.";

Q::usage = "Q[\[Psi]] is the electric charge of the field \[Psi].";

Y::usage = "Y[\[Psi]] is the U(1)Y hypercharge of the field \[Psi].";

FS::usage = "1. FS[A, \[Mu], \[Nu]] represent the field strength tensor of the abelian gauge group the gauge boson A is connected to.\n
2. FS[A, \[Mu], \[Nu], a] represent the field strength tensor of the non abelian gauge group the gauge boson A is connected to.\n
3. FS[A, \[Mu], \[Nu], a, f, g] represent the field strength tensor of a generic non abelian gauge group the gauge boson A is 
connected, with structure constant f and coupling constant g.";

AdjointRep::usage = "AdjointRep[name][a] returns represents the generator of the adjoint representation of the gauge group name."

Dual::usage = "1. Dual[FS][A, \[Mu], \[Nu]] is the dual field strength tensor of FS[A, \[Mu], \[Nu]].\n
2. Dual[FS][A, \[Mu], \[Nu], a] is the dual field strength tensor of FS[A, \[Mu], \[Nu], a].\n
Dual[FS][A, \[Mu], \[Nu], f, g] is the dual field strength tensor of FS[A, \[Mu], \[Nu], f, g].";

CC::usage = "CC[\[Psi]] represent the charge conjugated field \[Psi].";

HC::usage = "HC[V] represents the hermitian conjugate of the tensor parameter V.";

Eps::usage = "The Levi-Civita tensor.";

LeviCivita::usage = "The Levi-Civita tensor in FormCalc for Lorentz indices.";

IndexEps::usage = "The Levi-Civita tensor in FormCalc (except Lorentz indices)."


ME::usage = "The metric tensor (Minkowski-space).";

FV::usage = "FV[p, \[Mu]] is the four-vector p[\[Mu]].";

del::usage = "The derivative with respect to the space-time coordinate x[\[Mu]].";

anti::usage = "anti[\[Psi]] is the antifield associated with the field \[Psi].";

SP::usage = "SP[p1, p2] is the scalar produc of the two four-vectors p1 and p2.";

Ga::usage = "1. Ga[\[Mu]] is the Dirac matrix with Lorentz index \[Mu].\n
2. Ga[5] is the Dirac matrix \[Gamma][5].\n
3. Ga[\[Mu], r, s] is the element {r,s} Dirac matrix with Lorentz index \[Mu]].\n
4. Ga[5, r, s] is the element {r,s} Dirac matrix \[Gamma][5].";

SlashedP::usage = "1. SlashedP[k] is a shorthand for \!\(\*SubscriptBox[\"\[Gamma]\", \"\[Mu]\"]\) \!\(\*SuperscriptBox[SubscriptBox[\"p\", \"k\"], \"\[Mu]\"]\) .\n
2. SlashedP[k, r, s] denotes the element {r, s} of SlashedP[k].";

ProjP::usage = "1. ProjP is the projector on the right-handed fermions, i.e. ProjP = (1+Ga[5])/2.\n
2. ProjP[\[Mu]] is a shorthand for Ga[\[Mu]].ProjP.";

ProjM::usage = "1. ProjM is the projector on the right-handed fermions, i.e. ProjM = (1-Ga[5])/2.\n
2. ProjM[\[Mu]] is a shorthand for Ga[\[Mu]].ProjM";

Sig::usage = "Sig[\[Mu], \[Nu]] is the commutator of Dirac matrices.";

si::usage = "1. Si[\[Mu]] denotes the Pauli matrix \[Sigma][\[Mu]].\n
2. Si[\[Mu], r, s] denotes the element {r, s} ofthe Pauli matrix \[Sigma][\[Mu]].";

sibar::usage = "1. Sibar[\[Mu]] denotes the Pauli matrix \bar{\[Sigma]}[\[Mu]].\n
2. Sibar[\[Mu], r, s] denotes the element {r, s} ofthe Pauli matrix \bar{\[Sigma]}[\[Mu]].";

PauliSigma::usage = "PauliSigma[i] represents the Pauli matrix \[Sigma][i].";

right::usage = "right[\[Psi]] is equivalent to ProjP.\[Psi].";

left::usage = "left[\[Psi]] is equivalent to ProjM.\[Psi].";

DCint::usage = "Interaction part of the covariant derivative.";

ContstructNonAbelianNonFieldComponentDC::usage = "Internal FeynRules function.";

ContstructNonAbelianFieldComponentDC::usage = "Internal FeynRules function.";

(* MW edit: usage of covariant derivative DC *)
DC::usage = "1. DC[g][\[Psi],\[Mu]] denotes the covariant derivative of the field \[Psi].
g denotes the related gauge group. The representation is determined from the
indices declared for \[Psi]. The indices of \[Psi] must either be all suppressed
or all written out.\n
2. DC[g,Y][\[Psi],\[Mu]]. For Abelian groups, the charge can be given explicitely after the
group name. If absent, the symbol from the Charge option of the group declaration is used."

(* MW edit: usage of DiagProd *)
DiagProd::usage = "DiagProd[M,f] denotes the product of a field f with a matrix M
that is diagonal in all the indices carried by the field f."

PerformPauliAlgebra::usage = "PerformPauliAlgebra[expr] performs some basic algebra on the Pauli matrices.";

TP::usage = "TP[T][a] denotes the transpose of a gauge matrix.";


(* ::Subsection::Closed:: *)
(*ToolBox functions*)


numQ::usage = "Boolean function, returning True for all parameters in the model, as well as for all numerical values.\n
Note that the components of a tensor a considered as parameters, e.g. numQ[T[a, i, j] = True, and numQ[Ga[mu, r, s]] = True.";

CnumQ::usage = "Boolean function, returning True if a parameter is a complex parameter.";

TensQ::usage = "Boolean function, returning True for a tensor.";

CompTensQ::usage = "Boolean function, returning True for a complex parameter.";

FieldQ::usage = "Boolean function, FieldQ[\[Psi]] is True, is \[Psi] is a field.";

FermionQ::usage = "Boolean function, returning True for fermions.";

WeylFieldQ::usage = "Boolean function, returning True for Weyl fermions.";

BosonQ::usage = "Boolean function, returning True for bosons.";

DiracFieldQ::usage = "Boolean function, returning True for Dirac fermions.";

MajoranaFieldQ::usage = "Boolean function, returning True for Majorana fermions.";

ScalarFieldQ::usage = "Boolean function, returning True for scalar fields.";

VectorFieldQ::usage = "Boolean function, returning True for vector fields.";

Spin32FieldQ::usage = "Boolean function, returning True for spin 3/2 fields.";

RSpin32FieldQ::usage = "Boolean function, returning True for real spin 3/2 fields.";

CSpin32FieldQ::usage = "Boolean function, returning True for complex spin 3/2 fields.";

Spin32WeylFieldQ::usage="Boolean function, returning True for Weyl spin 3/2 fields.";

RFermionFieldQ::usage = "Boolean function, returning True for real fermion fields.";

CFermionFieldQ::usage = "Boolean function, returning True for complex fermion fields.";

Spin2FieldQ::usage = "Boolean function, returning True for spin2 fields.";

GhostFieldQ::usage = "Boolean function, returning True for ghost fields.";

GoldstoneQ::usage = "Boolean function, returning True for Goldstone bosons.";

UnitaryQ::usage = "Boolean function, returning True for unitary tensors.";

HermitianQ::usage = "Boolean function, returning True for hermitian tensors.";

OrthogonalQ::usage = "Boolean function, returning True for orthogonal tensors.";

QuantumNumberQ::usage = "Boolean function, returning True for quantum numbers defined in the model file.";

SelfConjugateQ::usage = "Boolean function, returning True for selfconjugate fields.";

UnphysicalQ::usage = "Boolean function, returning True for unphysical fields.";

GammaMatrixQ::usage = "Boolean function, returning True for Dirac matrices.";

AntiFieldQ::usage = "Boolean function, returning True anti particles."

GetFieldContent::usage = "Internal FeynRules function.";

PutIndices::usage = "Internal FeynRules function.";

PrePutIndices::usage = "Internal FeynRules function.";

NTIndex::usage = "Head of non tensor indices.";

ExpandIndices::usage = "ExpandIndices[L] makes the index structure of the lagrangian L explicit.";

FR$Dot::usage = "Internal version of Dot, which automatically expands over the fields.";

GetInteractionTerms::usage = "Filters out the interaction terms from a lagrangian.";

GetQuadraticTerms::usage = "Filters out the quadratic terms from a lagrangian.";

GetMassTerms::usage = "Filters out the mass terms from a lagrangian.";

(* BF: ciao 
(* MW edit: GetMassMatrix usage *)
GetMassMatrix::usage = "1. GetMassMatrix[L, {f1, f2, ...}] extracts the quadratic terms from the lagrangian L 
and returns the mass matrix of the fields f1, f2, etc. The fields f1, f2, etc should be class names.\n
2. GetMassMatrix[L, {f1bar, f2bar, ...}, {f1, f2, ...}] returns the matrix of mixing terms between the fields
f1bar, f2bar, ... and the fields f1, f2, ...";
*)
$IndList::usage = "$IndList[\[Psi]] a list containing all the indices declared for the filed or tensor \[Psi].";

NumericalValue::usage = "NumericalValue[param] returns the numerical value of a parameter specified in the model file. Returns Novalue if no numerical value has been specified.";

MR$FlavorList::usage = "List containing all flavor indices deined in the model file.";


(* ::Subsection:: *)
(*Other usefull symbols and functions*)


H::usage = "Symbol used in the MG interface to denote HEFT couplings.";

QCD::usage = "Symbol used in the MG interface to denote QCD couplings.";

QED::usage = "Symbol used in the MG interface to denote EW couplings.";

Lorentz::usage = "Name of the index type representing four-vectors indices.";

Colour::usage = "Default name for the quark colour indices. Used in the MC interfaces.";

Gluon::usage = "Default name for the gluon colour indices. Used in the MC interfaces.";

ReadAll::usage = "If turned to false, only the information needed to write Tex and FeynArts output is read from the model-file. The default value is True.";

EParamList::usage = "Output produced by LoadModel, if ReadAll is True. EParamList is the list of all external parameters in the model file (except masses ans decay width).";

IParamList::usage = "Output produced by LoadModel, if ReadAll is True. IParamList is the list of all internal parameters in the model file (except masses ans decay width).";

ParamList::usage = "Output produced by LoadModel, if ReadAll is True. ParamList is the list of all parameters in the model file (except masses ans decay width).";

ParamRules::usage = "Replacement list, linking the Mathematica symbols for parameters to the ParameterName for this symbol.";

PartList::usage = "Output produced by LoadModel, if ReadAll is True. PartList is the list of all particles in the model file.";

MassList::usage = "Output produced by LoadModel, if ReadAll is True. MassList is the list of all mass parameters in the model file.";

WidthList::usage = "Output produced by LoadModel, if ReadAll is True. Width is the list of all decay width parameters in the model file.";

MRIndexRange::usage = "An internal version of IndexRange.";

Output::usage = "Option of LoadModel and FeynmanRules. Defines the output mode of these functions.";

NoPDG::usage = "Default value of the particle class property PDG ";

NoBlockName::usgae = "NoBlockName[FRBlock] is the default value of the parameter class property BlockName.";

NoValue::usgae = "NoValue[1] is the default value of the parameter class property Value.";

FRBlock::usage = "Default BlockName assigned to a parameter.";

FRMGDefault::usage = "Default BlockName assigned to a dumb parameters in the MG interface.";

OrderBlock::usage = "OrderBlock[name] gives a matrix containing all parameter in the LH block. ";

DECAY::usage = "LH block of the decay rates.";
MASS::usage = "LH block of the masses.";
ZERO::usage = "Symbol prepresenting the number 0.";

Spin::usage = "Name of the index type representing Dirac indices.";

Spin1::usage = "Name of the index type representing (1/2, 0) Weyl indices.";
Spin2::usage = "Name of the index type representing (0, 1/2) Weyl indices.";

Ext::usage = "Index[name, Ext[i]] represents the index name of the external particle number i.";

Int::usage = "Int[i] renames the internal index i.";

IntLor::usage = "Int[\[Mu]] renames the internal index \[Mu].";

NoUnfold::usage = "FeynArts command ignored by FeynRules.";

Unfold::usage = "The command \n IndexRange[Index[name]] = Unfold[Range[   ]]\n in a model file instructs FeynRules to always expand over this index.";

G::usage = "MadGraph name for the strong coupling";

StrongCoupling::usage = "Generic name for the strong coupling.";

TeXFormat::usage = "TeXFormat[x, y] print x in the TeX output as y.";

sqrt::usage = "Fortran symbol for Sqrt.";

conjg::usage = "Fortran symbol for complex conjugation.";

MR$IntCount::usage = "Counter for internal indices."

MR$IntLorCount::usage = "Counter for internal Lorentz indices."

FermionFlow::usage = "Option of the FeynmanRules function, specifying whether vertices containing Majorana fields should be decomposed using the fermion flow rules.
The default value is False.";

PDGToMass::usage = "Relates the pdg code of a particle to its mass symbol.";

PDGToWidth::usage = "Relates the pdg code of a particle to its width symbol.";

FeynArts::usage = "Tag used in the FeynArts interface";

Done::usage = "Internal FeynRules symbol.";

CanonicalDimension::usage = "CanonicalDimension[term] gives the canonical dimension of term (term must be a product).";

M$ExtParams::usage = "Variable to store external variables in parameter file.";

M$Masses::usage = "Variable to store masses in parameter file.";

M$Widths::usage = "Variable to store widths in parameter file.";

M$IntParamsNumericalValues::usage = "Variable to store internal variable numerical values in parameter file.";

M$IntParams::usage = "Variable to store internal variable definitions in parameter file.";

M$Restrictions::usage = "Variable to be used in restriction (.rst) files.";

HCanti::usage = "Internal variable";

GS::usage = "FeynArts / FormCalc name for the string coupling.";

EL::usage = "FeynArts / FormCalc name for the electromagnetic couplings.";

Alfas::usage = "FeynArts / FormCalc name for the string coupling.";

Alfa::usage = "FeynArts / FormCalc name for the electromagnetic couplings.";

IndexSum::usage = "FeynArts function denoting a sum over an index.";

StoreInt::usage ="";

TreatMajoranasAndCC::usage = "Internal FeynRules function. Determines the fermion flow.";

ApplyDefinitions::usage = "Interanl FeynRules function.";

DTermFormat::usage = "Internal function";

AuxiliaryGluonMass::usage = "";


(* ::Subsection:: *)
(*Superfields*)


M$Superfields::usage = "List of the superfield included in the model file";

M$SuperfieldClasses::usage = "List of the superfield symbols defined in the model file";

M$SuperfieldRules::usage = "Rules attached to a superfield";

M$SuperfieldNames::usage = "List of the superfield names defined in the model file";

M$ChiralSuperfieldClasses::usage = "List of the chiral superfield symbols defined in the model file";

M$ChiralSuperfieldNames::usage = "List of the chiral superfield names defined in the model file";

M$VectorSuperfieldClasses::usage = "List of the vector superfield symbols defined in the model file";

M$VectorSuperfieldNames::usage = "List of the vector superfield names defined in the model file";

SuperfieldDeclaration::usage = "Declare the superfield present in the models.";

SuperfieldQ::usage = "Boolean function. Return true for a superfield.";

ChiralSuperfieldQ::usage = "Boolean function. Return true for a chiral superfield.";

SF2ClassName::usage="Transform a classname into the associated superfield label.";

SF2Boson::usage="From a vector superfield to its vector component.";

$TR::usage = "T_R of a Lie group invariant.";

$OptIndex::usage="Starting point for the index renaming function.";

Gaugino::usage="Gaugino component of a superfield.";

Scalar::usage="Scalar component of a superfield.";

Auxiliary::usage="Auxiliary component of a superfield.";

CSFKineticTerms::usage="Kinetic term for a given superfield";

SF2Components::usage="Main routine to expand a superfield in terms of the Grassmann variables.";

ScalarComponent::usage="Scalar coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

ThetaComponent::usage="Theta coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

ThetabarComponent::usage="Thetabar coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

ThetaThetabarComponent::usage="ThetaSigmaThetabar coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

Theta2Component::usage="Theta^2 coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

Thetabar2Component::usage="Thetbar^2 coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

Theta2ThetabarComponent::usage="Theta^2 Thetabar coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

Thetabar2ThetaComponent::usage="Thetabar^2 Theta coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

Theta2Thetabar2Component::usage="Theta^2 Thetabar^2 coefficient of the serieus expansion of a superfield in terms of the Grassmann variables.";

GrassmannExpand::usage="Expand a superfield in terms of the Grassmann variables.";

ToGrassmannBasis::usage="Simplifcation of the Grassmann spinors.";

ComponentList::usage="Get the 9 coefficients of a polynomial in Grassmann variables.";

alpha::usage="";

alphadot::usage="";

VSFKineticTerms::usage="Contract the superfield strength tensors associated to a given vector superfield and get the corresponding Lagrangian.";

Superfield::usage="Gauge group option";

OptimizeIndex::usage="Optimize of the index naming scheme.";

SolveEqMotionF::usage="Solve the equations of motion for the F auxiliary fields and insert the solutions back into the Lagrangian.";

SolveEqMotionD::usage="Solve the equations of motion for the D auxiliary fields and insert the solutions back into the Lagrangian";

SolveEqMotionFD::usage="Solve the equations of motion for all auxiliary fields and insert the solutions back into the Lagrangian";

FR$FTerms::usage="Stores the solution of the equation of motion for the F auxiliary fields";
FR$DTerms::usage="Stores the solution of the equation of motion for the D auxiliary fields";

Ueps::usage="";
Deps::usage="";
EDelta::usage="";
up::usage="";
down::usage="";
TensDot2::usage="";

QSUSY::usage="Supercharge (to get SUSY transformation laws of a superfield)";

QSUSYBar::usage="Supercharge (to get SUSY transformation laws of a superfield)";

DeltaSUSY::usage="Calculation the variation of an expression under a SUSY transformation";

eps0::usage="SUSY transformation parameter.";

eps1::usage="SUSY transformation parameter.";
eps1bar::usage="SUSY transformation parameter.";
eps2::usage="SUSY transformation parameter.";
eps2bar::usage="SUSY transformation parameter.";
eps3::usage="SUSY transformation parameter.";
eps3bar::usage="SUSY transformation parameter.";
eps4::usage="SUSY transformation parameter.";
eps4bar::usage="SUSY transformation parameter.";
eps5::usage="SUSY transformation parameter.";
eps5bar::usage="SUSY transformation parameter.";
eps6::usage="SUSY transformation parameter.";
eps6bar::usage="SUSY transformation parameter.";
eps7::usage="SUSY transformation parameter.";
eps7bar::usage="SUSY transformation parameter.";
eps8::usage="SUSY transformation parameter.";
eps8bar::usage="SUSY transformation parameter.";
eps9::usage="SUSY transformation parameter.";
eps9bar::usage="SUSY transformation parameter.";

DSUSY::usage="Superderivative (to get spinorial superfields from scalar ones)";

DSUSYBar::usage="Superderivative (to get spinorial superfields from scalar ones)";

nc::usage="Non commutative environment.";
ncc::usage="Non commutative environment without spin indices";

Tonc::usage="To a non commutative environment.";

SuperfieldStrengthL::usage="Get the chiral spinorial superfield associated to a scalar superfield.";

SuperfieldStrengthR::usage="Get the antichiral spinorial superfield associated to a scalar superfield.";

GaugeGroupTest::usage="Test the consistency of the gauge group options, in the case of both Superfield and GaugeBoson are present.";

SuperCurrent::usage="Compute the supercurrent associated to a SUSY Lagrangian.";

FR$Sugra=1;


(* ::Subsection:: *)
(*Loop stuff*)


FR$Loop::usage="allow for bilinear terms in FeynmanRules[]";

FR$deltaZ::usage="Wave function renormalization constants.";

FR$delta::usage="Parameter renormalization constants.";

FR$deltat::usage="tadpole renormalization constants.";

FieldRenormalization::usage="Derive renormalized field from bare field.";

ParameterRenormalization::usage="Derive renormalized parameters from bare paramneters.";

TadpoleRenormalization::usage="field shifts for the renormalisation of the tadpole";

ExtractCounterterms::usage="Main function to extract the counterterms from an expression.";

LoopOrder::usage="Loop orders for the FeynArts interface.";

FR$LoopDum::usage="Flag to count the number of loops";

FR$LoopOrder::usage="Number of loops";

CnumQ[FR$LoopDum]=False;
numQ[FR$LoopDum]=True;

FR$LoopSwitches::usage = "Give the list of exchange between the external parameters and the internal one for the on-shell renormalization";
FR$RmDblExt::usage = "Give the list of replacement rules between the external parameters that are doubly defined";

FR$Eps::usage = "Internal FeynRules symbol for the dimensional regulator.";
FR$MU::usage = "Internal FeynRules symbol for the renormalisation scale.";
FR$Cond::usage = "Internal FeynRules symbol for the condition function FR$Cond[a,b,c] = If[a==0,b,c].";

FR$CT::usage = "FeynRules symbole for the one-loop counterterms expansion";

OnShellRenormalization::usage = "return the renormalized Lagrangian in the on-shell scheme, i.e. all the masses are considered as external parameters. The 
options are QCDOnly, FlavorMixing, Only2Point, Simplify2pt and Exclude4ScalarsCT";

QCDOnly::usage = "If True, the on-shell renormalization is only done for the field having QCD interactions and the couplings with non-zero QDC interaction order.
 The default is False.";

FlavorMixing::usage = "If True, wave function renormalization include mixing between all the different fields with same quantum numbers. If a list of pairs of particle names (classmember 
 should be used if present), only the mixing between the pair of particles is allowed. The default is True.";

Only2Point::usage = "If True, only the mass and field are renormalized and not the other independent couplings. The default is False.";

Simplify2pt::usage="If True, the mass and kinetic terms are simplified using the value of the internal parameter before renormalization is performed. 
Default is True";

Exclude4ScalarsCT::usage="if True, the four scalar terms are keep in the lagrangian but not renormalized. Default is False";


numQ[FR$Eps]=True;
CnumQ[FR$Eps]=False;


R2Vertices::usage = "Option of WriteUFO. The list of R2 vertices, as they come out of FeynArts. The default is an empty list.";
UVCounterterms::usage = "Option of WriteUFO. The list of UV counterterms, as they come out of FeynArts. The default is an empty list.";
CTParameters::usage = "Option of WriteUFO. The list of CT paramters, as they come out of FeynArts. The default is an empty list.";
IPL::usage = "Internal variable tagging the particles running insides a loop.";


NLOCT$assumptions::usage = "List of assumptions returned by NLOCT.";


(* ::Subsection::Closed:: *)
(*Color representations*)


Sextet::usage = "Symbol to denote sextet indices.";

T6::usage = "Symbol used to denote the generators of the sextet representation.";

K6::usage = "Symbol used to denote the sextet-triplet-triplet Clebsch-Gordon.\n
See arXiv:0909.2666 for details and conventions.";

K6bar::usage = "Symbol used to denote the comnplex conjugate of the sextet-triplet-triplet Clebsch-Gordon.\n
See arXiv:0909.2666 for details and conventions.";

K3::usage = "Symbol used to denote the triplet-triplet-triplet Clebsch-Gordon.\n
It evaluates to Eps[i,j,k]/Sqrt[2].\n
See arXiv:0909.2666 for details and conventions.";

K3bar::usage = "Symbol used to denote the complex conjugate of the triplet-triplet-triplet Clebsch-Gordon.\n
It evaluates to Eps[i,j,k]/Sqrt[2].\n
See arXiv:0909.2666 for details and conventions.";


(* ::Subsection::Closed:: *)
(*RGE code*)


RGE::usage="Derivation of all the RGEs.";

GaugeCouplingsRGE::usage="Derivation of the renormalization group equations for the gauge coupling constants.";

GauginoMassesRGE::usage="Derivation of the gaugino mass terms and their RGE.";

YukawaRGEs::usage="Derives yukawas' RGEs.";
BilinearRGEs::usage="Derivation of the RGEs for the bilinear terms' in the superpotential.";
LinearRGEs::usage="RGEs for the linear terms in the superpotential.";
SuperpotentialRGE::usage="Derivation of the RGEs for the superpotential.";

ScaSoftRGE::usage="Derivation of the RGEs for the soft SUSY-breaking terms derived from the superpotential.";

FR$D::usage="Derivative with respect to t";

ExpandSUDot::usage="Expand the SU(N) invariant dot products contained in SUDot";

SUDot::usage="The synthax for a SU(N) invariant product is SUDot[ SF, SF, SF, ..., index], where index corresponds to the contracted SU(N) index and is contained into the explicit indices of each SF.
It works also for list of contracted indices.
A single index is always transformed to a list.
If a contracted index is on the form EpsUnfold[name], it can be explicitely expanded by the function ExpandSUDot.";

EpsUnfold::usage="Tag for unfolding the epsilon tensors.";

SUEps::usage="epsilon tensor appearing in the expansion of the SUDots. Necessary for the RGEs.";

FR$SuperW::usage="Generic function for a superpotential term (FR$Yuk[sf1,sf2,sf3,...])";

FR$SuperWRules::usage="rules to map a FR$Yuk function to the corresponding term in the superpotential.";

FR$Soft::usage="Generic function for a SOFT-SUSY breakin interaction term (FR$Soft[sf1,sf2,sf3,...])";

FR$SoftRules::usage="rules to map a FR$Soft function to the corresponding non-mass term in the soft SUSY-breaking Lagrangian.";

FR$ScalarMassRules::usage="rules to map a FR$Soft function to the corresponding scalar mass term in the soft SUSY-breaking Lagrangian.";

ComputeBetag1::usage="Compute the first coefficient of the beta functions of the gauge groups. Results are stored in FR$betag1.";
ComputeBetag2::usage="Compute the second coefficient of the beta functions of the gauge groups. Results are stored in FR$betag2.";

ComputeBetaMi2::usage="Compute the first coefficient of the beta functions of the gaugino mass terms. Results are stored in FR$betaMi2.";

ComputeBetaMs1::usage="Compute the first coefficient of the beta functions of the scalar mass terms. Results are stored in FR$betaMs1.";
ComputeBetaMs2::usage="Compute the seecond coefficient of the beta functions of the scalar mass terms. Results are stored in FR$betaMs2.";

ComputeBetaSoftInt11::usage="Compute the first coefficient of the beta functions of the linear soft interaction terms. Results are stored in FR$betaSI11.";
ComputeBetaSoftInt12::usage="Compute the second coefficient of the beta functions of the linear soft interaction terms. Results are stored in FR$betaSI112.";
ComputeBetaSoftInt21::usage="Compute the first coefficient of the beta functions of the bilinear soft interaction terms. Results are stored in FR$betaSI21.";
ComputeBetaSoftInt22::usage="Compute the second coefficient of the beta functions of the bilinear soft interaction terms. Results are stored in FR$betaSI22."
ComputeBetaSoftInt31::usage="Compute the first coefficient of the beta functions of the trilinear soft interaction terms. Results are stored in FR$betaSI31.";
ComputeBetaSoftInt32::usage="Compute the second coefficient of the beta functions of the trilinear soft interaction terms. Results are stored in FR$betaSI32.";

ComputeBetaSuperW11::usage="Compute the first coefficient of the beta functions of the linear superpotential terms. Results are stored in FR$betaSW11.";
ComputeBetaSuperW21::usage="Compute the first coefficient of the beta functions of the bilinear superpotential terms. Results are stored in FR$betaSW21.";
ComputeBetaSuperW31::usage="Compute the first coefficient of the beta functions of the trilinear superpotential terms. Results are stored in FR$betaSW31.";
ComputeBetaSuperW12::usage="Compute the second coefficient of the beta functions of the linear superpotential terms. Results are stored in FR$betaSW12.";
ComputeBetaSuperW22::usage="Compute the second coefficient of the beta functions of the bilinear superpotential terms. Results are stored in FR$betaSW22.";
ComputeBetaSuperW32::usage="Compute the second coefficient of the beta functions of the trilinear superpotential terms. Results are stored in FR$betaSW32.";

WriteSuSpectOutput::usage="Writes files to be linked to SuSpect 3.";

yVector::usage="";


(* ::Subsection:: *)
(*Decay package*)


CalculateM2Decays::usage="Compute the matrix elements relevant for all the 1 -> 2 decays.";
ExtractFeynmanRules::usage="Option for the decay package.";

SquaredM::usage="Option of the ComputeDecay function containing the list of the squared matrix elements relevant for the 1>2 decays.";
Lagrangian::usage="Option of the ComputeDecay function containing the Lagrangian.";

ComputeDecay::usage="Core function for the decay package";
ComputeDecays::usage="Core function for the decay package";

ComputeWidths::usage = "Function taking a list of vertices as input, and computing all the two-body widths from it.";

UpdateWidths::usage = "Updates the numerical values for the widths by those computed by ComputeWidths[ ].";

AddDecays::usage = "Option of WriteUFO. If True, then two-body decays are added to the UFO output. The default is True.";
SimplifyDecays::usage = "Option of ComputeWidths WriteUFO. If True, Mathematica tries to simplify the reuslts for the partial widths using the Simplify function. The default is False.";

GLine::usage = "Internal Gamma trace";
FR$myidx::usage="Internal index";


PartialWidth::usage = "ParticleWidth[in -> out] returns the partial width of the particle 'in' decaying into the particles specified in the list out.";
TotWidth::usage = "Width[in] returns the total width of the particle 'in'.";
BranchingRatio::usage = "BranchingRatio[in -> out] returns the branching ratio of the partial in decaying into the particles specified in the list out.";


(* ::Subsection::Closed:: *)
(*Mass diagonalization variables*)


M$vevs::usage="Contains the list of the fields acquiring a vev";

M$MixingsDescription::usage = "Contains the definitions of the rotations from the gauge-eigenbasis to the mass-eigenbasis.";

MassBasis::usage="Option when declaring mixings. It contains as a list a mass basis or a list of mass bases.";

GaugeBasis::usage="Option when declaring mixings. It contains as a list a gauge basis.";

MixingMatrix::usage="Option when declaring mixings. It contains a mixing matrix or a list of mixing matrices.";

MatValue::usage="Option when declaring mixings. It contains the numerical value of a predefined mixing matrix.";

FR$ToGaugeBasis::usage="List with all the rotations from the mass basis to the gauge basis";

MyPattern::usage="Internal Pattern";

Pseudoscalar::usage="Denotes the pseudoscalar part of a field";

Vector::usage = "Denotes a vector field";

ComputeTreeLevelMassMatrix::usage="Function extracting all the mass matrices from a Lagrangian.";
ComputeMassMatrix::usage="Function calling ComputeTreeLevelMassMatrix and (soon) ComputeOneLoopMassMatrix"

FR$MassMatrices::usage="List with the mass matrix names";

MassMatrix::usage="Convert a mass matrix name ot the corresponding analytical matrix";

MatrixSymbol::usage="Get the symbol used for a mixing matrix";

MixMatrix::usage="Get the predefined value of a mixing matrix";

Mix::usage="Tag for a mixing relation.";

MixingSummary::usage="Get a summary on one mixing relation";

WriteASperGe::usage="Writes the C++ code for the mass diagonalization";

$ASperGeDir::usage="";

Basis1::usage="";

Basis2::usage="";

FR$Debug::usage="To print many messages in ExpandIndices.";

RunASperGe::usage="Execute the ASPerGe program";


(* ::Subsection::Closed:: *)
(*Form factors*)


Particles::usage = "Option of the form factor classes, wich specifies the vertex to which the form factor will be attached.";


(* ::Subsection::Closed:: *)
(*Counters*)


FR$DecayCounter::usage="Internal counter for decays";

FR$DecayCntb::usage="Internal counter for decays";

FR$FRCounter::usage="Internal FR counter";

FR$FeynmanRules::usage="Internal FR coubter";

FR$IndexExpandCounter::usage="Internal FR coubter";


(* ::Subsection:: *)
(*EFT*)


NoDefinitions::usage = "To forbid field rotations";



(* ::Section:: *)
(*Error messages and warnings*)


(* ::Subsection:: *)
(*All messages*)


FR$Message::usage = "Messages";


LoadModel::NoClasses = "M$ClassesDescription not defined.";

LoadModel::Part = "Warning : All particles should have different names.";

LoadModel::Loaded = "Warning: Model-file already loaded! Definitions might have been overwritten.";

LoadModel::QN = "Warning: Selfconjugated fields should not carry quantumnumbers.";

LoadModel::Flavor = "IndexRange of FlavorIndex does not match the number of classmembers.";

LoadModel::ClassMembers = "ClassMembers not declared."; 

LoadModel::Gauge = "Only colour singlets (S), triplets (T) and octets (O) are supported.";

LoadModel::PartName = "The number of particles names for MG must correspond to the number of particles in the clas.";

LoadModel::PropLabel = "The number of particles names for MG must correspond to the number of particles in the clas.";

LoadModel::Width = "Wrong width assignment.";

LoadModel::Mass = "Wrong mass assignment.";

LoadModel::BlockName = "No BlockName for external parameter.";

LoadModel::ParamDef = "No definition for internal parameter.";

LoadModel::ExtParams = "Warning: External parameters should be given real values for the matrix element generators to function correctly.";

LoadModel::IntOrderHierarchy = "Warning: Double entries in M$InteractionOrderHierarchy encountered.";

LoadModel::IntOrderLimit = "Warning: Double entries in M$InteractionOrderLimit encountered.";

ParametersMG::EI = "The number of external and internal parameters does not math the total number of parameters.";

Particle::Mass = "All particles must have a different mass.";

Particle::Width = "All particles must have a different width.";

Gauge::WrongAdjInd = "Declared adjoint index does not match gauge boson index.";

Gauge::Abelian = "An abelian Gauge group must be specified by a gauge boson, a charge and a coupling constant.";

Gauge::NonAbelian = "An non abelian Gauge group must be specified by a gauge boson, a list of representations, a structure constant and a coupling constant.";

Gauge::SF = "Both the 'Superfield' and 'GaugeBoson' options are provided. However, they are inconsistent.";

Gauge::Invalid = "A gauge group must be either abelian or non abelian.";

Gauge::Coupling = "Warning: Coupling constant automatically added to parameter list."; 

Def::Definitions = "Warning: Doubly defined definitions.";

PartClass::NoGenMatch = "Number of particles does not match the number of Generations of the class.";

PartClass::FlavRange = "Warning: Flavor range does not match the number of particles in the class. ";

GetGenPos::Pos = "More than one generation index encountered.";

GetVert::FCList = "There's something wrong with FCList.";

Tensor::UnitHerm = "Warning: A matrix should not be at the same time unitary and hermitian."

Tensor::UnitOrth = "Warning: A matrix should not be at the same time unitary and orhtogonal.";

Tensor::HermOrth = "Warning: A matrix should not be at the same time hermitian and orhtogonal.";

FeynRules::Load = "Warning: Package already loaded! Definitions might have been overwritten.";

CheckOpChain::Head = "Wrong Head in operator chain.";

GetFC::Head = "Wrong Head in FC.";

Sym::NoSym = "No symmetrization possible.";

TensDot::ExpandTensDotForMG = "Cannot expand tensor product.";

TeX::NoVert = "No vertices. TeX output will be suppressed.";

PutIndices::NoIndexMatch = "Indices cannot be connected.";

QN::NonConserv = "Warning: non quantum number conserving vertex encountered!";

LoadModel::FAMass = "In FeynRules mode, all class members should be given a mass name.";

PutIndexName::NoIndexMatch = "Waring: Unable to restore particle indices."

FR::Version = "Warning: Mathematica version older than 6.0.";

FA::FlavExp = "Warning: For the FeynArts interface, the lagrangian must be given in flavor expanded form.";

MG::NoStructure = "Warning:";

Goldstone::Scalar = "Warning: Only scalars can be Goldtone bosons.";

Goldstone::List = "Warning: A Goldstone boson is described by a list {V, H}, where V is the vector boson and H the Higgs boson.";

MergeModels::Params = "Warning: Doubly defined parameter classes.";

MergeModels::Gauge = "Warning: Doubly defined gauge group classes.";

MergeModels::Particles = "Warning: Doubly defined particle classes.";

MergeModels::FormFactors = "Warning: Doubly defined form factor classes.";

MergeModels::Index = "Warning: Doubly defined indices.";

vevs::noscalar = "Warning: only scalar fields can acquire a vev.";

Weyl::Chirality = "Warning: Dirac fermions must have both a left and right-handed Weyl component.";

Vertex::MassEigenstates = "Warning: Some vertices contain fields that are not mass eigenstates.\n
Make sure that all indices were correctly expanded over (see the manual), or force flavor expansion by setting FlavorExpand -> True. 
The second option may however increase the computation time dramatically.";

SF::SF = " Superfields must have different names.";

SF::ClassName = "Class Names must be symbols";

SF::Type = "Only chiral and vector superfields are allowed.";

SF::GaugeBoson  = "GaugeBoson must be a vector field.";
VSF::GaugeBoson = "The gauge boson component of a vector superfield must be a declared gauge boson.";

SF::Gaugino  = "Gaugino must be a left-handed Weyl fermion.";
VSF::Gaugino = "The gaugino component of a vector superfield must be a declared Weyl fermion.";

SF::Weyl  = "Weyl must be a Weyl fermion.";
CSF::Weyl = "The Weyl component of a chiral superfield must be a declared Weyl fermion.";
CSF::Chirality = "A chiral superfield must have the same chirality as its Weyl component.";

SF::Scalar  = "Scalar must be a scalar field.";
CSF::Scalar = "The scalar component of a chiral superfield must be a declared scalar field.";

SF::Auxiliary = "The auxiliary field must be an unphysical scalar field.";
CSF::Auxiliary = "The auxiliary field in a chiral superfield cannot be self-conjugate.";
VSF::Auxiliary = "The auxiliary field in a vector superfield must be self-conjugate.";

CSF::Indices = "A chiral superfield and its components must have the same indices.";
VSF::Indices = "A vector superfield and its components must have the same indices.";

CSF::QNumbers = "A chiral superfield and its components must have the same quantum numbers.";

VSF::Boson = "A vector superfield attached to a gauge group must share the same gauge boson as the group, if the latter is provided.";

SuperfieldStrength::NonAbelian = "A abelian spinorial superfield cannot carry other indices than spinorial indices.";
SuperfieldStrength::Abelian = "A non-abelian spinorial superfield must carry gauge indices.";

SuperfieldStrength::Arguments = "The first argument of a spinorial superfield must be a vector superfield.";

DeltaSUSY::Arguments = "The second argument is not a valid SUSY transformation parameter: a left-handed Weyl fermion without any index.";

Colour::Eps = "Non valid Eps[i,j,k] found.";

RGE::InoMass="Gaugino mass Lagrangian not hermitian";

RGE::InoMassLag="Ino terms in LSoft non diagonal";

RGE::SuperWparams="The superpotential must contain at most one term for each parameter ";

RGE::SuperWrenorm="The superpotential contains non-renormalizable interactions";

IntOrder::Overwrite = "Warning: Conflicting definitions for InteractionOrder `1` found for `2`. Last definition is kept.";


UFORestrict::NoInternal = "Internal parameters cannot be restricted. Restriction ignored.";


(* ::Subsection::Closed:: *)
(*Decay.m*)


Decay::Overwrite = "Warning: Previous results will be overwritten.";

Decay::NumMass = "Mass is not numerical. Cannot decide whether decay channel is open.";

Decay::NotOpen = "Decay kinematically not allowed.";

Decay::Channel = "Decay channel not found.";

Decay::NoChannels = "No partial width in memory. Please compute widths first.";

Decay::NumWidth = "Width `1` did not evaluate to a floating point number. `1` not updated.";



(* ::Subsection::Closed:: *)
(*MassDiagonalization.m*)


MassDiag::BasisInconsistency = "Error: Inconsistency between the length of a mass and a gauge basis in the mixing declarations.";

MassDiag::MassBasisInconsistency = "Error: After rotations, a gauge basis gives rise to at least one mass basis and at most two mass bases.";

MassDiag::MassBasisIndexInconsistency = "Error: Inconsistencies with the understood indices _ in some bases.";

MassDiag::ValueName = "Size of the lists provided in Value and MixingMatrix inconsistent for one of the mixing declarations.";

MassDiag::BlockName = "Size of the lists provided in BlockName and MixingMatrix inconsistent for one of the mixing declarations.";

MassDiag::MissingNumericalValue = "If no symbol is provided for the mixing matrix, a numerical value should be provided.";

MassDiag::ValueGaugeInconsistency = "Inconsistencies between the values provided and the number of mixing matrices required.";

MassDiag::IgnoreMatrixName = "Warning: if the value of a mixing matrix is given, the related parameter name is ignored.";

MassDiag::IgnoreLHBlock = "Warning: if the value of a mixing matrix is given, the corresponding LH Block is implicit and not in the output mixingcard.";

MassDiag::BasisInconsistencyBlank = "Error: the number of understood indices _ in one of the basis is not consistent.";

MassDiag::BasisInconsistencyBlankRot = "Error: the number of understood indices _ in a gauge and mass basis is different.";

MassDiag::NumberofBlank = "Error: problem with the number of indices in the bases.";

MassDiag::DoubleDeclaration = "Error: Double mixing declaration (in the mixing and in the parameter class.";

MassDiag::BasisOptionNrArgs ="Problems with the options in the mixing declarations (number of blocknames, mixing matrices).";

MassDiag::TwoMassBases = "One mixing declaration wrongly contains two mass bases.";

MassDiag::OneMassOneGaugeBases = "One mixing declaration wrongly containing one single mass and one single gauge basis.";

MassDiag::OneMassTwoGaugeBases = "One mixing declaration wrongly containing one single mass basis and two gauge bases.";

MassDiag::Bases = "One mixing declaration wrongly implemented (number of gauge and mass bases).";

MassDiag::GBSPS = "Please specify the scalar (\"S\") or pseudoscalar (\"PS\") basis.";

MassDiag::GBFLR = "Please specify the left-handed (\"L\") or right-handed (\"R\") fermion gauge basis.";

MassDiag::NonExistingMix = "Non existing mixing relation ";

MassDiag::MixDeclUnknown = "Unknown mixing declaration. Please check the types of fields.";

MassDiag::FermionValBases = "Two mixing matrices must be provided for Dirac fermions.";

MassDiag::MajoranaBases = "Majorana mass matrices not implemented.";

MassDiag::IdMix = "The identifier of a mixing must be a string";

MassDiag::Basisprov = "Please provide two bases.";

MassDiag::MixingSummaryArgs = "MixingSummary takes a string as argument";


(* ::Subsection::Closed:: *)
(*Form factors*)


FormFactor::Equal = "Entry `1` in M$FormFactors is not of the type x == y.";
FormFactor::Rule = "Options of form factor `1` are not of the type x -> y or x :> y.";
FormFactor::Option = "Option `1` is not a form factor option.";

FormFactor::Particles = "Form factor `1` has no attribute Particles.";
FormFactor::Value = "Form factor `1` has no attribute Value.";

FormFactor::Vertex = "Warning: Form factor `1` appears in more than one vertex.";
FormFactor::VertexParticles = "Warning: Form factor `1` appears inside a vertex which does not match its Particle option.";


(* ::Subsection:: *)
(*NLO*)


NLO::FR$Eps = "Warning: Coefficients of Laurent series in \[Epsilon] still depend on \[Epsilon].";
NLO::Failed = "Failed to construct Laurent expansion.";

NLO::ExtMass = "Error : Not all the masses are external parameters.";


(* ::Section:: *)
(*Initialisation of the package*)


(* ::Subsection:: *)
(*Recursion limit*)


Unprotect[$RecursionLimit];$RecursionLimit=2500;


(* ::Subsection:: *)
(*Initialisation of some FR system variables*)


MR$DefinitionsDefault = {HC[Except[_?(TensQ)[___], xx_?(CnumQ)]] :> Conjugate[xx], HC[Except[_?(TensQ)[___], xx_?((Not[CnumQ[#]] && numQ[#]) &)]] :> xx, 
          HC[xx_?(Element[#, Reals] === True &)] :> xx, HC[xx_?(Element[#, Complexes] === True &)] :> Conjugate[xx], HC[Complex][aa_, bb_] :> Conjugate[Complex[aa, bb]],
          HC[Rational][aa_, bb_] :> Rational[aa, bb], HC[f_?(Not[CompTensQ[#]] &)] :> f, HCanti[Except[_?(TensQ)[___], xx_?(CnumQ)]] :> Conjugate[xx], HCanti[Except[_?(TensQ)[___], xx_?((Not[CnumQ[#]] && numQ[#]) &)]] :> xx, 
          HCanti[xx_?(Element[#, Reals] === True &)] :> xx, HCanti[xx_?(Element[#, Complexes] === True &)] :> Conjugate[xx], HCanti[Complex][aa_, bb_] :> Conjugate[Complex[aa, bb]],
          HCanti[Rational][aa_, bb_] :> Rational[aa, bb], HCanti[f_?(Not[CompTensQ[#]] &)] :> f};

MR$IntCount = 0;

MR$IntLorCount = 0;

$PDGNb = 9000000;

(* SM PDG codes *)

FR$SMPDGs = Join[Range[6],Range[11,16],Range[21,25]];

MR$QuantumNumbers = {};

FR$VertexNumber = 1;

$ConservedQN = True;

FR$MaxSimplify = 30;

If[Not[ValueQ[MR$ModelLoaded]], MR$ModelLoaded = False];

$Path = Append[$Path, Global`$FeynRulesPath <> "/Models"];

(* MG interface system variables *)

FR$DecomposedGluons = {};
MG$AuxTensCount =1;
FR$DecomposedGluonsTensors = {};

MG$HeavyGluonCount =1;

FR$InterfaceRuns = {};


$TensorSimplifyRules = {};

FR$CouplString = {};


FR$MakeWeylToDirac = {};

FR$FRTemplatePath = Global`$FeynRulesPath <> "/Interfaces/MadGraph";

FR$FRTemplateFiles = Select[Last[StringSplit[#,"/"]]&/@FileNames["*",{FR$FRTemplatePath},1],StringTake[#,1] =!= "."&];



FR$FeynArtsInterface = False;

FR$AutoFlavorExpand = {};


M$InteractionOrderHierarchy = {};
FR$InteractionOrderHierarchy = {};

M$InteractionOrderLimit = {};
FR$InteractionOrderLimit = {};

FR$InteractionOrderPerturbativeExpansion = {};



FR$PartialWidths = {};


$OptimizedParams = {{Sqrt2, Sqrt[2],False}, {SqrtPi, Sqrt[Pi],False}};

(* Initialization of the hypercharge:
   We define Y zero by default for all fields.
   Non zero values have to be given in the model file*)

Y[_] := 0;

FR$InteractionOrderEmergencyReplacement = {};
FR$AdditonalIParamsFromAbbreviations = {};


$UseFierzIdentities = False;


FR$FormFactors = {};


(*CD, 12.01.16: This is a renaming. In older Verison DTerm was called SymmetricTensor. *)
DTerm = SymmetricTensor;


(* ::Subsection::Closed:: *)
(*Initialisation of the superfield variables*)


M$Superfields={};


M$SuperfieldClasses = {}; M$ChiralSuperfieldClasses = {}; M$VectorSuperfieldClasses = {};

M$SuperfieldNames = {};   M$ChiralSuperfieldNames = {};   M$VectorSuperfieldNames = {};


(* ::Subsection::Closed:: *)
(*Beta function-related variables*)


NLoop::usage="Loop level for RGE calculation. Can take values 1 or 2 ";
FR$betag1={}; FR$betag2={}; FR$betaMi1={};FR$betaMi2={}; FR$SuperWRules = {}; FR$SoftRules = {};

FR$betaMs1={};FR$betaMs2={};
FR$betaSI11={};FR$betaSI21={};FR$betaSI31={};
FR$betaSI12={};FR$betaSI22={};FR$betaSI32={};
FR$betaSW11={};FR$betaSW21={};FR$betaSW31={};
FR$betaSW12={};FR$betaSW22={};FR$betaSW32={};
FR$Common={};

FRExp::usage="";
(* Useful? *)
FCasimir::usage"Gives the casimir associated to the reprsentation of the gauge group in which lies the superfield";
FormattingExpr::usage" Helps to reduce MyEps and MyDelta ";
MyDelta/: Conjugate[MyDelta[aa__]] := MyDelta[aa];
SetAttributes[MyDelta, Orderless];
MyDelta::usage="This variable mimics the IndexDelta variable. Some rules that apply to IndexDelta can't be applied to RGEs.";
MyEps/: Conjugate[MyEps[aa__]] := MyEps[aa];
SetAttributes[MyEps, Orderless];
MyEps::usage="This variable mimics the SUEps variable. Some rules that apply to SUEps can't be applied to RGEs.";


(* ::Subsection::Closed:: *)
(*Initialization of the Loop-related variables*)


M$DeltaToParameters={};

M$RenormalizationConstants={};


(* ::Subsection::Closed:: *)
(*Optimization index*)


$OptIndex=1;


(* ::Subsection:: *)
(*The FR$Dot function*)


(* This is an internal version of Dot, which automatically expands over the fields *)

FR$Dot[] = 1;

FR$Dot[xx___, aa_ + b_, yy___] := FR$Dot[xx, aa, yy] + FR$Dot[xx, b, yy];
FR$Dot[xx___, aa_*(b_ + c_), yy___] := FR$Dot[xx, aa * b ,yy] + FR$Dot[xx, aa * c, yy];
FR$Dot[xx___, n_?numQ * f_, yy___] := n * FR$Dot[xx, f, yy];
(* MW edit: as below, only pull out scalar fields with all indices written out *)
FR$Dot[xx___, n_?((ScalarFieldQ[#]  && Not[GhostFieldQ[#] === True] && Length[$IndList[#]] == 0)&)* f_, yy___] := n * FR$Dot[xx, f, yy];
FR$Dot[xx___, n_?((ScalarFieldQ[#]  && Not[GhostFieldQ[#] === True])&)[ind__]* f_, yy___] := n[ind] * FR$Dot[xx, f, yy];
FR$Dot[xx___, Power[n_?((ScalarFieldQ[#]  && Not[GhostFieldQ[#] === True])&), m_]* f_, yy___] := Power[n, m] * FR$Dot[xx, f, yy];
FR$Dot[xx___, f_ /  n_?numQ, yy___] := 1/n * FR$Dot[xx, f, yy];
FR$Dot[xx___, n_?numQ, yy___] := n * FR$Dot[xx, yy];
FR$Dot[xx___, v_?VectorFieldQ * f_, yy___] := v * FR$Dot[xx, f, yy];
FR$Dot[xx___, del[v_?VectorFieldQ, mu_] * f_, yy___] := del[v, mu] * FR$Dot[xx, f, yy];
FR$Dot[xx___, v_?VectorFieldQ, yy___] := v * FR$Dot[xx, yy];
FR$Dot[xx___, del[v_?VectorFieldQ, mu_], yy___] := del[v, mu] * FR$Dot[xx, yy];
FR$Dot[xx___, Power[v_?VectorFieldQ[mu_],2] yy___] := Power[v[mu],2] * FR$Dot[xx, yy];
FR$Dot[xx___, v_?Spin2FieldQ * f_, yy___] := v * FR$Dot[xx, f, yy];
FR$Dot[xx___, del[v_?Spin2FieldQ, mu_] * f_, yy___] := del[v, mu] * FR$Dot[xx, f, yy];
FR$Dot[xx___, v_?Spin2FieldQ, yy___] := v * FR$Dot[xx, yy];
FR$Dot[xx___, del[v_?Spin2FieldQ, mu_], yy___] := del[v, mu] * FR$Dot[xx, yy];
FR$Dot[xx___, s_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True] && Length[$IndList[#]] == 0)&), yy___] := s FR$Dot[xx, yy];
FR$Dot[xx___, del[s_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True] && Length[$IndList[#]] == 0)&), mu_], yy___] := del[s, mu] FR$Dot[xx, yy];
FR$Dot[xx___, s_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True])&)[ind__], yy___] := s[ind] FR$Dot[xx, yy] /; Length[{ind}] == Length[$IndList[s]] ;
FR$Dot[xx___, del[s_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True])&)[ind__], mu_], yy___] := del[s[ind], mu] FR$Dot[xx, yy]/; Length[{ind}] == Length[$IndList[s]];
FR$Dot[xx___, Power[s_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True] && Length[$IndList[#]] == 0)&), n_], yy___] := Power[s,n] FR$Dot[xx, yy];
FR$Dot[xx___, Power[del[s_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True] && Length[$IndList[#]] == 0)&), mu_], n_], yy___] := Power[del[s, mu],n] FR$Dot[xx, yy];
FR$Dot[xx___, Power[s_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True])&)[ind__], n_], yy___] := Power[s[ind],n] FR$Dot[xx, yy] /; Length[{ind}] == Length[$IndList[s]] ;
FR$Dot[xx___, Power[del[s_?((ScalarFieldQ[#] && Not[GhostFieldQ[#] === True])&)[ind__], mu_],n_], yy___] := Power[del[s[ind], mu],n] FR$Dot[xx, yy]/; Length[{ind}] == Length[$IndList[s]];


(* ::Subsection:: *)
(*FieldExpand*)


FieldExpand[x_]:=If[FR$FExpand,Expand[x],Expand[x,_?(Not[FreeQ[#,_?FieldQ]]&)]];


(* ::Subsection::Closed:: *)
(*Timer*)


Timer[expr_] := Block[{timelist},
    timelist = Timing[expr];
    Print[timelist[[1]]];
    timelist[[2]]];


(* ::Subsection:: *)
(*Loading the subroutines*)


If[$VersionNumber < 6, Message[FR::Version]];

$LocalFeynRulesPath = Global`$FeynRulesPath;

Get[ToFileName[$LocalFeynRulesPath, "ToolBox.m"]];






UpdateFRDistributedVariables[] :=    Block[{},
   DistributeDefinitions[ParamList, IParamList, EParamList, MassList, WidthList, PartList, MR$Restrictions, ParamRules, MR$Definitions, NumericalValue, Mass, Width];
];


FR$MaxKernels = If[Global`FR$Parallelize, Global`FR$KernelNumber, 1]; 


Block[{$Path = {$LocalFeynRulesPath, 
                StringJoin[$LocalFeynRulesPath, "/Core"], 
                StringJoin[$LocalFeynRulesPath, "/Interfaces"],
                StringJoin[$LocalFeynRulesPath, "/Interfaces/UFO"]}
       },

    << "FRFormat.m";
    << "PythonForm.m";

    << "OptimizedFlavExp.m";
    << "VertexListHandling.m";

    << "PYSplitStructures.m";

    Begin["PRIVATE`"];
    
    << "ClassDeclarations.m";
    << "Initialisation.m";
    << "FieldDeclarations.m";
    << "ExtractVertexTools.m";
    << "OutputRoutines.m";
    << "PutIndices.m";
    << "VertexRoutine.m";
    << "WeylFermions.m";
    << "Superfields.m";
    << "goldstino.m";
    << "InSurGe.m";
    << "Loop.m";
    <<"Decay.m";
    <<"MassDiagonalization.m";
    <<"FormFactors.m";
    <<"FAToFR.m";
   
    (* Loading interfaces *)


    << "MadGraphInterface.m";
    << "SherpaInterface.m";
    << "CalcHepInterface.m";
    << "TeXInterface.m";
    << "FeynArtsInterface.m";
	<< "WhizardOmegaInterface.m";
    << "PYIntMain.m";
    << "SuSpectInterface.m";
    << "AspergeInterface.m";
    End[];
    FR$Message={True,True,True,True,True,True};
 ];


(* ::Subsection::Closed:: *)
(*Loading the palette*)


(************************************************************************************)
(*    The FeynRules palette      *)

(* CreatePalette[Column[{" ",Style["   Gamma Matrices   ",FontWeight->Bold,FontSize->12],
                 PasteButton[Ga[\[Placeholder]],InputForm[Ga][\[Placeholder]]],PasteButton[Ga[\[Placeholder],\[Placeholder],\[Placeholder]],InputForm[Ga][\[Placeholder],\[Placeholder],\[Placeholder]]],
                 PasteButton[ProjP,ProjP],PasteButton[ProjM,InputForm[ProjM]],
                 PasteButton[ProjP[\[Placeholder],\[Placeholder]],InputForm[ProjP][\[Placeholder],\[Placeholder]]],
                 PasteButton[ProjM[\[Placeholder],\[Placeholder]],InputForm[ProjM][\[Placeholder],\[Placeholder]]],
                 Style["Derivative",FontWeight->Bold,FontSize->12],
                 PasteButton["\[PartialD]"[\[Placeholder],\[Placeholder]],InputForm[del][\[Placeholder],\[Placeholder]]],
                 Style["  Hermitian Conjugate  ",FontWeight->Bold,FontSize->12],
                 PasteButton[HC[\[Placeholder]],InputForm[HC][\[Placeholder]]],
                 Style["  Charge Conjugate  ",FontWeight->Bold,FontSize->12],
                 PasteButton[CC[\[Placeholder]],InputForm[CC][\[Placeholder]]]},Center],
                 WindowTitle->"FeynRules Palette",WindowSize->{Fit,350}]; *)

$FRPaletteAddress = StringJoin[$LocalFeynRulesPath, "/FRPalette.nb"];

FRPalette[] := NotebookOpen[$FRPaletteAddress];



EndPackage[];


(* ::Section:: *)
(*Protection*)


Protect[LoadModel, FeynmanRules, ReadAll, FlavorExpand, MaxParticles, MinParticles, ClassName, ClassMembers, SelfConjugate,
        Indices, IndexStyle, PropagatorLabel, PropagatorArrow, PropagatorType, Unphysical, Definitions, FlavorIndex, PDG, FullName, ParticleName, 
        AntiParticleName, Goldstone, Ghost, QuantumNumbers, TeXParticleName, TeXAntiParticleName, Abelian, GaugeBoson, CouplingConstant, StructureConstant, SymmetricTensor, 
        Representations, Colour, Gauge, Lorentz, Spin, IndexDelta, Ext, External, Internal, ParameterType, Value, Unitary, 
        Orthogonal, Hermitian, TensorClass, Description, TeX, BlockName, AllowSummation, InteractionOrder, QCD, QED, ComplexParameter, ScreenOutput, Name, 
        MinCanonicalDimension, MaxCanonicalDimension, TeXOutput, WriteMGOutput, WriteSHOutput, WriteCHOutput, WriteFeynArtsOutput, ConservedQuantumNumbers, MASS, DECAY, 
        ZERO, NoUnfold, FRBlock, NoValue, NoPDG, NoBlockName, PutIndices, PrePutIndices, GetFieldContent, PrintLagrangian, FR$VersionNumber,FR$VersionDate,GenericFile,DiracIndices, 
        Sextet,FieldExpand,FR$FExpand]



