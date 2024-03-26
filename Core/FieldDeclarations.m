(* ::Package:: *)

(* ::Title:: *)
(*Field declarations*)


(* ::Section::Closed:: *)
(*Useful stuff*)


GoToAnti[field_]:= anti[field] /; Not[AntiFieldQ[field]];
GoToAnti[field_]:= psi /; AntiFieldQ[field];


(* ::Section:: *)
(*LorIndNumber (To be changed if new classes are added)*)


LorIndNumber[field_]:=Which[ScalarFieldQ[field] === True, 0,
                            (DiracFieldQ[field] === True)||(MajoranaFieldQ[field] === True)||(WeylFieldQ[field] === True && 
                               Not[Spin32WeylFieldQ[field] === True])||(VectorFieldQ[field] === True), 1,
							(RSpin32FieldQ[field] === True)||(CSpin32FieldQ[field] === True)||(Spin32WeylFieldQ[field] === True), 2,
                            Spin2FieldQ[field] === True, 2]; 


(* ::Section:: *)
(*Scalar fields*)


(* ::Subsection::Closed:: *)
(*Real fields*)


RScalarField[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      BosonQ[a] = True;
      BosonQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      ScalarFieldQ[a] = True;
      ScalarFieldQ[a[ii___]] := True;
      AntiFieldQ[a] = False;
      anti[a] = a;
      anti[a[ii___]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];];

RScalarField[a_, n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      BosonQ[a] = True;
      BosonQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      ScalarFieldQ[a] = True;
      ScalarFieldQ[a[ii___]] := True;
      NInd[a] = n;
      AntiFieldQ[a] = False;
      anti[a] = a;
      anti[a[ii___]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];];


(* ::Subsection::Closed:: *)
(*Complex fields*)


CScalarField[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a][ii___]] := True;
      BosonQ[a] = True;
      BosonQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      ScalarFieldQ[a] = True;
      ScalarFieldQ[a[ii___]] := True;
      BosonQ[anti[a]] = True;
      BosonQ[anti[a][ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      ScalarFieldQ[anti[a]] = True;
      ScalarFieldQ[anti[a][ii___]] := True;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];

CScalarField[a_, n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a][ii___]] :=True;
      BosonQ[a] = True;
      BosonQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      ScalarFieldQ[a] = True;
      ScalarFieldQ[a[ii___]] := True;
      NInd[a] = n;
      NInd[anti[a]] = n;
      BosonQ[anti[a]] = True;
      BosonQ[anti[a][ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      ScalarFieldQ[anti[a]] = True;
      ScalarFieldQ[anti[a][ii___]] := True;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];


(* ::Section:: *)
(*Spin1/2*)


(* ::Subsection::Closed:: *)
(*Dirac fields*)


DiracField[psi_] := Module[{output},
      AntiFieldQ[psi] = False;
      AntiFieldQ[anti[psi]] = True;
      AntiFieldQ[anti[psi][ii___]] := True;
      FermionQ[psi] = True;
      FermionQ[psi[ii___]] := True;
      FieldQ[psi] = True;
      FieldQ[psi[ii___]] := True;
      DiracFieldQ[psi] = True;
      DiracFieldQ[psi[ii___]] := True;
      CFermionFieldQ[psi] = True;
      CFermionFieldQ[psi[ii___]] := True;
      FermionQ[anti[psi]] = True;
      FermionQ[anti[psi][ii___]] := True;
      FieldQ[anti[psi]] = True;
      FieldQ[anti[psi][ii___]] := True;
      DiracFieldQ[anti[psi]] = True;
      DiracFieldQ[anti[psi][ii___]] := True;
      CFermionFieldQ[anti[psi]] = True;
      CFermionFieldQ[anti[psi][ii___]] := True;
      anti[anti[psi][ii___]] := psi[ii];
      anti[anti[psi]] := psi;
      SpinorFieldQ[psi] = True;
      SpinorFieldQ[psi[ii___]] := True;
      SpinorFieldQ[anti[psi]] = True;
      SpinorFieldQ[anti[psi][ii___]] := True;
      CanonicalDimension[psi] = 3/2;
      CanonicalDimension[psi[___]] = 3/2;
      CanonicalDimension[anti[psi]] = 3/2;
      CanonicalDimension[anti[psi][___]] = 3/2;
      Format[psi[ii__], TraditionalForm] := Subscript[psi, ii];
      Format[psi[ii__], StandardForm] := Subscript[psi, ii];
      Format[anti[psi], TraditionalForm] := Overscript[psi,"-"];
      Format[anti[psi][ii__], TraditionalForm] := Subscript[Overscript[psi, "-"],ii];
      Format[anti[psi], StandardForm] := Overscript[psi,"-"];
      Format[anti[psi][ii__], StandardForm] := Subscript[Overscript[psi,"-"],ii];];

DiracField[psi_, n_?IntegerQ] := Module[{output},
      AntiFieldQ[psi] = False;
      AntiFieldQ[anti[psi]] = True;
      FermionQ[psi] = True;
      FermionQ[psi[ii___]] := True;
      FieldQ[psi] = True;
      FieldQ[psi[ii___]] := True;
      DiracFieldQ[psi] = True;
      DiracFieldQ[psi[ii___]] := True;
      CFermionFieldQ[psi] = True;
      CFermionFieldQ[psi[ii___]] := True;
      AntiFieldQ[psi] = False;
      AntiFieldQ[anti[psi]] = True;
      NInd[psi] = n;
      NInd[anti[psi]] = n;
      FermionQ[anti[psi]] = True;
      FermionQ[anti[psi][ii___]] := True;
      FieldQ[anti[psi]] = True;
      FieldQ[anti[psi][ii___]] := True;
      DiracFieldQ[anti[psi]] = True;
      DiracFieldQ[anti[psi][ii___]] := True;
      CFermionFieldQ[anti[psi]] = True;
      CFermionFieldQ[anti[psi][ii___]] := True;
      anti[anti[psi][ii___]] := psi[ii];
      anti[anti[psi]] := psi;
      SpinorFieldQ[psi] = True;
      SpinorFieldQ[psi[ii___]] := True;
      SpinorFieldQ[anti[psi]] = True;
      SpinorFieldQ[anti[psi][ii___]] := True;
      CanonicalDimension[psi] = 3/2;
      CanonicalDimension[psi[___]] = 3/2;
      CanonicalDimension[anti[psi]] = 3/2;
      CanonicalDimension[anti[psi][___]] = 3/2;
      Format[psi[ii__], TraditionalForm] := Subscript[psi, ii];
      Format[psi[ii__], StandardForm] := Subscript[psi, ii];
      Format[anti[psi], TraditionalForm] := Overscript[psi,"-"];
      Format[anti[psi][ii__], TraditionalForm] := Subscript[Overscript[psi,"-"],ii] ;
      Format[anti[psi], StandardForm] := Overscript[psi,"-"];
      Format[anti[psi][ii__], StandardForm] := Subscript[Overscript[psi,"-"],ii] ;];


(* ::Subsection::Closed:: *)
(*Majorana fields*)


 MajoranaField[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a[ii___]]] := True; 
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      MajoranaFieldQ[a] = True;
      MajoranaFieldQ[a[ii___]] := True;
      MajoranaFieldQ[anti[a]] = True;
      MajoranaFieldQ[anti[a][ii___]] := True;
      RFermionFieldQ[a] = True;
      RFermionFieldQ[a[ii___]] := True;
      RFermionFieldQ[anti[a]] = True;
      RFermionFieldQ[anti[a][ii___]] := True;
      SpinorFieldQ[a] = True;
      SpinorFieldQ[a[ii___]] := True;
      SpinorFieldQ[anti[a]] = True;
      SpinorFieldQ[anti[a][ii___]] := True;
      CanonicalDimension[a] = 3/2;
      CanonicalDimension[a[___]] = 3/2;
      CanonicalDimension[anti[a]] = 3/2;
      CanonicalDimension[anti[a][___]] = 3/2;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Overscript[a,"-"];
      Format[anti[a][ii__], TraditionalForm] := Subscript[Overscript[a,"-"],ii] ;
      Format[anti[a], StandardForm] := Overscript[a,"-"];
      Format[anti[a][ii__], StandardForm] := Subscript[Overscript[a,"-"],ii];];
      
  MajoranaField[a_,n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a[ii___]]] := True; 
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      MajoranaFieldQ[a] = True;
      MajoranaFieldQ[a[ii___]] := True;
      MajoranaFieldQ[anti[a]] = True;
      MajoranaFieldQ[anti[a][ii___]] := True;
      RFermionFieldQ[a] = True;
      RFermionFieldQ[a[ii___]] := True;
      RFermionFieldQ[anti[a]] = True;
      RFermionFieldQ[anti[a][ii___]] := True;
      SpinorFieldQ[a] = True;
      SpinorFieldQ[a[ii___]] := True;
      SpinorFieldQ[anti[a]] = True;
      SpinorFieldQ[anti[a][ii___]] := True;
      CanonicalDimension[a] = 3/2;
      CanonicalDimension[a[___]] = 3/2;
      CanonicalDimension[anti[a]] = 3/2;
      CanonicalDimension[anti[a][___]] = 3/2;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      NInd[a] = n;
      NInd[anti[a]] = n;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Overscript[a,"-"];
      Format[anti[a][ii__], TraditionalForm] := Subscript[Overscript[a,"-"],ii];
      Format[anti[a], StandardForm] := Overscript[a,"-"];
      Format[anti[a][ii__], StandardForm] := Subscript[a,ii];];



(* ::Subsection::Closed:: *)
(*Weyl fields*)


 WeylField[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a[ii___]]] := True; 
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      WeylFieldQ[a] = True;
      WeylFieldQ[a[ii___]] := True;
      WeylFieldQ[anti[a]] = True;
      WeylFieldQ[anti[a][ii___]] := True;
      SpinorFieldQ[a] = True;
      SpinorFieldQ[a[ii___]] := True;
      SpinorFieldQ[anti[a]] = True;
      SpinorFieldQ[anti[a][ii___]] := True;
      CanonicalDimension[a] = 3/2;
      CanonicalDimension[a[___]] = 3/2;
      CanonicalDimension[anti[a]] = 3/2;
      CanonicalDimension[anti[a][___]] = 3/2;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];
      
  WeylField[a_,n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a[ii___]]] := True; 
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      WeylFieldQ[a] = True;
      WeylFieldQ[a[ii___]] := True;
      WeylFieldQ[anti[a]] = True;
      WeylFieldQ[anti[a][ii___]] := True;
      SpinorFieldQ[a] = True;
      SpinorFieldQ[a[ii___]] := True;
      SpinorFieldQ[anti[a]] = True;
      SpinorFieldQ[anti[a][ii___]] := True;
      CanonicalDimension[a] = 3/2;
      CanonicalDimension[a[___]] = 3/2;
      CanonicalDimension[anti[a]] = 3/2;
      CanonicalDimension[anti[a][___]] = 3/2;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      NInd[a] = n;
      NInd[anti[a]] = n;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];



(* ::Section:: *)
(*Vector fields*)


(* ::Subsection::Closed:: *)
(*Real fields*)


RVectorField[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      BosonQ[a] = True;
      BosonQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      VectorFieldQ[a] = True;
      VectorFieldQ[a[ii___]] := True;
      anti[a] = a;
      anti[a[ii___]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];];

RVectorField[a_, n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      BosonQ[a] = True;
      BosonQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      VectorFieldQ[a] = True;
      VectorFieldQ[a[ii___]] := True;
      NInd[a] = n;
      anti[a] = a;
      anti[a[ii___]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];];


(* ::Subsection::Closed:: *)
(*Complex fields*)


CVectorField[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[anti[a]] = True;
      BosonQ[a] = True;
      BosonQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      VectorFieldQ[a] = True;
      VectorFieldQ[a[ii___]] := True;
      BosonQ[anti[a]] = True;
      BosonQ[anti[a][ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      VectorFieldQ[anti[a]] = True;
      VectorFieldQ[anti[a][ii___]] := True;
      anti[anti[a]] = a;
      anti[anti[a[ii___]]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];

CVectorField[a_, n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[anti[a]] = True;
      BosonQ[a] = True;
      BosonQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      VectorFieldQ[a] = True;
      VectorFieldQ[a[ii___]] := True;
      AntiFieldQ[a] = False;(*Duplicate line*)
      AntiFieldQ[anti[a]] = True;(*Duplicate line*)
      NInd[a] = n;
      NInd[anti[a]] = n;
      BosonQ[anti[a]] = True;
      BosonQ[anti[a][ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      VectorFieldQ[anti[a]] = True;
      VectorFieldQ[anti[a][ii___]] := True;
      anti[anti[a]] = a;
      anti[anti[a[ii___]]] = a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];


(* ::Section:: *)
(*Spin3/2*)


(* ::Subsection::Closed:: *)
(*Complex fields*)


CSpin32Field[psi_] := Module[{output},
      AntiFieldQ[psi] = False;
      AntiFieldQ[anti[psi]] = True;
      FermionQ[psi] = True;
      FermionQ[psi[ii___]] := True;
      FieldQ[psi] = True;
      FieldQ[psi[ii___]] := True;
      CSpin32FieldQ[psi] = True;
      CSpin32FieldQ[psi[ii___]] := True;
      CFermionFieldQ[psi] = True;
      CFermionFieldQ[psi[ii___]] := True;
      Spin32FieldQ[psi] = True;
      Spin32FieldQ[psi[ii___]] := True;
      FermionQ[anti[psi]] = True;
      FermionQ[anti[psi][ii___]] := True;
      FieldQ[anti[psi]] = True;
      FieldQ[anti[psi][ii___]] := True;
      CSpin32FieldQ[anti[psi]] = True;
      CSpin32FieldQ[anti[psi][ii___]] := True;
      CFermionFieldQ[anti[psi]] = True;
      CFermionFieldQ[anti[psi][ii___]] := True;
      Spin32FieldQ[anti[psi]] = True;
      Spin32FieldQ[anti[psi][ii___]] := True;
      anti[anti[psi][ii___]] := psi[ii];
      anti[anti[psi]] := psi;
      SpinorFieldQ[psi] = True;
      SpinorFieldQ[psi[ii___]] := True;
      SpinorFieldQ[anti[psi]] = True;
      SpinorFieldQ[anti[psi][ii___]] := True;
      CanonicalDimension[psi] = 3/2;
      CanonicalDimension[psi[___]] = 3/2;
      CanonicalDimension[anti[psi]] = 3/2;
      CanonicalDimension[anti[psi][___]] = 3/2;
      Format[psi[ii__], TraditionalForm] := Subscript[psi, ii];
      Format[psi[ii__], StandardForm] := Subscript[psi, ii];
      Format[anti[psi], TraditionalForm] := Overscript[psi,"-"];
      Format[anti[psi][ii__], TraditionalForm] := Subscript[Overscript[psi, "-"],ii];
      Format[anti[psi], StandardForm] := Overscript[psi,"-"];
      Format[anti[psi][ii__], StandardForm] := Subscript[Overscript[psi,"-"],ii];];

CSpin32Field[psi_, n_?IntegerQ] := Module[{output},
      AntiFieldQ[psi] = False;
      AntiFieldQ[anti[psi]] = True;
      FermionQ[psi] = True;
      FermionQ[psi[ii___]] := True;
      FieldQ[psi] = True;
      FieldQ[psi[ii___]] := True;
      CSpin32FieldQ[psi] = True;
      CSpin32FieldQ[psi[ii___]] := True;
      CFermionFieldQ[psi] = True;
      CFermionFieldQ[psi[ii___]] := True;
      Spin32FieldQ[psi] = True;
      Spin32FieldQ[psi[ii___]] := True;
      AntiFieldQ[psi] = False;
      AntiFieldQ[anti[psi]] = True;
      NInd[psi] = n;
      NInd[anti[psi]] = n;
      FermionQ[anti[psi]] = True;
      FermionQ[anti[psi][ii___]] := True;
      FieldQ[anti[psi]] = True;
      FieldQ[anti[psi][ii___]] := True;
      CSpin32FieldQ[anti[psi]] = True;
      CSpin32FieldQ[anti[psi][ii___]] := True;
      CFermionFieldQ[anti[psi]] = True;
      CFermionFieldQ[anti[psi][ii___]] := True;
      Spin32FieldQ[anti[psi]] = True;
      Spin32FieldQ[anti[psi][ii___]] := True;
      anti[anti[psi][ii___]] := psi[ii];
      anti[anti[psi]] := psi;
      SpinorFieldQ[psi] = True;
      SpinorFieldQ[psi[ii___]] := True;
      SpinorFieldQ[anti[psi]] = True;
      SpinorFieldQ[anti[psi][ii___]] := True;
      CanonicalDimension[psi] = 3/2;
      CanonicalDimension[psi[___]] = 3/2;
      CanonicalDimension[anti[psi]] = 3/2;
      CanonicalDimension[anti[psi][___]] = 3/2;
      Format[psi[ii__], TraditionalForm] := Subscript[psi, ii];
      Format[psi[ii__], StandardForm] := Subscript[psi, ii];
      Format[anti[psi], TraditionalForm] := Overscript[psi,"-"];
      Format[anti[psi][ii__], TraditionalForm] := Subscript[Overscript[psi,"-"],ii] ;
      Format[anti[psi], StandardForm] := Overscript[psi,"-"];
      Format[anti[psi][ii__], StandardForm] := Subscript[Overscript[psi,"-"],ii] ;];


(* ::Subsection::Closed:: *)
(*Real fields*)


 RSpin32Field[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a[ii___]]] := True; 
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      RSpin32FieldQ[a] = True;
      RSpin32FieldQ[a[ii___]] := True;
      RSpin32FieldQ[anti[a]] = True;
      RSpin32FieldQ[anti[a][ii___]] := True;
      RFermionFieldQ[a] = True;
      RFermionFieldQ[a[ii___]] := True;
      RFermionFieldQ[anti[a]] = True;
      RFermionFieldQ[anti[a][ii___]] := True;
      Spin32FieldQ[a] = True;
      Spin32FieldQ[a[ii___]] := True;
      Spin32FieldQ[anti[a]] = True;
      Spin32FieldQ[anti[a][ii___]] := True;
      SpinorFieldQ[a] = True;
      SpinorFieldQ[a[ii___]] := True;
      SpinorFieldQ[anti[a]] = True;
      SpinorFieldQ[anti[a][ii___]] := True;
      CanonicalDimension[a] = 3/2;
      CanonicalDimension[a[___]] = 3/2;
      CanonicalDimension[anti[a]] = 3/2;
      CanonicalDimension[anti[a][___]] = 3/2;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Overscript[a,"-"];
      Format[anti[a][ii__], TraditionalForm] := Subscript[Overscript[a,"-"],ii] ;
      Format[anti[a], StandardForm] := Overscript[a,"-"];
      Format[anti[a][ii__], StandardForm] := Subscript[Overscript[a,"-"],ii];];
      
  RSpin32Field[a_,n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a[ii___]]] := True; 
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      RSpin32FieldQ[a] = True;
      RSpin32FieldQ[a[ii___]] := True;
      RSpin32FieldQ[anti[a]] = True;
      RSpin32FieldQ[anti[a][ii___]] := True;
      RFermionFieldQ[a] = True;
      RFermionFieldQ[a[ii___]] := True;
      RFermionFieldQ[anti[a]] = True;
      RFermionFieldQ[anti[a][ii___]] := True;
      Spin32FieldQ[a] = True;
      Spin32FieldQ[a[ii___]] := True;
      Spin32FieldQ[anti[a]] = True;
      Spin32FieldQ[anti[a][ii___]] := True;
      SpinorFieldQ[a] = True;
      SpinorFieldQ[a[ii___]] := True;
      SpinorFieldQ[anti[a]] = True;
      SpinorFieldQ[anti[a][ii___]] := True;
      CanonicalDimension[a] = 3/2;
      CanonicalDimension[a[___]] = 3/2;
      CanonicalDimension[anti[a]] = 3/2;
      CanonicalDimension[anti[a][___]] = 3/2;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      NInd[a] = n;
      NInd[anti[a]] = n;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Overscript[a,"-"];
      Format[anti[a][ii__], TraditionalForm] := Subscript[Overscript[a,"-"],ii];
      Format[anti[a], StandardForm] := Overscript[a,"-"];
      Format[anti[a][ii__], StandardForm] := Subscript[a,ii];];



(* ::Subsection:: *)
(*Weyl real*)


(* ::Subsection:: *)
(*Weyl fields*)


 WSpin32Field[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a[ii___]]] := True; 
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      WeylFieldQ[a] = True;
      WeylFieldQ[a[ii___]] := True;
      WeylFieldQ[anti[a]] = True;
      WeylFieldQ[anti[a][ii___]] := True;
      SpinorFieldQ[a] = True;
      SpinorFieldQ[a[ii___]] := True;
      SpinorFieldQ[anti[a]] = True;
      SpinorFieldQ[anti[a][ii___]] := True;
      Spin32WeylFieldQ[a] = True;
      Spin32WeylFieldQ[a[ii___]] := True;
      Spin32WeylFieldQ[anti[a]] = True;
      Spin32WeylFieldQ[anti[a][ii___]] := True;
      CanonicalDimension[a] = 3/2;
      CanonicalDimension[a[___]] = 3/2;
      CanonicalDimension[anti[a]] = 3/2;
      CanonicalDimension[anti[a][___]] = 3/2;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];
      
  WSpin32Field[a_,n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[a[ii___]] := False;
      AntiFieldQ[anti[a]] = True;
      AntiFieldQ[anti[a[ii___]]] := True; 
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      WeylFieldQ[a] = True;
      WeylFieldQ[a[ii___]] := True;
      WeylFieldQ[anti[a]] = True;
      WeylFieldQ[anti[a][ii___]] := True;
      Spin32WeylFieldQ[a] = True;
      Spin32WeylFieldQ[a[ii___]] := True;
      Spin32WeylFieldQ[anti[a]] = True;
      Spin32WeylFieldQ[anti[a][ii___]] := True;
      SpinorFieldQ[a] = True;
      SpinorFieldQ[a[ii___]] := True;
      SpinorFieldQ[anti[a]] = True;
      SpinorFieldQ[anti[a][ii___]] := True;
      CanonicalDimension[a] = 3/2;
      CanonicalDimension[a[___]] = 3/2;
      CanonicalDimension[anti[a]] = 3/2;
      CanonicalDimension[anti[a][___]] = 3/2;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      NInd[a] = n;
      NInd[anti[a]] = n;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];



(* ::Section:: *)
(*Spin2*)


(* ::Subsection::Closed:: *)
(*Real fields*)


Spin2Field[h_] := Module[{output},
      AntiFieldQ[h] = False;
      AntiFieldQ[h[ii___]] := False;
      BosonQ[h] = True;
      BosonQ[h[ii___]] := True;
      FieldQ[h] = True;
      FieldQ[h[ii___]] := True;
      Spin2FieldQ[h] = True;
      Spin2FieldQ[h[ii___]] := True;
      anti[h] = h;
      anti[h[ii___]] := h[ii];
      CanonicalDimension[h] = 1;
      CanonicalDimension[h[___]] = 1;
      CanonicalDimension[anti[h]] = 1;
      CanonicalDimension[anti[h][___]] = 1;
      Format[h[ii__], TraditionalForm] := Subscript[h, ii];
      Format[h[ii__], StandardForm] := Subscript[h, ii];];
      
Spin2Field[h_, n_?IntegerQ] := Module[{output},
      AntiFieldQ[h] = False;
      AntiFieldQ[h[ii___]] := False;
      BosonQ[h] = True;
      BosonQ[h[ii___]] := True;
      FieldQ[h] = True;
      FieldQ[h[ii___]] := True;
      Spin2FieldQ[h] = True;
      Spin2FieldQ[h[ii___]] := True;
      NInd[h] = n;
      anti[h] = h;
      anti[h[ii___]] := h[ii];
      CanonicalDimension[h] = 2;
      CanonicalDimension[h[___]] = 2;
      CanonicalDimension[anti[h]] = 2;
      CanonicalDimension[anti[h][___]] = 2;
      Format[h[ii__], TraditionalForm] := Subscript[h, ii];
      Format[h[ii__], StandardForm] := Subscript[h, ii];];


(* ::Subsection::Closed:: *)
(*Complex fields*)


(* NC Added for complex tensor class. *)


CSpin2Field[h_] := Module[{output},
      AntiFieldQ[h] = False;
      AntiFieldQ[anti[h]] = True;
      AntiFieldQ[h[___]] := False;
      AntiFieldQ[anti[h][___]] := True;
      BosonQ[h] = True;
      BosonQ[h[___]] := True;
      FieldQ[h] = True;
      FieldQ[h[___]] := True;
      Spin2FieldQ[h] = True;
      Spin2FieldQ[h[___]] := True;
      BosonQ[anti[h]] = True;
      BosonQ[anti[h][___]] := True;
      FieldQ[anti[h]] = True;
      FieldQ[anti[h][___]] := True;
      Spin2FieldQ[anti[h]] = True;
      Spin2FieldQ[anti[h][___]] := True;
      anti[anti[h]] = h;
      anti[anti[h][ii___]] := h[ii];
      CanonicalDimension[h] = 1;
      CanonicalDimension[h[___]] = 1;
      CanonicalDimension[anti[h]] = 1;
      CanonicalDimension[anti[h][___]] = 1;
      Format[h[ii__], TraditionalForm] := Subscript[h, ii];
      Format[h[ii__], StandardForm] := Subscript[h, ii];
      Format[anti[h], TraditionalForm] := Power[h,\[Dagger]];
      Format[anti[h][ii__], TraditionalForm] := Power[Subscript[h, ii],\[Dagger]];
      Format[anti[h], StandardForm] := Power[h,\[Dagger]];
      Format[anti[h][ii__], StandardForm] := Power[Subscript[h, ii],\[Dagger]];
];
      
CSpin2Field[h_, n_?IntegerQ] := Module[{output},
      NInd[h] = n;
	  NInd[anti[h]] = n; 
	  CSpin2Field[h];
];


(* ::Section::Closed:: *)
(*Ghost fields*)


GhostField[a_] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[anti[a]] = True;
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      ScalarFieldQ[a] = True;
      ScalarFieldQ[a[ii___]] := True;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      ScalarFieldQ[anti[a]] = True;
      ScalarFieldQ[anti[a][ii___]] := True;
      GhostFieldQ[a] = True;
      GhostFieldQ[a[ii___]] := True;
      GhostFieldQ[anti[a]] = True;
      GhostFieldQ[anti[a][ii___]] := True;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];

GhostField[a_, n_?IntegerQ] := Module[{output},
      AntiFieldQ[a] = False;
      AntiFieldQ[anti[a]] = True;
      FermionQ[a] = True;
      FermionQ[a[ii___]] := True;
      FieldQ[a] = True;
      FieldQ[a[ii___]] := True;
      ScalarFieldQ[a] = True;
      ScalarFieldQ[a[ii___]] := True;
      NInd[a] = n;
      NInd[anti[a]] = n;
      FermionQ[anti[a]] = True;
      FermionQ[anti[a][ii___]] := True;
      FieldQ[anti[a]] = True;
      FieldQ[anti[a][ii___]] := True;
      ScalarFieldQ[anti[a]] = True;
      ScalarFieldQ[anti[a][ii___]] := True;
      GhostFieldQ[a] = True;
      GhostFieldQ[a[ii___]] := True;
      GhostFieldQ[anti[a]] = True;
      GhostFieldQ[anti[a][ii___]] := True;
      anti[anti[a]] = a;
      anti[anti[a][ii___]] := a[ii];
      CanonicalDimension[a] = 1;
      CanonicalDimension[a[___]] = 1;
      CanonicalDimension[anti[a]] = 1;
      CanonicalDimension[anti[a][___]] = 1;
      Format[a[ii__], TraditionalForm] := Subscript[a, ii];
      Format[a[ii__], StandardForm] := Subscript[a, ii];
      Format[anti[a], TraditionalForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], TraditionalForm] := Power[Subscript[a,ii] ,\[Dagger]];
      Format[anti[a], StandardForm] := Power[a,\[Dagger]];
      Format[anti[a][ii__], StandardForm] := Power[Subscript[a,ii] ,\[Dagger]];];
