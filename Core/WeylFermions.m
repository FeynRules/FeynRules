(* ::Package:: *)

(* ::Title:: *)
(*WeylFermions*)


(* ::Subsection::Closed:: *)
(*DeclareWeylFermions*)


(* ::Text:: *)
(*This function is called when loading a model.*)
(**)
(*It defines the link between Dirac and Weyl fermions.*)


       LeftWeylPart[weyl_[s_, inds___]] := Module[{r}, ProjPart[weyl][s,r] DiracPart[weyl][r,inds]];

       LeftWeylPart[Except[_[__], weyl_]] := ProjPart[weyl].DiracPart[weyl];

       RightWeylPart[weyl_[s_, inds___]] := Module[{r}, ProjPart[weyl][r,s] ADiracPart[weyl][r,inds]];

       RightWeylPart[Except[_[__], weyl_]] := ADiracPart[weyl].ProjPart[weyl];


DeclareWeylFermions[] := Block[{dirac, adirac, leftweyl, aleftweyl, rightweyl, arightweyl},
    Do[dirac = FR$MakeWeylToDirac[[dd]];

       leftweyl = WeylComponents[dirac][[1]];
       rightweyl = WeylComponents[dirac][[2]];

       (* Reorder the WeylComponents: 1 -> Left, 2 -> Right *)
       Which[(Chirality[leftweyl] === Left) && (Chirality[rightweyl] === Right),
                  WeylComponents[dirac] = {leftweyl, rightweyl},
             (Chirality[leftweyl] === Right) && (Chirality[rightweyl] === Left),
                  WeylComponents[dirac] = {rightweyl, leftweyl};
                  leftweyl = WeylComponents[dirac[[1]]];
                  rightweyl = WeylComponents[dirac][[2]],
             True,
                  Message[Weyl::Chirality]];

       (* Define the Weyl components for all 4 types of Dirac fermions *)
       WeylComponents[anti[dirac]] = anti /@ WeylComponents[dirac];
       WeylComponents[CC[dirac]] = anti /@ Reverse[WeylComponents[dirac]];
       WeylComponents[CC[anti[dirac]]] = Reverse[WeylComponents[dirac]];

       (* Define the replacements *)
       aleftweyl = anti[leftweyl];
       arightweyl = anti[rightweyl];
       adirac = anti[dirac];

       ProjPart[leftweyl] = ProjM;
       ProjPart[aleftweyl] = ProjP;
       ProjPart[rightweyl] = ProjP;
       ProjPart[arightweyl] = ProjM;

       DiracPart[leftweyl] = dirac;
       DiracPart[aleftweyl] = CC[dirac];
       DiracPart[rightweyl] = dirac;
       DiracPart[arightweyl] = CC[dirac];

       ADiracPart[leftweyl] = CC[adirac];
       ADiracPart[aleftweyl] = adirac;
       ADiracPart[rightweyl] = CC[adirac];
       ADiracPart[arightweyl] = adirac,

 {dd, Length[FR$MakeWeylToDirac]}]];


(* ::Subsection:: *)
(*Chirality*)


Chirality[del[a_,b_]]:=Chirality[a];
Chirality[del[a_[__],b_]]:=Chirality[a]; 


(* ::Section:: *)
(*Sigma matrices*)


TensQ[si[_]] := True;

numQ[si[_,_,_]] := True;
CnumQ[si[_,_,_]] := True;

$IndList[si] = {Index[Lorentz], Index[Spin1], Index[Spin2]};



TensQ[sibar[_]] := True;

numQ[sibar[_,_,_]] := True;
CnumQ[sibar[_,_,_]] := True;

$IndList[sibar] = {Index[Lorentz], Index[Spin2], Index[Spin1]};


HC[si[mu_]]:=si[mu];
HC[sibar[mu_]]:=sibar[mu];


MR$SigmaRules = { si[Except[Index[__], mu_], rr___]    :>  si[Index[Lorentz, mu], rr],
                  si[mu_, Except[Index[__], rr_], ss_] :>  si[mu, Index[Spin1, rr], ss],
                  si[mu_, rr_, Except[Index[__], ss_]] :>  si[mu, rr, Index[Spin2, ss]],
                  sibar[Except[Index[__], mu_], rr___]    :>  sibar[Index[Lorentz, mu], rr],
                  sibar[mu_, Except[Index[__], rr_], ss_] :>  sibar[mu, Index[Spin2, rr], ss],
                  sibar[mu_, rr_, Except[Index[__], ss_]] :>  sibar[mu, rr, Index[Spin1, ss]] };



If[FreeQ[$TensIndRules, si | sibar], 
   $TensIndRules = Join[$TensIndRules, MR$SigmaRules]];


(* ::Section::Closed:: *)
(*Spin indices*)


Index /: weyl_?(WeylFieldQ)[Index[Spin1, s1_], inds___] := weyl[Index[Spin2, s1], inds] /; Chirality[weyl] === Right;
Index /: weyl_?(WeylFieldQ)[Index[Spin2, s1_], inds___] := weyl[Index[Spin1, s1], inds] /; Chirality[weyl] === Left;


(* ::Section:: *)
(*WeylToDirac*)


WeylToDirac[expr_] := Block[{temp = Expand[expr] //. Dot -> FR$Dot //. FR$Dot ->Dot,
     weyls, nonweyls},

   temp = PrepareLagIndexExpansion[temp, False, {}, StandAlone -> True];

   If[temp ===0,
      Return[0]
     ];

   nonweyls = Select[temp, FreeQ[#, si|sibar|_?(WeylFieldQ[#] === True&)]&];
   weyls = Expand[temp - nonweyls];

   If[weyls =!= 0,
      (*Print["Factoring derivatives"];*)
      weyls = weyls //. {Dot[fs1__, del[fs2_?(WeylFieldQ[#] === True&), mu_]] :> del2[Dot[fs1, fs2], mu], Dot[del[fs1_?(WeylFieldQ[#] === True&), mu_], fs2__] :> del1[Dot[fs1, fs2], mu]};  
   
      (*Print["Introducing Gamma matrices"];*)
      weyls = weyls //. {sibar[mu_] :> Dot[Ga[mu], ProjM], si[mu_] :> Dot[Ga[mu], ProjP],
                   sibar[mu_, r_,s_] :> TensDot[Ga[mu], ProjM][r,s], si[mu_, r_,s_] :> TensDot[Ga[mu], ProjP][r,s]};
      (*Print["Introducing Weyls"];*)
      weyls = weyls //. {Dot[fs1__, fs2_?(WeylFieldQ[#] === True&)] :> Dot[fs1, LeftWeylPart[fs2]],
                   Dot[fs1_?(WeylFieldQ[#] === True&), fs2__] :> Dot[RightWeylPart[fs1], fs2]}; 
      (*Print["renaming indices"]; *)
      weyls = weyls /. Spin1|Spin2 -> Spin;
      weyls = weyls //. {del1[Dot[psi1_,psi2_],mu_] :> Dot[del[psi1,mu],psi2], del2[Dot[psi1_,psi2_],mu_] :> Dot[psi1, del[psi2, mu]]};
  
      weyls = PrePutIndices[weyls];
      weyls = TreatMajoranasAndCC[weyls, SymmetryFactor -> True];
      weyls = ApplyDefinitions[weyls] //. Dot -> FR$Dot //. FR$Dot -> Dot;
     ];

   temp = nonweyls + weyls;

   Return[temp];
];
   
  
