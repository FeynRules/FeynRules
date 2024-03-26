(* ::Package:: *)

(*
   
   $Id: WhizardOmegaInterface.m 2640 2021-05-21 14:19:13Z BenjF $

   This is the sugar coating of WOMathematicaInterface.m which restores its
   functionality as a FR interface.

   Christian Speckner, 2011
*)

Catch[Module[{path, importedopts, importedmisc, symrules},

   (* We override those. *)
   WO`HC[p_] := HC[p] /. {Dot[x_,Ga[0]] :> x, Dot[Ga[0], x_] :> x};

   (* Try to load the module. *)
   WO`InterfacePath = Null;
   If[StringQ[Global`$WOInterfacePath] && Length[FileNames[
         {"WOMathematicaInterface.m"}, Global`$WOInterfacePath]] == 1,
      WO`InterfacePath = Global`$WOInterfacePath
   ];
   If[!StringQ[WO`InterfacePath] && StringQ[Global`$FeynRulesPath] && Length[FileNames[
         {"WOMathematicaInterface.m"},
         path = ToFileName[{Global`$FeynRulesPath, "Interfaces", "WhizardOmega"}]]] == 1,
      WO`InterfacePath = path
   ];
   If[!StringQ[WO`InterfacePath], Throw[Null, "Unable to locate WOMathematicaInterface.m"]];
   Get[ToFileName[WO`InterfacePath, "WOMathematicaInterface.m"]];

   (* Version check *)
   If[WO`APIversion != 3, Throw[Null, "Invalid API version."]];

   (* Import the options for WriteWOOutput. *)
   importedopts = {
      "WOModelName", "WOMaxNcf", "WOGauge", "WOGaugeParameter", "WOVerbose",
      "WOAutoGauge", "WOMaxCouplingsPerFile",
      "WORunParameters", "WOOplotter", "WOFast", "WOWhizardVersion", "WOProgress"
   };
   importedmisc = {"WOUnitarity", "WOFeynman", "WORxi"};
   symrules = (
      MessageName[Evaluate[Symbol["FeynRules`" <> #]], "usage"] = MessageName[Evaluate[Symbol["WO`" <> #]], "usage"];
      Protect[Evaluate[Symbol["FeynRules`" <> #]]];
      Evaluate[Symbol["FeynRules`" <> #]] -> Evaluate[Symbol["WO`" <> #]]
   )& /@ Join[importedopts, importedmisc];
   Options[FeynRules`WriteWOOutput] = Join[(Symbol["FeynRules`" <> #] -> (Symbol["FeynRules`" <> #]
         /. symrules /. Options[WO`WriteOutput]) /. (symrules /. Rule[a_, b_] :> Rule[b, a]))&
      /@ importedopts, {Input -> Null, Output -> Null}
   ];

   (* Dito, but for WriteWOExtParams. *)
   Options[FeynRules`WriteWOExtParams] =
      {FeynRules`WOWhizardVersion -> (FeynRules`WOWhizardVersion /. Options[FeynRules`WriteWOOutput])};

   (* Frontend to WO`WriteOutput *)
   FeynRules`WriteWOOutput[lags___, options___] :=
      FeynRules`WriteWOOutput[{lags}, options] /;
            (And @@ ((Head[#] =!= Rule &) /@ {lags})) &&
            (And @@ ((Head[#] === Rule &)/@ {options})) &&
            (And @@ ((Head[#] =!= List &) /@ {lags}));
   FeynRules`WriteWOOutput[lags_List, options___] := Module[{
         fropts, backendopts, knownopts, unknownopts, vlist, modelname, opts, Addpar, output
      },


      (* Disentangle the different options. I. *)
      opts = {options};
      fropts = Select[opts, !FreeQ[#[[1]]& /@ Options[FeynmanRules], #[[1]]]&];
      knownopts = #[[1]]& /@ Join[Options[WriteWOOutput], Options[FeynmanRules]];
      unknownopts = Select[#[[1]]& /@ opts, FreeQ[knownopts, #]&];
      If[Length[unknownopts] > 0,
         Print["ERROR: unknown option(s): " <> WO`Concat[ToString /@ unknownopts, " , "]];
         Return[Null];
      ];

      (* Prepare options to FeynmanRules. *)
      Addpar[Rule[par_, val_]] := If[FreeQ[fropts, par], AppendTo[fropts, par -> val]];
      Addpar[ScreenOutput -> False];
      If[Length[Names["FeynRules`FlavorExpansion"]] == 0,
         Addpar[FlavorExpand -> True]
      ,
         Addpar[FlavorExpand -> FR$AutoFlavorExpand]
      ];

      (* Advertisement. *)
      Print[" - - - FeynRules interface to WHIZARD/O'Mega - - -"];
      Print[" - - - Authors:  C. Speckner, N. Christensen - - -"];
      Print[" - - -      Please cite arXiv:1010.3251      - - -"];
      Print[""];

      (* The model name - we have our own default here.*)
      modelname = If[FreeQ[#[[1]]& /@ opts, FeynRules`WOModelName],
         "fr_" <> ToString[M$ModelName]
      ,
         WOModelName /. opts
      ];
      opts = Select[opts, (#[[1]] =!= FeynRules`WOModelName)&];

      (* Output directory *)
      output = If[FreeQ[#[[1]]& /@ opts, Output], {}, {WO`WOOutputDir -> Output} /. opts];

      (* Create vertex list. *)
      vlist = Input /. opts /. ParamRules;
      If[Head[vlist] =!= List,
         (* We put this on hold to allow the backend to check the settings for consistency
            before doing the expensive generation of the vertex list. *)
         fropts /. List[pars__] :> (vlist = Hold[Module[{list},
            Print["---"];
            Print["Calculating feynman rules..."];
            FeynRules`FR$AbbIndexSumExpanded = {};
			(*Parallelize - NC*)
		    (*list = FeynRules`MergeVertices @@ (FeynRules`FeynmanRules[#, pars]& /@ lags);*)
            list = FeynRules`FeynmanRules[lags, FilterRules[{pars},Options[FeynmanRules]]];
			(*End Parallelize - NC*)
            If[Length[Names["FeynRules`FlavorExpansion"]] > 0,
               Print["Expanding flavors..."];
               list = Symbol["FeynRules`FlavorExpansion"][list]
            ];
            WO`VertexRules = (#[[1]] -> #[[2]])& /@ FeynRules`FR$AbbIndexSumExpanded;
            Print["---"];
            list /. FeynRules`ParamRules
         ]]);
      ,
         If[Head[FR$AbbIndexSumExpanded] === List,
            WO`VertexRules = (#[[1]] -> #[[2]])& /@ FeynRules`FR$AbbIndexSumExpanded];
      ];

      (* Call backend. *)
      backendopts = Join[
         Select[opts /. symrules, !FreeQ[#[[1]]& /@ Options[WO`WriteOutput], #[[1]]]&]
      ,
        {WO`WOModelName -> modelname, WO`WOVertexList -> vlist,
         WO`WOMassList -> MassList[[2]] /. {NoValue[x_] :> x, NoPDG[x_] :> x} /. ParamRules,
         WO`WOWidthList -> WidthList[[2]] /. {NoValue[x_] :> x, NoPDG[x_] :> x} /. ParamRules,
         WO`WOPartList -> PartList /. {NoValue[x_] :> x, NoPDG[x_] :> x} /. ParamRules,
         WO`WOEParamList -> EParamList /. NoValue[x_] :> x /. ParamRules,
         WO`WOIParamList -> IParamList /. NoValue[x_] :> x /. ParamRules,
         WO`WOExtraComment -> ("FeynRules model: " <> ToString[M$ModelName] <> "\n"
            <> "FeynRules version: " <> ToString[FR$VersionNumber])}
      , output];

      backendopts /. {o___} :> WO`WriteOutput[o];
   ];
   FeynRules`WriteWOOutput::usage = "Write WHIZARD / O'Mega model files.";
   Protect[FeynRules`WriteWOOutput];

   (* The frontend to WO`WriteExtParams. *)
   FeynRules`WriteWOExtParams[file_String, opts___] := Module[{unknownopts},
      (* Check options. *)
      If[Length[unknownopts =
            Select[#[[1]]& /@ {opts}, FreeQ[#[[1]]& /@ Options[FeynRules`WriteWOExtParams], #]&]],
         Print["ERROR: unknown option(s): " <> WO`Concat[ToString /@ unknownopts, " , "]];
         Return[Null];
      ];

      (* Call WO`WriteExtParams *)
      Join[{opts} /. symrules, {
         WO`WOModelName -> ToString[M$ModelName],
         WO`WOEParamList -> EParamList /. NoValue[x_] :> x /. ParamRules,
         WO`WOMassList -> MassList[[2]] /. NoValue[x_] :> x /. ParamRules,
         WO`WOWidthList -> WidthList[[2]] /. NoValue[x_] :> x /. ParamRules
      }] /. {x___} :> WO`WriteExtParams[file, x];
   ];
   FeynRules`WriteWOExtParams[opts___] := FeynRules`WriteWOExtParams["", opts];
   FeynRules`WriteWOExtParams::usage = "Write parameter file for WHIZARD.";
   Protect[FeynRules`WriteWOExtParams];

], _?StringQ, Print[#2 <> "\nERROR initializing Whizard / O'Mega interface."]&];
