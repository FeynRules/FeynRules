(* ::Package:: *)

(* ::Title:: *)
(*Generic Interface: Log file*)


(* ::Section::Closed:: *)
(*Initialize Log file*)


GenIntInitializeLogFile[name_String] := Block[{logfile},
    
   logfile = OpenWrite[name];

   WritePYFRHeader[logfile];
   WritePYCommentLine[logfile, ""];
   WritePYCommentLine[logfile, "This is the logfile for the model " <> M$ModelName];
   WriteString[logfile, "\n"];
   WritePYCommentLine[logfile, "Authors: " <> PYListString[MR$Authors]];
   WritePYCommentLine[logfile, "Model version: " <> PRIVATE`MR$Version];
   
   WriteString[logfile, ""];
   WriteString[logfile, ""];

   Close[logfile];

   (* Initialize the list of lines to be written into the logfile *)
   GenInt$LogFile = {};

   (* Return and exit *)
   Return[name]

];
   


(* ::Section:: *)
(*WriteToLogFile*)


(* ::Text:: *)
(*WriteToLogFile[ name ] appends the content of GenInt$LogFile to the file "name".*)
(**)
(*GenInt$LogFile is emptied at the end of this procedure.*)
(**)


WriteToLogFile[name_String] := Block[{logfile},
   
   logfile = OpenAppend[name];

   WriteString[logfile, # <> "\n"]& /@ GenInt$LogFile;

   Close[logfile];

   GenInt$LogFile = {};


];
