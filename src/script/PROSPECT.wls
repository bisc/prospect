#!/usr/bin/env wolframscript
(* ::Package:: *)

(*Print[$Path]
Print[Directory[]]
Print[$Input]
Print[$InputFileName]
Print["END OF PRINTS"]*)


(*Import[StringJoin[NotebookDirectory[], "/Helper/AllTesting.wl"]]*)
(*Import["/Users/alanismaiel/Documents/GitHub/datagen/src/script/Helper/AllTesting.wl"]*)
(*Import["Helper/AllTesting.wl"]*)
(*Print[$InputFileName]
Print["Front End"]
Print[$FrontEnd]
Print["End of"]
Import[DirectoryName[$InputFileName] <> "Helper/AllTesting.wl"];*)


Import[
If[($FrontEnd === Null), 
DirectoryName[$InputFileName],
StringJoin[NotebookDirectory[], "/"]
]<> "Helper/PROSPECTPackage.wl"]
(*
DirectoryName[$InputFileName] <> "Helper/AllTesting.wl"];
If[$FrontEnd \[Equal] Null,*)


Print[$ScriptCommandLine]


Clear[filename];
filename = None;
If[$FrontEnd === Null, 
filename = DirectoryName[$Input] <> $ScriptCommandLine[[2]],
filename = NotebookDirectory[] <> 
(*"testing/static/StaticEx.txt" (* Change line for other tests *)*)
(*"testing/time_invariant/InvariantEx1.txt"*)
(*"testing/time_invariant/TimeInvariant.txt"*)
(*"testing/static/StaticEx2.txt"*)
(*"testing/time_variant/VariantEx2.txt"*)
"testing/time_variant/TimeVariant.txt"
];


Print[filename]


results = generateData[filename]
Print[results];


(*parse[filename]*)


(*Switch[casetype,
"static", result = staticGenerate[]; Print[result],
"timeinvariant", result = invariantGenerate[]; Print[result],
"timevariant", result = variantGenerate[]; Print[result],
_, Message[generateData::invalidCase]; $Failed
]*)


(*Print[DirectoryName[$Input] <> $ScriptCommandLine[[2]]]*)


(*parse["/Users/alanismaiel/Documents/GitHub/datagen/testing/static/StaticEx.txt"]*)


(*staticGenerate[]*)


(*Print["specs:"];
Print[specs];
Print["variables"];
Print[variables];
Print["values"];
Print[values];
Print["timesteps"];
Print[timesteps];
Print["casetype"];
Print[casetype];*)


(*specs = specs /. (P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]]))*)


(*samples = staticGenerate[]
Print[samples]*)


(* Script Command Lines:
- First input is input file (NEEDS RELATIVE PATH)
- Second input is debug print, 1 indicates full debug, 0 indicates result only. 0 default.
Print[$ScriptCommandLine]
*)
