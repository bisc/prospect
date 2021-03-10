#!/usr/bin/env wolframscript
(* ::Package:: *)

Import[StringJoin[NotebookDirectory[], "/Helper/ProbRules.wl"]]
Import[StringJoin[NotebookDirectory[], "/Helper/ParsingInput.wl"]]
Import[StringJoin[NotebookDirectory[], "/Helper/Independence.wl"]]
Import[StringJoin[NotebookDirectory[], "/Helper/oGenerator.wl"]]
Import[StringJoin[NotebookDirectory[], "/Helper/EqualityCasing.wl"]]
Import[StringJoin[NotebookDirectory[], "/Helper/MainStaticGenerator.wl"]]


(*Import[StringJoin[NotebookDirectory[], "/Helper/AllTesting.wl"]]*)
(*Import["/Users/alanismaiel/Documents/GitHub/datagen/src/script/Helper/AllTesting.wl"]*)


parse["/Users/alanismaiel/Documents/GitHub/datagen/testing/static/StaticEx.txt"]


(*staticGenerate[]*)


specs


specs = specs /. (P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]]))


samples = staticGenerate[]
Print[samples]



