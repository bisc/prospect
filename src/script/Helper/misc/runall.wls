#!/usr/bin/env wolframscript
(* ::Package:: *)

Import["oGenerator.wl"]
Import["ParsingInput.wl"]
Import["Independence.wl"]
Import["MainStaticGenerator.wl"]
Import["ProbRules.wl"]


(*myProb[A || B] /. definizeRule*)


(*variables = {X, Y, Z};
values = {{1, 2}, {3, 4}, {5}};*)


(*indep[{X, Y}]*)\[AliasDelimiter]


(*Names["Global`*"]*)


parse["testing/static/StaticEx.txt"];


(*Names["Global`*"]*)


Print[specs];
Print[specs //.definizeRule];


(*(*Print[variables]*)
Print[specs];
staticGenerate[]
Print[variables];*)


Print[x];
x = \!\(\*
TagBox[
StyleBox[
RowBox[{"Equal", "[", 
RowBox[{
RowBox[{"P", "[", 
RowBox[{"Equal", "[", 
RowBox[{"B", ",", "\"\<red\>\""}], "]"}], "]"}], ",", "0.3`"}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\) /. (P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]]))
Print[x]

