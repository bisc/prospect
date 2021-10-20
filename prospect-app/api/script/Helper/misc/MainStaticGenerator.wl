(* ::Package:: *)

BeginPackage["MainStaticGenerator`"];


staticGenerate::usage="staticgen1"


Needs["ProbRules`"];
Needs["oGenerator`"];
Needs["EqualityCasing`"];
(*Import["ParsingInput.wl"];
Import["Independence.wl"];*)
Needs["Independence`"];
Needs["ParsingInput`"];


Begin["`Private`"];


Clear[staticGenerate]; 

(* Function for static generation *)
staticGenerate[]:=Module[{equalityAdjust,inequalityAdjust,  myProbAdjustEquals,myProbAdjustUnequals, oParams, oOutput, oEquations, oConstraints, oSolve, oRules, oFinalRules, oD, underCheck},

Print["Before"];
Print[specs];
(* Removing all the OR operations in the myProb and myCondProb statements *)
equalityAdjust = (Equal[test1_, test2_]) :> switchEquals[Equal[test1, test2],variables, values];
inequalityAdjust = (Unequal[test1_, test2_]) :> switchUnequals[Unequal[test1, test2],variables, values];
myProbAdjustEquals= myProb[test3_] :> myProb[ test3 /. equalityAdjust];
myProbAdjustUnequals= myProb[test3_] :> myProb[ test3 /. inequalityAdjust];
specs = specs //. myProbAdjustEquals //. myProbAdjustUnequals //.definizeRule //.eventsToDNFExtRule //. myProbAdjustEquals //. myProbAdjustUnequals;

Print["After"];
Print[specs];

(* Parameterizing the specifications with generated o parameters *)
oParams = oGen[ variables, values];
oOutput = oOutcomes[oParams];
oEquations= specs /. unconditionedProbability /. conditionedProbability;
(* Print[oEquations]; *)
AppendTo[oEquations, givenEquation[oParams]];

(* Solving the probability system based on the specifications and constraints *)
oConstraints=#\[Element]Interval[{0,1}]&/@oOutput;
oSolve = TimeConstrained[Solve[Join[oEquations,oConstraints],oOutput,Reals],60,$Failed];
If[FailureQ[oSolve],Print["Invalid system: solver timed out"]];

(* Generating data by placing the solution into a categorical distribution *)
If[!FailureQ[oSolve],
If[Length[oSolve] != 1 || Length[oSolve[[1]]] != Length[oOutput], 
Print[oSolve];
underCheck = TimeConstrained[FindInstance[Join[oEquations,oConstraints],oOutput,Reals, 1],60,$Failed];
Which[FailureQ[underCheck],Print["Invalid system: distribution either underspecified or conflicting"]
,
Length[underCheck] == 1,Print["Underspecified system: needs more valid specifications"]
,
True,Print["Invalid system: some specifications not consistent"]
];
$Failed,
(* List of rules mapping o parameters to probabilities *)
oRules = oSolve[[1]];
(* Editing the rules such that they map to probabilities and not a list w/single probability *)
oFinalRules = oRules//.{x_}:>x;
Print[oFinalRules];
(* A distribution mapping the values and their probabilities *)
oD = CategoricalDistribution[values, oParams /. oFinalRules];

Information[oD, "ProbabilityTable"];
RandomVariate[oD, numsamples]
],
Print["Invalid system: solver timed out"]
]
]


End[];


EndPackage[]; 
