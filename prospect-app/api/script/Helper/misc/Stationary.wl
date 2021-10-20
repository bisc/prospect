(* ::Package:: *)

BeginPackage["Stationary`"];


acquireValList::usage="independence1";
acquireEquations::usage="Acquiring a single equation for the stationary assumption"
stationary::usage="Performing the stationary assumption"


Begin["`Private`"];


(* Error messages *)
acquireValList::invalidVars="Variables must be defined in system";

(* Helper function;
Input: Class of a variable;
Output: Rule decrementing time step of variable in the class by 1
*)
Clear[classRule];
classRule[v_] := Module[{R, newRule},
Clear[R, newRule];
R = v;
newRule = R[x_] :> R[x-1];
newRule
]


(* Helper function (Also in Independence.nb);
Input: List of variables, list of list of values, single variable;Output: List of values corresponding to single variable *)
Clear[acquireValList];
acquireValList[varsTotal0_, valsTotal0_, a0_] := 
Module[{varsTotal = varsTotal0, valsTotal = valsTotal0, a = a0},
varPositionList = Flatten[Position[varsTotal, a]];
If[Length[varPositionList] == 0,
(Message[acquireValList::invalidVars]; $Failed),
varPosition = varPositionList[[1]];
valList = Flatten[valsTotal[[varPosition]]];
valList
]
]

(* Helper function;
Input: List of variables, list of list of values, two lists of selected variables;
Output: List of equations corresponding to the variables;

ASSUMPTION: Variables in t1 have equivalent values to corresponding variables in t2;
Furthermore, the lists are of equal length. Must be held in main function.
*)
Clear[acquireEquations];
acquireEquations[varsTotal0_, valsTotal0_, t10_, t20_] := 
Module[{varsTotal = varsTotal0, valsTotal = valsTotal0, t1 = t10,t2 = t20, tempList,tempVar, insideFuncL, insideFuncR, currTuple, tupleLength, currBooleanL, currBooleanR, tempProb, adjustedVals, totalTuples, addedEquations},
addedEquations = {};
tempList = {};
adjustedVals = {};

(* Creates list of adjusted values *)
Do[
tempList = acquireValList[varsTotal, valsTotal, t1[[i]]];
AppendTo[adjustedVals, tempList]
,{i, Length[t1]}]; (* End of check *)

(* Note: Each tuple is the length of the varLists t1 and t2 *)
totalTuples = Tuples[adjustedVals];

(* Make myProb equations for all tuples *)
Do[
(* Acquire current tuple *)
currTuple = totalTuples[[i]];
tupleLength = Length[currTuple];
currBooleanL = t1[[1]] == currTuple[[1]];
currBooleanR = t2[[1]] == currTuple[[1]];
If [tupleLength > 1,
Do[
insideFuncL[p_] := t1[[j]] == p;
insideFuncR[p_] := t2[[j]] == p;
currBooleanL = currBooleanL && insideFuncL[currTuple[[j]]];
currBooleanR = currBooleanR && insideFuncR[currTuple[[j]]];
,{j,2, Length[currTuple]}];
];
tempProb = myProb[currBooleanL] == myProb[currBooleanR];
AppendTo[addedEquations, tempProb];
,{i, Length[totalTuples]}]; (* End of check *)

addedEquations = Drop[addedEquations, -1];
addedEquations
]

(* 
Input: Classes of variables, list of variables, list of lists of values;
Output: List of equations matching stationary assumption
*)
Clear[stationary];
stationary[classTotal0_, varsTotal0_, valsTotal0_ ] := Module[{addedEquations = {}, classTotal = classTotal0, varsTotal = varsTotal0, valsTotal = valsTotal0, pSet, allRules = {}, tempRule, tempSetShift, tempSetNorm},
pSet = Subsets[varsTotal];

(* Remove empty set from list, no reason to check this *)
pSet = DeleteCases[pSet, {}];

(* Create add rules, includes rules for all variable classes *)
Do[
tempRule = classRule[classTotal[[i]]];
AppendTo[allRules, tempRule];
,{i, Length[classTotal]}];

(* 
UNOPTIMIZED;
For each subset S in pSet: ;
	1. Create a normal and shift set;
	2. Check if the shifted set is in pSet;
		a. If so, then apply acquireEquations on both the normal and shift set; 
*)

(*
Do[
tempSetShift = pSet[[i]];
tempSetNorm = pSet[[i]];

(* Apply all the shift rules *)
Do[
tempSetShift = tempSetShift /. allRules[[j]];
,{j, Length[allRules]}];

If[MemberQ[pSet, tempSetShift],
AppendTo[addedEquations, acquireEquations[varsTotal, valsTotal, tempSetNorm, tempSetShift]];
];
,{i, Length[pSet]}];

Flatten[addedEquations]
*)

(*
OPTIMIZED;
Go in reverse order on subsets in pSet ;
	1. Create a normal and shift set;
	2. Check if the shifted set is in pSet;
		a. If so, then apply acquireEquations on both the normal and shift set; 
		b. Set addedEquations to the result and break out;
*)


Do[
tempSetShift = pSet[[i]];
tempSetNorm = pSet[[i]];

(* Apply all the shift rules *)
Do[
tempSetShift = tempSetShift /. allRules[[j]];
,{j, Length[allRules]}];

(* If the greatest shift is found, set added equations to it, and break *)
If[MemberQ[pSet, tempSetShift],
addedEquations = acquireEquations[varsTotal, valsTotal, tempSetNorm, tempSetShift];
Break[]
];
,{i, Length[pSet], 1, -1}];

addedEquations

];

(* 
Input: Classes of variables, list of variables, list of lists of values;
Output: List of equations matching stationary assumption
*)
Clear[stationaryAll];
stationaryAll[classTotal0_, varsTotal0_, valsTotal0_ ] := Module[{addedEquations = {}, classTotal = classTotal0, varsTotal = varsTotal0, valsTotal = valsTotal0, pSet, allRules = {}, tempRule, tempSetShift, tempSetNorm},
pSet = Subsets[varsTotal];

(* Remove empty set from list, no reason to check this *)
pSet = DeleteCases[pSet, {}];

(* Create add rules, includes rules for all variable classes *)
Do[
tempRule = classRule[classTotal[[i]]];
AppendTo[allRules, tempRule];
,{i, Length[classTotal]}];

(* 
UNOPTIMIZED;
For each subset S in pSet: ;
	1. Create a normal and shift set;
	2. Check if the shifted set is in pSet;
		a. If so, then apply acquireEquations on both the normal and shift set; 
*)

Do[
tempSetShift = pSet[[i]];
tempSetNorm = pSet[[i]];

(* Apply all the shift rules *)
Do[
tempSetShift = tempSetShift /. allRules[[j]];
,{j, Length[allRules]}];

If[MemberQ[pSet, tempSetShift],
AppendTo[addedEquations, acquireEquations[varsTotal, valsTotal, tempSetNorm, tempSetShift]];
];
,{i, Length[pSet]}];

Flatten[addedEquations]
];



End[];


EndPackage[]; 
