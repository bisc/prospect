(* ::Package:: *)

BeginPackage["Independence`"];


acquireValList::usage="independence1"
indep::usage="independence3"
condindep::usage="independence4"


Needs["ParsingInput`"]


Begin["`Private`"];


(* Error messages *)
indepSetToProb::invalidAmount="Not enough variables defined";
indepSetToProb::invalidVars="Variables must be defined in system";

acquireValList::invalidVars="Variables must be defined in system";


(* Helper function: Given list of variables, a list of list of values, and a single variable, return the corresponding list of values *)
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


(* Takes in a set of variables to convert to an independence statement *)
Clear[indepSetToProb];
indepSetToProb[varsTotal0_, valsTotal0_, a0_] := 
Module[{varsTotal = varsTotal0, valsTotal = valsTotal0, 
a = a0,inc = 2, finalInc = 0, adjustedVals = {}, 
result = {}, tempVar, tempList, insideFunc, 
droppedTemp, finalTemp,subsetsSizeT, currentEvents, 
currentSubset, tempVarPos, allPerms, currPerm, leftHalf, rightHalf},
Print[varsTotal];
Print[valsTotal];
Print[a];
finalInc = Length[a];
If[finalInc < 2, 
(Message[indepSetToProb::invalidAmount]; $Failed),
(* Check if all variables are valid, if so then add their list to adjustedVals *)
(* Note: The adjusted value list turns to the form of variable \[Equal] element for all values *)
Do[
tempVar = Length[Position[varsTotal, a[[i]]]];
If[tempVar == 0,
(Message[indepSetToProb::invalidVars]; Throw["Failed"]),
tempList = acquireValList[varsTotal, valsTotal, a[[i]]];
insideFunc[p_] := a[[i]] == p;
droppedTemp = Drop[tempList, -1];
finalTemp = Map[insideFunc, droppedTemp];
AppendTo[adjustedVals, finalTemp]
]
,{i, finalInc}]; (* End of check *)

(* Creating lists of the independence statements 
1) Increment between the numbers 2 and finalInc (amount of independent variables). Denote number as i.
2) For each i, calculate all the subsets of size i that can be made among the variables
3) For each subset of variables, calculate the number of tuples that can be made from their lists
4) For each tuple, create a myProb equation calculating the independence
*)
Do[
subsetsSizeT = Subsets[a, {t1}];
Do[
(* Occurs for each subset of variables *)
currentEvents = {};
currentSubset = subsetsSizeT[[t2]];
  
Do[
tempVarPos = Flatten[Position[a, currentSubset[[t3]]]][[1]];
AppendTo[currentEvents, adjustedVals[[tempVarPos]]];
,{t3, Length[currentSubset]}]; (* Going through a specific subset of variables size t *)

(* Now currentEvents have all lists of events *)
allPerms = Tuples[currentEvents];

Do[
currPerm = allPerms[[t4]];
leftHalf = myProb[Apply[And, currPerm]];
rightHalf = myProb[currPerm[[1]]];
Do[
rightHalf = rightHalf * myProb[currPerm[[t5]]];
,{t5, 2, Length[currPerm]}];
AppendTo[result, leftHalf == rightHalf];
,{t4, Length[allPerms]}];

,{t2, Length[subsetsSizeT]}]; (* Going through all subsets of variables size t *)
,{t1, 2, finalInc}]; (* Incrementing size of equations *)
result

] (* End of If *)
] (* End of Module *)


Clear[indep];
indep[a_] := indepSetToProb[variables, values, a];
(*indep[a_] := 3*)


(* TO DO: Error check the conditioning *)
Clear[condindep];
condindep[a0_, b0_] := 
Module[{a = a0, b = b0 , originalIndep, addCondition,insideFunc, valList, allVals = {}, tuples = {}, retList = {}, tempCond},
originalIndep = indep[a];
(* Get lists of all values *)
Do[
insideFunc[p_] := b[[i]] == p;
valList = acquireValList[variables, values, b[[i]]];
valList = Map[insideFunc, valList];
AppendTo[allVals, valList];
,{i, Length[b]}];

(* tuples has all combinations of input values *)
tuples = Tuples[allVals];
Do[
(* Getting the overall condition *)
Do[
If[r == 1,
tempCond = tuples[[k]][[r]];,
tempCond = tempCond && tuples[[k]][[r]];
];
,{r, Length[tuples[[1]]]}];
addCondition = (myProb[c_]) :> myCondProb[c \[Conditioned] tempCond];
Do[

AppendTo[retList,originalIndep[[k]] /. addCondition];

,{k, Length[originalIndep]}];
,{k, Length[tuples]}];

retList
];


End[];


EndPackage[]; 
