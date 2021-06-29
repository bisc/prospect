(* ::Package:: *)

BeginPackage["TimeLists`"];


timeVarVals::usage="Gets the time values for variables, among other things"
timeValExtraction::usage="Gets the values from a single variable"
timeValPresentExtraction::usage="Gets the present time step variables"
timeDistsConditional::usage="Gets conditional time distributions"
singleConditionalDist::usage="Gets single conditional distribution"
validCoordinates::usage="Gets valid coordinate of variables"
tempCoordinate::usage="Helper function: gets temporary coordinate"
valFix::usage="Helper function to put values in list"


Begin["`Private`"];


timeVarVals::unequalarg="Lists must be the same size";
validCoordinates::unequalarg="Coordinates must be the same size";

(* timeVarVals Function ;

Input: List of n relevant time step amounts, list of n variables, list of n lists of random variable outputs ;

Output: List of m variables, list of m lists of random variable outputs
m is the number of random variables considered in the time dependent cases ;

Added Outputs for Generation: ;
- List of m booleans indicating whether the step is a past one or not ;
- List of m booleans indicating whether the step will be a past step on a following iteration ;
- List of k lists of values, where k is the number of past steps (in order of appearance) ;

*)
Clear[timeVarVals];
timeVarVals[steps0_, vars0_, vals0_] :=
Module[{steps = steps0, vars = vars0, vals = vals0, varL, valL, stepL, varsR = {}, valsR = {}, isPastStep = {}, isCurrStep = {}, willBePastStep = {}, pastStepVars = {}, pastStepValues = {}},

varL = Length[vars];
valL = Length[vals];
stepL = Length[steps];
If[ varL != valL || varL != stepL,
(Message[timeVarVals::unequalarg]; $Failed),
Do[
Do[
AppendTo[varsR, vars[[i]][t - j]];
AppendTo[valsR, vals[[i]]];
If[j == 0, 
AppendTo[isPastStep, False];
AppendTo[isCurrStep, True],
AppendTo[isPastStep, True];
AppendTo[isCurrStep, False];
AppendTo[pastStepValues, vals[[i]]];
AppendTo[pastStepVars, vars[[i]][t - j]]
];
If[j == steps[[i]], 
AppendTo[willBePastStep, False],
AppendTo[willBePastStep, True];
];
,{j, 0, steps[[i]]}];
,{i, stepL}];
{varsR, valsR, isPastStep, isCurrStep, willBePastStep, pastStepVars, pastStepValues}
]
]

(* timeValExtraction Function ;

Inputs: List of n values (representing one observation), list of n booleans (willBePastSet list), checking if index will be past step on next iteration ;

Output: List of k values, subset of the input, where each value corresponds to a t+1 past step variable ;

*)
Clear[timeValExtraction];
timeValExtraction[observation_, willBePastStep_] :=
Module[{pastSubset = {}},
Do[
If[willBePastStep[[i]],
AppendTo[pastSubset, observation[[i]]];
];
,{i, Length[observation]}];
pastSubset
]

(* timeValPresentExtraction Function ;

Inputs: List of n values (representing one observation), list of n booleans (isCurrStep list), checking if index will be past step on next iteration ;

Output: List of k values, subset of the input, where each value corresponds to a current step variable ;

*)
Clear[timeValPresentExtraction];
timeValPresentExtraction[observation_, isCurrStep_] :=
Module[{currSubset = {}},
Do[
If[isCurrStep[[i]],
AppendTo[currSubset, observation[[i]]];
];
,{i, Length[observation]}];
currSubset
]


(* timeDistsConditional Function ;

Inputs: ;
	- n dimensional array of elementary probabilities ;
	- List of n booleans (isPastStep list), checking if index is past step or not ;
	- List of m variables (pastStepVars list), giving all past step variables ;
	- List of m lists (pastStepValues list), giving values of all past step variables ;

Output: Function that maps a list of past step values to a conditional distribution;

*)

Clear[timeDistsConditional];
timeDistsConditional[oFinalArray_, isPastStep_, pastStepVars_, pastStepValues_] := 
Module[{f1,thisStepVals, allTuples,allCoords, currDistribution},
allTuples = Tuples[pastStepValues];
(* Going through all possible past step conditions *)
allCoords = Tuples[Range[1,#]&/@Dimensions@oFinalArray];

Do[
thisStepVals = allTuples[[i]];

(* Distribution for the current tuple *)
currDistribution = singleConditionalDist[oFinalArray, isPastStep, pastStepVars, pastStepValues, thisStepVals, allCoords];
(*Print[currDistribution];*)

(* TODO: ;
- Go through all coordinates of oFinalArray ;
	- If it's not a valid coordinate, set its position to 0 in currArray ;
	- If it is a valid coordinate, set its position to its current value divided by conditionalVal ;
	- Make f1 return the final currArray when the input is currTuple
*)
f1[thisStepVals] = CategoricalDistribution[values, currDistribution];
,{i, Length[allTuples]}];

f1
]


(* singleConditionalDist Function ;
Calculate the conditional distribution for a single conditioning ;

Inputs: ;
	- n dimensional array of elementary probabilities ;
	- List of n booleans (isPastStep list), checking if index is past step or not ;
	- List of m variables (pastStepVars list), giving all past step variables ;
	- List of m lists (pastStepValues list), giving values of all past step variables ;
	- List of m values that represent the condition ;
	- Set of tuples representing all possible index variations of past step values ;

Output: A conditional distribution based on the values of thisStepVals;

*)
Clear[singleConditionalDist];
singleConditionalDist[oFinalArray_, isPastStep_, pastStepVars_, pastStepValues_, thisStepVals_, allCoords_] :=
Module[{conditionedDist = oFinalArray, condArray = oFinalArray, currCoord, indexCoord, conditionedVal, valFix, oValList, tempApply},
currCoord = tempCoordinate[isPastStep, thisStepVals, pastStepValues];
(* The probability of the joint condition *)

(* Printing the condition *)
(* Print[Thread[pastStepVars \[Equal] thisStepVals]]; *)

(* Puts all the values in thisStepVals into a list so oDist can calculate *)
valFix[x_] := {x};
oValList = Map[valFix,thisStepVals];
conditionedVal = oDist[variables, values, pastStepVars, oValList, oFinalArray];

(*Print[oValList];
Print[conditionedVal];
Print[currCoord];
Print[];*)


Do[
indexCoord = allCoords[[i]];
tempApply = Join[{conditionedDist}, indexCoord];
If[validCoordinates[currCoord, indexCoord, isPastStep],
conditionedDist[[Sequence@@indexCoord]] = conditionedDist[[Sequence@@indexCoord]] / conditionedVal;,
conditionedDist[[Sequence@@indexCoord]] = 0;
];
,{i, Length[allCoords]}];

(*Print[conditionedDist];
Print[];*)

conditionedDist
]


(* validCoordinate Function (HELPER);
Given two coordinates (representing positions in oFinalArray), deduce if they have the same past step values ;

Inputs: ;
	- Two length n coordinates ;
	- List of n booleans (isPastStep list), checking if index is past step or not ;

Output: Boolean, True if past step values same, False otherwise ;
*)
Clear[validCoordinates];
validCoordinates[c1_, c2_, isPastStep_] :=
Module[{c1L = Length[c1], c2L = Length[c2], stepL = Length[isPastStep], isValid = True},
If[c1L != c2L || c2L != stepL,
(Message[validCoordinates::unequalarg]; $Failed),
Do[
If[isPastStep[[i]],
If[c1[[i]] != c2[[i]],
isValid = False;
Break[]
]
];
,{i, c1L}];
Return[isValid]
]
]

(* tempCoordinate Function (HELPER);

Inputs: ;
	- List of n booleans (isPastStep list), checking if index is past step or not ;
	- List of k values, where k is the number of past steps ;

Output: List of n values, "NA" if the index isn't a past step ;
Note that what the arbitrary value is set to doesn't matter, it just has to be something ;
*)
Clear[tempCoordinate];
tempCoordinate[isPastStep_, currCondition_, pastStepValues_] := 
Module[{finalCoord = {}, index, placeInValList},
index = 1;
Do[
If[isPastStep[[i]],
placeInValList = Flatten[Position[pastStepValues[[index]], currCondition[[index]]]][[1]];
AppendTo[finalCoord, placeInValList];
index = index + 1;,
AppendTo[finalCoord, "NA"];
];
,{i, Length[isPastStep]}];
finalCoord
]


End[];


EndPackage[]; 
