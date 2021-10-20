(* ::Package:: *)

BeginPackage["oGenerator`"];


(*Remove[oGen];*)


oGen::usage="oGenerator1"
givenEquation::usage="oGenerator2"
oOutcomes::usage="oGenerator3"
oDist::usage="oGenerator4"
oZeroOut::usage="oGenerator5"
allOthers::usage="oGenerator6"
unconditionedProbability::usage="oG7"
conditionedProbability::usage="og8"
getFirstElement::usage="og9"
getSecondElement::usage="og10"


oGen::unequalarg="Lists must be the same size";
oDist::unequalarg="Lists must be the same size";
allOthers::unequalarg="Lists must be the same size";
oDist::invalidvalue="Values must coordinate with their variables";
allOthers::invalidvalue="Value must coordinate with the variable";
allOthers::invalidvar="Variable must exist in list";


Needs["Independence`"]


Begin["`Private`"];


Clear[oGen];
oGen[vars0_, vals0_] := 
Module[{vars = vars0, vals = vals0, varL, valL, dimsList, oArray},
varL = Length[vars];
valL = Length[vals];
If[ varL != valL,
(Message[oGen::unequalarg]; $Failed),
dimsList = Map[Length, vals];
oArray = Array[Subscript[o,##]&,dimsList]
]
]


Clear[givenEquation];
givenEquation[oParams0_] := 
Module[ {oParams = oParams0},
Total[Flatten[oParams]] == 1
]


Clear[oOutcomes];
oOutcomes[oParams0_] := 
Module[ {oParams = oParams0},
Flatten[oParams]
]


Clear[oDist];
oDist[varsTotal0_, valsTotal0_, varsSelect0_, valsSelect0_, dist_] := 
Module[{varsTotal = varsTotal0, valsTotal = valsTotal0, varsSelect = varsSelect0, valsSelect = valsSelect0, result = {}, tempIndices = {},checkPosition, oArray = dist},
If[ Length[varsSelect] != Length[valsSelect],
(Message[oDist::unequalarg]; $Failed),
(* If there are multiple instances of same variable in AND statement, return 0; that is an impossible event *)
If[!DuplicateFreeQ[varsSelect],
0,
(* Remove this call, use the one already created *)
AppendTo[result, oArray];

Do[
checkPosition=Position[varsSelect,varsTotal[[i]]];

(* If the current variable is in the list varsSelect ;
1) Determine its position in varsSelect ;
2) Use its varsSelect position to determine its value in valsSelect ;
3) Determine the value's position in the variable's valsTotal list ;
4) Append this position to the results list ;

If the current variable isn't in the list, then append "All" to the list ;

*)
If[Length[checkPosition]>0,
position=checkPosition[[1]][[1]];

(* Add Do Loop for all values in a theoretical list *)

currValues = valsSelect[[position]];

(* Reset temp indices list for every variable *)
tempIndices = {};
Do[
(* Produces error if the value in the valsSelect list isn't a real value for the expected variable *)
valPosition = Flatten[Position[valsTotal[[i]], currValues[[r]]]];
If [Length[valPosition] > 0, 
valPosition = valPosition[[1]];,
(Message[oDist::invalidvalue]; $Failed)];

(* Print[valPosition]; *)
AppendTo[tempIndices, valPosition];,
{r, Length[currValues]}
];
AppendTo[result,tempIndices];
,
(* Print["No value"]; *)
(* Keeps a singleton counter, slightly different process if there only ends up being a single o parameter *)
AppendTo[result,All];];,
{i,Length[varsTotal]}
];

(* At the end, result is a list with {dist, All or a list of indices} ;
	Ex. {dist, All, {2, 3}, {2}, All, {1, 5, 6}} ;
	
dist is the n dimensional array *)

totalList = Flatten[{Apply[Part, result]}];
Total[totalList]
] (* For Inner If *)
] (* For Outer If *)
] (* For Module *)


(* oZeroOut Function ;
Input: ;
	- List of n random variables ;
	- List of n lists of random variable outputs;
	- List of x random variables included in the distribution;
	- List of x lists of outputs the x random variables should take on ;
	- n dimensional array to map the results to (should only be probability array) ;

Output: An n dimensional array such that all values that aren't the selected become 0 ;
*)

Clear[oZeroOut];
oZeroOut[varsTotal0_, valsTotal0_, varsSelect0_, valsSelect0_, dist_] := 
Module[{varsTotal = varsTotal0, valsTotal = valsTotal0, varsSelect = varsSelect0, valsSelect = valsSelect0, result = {}, tempIndices = {},checkPosition, oArray = dist},
If[ Length[varsSelect] != Length[valsSelect],
(Message[oDist::unequalarg]; $Failed),
(* If there are multiple instances of same variable in AND statement, return 0; that is an impossible event *)
If[!DuplicateFreeQ[varsSelect],
0,
(* Remove this call, use the one already created *)
AppendTo[result, oArray];

Do[
checkPosition=Position[varsSelect,varsTotal[[i]]];

(* If the current variable is in the list varsSelect ;
1) Determine its position in varsSelect ;
2) Use its varsSelect position to determine its value in valsSelect ;
3) Determine the value's position in the variable's valsTotal list ;
4) Append this position to the results list ;

If the current variable isn't in the list, then append "All" to the list ;

*)
If[Length[checkPosition]>0,
position=checkPosition[[1]][[1]];

(* Add Do Loop for all values in a theoretical list *)

currValues = valsSelect[[position]];

(* Reset temp indices list for every variable *)
tempIndices = {};
Do[
(* Produces error if the value in the valsSelect list isn't a real value for the expected variable *)
valPosition = Flatten[Position[valsTotal[[i]], currValues[[r]]]];
If [Length[valPosition] > 0, 
valPosition = valPosition[[1]];,
(Message[oDist::invalidvalue]; $Failed)];

(* Print[valPosition]; *)
AppendTo[tempIndices, valPosition];,
{r, Length[currValues]}
];
AppendTo[result,tempIndices];
,
(* Print["No value"]; *)
(* Keeps a singleton counter, slightly different process if there only ends up being a single o parameter *)
AppendTo[result,All];];,
{i,Length[varsTotal]}
];

totalList = Flatten[{Apply[Part, result]}];
Total[totalList]
] (* For Inner If *)
] (* For Outer If *)
] (* For Module *)


(* allOthers Function
Input: List of variables, list of list of values, selected variable, and its selected value
Output: List of values that aren't the selected value*)
Clear[allOthers];
allOthers[varsTotal0_, valsTotal0_, varSelect0_, valSelect0_] := 
Module[{varsTotal = varsTotal0, valsTotal = valsTotal0, varSelect = varSelect0, valSelect = valSelect0, checkPosition, currValues},
varL = Length[varsTotal];
valL = Length[valsTotal];
checkPosition=Position[varsTotal,varSelect];
(* Get position of variable, then get position of value list, then remove selected value from it *)

If[Length[checkPosition]>0,
position=checkPosition[[1]][[1]];
currValues = valsTotal[[position]];
checkPosition = Position[currValues, valSelect];
If[Length[checkPosition] > 0,
DeleteCases[currValues, valSelect],
(Message[allOthers::invalidvalue]; $Failed)
]
,
(Message[allOthers::invalidvar]; $Failed)
]
] (* For Module *)


(* Misc helper rules *)
Clear[extractingANDEvents, equalityPair, inEqualityPair, getFirstElement, getSecondElement];
extractingANDEvents = (myProb[a_]) :> BooleanVariables[a];
equalityPair = (Equal[var_, val_]) :> {var, Flatten[{val}]};
(* Turns inequality of variable and value into equality of variable and set of values *)
inEqualityPair = (Unequal[var_, val_]) :> Equal[var, allOthers[variables, values, var, val]];
getFirstElement[l_] := Part[l, 1];
getSecondElement[l_] := Part[l, 2];


(* Helper rule: given a myProb statement exclusively with AND intersections, returns the myProb statement in terms of o parameters *)
Clear[unconditionedProbability];
unconditionedProbability = (myProb[a_]) :> (
varValList = myProb[a] /. inEqualityPair /. extractingANDEvents /. equalityPair; 
testVars = Map[getFirstElement, varValList];
testVals = Map[getSecondElement, varValList];
oDist[variables,values, testVars, testVals, oGen[variables, values]]
);


(* Given a myCondProb statement exclusively with AND intersections, returns the myCondProb statement in terms of o parameters *)
(* NEEDED: A check to see if top/bottom parameter combination is invalid *)
Clear[conditionedProbability];
conditionedProbability = (myCondProb[a_ \[Conditioned] b_]) :> (
oBottom = myProb[b] /. unconditionedProbability;
all = Join[BooleanVariables[a], BooleanVariables[b]];
oTop = myProb[all] /. unconditionedProbability;
oTop / oBottom
);


End[];


EndPackage[]; 
