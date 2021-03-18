(* ::Package:: *)

BeginPackage["PROSPECTPackage`"];


(* Generator functions *)
staticGenerate::usage="Generating examples of case type static"
invariantGenerate::usage="Generating examples of case type time invariant"
variantGenerate::usage="Generating examples of case type time variant"


Clear[variables, seed, values, timesteps, classes, indepspecs, 
specs, basespecs, casetype, retInput, isPastStep, isCurrStep, 
willBePastStep, pastStepVars, pastStepValues];


(* Parse functions/global variables *)
parse::usage="Parsing input files"
variables::usage= "Global variable for variable classes"
values::usage="Global variable for variable values"
numsamples::usage="Global variable for number of samples"
specs::usage="Global variable for system specifications"
casetype::usage="Global variable for casetype"
classes::usage="Global variable for classes"


(* Parse global variables for time cases *)
timesteps::usage="Global variable for function timesteps"
isPastStep::usage="Global variable for determining which variables are in the blanket"
isCurrStep::usage="Global variable for determining which variables aren't in blanket"
willBePastStep::usage="Global variable for determining which variables will be in the next blanket"
pastStepVars::usage="Global variable for determining variables at past time steps"
pastStepValues::usage="Global variable for determining values of variables at past time steps"


(* Independence functions *)
acquireValList::usage="independence1"
indep::usage="independence3"
condindep::usage="independence4"


(* ProbRules functions (unsure if doing anything) *)
definizeRule::usage="pr1"
eventsToDNFExtRule::usage="pr2"


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

(* NOTE: This is originally from timelists function, couldn't figure out why it wasn't working, so here it is now *)
Clear[timeVarVals];
timeVarVals[steps0_, vars0_, vals0_] :=
Module[{steps = steps0, vars = vars0, vals = vals0, varL, valL, stepL, varsR = {}, valsR = {}, 
isPastStep = {}, isCurrStep = {}, willBePastStep = {}, pastStepVars = {}, pastStepValues = {}},
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


(* Global requirements (from probrules) *)
definizeRule = (myCondProb[a_ \[Conditioned] b_] :> myProb[a \[And] b] / myProb[b]);
eventsToDNFExtRule  = (myProb[a_]) /;( \[Not] (BooleanConvert[a, "DNF"] === a)):>(myProb[BooleanConvert[a, "DNF"]]);
pReplaceRule = (P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]]))


(* oGenerator functions *)
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


(* oGenerator errors *)
oGen::unequalarg="Lists must be the same size";
oDist::unequalarg="Lists must be the same size";
allOthers::unequalarg="Lists must be the same size";
oDist::invalidvalue="Values must coordinate with their variables";
allOthers::invalidvalue="Value must coordinate with the variable";
allOthers::invalidvar="Variable must exist in list";


(* Equals functions *)
switchEquals::usage="Switch equals signs to valid specs"
switchUnequals::usage="Switch unequals signs to valid specs"
switchEqualsQ::usage="Switch equals signs on q parameters"


(* Stationary functions *)
acquireEquations::usage="Acquiring a single equation for the stationary assumption"
stationary::usage="Performing the stationary assumption"
stationaryAll::usage="Getting all stationary equations, should not be used"
classRule::usage="Decrementing time step of variables in class"


(* TimeLists functions *)
timeVarVals::usage="Gets the time values for variables, among other things"
timeValExtraction::usage="Gets the values from a single variable"
timeValPresentExtraction::usage="Gets the present time step variables"
timeDistsConditional::usage="Gets conditional time distributions"
singleConditionalDist::usage="Gets single conditional distribution"
validCoordinates::usage="Gets valid coordinate of variables"
tempCoordinate::usage="Helper function: gets temporary coordinate"
valFix::usage="Helper function to put values in list"


generateData::usage="Generates the data"
debugP::usage="Debug number"


Begin["`Private`"];


(*
*
*
*
*
*
*
*)
(* BEGINNING OF INDEPENDENCE FUNCTIONS *)
(*
*
*
*
*
*
*
*)


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

If[debugP == 1,
Print[varsTotal];
Print[valsTotal];
Print[a];
];

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


(*
*
*
*
*
*
*
*)
(* END OF INDEPENDENCE NOTEBOOK *)
(* BEGINNING OF PARSE NOTEBOOK *)
(*
*
*
*
*
*
*
*)


Clear[parse];
(* Parse text input given file name *)
parse[s_]:=Module[{name=s,input,i,j,strassoc},
timesteps = Null;
input=Import[name, "Lines"];
If[debugP == 1,
Print[input];
];
retInput = input;
(* Process the configs (not specs) *)
i = 1;
While[StringMatchQ[ToString[input[[i]]],__~~": "~~__],
ToExpression[StringReplace[ToString[input[[i]]],": " ->"="]]; (* sets each config attribute to a value *)
If[debugP == 1,
Print[ToString[input[[i]]]];
];
i++
];

(* Association structure helps with interpreting independence specifications *)
strassoc="varvalues=<|"; (* association named varvalues *)
j=1;
While[j<Length[variables],
strassoc=strassoc<>(ToString[variables[[j]]]<>"->"<>ToString[values[[j]]]<>",");
j++
];
strassoc=strassoc<>(ToString[variables[[j]]]<>"->"<>ToString[values[[j]]]<>"|>");
ToExpression[strassoc];
i++;(* skip filler line *)
(* Process the specs *)

(* If not static case, adjust variables accordingly *)
If[timesteps === Null,
If[debugP == 1,
Print["Timesteps don't exist"];
]
,

If[debugP==1,
Print["Timesteps exist"];
];

classes = variables;
(* Generates several important variables required for solving/generating: ;

variables/values: The relevant variables and their corresponding values in the problem ;
- variables is length n, values is length n (list of lists) ;
USED FOR SOLVING AND GENERATING;

isPastStep/willBePastStep: Boolean lists determining if values are past steps currently (isPastStep), or will be past steps on the following iteration (willBePastStep) ;
- Both lists are length n ;
USED FOR GENERATING;

isCurrStep: Boolean list determining if values is at time t (current step) ;
- Length n list ;
NOT CURRENTLY USED;

pastStepVars: List of current past step variables ;
- Length m \[LessEqual] n ;
USED FOR GENERATING;

pastStepValues: List of the values for the current past step variables ;
- Length m \[LessEqual] n ;
USED FOR GENERATING;
 *)
 
{variables, values, isPastStep, isCurrStep, willBePastStep, pastStepVars, pastStepValues} = timeVarVals[timesteps, variables, values];
];

(* If seeded, then set it *)
If[seed === Null,
SeedRandom[];,
SeedRandom[seed];
];

(* Independence *)
indepspecs = {};
If[Length[Position[input, "independence"]] != 0,
i = Position[input, "independence"][[1]][[1]] + 1;
While[i<=Length[input]&&StringContainsQ[ToString[input[[i]]],"indep"],
Module[{line},
line=ToString[input[[i]]];
indepspecs=Join[indepspecs,ToExpression[StringReplace[input[[i]],{"="->"==","|"->","}]]];
i++
]
];
];

basespecs = {};
If[Length[Position[input, "basecase"]] != 0,
i = Position[input, "basecase"][[1]][[1]] + 1;
While[i<=Length[input] && (input[[i]] != ""),
Module[{expr=ToExpression[StringReplace[input[[i]]," ="->" =="]]},
AppendTo[basespecs,expr/.(P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]]))];
i++
];
];
];

specs = {};
If[Length[Position[input, "main"]] != 0,
If[debugP == 1,
Print["functioning"];
];
i = Position[input, "main"][[1]][[1]] + 1;
While[i<=Length[input],
Module[{expr=ToExpression[StringReplace[input[[i]]," ="->" =="]]},
(*Print[FullForm@expr];
expr = (P[A] /. (P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]])));
Print[FullForm@expr];
AppendTo[specs, expr];*)
(*Print[FullForm@expr];
Print["Ran a fullform"];*)
AppendTo[specs, expr /. pReplaceRule];
(*AppendTo[specs,expr/.(P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]]))];*)
i++
];
];
];

specs = Join[specs, indepspecs];
specs = specs /. (P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]]));
(* 
specs={};
While[i\[LessEqual]Length[input]&&StringContainsQ[ToString[input[[i]]],"indep"],
Module[{line},
line=ToString[input[[i]]];
specs=Join[specs,ToExpression[StringReplace[input[[i]],{"="\[Rule]"==","|"\[Rule]","}]]];
i++
]
];
*)

(* If[Length[specs]>0,i++ ]; (* skip filler line if there were independence statements*) *)

(*
(* Numerical specs *)
While[i\[LessEqual]Length[input],
Module[{expr=ToExpression[StringReplace[input[[i]]," ="\[Rule]" =="]]},
AppendTo[specs,expr/.P[e_]\[RuleDelayed](If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]])];
i++
];
];
*)
If[debugP == 1,
Print[specs]
];
]


(*
*
*
*
*
*
*
*)
(* END OF PARSE FUNCTIONS *)
(* BEGINNING OF STATIC GENERATION FUNCTIONS *)
(*
*
*
*
*
*
*
*)


Clear[staticGenerate]; 

(* Function for static generation *)
staticGenerate[]:=Module[{equalityAdjust,inequalityAdjust,  myProbAdjustEquals,myProbAdjustUnequals, oParams, 
oOutput, oEquations, oConstraints, oSolve, oRules, oFinalRules, oD, underCheck, sampleResult = {}},

If[debugP == 1,
Print["Before"];
Print[specs];
];
(* Removing all the OR operations in the myProb and myCondProb statements *)
equalityAdjust = (Equal[test1_, test2_]) :> switchEquals[Equal[test1, test2],variables, values];
inequalityAdjust = (Unequal[test1_, test2_]) :> switchUnequals[Unequal[test1, test2],variables, values];
myProbAdjustEquals= myProb[test3_] :> myProb[ test3 /. equalityAdjust];
myProbAdjustUnequals= myProb[test3_] :> myProb[ test3 /. inequalityAdjust];
specs = specs //. myProbAdjustEquals //. myProbAdjustUnequals //.definizeRule //.eventsToDNFExtRule //. myProbAdjustEquals //. myProbAdjustUnequals;

If[debugP == 1,
Print["After"];
Print[specs];
];

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
If[debugP == 1,
Print[oSolve];
];
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
If[debugP == 1,
Print[oFinalRules];
];
(* A distribution mapping the values and their probabilities *)
oD = CategoricalDistribution[values, oParams /. oFinalRules];

Information[oD, "ProbabilityTable"];
RandomVariate[oD, numsamples]
(*Print[numsamples];
Do[AppendTo[sampleResult, RandomVariate[oD]], numsamples];
sampleResult*)
],
Print["Invalid system: solver timed out"]
]
]


(*
*
*
*
*
*
*
*)
(* END OF STATIC GENERATION NOTEBOOK *)
(* BEGINNING OF O GENERATION NOTEBOOK *)
(*
*
*
*
*
*
*
*)


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


(*
*
*
*
*
*
*
*)
(* END OF O GENERATION NOTEBOOK *)
(* BEGINNING OF EQUALS NOTEBOOK *)
(*
*
*
*
*
*
*
*)


Clear[switchEquals, switchUnequals, switchEqualsQ];

equalityPairNorm = (Equal[var0_, val0_]) :> {var0, val0};
inequalityPairNorm = (Unequal[var0_, val0_]) :> {var0, val0};

switchEquals::valuesEval="Invalid equality\[LongDash]two values";
switchEquals::invalidVarsEqual="Invalid equality\[LongDash]two variables that don't have the same values";
switchEquals::invalidArg="Invalid argument in function; not an equality";

switchUnequals::valuesEval="Invalid inequality\[LongDash]two values";
switchUnequals::invalidVarsEqual="Invalid inequality\[LongDash]two variables that don't have the same values";
switchUnequals::invalidArg="Invalid argument in function; not an inequality";

switchEqualsQ::invalidArg="Invalid argument in function; not an equality";

switchEquals[eqls_, varsTotal0_, valsTotal0_]:=
Module[{eql = eqls, varsTotal = varsTotal0, valsTotal = valsTotal0, currCase, addCondition, checkPair, a, b, firstValList, secondValList, finalBool},
(* Case Type 1: Two Values Equal, Invalid *)
(* Case Type 2: Variable Equals Value, No Change *)
(* Case Type 3: Value Equals Variable, Must Flip *)
(* Case Type 4: Variables Equivalent, requires additional check *)
(* Case Type 5: Does not match to other cases *)
currCase = Switch[eqls,
_?(StringQ)==_?(StringQ),1,
_?(NumericQ)==_?(NumericQ),1,
_?(StringQ)==_?(NumericQ),1,
_?(NumericQ)==_?(StringQ),1,
True,1,
False,1,
x_==_?(StringQ),2,
x_==_?(NumericQ),2,
_?(StringQ)==x_,3,
_?(NumericQ)==x_,3,
x_==y_,4,
_,5];
Switch[
currCase,
1, (Message[switchEquals::valuesEval]; $Failed),
2, eql,
3, 
addCondition = (Equal[val_, var_]) :> Equal[var, val];
eql /. addCondition,
4,
checkPair = eql /. equalityPairNorm;
a = checkPair[[1]];
b = checkPair[[2]];
firstValList = acquireValList[varsTotal, valsTotal,a];
secondValList = acquireValList[varsTotal, valsTotal,b];


If[Sort[firstValList] == Sort[secondValList],
Do[
If[i == 1,
finalBool = (a == firstValList[[i]] && b == firstValList[[i]]);
,
finalBool = finalBool || (a == firstValList[[i]] && b == firstValList[[i]]);
]
,{i, Length[firstValList]}];
finalBool,
(Message[switchEquals::invalidVarsEqual]; $Failed)
]
,
5, (Message[switchEquals::invalidArg]; $Failed)
]
]

switchUnequals[eqls_, varsTotal0_, valsTotal0_]:=
Module[{eql = eqls, varsTotal = varsTotal0, valsTotal = valsTotal0, currCase, addCondition, checkPair, a, b, firstValList, secondValList, finalBool},
(* Case Type 1: Two Values Unequal, Invalid *)
(* Case Type 2: Variable Unequals Value, No Change *)
(* Case Type 3: Value Unequals Variable, Must Flip *)
(* Case Type 4: Variables not equivalent, requires additional check *)
(* Case Type 5: Does not match to other cases *)
currCase = Switch[eqls,
_?(StringQ)!=_?(StringQ),1,
_?(NumericQ)!=_?(NumericQ),1,
_?(StringQ)!=_?(NumericQ),1,
_?(NumericQ)!=_?(StringQ),1,
True,1,
False,1,
x_!=_?(StringQ),2,
x_!=_?(NumericQ),2,
_?(StringQ)!=x_,3,
_?(NumericQ)!=x_,3,
x_!=y_,4,
_,5];

Switch[
currCase,
1, (Message[switchUnequals::valuesEval]; $Failed),
2, eql,
3, 
addCondition = (Unequal[val_, var_]) :> Unequal[var, val];
eql /. addCondition,
4,
checkPair = eql /. inequalityPairNorm;
a = checkPair[[1]];
b = checkPair[[2]];
firstValList = acquireValList[varsTotal, valsTotal,a];
secondValList = acquireValList[varsTotal, valsTotal,b];


If[Sort[firstValList] == Sort[secondValList],
Do[
If[i == 1,
finalBool = (a != firstValList[[i]] && b != firstValList[[i]]);
,
finalBool = finalBool || (a != firstValList[[i]] && b != firstValList[[i]]);
]
,{i, Length[firstValList]}];
finalBool,
(Message[switchUnequals::invalidVarsEqual]; $Failed)
]
,
5, (Message[switchUnequals::invalidArg]; $Failed)
]
]

switchEqualsQ[qParam_] :=
Module[{currCase, retParam = qParam, addCondition},
(* Checks if the q is first or second *)
currCase = Switch[qParam,
_?(NumericQ)==x_,1,
x_==_?(NumericQ),2,
_, 3];
If [currCase == 3,(Message[switchEqualsQ::invalidArg]; $Failed)];
If [currCase == 1,
addCondition = (Equal[num_, q_]) :> Equal[q, num];
retParam = retParam /. addCondition
];
retParam
]


(*
*
*
*
*
*
*
*)
(* END OF EQUALS FUNCTIONS *)
(* BEGINNING OF TIME LISTS FUNCTIONS *)
(*
*
*
*
*
*
*
*)


(*timeVarVals::unequalarg="Lists must be the same size";
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
Module[{steps = steps0, vars = vars0, vals = vals0, varL, valL, stepL, varsR = {}, valsR = {}, 
isPastStep = {}, isCurrStep = {}, willBePastStep = {}, pastStepVars = {}, pastStepValues = {}},
Print["Running this"];
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
]*)

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


(*
*
*
*
*
*
*
*)
(* END OF TIME LISTS PACKAGE *)
(* BEGINNING OF STATIONARY PACKAGE *)
(*
*
*
*
*
*
*
*)


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
Module[{varsTotal = varsTotal0, valsTotal = valsTotal0, t1 = t10,
t2 = t20, tempList,tempVar, insideFuncL, insideFuncR, currTuple, 
tupleLength, currBooleanL, currBooleanR, tempProb, adjustedVals, totalTuples, addedEquations},
addedEquations = {};
tempList = {};
adjustedVals = {};

(*Print["What the heck are these"];
Print[t1];
Print[t2];
*)
(* Creates list of adjusted values *)
Do[
tempList = acquireValList[varsTotal, valsTotal, t1[[i]]];
AppendTo[adjustedVals, tempList]
,{i, Length[t1]}]; (* End of check *)

(* Note: Each tuple is the length of the varLists t1 and t2 *)
totalTuples = Tuples[adjustedVals];
If[debugP == 1,
Print["total tuples"];
Print[totalTuples];
];
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
(*Print["Curr Bool L"];
Print[currBooleanL];
Print["Curr Bool R"];
Print[currBooleanR];*)
tempProb = myProb[currBooleanL] == myProb[currBooleanR];
(*Print["Something to put in"];
Print[tempProb];*)
AppendTo[addedEquations, tempProb];
,{i, Length[totalTuples]}]; (* End of check *)

addedEquations = Drop[addedEquations, -1];
(*Print["Added equations from allEquations"];
Print[addedEquations];*)
addedEquations
]

(* 
Input: Classes of variables, list of variables, list of lists of values;
Output: List of equations matching stationary assumption
*)
Clear[stationary];
stationary[classTotal0_, varsTotal0_, valsTotal0_ ] := Module[{addedEquations = {}, classTotal = classTotal0, 
varsTotal = varsTotal0, valsTotal = valsTotal0, pSet, 
allRules = {}, tempRule, tempSetShift, tempSetNorm},
pSet = Subsets[varsTotal];

(* Remove empty set from list, no reason to check this *)
pSet = DeleteCases[pSet, {}];

(* Create add rules, includes rules for all variable classes *)
Do[
tempRule = classRule[classTotal[[i]]];
AppendTo[allRules, tempRule];
,{i, Length[classTotal]}];

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

(*Print["the temp sets"];
Print[tempSetNorm];
Print[tempSetShift];*)

(* If the greatest shift is found, set added equations to it, and break *)
If[MemberQ[pSet, tempSetShift],
addedEquations = acquireEquations[varsTotal, valsTotal, tempSetNorm, tempSetShift];
Break[]
];
,{i, Length[pSet], 1, -1}];

(*Print["Added equations from stationary"];
Print[addedEquations];*)
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


(*
*
*
*
*
*
*
*)
(* END OF STATIONARY FUNCTIONS *)
(* BEGINNING OF TIME INVARIANT GENERATOR FUNCTIONS *)
(*
*
*
*
*
*
*
*)


Clear[invariantGenerate];

(* Function for time invariant generation *)
invariantGenerate[]:=Module[{ equalityAdjust,inequalityAdjust,  myProbAdjustEquals,myProbAdjustUnequals,

 oParams, oOutput, oEquations, oConstraints, oSolve, oRules, oFinalRules,oFinalArray, oD,

 underCheck, baseCase,nextCase,currStep, condF, observations = {}},

If[debugP == 1,
Print["Adjust Lists"];
Print[specs];
Print[classes];
Print[variables];
Print[values];
Print[];
Print["Other specifications for Generation:"];
Print[isPastStep];
Print[isCurrStep];
Print[willBePastStep];
Print[pastStepVars];
Print[pastStepValues];
Print[];
];

testAdjust = (Equal[test1_, test2_]) :> test1;

(* Creating rules to switch around equal/unequal signs in the specifications *)
equalityAdjust = (Equal[test1_, test2_]) :> switchEquals[Equal[test1, test2],variables, values];
inequalityAdjust = (Unequal[test1_, test2_]) :> switchUnequals[Unequal[test1, test2],variables, values];
myProbAdjustEquals= myProb[test3_] :> myProb[ test3 //. equalityAdjust];
myProbAdjustUnequals= myProb[test3_] :> myProb[ test3 //. inequalityAdjust];

If[debugP == 1,
Print["Added Stationary Assumption"];
];
(* Adds equations assumed by the stationary assumption in the specifications *)
AppendTo[specs,stationary[classes, variables, values]]; 
specs = Flatten[specs]; 
If[debugP == 1,
Print[specs];
];
(* Modify the specifications to have equal signs in the right places/turn conditional distributions into marginal ones/put everything in proper forms *)
specs  = specs //. myProbAdjustEquals  //.definizeRule //. eventsToDNFExtRule //. myProbAdjustUnequals //.definizeRule //. eventsToDNFExtRule  //. myProbAdjustEquals //. myProbAdjustUnequals;

If[debugP == 1,
Print["After"];
Print[specs];
];
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

(* A distribution mapping the values and their probabilities *)
oFinalArray = oParams /. oFinalRules;
If[debugP == 1,
Print["Final Array"];
Print[oFinalArray];
];
oD = CategoricalDistribution[values, oFinalArray];
(* Function that takes in values for next step's blanket, returns its conditioned distribution *)
condF = timeDistsConditional[oFinalArray, isPastStep, pastStepVars, pastStepValues];

(* Generates all instances of the data *)
Do[
If[i == 1,
(* Base case *)
nextCase = RandomVariate[oD];
currStep = timeValPresentExtraction[nextCase, isCurrStep];
AppendTo[observations, currStep];,
(* timeValExtraction determines the values for next step's blanket *)
nextCase = RandomVariate[condF[timeValExtraction[nextCase, willBePastStep]]];
currStep = timeValPresentExtraction[nextCase, isCurrStep];
AppendTo[observations, currStep];
];
,{i, numsamples}];

observations

],
Print["Invalid system: solver timed out"]
]
]


(*
*
*
*
*
*
*
*)
(* END OF TIME INVARIANT GENERATOR FUNCTIONS *)
(* BEGINNING OF Q GENERATOR FUNCTIONS *)
(*
*
*
*
*
*
*
*)


(* Error messages *)
qGen::unequalarg="Lists must be the same size";

(* qGen Function ;
Input: List of m random variables, list of m lists of random variable outputs ;
Output: m dimensional array with numbered q parameters ;
*)
Clear[qGen];
qGen[vars_, vals_] := 
Module[{dimsList, qArray},
If[ Length[vars] != Length[vals],
(Message[qGen::unequalarg]; $Failed),
dimsList = Map[Length, vals];
qArray = Array[Subscript[q,##]&,dimsList]
]
]

(* givenEquation Function ;
Input: n dimensional array for the o parameters, where n is the number of variables ;
Output: A single equation summing all o parameters to 1 ;
*)
Clear[qOutcomes];
qOutcomes[qParams0_] := 
Module[ {qParams = qParams0},
Flatten[qParams]
]

Clear[qDist];
qDist[varsTotal0_, valsTotal0_, varsSelect0_, valsSelect0_, dist_] := oDist[varsTotal0, valsTotal0, varsSelect0, valsSelect0, dist];



(* Misc helper rules exclusively for q parameters *)
Clear[QextractingANDEvents, QequalityPair, QinEqualityPair, QgetFirstElement, QgetSecondElement];
QextractingANDEvents = (myProb[a_]) :> BooleanVariables[a];
QequalityPair = (Equal[var_, val_]) :> {var, Flatten[{val}]};
(* Turns inequality of variable and value into equality of variable and set of values *)
QinEqualityPair = (Unequal[var_, val_]) :> Equal[var, allOthers[baseStepVars, values, var, val]];
QgetFirstElement[l_] := Part[l, 1];
QgetSecondElement[l_] := Part[l, 2];

(* Helper rule: given a myProb statement exclusively with AND intersections, returns the myProb statement in terms of o parameters *)
Clear[QunconditionedProbability];
QunconditionedProbability = (myProb[a_]) :> (
varValList = myProb[a] /. QinEqualityPair /. QextractingANDEvents /. QequalityPair; 
testVars = Map[QgetFirstElement, varValList];
testVals = Map[QgetSecondElement, varValList];
qDist[baseStepVars,pastStepValues, testVars, testVals, qGen[baseStepVars, pastStepValues]]
);

(* Given a myCondProb statement exclusively with AND intersections, returns the myCondProb statement in terms of o parameters *)
(* NEEDED: A check to see if top/bottom parameter combination is invalid *)
Clear[QconditionedProbability];
QconditionedProbability = (myCondProb[a_ \[Conditioned] b_]) :> (
qBottom = myProb[b] /. QunconditionedProbability;
all = Join[BooleanVariables[a], BooleanVariables[b]];
qTop = myProb[all] /. QunconditionedProbability;
qTop / qBottom
);


(* DISTINCT FROM O GENERATOR *)
(* 
Input: List of variables, list of list of values, list of q parameters ;
Output: List of equations such that q parameter equals a myProb statement ;
*)
Clear[qMyProbs];
qMyProbs[vars0_, vals0_, qOutput_] := 
Module[{vars = vars0, vals = vals0, qL,valTuples,tupleLength, currTuple,insideFunc, currBoolean, tempProb, addedEquations},
qL = Length[qOutput];
valTuples = Tuples[vals];
(* Getting length of first tuple *)
tupleLength  = Length[valTuples[[1]]];
addedEquations = {};
(*
- Go through all tuples ;
- Create a myProb statement for each one, set it equal to the respective q parameter ;
- Remove the last one: using it will mess up the solver;
*)

Do[
currTuple = valTuples[[i]];
currBoolean = vars[[1]] == currTuple[[1]];

If [tupleLength > 1,
Do[
insideFunc[p_] := vars[[j]] == p;
currBoolean = currBoolean && insideFunc[currTuple[[j]]];
,{j, 2, Length[currTuple]}];
];
tempProb = myProb[currBoolean] == qOutput[[i]];
AppendTo[addedEquations, tempProb];
,{i, Length[valTuples]}];
If[debugP == 1,
Print[addedEquations];
];
addedEquations = Drop[addedEquations, -1];
addedEquations
]


(*
*
*
*
*
*
*
*)
(* END OF Q GENERATOR FUNCTIONS *)
(* BEGINNING OF TIME VARIANT GENERATOR FUNCTIONS *)
(*
*
*
*
*
*
*
*)


Clear[variantGenerate];

(* Function for time invariant generation *)
variantGenerate[]:=Module[{equalityAdjust,inequalityAdjust,  myProbAdjustEquals,myProbAdjustUnequals, 

qParams,qOutput,qEquations, qConstraints, qSolve, qRules, qFinalRules, qFinalArray, qD, qAppend, 

oParams, oOutput, oEquations, oConstraints, oSolve, oRules, oFinalRules,oFinalArray, oD, 

underCheck, baseCase,nextCase,currStep, condF, subArray, allCoords, cD,qNew,qCondition,tCondition,nextCondition, observations = {}},


If[debugP == 1,
Print["Adjust Lists"];
Print[classes];
Print[variables];
Print[values];
Print["specs"];
Print[specs];
Print["basecase"];
Print[basespecs];
Print[];
Print["Other specifications for Generation:"];
Print[isPastStep];
Print[isCurrStep];
Print[willBePastStep];
Print[pastStepVars];
Print[pastStepValues];
Print[];
];

testAdjust = (Equal[test1_, test2_]) :> test1;

(* Creating rules to switch around equal/unequal signs in the specifications *)
equalityAdjust = (Equal[test1_, test2_]) :> switchEquals[Equal[test1, test2],variables, values];
inequalityAdjust = (Unequal[test1_, test2_]) :> switchUnequals[Unequal[test1, test2],variables, values];
myProbAdjustEquals= myProb[test3_] :> myProb[ test3 //. equalityAdjust];
myProbAdjustUnequals= myProb[test3_] :> myProb[ test3 //. inequalityAdjust];

(* Modify the specifications to have equal signs in the right places/turn conditional distributions into marginal ones/put everything in proper forms *)

(* TODO: Add relevant independence specifications to basespecs *)
basespecs  = basespecs //. myProbAdjustEquals  //.definizeRule //. eventsToDNFExtRule //. myProbAdjustUnequals //.definizeRule //. eventsToDNFExtRule  //. myProbAdjustEquals //. myProbAdjustUnequals;

If[debugP == 1,
Print["After for Base"];
Print[basespecs];
];

(* baseStepVars: pastStepVars where the t equation is replaced with base case value *)
baseStepVars = pastStepVars /. t -> Max[timesteps];

(* 
Q GENERATION ; 
*) 

If[debugP == 1,
Print["Generating base case"];
];
qParams = qGen[pastStepVars, pastStepValues];
qOutput = qOutcomes[qParams];
qEquations= basespecs /. QunconditionedProbability /. QconditionedProbability;
AppendTo[qEquations, givenEquation[qParams]];
If[debugP == 1,
Print[qEquations];
];

qConstraints=#\[Element]Interval[{0,1}]&/@qOutput;
qSolve = TimeConstrained[Solve[Join[qEquations,qConstraints],qOutput,Reals],60,$Failed];
If[FailureQ[qSolve],Print["Invalid system: solver timed out"]];

(* Solving q base case *)
If[!FailureQ[qSolve],
If[Length[qSolve] != 1 || Length[qSolve[[1]]] != Length[qOutput], 
underCheck = TimeConstrained[FindInstance[Join[qEquations,qConstraints],qOutput,Reals, 1],60,$Failed];
Which[FailureQ[underCheck],Print["Invalid system: distribution either underspecified or conflicting"]
,
Length[underCheck] == 1,Print["Underspecified system: needs more valid specifications"]
,
True,Print["Invalid system: some specifications not consistent"]
];
$Failed,

(* THIS IS DISTRIBUTION OF Q BASE CASE;
UNSURE IF NEEDED, BUT HERE IT IS *)
(* List of rules mapping q parameters to probabilities *)
qRules = qSolve[[1]];
(* Editing the rules such that they map to probabilities and not a list w/single probability *)
qFinalRules = qRules//.{x_}:>x;
If[debugP == 1,
Print[];
Print[qFinalRules];
];
(* A distribution mapping the values and their probabilities *)
qD = CategoricalDistribution[pastStepValues, qParams /. qFinalRules];

Information[qD, "ProbabilityTable"];
RandomVariate[qD, numsamples]


],
Print["Invalid system: solver timed out"]
];

(*Print["Adding q equalities to specifications"];*)
qAppend = qMyProbs[pastStepVars, pastStepValues, qOutput];
AppendTo[specs,qAppend]; 
specs = Flatten[specs]; 
If[debugP == 1,
Print[specs];
];

specs  = specs //. myProbAdjustEquals  //.definizeRule //. eventsToDNFExtRule //. myProbAdjustUnequals //.definizeRule //. eventsToDNFExtRule  //. myProbAdjustEquals //. myProbAdjustUnequals;

If[debugP == 1,
Print[];
Print["After for Main"];
Print[specs];
];


(* Parameterizing the specifications with generated o parameters *)
oParams = oGen[ variables, values];
oOutput = oOutcomes[oParams];
oEquations= specs /. unconditionedProbability /. conditionedProbability;
(* Print[oEquations]; *)
AppendTo[oEquations, givenEquation[oParams]];

(* Print[oEquations]; *)


(* Solving the probability system based on the specifications and constraints *)
oConstraints=#\[Element]Interval[{0,1}]&/@oOutput;
oSolve = TimeConstrained[Solve[Join[oEquations,oConstraints],oOutput,Reals],120,$Failed];
If[FailureQ[oSolve],
Print["Invalid system: solver timed out"], 
If[debugP == 1,
Print[oSolve];
];
];

(* 
At this point, we have the following: ;
	- qFinalRules: A mapping of q parameters to the base case values ;
	- qParams: An m dimensional array of q parameters ;
	- oParams: An n dimensional array of o parameters ;

After getting oFinalRules, what needs to happen now is the following pattern: ;
	- Replace the q parameters in oFinalRules with the observed q values ;
	- Get the conditional distribution of the o parameters ;
	- 

 *)

(* Generating data by placing the solution into a categorical distribution *)
If[!FailureQ[oSolve],
If[Length[oSolve] != 1 || Length[oSolve[[1]]] != Length[oOutput], 
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

(* A distribution mapping the values and their probabilities *)
oFinalArray = oParams /. oFinalRules;
(*Print["Final Array"];
Print[oFinalArray];*)

(* Base case o distribution *)
subArray =oFinalArray /. qFinalRules;
If[debugP == 1,
Print["First sub array"];
Print[subArray];
];

allCoords = Tuples[Range[1,#]&/@Dimensions@oFinalArray];

tCondition = (t :>t+1);
qCondition = (Equal[q_, num_]) :>q :> num;
qNew = qFinalRules;
Do[
If[i == 1,
(* Base case *)
oD = CategoricalDistribution[values, subArray];
nextCase = RandomVariate[oD];
currStep = timeValPresentExtraction[nextCase, isCurrStep];
AppendTo[observations, currStep];,

(* timeValExtraction determines the values for next step's blanket *)

(* qNew is a list of q equalities that will hold for the next distribution *)
qNew = qAppend /. tCondition /. unconditionedProbability/. (oFinalRules /. qNew);
Do[
qNew[[i]] = switchEqualsQ[qNew[[i]]];
,{i, Length[qNew]}];

(* Turns qNew into a list of rules *)
qNew = qNew /. qCondition;

subArray = oFinalArray /. qNew;
If[debugP == 1,
Print["Next sub array"];
Print[subArray];
];
nextCondition = timeValExtraction[nextCase, willBePastStep];

cD = singleConditionalDist[subArray, isPastStep, pastStepVars, pastStepValues, nextCondition, allCoords];
cD = CategoricalDistribution[values, cD];
nextCase = RandomVariate[cD];
currStep = timeValPresentExtraction[nextCase, isCurrStep];
AppendTo[observations, currStep];

];
,{i, numsamples}];


observations

],
Print["Invalid system: solver timed out"]
]


]


(*
*
*
*
*
*
*
*)
(* FINAL GENERATING FUNCTION *)
(*
*
*
*
*
*
*
*)


(* Generate data function *)
generateData::invalidCase="No such case exists";
Clear[generateData];
generateData[filename_, debug1_] := Module[ {result = {}},
debugP = debug1;
parse[filename];
If[debugP == 1,
Print["casetype here:"];
Print[casetype];
];
Switch[casetype,
"static", result = staticGenerate[]; result,
"timeinvariant", result = invariantGenerate[]; result,
"timevariant", result = variantGenerate[]; result,
_, Message[generateData::invalidCase]; $Failed
]
]


End[];


EndPackage[]; 
