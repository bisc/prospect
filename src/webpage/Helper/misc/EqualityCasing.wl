(* ::Package:: *)

BeginPackage["EqualityCasing`"];


switchEquals::usage="Switch equals signs to valid specs"
switchUnequals::usage="Switch unequals signs to valid specs"
switchEqualsQ::usage="Switch equals signs on q parameters"


Begin["`Private`"];


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


End[];


EndPackage[]; 
