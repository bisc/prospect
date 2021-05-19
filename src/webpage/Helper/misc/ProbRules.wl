(* ::Package:: *)

BeginPackage["ProbRules`"];


definizeRule::usage="pr1"
eventsToDNFExtRule::usage="pr2"


(* Global requirements *)
definizeRule = (myCondProb[a_ \[Conditioned] b_] :> myProb[a \[And] b] / myProb[b]);
eventsToDNFExtRule  = (myProb[a_]) /;( \[Not] (BooleanConvert[a, "DNF"] === a)):>(myProb[BooleanConvert[a, "DNF"]]);


Begin["`Private`"];


SetAttributes[myIndep, Orderless];
SetAttributes[myCondIndep, Orderless];

Clear[myProb, myCondProb, myIndep, myCondIndep,myVariables, onlyAboutDet] ;

myProb[True] := 1;
myProb[False] := 0;
 
(* automatic vanilla disjunction breakup rule -- disable this for finer-grained control *)
myProb/: myProb[a_ \[Or] b_] := (myProb[a] + myProb[b] - myProb[Simplify[a \[And]b]]);

(* conditional reductions *) 
myCondProb[True \[Conditioned] _] := 1;
myCondProb[False \[Conditioned] _] := 0;
myCondProb[a_ \[Conditioned] True] := myProb[a];
myCondProb[Except[ __\[Conditioned]__, a_]] := myProb[a];
myCondProb[a_ \[Conditioned] False] := Throw["Cannot condition on zero-probability events"];

(* automatic conditional disjunction breakup rule -- disable this for finer-grained control *)
(* myCondProb[a_ \[Or] b_ \[Conditioned] c_] := (myCondProb[a\[Conditioned]c] + myCondProb[b\[Conditioned]c] - myCondProb[Simplify[a\[And]b]\[Conditioned]c]);*)
(* Sanity checks for notation *) 
myProb /: myProb[a_ \[Conditioned] b_] := Throw["Conditioned not allowed in vanilla probability"]; 
myProb /: myProb[a_, b_] := Throw["Commas not allowed in vanilla probability"]; 
myCondProb /: myCondProb[a_, b_] := Throw["Commas not allowed in conditioned probability"]; 
myIndep /: myIndep[a_ \[Conditioned] b_] := Throw["Conditioned not allowed in vanilla independence"];
myIndep /: myIndep[args___] := Throw["Need at least 2 arguments in vanilla independence"]/; Length[List[args]]<= 1 ;
myCondIndep /: myCondIndep[args___]:= Throw["Need at least 2 arguments in conditional independence"]/; Length[List[args]]<= 1 ;

Clear[disjBreakRule, disjCondBreakRule, allProbRules,  definizeRule,  bayesRule,negateRule, renegateRule,negateCondRule, renegateCondRule, removeIrrelevantGeneralRule, introCondRule,introCondRuleCond,introMargRule, introDetCondRule, introDetMargRule, eventsToDNFExtRule,eventsToDNFCondExtRule,  eventsToCNFExtRule, eventsToCNFCondExtRule,eventReasoningRule1, eventReasoningRule2, eventCondReasoningRule1,eventCondReasoningRule2];
disjBreakRule= (myProb[a_ \[Or] b_] :> (myProb[a] + myProb[b] - myProb[Simplify[a \[And]b]]));

disjCondBreakRule = 
(myCondProb[a_ \[Or] b_ \[Conditioned] c_] :> (myCondProb[a\[Conditioned]c] + myCondProb[b\[Conditioned]c] - myCondProb[Simplify[a\[And]b]\[Conditioned]c]));

(* conditional probability definition *)
definizeRule = (myCondProb[a_ \[Conditioned] b_] :> myProb[a \[And] b] / myProb[b]);

(* the "reverse" of the previous *)
reversizeRule1 = (myProb[a_ \[And] b_] :> myCondProb[a \[Conditioned] b] * myProb[b]); 
reversizeRule2 = (myProb[a_ \[And] b_] :> myCondProb[b \[Conditioned] a] * myProb[a]); 

(* conditional reverses, similar to above *)
reversizeCondRule1 = (myCondProb[a_ \[And] b_ \[Conditioned] c_ ] :> myCondProb[a \[Conditioned] b \[And]c] * myProb[b\[And]c]/myProb[c]); 
reversizeCondRule2 = (myCondProb[a_ \[And] b_ \[Conditioned] c_ ] :> myCondProb[b \[Conditioned] a \[And]c] * myProb[a\[And]c]/myProb[c]); 

(* smart reverses that only push GT to the right *) 
reversizeOnlyH1Rule = (myProb[a_ \[And] b_] /; onlyGTs[b] :> myCondProb[a \[Conditioned] b] * myProb[b]);

reversizeCondOnlyH1Rule = (myCondProb[a_ \[And] b_ \[Conditioned] c_ ] /; onlyGTs[b]:> myCondProb[a \[Conditioned] b \[And]c] * myProb[b\[And]c]/myProb[c]);  

(* smart reversizes that leave no GTs on the left *) 
reversizeAllH1Rule = (myProb[a_ \[And] b_] /; noGTs[a] :> myCondProb[a \[Conditioned] b] * myProb[b]);
reversizeCondAllH1Rule = (myCondProb[a_ \[And] b_ \[Conditioned] c_ ] /; noGTs[a]:> myCondProb[a \[Conditioned] b \[And]c] * myProb[b\[And]c]/myProb[c]);  

(* merger of the above *) 
reversizeAllOnlyH1Rule = (myProb[a_ \[And] b_] /; noGTs[a] \[And] onlyGTs[b] :> myCondProb[a \[Conditioned] b] * myProb[b]);

reversizeCondAllOnlyH1Rule = (myCondProb[a_ \[And] b_ \[Conditioned] c_ ] /; noGTs[a]\[And] onlyGTs[b]:> myCondProb[a \[Conditioned] b \[And]c] * myProb[b\[And]c]/myProb[c]);  

(* ordered reversize for pairs: only reverse if the condition is earlier in the event list  *) 
reversizeH1OrderRule[orderList_] :=  (myProb[a_ \[And] b_] /; (MemberQ[orderList, a]\[And]MemberQ[orderList, b]\[And]onlyGTs[b] \[And]  earlierInOrder[orderList, b,a]  ) :> myCondProb[a \[Conditioned] b] * myProb[b]);

 (* ordered reversize for lists: if all indices from one list are less than the other, then fine to reversize; subsumes the previous one *)
reversizeH1OrderListRule[orderList_] :=  (myProb[a_ \[And] b_] /; (SubsetQ[orderList, listifyAnd@a]\[And]SubsetQ[orderList, listifyAnd@b]\[And]onlyGTs[a]\[And] onlyGTs[b] \[And]  earlierInOrderLists[orderList, listifyAnd@b,listifyAnd@a]  ) :> myCondProb[a \[Conditioned] b] * myProb[b]);

(* same but for conditioned instead of vanilla *) 
reversizeCondH1OrderListRule[orderList_]  := (myCondProb[a_ \[And] b_ \[Conditioned] c_ ] /; (SubsetQ[orderList, listifyAnd@a]\[And]SubsetQ[orderList, listifyAnd@b]\[And]onlyGTs[b] \[And]  earlierInOrderLists[orderList, listifyAnd@b,listifyAnd@a]):> myCondProb[a \[Conditioned] b \[And]c] * myProb[b\[And]c]/myProb[c]);  

(* Modification: not requiring "a" to be all part of the order or have all GTs. Should allow reversizing when "a" has CH1s and NCs *) 
reversizeH1RelaxedOrderListRule[orderList_] :=  (myProb[a_ \[And] b_] /; (SubsetQ[orderList, listifyAnd@b]\[And]onlyGTs[b] \[And]  earlierInOrderLists[orderList, listifyAnd@b,listifyAnd@a]  ) :> myCondProb[a \[Conditioned] b] * myProb[b]);

(* same modification but for conditioned instead of vanilla *) 
reversizeCondH1RelaxedOrderListRule[orderList_]  := (myCondProb[a_ \[And] b_ \[Conditioned] c_ ] /; (SubsetQ[orderList, listifyAnd@b]\[And]onlyGTs[b] \[And]  earlierInOrderLists[orderList, listifyAnd@b,listifyAnd@a]):> myCondProb[a \[Conditioned] b \[And]c] * myProb[b\[And]c]/myProb[c]);  

(* an imperative/prescriptive way to reversize, by specifying exactly how to build the lists of GTs/non-GTs: *)
reversizeAllOnlyH1ImperativeRule = (myProb[exp_And] /; Length@listifyAnd@exp > 1 :> With[{h1sAnd =  Select[exp, onlyGTs]}, myCondProb[Select[exp, noGTs] \[Conditioned]h1sAnd] * myProb[h1sAnd]]);

(* same as above, but for conditional *) 
reversizeCondAllOnlyH1ImperativeRule = (myCondProb[exp_And \[Conditioned] c_ ] /; Length@listifyAnd@exp > 1 :> With[{h1sAnd =  Select[exp, onlyGTs]}, myCondProb[Select[exp, noGTs] \[Conditioned](h1sAnd \[And] c)] *myProb[h1sAnd \[And]c] / myProb[c]]);

(* bayes as a rule *)
bayesRule = (myCondProb[a_ \[Conditioned] b_] :>  myCondProb[b \[Conditioned] a] * myProb[a] / myProb[b]);

(* negation-related rules *) 
negateRule = (myProb[ \[Not]a_] :> 1 - myProb[a]); 
renegateRule = (myProb[a_] /; Not[Head[a]===Not] :> 1- myProb[\[Not]a ] );
negateCondRule = (myCondProb[ \[Not]a_ \[Conditioned] b_ ] :> 1 - myCondProb[a\[Conditioned]b]); (* used to be a def *)
renegateCondRule = (myCondProb[a_ \[Conditioned] b_] /; Not[Head[a]===Not]  :> 1- myCondProb[\[Not] a \[Conditioned] b]);


(* putting events in standard forms: DNF and CNF *) 
eventsToDNFExtRule  = (myProb[a_]) /;( \[Not] (BooleanConvert[a, "DNF"] === a)):>(myProb[BooleanConvert[a, "DNF"]]);
eventsToDNFCondExtRule  = (myCondProb[a_\[Conditioned]b_]) /;( \[Not] ( (BooleanConvert[a, "DNF"] === a) \[And] (BooleanConvert[b, "DNF"] === b) )):>(myCondProb[BooleanConvert[a, "DNF"]\[Conditioned]BooleanConvert[b, "DNF"]]);

(* CNF -- generally isn't as useful as DNF due to trapping disjunctions inside larger conjunctions *)
eventsToCNFExtRule  = (myProb[a_]) /;( \[Not] (BooleanConvert[a, "CNF"] === a)):>(myProb[BooleanConvert[a, "CNF"]]);
eventsToCNFCondExtRule  = (myCondProb[a_\[Conditioned]b_]) /;( \[Not] ( (BooleanConvert[a, "CNF"] === a) \[And] (BooleanConvert[b, "CNF"] === b) )):>(myCondProb[BooleanConvert[a, "CNF"]\[Conditioned]BooleanConvert[b, "CNF"]]);

(* EVENT ALGEBRA: to DNF plus reasoning NCs and contradictions away. Uses a global variable detAssns  *) 
eventReasoningRule1 := (myProb[exp_] :> myProb[BooleanMinimize[LogicalExpand[LogicalExpand@exp \[And]detAssns],"DNF", detAssns]] );

eventReasoningRule2 := (myProb[exp_] :> myProb[BooleanMinimize[LogicalExpand[LogicalExpand@exp \[Or]\[Not]detAssns],"DNF", detAssns]] );

eventCondReasoningRule1:= (myCondProb[exp1_\[Conditioned]exp2_] :> myCondProb[BooleanMinimize[LogicalExpand[LogicalExpand@exp1\[And]detAssns],"DNF", detAssns]\[Conditioned]BooleanMinimize[LogicalExpand[LogicalExpand@exp2 \[And]detAssns],"DNF", detAssns]]);

eventCondReasoningRule2:= (myCondProb[exp1_\[Conditioned]exp2_] :> myCondProb[BooleanMinimize[LogicalExpand[LogicalExpand@exp1\[Or]\[Not]detAssns],"DNF", detAssns]\[Conditioned]BooleanMinimize[LogicalExpand[LogicalExpand@exp2 \[Or]\[Not]detAssns],"DNF", detAssns]]);

(* a generalization of vanilla and conditional irrelevance *) 
removeIrrelevantGeneralRule = myCondProb[a_ \[Conditioned] conds__And] :> myCondProb[a \[Conditioned] DeleteCases[conds,  _?((myIndep[a, #]) \[Or](myCondIndep[a, #]\[Conditioned](DeleteCases[conds, #]))&)]];

(* tries conditions from the right part of myCondProb for conditional independence with the left part; if independent, removes them *)
removeIrrelevantGeneralInferenceRule = myCondProb[a_ \[Conditioned] conds__And] :> myCondProb[a \[Conditioned] DeleteCases[conds,  _?(Or@@mySimplify[((myIndep[a, #]) \[Or](myCondIndep[a, #]\[Conditioned](DeleteCases[conds, #]))),Subsets@allIndepRules, indepIndefenceIterLimit]&)]];

(* an augmented version of the previous one *) 
removeIrrelevantGeneralInferenceRuleAug[extraRule_] := myCondProb[a_ \[Conditioned] conds__And] :> myCondProb[a \[Conditioned] DeleteCases[conds,  _?(Or@@mySimplify[((myIndep[a, #]) \[Or](myCondIndep[a, #]\[Conditioned](DeleteCases[conds, #]))),Subsets@Join[{extraRule},allIndepRules], indepIndefenceIterLimit]&)]]


(* need conditional inference with exhaustive search *) 
indepIndefenceIterLimit = 10; 
indepInferenceTotalRule=  (myProb[seq__And]/; (Or@@mySimplify[Apply[myIndep,  seq],{allIndepRules}, indepIndefenceIterLimit] ) :> Apply[Times,myProb/@ Apply[List,seq]]) ;
(* a very weak version of the above*)
indepInferenceTwoRule =  (myProb[a_ \[And] b_]/; (Or@@mySimplify[myIndep[a,b],{allIndepRules},indepIndefenceIterLimit] ) :> myProb[a]myProb[b]);

(* a broadly applicable rule, may cost computation *) 
indepCondSplitTwoRule = (myCondProb[a_ \[And] b_\[Conditioned] seq2_] /;(myCondIndep[a, b]\[Conditioned] (seq2)) :> myCondProb[a\[Conditioned] seq2] myCondProb[b\[Conditioned]seq2] );

(* maybe this is automated already in the definitions *) 
indepCondSplitRule = (myCondProb[a_ \[And] b_And \[Conditioned] seq2_And] /;(myCondIndep[a, b]\[Conditioned] (seq2)) :> myCondProb[a\[Conditioned] seq2] myCondProb[b\[Conditioned]seq2] );

indepCondSplitRuleOr = (myCondProb[a_ \[And] b_Or \[Conditioned] seq2_And] /;(myCondIndep[a, b]\[Conditioned] (seq2)) :> myCondProb[a\[Conditioned] seq2] myCondProb[b\[Conditioned]seq2] );


(* note: this rule has requirements for And to not apply it extraneously; in those cases definitional rules should do the trick *) 
indepInferenceCondSplitRule = (myCondProb[a_ \[And] b_And \[Conditioned] seq2_And] /;(Or@@mySimplify[myCondIndep[a, b]\[Conditioned] (seq2),  Subsets@allIndepRules, indepIndefenceIterLimit]) :> myCondProb[a\[Conditioned] seq2] myCondProb[b\[Conditioned]seq2] );


(* augmenting with extra rules, usually the constructive ones *)
indepInferenceCondSplitRuleAug[extraRule_] := (myCondProb[a_ \[And] b_ \[Conditioned] seq2_] /;(Or@@mySimplify[myCondIndep[a, b]\[Conditioned] (seq2),  Subsets@Join[{extraRule},allIndepRules], indepIndefenceIterLimit]) :> myCondProb[a\[Conditioned] seq2] myCondProb[b\[Conditioned]seq2] );

vanillaProbRules = { negateRule, renegateRule, eventsToDNFExtRule,indepInferenceTwoRule, indepInferenceTotalRule}; 
condProbRules = {removeIrrelevantGeneralRule,definizeRule, reversizeRule1,reversizeRule2, negateCondRule, renegateCondRule, bayesRule, eventsToDNFCondExtRule };
allProbRules = Union[vanillaProbRules, condProbRules];



(* CONSTRUCTIVE PROBABILITY RULES, parameterized and need application limits to avoid infinite loops *)

(* introducing conditional, constructive version of definize and reversize. Only applies until certain depth and if the introduced expression is not part of the existing one *) 
introCondRule[introExp_, leafCount_:30] := (myProb[a_] /; (LeafCount[a] <= leafCount  \[And] FreeQ[a,introExp] ) :>  myProb[introExp \[And] a ] / myCondProb[introExp \[Conditioned] a] ); 

introCondRuleCond[introExp_, leafCount_:30] := (myCondProb[a_ \[Conditioned]b_] /; (LeafCount[a] <= leafCount  \[And] FreeQ[a,introExp] \[And] FreeQ[b,introExp]  ) :>  myCondProb[introExp \[And] a \[Conditioned] b ] / myCondProb[introExp \[Conditioned] (a \[And] b)] ); 


(* introducing a marginal variable (doesn't have an inverse, TODO add one); Only applies until certain depth and if the introduced expression is not part of the existing one *)

introMargRule[introExp_, leafCount_: Infinity] := 
(myProb[a_] /; (LeafCount[a] <= leafCount  \[And] FreeQ[a,introExp] ) :>  
myProb[a \[And] introExp] + myProb[a \[And] \[Not]introExp]); 

(* marginalize in extra events that are missing from the given list. Uses introMargRule. Counts negations as an event being present *) 
introMargAllEventsRule[allEventsList_List] := ( myProb[exps_] /;(*only for Ands or single terms*)( Length@exps < 2 \[Or] Head@exps===And ):>  (RightComposition[Sequence@@((*apply consecutive replacements*) Function[{x},x//.# ] &  /@introMargRule/@(* of what is missing from the list - negations count too *) Complement[allEventsList,Join[listifyAnd@exps],Not/@listifyAnd@exps ])])[ myProb[exps]]);

(* like above, but with condProb *) 
introMargCondRule[introExp_, leafCount_:30] := 
(myCondProb[a_\[Conditioned]b_] /; (LeafCount[a] <= leafCount \[And] FreeQ[a,introExp]\[And]FreeQ[b, introExp] ) :>  
myCondProb[a \[And] introExp\[Conditioned]b] + myCondProb[a \[And] \[Not]introExp\[Conditioned]b]);  

(* introduces marginalization in a conditional for all events in the list. Uses introMargCondRule. Warning: works well with H1s and negations passed as a list *) 
introMargCondAllEventsRule[allEventsList_List] := ( myCondProb[exps_ \[Conditioned] seq_]/;( Length@exps < 2 \[Or] Head@exps===And )\[And]( Length@seq < 2 \[Or] Head@seq===And ):> (RightComposition[Sequence@@(Function[{x},x//.# ] &  /@introMargCondRule/@Complement[allEventsList,Join[ listifyAnd@exps,Not/@listifyAnd@exps,  listifyAnd@seq, Not/@listifyAnd@seq]])])[ myCondProb[exps\[Conditioned]seq]]);

(* smart constructive rules for detectors *) 
introDetCondRule[det_, introExp_, leafCount_:30] := (myProb[a_] /; (LeafCount[a] <= leafCount  \[And] FreeQ[a,introExp]\[And]onlyAboutDet[det,myProb[a]] ) :>  myProb[introExp \[And] a ] / myCondProb[introExp \[Conditioned] a] );

introDetMargRule[det_, introExp_, leafCount_:30] := 
(myProb[a_] /; (onlyAboutDet[det,myProb[a]]\[And](LeafCount[a] <= leafCount)\[And]FreeQ[a,introExp]) :>  
myProb[a \[And] introExp](*myProb[introExp]*) + myProb[a \[And] \[Not]introExp](*myProb[\[Not]introExp]*));  

(*** HELPER FUNCTIONS ***) 
Clear[mySimplify, mySimplifyTrack,mySimplifySymb, subAndPerm];
(* Apply a set of rules from the list to the expression and return only numbers, up to a maximum number of iterations *)
mySimplify::argnotlist = "The rule list argument is not a list of lists, beware";
mySimplify[exp_, ruleList_:Subsets@allProbRules, iters_:250] := (If[\[Not]ListQ@ruleList[[1]], Message[mySimplify::argnotlist ]];
Select[(ReplaceRepeated[exp, # , MaxIterations->iters] & /@ ruleList), NumericQ@# || BooleanQ@# &]);

(* Apply mySimplify and, when aborted, output results so far *) 
mySimplifyTrack[ exp_, ruleList_:allProbRules, iters_:250] :=
(If[\[Not]ListQ@ruleList[[1]], Message[mySimplify::argnotlist ]];Select[Flatten@Last@Reap@CheckAbort[(Sow[ReplaceRepeated[exp, # , MaxIterations->iters] ]& /@ ruleList), "aborted"],NumericQ@# || BooleanQ@# &]);

mySimplifySymb[ exp_, ruleList_:Subsets@allProbRules, iters_:250] :=
(If[\[Not]ListQ@ruleList[[1]], Message[mySimplify::argnotlist]]; (ReplaceRepeated[exp, # , MaxIterations->iters] & /@ ruleList)//DeleteDuplicates);

(* returns non-trivial subsets and permutations of the arg list *) 
subAndPerm[l_] := 
Join[Delete[Delete[Subsets[l], Length[l]^2 -1 ], 1], Permutations[l]];(*  removing the empty set -- and full set to avoid duplicates *)

(* makes all arguments mutually independent, and therefore subset-wise independent *) 
makeIndep[args__] := (Evaluate@(Apply[myIndep, #] &/@(Subsets[List@args] // Delete[List/@Range[1,Length@List@args + 1]]))) = Table[True, Length@Subsets[List@args] - Length@Range[1,Length@List@args + 1] ];

(*** HELPER FUNCTIONS ***) 

(* helper function to get all the variables of a generalized expression *) 
myVariables[expr_] := DeleteDuplicates@Cases[expr,_Symbol,{0,\[Infinity]}];

(* helper function, returns a list of all And arguments, or just a list of one item *) 
listifyAnd[exp_] := If[Head[exp] === And, List@@exp, {exp}];

(* helper function, return true if x is earlier than y in the provided list *) 
earlierInOrder[lst_, x_, y_] :=  FirstPosition[lst, x][[1]] < FirstPosition[lst, y][[1]];

(* all list indices of elements in x are less than those in y. If either is not present in the order list, returns true *) 
earlierInOrderLists[lst_, xlist_, ylist_] :=  And @@( Outer[Less, Flatten@(Position[lst, #, {1}] & /@ xlist), Flatten@(Position[lst, #, {1}] & /@ ylist)] // Flatten ) ;

(* helper function, determines if the expression only has variables from the detector -- no more and no less *) 
onlyAboutDet[d_, exp_] := (DeleteCases[_?(\[Not]FreeQ[#,d["ch1"]]\[Or] \[Not]FreeQ[#, d["h1"]]\[Or] \[Not]FreeQ[#, d["nc"]]&) ]@myVariables@exp == {});

(* helper function, checks if an expression has anything from a detector *) 
mentionsDet[d_, exp_]:= (\[Not]FreeQ[exp,d["ch1"]]\[Or] \[Not]FreeQ[exp, d["h1"]]\[Or] \[Not]FreeQ[exp, d["nc"]]);

(* helper function, returns true if only h1 variables are present in an expression. Uses symbol manipulation *) 
onlyGTs[exp_]:=And@@ ((If[StringLength@SymbolName[#]>=3, StringTake[SymbolName[#], {-2, -1}]== "h1" \[And] StringTake[SymbolName[#], {-3, -1}]!= "ch1", If[StringLength@SymbolName[#]>=2,StringTake[SymbolName[#], {-2, -1}]== "h1", False]] &) /@myVariables[exp]) ;

(* a converse of the above: true when expression has no GTs. Based on symbol names *) 
noGTs[exp_]:=And@@ ((If[StringLength@SymbolName[#]>=3, StringTake[SymbolName[#], {-2, -1}]!= "h1" \[Or] StringTake[SymbolName[#], {-3, -1}]== "ch1", If[StringLength@SymbolName[#]>=2,StringTake[SymbolName[#], {-2, -1}]!= "h1", True]] &) /@myVariables[exp]) ;


End[];


EndPackage[]; 


(*myCondProb[A \[Conditioned] B] //. definizeRule*)
