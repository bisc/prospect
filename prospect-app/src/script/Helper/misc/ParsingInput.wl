(* ::Package:: *)

BeginPackage["ParsingInput`"]


parse::usage="Parsing input files"
variables::usage= "Global variable for variable classes"
values::usage="Global variable for variable values"
numsamples::usage="Global variable for number of samples"
specs::usage="Global variable for system specifications"


(*Import["Independence.wl"];*)
Needs["Independence`"];


Begin["`Private`"];


Clear[parse];
(* Parse text input given file name *)
parse[s_]:=Module[{name=s,input,i,j,strassoc},
Clear[variables,seed, values,timesteps,classes, indepspecs, 
specs, basespecs, casetype, retInput, isPastStep, isCurrStep, 
willBePastStep, pastStepVars, pastStepValues];
timesteps = Null;
input=Import[name, "Lines"];
Print[input];
retInput = input;
(* Process the configs (not specs) *)
i = 1;
While[StringMatchQ[ToString[input[[i]]],__~~": "~~__],
ToExpression[StringReplace[ToString[input[[i]]],": " ->"="]]; (* sets each config attribute to a value *)
Print[ToString[input[[i]]]];
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
Print["Timesteps don't exist"];,
Print["Timesteps exist"];
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
Print["functioning"];
i = Position[input, "main"][[1]][[1]] + 1;
While[i<=Length[input],
Module[{expr=ToExpression[StringReplace[input[[i]]," ="->" =="]]},
AppendTo[specs,expr/.(P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]]))];
(*Print[FullForm@expr];
expr = (P[A] /. (P[e_]:>(If[StringContainsQ[ToString[e]," | "],myCondProb[Conditioned@@e],myProb[e]])));
Print[FullForm@expr];
AppendTo[specs, expr];*)
i++
];
];
];

specs = Join[specs, indepspecs];

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

Print[specs]
]


End[];


EndPackage[]; 
