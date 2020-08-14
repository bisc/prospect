# Time Series Data Generation
Time series data generation from probabilistic and independence specifications.

Currently, only static case time series is operational: to perform, do the following.
1) Open the Main_Static_Generator.nb notebook, and run it
2) When prompted to enter file name for input, select a valid input file from the same directory as the Main_Static_Generator.nb and Parsing_input.nb notebooks.

A valid input file is a txt of the following form, containing three sections separated by blank lines:

casetype: "static"  
variables: (LIST OF VARIABLES HERE)  
values: (LIST OF LIST OF VARIABLES)  
numsamples: (NUMBER OF SAMPLES TO GENERATE)  
*blank line*  
(INDEPENDENCE SPECIFICATIONS LINE BY LINE: Of the form indep[{X, Y, Z...}] where X, Y, Z, etc. are variables previously specified)  
(CONDITIONAL INDEPENDENCE SPECIFICATIONS LINE BY LINE: Of the form condIndep[{X, Y, Z...} | event] where a valid event is boolean statement about previously specified variables, e.g. X = x && Y = y)\*  
*blank line*  
(PROBABILITY SPECIFICATIONS LINE BY LINE: Equations with left and right hand sides that utilize inputs of the form P[event] or P[event | event])\*

\*At this time, events should not contain any logical OR operators (e.g. ||, ∨).
