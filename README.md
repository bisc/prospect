# Time Series Data Generation
Time series data generation from probabilistic and independence specifications.

## Input File

A valid input file is a txt of the following form, containing three sections separated by blank lines:

casetype: (One of "static", "timeinvariant", or "timevariant")  
variables: (LIST OF VARIABLES HERE)  
values: (LIST OF LIST OF VARIABLES) 
timesteps: (LIST OF TIME STEPS FOR EACH VARIABLE) // Not in static case
numsamples: (NUMBER OF SAMPLES TO GENERATE)  

*blank line* 

independence

(INDEPENDENCE SPECIFICATIONS LINE BY LINE: Of the form indep[{X, Y, Z...}] where X, Y, Z, etc. are variables previously specified)

(CONDITIONAL INDEPENDENCE SPECIFICATIONS LINE BY LINE: Of the form condIndep[{X, Y, Z...} | {A, B, C...}] where X, Y, Z, A, B, C, etc. are all variables previously specified)

*blank line*

base // Only in timevariant case

(PROBABILITY SPECIFICATIONS LINE BY LINE: Equations with left and right hand sides that utilize inputs of the form P[event] or P[event | event])
- These equations can only contain variables from time steps less than t
- NOT, OR, AND boolean operations supported. Variable equality is also supported (ex. P[X = Y]) if X and Y have the same values

*blank line*

main

(PROBABILITY SPECIFICATIONS LINE BY LINE: Equations with left and right hand sides that utilize inputs of the form P[event] or P[event | event])
- NOT, OR, AND boolean operations supported. Variable equality is also supported (ex. P[X = Y]) if X and Y have the same values.


## Running Program
1) Open the Request_input.nb notebook, and run it
2) When prompted to enter file name for input, select a valid input file from the same directory as the Request_input.nb notebook. In the comments of the file exist several examples that can be copied/pasted to observe the functionality.
