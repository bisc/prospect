# Prospect tool for probability specification and data generation
Prospect generates time-series data from probabilistic constraints and independence specifications.

## Specification File Format

A valid Prospect file is a txt of the following form, containing three sections separated by blank lines:

casetype: <one of "static", "timeinvariant", or "timevariant">  
variables: <list of variable name here>  
values: <list of value lists for each variable>  
timesteps: <list of time steps for the Markov blanket shape for each variable> // Not in static case  
numsamples: <natural number of samples to generate>  

independence  
indep[{X, Y, Z...}] // an unconditional independence statement, where X, Y, Z, etc. are all variables previously specified  
condIndep[{X, Y, Z...} | {A, B, C...}] // a conditional independence statement, where X, Y, Z, A, B, C, etc. are all variables previously specified

base // only in time-variant case

< probability specifications line-by-line: algebraic equations with left- and right-hand sides over terms P[event] or P[event | event]  
- These equations can only contain variables with absolute time steps
- Events are predicates over variables taking values. NOT, OR, AND boolean operations supported. Variable equality is also supported (ex. P[X = Y]) if X and Y have the same value set>


main

< probability specifications line-by-line: algebraic equations with left- and right-hand sides over terms P[event] or P[event | event]  
- These equations can only contain variables with relative times and no earlier than those at t-max(timesteps for that variable)
- NOT, OR, AND boolean operations supported. Variable equality is also supported (ex. P[X = Y]) if X and Y have the same value set.>


## Running Prospect
1) Open the Request_input.nb notebook, and run it
2) When prompted to enter file name for input, select a valid input file from the same directory as the Request_input.nb notebook. In the comments of the file exist several examples that can be copied/pasted to observe the functionality.

TBA script instructions
