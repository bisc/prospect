# Time Series Data Generation
Time series data generation from probabilistic and independence specifications.

Currently, only static case time series is operational: to perform, do the following.
1) Open the Main_Static_Generator.nb notebook, and run it
2) When prompted to enter file name for input, select a valid input file from the same directory as the Main_Static_Generator.nb and Parsing_input.nb notebooks.

A valid input file is a txt of the following form:

casetype: "static"
variables: (LIST OF VARIABLES HERE)
values: (LIST OF LIST OF VARIABLES)
numsamples: (NUMBER OF SAMPLES TO GENERATE)

(INDEPENDENCE SPECIFICATIONS LINE BY LINE: Of the form indep[X, Y] where X and Y are variables previously specified)

(PROBABILITY SPECIFICATIONS LINE BY LINE: Equations with left and right hand sides that utilize inputs of the form P[events] or P[events | events])
