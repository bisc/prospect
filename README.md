# PROSPECT tool for probability specification and data generation
PROSPECT (a PRObability SPECification Tool) generates time-series data from probabilistic constraints and independence specifications.

## Using PROSPECT

PROSPECT is implemented in two forms: as a script and as a computational notebook -- both based on the Wolfram platform. 

### Script implementation

The script-based implementation of PROSPECT is the easiest to use, featuring a simple command-line interface. 

Requirements: [Wolfram Script](https://www.wolfram.com/wolframscript/) version 1.5+. 

#### Installing Wolfram Script

To run the script version of PROSPECT, you need to dowload and install a free version of the Wolfram Engine: 

1) Download and install the [Wolfram Engine](https://www.wolfram.com/engine/) for your OS. 
2) Create a Wolfram ID account (if you do not have one). 
3) Accept the terms of the [free license](https://www.wolfram.com/engine/free-license)
4) Run "wolframscript" in your command line
5) Input your Wolfram ID and password into the prompt

#### Running the PROSPECT script

The PROSPECT script PROSPECT.wls can be run in the command line: It takes in the following arguments: ```
./PROSPECT.wls <filename> <debug-mode> ```

The arguments are as follows: 
1) ```<filename>``` is a relative path for a valid specification file. 
2) (Optional) ``` <debug-mode> ``` is a flag for the debug mode: 1 or 0. Typing 1 indicates that all debug messages should be printed during the run, 0 indicates only the result should be printed. 0 by default.

The output will be the sampled data in a csv format. For example, if the command line is in the script directory, the following command can be run:
```
./PROSPECT.wls testing/static/Static.txt
```
This will print the data to be generated per the specifications in the Static.txt text file. This is equivalent to typing
```
./PROSPECT.wls testing/static/Static.txt 0
```
Furthermore, by typing the following command instead:
```
./PROSPECT.wls testing/static/Static.txt > output.csv
```
The output will be saved to an output.csv file created in the same directory.

### Notebook implementation
The notebook-based implementation of PROSPECT is most suitable development and debugging. 

Requirements: [Wolfram Mathematica](https://www.wolfram.com/mathematica/), version 12.1+. You can obtain a [free trial](https://www.wolfram.com/mathematica/trial/) or you may be at a university with an [academic license](https://www.wolfram.com/mathematica/pricing/colleges-universities/).

1) Open the src/notebook/PROSPECT.nb notebook, and run all it (Ctrl+A, Shift+Enter).
2) When prompted to enter a file name for input, enter either the name of a valid specification file's from the same directory as the PROSPECT.nb notebook, or an absolute path to the file. (In the comments of the file exist several examples that can be copied/pasted to observe the functionality.)
3) Mathematica will print the sampled data at the bottom of the notebook and generate output.csv in the same folder.

The experiment specs can be found in experiments/specs, and the data is in experiments/generated_data.

## Specification File Format

A valid PROSPECT file is a txt of the following form, containing three sections separated by blank lines:

```casetype: <one of "static", "timeinvariant", or "timevariant">  
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
```

## Running Baseline Pyro Scripts
Included in this repository are a collection of scripts written in Pyro v1.5.1 (based on Python v3.8.5). They can be found in experiments -> baseline. The "accurate" baseline scripts imperatively generate data from several example specifications found in experiments -> specs. The "naive" scripts highlight potential pitfalls of imperatively coding the specifications in this way. You can run the scripts using the follow steps.
1) Install Python and Pyro using the instructions here: https://pyro.ai/.
2) In your terminal, navigate to the directory containing the script(s).
3) For the static and time-invariant scripts, type "python <filename.py> <# of time steps desired>". For the time-variant scripts, type "python <filename.py> <# of time steps desired> <# of time series desired>".
4) After running a script, one or more .csv files with the generated data will appear in your directory. In the time-variant case, each file represents data for a variable, each table row is a time series, and each column is a time step.
