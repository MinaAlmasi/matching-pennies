# The Matching Pennies Game (Portfolio 1, ACM)
This repository contains the code for the `portfolio 1 assignment` in the course `Advanced Cognitive Modeling` at the Cognitive Science MSc. (F24).

Code was produced jointly by the group members:
* Milena Cholozynska (@milenacholo)
* Daniel Blumenkranz (@daniblu)
* Anton Drasbæk Schiønning (@drasbaek)
* Mina Almasi (@MinaAlmasi)

## Overview 
The repository contains two folders: 
1. `src` folder with all R scripts (see table below for more details)
2. `out` folder with data and plots

Scripts in the `src` folder:
| Script        | Description                                   |
|---------------|-----------------------------------------------|
| `agents.R`  | Contains functions for implementing agents with strategies `WIN-SHIFT-LOSE-STAY` and `REINFORCEMENT LEARNING`.     |
| `game.R`  | Contains functions for making agents play against each other in various combinations.         |
| `simulate.R`   | Run to simulate three combinations of games with 50 agents and a 100 trials for each game (`WSLS-WSLS`, `RL-RL`, `WSLS-RL`). Data is saved to the `out` folder.              |
| `plot.R`  | Run to visualise the results from `simulate.R` in various plots. Plots are saved to the `out` folder       |

## Usage 
### Setup
To use the code, ensure that you have `R` and `RScript` installed. All code was developed using `R` version `4.3.2` and was primarily tested on a MacBook.

Furthermore, please install the package `pacman`. Within your `R console`, this can be done as such: 
```
install.packages("pacman")
```
### Run Code 
Code can be run by using `RScript` in your `bash` terminal
```bash
RScript src/simulate.R
```
Note that only `simulate.R` and `plot.R` can be run.

