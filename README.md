# Misclassification

This is a code that is used to simulate and understand the misclassification of Citizen science observations. 
The model assumes a Poisson Point process for the observations, and then the reported species are verified by a Machine Learning algorithm. 

Using the verified and unverified data, we seek to estimate the classification probabilities.

There are 2 folders in this repository. 

a. MAIN:

It contains three R-scripts. 
i. sim_alternative.R - This function simulates the data for the problem at hand. It returns a list of the unverified data, verified data, 
classification probabilities and the covariates.

ii. nimble_alternative.R - This sript is the code for the problem written in NIMBLE.

iii. estimation_alternative.R - This script is for the estimation of parameters. This is the initial work, so only one set of parameter values are used.

b. TEST:
This contains the unit test for the simulations of the data (sim_alternative.R). Note that the scripts in the MAIN also contain self checks for the simulation.

A document trying to state the intuition behind the problem is attached in the repository.
