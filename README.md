# Qlearning_hierarchical_fitting_with_stan
Parameter recovery examples for RL models using stan.

## 01_simulate_data.R 
We start by simulating parameters and data. This file has four sections.
#### Part A: Simulate 'true' population and individual level parameters.
We start by fixing population level parameters. 
Learning rate (α) population parameters are location and scale for a normal distrbution (unbounded). We then sample individual learning rates from the unbonded normal distrbution  and  then transformed them to be between 0 and 1 using a logit function.
Noise parameter (β) population parameters are location and scale for a lognormal distrbution, from which we then sample individual parameters.

#### Part B - Simulate the data according to a Qlearning alogrithm 
For each block, and each trial we do the following:
1. Offer the subject arms for selection (e.g., you might have the task defined with 4 arms in total, 2 offered randomly each trial)
2. Make selection according to softmax
3. Observe outcome
4. Update Qvalues

#### Part C - Convert data frame to a stan compatible format
This has two subparts. 
1. Add missing data using padding. This means that stan will ignore missing data.
2. Convert the data frame to a list with matriceis for each variable

#### Part D - Sanity checks
Here we run on the data frame some sequnitial trial regression analysis as a sanity check
1. see that higher excpected value for the chosen bandit means higher reward
2. see that agents tend to stay with their previous bandit selection after rewarded vs. unrewarded trials

## 02_model_fit_stan
Here we run mcmc using stan to recover both the population and individual level parameters.
The stam model is located in the 'models'folders.

## 03_compare_parameters 
Here we plot the recovered and true parameters. Below is the results of a simulation with 200 agents 1000 trials each (4 arms, 2 offers each trial).

Posterior distrbutions for population level parameters (dashed lines are the true value):
![Posterior](https://github.com/shahar-lab/Qlearning_hierarchical_fitting_with_stan/blob/main/graphics/population_level_200agents_1000trials.jpeg)

Individual level correlations between true and recovered parameters (pearson r is also indicated in the upper left corner):

![This is an image](https://github.com/shahar-lab/Qlearning_hierarchical_fitting_with_stan/blob/main/graphics/individual_level_200agents_100trials.jpeg)

