#aim: Parameter recovery Hierarchical fit Stan 
#contributor: Shira Niv, Nitzan Shahar, 2021


rm(list=ls())
library(rstan) 
library(dplyr)


# parameter recovery with stan --------------------------------------------
load('data/simulation_20subjects_200trials_4arms_standata.Rdata')

#fit stan model   
rl_fit<- stan(file = "models/model_Narmed_bandit_alpha_beta_Phi_approx.stan", 
              data=data_for_stan, 
              iter=4000,
              chains=6,
              cores =6) 

saveRDS(rl_fit, "data/stanfit_20subjects_200trials_4arms_6chains_4000iter.rds")

rl_fit<- stan(file = "models/model_Narmed_bandit_alpha_beta_Phi_approx_noalpha.stan", 
              data=data_for_stan, 
              iter=4000,
              chains=6,
              cores =6) 

saveRDS(rl_fit, "data/stanfit_20subjects_200trials_4arms_6chains_4000iter_noalpha.rds")

