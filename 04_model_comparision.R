#Aim: Compare true with null model
#Contributor: Nitzan Shahar, 2021

library(bridgesampling)

#true model
rl_fit=readRDS("data/stanfit_simulation_20subjects_200trials_4arms_6chains_4000iter.rds")

emptymodel<- stan(file = "models/model_Narmed_bandit_alpha_beta_Phi_approx.stan", 
                  data=data_for_stan, 
                  iter=1,
                  chains=1) 
rl_alpha_beta = bridge_sampler(rl_fit, stanfit_model=emptymodel,silent = F,cores =4)

#null model
rl_fit=readRDS("data/stanfit_simulation_20subjects_200trials_4arms_6chains_4000iter_noalpha.rds")

emptymodel<- stan(file = "models/model_Narmed_bandit_alpha_beta_Phi_approx.stan", 
                  data=data_for_stan, 
                  iter=1,
                  chains=1) 
rl_noalpha = bridge_sampler(rl_fit, stanfit_model=emptymodel,silent = F,cores =4)


#compare
bf(rl_alpha_beta,rl_noalpha)
