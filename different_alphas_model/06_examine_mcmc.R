rm(list=ls())
model_name=c('different_alphas')
folder_name=c('')

rl_fit=readRDS(paste('./data/',model_name,'_RDSfile.rds',sep=""))
library(bayesplot)
library(ggplot2)
library(rstan)

mypars = c('population_locations[1]',
           'population_locations[2]',
           'population_locations[3]',
           'population_locations[4]',
           'population_locations[5]',
           'population_locations[6]',
           'population_locations[7]',
           'population_scales[1]',
           'population_scales[2]',
           'population_scales[3]',
           'population_scales[4]',
           'population_scales[5]',
           'population_scales[6]',
           'population_scales[7]')

#Trace plots
mcmc_trace(rl_fit,pars=mypars)
mcmc_pairs(rl_fit,pars=mypars)
