rm(list=ls())
model_name=c('null')
rl_fit=readRDS(paste('./data/',model_name,'_RDSfile.rds',sep=""))
library(bayesplot)

#Trace plots
mcmc_trace(rl_fit,  pars = c('population_locations[1]',
                             'population_locations[2]',
                             'population_scales[1]',
                             'population_scales[2]'))
