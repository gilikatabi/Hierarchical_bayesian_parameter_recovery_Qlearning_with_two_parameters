#aim: Hierarchical fit Stan 


rm(list=ls())
library(rstan) 
detectCores()

# fit stan model--------------------------------------------------
model_name = 'different_alphas'

#load data
load('./data/standata.Rdata')
load('./data/my_compiledmodel.rdata')

{
  start_time <- Sys.time()

    rl_fit<- sampling(my_compiledmodel, 
                data=data_for_stan, 
                iter=2000,
                warmup = 1000,
                chains=4,
                cores =4) 

  end_time <- Sys.time()
  end_time-start_time
}

#save

saveRDS(rl_fit, paste('./data/',model_name,'_RDSfile.rds',sep=""))

pars <- rstan::extract(rl_fit, permuted = TRUE)

save(pars, file=paste('./data/',model_name,'_recovered_parameters.rdata',sep=""))


