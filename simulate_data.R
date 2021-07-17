#aim: Created simulated parameters and data for null model, model 1_onePE, model2_twoPE
#author: Nitzan Shahar

rm(list=ls())

Nsubjects =10        #number of agents

# generate population and subject level parameters -----------------------------------------------------------

#population location parameters
mu=c(
  alpha        =psych::logit(0.4),
  beta         =log(3)
)
Nparam=length(mu)

#population scale parameters
tau          =c(.25,.05) #var vector
cov_param    =0
sigma        = diag(tau)
sigma[!diag(nrow=Nparam)]=cov_param

# sample and plotaux parameters
auxiliary_parameters = MASS::mvrnorm(n = Nsubjects, mu = mu, Sigma = sigma)
true.parameters=cbind(
  alpha=psych::logistic(auxiliary_parameters[,1]),
  beta =exp(auxiliary_parameters[,2])
)
psych::multi.hist(true.parameters,density=F,nrow=1)


# generate data -----------------------------------------------------------

cfg = list(Nblocks =4,
           Ntrials=50,
           Narms  =4,    
           Nraffle=2,  #(i.e., offer Nraffle arms each trial from a deck of Narms)
           rndwlk =read.csv('models/rndwlk1.csv',header=F)[,1:50])
source('models/simulation_model_null.R')

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
}
save(df,file='results/simulation_10subjects_model_null.Rdata')
