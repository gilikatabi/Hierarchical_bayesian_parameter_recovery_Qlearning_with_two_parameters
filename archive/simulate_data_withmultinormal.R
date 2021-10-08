#aim: Generated simulated data for Narmed bandit task using hierarchical group parameters and individual parameters
#author: Nitzan Shahar, 2021

rm(list=ls())

Nsubjects =100        #number of agents
Ntrials   =200
Narms     =2
rndwlk    =read.csv('data/rndwlk_4frc_1000trials.csv',header=F)[,1:Ntrials]
Nraffle   =2

# generate population and subject level parameters -----------------------------------------------------------

#population location parameters
mu=c(
  alpha        =psych::logit(0.4),
  beta         =log(3)
)
Nparam=length(mu)

#population scale parameters
tau          =c(2,.25) #var vector
cov_param    =0
sigma        = diag(tau)
sigma[!diag(nrow=Nparam)]=cov_param

# sample aux parameters
auxiliary_parameters = MASS::mvrnorm(n = Nsubjects, mu = mu, Sigma = sigma)

#plot true parameters
true.parameters=cbind(
  subject=seq(1,Nsubjects),
  alpha  =psych::logistic(auxiliary_parameters[,1]),#use "runif(Nsubjects)" in case you want to test uniform alpha
  beta   =5#exp(auxiliary_parameters[,2])
)
psych::multi.hist(true.parameters,density=F,nrow=1)


# generate data -----------------------------------------------------------

cfg = list(Nblocks =1,
           Ntrials=Ntrials,
           Narms  =Narms,    
           Nraffle=Nraffle,  #(i.e., offer Nraffle arms each trial from a deck of Narms)
           rndwlk =read.csv('data/rndwlk_4frc_1000trials.csv',header=F)[,1:Ntrials])

source('models/simulation_Narmed_bandit_task.R')

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
}


#check simulated data -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(effects)

#plot mean reward vs expected value
model<-glmer(reward ~ expval_ch+(1| subject), 
             data = df, family = binomial)
plot(effect('expval_ch',model))


#plot mean reward vs parameters
model<-glmer(reward ~ poly(alpha,2)+(1| subject), 
             data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('alpha',model)) #(if you use uniform alpha - you will be able to see a nice hyperbolic)


#pStay model-agnostic analysis
df=df%>%mutate(stay=(choice==lag(choice,default=0))*1,
               reward_oneback=lag(reward,default=0))

model<-glmer(stay ~ reward_oneback+(reward_oneback| subject), 
             data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))

df_plot=cbind(coef(model)$subject,as.data.frame(true.parameters))

gridExtra::grid.arrange(
  ggplot(df_plot,aes(x=alpha,y=reward_oneback ))+geom_point(size=3),
  ggplot(df_plot,aes(x=beta,y=reward_oneback ))+geom_point(size=3),
  ggplot(df_plot,aes(x=alpha,y=beta,color=reward_oneback ))+geom_point(size=3),
  ncol=3
)

#save-------------------------------------------------------------------
save(df,file='results/simulation_10subjects_model_null.Rdata')
