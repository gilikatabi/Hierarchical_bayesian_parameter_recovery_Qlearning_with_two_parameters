#aim: Generated simulated data for Narmed bandit task using hierarchical group parameters and individual parameters
#author: Nitzan Shahar, 2021

rm(list=ls())

Nsubjects =20        #number of agents
Ntrials   =200
Narms     =4
rndwlk    =read.csv('data/rndwlk_4frc_1000trials.csv',header=F)[,1:Ntrials]
Nraffle   =2

# generate population and subject level parameters -----------------------------------------------------------

#population location parameters
  
  #true population level parameters
  mu_alpha   =0.4
  mu_beta    =4
  beta_range =10 #not a free parameter - this is a pre-defined boundery for beta
  
  #transform to aux scale
  mu_aux     =c(qnorm(mu_alpha),qnorm(mu_beta/beta_range))
  
  #transform back to natural scale (just for practice)
  mu_alpha   =pnorm(mu_aux[1], 0,1);
  mu_beta    =pnorm(mu_aux[2], 0,1)*beta_range;
  print(paste(round(mu_alpha,2),round(mu_beta,2)))
  
  #scale parameter for the random effect
  sigma_aux  =c(0.4,0.4) 


#individual level parameters
  
  #random effect for each individual
  alpha_individal_aux=rnorm(Nsubjects,0, 1);
  beta_indvidial_aux =rnorm(Nsubjects,0, 1);
  
  #RL parameters per individual given population and group effects
  alpha          = pnorm(mu_aux[1]  + sigma_aux*alpha_individal_aux);
  beta           = pnorm(mu_aux[2]  + sigma_aux*beta_indvidial_aux) * 10;

  #plot true parameters
  true.parameters=cbind(
    subject=seq(1,Nsubjects),
    alpha  =alpha,
    beta   =beta
  )
  psych::multi.hist(true.parameters,density=F,nrow=1)

  #check that sample means is near true population means
  print(paste(round(mean(alpha),2),round(mean(beta),2)))
  


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

#sanity check 1: plot mean reward vs expected value
  model<-glmer(reward ~ expval_ch+(1| subject),data = df, family = binomial)
  plot(effect('expval_ch',model))

  
#sanity check 2: pStay model-agnostic analysis
  df=df%>%mutate(stay=(choice==lag(choice,default=0))*1,
                 reward_oneback=lag(reward,default=0))
  
  model<-glmer(stay ~ reward_oneback+(reward_oneback| subject), 
               data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
  plot(effect('reward_oneback',model))
  
  #plot reward effect on pstay agianst parameters
  df_plot=cbind(coef(model)$subject,as.data.frame(true.parameters))
  gridExtra::grid.arrange(
    ggplot(df_plot,aes(x=alpha,y=reward_oneback ))+geom_point(size=3),
    ggplot(df_plot,aes(x=beta,y=reward_oneback ))+geom_point(size=3),
    ggplot(df_plot,aes(x=alpha,y=beta,color=reward_oneback ))+geom_point(size=3),
    ncol=3
  )
  

#sanity check 3: plot mean reward vs parameters
model<-glmer(reward ~ poly(alpha,2)+beta+(1| subject), 
             data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('alpha',model)) #(if you use uniform alpha and fixed beta - you will be able to see a nice hyperbolic)
plot(effect('beta',model)) #(if you use uniform alpha - you will be able to see a nice hyperbolic)


#save-------------------------------------------------------------------
save(df,file=paste('data/simulation_',Nsubjects,'subjects_',Ntrials,'trials_',Narms,'arms.Rdata',sep=""))
save(true.parameters,file=paste('data/simulation_',Nsubjects,'subjects_',Ntrials,'trials_',Narms,'arms_parameters.Rdata',sep=""))
