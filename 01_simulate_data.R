#This code generate artificial data based on a Q-learning model (alpha and beta).
#It has four parts:
# a. Generate true parameters based on an hierarchical structure 
# b. simulate data
# c. Convert data to a stan-data format
# d. run some linear regression sanity checks on the code.


rm(list=ls())

model_name=c('null')

Nsubjects           =50 


#-------------------------------------------------------------------------------------------------------------
# Part A: Generate true population and individual level parameters



  #true population level parameters 
  population_locations    =c(qlogis(0.3),4) #population mean for learning rate and noise parameters
  population_scales       =c(1,1)       #population sd for learning rate and noise parameters


  #individual parameters 
  alpha          = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects));
  beta           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects)); #same as using rlnorm

  #check histograms and sample means
  print(paste(plogis(mean(qlogis(alpha))),mean(beta)))
  {
  par(mfrow=c(2,2))
  hist(alpha)
  hist(beta)
  plot(alpha,beta)
  }
  #save
  true.parameters=cbind(subject = seq(1,Nsubjects),
                        alpha   = alpha,
                        beta    = beta)
  save(true.parameters,file=paste('./data/',model_name,'_true_parameters.Rdata',sep=""))
  

  
  
  
  
#-------------------------------------------------------------------------------------------------------------
# Part B: Simulate data based on task values and individual parameters from previous section
  
#set some task variables 
cfg = list(Nblocks         =4,
           Ntrials_perblock=50,
           Narms           =4,  #number of arms in the task 
           Nraffle         =2,  #number of arms offered for selection each trial
           rndwlk          =read.csv('./functions/rndwlk_4frc_1000trials.csv',header=F))

#run simulation
source('./models/simulation_Narmed_bandit_task.R')
  #windows
  df=data.frame()
  for (subject in 1:Nsubjects) {
    df=rbind(df, sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
  }

  #linux (e.g., on a vm)
    # library(parallel)
    # 
    # df=mclapply(1:Nsubjects, function(subject) {
    #   rbind(sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
    #   },mc.cores=8
    #   )
    #
    # df=do.call(rbind,df)

#save
save(df,file=paste('./data/',model_name,'_simdata.Rdata',sep=""))






#-------------------------------------------------------------------------------------------------------------
# Part C: Convert the data to a stan format. 

###adding missing data###
# OPTIONAL: Add some random missing data before converting and saving to stan format.
# you can also skip this if no missing data is required

# the percentage of missing data for each individual will be sampled from a uniform 
# distribution from 0 up to "max_precent_of_aborted_trials" which will be the upper limit of that distribution.
# we can set max_precent_of_aborted_trials=0 to have no missing data.
# the 'make_standdata' function is using padding to technically ignore missing data.

source('./functions/add_missingdata.R')
df=add_missingdata(df,max_precent_of_aborted_trials=0)

#check the percent of missing data for each individual
library(dplyr)  
df%>%group_by(subject)%>%summarise(mean(abort)) 

#take out missing data trials
df<-df[df$abort==0,]



###convert to a standata format ###
  
source('./functions/make_mystandata.R')
data_for_stan<-make_mystandata(data=df, 
                               subject_column     =df$subject,
                               block_column       =df$block,
                               var_toinclude      =c(
                                 'first_trial_in_block',
                                 'trial',
                                 'offer1',
                                 'offer2',
                                 'action',
                                 'reward',
                                 'selected_offer'),
                               additional_arguments=list(Narms=4, Nraffle=2))

save(data_for_stan,file=paste('./data/',model_name,'_standata.Rdata',sep=""))





#-------------------------------------------------------------------------------------------------------------
# Part D: Run some basic sanity checks using linear / logistic regression
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(effects)

#sanity check 1: plot mean reward vs expected value. This has to be positive (higher excpected value - higer mean reward)
model<-glmer(reward ~ expval_ch+(1| subject),data = df, family = binomial)
plot(effect('expval_ch',model))


#sanity check 2: pStay model-agnostic analysis.
#Show a tendency to stay with the same bandit after reward, and switch after unrewarded trials
df=df%>%mutate(stay=(action==lag(action,default=0))*1,
               reward_oneback=lag(reward,default=0))

model<-glmer(stay ~ reward_oneback+(reward_oneback| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))


#sanity check 3: plot mean reward vs parameters
model<-glm(reward ~ poly(alpha,2)+beta, 
             data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
             family = binomial)
plot(effect('alpha',model)) #(should get some hyperbolic function, mostly when beta is fixed across all subjects)
plot(effect('beta',model))  #(should see some positive linear trend)
