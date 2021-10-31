#This code generate artificial data based on a Q-learning model (alpha and beta).
#It has four parts:
# a. Generate true parameters based on an hierarchical structure 
# b. simulate data
# c. Convert data to a stan-data format
# d. run some linear regression sanity checks on the code.


rm(list=ls())

model_name=c('null')

Nsubjects           =20000   
Nblocks             =5    
Ntrials_perblock    =100
Ntrials             =Nblocks*Ntrials_perblock
rndwlk              =read.csv('./functions/rndwlk_4frc_1000trials.csv',header=F)[,1:Ntrials_perblock]
Narms               =4  #this is the number of overall bandits in the task
Nraffle             =2  #this is the amount of arms offered for selection each trial



#-------------------------------------------------------------------------------------------------------------
# Part A: Generate true population and individual level parameters


###population location parameters##
  
  #true population level parameters 
  mu_alpha   =0.5
  mu_beta    =4
  
  #true unbounded location and scale parameters (alpha between 0 and 1, beta between 0 and beta_upper_limit).
  beta_upper_limit = 1
  mu_aux           =c(qnorm(mu_alpha),qnorm(mu_beta/beta_upper_limit))
  sigma_aux        =c(1,1) 
  
  #transform location parameters back to natural scale (just for practice!)
  print(paste(pnorm(mu_aux[1], 0,1),pnorm(mu_aux[2])*beta_upper_limit))
  


###individual level parameters###
  
  #random effect for each individual
  alpha_individal_aux=rnorm(Nsubjects,0, 1);
  beta_indvidial_aux =rnorm(Nsubjects,0, 1);
  
  #individual parameters in natural scale
  
  alpha          = pnorm(mu_aux[1]+ sigma_aux[1]*alpha_individal_aux);
  beta           = pnorm(mu_aux[2]+ sigma_aux[2]*beta_indvidial_aux)*beta_upper_limit;

  #check histograms and sample means
  par(mfrow=c(2,2))
  hist(alpha)
  hist(beta)
  plot(alpha,beta)
  print(paste(round(mean(alpha),2),round(mean(beta),2)))
  
  #save
  true.parameters=cbind(subject = seq(1,Nsubjects),
                        alpha   = alpha,
                        beta    = beta)
  save(true.parameters,file=paste('./data/',model_name,'_true_parameters.Rdata',sep=""))
  

  
  
  
  
#-------------------------------------------------------------------------------------------------------------
# Part B: Simulate data based on task values and individual parameters from previous secion
  
#set some task variables 
cfg = list(Nblocks         =Nblocks,
           Ntrials_perblock=Ntrials_perblock,
           Narms           =Narms,    
           Nraffle         =Nraffle,  
           rndwlk          =rndwlk)

#run simulation
source('./models/simulation_Narmed_bandit_task.R')
df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
}

#save
save(df,file=paste('./data/',model_name,'_simdata.Rdata',sep=""))






#-------------------------------------------------------------------------------------------------------------
# Part C: Convert the data to a stan format. 
# OPTIONAL: Add some random missing data before converting and saving to stan format.

###adding missing data###

# the percentage of missing data for each individual will be sampled from a uniform 
# distribution from 0 up to "max_precent_of_aborted_trials" which will be the upper limit of that distribution.
# we can set max_precent_of_aborted_trials=0 to have no missing data.
# the 'make_standdata' function is using padding to technically handle missing data.

  #set the maximum percent of missing data per individual for the sample
  max_precent_of_aborted_trials=0

  #generate missing data
  df$abort =0
  Nsubjects=max(df$subject)

  for (subject in seq(1:max(df$subject))){
    index_abort           =sample(which(df$subject==subject),runif(1,min=0,max=max_precent_of_aborted_trials)*Ntrials)  #index of rows to abort
    df$abort[index_abort] =1
  }
  
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
