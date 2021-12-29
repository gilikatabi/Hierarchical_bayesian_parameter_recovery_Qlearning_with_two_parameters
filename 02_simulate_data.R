#This code generate artificial data based on simulated parameters

rm(list=ls())

#load parameters
load('./data/true_parameters.Rdata')


#set sample size
Nsubjects =dim(true.parameters)[1] 

#set task variables 
cfg = list(Nblocks         =4,
           Ntrials_perblock=50,
           Narms           =4,  #number of arms in the task 
           Nraffle         =2,  #number of arms offered for selection each trial
           rndwlk          =read.csv('./functions/rndwlk_4frc_1000trials.csv',header=F))

#run simulation
source('./models/simulation_Narmed_bandit_task.R')

df=data.frame()
for (subject in 1:Nsubjects) {
  df=rbind(df, sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
}

#save
save(df,file='./data/simdata.Rdata')


###convert to a standata format ###----------------------------------------------------------------------------------
  
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

save(data_for_stan,file='./data/standata.Rdata')


