#This code generate artificial data based on simulated parameters

rm(list=ls())

#load parameters
load('./data/true_parameters.Rdata')


#set sample size
Nsubjects =dim(true.parameters)[1] 

#set task variables 
cfg = list(Nstages         = 3,
           Nblocks         = 2,
           Ntrials_perblock= 100,
           Narms           = 2,  #number of arms in the task 
           Nstates         = 4,
           rndwlk          = read.csv('./functions/rndwlk_depth3_100trials.csv',header=F))

#run simulation
source('./models/simulation_eligibility_traces_depth_3.R')

df=data.frame()
for (subject in 1:Nsubjects) {
  df = rbind(df, sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
  
  #df=rbind(df, sim.block(subject=subject, parameters=true.parameters[subject,],cfg=cfg))
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
                                 'state1',
                                 'state2',
                                 'state3',
                                 'choice1',
                                 'choice2',
                                 'choice3',
                                 'reward'),
                               additional_arguments=list(Nstages=3,Nstates=4))

save(data_for_stan,file='./data/standata.Rdata')


