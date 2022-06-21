###convert to a standata format ###----------------------------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(hablar)

load('./data/model_df.Rdata')

df <- df %>% convert(int(subject),int(block), int(trial),int(choice1),int(choice2),int(choice3),int(reward))
df$first_trial_in_block <- as.double(df$first_trial_in_block)
df$state1 <- as.double(df$state1)
df$state2 <- as.double(df$state2)
df$state3 <- as.double(df$state3)

df <- as.data.frame(df)
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
