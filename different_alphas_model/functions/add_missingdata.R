
add_missingdata<-function(df,max_precent_of_aborted_trials){
  

#add an abort column with no missing data
df$abort =0

#count the number of subjects
Nsubjects=max(df$subject)
Ntrials  =max(df$trial)*max(df$block)
#for each subject:
#1. simulate a missing data rate from a uniform distribution [0,max_precent_of_aborted_trials]
#2. randomly label the 'abort' column to 1 noting that the trial should be considered as missing data
for (subject in seq(1:max(df$subject))){
  index_abort           =sample(which(df$subject==subject),runif(1,min=0,max=max_precent_of_aborted_trials)*Ntrials)  #index of rows to abort
  df$abort[index_abort] =1
}
df$abort[df$first_trial_in_block==1]=0
return(df)}