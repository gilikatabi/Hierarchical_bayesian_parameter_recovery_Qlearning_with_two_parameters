#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
#pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']

  #task variables
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  Narms              = cfg$Narms   #number of overall bandits in the task
  Nraffle            = cfg$Nraffle #number of bandits offered for selection each trial
  expvalues          = cfg$rndwlk  #bandit's true excpected value
  rownames(expvalues)= c('ev1','ev2','ev3','ev4')
  df                 = data.frame()
  
for (block in 1:Nblocks){
  
  #rest and allocate initial value for Qvalues
  Qval      = as.matrix(t(rep(0.5,Narms)))
  colnames(Qval)     = sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
  
  
  for (trial in 1:Ntrials_perblock){

    #select bandits to be offered to the agent
    raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    raffle    = sort(raffle)
    
    #simulate agent's action
    p         = exp(beta*Qval[raffle]) / sum(exp(beta*Qval[raffle]))
    action    = sample(raffle,1,prob=p)
    
    #simulate outcome
    reward = sample(0:1,1,prob=c(1-expvalues[action,trial],expvalues[action,trial]))
    
    #save trial's data
      dfnew=data.frame(
            subject              = subject,
            block                = block,
            trial                = trial,
            first_trial_in_block = (trial==1)*1,
            action               = action,
            offer1               = raffle[1],
            offer2               = raffle[2],
            selected_offer       = (action==raffle[2])*1+1, 
            expval_ch            = expvalues[action,trial],
            expval_unch          = expvalues[raffle[action!=raffle],trial],
            reward               = reward
            )
      #add Qvalues
      dfnew=cbind(dfnew,Qval)
      
      #add true excpected values
      dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    
    
    #updating Qvalues
    Qval[action] = Qval[action] + alpha*(reward - Qval[action])
  }
}     
  return (df)
}