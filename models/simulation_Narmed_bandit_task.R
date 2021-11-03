#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  
  print(paste('subject',subject))

#pre-allocation of variables and parameters
  
  #set learning rate and noise parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']

  #task variables
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  Narms              = cfg$Narms            #number of overall bandits in the task
  Nraffle            = cfg$Nraffle          #number of bandits offered for selection each trial
  expvalues          = cfg$rndwlk           #bandit's true excpected value
  rownames(expvalues)= c('ev1','ev2','ev3','ev4')
  df                 = data.frame()

#main simulation section where the agent will make choice according to a Qlearning algorithm

for (block in 1:Nblocks){
  
  #Block in empirical tasks tend to have new bandits. For this reason we reset Qvallues at the start of each block.
  Qval      = as.matrix(t(rep(0.5,Narms)))
  colnames(Qval)     = sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
  
  
  for (trial in 1:Ntrials_perblock){

    #select bandits to be offered to the agent
    #e.g., if you have 4 bandits (Narms=4) and 2 offers (Nraffle=2)
    #we first need to select which of the four arms will be offered for the subject's selection 
    #(e.g., raffle = c(4,1) for an offer of the forth and first bandit in trial t)
    raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    raffle    = sort(raffle)
    
    #simulate agent's action according to a softmax policy
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
       
    
    
    #updating Qvalues according to a prediction error signal
    Qval[action] = Qval[action] + alpha*(reward - Qval[action])
  }
}     
  return (df)
}