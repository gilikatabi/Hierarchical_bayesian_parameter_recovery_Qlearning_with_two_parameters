#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  
  print(paste('subject',subject))
  
  #pre-allocation of variables and parameters
  
  #set learning rate and noise parameters
  alpha  = parameters['alpha']
  beta   = parameters['beta']
  lambda = parameters['lambda']
  
  #task variables
  Nstages            = cfg$Nstages
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  Narms              = cfg$Narms            #number of overall bandits in the task
  Nstates            = cfg$Nstates
  expvalues          = cfg$rndwlk           #bandit's true excpected value
  rownames(expvalues)= c('ev1','ev2','ev3','ev4','ev5','ev6','ev7','ev8')
  df                 = data.frame()
  
  #main simulation section where the agent will make choice according to a Qlearning algorithm
  for (block in 1:Nblocks){
    
    #Block in empirical tasks tend to have new bandits. For this reason we reset Qvallues at the start of each block.
    Qval      = array(0, dim = c(Narms, Nstates, Nstages))
    ###?
    #colnames(Qval)     = sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
    
    
    for (trial in 1:Ntrials_perblock){
      
      #simulate agent's actions according to a softmax policy
      state1  = 1
      p_1     = exp(beta*Qval[, state1, 1]) / sum(exp(beta*Qval[, state1, 1]))
      choice1 = sample(1:2,1,prob=p_1)
      
      state2  = choice1
      p_2     = exp(beta*Qval[, state2, 2]) / sum(exp(beta*Qval[, state2, 2]))
      choice2 = sample(1:2,1,prob=p_2)
      
      state3  = -2 + 2*choice1 + choice2
      p_3       = exp(beta*Qval[, state3, 3]) / sum(exp(beta*Qval[, state3, 3]))
      choice3 = sample(1:2,1,prob=p_3)
      
      #simulate outcome
      reward = sample(0:1,1,prob=c(1-expvalues[-2 + 2*state3 + choice3,trial],expvalues[-2 + 2*state3 + choice3,trial]))
      
      #save trial's data
      dfnew=data.frame(
        subject              = subject,
        block                = block,
        trial                = trial,
        first_trial_in_block = (trial==1)*1,
        choice1              = choice1,
        choice2              = choice2,
        choice3              = choice3,
        state1               = state1,
        state2               = state2,
        state3               = state3,
        expval_ch            = expvalues[-2 + 2*state3 + choice3,trial],
        reward               = reward
      )
      #browser()
      #add Qvalues
      #dfnew=cbind(dfnew,Qval)
      
      #add true excpected values
      dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
      
      #bind to the overall df
      df=rbind(df,dfnew)
      
      #update Qvalues
      PE_1 = Qval[choice2, state2, 2] - Qval[choice1, state1, 1]
      PE_2 = Qval[choice3, state3, 3] - Qval[choice2, state2, 2]
      PE_3 = reward                   - Qval[choice3, state3, 3]
      
      Qval[choice1, state1, 1] = Qval[choice1, state1, 1] + 
        alpha*PE_1 +
        alpha*lambda*PE_2 +
        alpha*(lambda**2)*PE_3
      
      Qval[choice2, state2, 2] = Qval[choice2, state2, 2] + 
        alpha*PE_2 + alpha*lambda*PE_3
      
      Qval[choice3, state3, 3] = Qval[choice3, state3, 3] + alpha*PE_3
      
    }
  }
  return (df)
}