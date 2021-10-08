#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  alpha = parameters['alpha']
  beta  = parameters['beta']
  
  Narms     = cfg$Narms
  Nraffle   = cfg$Nraffle
  Nblocks   = cfg$Nblocks
  Ntrials   = cfg$Ntrials
  expvalues = cfg$rndwlk

  Qval      = rep(0,Narms)
  df     =data.frame()
for (block in 1:Nblocks){
  for (trial in 1:Ntrials){
    #computer offer
    #raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    raffle=c(1,2)
    #players choice
    p         = exp(beta*Qval[raffle]) / sum(exp(beta*Qval[raffle]))
    choice    = sample(raffle,1,prob=p)
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[choice,trial],expvalues[choice,trial]))
    
    #save trial's data
    df=rbind(df,data.frame(
            subject    = subject,
            block      = block,
            trial      = trial,

            
            
            choice     = choice,
            expval_ch  = expvalues[choice,trial],
            reward     = reward
    ))
          
    #updating Qvalues
    Qval[choice] = Qval[choice] + alpha*(reward - Qval[choice])
  }
}     
  return (df)
}




