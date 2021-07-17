#### simulate Rescorla-Wagner block for participant ----
sim.block = function(parameters,cfg,subject){ 
  print(paste('subject',subject))
  
  alpha = psych::logistic(parameters[1])
  beta  = exp(parameters[2])
  
  Narms     = cfg$Narms
  Nraffle   = cfg$Nraffle
  Nblocks   = cfg$Nblocks
  Ntrials   = cfg$Ntrials
  expvalues = cfg$rndwlk

  Q      = rep(0,Narms)
  df     =data.frame()
for (block in 1:Nblocks){
  for (trial in 1:Ntrials){
    #computer offer
    raffle    = sample(1:Narms,Nraffle,prob=rep(1/Narms,Narms)) 
    
    #players choice
    p         = exp(beta*Q[raffle]) / sum(exp(beta*Q[raffle]))
    action    = sample(raffle,1,prob=p)
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[action,trial],expvalues[action,trial]))
    
    #save trial's data
    df=rbind(df,data.frame(
            subject= subject,
            block  = block,
            trial  = trial,
            offer1 = raffle[1],
            offer2 = raffle[2],
            action = action,
            expval1= expvalues[raffle[1],trial],
            expval2= expvalues[raffle[2],trial],
            Q1     =Q[1],
            Q2     =Q[2],
            Q3     =Q[3],
            Q4     =Q[4],
            reward = reward
    ))
          
    #updating Qvalues
    Q[action] = Q[action] + alpha*(reward - Q[action])
  }
}     
  return (df)
}