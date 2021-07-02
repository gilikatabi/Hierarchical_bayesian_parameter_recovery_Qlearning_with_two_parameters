#### simulate Rescorla-Wagner block for participant ----
sim.block = function(Ntrials, Nalt, alpha, beta, expvalue){ 
  Q      = rep(0,Nalt)
  action = rep(NA, Ntrials)
  reward = rep(NA, Ntrials)
  
  for (t in 1:Ntrials){
    p         = exp(beta*Q) / sum(exp(beta*Q))
    action[t] = sample(1:Nalt,1,prob=p)
    reward[t] = sample(0:1,1,prob=c(1-expvalue[action[t],t],expvalue[action[t],t]))
    Q[action[t]] = Q[action[t]] + alpha*(reward[t] - Q[action[t]])
  }
  return (data.frame(action,reward))
}