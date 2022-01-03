#This code generate artificial model parameters in an hierarchical structure

rm(list=ls())

# set sample size
Nsubjects =20 


#Population level parameters 
population_locations    =c(qlogis(0.3),4,qlogis(0.8)) #population mean for learning rate and noise parameters
population_scales       =c(1,1.5,1)         #population sd for learning rate and noise parameters


#Individual parameters 
alpha          = plogis(population_locations[1]+population_scales[1]*rnorm(Nsubjects));
beta           =       (population_locations[2]+population_scales[2]*rnorm(Nsubjects)); 
lambda         = plogis(population_locations[3]+population_scales[3]*rnorm(Nsubjects));

#check histograms and sample means
  print(paste(plogis(mean(qlogis(alpha))),mean(beta)))
  {
  par(mfrow=c(2,2))
  hist(alpha)
  hist(beta)
  hist(lambda)
  plot(alpha,beta)
  }

#save
true.parameters=cbind(subject = seq(1,Nsubjects),
                        alpha = alpha,
                        beta  = beta,
                        lambda = lambda)

save(true.parameters,file='./data/true_parameters.Rdata')
  

  