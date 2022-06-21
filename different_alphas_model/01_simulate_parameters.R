#This code generate artificial model parameters in an hierarchical structure

rm(list=ls())

# set sample size
Nsubjects =20 


#Population level parameters 
population_locations = c(4,qlogis(0.5),qlogis(0.3),qlogis(0.1),qlogis(0.5),qlogis(0.3),qlogis(0.7)) #population mean for learning rate and noise parameters
population_scales    = c(1.5,1,1,1,1,1,1)         #population sd for learning rate and noise parameters


#Individual parameters
beta   =       (population_locations[1]+population_scales[1]*rnorm(Nsubjects)); 
alpha_1 = plogis(population_locations[2]+population_scales[2]*rnorm(Nsubjects));
alpha_2 = plogis(population_locations[3]+population_scales[3]*rnorm(Nsubjects));
alpha_3 = plogis(population_locations[4]+population_scales[4]*rnorm(Nsubjects));
alpha_4 = plogis(population_locations[5]+population_scales[5]*rnorm(Nsubjects));
alpha_5 = plogis(population_locations[6]+population_scales[6]*rnorm(Nsubjects));
alpha_6 = plogis(population_locations[7]+population_scales[7]*rnorm(Nsubjects));

#check histograms and sample means
  print(paste(plogis(mean(qlogis(alpha_1))),mean(beta)))
  {
  par(mfrow=c(2,2))
  hist(alpha_1)
  hist(alpha_2)
  hist(alpha_3)
  hist(alpha_4)
  hist(alpha_5)
  hist(alpha_6)
  
  hist(beta)
  plot(alpha_1,beta)
  plot(alpha_2,beta)
  plot(alpha_3,beta)
  plot(alpha_4,beta)
  plot(alpha_5,beta)
  plot(alpha_6,beta)
  }

#save
true.parameters=cbind(subject = seq(1,Nsubjects),
                        beta  = beta,
                        alpha_1 = alpha_1,
                        alpha_2 = alpha_2,
                        alpha_3 = alpha_3,
                        alpha_4 = alpha_4,
                        alpha_5 = alpha_5,
                        alpha_6 = alpha_6)

save(true.parameters,file='./data/true_parameters.Rdata')
  

  