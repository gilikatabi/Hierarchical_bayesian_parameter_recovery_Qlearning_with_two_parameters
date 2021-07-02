#aim: Parameter recovery Hierarchical fit Stan - using cholesky covariance matrix 
#contributor: Shira Niv, Nitzan Shahar

rm(list=ls())
library('rstan') # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 
library(MASS)



# generate population and subject level parameters -----------------------------------------------------------

source('sim_Narmed_bandit.R')
rndwlk<-read.csv('rndwlk_4frc_1000trials.csv',header=F)

#generate parameters and data for N agents. 
Nsubj =10       #number of agents
Nalt  =4         #number of alternatives
Ntrl  =300       #number of trials
Nparam=2         #number of parameters

#population parameters
alpha_mu     =0.5
beta_mu      =5

#population aux parameters
alpha_aux_mu    = logit(alpha_mu)
beta_aux_mu     = log(beta_mu)
alpha_aux_var = 0.2
beta_aux_var  = 0.05
corr_alpha_beta = 0.2
cov_alpha_beta  =sqrt(alpha_aux_var)*sqrt(beta_aux_var)*corr_alpha_beta #cov_xy=sd_x*sd_y*cor_xy

#creat a mean vector and a variance-covariance matrix (i.e., sigma_matrix)
mu_vector    =c(alpha_aux_mu,beta_aux_mu)
tau          =c(alpha_aux_var,beta_aux_var) #var vector
sigma_matrix = diag(tau)
sigma_matrix[!diag(nrow=Nparam)]=cov_alpha_beta

#demonstrate conversion from cov matrix (sigma_matirx), to cor matrix (Omega) to cholesky factor (L_omega)
Omega=cov2cor(sigma_matrix)    #cov to cor
L_Omega=t(chol(Omega))         #cor to cholesky
round(t(L_Omega) %*% L_Omega,1)#cholesky back to cor

# sample aux parameters from the population with mu_vecto and cov matrix (sigma_matrix)
auxiliary_parameters = mvrnorm(n = Nsubj, mu = mu_vector, Sigma = sigma_matrix)


#convert auxiliary parameters to true parameters 
true.parms <-auxiliary_parameters
colnames(true.parms)<-c("alpha","beta")
true.parms[,1]<-inv.logit(true.parms[,1])
true.parms[,2]<-exp(true.parms[,2])
hist(true.parms[,1])
hist(true.parms[,2])


#check that we got data with statistics as expected
cat(paste('true alpha population parm is', alpha_mu,',  sample mean is',round(mean(true.parms[,1]),3)),
    paste('true beta population parm is',  beta_mu,',  sample mean is',round(mean(true.parms[,2]),3)),
    paste('true alpha population parm is', alpha_aux_mu,',  sample mean is',round(mean(auxiliary_parameters[,1]),3)),
    paste('true beta population parm is',  beta_aux_mu,',  sample mean is',round(mean(auxiliary_parameters[,2]),3)),
    paste('true alpha aux var parm is',    alpha_aux_var,',  sample mean is',round(var(auxiliary_parameters[,1]),3)),
    paste('true beta aux var parm is',     beta_aux_var,',  sample mean is',round(var(auxiliary_parameters[,2]),3)),
    paste('true corr between alpha and beta aux parms is', corr_alpha_beta,'true sample mean is',round(cor(auxiliary_parameters[,1],auxiliary_parameters[,2]),3)),
    sep = '\n')



# run a simulation study -----------------------------------------------------------
# simulating N agents in the 2 step task 

df<- lapply(1:Nsubj,function(s)           {
                      
                      df_subj=cbind(subj=rep(s,Ntrl),
                                    trial=(1:Ntrl),
                                    sim.block(Ntrl,Nalt,true.parms[s,1],true.parms[s,2],rndwlk))
                      })

df<-do.call(rbind,df)



# parameter recovery with stan --------------------------------------------

#prepare action and reward matrices (subject x trial)
a1=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'action']}))
reward=t(sapply(1:Nsubj,function(subj) {df[df$subj==subj,'reward']}))

#prepare data
model_data <- list(Nsubj = Nsubj,
                   Ntrials = Ntrl,
                   Narms = Nalt,
                   a1 = a1,
                   reward = reward,
                   Nparam = Nparam
                   )
        
#fit stan model   
<<<<<<< Updated upstream
rl_fit<- stan(file = "Hierarchical_cov_matrix_cholesky.stan", data=model_data, iter=2000,chains=6,cores =6) #iter - number of MCMC samples 
=======
start_time <- Sys.time()
rl_fit<- stan(file = "Hierarchical_cov_matrix_cholesky.stan", 
              data=data_for_stan, 
              iter=2000,
              chains=4,
              cores =4) 
end_time <- Sys.time()
end_time-start_time

#first run: 4 cores, 4 chains, Time difference of 5.888444 mins
#second run: 4 cores, 4 chains, Time difference of 3.35554 mins
>>>>>>> Stashed changes

print(rl_fit)
rl_fit<-readRDS('fit.rds')


# compare recovered parameters to true parameters  --------------------------------------------

        
#population level (hyperparameter)
alpha_aux_mu_recovered   = (summary(rl_fit , pars=c("mu[1]"))$summary[,1])
beta_aux_mu_recovered    = summary(rl_fit , pars=c("mu[2]"))$summary[,1]
sigma_recovered          = matrix(summary(rl_fit , pars=c("sigma_matrix"))$summary[,1],2,2)
omega_recovered          = cov2cor(sigma_matrix_recovered)

tau_recovered            =summary(rl_fit , pars=c("tau"))$summary[,1]
L_Omega_recovered        =matrix(summary(rl_fit , pars=c("L_Omega"))$summary[,1],2,2)
omega=diag(tau_recovered)*L_Omega_recovered*t(L_Omega_recovered)
omega*diag(tau_recovered)
cov2cor(omega*diag(tau_recovered))




#compare recovered to true population parameters
#location parameters
cat(paste('true alpha population parm is',     alpha_mu,' sample mean is',    mean(true.parms[,1]),'and recovered is',          inv.logit(alpha_aux_mu_recovered)),
    paste('true beta population parm is',      beta_mu,' sample mean is',     mean(true.parms[,2]),'and recovered is',          exp(beta_aux_mu_recovered)),
    paste('true alpha aux population parm is', alpha_aux_mu,' sample mean is',mean(auxiliary_parameters[,1]),'and recovered is',alpha_aux_mu_recovered),
    paste('true beta aux population parm is',  beta_aux_mu,' sample mean is', mean(auxiliary_parameters[,2]),'and recovered is',beta_aux_mu_recovered),
    sep = '\n')

#scale parameters
cat(paste('true alpha aux var parm is',        alpha_aux_var,' sample mean is',  var(auxiliary_parameters[,1]),'and recovered is',alpha_aux_var_recovered),
    paste('true beta aux var parm is',         beta_aux_var,' sample mean is',   var(auxiliary_parameters[,2]),'and recovered is',beta_aux_var_recovered),
    paste('true corr between aux parms is',    corr_alpha_beta,' sample mean is',cor(auxiliary_parameters[,1],auxiliary_parameters[,2]),
      'and recovered is',cov_alpha_beta_recovered/(sqrt(alpha_aux_var_recovered)*sqrt(beta_aux_var_recovered))),
    sep = '\n')


#individual level parameters (subjects parameters)
alpha_individual_recovered=summary(rl_fit , pars=c("alpha"))$summary[,1] 
beta_individual_recovered=summary(rl_fit , pars=c("beta"))$summary[,1]
plot(true.parms[,1],(alpha_individual_recovered))
plot(true.parms[,2],(beta_individual_recovered))
cor(true.parms,cbind(alpha_individual_recovered,beta_individual_recovered))

#additional
summary(rl_fit, pars=c("L_Omega"))$summary
sigma_matrix_recovered=summary(rl_fit, pars=c("sigma_matrix"))$summary[,1]


cor(true.parms[,1],true.parms[,2])
cor(alpha_recovered,beta_recovered)
var(true.parms[,1])
var(alpha_recovered)
var(true.parms[,2])
var(beta_recovered)





