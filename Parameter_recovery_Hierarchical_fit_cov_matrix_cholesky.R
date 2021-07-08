#aim: Parameter recovery Hierarchical fit Stan - using cholesky covariance matrix 
#contributor: Shira Niv, Nitzan Shahar

rm(list=ls())
library('rstan') # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 
library(MASS)
library(dplyr)



# generate population and subject level parameters -----------------------------------------------------------

Nsubjects =10       #number of agents

#population parameters
alpha_mu     =0.5
beta_mu      =5
Nparam=2

#population aux parameters (Phi approximation is like pnorm, qnorm is its inverse)
alpha_aux_mu    = qnorm(alpha_mu) #logit(alpha_mu)
beta_aux_mu     = qnorm(beta_mu/10)#log(beta_mu)
alpha_aux_var = 0.2
beta_aux_var  = 0.05
corr_alpha_beta = 0
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
auxiliary_parameters = mvrnorm(n = Nsubjects, mu = mu_vector, Sigma = sigma_matrix)


#convert auxiliary parameters to true parameters 
true.parms <-auxiliary_parameters
colnames(true.parms)<-c("alpha","beta")
true.parms[,1]<-pnorm(true.parms[,1]) #inv.logit(true.parms[,1])
true.parms[,2]<-pnorm(true.parms[,2])*10
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

Nalt  =2         #number of alternatives
Ntrials  =300       #number of trials
source('models/sim_Narmed_bandit.R')
rndwlk<-read.csv('data/rndwlk_4frc_1000trials.csv',header=F)


df<- lapply(1:Nsubjects,function(s)           {
                      
                      df_subj=cbind(subject=rep(s,Ntrials),
                                    trial=(1:Ntrials),
                                    sim.block(Ntrials,Nalt,true.parms[s,1],true.parms[s,2],rndwlk))
                      })

df<-do.call(rbind,df)

# prepare stan data ---------------------------------------

# add abort column to simulate missing trials 
max_precent_of_aborted_trials=0.1
df$abort<-0
for (subject in seq(1:Nsubjects)){
  
  index_abort           =sample(which(df$subject==subject),runif(1,min=0,max=max_precent_of_aborted_trials)*Ntrials)  #index of rows to abort
  
  df$abort[index_abort]=1
}

df%>%group_by(subject)%>%summarise(mean(abort)) #count and omit aborted trials
df<-df[df$abort==0,]
df%>%group_by(subject)%>%summarise(mean(abort))

#arrange data for stan in a subject x trial
source('functions/make_mystandata.R')
data_for_stan<-make_mystandata(data=df, 
                               subject_column      =df$subject,
                               var_toinclude      =c(
                                 'action',
                                 'reward'),
                                 additional_arguments=list(Narms=4))
# parameter recovery with stan --------------------------------------------

#fit stan model   

start_time = Sys.time()
stan_fit    = stan(file = "models/model_Narmed_bandit_alpha_beta_withPhi.stan", 
                  data=data_for_stan, 
                  iter=2000,
                  chains=2,
                  cores =2) 
end_time <- Sys.time()

end_time-start_time

#second run: withPhi, 10 subjects, 300 trials, 2000 iteration, 2 cores, 2 chains, took 1.99min
#second run: noPhi, 10 subjects, 300 trials, 2000 iteration, 2 cores, 2 chains, took ??min

parVals <- rstan::extract(stan_fit, permuted = TRUE)


# exmine model   --------------------------------------------
library("bayesplot")
library(ggplot2)

#mcmc chains
traceplot(stan_fit, inc_warmup = TRUE, nrow = 3)

#plot mcmc chains
color_scheme_set("blue")
mcmc_trace(stan_fit, pars = c("mu[1]", "mu[2]","mu[3]"), n_warmup=0,
           facet_args = list(ncol = 1, strip.position = "left"))

plot(stan_fit)

#plot posteriors
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(stan_fit,
           pars = c("mu[2]"),
           prob = 0.95) + plot_title

       extract_ic(stan_fit, ic = "looic")$LOOIC$estimates[3,1]

n_chains <- length(stan_fit@stan_args)
lik=loo::extract_log_lik(stanfit = stan_fit, parameter_name = "log_lik")
rel_eff = loo::relative_eff(
                              exp(lik),  
                              chain_id = rep(1:n_chains, each = nrow(lik) / n_chains),
                              cores = 2)

LOOIC <- loo::loo(lik, r_eff = rel_eff, cores = 2)


#posterior predictive check -----------------------------------------------
library(dplyr)
dim(parVals$y_pred)
y_pred_mean=apply(parVals$y_pred, c(2,3), mean)
dim(y_pred_mean)

# empirical data 
true_y = array(NA, c(Nsubjects, Ntrials))
true_y = t(sapply(unique(df$subject),function(subject)   
          { current_var=df$action[df$subject==subject]
            c(current_var,rep(-1,Ntrials-sum(df$subject==subject)))}))

## Subject #1
subject=2
{
  plot(true_y[subject, ], type="l", xlab="Trial", ylab="Choice (0 or 1)", yaxt="n")
  lines(y_pred_mean[subject,], col="red", lty=2)
  axis(side=2, at = c(0,1) )
  legend("bottomleft", legend=c("True", "PPC"), col=c("black", "red"), lty=1:2)
}


# compare recovered parameters to true parameters  --------------------------------------------
        
#population level (hyperparameter)
mean(pnorm(parVals$mu_aux[,1]))
mean(pnorm(parVals$mu_aux[,2]))*10
hist(parVals$mu_alpha)
#individual level parameters (subjects parameters)
plot(true.parms[,1], apply(parVals$alpha, 2, mean))
plot(true.parms[,2],apply(parVals$beta, 2, mean))

