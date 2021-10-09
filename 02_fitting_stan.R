#aim: Parameter recovery Hierarchical fit Stan - using cholesky covariance matrix 
#contributor: Shira Niv, Nitzan Shahar, 2021


rm(list=ls())
library('rstan') # observe startup messages
library("truncnorm")
library(parallel)
library(gtools) #inv.logit function 
library(MASS)
library(dplyr)



#load data-------------------------------------------------
load('data/simulation_20subjects_200trials_4arms.Rdata')
load('data/simulation_20subjects_200trials_4arms_parameters.Rdata')


# prepare stan data ---------------------------------------

# add abort column to simulate missing trials 
max_precent_of_aborted_trials=0.1
df$abort<-0
Nsubjects=max(df$subject)
Ntrials  =max(df$trial)

for (subject in seq(1:max(df$subject))){
  index_abort           =sample(which(df$subject==subject),runif(1,min=0,max=max_precent_of_aborted_trials)*Ntrials)  #index of rows to abort
  df$abort[index_abort] =1
}

df%>%group_by(subject)%>%summarise(mean(abort)) #count and omit aborted trials
df<-df[df$abort==0,]
df%>%group_by(subject)%>%summarise(mean(abort))

source('functions/make_mystandata.R')
df$action=df$choice
data_for_stan<-make_mystandata(data=df, 
                               subject_column      =df$subject,
                               var_toinclude      =c(
                                 'action',
                                 'reward'),
                                 additional_arguments=list(Narms=4))



# parameter recovery with stan --------------------------------------------

#fit stan model   

start_time <- Sys.time()
models_names=c("models/model_Narmed_bandit_alpha_beta_Phi_approx.stan")

rl_fit<- stan(file = models_names[1], 
              data=data_for_stan, 
              iter=200,
              chains=1,
              cores =1) 
end_time <- Sys.time()

end_time-start_time


#keep parameters
parVals <- rstan::extract(rl_fit, permuted = TRUE)
names(parVals)
posterior <- as.array(rl_fit)


# compare recovered parameters to true parameters  --------------------------------------------
#population level (hyperparameter)
  #intervals
  color_scheme_set("red")
  mcmc_intervals(posterior, pars = c("mu_alpha", "mu_beta"))
  
  #areas
  mcmc_areas(
    posterior,
    pars = c("mu_alpha", "mu_beta"),
    prob = 0.8, # 80% intervals
    prob_outer = 0.99, # 99%
    point_est = "mean"
  )

  #simple print
  mean(parVals$mu_alpha)
  mean(parVals$mu_beta)
  hist(parVals$mu_alpha)
  hist(parVals$mu_beta)
  

#compare true vs fitted group level parameters (that is - subjects parameters since subject is a "group" in the hierarchical model)
plot(true.parameters[,'alpha'], apply(parVals$alpha, 2, mean))
plot(true.parameters[,'beta'],apply(parVals$beta, 2, mean))




# MCMC diagnostics ---------------------------------------------------------------------
library("bayesplot")
library("ggplot2")
mypars=c("mu_alpha", "mu_beta")

#print summary
summary(rl_fit, pars = mypars, probs = c(0.1, 0.9))$summary

#mcmc hist by chain
color_scheme_set("brightblue")
mcmc_hist_by_chain(posterior, pars = mypars)
mcmc_dens_overlay(posterior, pars = mypars)

#bivariate plots
color_scheme_set("gray")
mcmc_scatter(posterior, pars = c("mu_alpha", "mu_beta"),
             size = 1.5, alpha = 0.5)

#trace plots
color_scheme_set("mix-blue-red")
mcmc_trace(posterior, pars = mypars,
           facet_args = list(ncol = 1, strip.position = "left"),
           n_warmup=100,inc_warmup=T)

#R-hat
rhats <- rhat(rl_fit)
color_scheme_set("brightblue") # see help("color_scheme_set")
mcmc_rhat(rhats)

#effective sample size
ratios_cp <- neff_ratio(rl_fit)
mcmc_neff(ratios_cp, size = 2)

#autocorrelations
mcmc_acf(rl_fit, pars = "mu_alpha", lags = 10)
mcmc_acf(rl_fit, pars = "mu_beta", lags = 10)

#optional:
#shiny
library("shinystan")
launch_shinystan(stan_fit)




#posterior predictive check -----------------------------------------------
library(dplyr)
dim(parVals$y_pred)
y_pred_mean=apply(parVals$y_pred, c(2,3), mean)
y_pred_mean=as.vector(y_pred_mean)
y_pred_mean[y_pred_mean<0]<-NA
y_pred_mean<-na.omit(y_pred_mean)
length(y_pred_mean)

# empirical data 
df$y_rep=as.vector(y_pred_mean)

true_y = array(NA, c(Nsubjects, Ntrials))
true_y = t(sapply(unique(df$subject),function(subject)   
          { current_var=df$action[df$subject==subject]
            c(current_var,rep(-1,Ntrials-sum(df$subject==subject)))}))

## Subject #1
subject=2
{
  plot(true_y[subject,1:280 ], type="l", xlab="Trial", ylab="Choice (1 to 4)", yaxt="n")
  lines(y_pred_mean[subject,1:280], col="red", lty=2)
  axis(side=2, at = c(1,2,3,4) )
  legend("bottomleft", legend=c("True", "PPC"), col=c("black", "red"), lty=1:2)
}



library(loo)
loo(rl_fit)


df<-read.delim('data/bandit2arm_exampleData.txt')
colnames(df)<-c('subject','trial','action','reward')
df$reward[df$reward==c(-1)]=0

lik<-extract_log_lik(rl_fit, parameter_name = "log_lik", merge_chains = TRUE) 
dim(lik)
hist(apply(lik,1,mean))
loo1_r_eff <- relative_eff(exp(loo1_loglike_vector), cores = 4)
loo_1 <- loo(log_lik_1, r_eff = r_eff, cores = 2)