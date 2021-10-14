#Aim: Examine fit results
#Contributor: Nitzan Shahar, 2021

model_name=c("data/stanfit_simulation_20subjects_200trials_4arms_6chains_4000iter.rds",
             "data/stanfit_simulation_20subjects_200trials_4arms_6chains_4000iter_noalpha.rds")
rl_fit=readRDS(model_name[1])


#keep parameters --------------------------------------------------------------------------------
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
load('data/simulation_50subjects_200trials_4arms_parameters.Rdata')
plot(true.parameters[,'alpha'], apply(parVals$alpha, 2, mean))
plot(true.parameters[,'beta'],apply(parVals$beta, 2, mean))
cor(true.parameters[,'alpha'], apply(parVals$alpha, 2, mean))
cor(true.parameters[,'beta'],apply(parVals$beta, 2, mean))




# MCMC diagnostics ---------------------------------------------------------------------
library("bayesplot")
library("ggplot2")
mypars=c("mu_alpha", "mu_beta")

#print summary
summary(rl_fit, pars = mypars, probs = c(0.1, 0.9))$summary

#mcmc hist by chain
color_scheme_set("brightblue")
mcmc_dens_overlay(posterior, pars = mypars)

#bivariate plots
color_scheme_set("gray")
mcmc_scatter(posterior, pars = c("mu_alpha", "mu_beta"),
             size = 1.5, alpha = 0.5)

#trace plots
color_scheme_set("mix-blue-red")
mcmc_trace(posterior, pars = mypars,
           facet_args = list(ncol = 1, strip.position = "left"))

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
hist(parVals$y_pred)
y_pred_mean=apply(parVals$y_pred, c(2,3), mean)
y_pred_mean=as.vector(y_pred_mean)
y_pred_mean[y_pred_mean<0]<-NA
y_pred_mean<-na.omit(y_pred_mean)
length(y_pred_mean)
hist(y_pred_mean)
df$y_prd=as.vector(y_pred_mean)
df$y_prd=df$y_rep

