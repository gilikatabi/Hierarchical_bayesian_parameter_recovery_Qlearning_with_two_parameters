rm(list=ls())

typical_params <- load("~/phd_studies/Tree_task/parameter_recovery/Tree_task_hierarchical_bayesian_parameter_recovery/data/pilot_dec21_typical/null_recovered_parameters.rdata")
typical_params =pars
typical_alpha <- apply(typical_params$alpha, 2,mean)
typical_beta <- apply(typical_params$beta, 2,mean)
typical_lambda <- apply(typical_params$lambda, 2,mean)


ADHD_params = load("~/phd_studies/Tree_task/parameter_recovery/Tree_task_hierarchical_bayesian_parameter_recovery/data/pilot_dec21_ADHD/null_recovered_parameters.rdata")
ADHD_params =pars

ADHD_alpha <- apply(ADHD_params$alpha, 2,mean)
ADHD_beta <- apply(ADHD_params$beta, 2,mean)
ADHD_lambda <- apply(ADHD_params$lambda, 2,mean)


t.test(typical_alpha, ADHD_alpha)
t.test(typical_beta, ADHD_beta)
t.test(typical_lambda, ADHD_lambda)
