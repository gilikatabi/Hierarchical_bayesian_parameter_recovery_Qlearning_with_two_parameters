data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;           //number of subjects
  int<lower = 1> Ntrials;             //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];  //number of trials left for each subject after data omission
  int<lower = 2> Narms;       //number of overall alternatives

  //Behavioral data:
  //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> action[Nsubjects,Ntrials];        //index of which arm was pulled coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  
}

transformed data{
  int<lower = 1> Nparameters=2; //number of parameters
  vector[Narms] Qvalue_initial;     // initial values for Qvalues (defined here to aviod doing this many times across iterations)
  Qvalue_initial = rep_vector(0.0, Narms);
}

parameters {
//population level parameters 
  vector[Nparameters] mu;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] tau;          //vector of random effects variance for each model parameter
  cholesky_factor_corr[Nparameters] L_Omega; //lower triangle of a correlation matrix to be used for the random effect of the model parameters
  
  //subject level parameters
  vector[Nparameters] auxiliary_parameters[Nsubjects]; 

}


transformed parameters {
    //population level
    matrix[Nparameters,Nparameters] sigma_matrix;

    //individuals level
    real alpha[Nsubjects];
    real beta[Nsubjects];
    
    sigma_matrix = diag_pre_multiply(tau, (L_Omega*L_Omega')); //L_Omega*L_omega' give us Omega (the corr matrix). 
    sigma_matrix = diag_post_multiply(sigma_matrix, tau);     // diag(tau)*omega*diag(tau) gives us sigma_matirx (the cov matrix)
      
  for (subject in 1:Nsubjects) {
        alpha[subject]              = inv_logit(auxiliary_parameters[subject][1]);
        beta[subject]               = exp(auxiliary_parameters[subject][2]);
  }

}



model {

  // population level priors (hyper-parameters)
  mu  ~ normal(0, 5);             // mu is a vector 1xNparameters with the population mean (i.e., location) for each model parameter
  tau ~ cauchy(0, 1);             //tau is the hyperparameters variance vector
  L_Omega ~ lkj_corr_cholesky(2); //L_omega is the lower triangle of the correlations. Setting the lkj prior to 2 means the off-diagonals are priored to be near zero

  // indvidual level priors (subject parameters)
  auxiliary_parameters ~ multi_normal(mu, sigma_matrix);


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector[Narms] Qcard; 
    
    Qcard=Qvalue_initial;
         
      for (trial in 1:Ntrials_per_subject[subject]){
            
        //liklihood function 
        action[subject, trial] ~ categorical_logit(beta[subject] * Qcard);
            
        //Qvalues update
        Qcard[action[subject,trial]] += alpha[subject] * (reward[subject,trial] - Qcard[action[subject,trial]]);

      } 
  }
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}

generated quantities {
  //define parameters that will be saved in the final output
  real<lower=0, upper=1>  mu_alpha;
  real<lower=0, upper=10> mu_beta;
  real log_lik[Nsubjects];
  real y_pred[Nsubjects, Ntrials];

  // Set all posterior predictions to -1 (avoids NULL values)
  for (i in 1:Nsubjects) {
    for (t in 1:Ntrials) {
      y_pred[i, t] = -1;
    }
  }

mu_alpha=inv_logit(mu[1]);
mu_beta=exp(mu[2]);

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial (placed in generetaed quantities block to save time and memory)

  { // 
    for (subject in 1:Nsubjects) {
        vector[Narms] Qcard; 
        Qcard=Qvalue_initial;

        log_lik[subject] = 0;

        for (trial in 1:Ntrials_per_subject[subject]){

        // compute log likelihood of current trial
        log_lik[subject] += categorical_logit_lpmf(action[subject, trial] | beta[subject] * Qcard);

        // generate posterior prediction for current trial
        y_pred[subject, trial] = categorical_rng(softmax(beta[subject] * Qcard));

 
        //Qvalues update
        Qcard[action[subject,trial]] += alpha[subject] * (reward[subject,trial] - Qcard[action[subject,trial]]);      
        }
    }
  }
}
