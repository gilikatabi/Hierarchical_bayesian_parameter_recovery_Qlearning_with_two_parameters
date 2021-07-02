data {
  
  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;           //number of subjects
  int<lower = 1> Ntrials;             //number of trials without missing data, typically the maximum of trials per subject
  int<lower = 1> Ntrials_per_subject[Nsubjects];  //number of trials left for each subject after data omission

  //Behavioral data:
  //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> action[Nsubjects,Ntrials];        //index of which arm was pulled coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull

}
transformed data{
    int<lower = 1> Nparameters; //number of parameters
    int<lower = 2> Narms;       //number of overall alternatives

    Nparameters=2;
    Narms      =4;
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
//declare variables and parameters
      //population level
      matrix[Nparameters,Nparameters] sigma_matrix;
      
      //individuals level
      real alpha[Nsubjects];
      real beta[Nsubjects];


      //additional variabels
      vector[Nsubjects] log_like;
      vector[Ntrials]   log_like_individual;
      vector<lower=0, upper=1>[Narms] Qcard;

//preassignment
      //Scale matrix for individual level parameters
      //specifically we are intrested in getting a sigma matrix we is the covariance matrix that is used to sample
      //the model parameters from a multivariate normal in the "model" block for stan
      //here, we take tau (variance vector) and L_Omega (Lower triangle of correlation matrix)
      //and convert them to the sigma_matrix (covariance matrix)
      sigma_matrix = diag_pre_multiply(tau, (L_Omega*L_Omega')); //L_Omega*L_omega' give us Omega (the corr matrix). 
      sigma_matrix = diag_post_multiply(sigma_matrix, tau);     // diag(tau)*omega*diag(tau) gives us sigma_matirx (the cov matrix)
          
      log_like=rep_vector(0,Nsubjects);
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  for (subject in 1:Nsubjects){
        //assiging subject level model parameters
        //use inv_logit for parameters that should be between 0 and 1
        //use exp for parameters that should be positive
        //leave without transformation for the reast of model parameters
        alpha[subject]              = inv_logit(auxiliary_parameters[subject][1]);
        beta[subject]               = exp(auxiliary_parameters[subject][2]);
        
        //pre-assignment per individual
        Qcard   =rep_vector(0,Narms);
        log_like_individual=rep_vector(0,Ntrials);

        //trial by trial loop
         for (trial in 1:Ntrials_per_subject[subject]){
            //liklihood function (softmax)
            log_like_individual[trial]=log_softmax(Qcard*beta[subject])[action[subject,trial]];

            //Qvalues update
            Qcard[action[subject,trial]] += alpha[subject] * (reward[subject,trial] - Qcard[action[subject,trial]]);
            
        } 
   log_like[subject]=sum(log_like_individual);

  }
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
model {
  
  // population level priors (hyper-parameters)
  mu  ~ normal(0, 5);             // mu is a vector 1xNparameters with the population mean (i.e., location) for each model parameter
  tau ~ cauchy(0, 1);             //tau is the hyperparameters variance vector
  L_Omega ~ lkj_corr_cholesky(2); //L_omega is the lower triangle of the correlations. Setting the lkj prior to 2 means the off-diagonals are priored to be near zero

  // indvidual level priors (subject parameters)
  auxiliary_parameters ~ multi_normal(mu, sigma_matrix);

  target += sum(log_like);

}

