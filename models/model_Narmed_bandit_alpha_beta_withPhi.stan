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
// Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  //population level parameters 
  vector[Nparameters] mu_aux;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] sigma_aux;          //vector of random effects variance for each model parameter
  
//individuals level
  vector[Nsubjects] alpha_individal_aux;
  vector[Nsubjects] beta_indvidial_aux;
}


transformed parameters {
//declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects]  alpha;
  vector<lower=0, upper=10>[Nsubjects] beta;
    
  for (subject in 1:Nsubjects) {
    alpha[subject]   = Phi_approx(mu_aux[1]  + sigma_aux[1]  * alpha_individal_aux[subject]);
    beta[subject] = Phi_approx(mu_aux[2] + sigma_aux[2] * beta_indvidial_aux[subject]) * 10;
  }

}



model {
  
  // population level priors (hyper-parameters)
  mu_aux  ~ normal(0, 1);            
  sigma_aux ~ normal(0, 0.2);        

  // indvidual level priors (subjects' parameters)
  alpha_individal_aux~ normal(0, 1);
  beta_indvidial_aux ~ normal(0, 1);
 

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
mu_alpha=Phi_approx(mu_aux[1]);
mu_beta=Phi_approx(mu_aux[2])*10;

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
