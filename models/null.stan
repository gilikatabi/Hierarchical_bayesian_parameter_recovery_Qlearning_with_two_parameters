data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;                                           //number of blocks
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Nraffle;                                           //number of offers per trial



  //Behavioral data:
  //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> action[Nsubjects,Ntrials];               //index of which arm was pulled coded 1/2/3/4
  int<lower = 0> reward[Nsubjects,Ntrials];               //reward outcome coded 0 or 1
  int<lower = 0> offer1[Nsubjects,Ntrials];               //which bandit was included in the first offer (e.g., left side)
  int<lower = 0> offer2[Nsubjects,Ntrials];               //which bandit was included in the second offer (e.g., right side)
  int<lower = 0> selected_offer[Nsubjects,Ntrials];       // which offer was selected by the individual
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials]; //coding whether a trial is the first in a block to allow for Qval rest
  
}

transformed data{
  int<lower = 1> Nparameters=2; //number of parameters in the model
  
}

parameters {

  //population level parameters 
  vector         [Nparameters]          population_locations; //a vector with the location parameters for learning rate and noise
  vector<lower=0>[Nparameters]          population_scales;    //a vector with scaling parameters for learning rate and noise
  
  //individuals level parameters
  vector          [Nsubjects]   alpha_random_effect; //random effect for learning rate (alpha is declared in transformed parameters)
  vector<lower=0> [Nsubjects] beta;                  //noise parameter
}



transformed parameters {
  real  alpha[Nsubjects]; //learning rate parameter

  //transform from unbounded scale to a natural scale of 0 to 1
  for (subject in 1:Nsubjects) {
    alpha[subject]   = inv_logit(population_locations[1]  + population_scales[1]  * alpha_random_effect[subject]);//
  }
}


model {
  // population level priors 
  population_locations   ~ normal(0,2);
  population_scales      ~ cauchy(0,2);
  

  //indvidual level priors
  alpha_random_effect ~ normal(0,1); 
  beta  ~ lognormal(population_locations[1],population_scales[1]);
  
  
  
  //Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector[Narms]   Qcard; //Qvalues for all bandits in the task
    vector[Nraffle] Qoffer;//Qvalues for the bandits that were offered each trial 
    
 
      for (trial in 1:Ntrials_per_subject[subject]){
        
        //reset Qvalues in the start of each block
        if (first_trial_in_block[subject,trial] == 1) {
                        Qcard= rep_vector(0.5, Narms);
        }
        
        //allocate Qvalues according to offer
          Qoffer[1]=Qcard[offer1[subject,trial]];
          Qoffer[2]=Qcard[offer2[subject,trial]];

        //liklihood function
         target +=log_softmax(beta[subject] * Qoffer)[selected_offer[subject, trial]];

        //Qvalues update
        Qcard[action[subject,trial]] += alpha[subject] * (reward[subject,trial] - Qcard[action[subject,trial]]);
      } 
    }
}
