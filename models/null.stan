data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                         //number of subjects
  int<lower = 1> Nblocks;                           //number of blocks
  int<lower = 1> Ntrials;                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies.
  int<lower = 1> Ntrials_per_subject[Nsubjects];    //number of trials left for each subject after data omission


  //Behavioral data:
  int<lower = 0> choice1[Nsubjects,Ntrials];           
  int<lower = 0> choice2[Nsubjects,Ntrials];           
  int<lower = 0> choice3[Nsubjects,Ntrials];           
  int<lower = 0> state1[Nsubjects,Ntrials];           
  int<lower = 0> state2[Nsubjects,Ntrials];           
  int<lower = 0> state3[Nsubjects,Ntrials];           
  int<lower = 0> reward[Nsubjects,Ntrials];           	  //reward outcome coded 0 or 1
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials]; //coding whether a trial is the first in a block to allow for Qval rest
 
}

transformed data{
  int<lower = 1> Nparameters=3; //number of parameters in the model
}



parameters {

  //population level parameters 
  vector         [Nparameters] population_locations; //a vector with the location  for learning rate and noise parameters
  vector<lower=0>[Nparameters] population_scales;    //a vector with scaling for learning rate and noise parameters
  
  //individuals level parameters
  vector          [Nsubjects] alpha_random_effect;  //random effect for learning rate (alpha is declared in transformed parameters)
  vector          [Nsubjects] beta_random_effect;   //noise parameter
  vector          [Nsubjects] lambda_random_effect; //random effect for eligibility factor
}



transformed parameters {
  vector     	[Nsubjects] alpha; //learning rate parameter
  vector     	[Nsubjects] beta ; //noise parameter
  vector     	[Nsubjects] lambda; //eligibility parameter


  for (subject in 1:Nsubjects) {
    alpha[subject]   = inv_logit(population_locations[1]  + population_scales[1]  * alpha_random_effect[subject]);
	  beta [subject]   =          (population_locations[2]  + population_scales[2]  * beta_random_effect[subject]);
    lambda [subject] = inv_logit(population_locations[3]  + population_scales[3]  * lambda_random_effect[subject]);

  }
}



model {
  // population level priors
  population_locations   ~ normal(0,3);
  population_scales  	~ cauchy(0,3);
 

  //individual level priors
  alpha_random_effect ~ std_normal();  // similar to ~ normal(0,1)
  beta_random_effect  ~ std_normal();  // similar to ~ normal(0,1)
  lambda_random_effect  ~ std_normal();  // similar to ~ normal(0,1)

 
  //Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    int ch1; 
  	int ch2; 
  	int ch3; 
  	int st1; 
  	int st2; 
  	int st3; 
    real PE1;
    real PE2;
    real PE3;
	  real Qval[2,4,3]; //number of arms x number of states x number of stages (for a tree of three stages the last stage will have 4 states)
    vector [2]y;
    vector [Ntrials_per_subject[subject]]Qdiff1;
    vector [Ntrials_per_subject[subject]]Qdiff2;
    vector [Ntrials_per_subject[subject]]Qdiff3;


  	for (trial in 1:Ntrials_per_subject[subject]){
   	 
    		//reset Qvalues in the start of each block
    		if (first_trial_in_block[subject,trial] == 1) {
                  	  	Qval= rep_array(0, 2, 4, 3);
    		}

        //allocate choices
        ch1=choice1[subject,trial];
        ch2=choice2[subject,trial];
        ch3=choice3[subject,trial];
        st1=1;
        st2=state2[subject,trial];
        st3=state3[subject,trial];
        	
        //calculate Qvalue in favor of empirical choice

        //Qdiff1[trial]=Qval[1,st1,1]-Qval[2,st1,1];
        //Qdiff2[trial]=Qval[1,st2,2]-Qval[2,st2,2];
        //Qdiff3[trial]=Qval[1,st3,3]-Qval[2,st3,3];
        y = to_vector(Qval[,st1,1]);
        Qdiff1[trial] = softmax(beta[subject]*y)[ch1];
        
        y = to_vector(Qval[,st2,2]);
        Qdiff2[trial] = softmax(beta[subject]*y)[ch2];
        
        y = to_vector(Qval[,st3,3]);
        Qdiff3[trial] = softmax(beta[subject]*y)[ch3];
        
        //PE
        PE1=Qval[ch2,st2,2] - Qval[ch1,st1,1];
        PE2=Qval[ch3,st3,3] - Qval[ch2,st2,2];
        PE3=reward[subject,trial]  - Qval[ch3,st3,3];
        
        Qval[ch1,st1,1]=Qval[ch1,st1,1]+alpha[subject]*PE1 + alpha[subject]*lambda[subject]*PE2 + alpha[subject]*(pow(lambda[subject],2))*PE3;
        Qval[ch2,st2,2]=Qval[ch2,st2,2]+alpha[subject]*PE2 + alpha[subject]*lambda[subject]*PE3;
        Qval[ch3,st3,3]=Qval[ch3,st3,3]+alpha[subject]*PE3;
      }
 	    //softmax (model likelihood) 
   	  //target +=log_softmax(beta[subject] * Qdiff1);
   		//target +=log_softmax(beta[subject] * Qdiff2);
   	  //target +=log_softmax(beta[subject] * Qdiff3);
   	  
   	  target +=log(Qdiff1);
   		target +=log(Qdiff2);
   	  target +=log(Qdiff3);
	}
}

