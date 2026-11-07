// These documents contain the cmdstan hierarchical models in a logistic regression.
// These models implement Gaussian and horseshoe priors for the random effects.
// The Gaussian prior implements a ridge; the horseshoe a sparsity prior.
//
// A grouping level and a lower level are implemented:
// beta: the fixed effects
// gamma: the group effects
// delta: the observations effects
// In this application, groups are the initial letters of the ICD-10-GM codes
// (first-character level) and observations are the three-character categories.
//
// gsgs:   Gaussian prior for both groups and observations
// hsgs:   Horseshoe for groups, Gaussian for observations
// hst:    Horseshoe for groups, Student's t for observations
// hshs:   Horseshoe for both groups and observations

data {
  int<lower=1> N;  //number of obs
  int<lower=1> K;  //number of groups (initial letters)
  int<lower=1> J;  //number of predictors (three-character categories)
  int<lower=1> C;  //number of confounds
  int<lower=0> deltaDf;  //df of delta ~ t
  
  //indices linking predictors to groups (1-based)
  array[J] int<lower=1, upper=K> group_index;

  //horseshoe
  real<lower=0> scale_global;
  real<lower=1> nu_global;
  real<lower=1> nu_local;
  real<lower=0> slab_scale;
  real<lower=0> slab_df;
  
  //data
  array[N] int<lower=0, upper=1> Y;   //outcome
  matrix[N,K] groups;
  matrix[N,J] preds;
  matrix[N,C] confs;
}
parameters {
  real alpha;
  vector[C] beta;
  //vector[K] gamma; in transformed parameters section
  vector[J] deltaaux;
  
  //hyperparameters
  real<lower=0> sigmaDelta;

  //horsehoe for gamma  
  vector[K] z;
  real<lower=0> aux1_global;
  real<lower=0> aux2_global;
  vector<lower=0>[K] aux1_local;
  vector<lower=0>[K] aux2_local;
  real<lower=0> caux;
}
transformed parameters {
  real<lower=0> tau;
  vector<lower=0>[K] lambda;
  vector<lower=0>[K] lambda_tilde;
  real<lower=0> c;   //slab scale
  lambda = aux1_local .* sqrt(aux2_local);
  tau = aux1_global * sqrt(aux2_global) * scale_global; 
  c = slab_scale * sqrt(caux);
  lambda_tilde = sqrt (c^2 * square(lambda) ./ (c^2 + tau ^2* square(lambda)));
  vector[K] gamma;  //regression coefficients groups random effect
  gamma = z .* lambda_tilde * tau;
  
  vector[J] delta;
  delta = deltaaux * sigmaDelta;
}
model {
  //modelled parameters
  alpha ~ normal(0, 5);
  for (i in 1:C) {
    beta[i] ~ normal(0, 2.5);  
  }
  z ~ std_normal();
  deltaaux ~ student_t(deltaDf, 0, 1);
  //horseshoe
  aux1_local ~ normal(0, 1);
  aux2_local ~ inv_gamma(0.5 * nu_local, 0.5 * nu_local);
  aux1_global ~ normal(0, 1);
  aux2_global ~ inv_gamma(0.5  * nu_global, 0.5 * nu_global);
  caux ~ inv_gamma (0.5* slab_df , 0.5 * slab_df);
  
  //other priors
  sigmaDelta ~ cauchy(0.01, 1);
  Y ~ bernoulli_logit(alpha + confs * beta + groups * gamma + preds * delta);
}
generated quantities {
  array[J] real total_effect;  //total higher + lower levels effects
  array[J] real total_OR;      //total odds ratios
  vector[N] p_hat;   //predicted probability per observation



  p_hat = inv_logit(alpha + confs * beta + groups * gamma + preds * delta);

  

  for (j in 1:J) {
    total_effect[j] = delta[j] + gamma[group_index[j]];
    total_OR[j] = exp(total_effect[j]);
  }
}
