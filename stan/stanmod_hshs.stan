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
  
  //indices linking predictors to groups (1-based)
  array[J] int<lower=1, upper=K> group_index;

  //horseshoe groups
  real<lower=0> grp_scale_global;
  real<lower=1> grp_nu_global;
  real<lower=1> grp_nu_local;
  real<lower=0> grp_slab_scale;
  real<lower=0> grp_slab_df;
  
  //horseshoe observations
  real<lower=0> obs_scale_global;
  real<lower=1> obs_nu_global;
  real<lower=1> obs_nu_local;
  real<lower=0> obs_slab_scale;
  real<lower=0> obs_slab_df;

  //data
  array[N] int<lower=0, upper=1> Y;   //outcome
  matrix[N,K] groups;
  matrix[N,J] preds;
  matrix[N,C] confs;
}
parameters {
  real alpha;
  vector[C] beta;

  //horsehoe for groups (gamma)
  vector[K] gammaaux;
  real<lower=0> grp_aux1_global;
  real<lower=0> grp_aux2_global;
  vector<lower=0>[K] grp_aux1_local;
  vector<lower=0>[K] grp_aux2_local;
  real<lower=0> grp_caux;

  //horsehoe for obs (delta)
  vector[J] deltaaux;
  real<lower=0> obs_aux1_global;
  real<lower=0> obs_aux2_global;
  vector<lower=0>[J] obs_aux1_local;
  vector<lower=0>[J] obs_aux2_local;
  real<lower=0> obs_caux;
}
transformed parameters {
  //horseshoe groups (gamma)
  real<lower=0> grp_tau;
  vector<lower=0>[K] grp_lambda;
  vector<lower=0>[K] grp_lambda_tilde;
  real<lower=0> grp_c;   //slab scale
  grp_lambda = grp_aux1_local .* sqrt(grp_aux2_local);
  grp_tau = grp_aux1_global * sqrt(grp_aux2_global) * grp_scale_global; 
  grp_c = grp_slab_scale * sqrt(grp_caux);
  grp_lambda_tilde = sqrt(grp_c^2 * square(grp_lambda) ./ 
                            (grp_c^2 + grp_tau ^2* square(grp_lambda)));
  vector[K] gamma;  //regression coefficients groups random effect
  gamma = gammaaux .* grp_lambda_tilde * grp_tau;
  
  //horseshoe observations (delta)
  real<lower=0> obs_tau;
  vector<lower=0>[J] obs_lambda;
  vector<lower=0>[J] obs_lambda_tilde;
  real<lower=0> obs_c;   //slab scale
  obs_lambda = obs_aux1_local .* sqrt(obs_aux2_local);
  obs_tau = obs_aux1_global * sqrt(obs_aux2_global) * obs_scale_global; 
  obs_c = obs_slab_scale * sqrt(obs_caux);
  obs_lambda_tilde = sqrt(obs_c^2 * square(obs_lambda) ./ 
                            (obs_c^2 + obs_tau ^2* square(obs_lambda)));
  vector[J] delta;  //regression coefficients groups random effect
  delta = deltaaux .* obs_lambda_tilde * obs_tau;
}
model {
  //modelled parameters
  alpha ~ normal(0, 5);
  for (i in 1:C) {
    beta[i] ~ normal(0, 2.5);  
  }
  gammaaux ~ std_normal();
  deltaaux ~ std_normal();

  //horseshoe groups
  grp_aux1_local ~ normal(0, 1);
  grp_aux2_local ~ inv_gamma(0.5 * grp_nu_local, 0.5 * grp_nu_local);
  grp_aux1_global ~ normal(0, 1);
  grp_aux2_global ~ inv_gamma(0.5  * grp_nu_global, 0.5 * grp_nu_global);
  grp_caux ~ inv_gamma(0.5* grp_slab_df , 0.5 * grp_slab_df);
  
  //horseshoe obs
  obs_aux1_local ~ normal(0, 1);
  obs_aux2_local ~ inv_gamma(0.5 * obs_nu_local, 0.5 * obs_nu_local);
  obs_aux1_global ~ normal(0, 1);
  obs_aux2_global ~ inv_gamma(0.5  * obs_nu_global, 0.5 * obs_nu_global);
  obs_caux ~ inv_gamma(0.5* obs_slab_df , 0.5 * obs_slab_df);

  //likelihood
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