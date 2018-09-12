functions {
  /*
    * Alternative to neg_binomial_2_log_rng() that 
  * avoids potential numerical problems during warmup
  */
    int neg_binomial_2_log_safe_rng(real eta, real phi) {
      real gamma_rate = gamma_rng(phi, phi / exp(eta));
      if (gamma_rate >= exp(20.79))
        return -9;
      
      return poisson_rng(gamma_rate);
    }
}

data {
  int<lower=1> N;
  int<lower=0> complaints[N];
  vector<lower=0>[N] traps;
  vector[N] log_sq_foot;
  
  int<lower=1> J;  // number of buildings
  int<lower=1> K;  // number of building variables
  matrix[J,K] building_data;
  int<lower=1,upper=J> building_idx[N];
}
parameters {
  real<lower=0> inv_phi;
  real beta;  // coefficient on traps
  real alpha;  
  vector[J] mu;
  real<lower=0> sigma_mu;
  vector[K] zeta;  // building variable coeffients
}
transformed parameters {
  real phi = inv(inv_phi); // 1/inv_phi
}
model {
  mu ~ normal(alpha + building_data * zeta, sigma_mu);
  
  alpha ~ normal(log(4), 1);
  beta ~ normal(-0.25, 1);
  inv_phi ~ normal(0, 1);
  zeta ~ normal(0, 1);  
  // for (k in 1:K) zeta[k] ~ normal(0,1);
  sigma_mu ~ normal(0, 1);
  
  complaints ~ neg_binomial_2_log(
    mu[building_idx] +
    beta * traps + log_sq_foot, 
    phi);
}
generated quantities {
  int y_rep[N];
  for (n in 1:N) {
    real eta = mu[building_idx[n]] + beta * traps[n] + 
      log_sq_foot[n];
    y_rep[n] = neg_binomial_2_log_safe_rng(eta, phi);
  }
}



