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
  
  // 'exposure'
  vector[N] log_sq_foot;  
  
  // building-level data
  int<lower=1> K;
  int<lower=1> J;
  int<lower=1, upper=J> building_idx[N];
  matrix[J,K] building_data;
  
  // month info
  // ... Number of months (M) and month indexes (mo_idx)
}
parameters {
  real<lower=0> inv_phi;   // 1/phi (easier to think about prior for 1/phi instead of phi)
  
  vector[J] mu_raw;        // N(0,1) params for non-centered param of building-specific intercepts
  real<lower=0> sigma_mu;  // sd of buildings-specific intercepts
  real alpha;              // 'global' intercept
  vector[K] zeta;          // coefficients on building-level predictors in model for mu
  
  vector[J] kappa_raw;       // N(0,1) params for non-centered param of building-specific slopes
  real<lower=0> sigma_kappa; // sd of buildings-specific slopes
  real beta;                 // 'global' slope on traps variable
  vector[K] gamma;           // coefficients on building-level predictors in model for kappa
  
  
  real<lower=0,upper=1> rho_raw;  // used to construct rho, the AR(1) coefficient
  // ... N(0,1) params for non-centered param of AR(1) process (mo_raw)
  // ... sd of month-specific parameters (sigma_mo)
}
transformed parameters {
  real phi = inv(inv_phi);
  
  // non-centered parameterization of building-specific intercepts and slopes
  vector[J] mu = alpha + building_data * zeta + sigma_mu * mu_raw;
  vector[J] kappa = beta + building_data * gamma + sigma_kappa * kappa_raw;
  
  // AR(1) process priors
  real rho = 2.0 * rho_raw - 1.0;
  vector[M] mo = sigma_mo * mo_raw;
  mo[1] /= sqrt(1 - rho^2);   //   mo[1] = mo[1]/sqrt(1-rho^2)
  
  // loop over the rest of the mo vector to add in the dependence on previous month
  // ...
}
model {
  inv_phi ~ normal(0, 1);
  
  kappa_raw ~ normal(0,1) ;
  sigma_kappa ~ normal(0, 1);
  beta ~ normal(-0.25, 1);
  gamma ~ normal(0, 1);
  
  mu_raw ~ normal(0,1) ;
  sigma_mu ~ normal(0, 1);
  alpha ~ normal(log(4), 1);
  zeta ~ normal(0, 1);
  
  rho_raw ~ beta(10, 5);
  // ... prior for mo_raw
  // ... prior for sigma_mo
  
  complaints ~ neg_binomial_2_log(mu[building_idx] 
                                 + kappa[building_idx] .* traps 
                                 // ... add month-specific parameters
                                 + log_sq_foot, 
                                 phi);
}
generated quantities {
  int y_rep[N];
  for (n in 1:N) {
    real eta_n = 
      mu[building_idx[n]] 
      + kappa[building_idx[n]] * traps[n] 
      // ... add month-specific parameter
      + log_sq_foot[n];
      
    y_rep[n] = neg_binomial_2_log_safe_rng(eta_n, phi);
  }
}
