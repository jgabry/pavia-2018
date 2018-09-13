// Two-component mixture using log_mix function shortcut
data { 
  int<lower=1> N;         // number of data points 
  real y[N];              // observations 
} 
parameters { 
  real theta;             // mixing proportion 
  real mu[2];             // locations of mixture components 
  real<lower=0> sigma[2]; // scales of mixture components 
} 
model { 
  
  // Log-likelihood
  for (n in 1:N){
    target += log_mix(
      theta,
      normal_lpdf(y[n] | mu[1], sigma[1]), 
      normal_lpdf(y[n] | mu[2], sigma[2])
   );
  }                          
  
  // Priors 
  theta ~ beta(2, 2);  
  sigma ~ cauchy(0, 2.5); 
  mu ~ normal(0, 10); 
}

