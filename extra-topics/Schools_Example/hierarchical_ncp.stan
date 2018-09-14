// Non-centered parameterization

data {
  int<lower=1> J;  // Number of schools
  vector[J] y;     // Est. treatment effect in each school 
  vector<lower=0>[J] sigma; // Est. std err of y
}
parameters {
  vector[J] theta_raw;  // School-specifc parameters
  real mu;    // Population mean
  real<lower=0> tau;  // Population SD
}
transformed parameters {
  vector[J] theta = mu + theta_raw * tau;
}
model {
  y ~ normal(theta, sigma);
  theta_raw ~ normal(0, 1);  // theta ~ normal(mu, tau);
  mu ~ normal(0, 10);
  tau ~ normal(0, 10);
}
