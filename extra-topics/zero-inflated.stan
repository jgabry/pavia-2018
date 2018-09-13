data { 
  int<lower=0> N; 
  int<lower=0> y[N]; 
} 
parameters { 
 real<lower=0, upper=1> theta; // Pr(zero) without Poisson
 real<lower=0> lambda;         // Poisson rate
} 
model { 
  for (n in 1:N) { 
    
    if (y[n] == 0) {
      target += log_sum_exp(
        log(theta),  // 0 but doesn't come from Poisson
        log1m(theta) + poisson_lpmf(0 | lambda)  // 0 from Poisson
       );
    } else {
       target += log1m(theta) + poisson_lpmf(y[n] | lambda);
     } 
     
  }
}
