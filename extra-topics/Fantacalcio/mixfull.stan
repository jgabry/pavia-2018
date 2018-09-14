data{
  // Dimensions
  int<lower=0> N;  // Number of players
  int<lower=1> J;  // Number of positions
  int<lower=1> T;  // Number of matches
  int<lower=1> K;  // Number of team clusters
  int<lower=1> D;  // Number of mixture components

  // Variables
  vector[T] y[N];                             // Outcome
  int<lower=1,upper=J> position[N];           // Position
  int<lower=1,upper=K> team[N];               // Team cluster
  int<lower=1,upper=K> opp_team[N, T-1];      // Opponent team cluster for each game
  int<lower=0,upper=1> home[N, T-1];          // Home/Away variable (0=Away, 1=Home)
  vector[N] price_std;                        // Initial price for every player
  real avg_rating[N, T-1];                    // Lagged average ratings
  
  // Out-of-sample stuff
  int<lower=1> T_twiddle;                                 // Number of games
  int<lower=0,upper=1> home_twiddle[N, T_twiddle];        // Home/Away
  int<lower=1,upper=K> opp_team_twiddle[N, T_twiddle];    // Opponent team cluster
}
parameters {
  // For non-centered parameterizations
  vector[N] alpha_raw;                  // Player intercepts
  vector[K] gamma_raw;                  // Team-cluster incercepts
  vector[K] beta_raw;                   // Opponent team-cluster intercepts
  vector[J] rho_raw;                    // Position intercepts
  
  vector[J] lambda;                     // Coefs on lagged average rating
  vector[J] delta;                      // Coefs on standardized price
  real alpha0;                          // Global intercept
  real theta;                           // Coef on home/away indicator
  
  // Scale parameters
  real<lower=0> sigma_y;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_gamma;
  real<lower=0> sigma_rho;
  
  // Parameters in logit submodel
  vector[J] zeta;
  real pzero;
}
transformed parameters{
  // Non-centered parameterizations
  vector[N] alpha = alpha_raw * sigma_alpha;
  vector[K] beta = beta_raw * sigma_beta;
  vector[K] gamma = gamma_raw * sigma_gamma;
  vector[J] rho = rho_raw * sigma_rho;
  
  vector[T] eta[N];
  for (n in 1:N) {
    eta[n, 1] = 0; // just needs some value
    for (l in 2:T) {
      eta[n,l] =
         alpha0
         + alpha[n]
         + delta[position[n]] * price_std[n]
         + (gamma[team[n]] + beta[opp_team[n, l-1]])
         + rho[position[n]]
         + theta * home[n, l-1]
         + lambda[position[n]] * avg_rating[n, l-1];
    }
  }
}
model{
  // Mixture
  for (n in 1:N) {
    for (l in 2:T) {
      real pi_eta = pzero + zeta[position[n]] * avg_rating[n, l-1];
      target += 
        log_mix(
          inv_logit(pi_eta),
          normal_lpdf(y[n,l] | eta[n,l], sigma_y),
          normal_lpdf(y[n,l] | 0, 0.1) // 0
        );
    }
  }

  // Log-priors
  target += normal_lpdf(alpha0 | 0, 5);
  target += 
    normal_lpdf(alpha_raw | 0, 1)
    + normal_lpdf(sigma_alpha | 0, 2.5);
  target += 
    normal_lpdf(rho_raw | 0, 1)
    + normal_lpdf(sigma_rho | 0, 2.5);
  target += 
    normal_lpdf(beta_raw | 0, 1)
    + normal_lpdf(sigma_beta | 0, 2.5);
  target += 
    normal_lpdf(gamma_raw | 0, 1)
    + normal_lpdf(sigma_gamma | 0, 2.5);
  
  target += normal_lpdf(theta | 0, 2.5);
  target += normal_lpdf(lambda | 0, 1);
  target += normal_lpdf(delta | 0, 5);
  target += cauchy_lpdf(sigma_y | 0, 5);
  
  target += normal_lpdf(zeta | 0, 1);
  target += normal_lpdf(pzero | 0, 2.5);
}
generated quantities{
  vector[T] y_rep[N];                     // In-sample replications/predictions
  vector[T-1] pi_eta_rep[N];
  int<lower=0, upper=1> V[N, T-1];

  vector[T_twiddle] eta_twiddle[N];
  real y_twiddle[N, T_twiddle];           // Out-of-sample predictions
  vector[T_twiddle] pi_eta_twiddle[N];
  int<lower=0, upper=1> V_twiddle[N, T_twiddle];
  real avg_rating_twiddle[N, T_twiddle];

  avg_rating_twiddle[,1] = avg_rating[,T-1];
  y_twiddle[,1] = y[,T];
  y_rep[,1] = y[,1];

  for (n in 1:N) {
    for (l in 2:T) {
      pi_eta_rep[n, l-1] = pzero + zeta[position[n]] * avg_rating[n,l-1];
      V[n, l-1] = bernoulli_logit_rng(pi_eta_rep[n,l-1]);
      y_rep[n,l] = (V[n, l-1] == 1) ? normal_rng(eta[n,l], sigma_y) : 0;
    }
  }

  for (n in 1:N) {
    pi_eta_twiddle[n,1] = 
      pzero+ zeta[position[n]] * avg_rating_twiddle[n,1];
      V_twiddle[n,1] = bernoulli_logit_rng(pi_eta_twiddle[n,1]);
        eta_twiddle[n,1] = 
                          alpha0 
                          + alpha[n]
                          + (gamma[team[n]]+beta[opp_team_twiddle[n,1]])
                          + delta[position[n]]*price_std[n]
                          + theta * home_twiddle[n,1]+rho[position[n]]
                          + lambda[position[n]] * avg_rating[n,T-1];
  }

  for (n in 1:N) {
    for (l in 2:T_twiddle) {
      avg_rating_twiddle[n,l] = 
        sum(y_twiddle[n,1:(l-1)]) / size(y_twiddle[n,1:(l-1)]);
      pi_eta_twiddle[n,l] = 
        pzero+ zeta[position[n]] * avg_rating_twiddle[n,l];
        V_twiddle[n,l] = bernoulli_logit_rng(pi_eta_twiddle[n,l]);
              eta_twiddle[n,l] = 
                                alpha0
                                + alpha[n]
                                +(beta[opp_team_twiddle[n,l]]+gamma[team[n]])
                                + delta[position[n]]*price_std[n]
                                + theta * home_twiddle[n,l]+rho[position[n]]
                                + lambda[position[n]] * avg_rating_twiddle[n,l-1];

      y_twiddle[n,l] = 
        (V_twiddle[n,l] == 1) ? normal_rng(eta_twiddle[n,l], sigma_y  ) : 0;
    }
  }

}
