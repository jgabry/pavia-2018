data{
   // Dimensions
  int<lower=0> N;  // Number of players
  int<lower=1> J;  // Number of positions
  int<lower=1> T;  // Number of matches
  int<lower=1> K;  // Number of team clusters

  // Variables
  matrix[N,T] y;                            // Outcome
  int<lower=1,upper=J> position[N];         // Position
  int<lower=1,upper=K> team[N];             // Team cluster
  int<lower=1,upper=K> opp_team[N, T-1];    // Opponent team cluster for each game
  matrix<lower=0,upper=1>[N, T-1] home;     // Home/Away variable (0=Away, 1=Home)
  vector[N] price_std;                      // Initial standardized price
  real avg_rating[N, T-1];                  // Lagged average rating
  
  // Out-of-sample stuff
  int<lower=1> T_twiddle;                               // Number of games
  int<lower=0,upper=1> home_twiddle[N, T_twiddle];      // Home/Away
  int<lower=1,upper=K> opp_team_twiddle[N, T_twiddle];  // Opponent team cluster
}
parameters{
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
}
transformed parameters {
  // Non-centered parameterizations
  vector[N] alpha = alpha_raw * sigma_alpha;
  vector[K] beta = beta_raw * sigma_beta;
  vector[K] gamma = gamma_raw * sigma_gamma;
  vector[J] rho = rho_raw * sigma_rho;
}
model{
  matrix[N, T] eta;
  for (n in 1:N) {  
    for (l in 2:T) {
     eta[n,l] = 
        alpha0
        + alpha[n]
        + gamma[team[n]] 
        + beta[opp_team[n, l-1]]
        + rho[position[n]] 
        + delta[position[n]] * price_std[n]
        + lambda[position[n]] * avg_rating[n, l-1]
        + theta * home[n, l-1];
    }
  }
  target += normal_lpdf(to_vector(y[, 2:T]) | to_vector(eta[, 2:T]), sigma_y);
  
  
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
}
generated quantities{  
  matrix[N, T] y_rep;                     // Replicated data (in-sample predictions)
  real y_twiddle[N, T_twiddle];           // Out-of-sample predictions
  real avg_rating_twiddle[N, T_twiddle];  // Out-of-sample lagged average ratings


  avg_rating_twiddle[,1] = avg_rating[, T-1];
  
  for (n in 1:N) {
    y_twiddle[n,1] = y[n,T];
    y_rep[n,1] = y[n,1];
    for (l in 2:T) {
      //if (l <= T)
        y_rep[n,l] = 
          normal_rng(
            alpha0 
            + alpha[n] 
            + beta[opp_team[n, l-1]] 
            + gamma[team[n]]
            + rho[position[n]]
            + delta[position[n]] * price_std[n]
            + lambda[position[n]] * avg_rating[n,l-1]
            + theta * home[n,l-1],
            sigma_y
          );
          }
      //if (l <= T_twiddle)
      for (l in 2:T_twiddle) {
        avg_rating_twiddle[n,l] = 
        sum(y_twiddle[n,1:(l-1)]) / size(y_twiddle[n,1:(l-1)]);
        y_twiddle[n,l] = 
          normal_rng(
            alpha0
            + alpha[n]
            + beta[opp_team_twiddle[n, l]] 
            + gamma[team[n]]
            + rho[position[n]]
            + delta[position[n]] * price_std[n] 
            + lambda[position[n]] * avg_rating_twiddle[n, l-1]
            + theta * home_twiddle[n,l],
            sigma_y
          );
      }
  }
}
