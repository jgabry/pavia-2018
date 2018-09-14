# Stan fits

# parameters we don't need to save
drop_pars <- c("alpha_raw", "beta_raw", "gamma_raw", "theta_raw", "rho_raw")

# HAr model ---------------------------------------------------------------
HAr_dat <- list(
  N = N,
  J = J,
  T = L,
  K = K,
  y = y,
  team = serie$cluster,
  position = position,
  opp_team = opp_team,
  home = home,
  avg_rating = avg_rating_lag,
  home_twiddle = home_twiddle,
  price_std = price_std,
  opp_team_twiddle = opp_team_twiddle,
  T_twiddle = L_twiddle
)

HAr_fit <-
  stan(
    file = "HArFull.stan",
    data = HAr_dat,
    iter = 1000,
    chains = 4,
    cores = 4,
    seed = 1,
    control = list(
      adapt_delta = 0.99,
      stepsize = 0.01,
      max_treedepth = 13
    ),
    pars = drop_pars,
    include = FALSE
  )
saveRDS(HAr_fit, file = "HAr_stanfit.RDS")
#launch_shinystan(HAr_fit)



# MIX model ---------------------------------------------------------------
MIX_dat <- HAr_dat
MIX_dat$D <- 2
MIX_fit <-
  stan(
    file = "MIXFull.stan",
    data = MIX_dat,
    iter = 1000,
    chains = 4,
    cores = 4,
    seed = 1,
    control = list(
      stepsize = 0.01,
      adapt_delta = 0.99,
      max_treedepth = 13
    ),
    pars = drop_pars,
    include = FALSE
  )
saveRDS(MIX_fit, file = "MIX_stanfit.RDS")
#launch_shinystan(MIX_fit)



# HAr-mis model -----------------------------------------------------------

#Since Stan doesn't accept 'NA', I assign for every 0 of the dataset some fake values (0.001)
zeros <- y[, 2:L] == 0
y[, 2:L][zeros] <- 0.001 #fake value for Stan reading NA

#Further data: observed, unobserved indexes
i <- 1
j <- 1
n_cen <- l_cen <- n_obs <- l_obs <- c()
for (n in 1:N) {
  for (l in 2:L) {
    if (y[n, l] == 0.001) { # da correggere con 0
      n_cen[i] <- n
      l_cen[i] <- l
      i <- i + 1
    } else {
      n_obs[j] <- n
      l_obs[j] <- l
      j <- j + 1
    }
  }
}
Ncen <- length(n_cen)
Nobs <- length(n_obs)

HAr_miss_dat <- HAr_dat
HAr_miss_dat$y <- y
HAr_miss_dat$Ncen <- Ncen
HAr_miss_dat$Nobs <- Nobs
HAr_miss_dat$n_cen <- n_cen
HAr_miss_dat$n_obs <- n_obs
HAr_miss_dat$l_cen <- l_cen
HAr_miss_dat$l_obs <- l_obs

HAr_miss_fit <- stan(
  file = "HAr-misFull.stan",
  data = HAr_miss_dat,
  iter = 1000,
  chains = 4,
  cores = 4,
  seed = 1,
  control = list(
    stepsize = 0.01,
    adapt_delta = 0.99,
    max_treedepth = 13
  ),
  pars = c("eta", drop_pars),
  include = FALSE
)
saveRDS(HAr_miss_fit, file = "HAr_miss_stanfit.RDS")
# launch_shinystan(HAr_miss_fit)

