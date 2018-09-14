## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, 
  dev = "png",
  dpi = 150,
  fig.align = "center",
  comment = NA
)
library(rstan)
library(dplyr)
library(lubridate)
library(ggplot2)
library(bayesplot)

theme_set(bayesplot::theme_default())

# seed for R's pseudo-RNGs, not Stan's
set.seed(1123) 

## ----load-data-----------------------------------------------------------
pest_data <- readRDS('data/pest_data.RDS')
str(pest_data)

## ----describe-data-------------------------------------------------------
N_buildings <- length(unique(pest_data$building_id))
N_buildings

## ----data-plots----------------------------------------------------------
ggplot(pest_data, aes(x = complaints)) + 
  geom_bar()

ggplot(pest_data, aes(x = traps, y = complaints, color = live_in_super == TRUE)) + 
  geom_jitter()

## ---- data-plots-ts, fig.width = 6, fig.height = 8-----------------------
ggplot(pest_data, aes(x = date, y = complaints, color = live_in_super == TRUE)) + 
  geom_line(aes(linetype = "Number of complaints")) + 
  geom_point(color = "black") + 
  geom_line(aes(y = traps, linetype = "Number of traps"), color = "black", size = 0.25) + 
  facet_wrap(~building_id, scales = "free", ncol = 2, labeller = label_both) + 
  scale_x_date(name = "Month", date_labels = "%b") + 
  scale_y_continuous(name = "", limits = range(pest_data$complaints)) + 
  scale_linetype_discrete(name = "") + 
  scale_color_discrete(name = "Live-in super")

## ---- cache=TRUE, results="hide", message=FALSE--------------------------
comp_dgp_simple <- stan_model('stan_programs/simple_poisson_regression_dgp.stan')

## ------------------------------------------------------------------------
print(comp_dgp_simple)

## ----runpoissondgp-------------------------------------------------------
fitted_model_dgp <- sampling(
  comp_dgp_simple,
  data = list(N = nrow(pest_data), mean_traps = mean(pest_data$traps)),
  chains = 1,
  iter = 1,
  algorithm = 'Fixed_param',
  seed = 123
  )

# see http://mc-stan.org/rstan/articles/stanfit_objects.html for various
# ways of extracting the contents of the stanfit object
samps_dgp <- rstan::extract(fitted_model_dgp)
str(samps_dgp)

## ------------------------------------------------------------------------
stan_dat_fake <- list(
  N = nrow(pest_data), 
  traps = samps_dgp$traps[1, ], 
  complaints = samps_dgp$complaints[1, ]
)
str(stan_dat_fake)

## ---- cache=TRUE, results="hide", message=FALSE--------------------------
comp_model_P <- stan_model('stan_programs/simple_poisson_regression.stan')
fit_model_P <- sampling(comp_model_P, data = stan_dat_fake, seed = 123)

posterior_alpha_beta <- as.matrix(fit_model_P, pars = c('alpha','beta'))
head(posterior_alpha_beta)

## ------------------------------------------------------------------------
true_alpha_beta <- c(samps_dgp$alpha, samps_dgp$beta)
mcmc_recover_hist(posterior_alpha_beta, true = true_alpha_beta)

## ------------------------------------------------------------------------
y_rep <- as.matrix(fit_model_P, pars = "y_rep")
ppc_dens_overlay(y = stan_dat_fake$complaints, yrep = y_rep[1:200, ])

## ------------------------------------------------------------------------
ppc_rootogram(stan_dat_fake$complaints, yrep = y_rep)

## ----stan-data-----------------------------------------------------------
stan_dat_simple <- list(
  N = nrow(pest_data), 
  complaints = pest_data$complaints,
  traps = pest_data$traps
)
str(stan_dat_simple)

## ----fit_P_real_data, cache=TRUE-----------------------------------------
fit_P_real_data <- sampling(comp_model_P, data = stan_dat_simple)

## ----results_simple_P----------------------------------------------------
print(fit_P_real_data, pars = c('alpha','beta'))

## ----hist_simple_P-------------------------------------------------------
mcmc_hist(as.matrix(fit_P_real_data, pars = c('alpha','beta')))
mcmc_scatter(as.matrix(fit_P_real_data, pars = c('alpha','beta')), alpha = 0.2)

## ------------------------------------------------------------------------
y_rep <- as.matrix(fit_P_real_data, pars = "y_rep")

## ----marginal_PPC--------------------------------------------------------
ppc_dens_overlay(y = stan_dat_simple$complaints, y_rep[1:200,])

## ------------------------------------------------------------------------
prop_zero <- function(x) mean(x == 0)
ppc_stat(y = stan_dat_simple$complaints, yrep = y_rep, stat = "prop_zero")

## ------------------------------------------------------------------------
mean_y_rep <- colMeans(y_rep)
std_resid <- (stan_dat_simple$complaints - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid) + hline_at(2) + hline_at(-2)

## ------------------------------------------------------------------------
ppc_rootogram(stan_dat_simple$complaints, yrep = y_rep)

## ------------------------------------------------------------------------
ppc_intervals(
  y = stan_dat_simple$complaints, 
  yrep = y_rep,
  x = stan_dat_simple$traps
) + 
  labs(x = "Number of traps", y = "Number of complaints")

## ------------------------------------------------------------------------
ggplot(pest_data, aes(x = log(total_sq_foot), y = log1p(complaints))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

## ------------------------------------------------------------------------
stan_dat_simple$log_sq_foot <- log(pest_data$total_sq_foot/1e4)
stan_dat_simple$live_in_super <- pest_data$live_in_super

## ----compmultPDGP, cache=TRUE, results="hide", message=FALSE-------------
comp_dgp_multiple <- stan_model('stan_programs/multiple_poisson_regression_dgp.stan')

## ----runpoissondgp2------------------------------------------------------
fitted_model_dgp <-
  sampling(
  comp_dgp_multiple,
  data = list(N = nrow(pest_data)),
  chains = 1,
  cores = 1,
  iter = 1,
  algorithm = 'Fixed_param',
  seed = 123
  )
samps_dgp <- rstan::extract(fitted_model_dgp)

## ------------------------------------------------------------------------
stan_dat_fake <- list(
  N = nrow(pest_data), 
  log_sq_foot = samps_dgp$log_sq_foot[1, ],
  live_in_super = samps_dgp$live_in_super[1, ],
  traps = samps_dgp$traps[1, ], 
  complaints = samps_dgp$complaints[1, ]
)

## ---- cache=TRUE, message=FALSE, warning=FALSE---------------------------
comp_model_P_mult <- stan_model('stan_programs/multiple_poisson_regression.stan')
fit_model_P_mult <- sampling(comp_model_P_mult, data = stan_dat_fake, chains = 4, cores = 4)

## ------------------------------------------------------------------------
posterior_alpha_beta <- as.matrix(fit_model_P_mult, pars = c('alpha','beta','beta_super'))
true_alpha_beta <- c(samps_dgp$alpha,samps_dgp$beta,samps_dgp$beta_super)
mcmc_recover_hist(posterior_alpha_beta, true = true_alpha_beta)

## ----fit_mult_P_real_dat-------------------------------------------------
fit_model_P_mult_real <- sampling(comp_model_P_mult, data = stan_dat_simple)
y_rep <- as.matrix(fit_model_P_mult_real, pars = "y_rep")
ppc_dens_overlay(stan_dat_simple$complaints, y_rep[1:200,])

## ------------------------------------------------------------------------
prop_zero <- function(x) mean(x == 0)
ppc_stat(y = stan_dat_simple$complaints, yrep = y_rep, stat = "prop_zero", binwidth = 0.01)

## ------------------------------------------------------------------------
ppc_intervals(
  y = stan_dat_simple$complaints, 
  yrep = y_rep,
  x = stan_dat_simple$traps
) + 
  labs(x = "Number of traps", y = "Number of complaints")

## ---- cache=TRUE, results="hide", message=FALSE--------------------------
comp_dgp_multiple_NB <- stan_model('stan_programs/multiple_NB_regression_dgp.stan')

## ----fake_data_dgp_NB----------------------------------------------------
fitted_model_dgp_NB <-
  sampling(
  comp_dgp_multiple_NB,
  data = list(N = nrow(pest_data)),
  chains = 1,
  cores = 1,
  iter = 1,
  algorithm = 'Fixed_param',
  seed = 123
  )
samps_dgp_NB <- rstan::extract(fitted_model_dgp_NB)

## ----NB_fake_stan_dat----------------------------------------------------
stan_dat_fake_NB <- list(
  N = nrow(pest_data), 
  log_sq_foot = samps_dgp_NB$log_sq_foot[1, ],
  live_in_super = samps_dgp_NB$live_in_super[1, ],
  traps = samps_dgp_NB$traps[1, ], 
  complaints = samps_dgp_NB$complaints[1, ]
)

## ---- cache=TRUE, results="hide", message=FALSE--------------------------
comp_model_NB <- stan_model('stan_programs/multiple_NB_regression.stan')

## ----runNBoverfake, message=FALSE, warning=FALSE-------------------------
fitted_model_NB <- sampling(comp_model_NB, data = stan_dat_fake_NB, 
                            chains = 4, cores = 4)
posterior_alpha_beta_NB <- 
  as.matrix(fitted_model_NB,
            pars = c('alpha',
                     'beta',
                     'beta_super',
                     'inv_phi')
  )

## ------------------------------------------------------------------------
true_alpha_beta_NB <- 
  c(samps_dgp_NB$alpha,
    samps_dgp_NB$beta,
    samps_dgp_NB$beta_super,
    samps_dgp_NB$inv_phi
  )
mcmc_recover_hist(posterior_alpha_beta_NB, true = true_alpha_beta_NB)

## ----runNB---------------------------------------------------------------
fitted_model_NB <- sampling(comp_model_NB, data = stan_dat_simple)
samps_NB <- rstan::extract(fitted_model_NB)

## ----ppc-full------------------------------------------------------------
y_rep <- samps_NB$y_rep
ppc_dens_overlay(stan_dat_simple$complaints, y_rep[1:200,])

## ------------------------------------------------------------------------
ppc_stat(y = stan_dat_simple$complaints, yrep = y_rep, stat = "prop_zero")

## ------------------------------------------------------------------------
mean_inv_phi <- mean(samps_NB$inv_phi)
mean_y_rep <- colMeans(y_rep)
std_resid <- (stan_dat_simple$complaints - mean_y_rep) / sqrt(mean_y_rep + mean_y_rep^2*mean_inv_phi)
qplot(mean_y_rep, std_resid) + hline_at(2) + hline_at(-2)

## ------------------------------------------------------------------------
ppc_rootogram(stan_dat_simple$complaints, yrep = y_rep)

## ------------------------------------------------------------------------
ppc_intervals(
  y = stan_dat_simple$complaints, 
  yrep = y_rep,
  x = stan_dat_simple$traps
) + 
  labs(x = "Number of traps", y = "Number of complaints")

## ----ppc-group_means-----------------------------------------------------
ppc_stat_grouped(
  y = stan_dat_simple$complaints, 
  yrep = y_rep, 
  group = pest_data$building_id, 
  stat = 'mean',
  binwidth = 0.2
)

## ----prep-data-----------------------------------------------------------
N_months <- length(unique(pest_data$date))

# Add some IDs for building and month
pest_data <- pest_data %>%
  mutate(
    building_fac = factor(building_id, levels = unique(building_id)),
    building_idx = as.integer(building_fac),
    ids = rep(1:N_months, N_buildings),
    mo_idx = lubridate::month(date)
  )

# Center and rescale the building specific data
building_data <- pest_data %>%
    select(
      building_idx,
      live_in_super,
      age_of_building,
      total_sq_foot,
      average_tenant_age,
      monthly_average_rent
    ) %>%
    unique() %>%
    arrange(building_idx) %>%
    select(-building_idx) %>%
    scale(scale=FALSE) %>%
    as.data.frame() %>%
    mutate( # scale by constants
      age_of_building = age_of_building / 10,
      total_sq_foot = total_sq_foot / 10000,
      average_tenant_age = average_tenant_age / 10,
      monthly_average_rent = monthly_average_rent / 1000
    ) %>%
    as.matrix()

# Make data list for Stan
stan_dat_hier <-
  with(pest_data,
        list(complaints = complaints,
             traps = traps,
             N = length(traps),
             J = N_buildings,
             M = N_months,
             log_sq_foot = log(pest_data$total_sq_foot/1e4),
             building_data = building_data[,-3],
             mo_idx = as.integer(as.factor(date)),
             K = 4,
             building_idx = building_idx
             )
        )

## ----comp-NB-hier, cache=TRUE, results="hide", message=FALSE-------------
comp_model_NB_hier <- stan_model('stan_programs/hier_NB_regression.stan')

## ----run-NB-hier---------------------------------------------------------
fitted_model_NB_hier <-
  sampling(
   comp_model_NB_hier,
   data = stan_dat_hier,
   chains = 4,
   cores = 4,
   iter = 4000
  )

## ------------------------------------------------------------------------
samps_hier_NB <- rstan::extract(fitted_model_NB_hier)

## ----print-NB-hier-------------------------------------------------------
print(fitted_model_NB_hier, pars = c('sigma_mu','beta','alpha','phi','mu'))

## ------------------------------------------------------------------------
# use as.array to keep the markov chains separate for trace plots
mcmc_trace(
  as.array(fitted_model_NB_hier,pars = 'sigma_mu'),
  np = nuts_params(fitted_model_NB_hier),
  window = c(500,1000)
)

## ------------------------------------------------------------------------
# assign to object so we can compare to another plot later
scatter_with_divs <- mcmc_scatter(
  as.array(fitted_model_NB_hier),
  pars = c("mu[4]", 'sigma_mu'),
  transform = list('sigma_mu' = "log"),
  np = nuts_params(fitted_model_NB_hier)
)
scatter_with_divs

## ------------------------------------------------------------------------
N_sims <- 1000
log_sigma <- rep(NA, N_sims)
theta <- rep(NA, N_sims)
for (j in 1:N_sims) {
  log_sigma[j] <- rnorm(1, mean = 0, sd = 1)
  theta[j] <- rnorm(1, mean = 0, sd = exp(log_sigma[j]))
}
draws <- cbind("mu" = theta, "log(sigma_mu)" = log_sigma)
mcmc_scatter(draws)

## ------------------------------------------------------------------------
parcoord_with_divs <- mcmc_parcoord(
  as.array(fitted_model_NB_hier, pars = c("sigma_mu", "mu")),
  np = nuts_params(fitted_model_NB_hier)
)
parcoord_with_divs

## ----comp-NB-hier-ncp, cache=TRUE----------------------------------------
comp_model_NB_hier_ncp <- stan_model('stan_programs/hier_NB_regression_ncp.stan')

## ----run-NB-hier-ncp-----------------------------------------------------
fitted_model_NB_hier_ncp <- sampling(comp_model_NB_hier_ncp, data = stan_dat_hier, chains = 4, cores = 4)

## ----n-eff-NB-hier-ncp-check---------------------------------------------
print(fitted_model_NB_hier_ncp, pars = c('sigma_mu','beta','alpha','phi','mu'))

## ------------------------------------------------------------------------
scatter_no_divs <- mcmc_scatter(
  as.array(fitted_model_NB_hier_ncp),
  pars = c("mu[4]", 'sigma_mu'),
  transform = list('sigma_mu' = "log"),
  np = nuts_params(fitted_model_NB_hier_ncp)
)
bayesplot_grid(scatter_with_divs, scatter_no_divs,
               grid_args = list(ncol = 2), ylim = c(-11, 1))

## ------------------------------------------------------------------------
parcoord_no_divs <- mcmc_parcoord(
  as.array(fitted_model_NB_hier_ncp, pars = c("sigma_mu", "mu")),
  np = nuts_params(fitted_model_NB_hier_ncp)
)
bayesplot_grid(parcoord_with_divs, parcoord_no_divs,
               ylim = c(-3, 3))

## ----samps-full-hier-----------------------------------------------------
samps_NB_hier_ncp <- rstan::extract(fitted_model_NB_hier_ncp, pars = c('y_rep','inv_phi'))

## ----ppc-full-hier-------------------------------------------------------
y_rep <- as.matrix(fitted_model_NB_hier_ncp, pars = "y_rep")
ppc_dens_overlay(stan_dat_hier$complaints, y_rep[1:200,])

## ----ppc-group_means-hier------------------------------------------------
ppc_stat_grouped(
  y = stan_dat_hier$complaints,
  yrep = y_rep,
  group = pest_data$building_id,
  stat = 'mean',
  binwidth = 0.5
)

## ------------------------------------------------------------------------
ppc_stat_grouped(
  y = stan_dat_hier$complaints,
  yrep = y_rep,
  group = pest_data$building_id,
  stat = 'sd',
  binwidth = 0.5
)

## ------------------------------------------------------------------------
prop_zero <- function(x) mean(x == 0)
ppc_stat(
  y = stan_dat_hier$complaints,
  yrep = y_rep,
  stat = prop_zero,
  binwidth = 0.025
)

# plot separately for each building
ppc_stat_grouped(
  y = stan_dat_hier$complaints,
  yrep = y_rep,
  group = pest_data$building_id,
  stat = prop_zero,
  binwidth = 0.025
)

## ------------------------------------------------------------------------
ppc_intervals(
  y = stan_dat_hier$complaints,
  yrep = y_rep,
  x = stan_dat_hier$traps
) +
  labs(x = "Number of traps", y = "Number of complaints")

## ------------------------------------------------------------------------
mean_y_rep <- colMeans(y_rep)
mean_inv_phi <- mean(as.matrix(fitted_model_NB_hier_ncp, pars = "inv_phi"))
std_resid <- (stan_dat_hier$complaints - mean_y_rep) / sqrt(mean_y_rep + mean_y_rep^2*mean_inv_phi)
qplot(mean_y_rep, std_resid) + hline_at(2) + hline_at(-2)

## ------------------------------------------------------------------------
ppc_rootogram(stan_dat_hier$complaints, yrep = y_rep)

## ------------------------------------------------------------------------
stan_dat_hier <- readRDS('data/pest_data_longer_stan_dat.RDS')

## ----comp-NB-hier-slopes, cache=TRUE, results="hide", message=FALSE------
comp_model_NB_hier_slopes <- stan_model('stan_programs/hier_NB_regression_ncp_slopes_mod.stan')

## ----run-NB-hier-slopes--------------------------------------------------
fitted_model_NB_hier_slopes <-
  sampling(
    comp_model_NB_hier_slopes,
    data = stan_dat_hier,
    chains = 4, cores = 4,
    control = list(adapt_delta = 0.95)
  )

## ------------------------------------------------------------------------
mcmc_hist(
  as.matrix(fitted_model_NB_hier_slopes, pars = "sigma_kappa"),
  binwidth = 0.005
)

## ------------------------------------------------------------------------
print(fitted_model_NB_hier_slopes, pars = c('kappa','beta','alpha','phi','sigma_mu','sigma_kappa','mu'))

## ------------------------------------------------------------------------
mcmc_hist(
  as.matrix(fitted_model_NB_hier_slopes, pars = "beta"),
  binwidth = 0.005
)

## ----ppc-full-hier-slopes------------------------------------------------
y_rep <- as.matrix(fitted_model_NB_hier_slopes, pars = "y_rep")
ppc_dens_overlay(
  y = stan_dat_hier$complaints,
  yrep = y_rep[1:200,]
)

## ----ppc-group_max-hier-slopes-mean-by-mo--------------------------------
select_vec <- which(stan_dat_hier$mo_idx %in% 1:12)
ppc_stat_grouped(
  y = stan_dat_hier$complaints[select_vec],
  yrep = y_rep[,select_vec],
  group = stan_dat_hier$mo_idx[select_vec],
  stat = 'mean'
) + xlim(0, 11)

## ----comp-NB-hier-mos, cache=TRUE, results="hide", message=FALSE---------
comp_model_NB_hier_mos <- stan_model('stan_programs/hier_NB_regression_ncp_slopes_mod_mos.stan')

## ----run-NB-hier-slopes-mos----------------------------------------------
fitted_model_NB_hier_mos <- sampling(comp_model_NB_hier_mos, data = stan_dat_hier, chains = 4, cores = 4, control = list(adapt_delta = 0.9))

## ----ppc-full-hier-mos---------------------------------------------------
y_rep <- as.matrix(fitted_model_NB_hier_mos, pars = "y_rep")
ppc_dens_overlay(
  y = stan_dat_hier$complaints,
  yrep = y_rep[1:200,]
)

## ------------------------------------------------------------------------
select_vec <- which(stan_dat_hier$mo_idx %in% 1:12)
ppc_stat_grouped(
  y = stan_dat_hier$complaints[select_vec],
  yrep = y_rep[,select_vec],
  group = stan_dat_hier$mo_idx[select_vec],
  stat = 'mean'
)

## ------------------------------------------------------------------------
# 1) compare draws from prior and draws from posterior
rho_draws <- cbind(
  2 * rbeta(4000, 10, 5) - 1, # draw from prior
  as.matrix(fitted_model_NB_hier_mos, pars = "rho")
)
colnames(rho_draws) <- c("prior", "posterior")
mcmc_hist(rho_draws, freq = FALSE, binwidth = 0.025,
          facet_args = list(nrow = 2)) + xlim(-1, 1)


# 2) overlay prior density curve on posterior draws
gen_rho_prior <- function(x) {
  alpha <- 10; beta <- 5
  a <- -1; c <- 1
  lp <- (alpha - 1) * log(x - a) +
        (beta - 1) * log(c - x) -
        (alpha + beta - 1) * log(c - a) -
         lbeta(alpha, beta)
  return(exp(lp))
}
mcmc_hist(as.matrix(fitted_model_NB_hier_mos, pars = "rho"),
          freq = FALSE, binwidth = 0.01) +
  overlay_function(fun = gen_rho_prior) +
  xlim(-1,1)

## ------------------------------------------------------------------------
print(fitted_model_NB_hier_mos, pars = c('rho','sigma_mu','sigma_kappa','gamma'))

## ------------------------------------------------------------------------
ppc_intervals(
  y = stan_dat_hier$complaints,
  yrep = y_rep,
  x = stan_dat_hier$traps
) +
  labs(x = "Number of traps", y = "Number of complaints")

## ----comp-rev, cache=TRUE, results="hide", message=FALSE-----------------
comp_rev <- stan_model('stan_programs/hier_NB_regression_ncp_slopes_mod_mos_predict.stan')
print(comp_rev)

## ----run-NB-hier-rev-----------------------------------------------------
rev_model <- sampling(comp_rev, data = stan_dat_hier, cores = 4,
                      control = list(adapt_delta = 0.9))

## ----materials-cost-formulation------------------------------------------
N_traps <- 20
costs <- 10 * (0:N_traps)

## ----labor-cost-formulation----------------------------------------------
N_months_forward <- 12
N_months_labor <- N_months_forward / 2
hourly_rate_low <- 20
hourly_rate_high <- 30
costs <- costs +
  (0:N_traps < 5 & 0:N_traps > 0) * (N_months_labor * hourly_rate_low) +
  (0:N_traps >= 5 & 0:N_traps < 10) * (N_months_labor * (hourly_rate_low + 1 * hourly_rate_high)) +
  (0:N_traps >= 10 & 0:N_traps < 15) * (N_months_labor * (hourly_rate_low + 2 * hourly_rate_high)) +
  (0:N_traps >= 15) * (N_months_labor * (hourly_rate_low + 3 * hourly_rate_high))

## ----rev-curves, fig.width = 6, fig.height = 8---------------------------
# extract as a list for convenience below
samps_rev <- rstan::extract(rev_model)

# total and mean profit
tot_profit <- sweep(samps_rev$rev_, 3, STATS = costs, FUN = '-')
mean_profit <- t(apply(tot_profit, c(2, 3), median))

# lower and upper ends of 50% central interval
lower_profit <- t(apply(tot_profit, c(2, 3), quantile, 0.25))
upper_profit <- t(apply(tot_profit, c(2, 3), quantile, 0.75))

profit_df <-
  data.frame(
    profit = as.vector(mean_profit),
    lower = as.vector(lower_profit),
    upper = as.vector(upper_profit),
    traps = rep(0:N_traps, times = N_buildings),
    building = rep(1:N_buildings, each = N_traps + 1)
  )
ggplot(data = profit_df, aes(x = traps, y = profit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
  geom_line() +
  facet_wrap(~ building, scales = 'free_y', ncol = 2)

## ----comp-NB-hier-gp, cache=TRUE, results="hide", message=FALSE----------
comp_model_NB_hier_gp <- stan_model('stan_programs/hier_NB_regression_ncp_slopes_mod_mos_gp.stan')

## ----run-NB-hier-gp------------------------------------------------------
fitted_model_NB_hier_gp <- sampling(comp_model_NB_hier_gp, data = stan_dat_hier, chains = 4, cores = 4, control = list(adapt_delta = 0.9))
samps_gp <- rstan::extract(fitted_model_NB_hier_gp)

## ------------------------------------------------------------------------
length_scale_draws <- cbind(
  prior = rgamma(4000, 10, 2),
  posterior = samps_gp$gp_len
)
mcmc_areas(length_scale_draws)

## ------------------------------------------------------------------------
noise_to_length_scale_ratio_draws <- cbind(
  prior = abs(rnorm(4000)) / rgamma(4000, 10, 2),
  posterior = samps_gp$sigma_gp / samps_gp$gp_len
)
mcmc_areas(noise_to_length_scale_ratio_draws)

## ---- message=FALSE, warning=FALSE---------------------------------------
# visualizing 50% intervals
mo_ar_intervals <- mcmc_intervals_data(as.matrix(fitted_model_NB_hier_mos, pars = "mo"), prob = 0.5)
mo_gp_intervals <- mcmc_intervals_data(as.matrix(fitted_model_NB_hier_gp, pars = "gp"), prob = 0.5)
plot_data <- bind_rows(mo_ar_intervals, mo_gp_intervals)
plot_data$prior <- factor(rep(c("AR1", "GP"), each = 36), levels = c("GP", "AR1"))
plot_data$time <- rep(1:36, times = 2)

ggplot(plot_data, aes(x = time, y = m, ymin = l, ymax = h, fill = prior)) +
  geom_ribbon(alpha = 2/3)

## ---- message=FALSE, warning=FALSE---------------------------------------
# visualizing 50% intervals
mo_noise_intervals <- mcmc_intervals_data(as.matrix(fitted_model_NB_hier_gp, pars = "mo_noise"), prob = 0.5)
gp_exp_quad_intervals <- mcmc_intervals_data(as.matrix(fitted_model_NB_hier_gp, pars = "gp_exp_quad"), prob = 0.5)

plot_data <- bind_rows(mo_noise_intervals, gp_exp_quad_intervals)
plot_data$time <- rep(1:36, times = 2)
plot_data$term <- factor(rep(c("Monthly Noise", "Smooth Trend"), each = 36),
                         levels = c("Smooth Trend", "Monthly Noise"))

ggplot(plot_data, aes(x = time, y = m, ymin = l, ymax = h, fill = term)) +
  geom_ribbon(alpha = 0.5) +
  geom_line(aes(color = term), size = 0.5) +
  ylab(NULL)

