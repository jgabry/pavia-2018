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
fit_P_real_data <-
  stan("stan_programs/simple_poisson_regression.stan", 
       data = stan_dat_simple)

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

