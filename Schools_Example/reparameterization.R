# Centered (cp) vs non-centered (ncp) parameterizations of 
# hierarchical meta-analysis model

library("rstan")
library("ggplot2")
library("bayesplot")
library("shinystan")

# set ggplot plotting theme to bayesplot theme
theme_set(bayesplot::theme_default(base_size = 14))


# Data --------------------------------------------------------------------
# Estimated treatment effects (and standard errors) of a test coaching program 
# conducted in eight schools
y <- c(28,  8, -3,  7, -1,  1, 18, 12)
sigma <- c(15, 10, 16, 11,  9, 11, 10, 18)


# Plot the data ---------------------------------------------------------
dataplot <- ggplot(data.frame(x = factor(1:length(y)), y, sigma), aes(x,y)) +
  geom_hline(yintercept = 0, col = "darkgray", linetype = 2) +
  geom_hline(yintercept = mean(y), col = "blue", linetype = 2) +
  geom_pointrange(aes(ymin = y - sigma, ymax = y + sigma), size = 1, color = "blue") + 
  labs(y = expression(y %+-% sigma), x = NULL) +
  scale_x_discrete(labels = paste0("y[", 1:length(y), "]")) +
  coord_flip()

plot(dataplot)


# Translate Stan programs to C++ and compile -------------------------------
# centered and non-centered parameterizations for comparison
cp_mod <- stan_model("hierarchical.stan")
ncp_mod <- stan_model("hierarchical_ncp.stan")

# Sample from posteriors --------------------------------------------------

standata <- list(J = length(y), y = y, sigma = sigma)

cp_fit1 <- sampling(cp_mod, data = standata) # lots of warnings
# launch_shinystan(cp_fit1)

ncp_fit1 <- sampling(ncp_mod, data = standata) # no warnings (or very few warnings)
# launch_shinystan(ncp_fit1)

# extract draws and diagnostics for plotting
# (as.array keeps the markov chains separate so we can make trace plots)
posterior_cp1 <- as.array(cp_fit1, pars = c("mu", "tau", "theta"))
posterior_ncp1 <- as.array(ncp_fit1, pars = c("mu", "tau", "theta"))
hmc_diagnostics_cp1 <- nuts_params(cp_fit1)
hmc_diagnostics_ncp1 <- nuts_params(ncp_fit1)

# traceplot with divergences
color_scheme_set("mix-darkgray-blue")
mcmc_trace(posterior_cp1, pars = "tau", np = hmc_diagnostics_cp1) + 
  labs(y = NULL, x = "Post-warmup iteration")

mcmc_trace(posterior_ncp1, pars = "tau", np = hmc_diagnostics_ncp1) + 
  labs(y = NULL, x = "Post-warmup iteration")


# compare autocorrelation
color_scheme_set("brightblue")
mcmc_acf(posterior_cp1, pars = "tau", lags = 20)
mcmc_acf(posterior_ncp1, pars = "tau", lags = 20)

# large effective sample sizes
print(ncp_fit1)

# parallel coordinates 
color_scheme_set("darkgray")
div_style <- parcoord_style_np(div_color = "green", div_size = 0.15, div_alpha = 0.4)
mcmc_parcoord(
  posterior_cp1,
  size = 0.15,
  alpha = 0.2,
  np = hmc_diagnostics_cp1,
  np_style = div_style
) 

mcmc_parcoord(
  posterior_ncp1,
  size = 0.15,
  alpha = 0.2,
  np = hmc_diagnostics_ncp1,
  np_style = div_style
) 


# scatterplot with divergences
div_style <- scatter_style_np(div_color = "green", div_size = 2.5, div_alpha = 0.75)
scatter_cp1 <- mcmc_scatter(
  posterior_cp1,
  size = 1.5,
  alpha = 2/3,
  pars = c("theta[1]", "tau"), 
  transform = list(tau = "log"), 
  np = hmc_diagnostics_cp1,
  np_style = div_style
) + ylim(-7, 4)

# non-centered parameterization
scatter_ncp1 <- mcmc_scatter(
  posterior_ncp1,
  size = 1.5,
  alpha = 2/3,
  pars = c("theta[1]", "tau"), 
  transform = list(tau = "log"), 
  np = hmc_diagnostics_ncp1,
  np_style = div_style
) + ylim(-7, 4)

bayesplot_grid(scatter_cp1, scatter_ncp1, grid_args = list(ncol = 2), 
               titles = c("CP", "NCP"))

# look at posterior distributions 

mu <- as.matrix(ncp_fit1, pars = "mu")
mean(mu > 0) # est. posterior Pr(mu > 0)

# a few different ways to compare prior to posterior visually 
color_scheme_set("brightblue")

mu_prior_pdf <- function(x) dnorm(x, mean = 0, sd = 10)
mcmc_hist(mu, freq=FALSE, binwidth = 1) + 
  overlay_function(fun = mu_prior_pdf) + 
  xlim(-30, 30)

mu_prior_samples <- rnorm(nrow(mu), mean = 0, sd = 10)
mu_compare <- cbind(mu_prior = mu_prior_samples, mu)
mcmc_hist(mu_compare, binwidth = 1.5, facet_args = list(nrow = 2)) + xlim(-30, 30)

tau <- as.matrix(ncp_fit1, pars = "tau")
tau_prior_pdf <- function(x) 2 * dnorm(x, mean = 0, sd = 10)
mcmc_hist(tau, freq=FALSE, binwidth = 1) + 
  overlay_function(fun = tau_prior_pdf) + 
  xlim(0, 30)


thetas <- as.matrix(ncp_fit1, pars = "theta")
mcmc_intervals(cbind(mu, thetas)) + vline_at(mean(mu))
mcmc_areas_ridges(cbind(mu, thetas)) + vline_at(mean(mu))

# add the parameter estimates to the plot of the data to visually 
# show the partial pooling
theta_means <- apply(thetas, 2, mean)
theta_sds <- apply(thetas, 2, sd)
dataplot + 
  geom_hline(yintercept = mean(mu), col = "red3", linetype = 2) +
  geom_pointrange(
    data = data.frame(x = factor(1:length(y)), m = theta_means, s = theta_sds), 
    aes(y = theta_means, ymin = theta_means - theta_sds, ymax = theta_means + theta_sds),
    color = "red"
  ) 


# for demonstration purposes, make the data more informative by 
# shrinking sigma. now the centered parameterization will also work ok. 
standata2 <- standata
standata2$sigma <- standata2$sigma / 10
fit_cp2 <- sampling(cp_mod, data = standata2)
