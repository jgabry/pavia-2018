# Results
library("rstan")
library("ggplot2")
library("bayesplot")
library("reshape2")
library("dplyr")

HAr_fit <- readRDS("HAr_stanfit.RDS")
HAr_miss_fit <- readRDS("HAr_miss_stanfit.RDS")
MIX_fit <- readRDS("MIX_stanfit.RDS")

HAr_draws <- rstan::extract(HAr_fit)
HAr_miss_draws <- rstan::extract(HAr_miss_fit)
MIX_draws <- rstan::extract(MIX_fit)

param_names <- c(
  paste0("beta[", 1:5, "]"),
  paste0("gamma[", 1:5, "]"),
  paste0("delta[", 1:4, "]"),
  paste0("rho[", 1:4, "]"),
  paste0("lambda[", 1:4, "]"),
  "theta", 
  paste0("sigma[", c("y", "alpha", "beta", "gamma", "rho"), "]")
)


post_means <- function(draws) {
  with(draws,
       c(
         colMeans(beta),
         colMeans(gamma),
         colMeans(delta),
         colMeans(rho),
         colMeans(lambda),
         mean(theta),
         mean(sigma_y),
         mean(sigma_alpha),
         mean(sigma_beta),
         mean(sigma_gamma),
         mean(sigma_rho)
       )
  )
}

post_sds <- function(draws) {
  with(draws,
       c(
         apply(beta, 2, sd),
         apply(gamma, 2, sd),
         apply(delta, 2, sd),
         apply(rho, 2, sd),
         apply(lambda, 2, sd),
         sd(theta),
         sd(sigma_y),
         sd(sigma_alpha),
         sd(sigma_beta),
         sd(sigma_gamma),
         sd(sigma_rho)
       ))
}

HAr_means <- post_means(HAr_draws)
HAr_sds <- post_sds(HAr_draws)
HAr_miss_means <- post_means(HAr_miss_draws)
HAr_miss_sds <- post_sds(HAr_miss_draws)
MIX_means <- post_means(MIX_draws)
MIX_sds <- post_sds(MIX_draws)
MIX_means_logit <- colMeans(MIX_draws$zeta)
MIX_sds_logit <- apply(MIX_draws$zeta, 2, sd)

plot_data_ar <- data.frame(
  est = rev(HAr_means), 
  se_est = rev(HAr_sds), 
  parameter = factor(seq_along(param_names), labels = rev(param_names)), 
  model = "HAr"
)
plot_data_ar_miss <- data.frame(
  est = rev(HAr_miss_means), 
  se_est = rev(HAr_miss_sds), 
  parameter = factor(seq_along(param_names), labels = rev(param_names)),
  model = "HAr-mis"
)

param_names_mix <- c(param_names, paste0("zeta[", 1:4,"]"))
plot_data_mix <- data.frame(
  est = rev(c(MIX_means, MIX_means_logit)), 
  se_est = rev(c(MIX_sds, MIX_sds_logit)), 
  parameter = factor(seq_along(param_names_mix), labels = rev(param_names_mix)),
  model = "MIX"
)

plot_data <- rbind(plot_data_mix, plot_data_ar, plot_data_ar_miss)
plot_data$model <- factor(plot_data$model, levels = c("HAr", "HAr-mis", "MIX"))

# When running this ggplot code if you get an error about a polygon edge just
# keep rerunning the code and it will eventually work
ggplot(plot_data,
       aes(
         x = parameter,
         y = est,
         ymin = est - se_est,
         ymax = est + se_est,
         color = model, 
         group = model
       )) + 
  hline_0(color = "gray") +
  geom_pointrange(fatten = 2.25,
                  size = 0.5,
                  position = position_dodge(width = 0.4)) + 
  coord_flip() + 
  #scale_color_hue("") +
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = parse(text = rev(param_names_mix))) +
  labs(x = NULL, y = NULL, title = "Posterior mean +/- sd") +
  theme_default(base_size = 16) +
  legend_move(position = "top") +
  theme(panel.grid.major.y = element_line(size = 0.1, color = "lightgray")) 

ggsave("../ImagesPaper/HArHArmisMix_estimate_gg_grey.pdf", height = 7.5, width = 6)





alpha_mean_HAr <- colMeans(HAr_draws$alpha)
alpha_sd_HAr <- apply(HAr_draws$alpha, 2, sd)
alpha_mean_HAr_miss <- colMeans(HAr_miss_draws$alpha)
alpha_sd_HAr_miss <- apply(HAr_miss_draws$alpha, 2, sd)
alpha_mean_MIX <- colMeans(MIX_draws$alpha)
alpha_sd_MIX <- apply(MIX_draws$alpha, 2, sd)

plot_data_alpha <- data.frame(
  est = c(alpha_mean_HAr, alpha_mean_HAr_miss, alpha_mean_MIX), 
  se_est = c(alpha_sd_HAr, alpha_sd_HAr_miss, alpha_sd_MIX), 
  parameter = rep(paste0("alpha[", 1:237, "]"), 3), 
  model = rep(c("HAr", "HAr-mis", "MIX"), each = 237)
)
plot_data_alpha$model <- factor(plot_data_alpha$model, levels = c("HAr", "HAr-mis", "MIX"))
plot_data_alpha$position <- rep(position, 3)
ggplot(plot_data_alpha,
       aes(
         x = parameter,
         y = est,
         ymin = est - se_est,
         ymax = est + se_est,
         color = factor(position)
         # group = model
       )) + 
  # hline_0(color = "gray") +
  # geom_pointrange(fatten = 1.25,
  #                 size = 0.25,
  #                 position = position_dodge(width = 2/5)) +
  geom_point() +
  facet_wrap("model") +
  scale_color_hue("") +
  labs(x = NULL, y = NULL, title = "Posterior mean +/- sd") +
  theme_default(base_size = 16) +
  legend_move(position = "top") +
  theme(panel.grid.major.y = element_line(size = 0.1, color = "lightgray")) + 
  xaxis_text(FALSE) + 
  xaxis_ticks(FALSE)

ggsave("../ImagesPaper/HArHArmisMix_alpha_estimate_gg.pdf", height = 7.5, width = 6)







# 
# #Individual intercepts
# 
# par(mfrow=c(1,1))
# par(mar=c(5, 4, 4, 2) + 0.1, mgp=c(2,.5,0),tck=-.0)
# 
# alpha_ar=colMeans(HAr_draws$alpha)
# alpha_mix=colMeans(MIX_draws$alpha)
# alpha_ar_miss=colMeans(HAr_miss_draws$alpha)
# 
# plot(1:N, alpha_ar, xlab="Player ID", ylab="Individual intercepts")
# points((1:N)[Ruolo_num==1], alpha_ar[Ruolo_num==1], col="red", pch=21, bg="red")
# points((1:N)[Ruolo_num==2], alpha_ar[Ruolo_num==2], col="darkgreen", pch=21, bg="darkgreen")
# points((1:N)[Ruolo_num==3], alpha_ar[Ruolo_num==3], col="blue", pch=21, bg="blue")
# points((1:N)[Ruolo_num==4], alpha_ar[Ruolo_num==4], col="blueviolet", pch=21, bg="blueviolet")
# 
# 
