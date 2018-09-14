##  generate hypothetical players

#pag 140; 364; 417 Gelman Hill

#fake predictors
k <- 3 # team-cluster 1
fake_price_std <- c( mean(price_std[position==1]), mean(price_std[position==2]),
                     mean(price_std[position==3]), mean(price_std[position==4]))
fake_home_away <- rep(1, N)


## HAr
theta.true.ar <- median(HAr_draws$theta)
lambda.true.ar <- apply(HAr_draws$lambda, 2, median)
delta.true.ar <- apply(HAr_draws$delta, 2, median)
sigma.y.true.ar <- median(HAr_draws$sigma_y)
sigma.a.true.ar <- median(HAr_draws$sigma_alpha)
sigma.b.true.ar <- median(HAr_draws$sigma_beta)
sigma.gamma.true.ar <- median(HAr_draws$sigma_gamma)

a0.true.ar <- mean(HAr_draws$alpha0)
a.true.ar <- colMeans(HAr_draws$alpha)
b.true.ar <- colMeans(HAr_draws$beta)
gamma.true.ar <- colMeans(HAr_draws$gamma)
rho.true.ar <- colMeans(HAr_draws$rho)


n.sims <- nrow(HAr_draws$alpha)
y.fake.ar <- array(NA, c(n.sims, N, L))
y.fake.ar[, , 1] <- matrix(0, n.sims, N)
for (s in 1:n.sims) {
  for (n in 1:N) {
    for (l in 2:L) {
      y.fake.ar[s, n, l] <- 
        rnorm(
          1,
          mean =
            a0.true.ar
          + a.true.ar[n]
          + b.true.ar[k]
          + gamma.true.ar[k]
          + delta.true.ar[position[n]] * fake_price_std[position[n]]
          + theta.true.ar * fake_home_away[n] + rho.true.ar[position[n]]
          + lambda.true.ar[position[n]] * sum(y.fake.ar[s, n, (1:(l - 1))]) / length((1:(l - 1))),
          sd = sigma.y.true.ar
        )
    }
  }
}


##MIX

#true values for unmodelled parameters

theta.true.mix <- median(MIX_draws$theta)
lambda.true.mix <- apply(MIX_draws$lambda, 2, median)
delta.true.mix <- apply(MIX_draws$delta, 2, median)
sigma.y.true.mix <- median(MIX_draws$sigma_y)
sigma.a.true.mix <- median(MIX_draws$sigma_alpha)
sigma.b.true.mix <- median(MIX_draws$sigma_beta)
sigma.gamma.true.mix <- median(MIX_draws$sigma_gamma)
pi.true.mix <- apply(MIX_draws$pi_eta_rep, c(2, 3), median)

a0.true.mix <- mean(MIX_draws$alpha0)
a.true.mix <- colMeans(MIX_draws$alpha)
b.true.mix <- colMeans(MIX_draws$beta)
gamma.true.mix <- colMeans(MIX_draws$gamma)
rho.true.mix <- colMeans(MIX_draws$rho)


y.fake.mix <- array(0, c(n.sims, N, L))
y.fake.mix[, , 1] <- matrix(0, n.sims, N)
V.fake.mix <- array(0, c(n.sims, N, L))
for (s in 1:n.sims) {
  for (n in 1:N) {
    for (l in 2:L) {
      V.fake.mix[s, n, l] <- MIX_draws$V[s, n, l - 1]
      if (V.fake.mix[s, n, l] == 0) {
        y.fake.mix[s, n, l] <- 0
      } else{
        y.fake.mix[s, n, l] <- 
          rnorm(
            1,
            a0.true.mix 
            + a.true.mix[n]
            + (b.true.mix[1] + gamma.true.mix[1])
            + delta.true.mix[position[n]] * fake_price_std[position[n]]
            + theta.true.mix * fake_home_away[n] 
            + rho.true.mix[position[n]]
            + lambda.true.mix[position[n]] * sum(y.fake.mix[s, n, (1:(l - 1))]) / length((1:(l - 1))),
            sigma.y.true.mix
          )
      }
    }
  }
}



#HAr missing

theta.true.ar_miss <- median(HAr_miss_draws$theta)
lambda.true.ar_miss <- apply(HAr_miss_draws$lambda, 2, median)
delta.true.ar_miss <- apply(HAr_miss_draws$delta, 2, median)
sigma.y.true.ar_miss <- median(HAr_miss_draws$sigma_y)
sigma.a.true.ar_miss <- median(HAr_miss_draws$sigma_alpha)
sigma.gamma.true.ar_miss <- median(HAr_miss_draws$sigma_gamma)
sigma.b.true.ar_miss <- median(HAr_miss_draws$sigma_beta)

a0.true.ar_miss <- mean(HAr_miss_draws$alpha0)
a.true.ar_miss <- colMeans(HAr_miss_draws$alpha)
b.true.ar_miss <- colMeans(HAr_miss_draws$beta)
gamma.true.ar_miss <- colMeans(HAr_miss_draws$gamma)
rho.true.ar_miss <- colMeans(HAr_miss_draws$rho)


y.fake.ar_miss <- array(NA, c(n.sims, N, L))
y.fake.ar_miss[, , 1] <- matrix(0, n.sims, N)
for (s in 1:n.sims) {
  for (n in 1:N) {
    for (l in 2:L) {
      y.fake.ar_miss[s, n, l] <-
        rnorm(
          1,
          a0.true.ar_miss 
          + a.true.ar_miss[n]
          + (b.true.ar_miss[1] + gamma.true.ar_miss[1])
          + delta.true.ar_miss[position[n]] * fake_price_std[position[n]]
          + theta.true.ar_miss * fake_home_away[n]
          + rho.true.ar_miss[position[n]]
          + lambda.true.ar_miss[position[n]] * sum(y.fake.ar_miss[s, n, (1:(l - 1))]) / length((1:(l - 1))),
          sigma.y.true.ar_miss
        )
    }
  }
}


stime.fake.HAr <- apply(y.fake.ar, c(2, 3), median)
stime.fake.mix <- apply(y.fake.mix, c(2, 3), median)
stime.fake.HAr.miss <- apply(y.fake.ar_miss, c(2, 3), median)

stime.fake <- list(
  HAr = stime.fake.HAr[, 2:20], 
  MIX = stime.fake.mix[, 2:20],
  `HAr-mis` = stime.fake.HAr.miss[, 2:20]
)
save(stime.fake, file = "stime.fake_for_plotting.rda")


# ggplot version ----------------------------------------------------------
library(reshape2)
library(ggplot2)
library(bayesplot)

plot_data_fake <- melt(stime.fake, varnames = c("player", "match"))
colnames(plot_data_fake)[4] <- "model"

# reorder the levels so ggplot plots them in the order we want
plot_data_fake$model <- factor(plot_data_fake$model, levels = c("HAr",  "MIX", "HAr-mis"))

ruolo_map <- data.frame(
  ruolo = factor(position, labels = c("Forward", "Midfielder", "Defender", "Goalkeeper")), 
  player = 1:length(position)
)
plot_data_fake <- left_join(plot_data_fake, ruolo_map, by = "player")

ggplot(plot_data_fake, aes(x = model, y = value, color = ruolo)) +
  geom_point(
    position = position_jitter(), 
    size = 1.75,
    alpha = 0.3
  ) + 
  scale_y_continuous(expand = c(0.025, 0), breaks = seq(0, 9, by = 3)) +
  scale_x_discrete(expand = c(0, 0.5)) +
  scale_color_hue("Position") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  labs(
    x = "", 
    y = "Rating", 
    title = "Simulated ratings under each model", 
    subtitle = "for hypothetical players differing only by position"
  ) +
  theme_default(base_size = 15) + 
  xaxis_text(face = "bold")

ggsave("../ImagesPaper/fake_data_gg.pdf", width = 6, height = 4)


 #faceted image

ggplot(plot_data_fake, aes(x = model, y = value)) +
  geom_point(
    position = position_jitter(), 
    size = 1.75,
    alpha = 0.3
  ) + 
  scale_y_continuous(expand = c(0.025, 0), breaks = seq(0, 9, by = 3)) +
  scale_x_discrete(expand = c(0, 0.5)) +
  scale_color_hue("Position") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  labs(
    x = "", 
    y = "Rating", 
    title = "Simulated ratings under each model", 
    subtitle = "for hypothetical players differing only by position"
  ) +
  theme_default(base_size = 15) + 
  xaxis_text(face = "bold")+
  facet_wrap(~ruolo)

ggsave("../ImagesPaper/fake_data_gg_faceted.pdf", width = 6, height = 4)





