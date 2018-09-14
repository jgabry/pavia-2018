library(bayesplot)
library(ggplot2)
library(reshape2)
library(dplyr)


# Plot cumulative total ratings

t_simul_ar <- apply(HAr_draws$y_rep, c(2, 3), median)
t_simul_ar_mix <- apply(MIX_draws$y_rep, c(2, 3), median)
t_simul_ar_miss <- apply(HAr_miss_draws$y_rep, c(2, 3), median)

team_name <- "Napoli"
is_team <- serie$squadra == team_name
sub <- subset(serie, is_team)
sub$player <- sub$Giocatore
sub$position <- factor(position[is_team], labels = c("F", "M", "D", "Gk"))

melt_t <- function(tsub, sub, model_name) {
  dimnames(tsub) <- list(player = unique(sub$player), match = NULL)
  mtsub <- melt(tsub)
  mtsub$position <- sub$position
  plotdata <- mtsub %>% group_by(player) %>% mutate(cumrating = cumsum(value))
  plotdata$model <- model_name
  plotdata
}
t_obs <- melt_t(subset(y, is_team), sub, "Observed")
t_HAr <- melt_t(subset(t_simul_ar, is_team), sub, "HAr")
t_MIX <- melt_t(subset(t_simul_ar_mix, is_team), sub, "MIX")
t_HAr_mis <- melt_t(subset(t_simul_ar_miss, is_team), sub, "HAr-mis")
plotdata <- bind_rows(t_obs, t_HAr,  t_MIX, t_HAr_mis)
plotdata$model <- factor(plotdata$model, levels = c("Observed", "HAr", "HAr-mis", "MIX"))

lookup <- sub[, c("player", "position")]
label_w_position <- function(player_name) {
  paste0(player_name, " (", with(lookup, position[player == player_name]),")")
}

ggplot(subset(plotdata, model != "Observed"), 
       aes(x = match, y = cumrating, color = model)) + 
  geom_line(aes(linetype = "Observed"), 
            data = subset(plotdata, model == "Observed"), 
            color = "black") + 
  geom_line() +
  scale_color_hue("") +
  scale_linetype_manual("", values = 2, labels = "Observed") +
  facet_wrap("player", labeller = as_labeller(label_w_position), 
             scales = "free") +
  labs(x = "Match", y = "Cumulative Ratings", 
       title = "Observed vs predicted cumulative ratings", 
       subtitle = "for selected team Napoli") +
  theme_default(base_size = 15) + 
  facet_text(size = rel(0.45)) + 
  xaxis_text(size = rel(0.7)) + 
  yaxis_text(size = rel(0.7)) + 
  coord_fixed() + 
  legend_move("top")

ggsave("../ImagesPaper/NapoliPostCum_gg.pdf", width = 7, height = 7)

#no color version

ggplot(subset(plotdata, model != "Observed"), 
       aes(x = match, y = cumrating, color = model)) + 
  geom_line(aes(linetype = "Observed"), 
            data = subset(plotdata, model == "Observed"), 
            color = "black") + 
  geom_line() +
  scale_fill_grey()+
  scale_color_grey() +
  scale_linetype_manual("", values = 2, labels = "Observed") +
  facet_wrap("player", labeller = as_labeller(label_w_position), 
             scales = "free") +
  labs(x = "Match", y = "Cumulative Ratings", 
       title = "Observed vs predicted cumulative ratings", 
       subtitle = "for selected team Napoli") +
  theme_default(base_size = 15) + 
  facet_text(size = rel(0.45)) + 
  xaxis_text(size = rel(0.7)) + 
  yaxis_text(size = rel(0.7)) + 
  coord_fixed() + 
  legend_move("top")

ggsave("../ImagesPaper/NapoliPostCum_gg_grey.pdf", width = 7, height = 7)





# PPC (histograms of test stats)

ppc_mean_plots <- ppc_sd_plots <- ppc_min_plots <- ppc_max_plots <- ppc_median_plots <- list()
ysub <- c(HAr_dat$y[,2:20])
yrep_HAr <- HAr_draws$y_rep[,,2:20]
dim(yrep_HAr) <- c(2000, 237 * 19)
color_scheme_set("red")
ppc_mean_plots[["HAr"]] <- ppc_stat(ysub, yrep_HAr, stat = "mean")
ppc_sd_plots[["HAr"]] <- ppc_stat(ysub, yrep_HAr, stat = "sd")
ppc_min_plots[["HAr"]] <- ppc_stat(ysub, yrep_HAr, stat = "min")
ppc_max_plots[["HAr"]] <- ppc_stat(ysub, yrep_HAr, stat = "max")
ppc_median_plots[["HAr"]] <- ppc_stat(ysub, yrep_HAr, stat = "median")
print(c(ppc_mean_plots, ppc_sd_plots, ppc_min_plots, ppc_max_plots, ppc_median_plots))

color_scheme_set("blue")
yrep_MIX <- MIX_draws$y_rep[,,2:20]
dim(yrep_MIX) <- c(2000, 237 * 19)
ppc_mean_plots[["MIX"]] <- ppc_stat(ysub, yrep_MIX, stat = "mean")
ppc_sd_plots[["MIX"]] <- ppc_stat(ysub, yrep_MIX, stat = "sd")
ppc_min_plots[["MIX"]] <- ppc_stat(ysub, yrep_MIX, stat = "min", binwidth = 0.05)
ppc_max_plots[["MIX"]] <- ppc_stat(ysub, yrep_MIX, stat = "max")
ppc_median_plots[["MIX"]] <- ppc_stat(ysub, yrep_MIX, stat = "median")
print(c(ppc_mean_plots, ppc_sd_plots, ppc_min_plots, ppc_max_plots, ppc_median_plots))

mean_pure <- mean(y[y != 0.001 & y != 0])
sd_pure <- sd(y[y != 0.001 & y != 0])
max_pure <- max(y[y != 0.001 & y != 0])
min_pure <- min(y[y != 0.001 & y != 0])
median_pure <- median(y[y != 0.001 & y != 0])


source("ppc_stat_for_missing_y.R")
yrep_HAr_miss <- HAr_miss_draws$y_rep[,,2:20]
dim(yrep_HAr_miss) <- c(2000, 237 * 19)
color_scheme_set("green")
ppc_mean_plots[["HAr_mis"]] <- ppc_stat_for_missing_y(yval = mean_pure, yrep_HAr_miss, stat = "mean")
ppc_sd_plots[["HAr_mis"]] <- ppc_stat_for_missing_y(yval = sd_pure, yrep_HAr_miss, stat = "sd")
ppc_min_plots[["HAr_mis"]] <- ppc_stat_for_missing_y(yval = min_pure, yrep_HAr_miss, stat = "min")
ppc_max_plots[["HAr_mis"]] <- ppc_stat_for_missing_y(yval = max_pure, yrep_HAr_miss, stat = "max")
ppc_median_plots[["HAr_mis"]] <- ppc_stat_for_missing_y(yval = median_pure, yrep_HAr_miss, stat = "median")
print(c(ppc_mean_plots, ppc_sd_plots, ppc_min_plots, ppc_max_plots, ppc_median_plots))

all_plots <-
  c(ppc_mean_plots,
    ppc_median_plots,
    ppc_sd_plots,
    ppc_min_plots,
    ppc_max_plots)

pdf("../ImagesPaper/PosteriorAllModels_gg.pdf", width = 6)
bayesplot_grid(
  plots = all_plots,
  legends = FALSE,
  subtitles = rep(c("Mean", "Median", "SD", "Min", "Max"), each = 3),
  titles = c("HAr model", "MIX model", "HAr-mis model", rep("", 12)),
  grid_args = list(ncol = 3)
)
dev.off()


# Maximum statistic for the HAr-mis model
source("ppc_stat_for_missing_y.R")
Positions <- c("Forward", "Midfield", "Defense", "Goalkeeper")
mean_pure_ruolo <- c()
sd_pure_ruolo <- c()
min_pure_ruolo <- c()
max_pure_ruolo <- c()

for (j in 1:J) {
  mean_pure_ruolo[j] <- mean(y[y != 0.001 & y != 0 & position == j])
  sd_pure_ruolo[j] <- sd(y[y != 0.001 & y != 0 & position == j])
  min_pure_ruolo[j] <- min(y[y != 0.001 & y != 0 & position == j])
  max_pure_ruolo[j] <- max(y[y != 0.001 & y != 0 & position == j])
}

ppc_max_plots_har_by_ruolo <- list()
color_scheme_set("green")
for (j in 1:J) {
  yrep_HAr_miss_j <- HAr_miss_draws$y_rep[,position==j,]
  dim(yrep_HAr_miss_j) <- c(nrow(yrep_HAr_miss_j), prod(dim(yrep_HAr_miss_j)[2:3]))
  
  ppc_max_plots_har_by_ruolo[[Positions[j]]] <- 
    ppc_stat_for_missing_y(yval = max_pure_ruolo[j], yrep_HAr_miss_j, stat = "max")
}


pdf("../ImagesPaper/Post_HARMissing_Max_gg.pdf", height = 2)
bayesplot_grid(plots = ppc_max_plots_har_by_ruolo, 
               subtitles = Positions, 
               titles = c("HAr-mis model: maximum by position", "", "", ""),
               legends = FALSE,
               xlim = c(9, 18),
               grid_args = list(nrow = 1))
dev.off()


# 
# ppc_sd_plots_har_by_ruolo <- list()
# color_scheme_set("green")
# for (j in 1:J) {
#   yrep_HAr_miss_j <- HAr_miss_draws$y_rep[,position==j,]
#   dim(yrep_HAr_miss_j) <- c(nrow(yrep_HAr_miss_j), prod(dim(yrep_HAr_miss_j)[2:3]))
#   
#   ppc_sd_plots_har_by_ruolo[[Positions2[j]]] <- 
#     ppc_stat_for_missing_y(yval = sd_pure_ruolo[j], yrep_HAr_miss_j, stat = "sd")
# }
# pdf("../ImagesPaper/Post_HARMissing_SD_gg.pdf", height = 2)
# bayesplot_grid(plots = ppc_sd_plots_har_by_ruolo, 
#                subtitles = Positions, 
#                titles = c("HAr-mis model: SD by position", "", "", ""),
#                legends = FALSE,
#                # xlim = c(6, 18),
#                grid_args = list(nrow = 1))
# dev.off()
