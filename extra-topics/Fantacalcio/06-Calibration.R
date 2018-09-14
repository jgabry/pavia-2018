team_name <- "Napoli"
is_team <- serie$squadra == team_name
sub <- subset(serie, is_team)
sub$player <- sub$Giocatore
sub$position <- factor(position[is_team], labels = c("F", "M", "D", "Gk"))

y_all <- as.matrix(serie[, grep("^f", colnames(serie))])
y_all[is.na(y_all)] <- 0
tsub <- subset(y_all, is_team)
dimnames(tsub) <- list(player = unique(sub$player), match = NULL)
mtsub <- melt(tsub)
mtsub$position <- sub$position

#for coloring the imputed points differently
tmp <- tsub == 0
dimnames(tmp) <- list(player = unique(sub$player), match = NULL)
mtsub$missing <- melt(tmp, value.name = "missing")$missing


t_simul_HAr <- apply(HAr_draws$y_rep, c(2, 3), median)
t_simul_MIX <- apply(MIX_draws$y_rep, c(2, 3), median)
t_simul_HAr_miss <- apply(HAr_miss_draws$y_rep, c(2, 3), median)

t_25_HAr <- apply(HAr_draws$y_rep, c(2,3), function(x) quantile(x, 0.25))
t_75_HAr <- apply(HAr_draws$y_rep, c(2,3), function(x) quantile(x, 0.75))
t_25_MIX <- apply(MIX_draws$y_rep, c(2,3), function(x) quantile(x, 0.25))
t_75_MIX <- apply(MIX_draws$y_rep, c(2,3), function(x) quantile(x, 0.75))
t_25_HAr_miss <- apply(HAr_miss_draws$y_rep, c(2,3), function(x) quantile(x, 0.25))
t_75_HAr_miss <- apply(HAr_miss_draws$y_rep, c(2,3), function(x) quantile(x, 0.75))

out_sample_HAr <-
  apply(HAr_draws$y_twiddle, c(2, 3), median)
out_sample_MIX <-
  apply(MIX_draws$y_twiddle, c(2, 3), median)
out_sample_HAr_miss <-
  apply(HAr_miss_draws$y_twiddle, c(2, 3), median)

t_25_HAr_out <- apply(HAr_draws$y_twiddle, c(2,3), function(x) quantile(x, 0.25))
t_75_HAr_out <- apply(HAr_draws$y_twiddle, c(2,3), function(x) quantile(x, 0.75))
t_25_MIX_out <- apply(MIX_draws$y_twiddle, c(2,3), function(x) quantile(x, 0.25))
t_75_MIX_out <- apply(MIX_draws$y_twiddle, c(2,3), function(x) quantile(x, 0.75))
t_25_HAr_miss_out <- apply(HAr_miss_draws$y_twiddle, c(2,3), function(x) quantile(x, 0.25))
t_75_HAr_miss_out <- apply(HAr_miss_draws$y_twiddle, c(2,3), function(x) quantile(x, 0.75))

q_HAr_50 <- cbind(t_simul_HAr, out_sample_HAr)
q_MIX_50 <- cbind(t_simul_MIX, out_sample_MIX)
q_HAr_miss_50 <- cbind(t_simul_HAr_miss, out_sample_HAr_miss)

q_HAr_25 <- cbind(t_25_HAr, t_25_HAr_out)
q_HAr_75 <- cbind(t_75_HAr, t_75_HAr_out)
q_MIX_25 <- cbind(t_25_MIX, t_25_MIX_out)
q_MIX_75 <- cbind(t_75_MIX, t_75_MIX_out)
q_HAr_miss_25 <- cbind(t_25_HAr_miss, t_25_HAr_miss_out)
q_HAr_miss_75 <- cbind(t_75_HAr_miss, t_75_HAr_miss_out)

make_pred_data <- function(is_team, players, q_25, q_50, q_75) {
  t_25_sub <- subset(q_25, is_team)
  t_50_sub <- subset(q_50, is_team)
  t_75_sub <- subset(q_75, is_team)
  dimnames(t_25_sub) <- dimnames(t_50_sub) <- dimnames(t_75_sub) <- 
    list(player = unique(players), match = NULL)
  
  mt_25_sub <- melt(t_25_sub)
  mt_50_sub <- melt(t_50_sub)
  mt_75_sub <- melt(t_75_sub)
  
  data.frame(
    player = mt_50_sub$player,
    match = mt_50_sub$match, 
    mid = mt_50_sub$value, 
    lo = mt_25_sub$value, 
    hi = mt_75_sub$value
  )
}

pred_data_HAr <- make_pred_data(is_team, sub$player, q_HAr_25, q_HAr_50, q_HAr_75)
pred_data_MIX <- make_pred_data(is_team, sub$player, q_MIX_25, q_MIX_50, q_MIX_75)
pred_data_HAr_miss <- make_pred_data(is_team, sub$player, q_HAr_miss_25, q_HAr_miss_50, q_HAr_miss_75)

calib_plot <- function(pred_data, observed_data, model_name) {
  # observed_data is the mtsub data.frame created earlier
  
  position_lookup <-
    observed_data %>% 
    group_by(player) %>% 
    summarise(pos = first(position))
  
  label_w_position <- function(player_name) {
    paste0(player_name, " (", with(position_lookup, pos[player == player_name]),")")
  }
  
  ggplot() + 
    geom_vline(xintercept = 20, size = .5, color="black")+
    geom_ribbon(
      aes(x = match, ymin = lo, ymax = hi), 
      data = subset(pred_data, match != 1),
      fill = color_scheme_get("gray")[[2]]
    ) + 
    geom_line(
      aes(x = match, y = mid), 
      data = subset(pred_data, match != 1),
      size = 1,
      color = color_scheme_get("gray")[[4]]
    ) + 
    geom_point(
      aes(x = match, y = value, color = missing), 
      data = subset(observed_data, match != 1), 
      # alpha = 0.8, 
      show.legend = FALSE, 
      size = .5
    ) + 
    scale_color_manual(values = c(color_scheme_get("blue")[[4]], 
                                  color_scheme_get("red")[[4]])) + 
    facet_wrap("player", scales = "free", labeller = as_labeller(label_w_position)) + 
    #lims(y = c(0, 20)) + 
    scale_x_continuous(breaks = c(10, 20, 30)) + 
    labs(x = "Match", y = "Rating", 
         title = paste("Calibration for the", model_name, "model"), 
         subtitle = "for selected team Napoli") +
    theme_default(base_size = 15) + 
    facet_text(size = rel(0.45)) + 
    xaxis_text(size = rel(0.7)) + 
    yaxis_text(size = rel(0.7)) +
    xaxis_title(size = rel(0.7)) + 
    yaxis_title(size = rel(0.7))
}

calib_plot(pred_data_HAr, observed_data = mtsub, model_name = "HAr")
ggsave("../ImagesPaper/HArCalibrationNapoli2.pdf", width = 6, height = 4.5)

calib_plot(pred_data_MIX, observed_data = mtsub, model_name = "MIX")
ggsave("../ImagesPaper/MIXCalibrationNapoli2.pdf", width = 6, height = 4.5)

calib_plot(pred_data_HAr_miss, observed_data = mtsub, model_name = "HAr-mis")
ggsave("../ImagesPaper/HArmisCalibrationNapoli2.pdf", width = 6, height = 4.5)

