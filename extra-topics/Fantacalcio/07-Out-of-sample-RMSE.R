#RMSE as distributions

  #Hmm, I'm not sure if we should replace them or use both. Maybe we essentially 
  #keep what we have but make the point estimates using the distributions of RMSE 
  #rather than compute the RMSE using point estimates

#Scenario A


rmse <- function(x, y, na.rm = TRUE) sqrt(mean((x - y)^2, na.rm = na.rm))


y_all <- as.matrix(serie[, grep("^f", colnames(serie))])
y_all[is.na(y_all)] <- 0
y2 <- y_all[, (L+1):ncol(y_all)]
y2_miss=y2
y2_miss[y2==0]=NA

S <- nrow(HAr_draws$beta)
RMSE_distr_MIX <- RMSE_distr_HAr <- RMSE_distr_HAr_miss <- matrix(NA, S, N)

for (s in 1:S) {
  for (n in 1:N) {
    RMSE_distr_HAr[s, n] <- rmse(HAr_draws$y_twiddle[s, n, ], y2[n, ])
    RMSE_distr_MIX[s, n] <- rmse(MIX_draws$y_twiddle[s, n, ], y2[n, ])
    RMSE_distr_HAr_miss[s, n] <- rmse(HAr_miss_draws$y_twiddle[s, n, ], as.vector(y2_miss[n, ]), na.rm=TRUE)
  }
}


b <- apply(RMSE_distr_HAr, 2, mean)
d <- apply(RMSE_distr_MIX, 2, mean)
e=c()
for (n in 1:N){
e[n] <- mean(RMSE_distr_HAr_miss[,n], nan.rm=TRUE)}

RMSE_MIX <- RMSE_HAr <- RMSE_HAr_miss <- c()
for (j in 1:J) {
  RMSE_HAr[j] <- mean(b[position == j])
  RMSE_MIX[j] <- mean(d[position == j])
  RMSE_HAr_miss[j] <- mean(e[position == j], na.rm=TRUE)
}


# ggplot version ----------------------------------------------------------
library(ggplot2)
library(bayesplot)

plotdata <- data.frame(
  value = c(RMSE_HAr,  RMSE_HAr_miss, RMSE_MIX), 
  position = rep(1:4, times = 3),
  model = gl(3, 4, labels = c("HAr", "HAr-mis", "MIX"))
)

ggplot(plotdata, aes(x = position, y = value, color = model, fill = model)) + 
  geom_line(size = 1.5, alpha = 0.5) + 
  geom_point(shape = 21, size = 4) +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Forward", "Midfield", "Defense", "Goalkeeper"),
    expand = c(0.05, .25)
  ) + 
  scale_y_continuous(limits = c(0, 5.5)) +
  labs(
    x = "", 
    y = expression(bar(RMSE)[j]), 
    title = "RMSE of out-of-sample predictions",
    subtitle = "by model and position"
  ) +
  scale_color_hue("") +
  scale_fill_hue("") +
  theme_default(base_size = 15) + 
  legend_move("top") +
  xaxis_text(face = "bold", size = rel(0.9))

ggsave("../ImagesPaper/RMSE_out_distr_gg_new.pdf", width = 5.5, height = 4)


#no colors

#plotdata_long=melt(plotdata, id.vars="position")

ggplot(plotdata, aes(x = position, y = value, linetype = model)) + 
  geom_line(aes(linetype=model),size = 1.5, alpha = 0.5) + 
  geom_point(shape = 21, size = 4) +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Forward", "Midfield", "Defense", "Goalkeeper"),
    expand = c(0.05, .25)
  ) + 
  scale_y_continuous(limits = c(0, 5.5)) +
  labs(
    x = "", 
    y = expression(bar(RMSE)[j]), 
    title = "RMSE of out-of-sample predictions",
    subtitle = "by model and position"
  ) +
  scale_linetype_manual(values=c("solid", "dotted", "twodash")  )+
  #scale_color_hue("") +
  #scale_fill_hue("") +
  theme_default(base_size = 15) + 
  legend_move("top") +
  xaxis_text(face = "bold", size = rel(0.9))
  #theme(legend.position="top")

ggsave("../ImagesPaper/RMSE_out_distr_gg_nocolor.pdf", width = 5.5, height = 4)
