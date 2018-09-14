# Roster selection
y_all <- as.matrix(serie[, grep("^f", colnames(serie))])
y_all[is.na(y_all)] <- 0
y2 <- y_all[, (L+1):ncol(y_all)]

out_sample_HAr <- apply(HAr_draws$y_twiddle, c(2, 3), median)
out_sample_MIX <- apply(MIX_draws$y_twiddle, c(2, 3), median)
out_sample_HAr_miss <- apply(HAr_miss_draws$y_twiddle, c(2,3), median)
  
## Best rosters
players <- serie$Giocatore
media_HAr <- (apply(out_sample_HAr, 1, sum)) / (L - 1)
media_HAr_miss <- (apply(out_sample_HAr_miss, 1, sum)) / (L - 1)
media_MIX <- c()
for (n in 1:N) {
  if (sum(out_sample_MIX[n, ] != 0) > 14) {
    media_MIX[n] <-
      sum(out_sample_MIX[n, ][out_sample_MIX[n, ] != 0]) / sum(out_sample_MIX[n, ] != 0)
  } else{
    media_MIX[n] <- 0
  }
}

media_vera <- c()
for (n in 1:237) {
  if (sum(y2[n, ] != 0) > 14) {
    media_vera[n] <- sum(y2[n, ][y2[n, ] != 0]) / 19
  } else{
    media_vera[n] <- 0
  }
}



# HAr predicted team
sort_HAr_att <- sort(media_HAr[position == 1], decreasing = T, index.return = T)
sort_HAr_centr <- sort(media_HAr[position == 2], decreasing = T, index.return = T)
sort_HAr_def <- sort(media_HAr[position == 3], decreasing = T, index.return = T)
sort_HAr_port <- sort(media_HAr[position == 4], decreasing = T, index.return = T)

attacco_HAr <- as.vector(players[position == 1])[sort_HAr_att$ix][1:3]
centrocampo_HAr <- as.vector(players[position == 2])[sort_HAr_centr$ix][1:3]
difesa_HAr <- as.vector(players[position == 3])[sort_HAr_def$ix][1:4]
portiere_HAr <- as.vector(players[position == 4])[sort_HAr_port$ix][1]
squadra_HAr <- c(portiere_HAr, difesa_HAr, centrocampo_HAr, attacco_HAr)

  
# MIX predicted team
sort_MIX_att <- sort(media_MIX[position == 1], decreasing = T, index.return = T)
sort_MIX_centr <- sort(media_MIX[position == 2], decreasing = T, index.return = T)
sort_MIX_def <- sort(media_MIX[position == 3], decreasing = T, index.return = T)
sort_MIX_port <- sort(media_MIX[position == 4], decreasing = T, index.return = T)

attacco_MIX <- as.vector(players[position == 1])[sort_MIX_att$ix][1:3]
centrocampo_MIX <- as.vector(players[position == 2])[sort_MIX_centr$ix][1:3]
difesa_MIX <- as.vector(players[position == 3])[sort_MIX_def$ix][1:4]
portiere_MIX <- as.vector(players[position == 4])[sort_MIX_port$ix][1]
squadra_MIX <- c(portiere_MIX, difesa_MIX, centrocampo_MIX, attacco_MIX)


# HAr-mis predicted team
sort_HAr_miss_att <- sort(media_HAr_miss[position==1], decreasing=T, index.return=T)
sort_HAr_miss_centr <- sort(media_HAr_miss[position==2], decreasing=T, index.return=T)
sort_HAr_miss_def <- sort(media_HAr_miss[position==3], decreasing=T, index.return=T)
sort_HAr_miss_port <- sort(media_HAr_miss[position==4], decreasing=T, index.return=T)

attacco_HAr_miss <- as.vector(players[position == 1])[sort_HAr_miss_att$ix][1:3]
centrocampo_HAr_miss <- as.vector(players[position == 2])[sort_HAr_miss_centr$ix][1:3]
difesa_HAr_miss <- as.vector(players[position == 3])[sort_HAr_miss_def$ix][1:4]
portiere_HAr_miss <- as.vector(players[position == 4])[sort_HAr_miss_port$ix][1]
squadra_HAr_miss <- c(portiere_HAr_miss, difesa_HAr_miss, centrocampo_HAr_miss, attacco_HAr_miss)


# True team
sort_vera_att <- sort(media_vera[position == 1], decreasing = T, index.return = T)
sort_vera_centr <- sort(media_vera[position == 2], decreasing = T, index.return = T)
sort_vera_def <- sort(media_vera[position == 3], decreasing = T, index.return = T)
sort_vera_port <- sort(media_vera[position == 4], decreasing = T, index.return = T)

attacco_vero <- as.vector(players[position == 1])[sort_vera_att$ix][1:3]
centrocampo_vero <- as.vector(players[position == 2])[sort_vera_centr$ix][1:3]
difesa_vero <- as.vector(players[position == 3])[sort_vera_def$ix][1:4]
portiere_vero <- as.vector(players[position == 4])[sort_vera_port$ix][1]
squadra_vera <- c(portiere_vero, difesa_vero, centrocampo_vero, attacco_vero)


# Functions to plot the teams on the image of a pitch
soccer_pitch_background <- function() {
  par(
    mfrow = c(1, 1),
    mar = c(0.1, 0.2, 0.2, 0.1),
    mgp = c(2, .5, 0),
    tck = -.0
  )
  par(bg = "chartreuse3")
  par(bty = "n")
  par(xaxt = "n")
  par(yaxt = "n")
  plot(1:2,
       type = "n",
       xlab = " ",
       ylab = " ")
  lines(c(1, 1.2), c(1.2, 1.2), col = "white")
  lines(c(1.2, 1.2), c(1.2, 1.8), col = "white")
  lines(c(1, 1.2), c(1.8, 1.8), col = "white")
  lines(c(1, 1.09), c(1.3, 1.3), col = "white")
  lines(c(1.09, 1.09), c(1.3, 1.7), col = "white")
  lines(c(1, 1.09), c(1.7, 1.7), col = "white")
  lines(c(1, 1), c(2, 1), col = "white")
  lines(c(1, 2), c(1, 1), col = "white")
  lines(c(1, 2), c(2, 2), col = "white")
  lines(c(2, 2), c(1, 2), col = "white")
  lines(c(1.8, 2), c(1.2, 1.2), col = "white")
  lines(c(1.8, 1.8), c(1.2, 1.8), col = "white")
  lines(c(1.8, 2), c(1.8, 1.8), col = "white")
  lines(c(1.91, 2), c(1.3, 1.3), col = "white")
  lines(c(1.91, 2), c(1.7, 1.7), col = "white")
  lines(c(1.91, 1.91), c(1.3, 1.7), col = "white")
  lines(c(1.5, 1.5), c(1, 2), col = "white")
  symbols(
    x = 1.5,
    y = 1.5,
    circles = 0.1,
    inches = FALSE,
    fg = "white",
    add = TRUE
  )
}

plot_team <- function(squadra, port, def, centr, att) {
  op <- par(no.readonly = TRUE)
  on.exit(expr = par(op))
  
  soccer_pitch_background()
  text(1.05, 1.5, paste(squadra[1],"(", round(port$x[1],2), ")" ), cex=1, srt = 90)
  
  text(1.25, 1.8, paste(squadra[2],"(", round(def$x[1],2), ")" ),  cex=1) 
  text(1.25, 1.6, paste(squadra[3],"(", round(def$x[2],2), ")" ),  cex=1)  
  text(1.25, 1.4, paste(squadra[4],"(", round(def$x[3],2), ")" ),  cex=1) 
  text(1.25, 1.2, paste(squadra[5],"(", round(def$x[4],2), ")" ),  cex=1) 
  
  text(1.55, 1.7, paste(squadra[6],"(", round(centr$x[1],2), ")" ),  cex=1) 
  text(1.55, 1.5, paste(squadra[7],"(", round(centr$x[2],2), ")" ),  cex=1)  
  text(1.55, 1.3, paste(squadra[8],"(", round(centr$x[3],2), ")" ),  cex=1) 
  
  text(1.85, 1.7, paste(squadra[9],"(", round(att$x[1],2), ")" ),  cex=1) 
  text(1.85, 1.5, paste(squadra[10],"(", round(att$x[2],2), ")" ),  cex=1)  
  text(1.85, 1.3, paste(squadra[11],"(", round(att$x[3],2), ")" ),  cex=1) 
}


# make the plots
plot_team(squadra_vera, sort_vera_port, sort_vera_def, sort_vera_centr, sort_vera_att)
dev.copy2pdf(file="../ImagesPaper/observed_team.pdf")

plot_team(squadra_HAr, sort_HAr_port, sort_HAr_def, sort_HAr_centr, sort_HAr_att)
dev.copy2pdf(file="../ImagesPaper/HAr_team.pdf")

plot_team(squadra_MIX, sort_MIX_port, sort_MIX_def, sort_MIX_centr, sort_MIX_att)
dev.copy2pdf(file="../ImagesPaper/MIX_team.pdf")

plot_team(squadra_HAr_miss, sort_HAr_miss_port, sort_HAr_miss_def, sort_HAr_miss_centr, sort_HAr_miss_att)
dev.copy2pdf(file="../ImagesPaper/HArMiss_team.pdf")




## Models scores

     ##Figure 11 paper


# Also, I really like the plots with the best teams based on the out-of-sample predictions. 
# Nice! I'm thinking that we should add some more to that section, because it's a cool idea
# and I think we could do some interesting things. For example, what do you think about 
# comparing the models based on the sum of the ratings for the 11 players in the
# best teams? That is, we take the best teams from those plots but use the observed ratings 
# for those players to compute a "score" for each model. Or something like that?
# 


#HAr

sum_att_HAr = sum_centr_HAr = sum_def_HAr = c()
for (i in 1:3) {
  sum_att_HAr[i] <-
    media_HAr[(1:N)[players == as.vector(players[position == 1])[sort_vera_att$ix][i]]]
  sum_centr_HAr[i] <-
    media_HAr[(1:N)[players == as.vector(players[position == 2])[sort_vera_centr$ix][i]]]
}

for (i in 1:4) {
  sum_def_HAr[i] <-
    media_HAr[(1:N)[players == as.vector(players[position == 3])[sort_vera_def$ix][i]]]
}

sum_port_HAr <-
  media_HAr[(1:N)[players == as.vector(players[position == 4])[sort_vera_port$ix][1]]]
scoring_HAr <-
  sum(sum(sum_att_HAr) + sum(sum_centr_HAr) + sum(sum_def_HAr) + sum_port_HAr)



#MIX

sum_att_MIX = sum_centr_MIX = sum_def_MIX = c()
for (i in 1:3) {
  sum_att_MIX[i] <-
    media_MIX[(1:N)[players == as.vector(players[position == 1])[sort_vera_att$ix][i]]]
  sum_centr_MIX[i] <-
    media_MIX[(1:N)[players == as.vector(players[position == 2])[sort_vera_centr$ix][i]]]
}

for (i in 1:4) {
  sum_def_MIX[i] <-
    media_MIX[(1:N)[players == as.vector(players[position == 3])[sort_vera_def$ix][i]]]
}

sum_port_MIX <-
  media_MIX[(1:N)[players == as.vector(players[position == 4])[sort_vera_port$ix][1]]]
scoring_MIX <-
  sum(sum_att_MIX) + sum(sum_centr_MIX) + sum(sum_def_MIX) + sum_port_MIX


# HAr-mis

sum_att_HAr_miss = sum_centr_HAr_miss = sum_def_HAr_miss = c()
for (i in 1:3) {
  sum_att_HAr_miss[i] <-
    media_HAr_miss[(1:N)[players == as.vector(players[position == 1])[sort_vera_att$ix][i]]]
  sum_centr_HAr_miss[i] <-
    media_HAr_miss[(1:N)[players == as.vector(players[position == 2])[sort_vera_centr$ix][i]]]
}

for (i in 1:4) {
  sum_def_HAr_miss[i] <-
    media_HAr_miss[(1:N)[players == as.vector(players[position == 3])[sort_vera_def$ix][i]]]
}

sum_port_HAr_miss <-
  media_HAr_miss[(1:N)[players == as.vector(players[position == 4])[sort_vera_port$ix][1]]]
scoring_HAr_miss <-
  sum(sum_att_HAr_miss) + sum(sum_centr_HAr_miss) + sum(sum_def_HAr_miss) + sum_port_HAr_miss


#vero

sum_port_vero <- sort_vera_port$x[1]
sum_def_vero <- c(sort_vera_def$x[1],
                  sort_vera_def$x[2],
                  sort_vera_def$x[3],
                  sort_vera_def$x[4])
sum_centr_vero <- c(sort_vera_centr$x[1], sort_vera_centr$x[2],
                    sort_vera_centr$x[3])
sum_att_vero <-
  c(sort_vera_att$x[1], sort_vera_att$x[2], sort_vera_att$x[3])

scoring_vero <-
  sum(sum_att_vero) + sum(sum_centr_vero) + sum(sum_def_vero) + sum(sum_port_vero)


library(ggplot2)
library(bayesplot)
library(reshape2)

models <- c("Observed", "HAr",  "MIX", "HAr-mis")
V <-
  c(sum(sum_port_vero),
    sum(sum_def_vero),
    sum(sum_centr_vero),
    sum(sum_att_vero))
A <-
  c(sum(sum_port_HAr),
    sum(sum_def_HAr),
    sum(sum_centr_HAr),
    sum(sum_att_HAr))

C <-
  c(sum(sum_port_MIX),
    sum(sum_def_MIX),
    sum(sum_centr_MIX),
    sum(sum_att_MIX))
D <-
  c(
    sum(sum_port_HAr_miss),
    sum(sum_def_HAr_miss),
    sum(sum_centr_HAr_miss),
    sum(sum_att_HAr_miss)
  )

M <- rbind(V, A, C, D)

plotdata <- melt(t(M), varnames = c("position", "model"))
plotdata$model <- factor(plotdata$model, labels = models)
plotdata$position <- factor(plotdata$position, levels = 4:1)
ggplot(plotdata, aes(x = model, 
                     y = value, 
                     fill = position, 
                     alpha = model == "Observed",
                     color = model == "Observed")) + 
  geom_col() + 
  scale_fill_hue(
    "Position", 
    labels = c("Forward", "Midfield", "Defense", "Goalkeeper")
  ) +
  scale_x_discrete(
    labels = c("Observed", "HAr",  "MIX", "HAr-mis")
  ) + 
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  scale_color_manual(values = c("white", NA), guide = "none") +
  scale_alpha_manual(values = c(1, 0.5), guide = "none") +
  labs(
    x = "", 
    y = "Total Score", 
    title = "Predictions for the team of best observed players"
  ) +
  theme_default(base_size = 15) + 
  xaxis_text(face = "bold")
ggsave("../ImagesPaper/Models_scores_best_gg.pdf", width = 7)
