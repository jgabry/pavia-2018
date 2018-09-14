
#########################################
##Section 3: DATA
#########################################


library(rstan)
library(shinystan)
library(dplyr)
library(ggplot2)
library(bayesplot)

load("serie.rda")

L <- 20
y <- as.matrix(serie[, 4:(L + 3)])
N <- nrow(y)
y[is.na(y)] <- 0     #-----> zero values where "NA"


# I clean the dataset: I consider only the players who had played at least one third of the
# matches for the first part of the season.

countZERI <- apply(y, 1, function(x) sum(x == 0))
serie <- subset(serie, countZERI < 13)
y <- as.matrix(serie[, 4:(L + 3)])
N <- nrow(y)

# assign zero values
y[is.na(y)] <- 0  

## Predictors
n_matches <- 38
position <- as.numeric(as.factor(serie$ruolo))
J <- length(unique(position))
team <- as.numeric(as.factor(serie$squadra))
L_twiddle <- n_matches - L + 1


# team cluster lookup table
cluster_map <- data.frame(
  squadra = c("Palermo", "Carpi", "Frosinone", "Verona",
           "Genoa", "Sampdoria", "Empoli", "Udinese",
           "Bologna", "Chievo", "Atalanta", "Torino",
           "Fiorentina", "Milan", "Sassuolo", "Lazio",
           "Juventus", "Roma", "Inter", "Napoli"), 
  cluster = rep(1:5, each = 4)
)

# add cluster variable to serie
serie <- left_join(serie, cluster_map, by = "squadra")
K <- length(unique(serie$cluster))

# team clusters for opponents
opponent <- serie[, grep("avversario", colnames(serie))]
opp_team <- opponent
opp_team[] <- cluster_map$cluster[match(unlist(opponent), cluster_map$squadra)]

opp_team_twiddle <- as.matrix(opp_team[, L:n_matches])
opp_team <- as.matrix(opp_team[, 1:(L-1)])

# home/away
home_away <- 
  serie[, grep("^home", colnames(serie))] %>% 
  mutate_all(.funs = function(x) ifelse(x == "A", 0, 1))
home_twiddle <- as.matrix(home_away[, L:n_matches])
home <- as.matrix(home_away[, 1:(L-1)])


# prices of the players
price <- serie$quotiniziale
price_std <- as.vector(scale(price))

frac_played <- matrix(NA, N, L - 1)
for (l in 2:L) {
  frac_played[, l - 1] <-
    apply(y[, 1:l], 1, function(x)
      sum(x != 0) / length(x))
}

avg_rating_lag <- matrix(NA, N, L - 1)
for (l in 2:L) {
  avg_rating_lag[, l - 1] <- apply(y[, 1:l], 1, mean)
}


# plot price vs ratings (for non-missing matches)
y_all <- as.matrix(serie[, grep("^f", colnames(serie))])
average_ratings <- apply(y_all[, 2:39], 1 , mean, na.rm=TRUE)


plot_data <- 
  data.frame(
    price = price_std, 
    y_bar = average_ratings,
    position = factor(position, labels = c("Forward", "Midfield", "Defense", "Goalkeeper"))
  )
  
ggplot(plot_data, aes(x = price, y = y_bar, color = position)) + 
  geom_point(size = 3, alpha = 0.8) + 
  labs(x = "Initial Price (Standardized)", y = "Average Rating") +
  scale_color_hue("") + 
  coord_fixed() +
  scale_y_continuous(breaks = 5:10) +
  theme_default(base_size = 15) + 
  theme(legend.text = element_text(size = rel(0.7))) + 
  legend_move("top")

ggsave("../ImagesPaper/PriceRatings_gg.pdf")

#faceted image

ggplot(plot_data, aes(x = price, y = y_bar)) + 
  geom_point(size = 0.8, alpha = 0.8) + 
  labs(x = "Initial Price (Standardized)", y = "Average Rating") +
  scale_color_hue("") + 
  coord_fixed() +
  scale_y_continuous(breaks = 5:10) +
  theme_default(base_size = 15) + 
  theme(legend.text = element_text(size = rel(0.7))) + 
  legend_move("top")+
  facet_wrap(~position)

ggsave("../ImagesPaper/PriceRatings_gg_faceted_smaller.pdf")



ggplot(plot_data, aes(x = y_bar, fill = position)) + 
  geom_density(alpha = 0.7, color = NA) + 
  geom_line(stat = "density", color = "black", size = 0.1) + 
  labs(x = "Average Rating") +
  scale_fill_hue("") + 
  coord_cartesian(expand = FALSE) +
  theme_default(base_size = 15) + 
  theme(legend.text = element_text(size = rel(0.7))) +
  legend_move("top")

ggsave("../ImagesPaper/RatingsDens_gg.pdf")

#faceted image

ggplot(plot_data, aes(x = y_bar)) + 
  geom_density(alpha = 0.7, color = NA) + 
  geom_line(stat = "density", color = "black", size = 0.1) + 
  labs(x = "Average Rating") +
  scale_fill_hue("") + 
  coord_cartesian(expand = FALSE) +
  theme_default(base_size = 15) + 
  theme(legend.text = element_text(size = rel(0.7))) +
  legend_move("top")+
  facet_wrap(~position)
#+
 # xlim(0,10)

ggsave("../ImagesPaper/RatingsDens_gg_faceted.pdf")



# Number of matches played
matches_played <- data.frame(
  n = apply(y_all[, -1], 1, function(x) sum(!is.na(x))), 
  pos = factor(position, labels = c("Forward", "Midfield", "Defense", "Goalkeeper"))
)
summary(matches_played)

# Average by position
aggregate(n ~ pos, data = matches_played, FUN = "mean")





# Other interesting plots -------------------------------------------------

m <- melt(y_all[, 2:39], varnames = c("player", "match"))
m <- data.frame(pos = position,
                price = price_std,
                player = seq_along(position), 
                squadra = serie$squadra, 
                cluster = serie$cluster, stringsAsFactors = FALSE) %>%
  right_join(m, by = "player") %>%
  mutate(
    pos = factor(pos, labels = c("Forward", "Midfield", 
                                 "Defense", "Goalkeeper")),
    match = rep(1:38, each = 237)
  )

m %>%
  group_by(match, pos) %>%
  summarise(
    ybar = mean(value, na.rm = TRUE),
    ysd = sd(value, na.rm = TRUE)
  ) %>%
  ggplot(aes(x=match, y =ybar, color = pos)) + 
  geom_ribbon(aes(ymin = ybar - ysd, ymax = ybar + ysd, fill=pos), 
              alpha = 0.1, 
              linetype = 2, size = 0.5) +
  geom_line(size = 1) + 
  labs(x = "Match", y = "Average Rating +/- 1 SD") +
  scale_color_hue("") + 
  scale_fill_hue("") +
  scale_x_continuous(breaks = c(1,10,20,30,38)) +
  scale_y_continuous(limits = c(2, 11), breaks = c(4, 6, 8, 10)) +
  theme_default(base_size = 15) + 
  theme(legend.text = element_text(size = rel(0.7))) +
  legend_move("top")

ggsave("../ImagesPaper/AvgRatingsByPosByMatch_gg.pdf")



m2 <- m %>%
  group_by(match, cluster) %>%
  summarise(ybar = mean(value, na.rm = TRUE))
m3 <- m2 %>% 
  group_by(match) %>%
  summarise(ybar = mean(ybar, na.rm = TRUE)) %>%
  mutate(cluster = 0) %>%
  bind_rows(m2)
  
m3 %>%
  filter(cluster %in% 1:5) %>%
  ggplot(aes(x = match, y = ybar)) +
  geom_line(data = m3 %>% filter(cluster == 0), aes(linetype = "Avg"), size = .7) +
  geom_line(aes(color = factor(cluster), size = factor(cluster))) + 
  labs(x = "Match", y = "Average Rating", title = "Average team-cluster rating by match") +
  scale_linetype_manual("", values = 2, labels = "All teams combined") +
  scale_color_hue("Cluster", direction = -1) +
  scale_size_manual(values = seq(.25, 1, length.out = 5), guide = "none") +
  scale_x_continuous(breaks = c(1,10,20,30,38)) +
  scale_y_continuous(limits = c(5, 8), breaks = c(4, 6, 8, 10)) +
  theme_default(base_size = 15) + 
  theme(legend.text = element_text(size = rel(0.7))) +
  legend_move("top")

ggsave("../ImagesPaper/AvgRatingsByTeamByMatch_gg.pdf")

