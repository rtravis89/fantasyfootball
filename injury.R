#NFL Injury Data
library(tidyverse)
library(rstan)

options(tibble.print_max = 100)
setwd("C:\\Users\\rtravis\\Documents\\NFL\\scripts")

player <- read_csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\PLAYER.csv")
game <- read_csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\GAME.csv")
injury <- read_csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\INJURY.csv")
offense <- read_csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\OFFENSE.csv")

#1. all players who were injured
#2. All players

#players who had any injury indication in 2017
inj.players <- game %>% filter(seas == 2017 & wk %in% c(1:17)) %>%
  select(gid) %>% left_join(injury, by = c("gid" = "gid")) %>% select(player, gstat) %>%
  mutate(injured = ifelse(gstat %in% c("IR", "Out"), 1, 0)) %>% group_by(player) %>%
  summarize(games.inj = sum(injured)) %>% mutate(injured = ifelse(games.inj > 0, 1, 0)) %>%
  left_join(player, by = c("player" = "player")) %>% select(player, pos1, games.inj, injured) 
#All players who played an offensive snap in 2017
all.players <- game %>% filter(seas == 2017 & wk %in% c(1:17)) %>%
  select(gid) %>% left_join(offense, by = c("gid" = "gid")) %>%
  select(player) %>% left_join(player, by = c("player" = "player")) %>%
  select(player, pos1) %>% group_by(player) %>% distinct()
#Join the two tables. Players who didn't get hurt will have missing values for injured
inj.data <- all.players %>% full_join(inj.players, by = c("player" = "player")) %>%
  mutate(pos = ifelse(is.na(pos1.x),pos1.y,pos1.x)) %>% 
  ungroup() %>% select(pos,injured) %>%
  filter(pos %in% c("QB","RB","TE","WR"))
#missing values for injured indicator should be 0
inj.data$injured[is.na(inj.data$injured)] <- 0

#Fit intercept only model in Stan
#M is number of columns, N is number of rows
data.list <- list(N = nrow(inj.data), y = inj.data$injured)

fit <- stan(file ='intercept_LR.stan',
            data = data.list)

#Diagnostics
traceplot(fit)

#Posterior Samples
params <- extract(fit)

#Posterior Predictive Check
p_hat_ppc = data.frame(p_hat_ppc = params$p_hat_ppc)
OVERALL.mean <- mean(data.list$y)
QB.mean <- mean(data.list$y[inj.data$pos == "QB"])
RB.mean <- mean(data.list$y[inj.data$pos == "RB"])
TE.mean <- mean(data.list$y[inj.data$pos == "TE"])
WR.mean <- mean(data.list$y[inj.data$pos == "WR"])

ggplot(p_hat_ppc, aes(p_hat_ppc)) + geom_histogram(binwidth = 1/100) +
  geom_vline(aes(xintercept = OVERALL.mean, col = "Overall")) +
  geom_vline(aes(xintercept = QB.mean, col = "QB")) +
  geom_vline(aes(xintercept = RB.mean, col = "RB")) +
  geom_vline(aes(xintercept = TE.mean, col = "TE")) +
  geom_vline(aes(xintercept = WR.mean, col = "WR")) +
  scale_color_manual(name = "Position", 
                     values = c(Overall = "black",
                                QB = "red",
                                RB = "blue",
                                TE = "green",
                                WR = "purple")) +
  ggtitle("Posterior Predictive Distribution for Probability of Injury") +
  theme_bw()


#Enrich model by adding position variable

#Fit model with position variable
data.list <- list(N = nrow(inj.data), N_pos = 4, pos = as.numeric(factor(inj.data$pos)),
                  y = inj.data$injured)

fit2 <- stan(file ='LR2.stan',
            data = data.list)

#Diagnostics
traceplot(fit2)

#Posterior Samples
params <- extract(fit2)

p_hat_ppc = data.frame(params)

library(gridExtra)
o <- ggplot(p_hat_ppc, aes(p_hat_ppc)) + geom_histogram(binwidth = 1/100) +
  geom_vline(aes(xintercept = OVERALL.mean, col = "red")) + theme_bw()
a <- ggplot(p_hat_ppc, aes(p_hat_QB_ppc)) + geom_histogram(binwidth = 1/25) +
  geom_vline(aes(xintercept = QB.mean, col = "red")) + theme_bw()
b <- ggplot(p_hat_ppc, aes(p_hat_RB_ppc)) + geom_histogram(binwidth = 1/25) +
  geom_vline(aes(xintercept = RB.mean, col = "red")) + theme_bw()
c <- ggplot(p_hat_ppc, aes(p_hat_TE_ppc)) + geom_histogram(binwidth = 1/25) +
  geom_vline(aes(xintercept = TE.mean, col = "red")) + theme_bw()
d <- ggplot(p_hat_ppc, aes(p_hat_WR_ppc)) + geom_histogram(binwidth = 1/25) +
  geom_vline(aes(xintercept = WR.mean, col = "red")) + theme_bw()
grid.arrange(o,a,b,c,d)



#Adding height and Weight
#players who had any injury indication in 2017
inj.players <- game %>% filter(seas == 2017 & wk %in% c(1:17)) %>%
  select(gid) %>% left_join(injury, by = c("gid" = "gid")) %>% select(player, gstat) %>%
  mutate(injured = ifelse(gstat %in% c("IR", "Out"), 1, 0)) %>% group_by(player) %>%
  summarize(games.inj = sum(injured)) %>% mutate(injured = ifelse(games.inj > 0, 1, 0)) %>%
  left_join(player, by = c("player" = "player")) %>% select(player, pos1, height, weight, games.inj, injured) 
#All players who played an offensive snap in 2017
all.players <- game %>% filter(seas == 2017 & wk %in% c(1:17)) %>%
  select(gid) %>% left_join(offense, by = c("gid" = "gid")) %>%
  select(player) %>% left_join(player, by = c("player" = "player")) %>%
  select(player, pos1, height, weight) %>% group_by(player) %>% distinct()
#Join the two tables. Players who didn't get hurt will have missing values for injured
inj.data <- all.players %>% full_join(inj.players, by = c("player" = "player")) %>%
  mutate(pos = ifelse(is.na(pos1.x),pos1.y,pos1.x),
         height = ifelse(is.na(height.x),height.y,height.x),
         weight = ifelse(is.na(weight.x),weight.y,weight.x)) %>% 
  ungroup() %>% select(pos, height, weight,injured) %>%
  filter(pos != "K")
#missing values for injured indicator should be 0
inj.data$injured[is.na(inj.data$injured)] <- 0



