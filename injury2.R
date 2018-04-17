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
  left_join(player, by = c("player" = "player")) %>% 
  select(player, pos1, height, weight, games.inj, injured) 
#All players who played an offensive snap in 2017
all.players <- game %>% filter(seas == 2017 & wk %in% c(1:17)) %>%
  select(gid) %>% left_join(offense, by = c("gid" = "gid")) %>%
  select(player) %>% left_join(player, by = c("player" = "player")) %>%
  select(player, pos1, height, weight) %>%
  group_by(player) %>% distinct()
#Join the two tables. Players who didn't get hurt will have missing values for injured
inj.data <- all.players %>% full_join(inj.players, by = c("player" = "player")) %>%
  mutate(pos = ifelse(is.na(pos1.x),pos1.y,pos1.x),
         height = ifelse(is.na(height.x),height.y,height.x),
         weight = ifelse(is.na(weight.x),weight.y,weight.x)) %>% 
  select(player, pos,height, weight, injured) %>%
  filter((!pos %in% c("K", "LS")))
#missing values for injured indicator should be 0
inj.data$injured[is.na(inj.data$injured)] <- 0

#5 players with missing position values
#These players are not in the player data table, hence why they are missing
miss.players <- inj.data[which(is.na(inj.data$pos)),][,"player"]

inj.data[which(is.na(inj.data$pos)),][, "player"] %>%
  left_join(player, by = c("player" = "player"))

#Remove missing players
inj.data <- anti_join(inj.data, miss.players, by = c("player" = "player"))

#Create design matrix
dm <- model.matrix(injured ~ pos + height + weight, data = inj.data)

#Fit model in Stan
data.list <- list(N = nrow(dm),
                  K = ncol(dm[,-1]),
                  X = dm[,-1], #do not include intercept
                  y = inj.data$injured)

fit <- stan(file ='LR3.stan',
            data = data.list)

params <- data.frame(extract(fit))

params[,1:7] %>% gather(key = "Positions", value = "Estimate", beta.1:beta.7) %>%
  ggplot(aes(Estimate)) + geom_density() + facet_grid(~ Positions)
params[,8:9] %>% gather(key = "Coefficients", value = "Estimate", beta.8:beta.9) %>%
  ggplot(aes(Estimate)) + geom_density() + facet_grid(~ Coefficients)