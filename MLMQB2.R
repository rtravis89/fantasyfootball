#Running back yards
require(tidyverse)
library(brms)

###NFL data
player <- read.csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\PLAYER.csv", stringsAsFactors = FALSE)
offense <- read.csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\OFFENSE.csv", stringsAsFactors = FALSE)
game <- read.csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\GAME.csv", stringsAsFactors = FALSE)
sack <- read.csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\SACK.csv", stringsAsFactors = FALSE)
play <- read.csv("C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\PLAY.csv", stringsAsFactors = FALSE)

#filter data by regular season
game <- filter(game, wk %in% c(1:17))

#Select QBs
QB <- filter(player, pos1 == "QB") %>% 
  dplyr::select(player,pname,height, weight, dob)
QB$dob <- as.Date(QB$dob, "%m/%d/%Y")

#Need to get sacks from SACK dataset. Linked to QB. link to season by play id.
#link to year via play id to game id
#Sack yards by season
sack.season <- play %>% dplyr::select(gid, pid) %>%  right_join(sack[,c("pid","qb","ydsl")], by = "pid") %>%
  right_join(game[,c("gid","seas")], by = "gid") %>% group_by(seas, qb) %>% 
  summarise(total.ydsl = sum(ydsl),
            total.sacks = n())

QB.stats <- offense %>% semi_join(game, by = "gid") %>%
  dplyr::select(player, pa, pc, py, ints, tdp, dcp, year, team) %>%
  mutate(depth = ifelse(dcp == 0,1,0)) %>%
  group_by(year, player) %>% summarise(total.pa = sum(pa),
                                       total.pc = sum(pc),
                                       total.py = sum(py),
                                       total.ints = sum(ints),
                                       total.tdp = sum(tdp),
                                       total.starts = sum(depth)) %>%
  right_join(QB, by = "player") %>% left_join(sack.season, by = c("year" = "seas", "player" = "qb")) %>%
#Adjusted net yards per attempt
#(pass yards + 20*(pass TD) - 45*(interceptions thrown) - sack yards)/(passing attempts + sacks)
 mutate(anypa = (total.py + 20*total.tdp - 45*total.ints + total.ydsl)/
                      (total.pa + total.sacks),
        age = year - lubridate::year(dob))

#Remove players with anypa == NA for now. 
QB.stats <- filter(QB.stats, !is.na(anypa))

DATA <- as.data.frame(QB.stats[,c("anypa","player","age","weight","height","total.starts")])
DATA$player <- factor(DATA$player)
DATA$age2 <- DATA$age^2

####Modeling####
get_prior(anypa ~ 1 + age + age2 + (1 + age + age2|player),
          data = DATA,family = gaussian())


brms.mod1 <- brm(anypa ~ 1 + age + age2 + (1 + age + age2|player),
                 data = DATA,family = gaussian(),
                 prior = c(set_prior("normal(0,10)", class = "Intercept"),
                           set_prior("cauchy(0,1)", class = "sd", group = "player"),
                           set_prior("normal(0,1)", class = "b"),
                           set_prior("lkj(2)", class = "cor")),
                 chains = 4, iter = 10000, warmup = 1000, cores = 4,
                 control = list(adapt_delta = .99,
                                max_treedepth = 12))

#Model Summary
summary(brms.mod1)

#Marginal effects
marginal_effects(brms.mod1)


#Player summary
post <- posterior_samples(brms.mod1)

post %>% 
  select('r_player[AB-2900,Intercept]':'r_player[ZM-0150,age2]') %>%
  gather() %>%
  mutate(intercept = ifelse(str_detect(key, "Intercept"),1,0)) %>%
  mutate(key = fct_reorder(key, desc(intercept))) %>%
  ggplot(aes(x = key, y = value, group = key))
  