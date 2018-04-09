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

#Select Positions
POS <- filter(player, pos1 %in% c("QB","RB","TE","WR")) %>% 
  dplyr::select(player,pos1, pname,height, weight, dob)
POS$dob <- as.Date(POS$dob, "%m/%d/%Y")


POS.stats <- offense %>% semi_join(game, by = "gid") %>%
  dplyr::select(player,pa,pc,py,ints,tdp,ra,ry,tdr,trg,
                rec,recy,tdrec, fuml, conv,tdret, year, team, dcp) %>%
  mutate(starts = ifelse(dcp == 0,1,0)) %>%
  group_by(year, player) %>% summarise(total.pa = sum(pa),
                                       total.pc = sum(pc),
                                       total.py = sum(py),
                                       total.ints = sum(ints),
                                       total.tdp = sum(tdp),
                                       total.ra = sum(ra),
                                       total.ry = sum(ry),
                                       total.tdr = sum(tdr),
                                       total.trg = sum(trg),
                                       total.rec = sum(rec),
                                       total.recy = sum(recy),
                                       total.tdrec = sum(tdrec),
                                       total.fuml = sum(fuml),
                                       total.conv = sum(conv),
                                       total.tdret = sum(tdret),
                                       total.starts = sum(starts)) %>%
  right_join(POS, by = "player") %>%
  #Adjusted net yards per attempt
  #(pass yards + 20*(pass TD) - 45*(interceptions thrown) - sack yards)/(passing attempts + sacks)
  mutate(fanpts = 4*total.tdp + (1/25)*total.py + 2*total.conv - 2*total.ints - 2*total.fuml +
           6*total.tdr + (1/10)*total.ry + 6*total.tdrec + (1/10)*total.recy + .5*total.rec +
           6*total.tdret,
         age = year - lubridate::year(dob))

#Remove players with fanpts == NA for now. 
POS.stats <- filter(POS.stats, !is.na(fanpts))

#Exploratory Plots
POS.stats %>% dplyr::select(fanpts, year) %>%
  group_by(year) %>% summarise(total = sum(fanpts)) %>%
  ggplot(aes(x = year, y = total)) + geom_point()

#Create lagged variables
POS.stats.lag <- POS.stats %>% mutate(lag.fanpts = lag(fanpts, order_b = year)) %>%
  dplyr::select(year, player,pos1,  fanpts, lag.fanpts) %>% arrange(year)

#simple linear model 
lag.lm <- lm(fanpts ~ 1 + year + pos1 + lag.fanpts, data = POS.stats.lag)

get_prior(fanpts ~ 1 + year + pos1 + lag.fanpts + (1|pos1/player), data = POS.stats.lag)

brms.lag <- brm(fanpts ~ 1 + year + pos1 + lag.fanpts + (1|player),
                data = POS.stats.lag, family = gaussian(),
                prior = c(set_prior("normal(0,10)", class = "Intercept"),
                          set_prior("cauchy(0,1)", class = "sd"),
                          set_prior("normal(0,1)", class = "b")),                 
                          chains = 4, iter = 10000, warmup = 1000, cores = 4,
                          control = list(adapt_delta = .99,
                                         max_treedepth = 12))