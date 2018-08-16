library(tidyverse)
library(ebbr)


#Data from armchair analysis
player <- read_csv('C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\PLAYER.csv')
offense <- read_csv('C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\OFFENSE.csv')
game <- read_csv('C:\\Users\\rtravis\\Documents\\NFL\\nfl_00-17\\GAME.csv')

#Game level offensive data; Only players with recorded information for each game
offensive_games <- offense %>%
  inner_join(player %>% select(player, pos1)) %>%
  filter(pos1 %in% c('WR', 'TE', 'RB', 'QB')) %>%
  dplyr::select(-seas) %>%
  inner_join(game %>% select(gid, wk, seas))

#All possible games from 2000-2017
all_games <- offensive_games %>%
  group_by(player, seas) %>%
  do({
    data_frame(wk = 1L:17L)
  })

#Join offensive player data to all games dataset
all_offense <- all_games %>%
  left_join(offensive_games) %>%
  dplyr::select(player, seas, wk, pa, py, tdp, ints, ra, ry, tdr, trg, rec, recy, tdrec, fuml) %>%
  mutate(pass_attempts = pa,
         pass_yards = py,
         pass_tds = tdp,
         interceptions = ints,
         rush_attempts = ra,
         rush_yards = ry,
         rush_tds = tdr,
         targets = trg,
         receptions = rec,
         rec_yards = recy,
         rec_tds = tdrec,
         fumbles = fuml,
         games = as.integer(!is.na(trg))) %>%
  select(-pa,-py,-tdp,-ints,-ra,-ry,-tdr,-trg,-rec,-recy,-tdrec,-fuml)

#Long form dataset with each metric for each player and season
seasons <- all_offense %>%
  filter(games > 0, rush_tds < rush_attempts + 1) %>%
  gather(metric, value, pass_attempts:games) %>%
  mutate(value = coalesce(value, 0L)) %>%
  group_by(player, seas, metric) %>%
  summarise(value = sum(value)) %>%
  ungroup()

seasons %>% inner_join(player %>% dplyr::select(player, pos1)) %>% 
  group_by(pos1, metric) %>% ggplot(aes(value)) + geom_histogram() +
  facet_grid(rows = vars(pos1), cols = vars(metric), scales = "free")



####Smoothing Rates with empirical bayes----
seasons_with_rates <- 
  #creates column for each metric
  seasons %>%
  spread(metric, value) %>%
  #Get pos1 variable
  inner_join(player %>% dplyr::select(player, pos1)) %>%
  group_by(seas, pos1) %>%
  #Perform empirical bayes smoothing for metrics within each year and positions
  do({
    #Fumble rate
    x4 <- add_ebb_estimate(.,
                           pmin(fumbles, rush_attempts + targets + pass_attempts),
                           pmax((rush_attempts + targets + pass_attempts), 1))
    
    if (first(.$pos1) == 'QB') {
      x1 <- data_frame(.fitted = rep(0, nrow(.)))
      x2 <- data_frame(.fitted = rep(0, nrow(.)))
      x5 <- add_ebb_estimate(., pmax(interceptions, 0), pmax(pass_attempts, 1))
      x6 <- add_ebb_estimate(., pmax(pass_tds, 0), pmax(pass_attempts, 1))
    } else {
      x1 <- add_ebb_estimate(., receptions, pmax(targets, 1))
      x2 <- add_ebb_estimate(., rec_tds, pmax(targets, 1))
      x5 <- data_frame(.fitted = rep(0, nrow(.)))
      x6 <- data_frame(.fitted = rep(0, nrow(.)))
    }
    if (first(.$pos1) %in% c('RB', 'QB')) {
      x3 <- add_ebb_estimate(., rush_tds, pmax(rush_attempts, 1))
    } else {
      x3 <- data_frame(.fitted = rep(0, nrow(.)))
    }
    #yards per carry
    ypc <- with(., sum(rush_yards) / sum(rush_attempts))
    #yards per target
    ypt <- with(., sum(rec_yards) / sum(targets))
    #yards per pass
    ypp <- with(., sum(pass_yards) / sum(pass_attempts))
    new.cols <- data_frame(rec_trg = x1$.fitted,
                           tdrec_trg = x2$.fitted,
                           tdr_ra = x3$.fitted,
                           fuml_ratrg = x4$.fitted,
                           ry_ra = (.$rush_yards + ypc) / (.$rush_attempts + 1),
                           recy_trg = (.$rec_yards + ypt) / (.$targets + 1),
                           ints_pa = x5$.fitted,
                           tdp_pa = x6$.fitted,
                           py_pa = (.$pass_yards + ypp) / (.$pass_attempts+1))
    bind_cols(., new.cols)
  }) %>%
  ungroup() %>%
  dplyr::select(-pos1) %>%
  gather(metric, value, -player, -seas)