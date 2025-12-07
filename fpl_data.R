# use the fplr package from ewenme to get fpl positions, prices
# not just positions but also player prices
library(fplr)

# get all players data
players <- fpl_get_player_all()

# get all fixtures data
fixtures <- fpl_get_fixtures()

# get all teams data
teams <- fpl_get_teams()

positions <- positions %>% add_column(element_type = 1:4)

teams <- attack_23_24 %>% select(Squad) %>%
  add_column(team = 1:20) 

players_fpl <- fpl_get_player_all()

players_fpl <- players_fpl %>%
  add_column(Position = "", Squad = "")

for(n in 1:nrow(players_fpl)){
  
  print(paste("Player", n, "out of", nrow(players_fpl)))
  e_type <- players_fpl$element_type[n]
  tm <- players_fpl$team[n]
  pos <- positions %>% filter(element_type == e_type) %>% select(singular_name_short)
  sqd <- teams %>% filter(team == tm) %>% select(Squad)
  players_fpl$Position[n] <- pos$singular_name_short
  players_fpl$Squad[n] <- sqd$Squad
  
}

players_fpl <- players_fpl %>%
  arrange(second_name) %>%
  mutate(Player = paste0(first_name, " ", second_name)) %>%
  select(c(Player, now_cost, Squad, Position))
  
# positions, teams and prices all sorted


