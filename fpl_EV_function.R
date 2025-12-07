# function for producing expected points 
player_FPL_EV <- function(player, fixture, location, xmins){
  
  # filter player out of all_stats
  player_stats <- all_stats |>
    filter(Player == player)
  
  
  
}