# function for producing expected points 
FPL_EV <- function(player, gameweek, xmins){
  
  # produce following variables for all players
  player_stats <- all_stats |>
    mutate(
      # projected non-penalty goals against opponent
      `npGoals90` = npxG90 * finishing_skill * opp_defence_multiplier * location,
      # projected assists against opponent - 50-50 split between xA and xAG,
      # and include nonpassingSCA as FPL attributes assists in these cases too
      `Ast90` = (0.5 * (xAG90 + xA90) + 0.14 * nonpassSCA_90) * opp_defence_multiplier * location,
      # penGoals90, probability of scoring times probability of taking times
      # probability of getting a pen (0.005 * penalty area touches * opposition multiplier)
      `penGoals90` = 0.77 * pen_taker_prob *
        0.005 * AttPenTouch90 * opp_att_pen_touch_against_multiplier *
        location,
      `npGA90` = 0.8 * npxG90_against + 0.2 * `G-PK90_against` *
        opp_attack_multiplier * location,
      `penGA90` = 0.005 * AttPenTouch90_against * opp_att_pen_touch_multiplier *
        location
      `CS` = 
    )
  
  
  
}