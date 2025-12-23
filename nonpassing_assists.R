# estimating the probability a nonpassing SCA becomes a GCA using previous years
# data

# load in stats library
library(stats)

# get player data from 24-25 season
player_stats_24_25 <- get_player_stats(season = "2024-2025")

# inspect further if required
View(player_stats_24_25)

# filter out players who didn't play a lot of minutes,
# let's say at least a quarter of the season
player_stats_24_25_min_9.5_games <- player_stats_24_25 |>
  filter(`90s` >= 9.5) |>
  # find the actual counts, i.e. not per 90
  mutate(
    nonpassGCA = nonpassGCA_90 * `90s`,
    nonpassSCA = nonpassSCA_90 * `90s`
  )

nrow(player_stats_24_25_min_9.5_games)
# that leaves 330 players

# plot nonpassGCA against nonpassSCA
plot(player_stats_24_25_min_9.5_games$nonpassSCA,
     player_stats_24_25_min_9.5_games$nonpassGCA,
     xlab = "SCA excluding passes",
     ylab = "GCA excluding passes")

# create generalised linear model to estimate relationship between
# nonpass SCA and nonpass GCA (SCA of 0 implies GCA of 0,
# so no intercept required)

glm_nonpassing_assists <- glm(
  nonpassGCA ~ nonpassSCA - 1, data = player_stats_24_25_min_9.5_games
)

# plot line of best fit
abline(glm_nonpassing_assists)

# coefficient estimate is
glm_nonpassing_assists$coefficients
# roughly 0.13, so this model would estimate 1 nonpassing assist
# from roughly 8 (7.69) nonpassing shot-creating actions  

# and a player producing 1 nonpassing shot-creating action
# a game is roughly expected to get 0.13 nonpassing assists per game


# see if the season before produced a similar figure

# get player data from 24-25 season
player_stats_23_24 <- get_player_stats(season = "2023-2024")

# inspect further if required
View(player_stats_23_24)

# filter out players who didn't play a lot of minutes,
# let's say at least a quarter of the season
player_stats_23_24_min_9.5_games <- player_stats_23_24 |>
  filter(`90s` >= 9.5) |>
  # find the actual counts, i.e. not per 90
  mutate(
    nonpassGCA = nonpassGCA_90 * `90s`,
    nonpassSCA = nonpassSCA_90 * `90s`
  )

nrow(player_stats_23_24_min_9.5_games)
# that leaves 343 players

# plot nonpassGCA against nonpassSCA
plot(player_stats_23_24_min_9.5_games$nonpassSCA,
     player_stats_23_24_min_9.5_games$nonpassGCA,
     xlab = "SCA excluding passes",
     ylab = "GCA excluding passes")

# create generalised linear model to estimate relationship between
# nonpass SCA and nonpass GCA (SCA of 0 implies GCA of 0,
# so no intercept required), loglink function with Poisson assumed for response variable

glm_nonpassing_assists_23_24 <- glm(
  nonpassGCA ~ nonpassSCA - 1, data = player_stats_23_24_min_9.5_games
)

# plot line of best fit
abline(glm_nonpassing_assists_23_24)

# coefficient estimate is
glm_nonpassing_assists_23_24$coefficients
# roughly 0.15, so a little higher