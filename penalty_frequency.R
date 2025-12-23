# want to estimate probability of a penalty based on penalty area touches

# load team data from previous seasons
team_stats_24_25 <- get_team_stats(season = "2024-2025")
team_stats_23_24 <- get_team_stats(season = "2023-2024")
team_stats_22_23 <- get_team_stats(season = "2022-2023")
team_stats_21_22 <- get_team_stats(season = "2021-2022")

# only interested in PKatt and Att_pen_touch columns
pens_pen_touches_24_25 <- team_stats_24_25 |>
  select(c(PKatt, Att_pen_touch))

pens_pen_touches_23_24 <- team_stats_23_24 |>
  select(c(PKatt, Att_pen_touch))

pens_pen_touches_22_23 <- team_stats_22_23 |>
  select(c(PKatt, Att_pen_touch))

pens_pen_touches_21_22 <- team_stats_21_22 |>
  select(c(PKatt, Att_pen_touch))

# merge those together
pens_pen_touches <- pens_pen_touches_24_25 |>
  bind_rows(pens_pen_touches_23_24) |>
  bind_rows(pens_pen_touches_22_23) |>
  bind_rows(pens_pen_touches_21_22)

# plot
plot(pens_pen_touches$Att_pen_touch, pens_pen_touches$PKatt,
     xlab = "Attacking penalty area touches",
     ylab = "Penalties")

# fit glm, poisson regression with loglink function
glm_penalties <- glm(
  PKatt ~ Att_pen_touch - 1, data = pens_pen_touches
)

# plot line of best fit
abline(glm_penalties)

# coefficient estimate is
glm_penalties$coefficients
# roughly 0.005. Not much of an effect it would seem, more investigation
# needed to conclude that