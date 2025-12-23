# loading in current season's prem stats
# firstly load in required packages

library(readxl)
library(tidyverse)
library(rvest)

# write function that loads in all latest data and creates one tibble



# define url for standard stats
standard_25_26_url <- "https://fbref.com/en/comps/9/stats/Premier-League-Stats"

# use read_html_live (fbref must be dynamically generated with javascript
# as read_html could not open connection) to read in HTML object
standard_25_26_html <- read_html_live(standard_25_26_url)

# extract table element
standard_25_26 <- standard_25_26_html |>
  html_elements("table") |>
  html_table()

# get attack data for each team
attack_25_26 <- standard_25_26[[1]]

# define column names to be current first row (Squad, # Pl, etc.)
colnames(attack_25_26) <- attack_25_26[1, ]

# paste "90" onto the end of the per 90 columns (col 23 onwards)
colnames(attack_25_26)[-c(1:22)] <- paste0(colnames(attack_25_26)[-c(1:22)], "90")

# remove first row (these are the new column names)
attack_25_26 <- attack_25_26[-1, ]

# convert Min column to numeric
attack_25_26 <- attack_25_26 |>
  mutate(`Min` = ifelse(grepl(",", `Min`),
                        as.numeric(str_remove(`Min`, ",")),
                        as.numeric(`Min`)))

# now equivalent code for defence

defence_25_26 <- standard_25_26[[2]]
colnames(defence_25_26) <- defence_25_26[1, ]
colnames(defence_25_26)[-c(1:22)] <- paste0(colnames(defence_25_26)[-c(1:22)], "90")
defence_25_26 <- defence_25_26[-1, ]
defence_25_26 <- defence_25_26 %>%
  mutate(`Min` = ifelse(grepl(",", `Min`),
                        as.numeric(str_remove(`Min`, ",")),
                        as.numeric(`Min`)))

# now extract player stats from 
player_standard_25_26 <- standard_25_26[[3]]
colnames(player_standard_25_26) <- player_standard_25_26[1, ]
colnames(player_standard_25_26)[-c(1:26, ncol(player_standard_25_26))] <- paste0(
  colnames(player_standard_25_26)[-c(1:26, ncol(player_standard_25_26))], "90")
player_standard_25_26 <- player_standard_25_26[-1, ]
player_standard_25_26 <- player_standard_25_26 %>%
  mutate(`Min` = ifelse(grepl(",", `Min`),
                        as.numeric(str_remove(`Min`, ",")),
                        as.numeric(`Min`)))

# define url for passing stats
passing_25_26_url <- "https://fbref.com/en/comps/9/passing/Premier-League-Stats"
# use read_html_live (fbref must be dynamically generated with javascript
# as read_html could not open connection) to read in HTML object
passing_25_26_html <- read_html_live(passing_25_26_url)

# extract table element
passing_25_26 <- passing_25_26_html |>
  html_elements("table") |>
  html_table()

# now extract player passing stats
player_passing_25_26 <- passing_25_26[[3]]

# take first row to be column names
colnames(player_passing_25_26) <- player_passing_25_26[1, ]

# append _tot to columns 9 to 13
colnames(player_passing_25_26)[9:13] <- paste0(
  colnames(player_passing_25_26)[9:13], "_tot"
)

# append _short to columns 14 to 16
colnames(player_passing_25_26)[14:16] <- paste0(
  colnames(player_passing_25_26)[14:16], "_short"
)

# append _med to columns 17 to 19
colnames(player_passing_25_26)[17:19] <- paste0(
  colnames(player_passing_25_26)[17:19], "_med"
)

# append _long to columns 20 to 22
colnames(player_passing_25_26)[20:22] <- paste0(
  colnames(player_passing_25_26)[20:22], "_long"
)

player_passing_25_26 <- player_passing_25_26[-1, ]


# define url for defensive actions stats
def_act_25_26_url <- "https://fbref.com/en/comps/9/defense/Premier-League-Stats"
# use read_html_live (fbref must be dynamically generated with javascript
# as read_html could not open connection) to read in HTML object
def_act_25_26_html <- read_html_live(def_act_25_26_url)

# extract table element
def_act_25_26 <- def_act_25_26_html |>
  html_elements("table") |>
  html_table()

# now extract player def_act stats
player_def_act_25_26 <- def_act_25_26[[3]]

# set first row to be the column names
colnames(player_def_act_25_26) <- player_def_act_25_26[1, ]

# change some column names so they're a bit clearer
colnames(player_def_act_25_26)[9] <- "Tkl_player"
colnames(player_def_act_25_26)[14] <- "Tkl_dribbler"

player_def_act_25_26 <- player_def_act_25_26[-1, ]

# and extract team opposition def_act stats
def_act_opp_25_26 <- def_act_25_26[[2]]

#set column names to be first row of table
colnames(def_act_opp_25_26) <- def_act_opp_25_26[1, ]

# change some column names so they're a bit clearer
colnames(def_act_opp_25_26)[4] <- "Tkl_player"
colnames(def_act_opp_25_26)[9] <- "Tkl_dribbler"

def_act_opp_25_26 <- def_act_opp_25_26[-1, ]

# define url for misc stats stats
misc_25_26_url <- "https://fbref.com/en/comps/9/misc/Premier-League-Stats"
# use read_html_live (fbref must be dynamically generated with javascript
# as read_html could not open connection) to read in HTML object
misc_25_26_html <- read_html_live(misc_25_26_url)

# extract table element
misc_25_26 <- misc_25_26_html |>
  html_elements("table") |>
  html_table()

# now extract player misc stats
player_misc_25_26 <- misc_25_26[[3]]

# set first row to be column names, then remove first row
colnames(player_misc_25_26) <- player_misc_25_26[1, ]
player_misc_25_26 <- player_misc_25_26[-1, ]

# and extract team opposition misc stats
misc_opp_25_26 <- misc_25_26[[2]]
colnames(misc_opp_25_26) <- misc_opp_25_26[1, ]
colnames(misc_opp_25_26)[-c(1:22)] <- paste0(colnames(misc_opp_25_26)[-c(1:22)], "90")
misc_opp_25_26 <- misc_opp_25_26[-1, ]

# define url for adv gk stats
adv_gk_25_26_url <- "https://fbref.com/en/comps/9/keepersadv/Premier-League-Stats"
# use read_html_live (fbref must be dynamically generated with javascript
# as read_html could not open connection) to read in HTML object
adv_gk_25_26_html <- read_html_live(adv_gk_25_26_url)

# extract table element
adv_gk_25_26 <- adv_gk_25_26_html |>
  html_elements("table") |>
  html_table()

# now extract player adv_gk stats
player_adv_gk_25_26 <- adv_gk_25_26[[3]]
colnames(player_adv_gk_25_26) <- player_adv_gk_25_26[1, ]
player_adv_gk_25_26 <- player_adv_gk_25_26[-1, ]

# define url for play_time stats stats
play_time_25_26_url <- "https://fbref.com/en/comps/9/playingtime/Premier-League-Stats"
# use read_html_live (fbref must be dynamically generated with javascript
# as read_html could not open connection) to read in HTML object
play_time_25_26_html <- read_html_live(play_time_25_26_url)

# extract table element
play_time_25_26 <- play_time_25_26_html |>
  html_elements("table") |>
  html_table()

# now extract player play_time stats
player_play_time_25_26 <- play_time_25_26[[3]]
colnames(player_play_time_25_26) <- player_play_time_25_26[1, ]
colnames(player_play_time_25_26)[29] <- "xGOn-Off"
player_play_time_25_26 <- player_play_time_25_26[-1, ]
player_play_time_25_26 <- player_play_time_25_26 %>%
  mutate(`Min` = ifelse(grepl(",", `Min`),
                        as.numeric(str_remove(`Min`, ",")),
                        as.numeric(`Min`)))

# define url for shot-creating actions stats stats
sca_25_26_url <- "https://fbref.com/en/comps/9/gca/Premier-League-Stats"
# use read_html_live (fbref must be dynamically generated with javascript
# as read_html could not open connection) to read in HTML object
sca_25_26_html <- read_html_live(sca_25_26_url)

# extract table element
sca_25_26 <- sca_25_26_html |>
  html_elements("table") |>
  html_table()

# now extract player sca stats
player_sca_25_26 <- sca_25_26[[3]]

# make first row the column names
colnames(player_sca_25_26) <- player_sca_25_26[1, ]

# append "_sca" to columns about SCA
colnames(player_sca_25_26)[11:16] <- paste0(
  colnames(player_sca_25_26)[11:16], "_sca")

# and same for GCA
colnames(player_sca_25_26)[19:24] <- paste0(
  colnames(player_sca_25_26)[19:24], "_gca")

# remove first row which is not needed
player_sca_25_26 <- player_sca_25_26[-1, ]

# define url for possession stats stats
possession_25_26_url <- "https://fbref.com/en/comps/9/possession/Premier-League-Stats"
# use read_html_live (fbref must be dynamically generated with javascript
# as read_html could not open connection) to read in HTML object
possession_25_26_html <- read_html_live(possession_25_26_url)

# extract table element
possession_25_26 <- possession_25_26_html |>
  html_elements("table") |>
  html_table()

# now extract player possession stats
player_possession_25_26 <- possession_25_26[[3]]

# make row 1 the column headers
colnames(player_possession_25_26) <- player_possession_25_26[1, ]

# remove first row
player_possession_25_26 <- player_possession_25_26[-1, ]

#############################