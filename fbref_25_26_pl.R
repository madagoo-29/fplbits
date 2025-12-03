# loading in current season's prem stats
# firstly load in required packages

library(readxl)
library(tidyverse)
library(rvest)

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
colnames(player_passing_25_26) <- player_passing_25_26[1, ]
colnames(player_passing_25_26)[-c(1:26, ncol(player_passing_25_26))] <- paste0(
  colnames(player_passing_25_26)[-c(1:26, ncol(player_passing_25_26))], "90")
player_passing_25_26 <- player_passing_25_26[-1, ]
player_passing_25_26 <- player_passing_25_26 %>%
  mutate(`Min` = ifelse(grepl(",", `Min`),
                        as.numeric(str_remove(`Min`, ",")),
                        as.numeric(`Min`)))
