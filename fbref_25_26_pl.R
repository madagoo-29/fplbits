# loading in current season's prem stats
# firstly load in required packages

library(readxl)
library(tidyverse)
library(rvest)

standard_25_26_url <- "https://fbref.com/en/comps/9/stats/Premier-League-Stats"
standard_25_26_html <- read_html_live(standard_25_26_url)

standard_25_26 <- standard_25_26_html |>
  html_elements("table") |>
  html_table()

attack_25_26 <- standard_25_26[[1]]
colnames(attack_25_26) <- attack_25_26[1, ]
colnames(attack_25_26)[-c(1:22)] <- paste0(colnames(attack_25_26)[-c(1:22)], "90")
attack_25_26 <- attack_25_26[-1, ]
attack_25_26 <- attack_25_26 %>%
  mutate(`Min` = ifelse(grepl(",", `Min`),
                        as.numeric(str_remove(`Min`, ",")),
                        as.numeric(`Min`)))

defence_25_26 <- standard_25_26[[2]]
colnames(defence_25_26) <- defence_25_26[1, ]
colnames(defence_25_26)[-c(1:22)] <- paste0(colnames(defence_25_26)[-c(1:22)], "90")
defence_25_26 <- defence_25_26[-1, ]
defence_25_26 <- defence_25_26 %>%
  mutate(`Min` = ifelse(grepl(",", `Min`),
                        as.numeric(str_remove(`Min`, ",")),
                        as.numeric(`Min`)))
