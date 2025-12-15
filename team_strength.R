# straightforward team strength model using just premier league
# data. Use previous year's data and update with current year
# 80% xG, 20% goals

# load in previous year
# define url for standard stats

team_strength <- function(year, xG_to_G_ratio = 3){
  
  # load in team standard stats from previous year, firstly defining the url
  standard_prev_url <- paste0(
    "https://fbref.com/en/comps/9/", year - 1, "-", year, "/", year - 1, "-", year, "-",
    "Premier-League-Stats"
  )
  
  # use read_html_live (fbref must be dynamically generated with javascript
  # as read_html could not open connection) to read in HTML object
  standard_prev_html <- read_html_live(standard_prev_url)
  
  # extract table element
  standard_prev <- standard_prev_html |>
    html_elements("table") |>
    html_table()
  
  # get attack data for each team
  attack_prev <- standard_prev[[1]]
  
  # define column names to be current first row (Squad, # Pl, etc.)
  colnames(attack_prev) <- attack_prev[1, ]
  
  # paste "90" onto the end of the per 90 columns (col 23 onwards)
  colnames(attack_prev)[-c(1:22)] <- paste0(colnames(attack_prev)[-c(1:22)], "90")
  
  # remove first row (these are the new column names)
  attack_prev <- attack_prev[-1, ]
  
  # convert Min column to numeric
  attack_prev <- attack_prev |>
    mutate(
      `Min` = ifelse(grepl(",", `Min`),
                          as.numeric(str_remove(`Min`, ",")),
                          as.numeric(`Min`)),
      # now create prnpG90 variable, use xG_to_G_ratio argument
      prnpG90 = xG_to_G_ratio/(xG_to_G_ratio+1) * as.numeric(npxG90) +
        1/(xG_to_G_ratio+1) * as.numeric(`G-PK90`)
      )
  
  # now equivalent code for defence
  
  defence_prev <- standard_prev[[2]]
  colnames(defence_prev) <- defence_prev[1, ]
  colnames(defence_prev)[-c(1:22)] <- paste0(colnames(defence_prev)[-c(1:22)], "90")
  defence_prev <- defence_prev[-1, ]
  defence_prev <- defence_prev %>%
    mutate(
      `Min` = ifelse(grepl(",", `Min`),
                          as.numeric(str_remove(`Min`, ",")),
                          as.numeric(`Min`)),
      
      # now create prnpG90 variable, use xG_to_G_ratio argument
      prnpG90 = xG_to_G_ratio/(xG_to_G_ratio+1) * as.numeric(npxG90) +
        1/(xG_to_G_ratio+1) * as.numeric(`G-PK90`))
  
  # load in previous year championship stats for three promoted teams
  standard_champ_prev_url <- paste0(
    "https://fbref.com/en/comps/10/", year - 1, "-", year, "/", year - 1, "-", year, "-",
    "Championship-Stats"
  )
  
  # use read_html_live (fbref must be dynamically generated with javascript
  # as read_html could not open connection) to read in HTML object
  standard_champ_prev_html <- read_html_live(standard_champ_prev_url)
  
  # extract table element
  standard_champ_prev <- standard_champ_prev_html |>
    html_elements("table") |>
    html_table()
  
  # get attack data for each team
  attack_champ_prev <- standard_champ_prev[[1]]
  
  # define column names to be current first row (Squad, # Pl, etc.)
  colnames(attack_champ_prev) <- attack_champ_prev[1, ]
  
  # paste "90" onto the end of the per 90 columns (col 23 onwards)
  colnames(attack_champ_prev)[-c(1:22)] <- paste0(colnames(attack_champ_prev)[-c(1:22)], "90")
  
  # remove first row (these are the new column names)
  attack_champ_prev <- attack_champ_prev[-1, ]
  
  # convert Min column to numeric
  attack_champ_prev <- attack_champ_prev |>
    mutate(
      `Min` = ifelse(grepl(",", `Min`),
                     as.numeric(str_remove(`Min`, ",")),
                     as.numeric(`Min`)),
      # now create prnpG90 variable, use xG_to_G_ratio argument
      prnpG90 = xG_to_G_ratio/(xG_to_G_ratio+1) * as.numeric(npxG90) +
        1/(xG_to_G_ratio+1) * as.numeric(`G-PK90`)
    )
  
  # now equivalent code for defence
  
  defence_champ_prev <- standard_champ_prev[[2]]
  colnames(defence_champ_prev) <- defence_champ_prev[1, ]
  colnames(defence_champ_prev)[-c(1:22)] <- paste0(colnames(defence_champ_prev)[-c(1:22)], "90")
  defence_champ_prev <- defence_champ_prev[-1, ]
  defence_champ_prev <- defence_champ_prev %>%
    mutate(
      `Min` = ifelse(grepl(",", `Min`),
                     as.numeric(str_remove(`Min`, ",")),
                     as.numeric(`Min`)),
      
      # now create prnpG90 variable, use xG_to_G_ratio argument
      prnpG90 = xG_to_G_ratio/(xG_to_G_ratio+1) * as.numeric(npxG90) +
        1/(xG_to_G_ratio+1) * as.numeric(`G-PK90`))
  
  # then load in current year stats
  standard_current_url <- "https://fbref.com/en/comps/9/stats/Premier-League-Stats"
  
  # use read_html_live (fbref must be dynamically generated with javascript
  # as read_html could not open connection) to read in HTML object
  standard_current_html <- read_html_live(standard_current_url)
  
  # extract table element
  standard_current <- standard_current_html |>
    html_elements("table") |>
    html_table()
  
  # get attack data for each team
  attack_current <- standard_current[[1]]
  
  # define column names to be current first row (Squad, # Pl, etc.)
  colnames(attack_current) <- attack_current[1, ]
  
  # paste "90" onto the end of the per 90 columns (col 23 onwards)
  colnames(attack_current)[-c(1:22)] <- paste0(colnames(attack_current)[-c(1:22)], "90")
  
  # remove first row (these are the new column names)
  attack_current <- attack_current[-1, ]
  
  # convert Min column to numeric
  attack_current <- attack_current |>
    mutate(
      `Min` = ifelse(grepl(",", `Min`),
                     as.numeric(str_remove(`Min`, ",")),
                     as.numeric(`Min`)),
      # now create prnpG90 variable, use xG_to_G_ratio argument
      prnpG90 = xG_to_G_ratio/(xG_to_G_ratio+1) * as.numeric(npxG90) +
        1/(xG_to_G_ratio+1) * as.numeric(`G-PK90`)
    )
  
  # now equivalent code for defence
  
  defence_current <- standard_current[[2]]
  colnames(defence_current) <- defence_current[1, ]
  colnames(defence_current)[-c(1:22)] <- paste0(colnames(defence_current)[-c(1:22)], "90")
  defence_current <- defence_current[-1, ]
  defence_current <- defence_current %>%
    mutate(
      `Min` = ifelse(grepl(",", `Min`),
                     as.numeric(str_remove(`Min`, ",")),
                     as.numeric(`Min`)),
      
      # now create prnpG90 variable, use xG_to_G_ratio argument
      prnpG90 = xG_to_G_ratio/(xG_to_G_ratio+1) * as.numeric(npxG90) +
        1/(xG_to_G_ratio+1) * as.numeric(`G-PK90`))
  
  team_strength
}

