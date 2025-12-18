# write function that loads in all latest data and creates one tibble

get_player_stats <- function(season = "2025-2026"){
  
  # load in relevant packages
  library(readxl)
  library(tidyverse)
  library(rvest)
  
  # will sequentially load in and tidy all of the relevant stats pages from
  # fbref for fpl. Start with playing time, which will be used as base
  # and all others will be joined to that
  
  # define url for playing time stats
  play_time_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/playingtime/", season, "-",
    "Premier-League-Stats"
  )
  
  play_time_html <- read_html_live(play_time_url)
  
  # extract table element
  play_time_tables <- play_time_html |>
    html_elements("table") |>
    html_table()
  
  # now extract player play_time stats
  play_time_stats <- play_time_tables[[3]]
  colnames(play_time_stats) <- play_time_stats[1, ]
  colnames(play_time_stats)[29] <- "xGOn-Off"
  play_time_stats <- play_time_stats[-1, ] |>
    # tidy up Min column
    mutate(`Min` = if_else(
      # if Min contains a comma
      grepl(",", `Min`),
      # remove and convert to numeric
      as.numeric(str_remove(`Min`, ",")),
      # otherwise just convert to numeric
      as.numeric(`Min`))) |>
    # and select columns we care about
    select(c(Player, Squad, MP, Min, `Mn/MP`, `Min%`, `90s`, `Starts`, `Mn/Start`, `Compl`,
             `Subs`, `Mn/Sub`))
  
  # define url for standard stats
  standard_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/stats/", season, "-",
    "Premier-League-Stats"
  )
  
  # use read_html_live (fbref must be dynamically generated with javascript
  # as read_html could not open connection) to read in HTML object
  standard_html <- read_html_live(standard_url)
  
  # extract table element
  standard_tables <- standard_html |>
    html_elements("table") |>
    html_table()
  
  # now extract player stats from standard
  standard_stats <- standard_tables[[3]]
  
  # make first row the column headers
  colnames(standard_stats) <- standard_stats[1, ]
  
  # append "90" for per 90 stats
  colnames(standard_stats)[-c(1:26, ncol(standard_stats))] <- paste0(
    colnames(standard_stats)[-c(1:26, ncol(standard_stats))], "90")
  
  # remove first row that has been used for column headers
  standard_stats <- standard_stats[-1, ]
  
  # remove comma from mins
  standard_stats <- standard_stats |>
    # select columns we need
    select(c(Player, `G-PK90`, `Ast90`, `npxG90`, `xAG90`, `PK`, `PKatt`,
             `CrdY`, `CrdR`))
  
  # load in passing stats
  # define url for passing stats
  passing_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/passing/", season, "-",
    "Premier-League-Stats"
  )
  
  # use read_html_live (fbref must be dynamically generated with javascript
  # as read_html could not open connection) to read in HTML object
  passing_html <- read_html_live(passing_url)
  
  # extract table element
  passing_tables <- passing_html |>
    html_elements("table") |>
    html_table()
  
  # now extract player passing stats
  passing_stats <- passing_tables[[3]]
  
  # take first row to be column names
  colnames(passing_stats) <- passing_stats[1, ]
  
  # append _tot to columns 9 to 13
  colnames(passing_stats)[9:13] <- paste0(
    colnames(passing_stats)[9:13], "_tot"
  )
  
  # append _short to columns 14 to 16
  colnames(passing_stats)[14:16] <- paste0(
    colnames(passing_stats)[14:16], "_short"
  )
  
  # append _med to columns 17 to 19
  colnames(passing_stats)[17:19] <- paste0(
    colnames(passing_stats)[17:19], "_med"
  )
  
  # append _long to columns 20 to 22
  colnames(passing_stats)[20:22] <- paste0(
    colnames(passing_stats)[20:22], "_long"
  )
  
  # and remove first row that is used for column headers
  passing_stats <- passing_stats[-1, ] |>
    # select columns we care about
    select(c(`Player`, Cmp_tot, Att_tot, `xA`, `KP`))
  
  # define url for defensive actions stats
  def_act_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/defense/", season, "-",
    "Premier-League-Stats"
  )
  
  def_act_html <- read_html_live(def_act_url)
  
  # extract table element
  def_act_tables <- def_act_html |>
    html_elements("table") |>
    html_table()
  
  # now extract player def_act stats
  def_act_stats <- def_act_tables[[3]]
  
  # set first row to be the column names
  colnames(def_act_stats) <- def_act_stats[1, ]
  
  # change some column names so they're a bit clearer
  colnames(def_act_stats)[9] <- "Tkl_player"
  colnames(def_act_stats)[14] <- "Tkl_dribbler"
  
  # remove first row
  def_act_stats <- def_act_stats[-1, ] |>
    # and select columns we care about
    select(c(Player, Sh, `Tkl_player`, `Int`, `Clr`))
  
  # define url for misc stats
  misc_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/misc/", season, "-",
    "Premier-League-Stats"
  )
  
  # use read_html_live (fbref must be dynamically generated with javascript
  # as read_html could not open connection) to read in HTML object
  misc_html <- read_html_live(misc_url)
  
  # extract table element
  misc_tables <- misc_html |>
    html_elements("table") |>
    html_table()
  
  # now extract player misc stats
  misc_stats <- misc_tables[[3]]
  
  # set first row to be column names, then remove first row
  colnames(misc_stats) <- misc_stats[1, ]
  misc_stats <- misc_stats[-1, ] |>
    # and select columns we want
    select(c(Player, Recov))
  
  # define url for misc stats
  adv_gk_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/keepersadv/", season, "-",
    "Premier-League-Stats"
  )
  
  adv_gk_html <- read_html_live(adv_gk_url)
  
  # extract table element
  adv_gk_tables <- adv_gk_html |>
    html_elements("table") |>
    html_table()
  
  # now extract player adv_gk stats
  adv_gk_stats <- adv_gk_tables[[3]]
  colnames(adv_gk_stats) <- adv_gk_stats[1, ]
  
  # edit column names so we don't have duplicates
  colnames(adv_gk_stats)[18:20] <- paste0(
    colnames(adv_gk_stats)[18:20], "_launched"
  )
  
  colnames(adv_gk_stats)[21:24] <- paste0(
    colnames(adv_gk_stats)[21:24], "_passes"
  )
  
  colnames(adv_gk_stats)[25:27] <- paste0(
    colnames(adv_gk_stats)[25:27], "_goalkick"
  )
  
  adv_gk_stats <- adv_gk_stats[-1, ] |>
    # and drop columns we don't need - Rk, Nation, Pos, Squad, Age, Born, 90s
    select(-c(Rk, `Nation`, Pos, Squad, Age, Born, `90s`))
  
  # define url for shot-creating action stats
  sca_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/gca/", season, "-",
    "Premier-League-Stats"
  )
  
  # use read_html_live (fbref must be dynamically generated with javascript
  # as read_html could not open connection) to read in HTML object
  sca_html <- read_html_live(sca_url)
  
  # extract table element
  sca_tables <- sca_html |>
    html_elements("table") |>
    html_table()
  
  # now extract player sca stats
  sca_stats <- sca_tables[[3]]
  
  # make first row the column names
  colnames(sca_stats) <- sca_stats[1, ]
  
  # append "_sca" to columns about SCA
  colnames(sca_stats)[11:16] <- paste0(
    colnames(sca_stats)[11:16], "_sca")
  
  # and same for GCA
  colnames(sca_stats)[19:24] <- paste0(
    colnames(sca_stats)[19:24], "_gca")
  
  # remove first row which is not needed
  sca_stats <- sca_stats[-1, ] |>
    # and drop columns we don't need - Rk, Nation, Pos, Squad, Age, Born, 90s
    select(-c(Rk, `Nation`, Pos, Squad, Age, Born, `90s`))
  
  # define url for possession stats
  possess_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/possession/", season, "-",
    "Premier-League-Stats"
  )
  
  # as read_html could not open connection) to read in HTML object
  possess_html <- read_html_live(possess_url)
  
  # extract table element
  possess_tables <- possess_html |>
    html_elements("table") |>
    html_table()
  
  # now extract player possession stats
  possess_stats <- possess_tables[[3]]
  
  # make row 1 the column headers
  colnames(possess_stats) <- possess_stats[1, ]
  
  # remove first row
  possess_stats <- possess_stats[-1, ] |>
    # and drop columns we don't need - Rk, Nation, Pos, Squad, Age, Born, 90s
    select(-c(Rk, `Nation`, Pos, Squad, Age, Born, `90s`))
  
  # now join everything to play_time_stats
  all_stats <- play_time_stats |>
    full_join(standard_stats) |>
    full_join(passing_stats) |>
    full_join(def_act_stats) |>
    full_join(misc_stats) |>
    full_join(adv_gk_stats) |>
    full_join(sca_stats) |>
    full_join(possess_stats)
  
  return(all_stats)
  
}

get_player_stats(season = "2025-2026")
