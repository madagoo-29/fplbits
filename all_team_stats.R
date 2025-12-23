# write function that loads in all latest team data and creates one tibble

get_team_stats <- function(season = "2025-2026"){
  
  # load in relevant packages
  library(readxl)
  library(tidyverse)
  library(rvest)
  library(janitor)
  library(stringr)
  
  # will sequentially load in and tidy all of the relevant stats pages from
  # fbref for fpl. Start with standard stats, which will be used as base
  # and all others will be joined to that
  
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
  
  # now extract teams stats from standard
  standard_stats_for <- standard_tables[[1]]
  
  # make first row the column headers
  colnames(standard_stats_for) <- standard_stats_for[1, ]
  
  # append "90" for per 90 stats
  colnames(standard_stats_for)[-c(1:22)] <- paste0(
    colnames(standard_stats_for)[-c(1:22)], "90")
  
  # tidy up Min column
  standard_stats_for <- standard_stats_for |>
    # tidy up Min column
    mutate(`Min` = if_else(
      # if Min contains a comma
      grepl(",", `Min`),
      # remove and convert to numeric
      as.numeric(str_remove(`Min`, ",")),
      # otherwise just convert to numeric
      as.numeric(`Min`)))
  
  # remove first row
  standard_stats_for <- standard_stats_for[-1, ] |>
    # select columns we need
    select(c(Squad, `90s`, `Gls90`, `G-PK90`, `npxG90`, `PKatt`,
             `CrdY`, `CrdR`))
  
  # now extract teams stats from standard
  standard_stats_against <- standard_tables[[2]]
  
  # make first row the column headers
  colnames(standard_stats_against) <- standard_stats_against[1, ]
  
  # append "90" for per 90 stats
  colnames(standard_stats_against)[-c(1:22)] <- paste0(
    colnames(standard_stats_against)[-c(1:22)], "90")
  
  # tidy up Min column
  standard_stats_against <- standard_stats_against |>
    # tidy up Min column
    mutate(`Min` = if_else(
      # if Min contains a comma
      grepl(",", `Min`),
      # remove and convert to numeric
      as.numeric(str_remove(`Min`, ",")),
      # otherwise just convert to numeric
      as.numeric(`Min`)))
  
  # remove first row
  standard_stats_against <- standard_stats_against[-1, ] |>
    # select columns we need
    select(c(Squad, `G-PK90`, `npxG90`)) |>
    # append _against onto all variables apart from squad
    rename(`G-PK90_against` = `G-PK90`,
           `npxG90_against` = `npxG90`) |>
    # remove "vs " from Squad
    mutate(
      Squad = str_remove(Squad, "vs ")
    )
  
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
  
  # now extract team passing stats but against
  passing_stats <- passing_tables[[2]]
  
  # take first row to be column names
  colnames(passing_stats) <- passing_stats[1, ]
  
  # append _tot to columns 9 to 13
  colnames(passing_stats)[4:8] <- paste0(
    colnames(passing_stats)[4:8], "_tot"
  )
  
  # append _short to columns 9 to 11
  colnames(passing_stats)[9:11] <- paste0(
    colnames(passing_stats)[9:11], "_short"
  )
  
  # append _med to columns 12 to 14
  colnames(passing_stats)[12:14] <- paste0(
    colnames(passing_stats)[12:14], "_med"
  )
  
  # append _long to columns 15 to 17
  colnames(passing_stats)[15:17] <- paste0(
    colnames(passing_stats)[15:17], "_long"
  )
  
  passing_stats <- passing_stats |>
    # select columns we care about
    select(c(`Squad`, Cmp_tot, Att_tot)) |>
    # rename to append against for the passing stats and remove "vs" from Squad name
    rename(`Cmp_tot_against` = `Cmp_tot`,
           `Att_tot_against` = `Att_tot`) |>
    mutate(
      Squad = str_remove(Squad, "vs ")
    )
  
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
  
  # now extract team def_act stats, but against not for
  def_act_stats <- def_act_tables[[2]]
  
  # set first row to be the column names
  colnames(def_act_stats) <- def_act_stats[1, ]
  
  # change some column names so they're a bit clearer
  colnames(def_act_stats)[4] <- "Tkl_player"
  colnames(def_act_stats)[9] <- "Tkl_dribbler"
  
  def_act_stats <- def_act_stats |>
    # and select columns we care about
    select(c(Squad, Sh, `Tkl_player`, `Int`, `Clr`)) |>
    # rename variables slightly
    rename(`Sh_blks_against` = `Sh`,
           `Tkl_player_against` = `Tkl_player`,
           `Int_against` = `Int`,
           `Clr_against` = `Clr`) |>
    # remove "vs " from Squad name
    mutate(
      Squad = str_remove(Squad, "vs ")
    )
  
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
  
  # now extract squad misc stats, but against
  misc_stats <- misc_tables[[2]]
  
  # set first row to be column names
  colnames(misc_stats) <- misc_stats[1, ]
  
  misc_stats <- misc_stats |>
    # and select columns we want
    select(c(Squad, Recov)) |>
    # append against to Recov
    rename(`Recov_against` = `Recov`) |>
    # remove "vs " from Squad
    mutate(
      Squad = str_remove(Squad, "vs ")
    )
  
  # define url for shooting stats
  shooting_url <- paste0(
    "https://fbref.com/en/comps/9/", season, "/shooting/", season, "-",
    "Premier-League-Stats"
  )
  
  # use read_html_live (fbref must be dynamically generated with javascript
  # as read_html could not open connection) to read in HTML object
  shooting_html <- read_html_live(shooting_url)
  
  # extract table element
  shooting_tables <- shooting_html |>
    html_elements("table") |>
    html_table()
  
  # now extract squad shooting stats, but against
  shooting_stats <- shooting_tables[[2]]
  
  # set first row to be column names
  colnames(shooting_stats) <- shooting_stats[1, ]
  
  shooting_stats <- shooting_stats |>
    # and select columns we want
    select(c(Squad, `Sh/90`)) |>
    # append against to Sh90
    rename(`Sh_90_against` = `Sh/90`) |>
    # remove "vs " from Squad
    mutate(
      Squad = str_remove(Squad, "vs ")
    )
  
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
  
  # now extract team possession stats, but for
  possess_stats_for <- possess_tables[[1]]
  
  # make row 1 the column headers
  colnames(possess_stats_for) <- possess_stats_for[1, ]
  
  possess_stats_for <- possess_stats_for |>
    # and select columns of interest
    select(c(`Squad`, `Succ`, `Att Pen`)) |>
    # rename columns
    rename(`Succ_drib` = `Succ`,
           `Att_pen_touch` = `Att Pen`)
  
  # now extract team possession stats, but against
  possess_stats_against <- possess_tables[[2]]
  
  # make row 1 the column headers
  colnames(possess_stats_against) <- possess_stats_against[1, ]
  
  possess_stats_against <- possess_stats_against |>
    # and select columns of interest
    select(c(`Squad`, `Succ`, `Att Pen`)) |>
    # append against to Succ and Att Pen
    rename(`Succ_drib_against` = `Succ`,
           `Att_pen_touch_against` = `Att Pen`) |>
    # remove "vs " from Squad
    mutate(
      Squad = str_remove(Squad, "vs ")
    )
  
  # now join everything to play_time_stats
  all_stats <- standard_stats_for |> 
    left_join(standard_stats_against, by = join_by(`Squad` == `Squad`)) |>
    left_join(passing_stats, by = join_by(`Squad` == `Squad`)) |>
    left_join(def_act_stats, by = join_by(`Squad` == `Squad`)) |>
    left_join(possess_stats_for, by = join_by(`Squad` == `Squad`)) |>
    left_join(possess_stats_against, by = join_by(`Squad` == `Squad`)) |>
    left_join(misc_stats, by = join_by(`Squad` == `Squad`)) |>
    left_join(shooting_stats, by = join_by(`Squad` == `Squad`))
  
  # now define some new useful variables to have
  all_stats <- all_stats |>
    mutate(
      # first convert all appropiate variables to numeric (i.e.,
      # not Squad)
      across(!Squad, as.numeric)
    )
  
  return(all_stats)
  
}

team_stats_current <- get_team_stats(season = "2025-2026")
View(team_stats_current)
