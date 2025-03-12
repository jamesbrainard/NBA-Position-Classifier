##################################################
# NBA Player Data Extraction & Processing
# 
# This file contains functions for scraping
# NBA player data from Basketball Reference.
# 
# Author: James Brainard
##################################################

extract_and_process_player_data <- function(year = 2025) {
  library(rvest)
  library(dplyr)
  library(stringr)
  
  # URLs for Basketball Reference data
  base_url <- "https://www.basketball-reference.com/leagues/"
  urls <- list(
    per_game = paste0(base_url, "NBA_", year, "_per_game.html"),
    per_poss = paste0(base_url, "NBA_", year, "_per_poss.html"),
    advanced = paste0(base_url, "NBA_", year, "_advanced.html"),
    play_by_play = paste0(base_url, "NBA_", year, "_play-by-play.html"),
    shooting = paste0(base_url, "NBA_", year, "_shooting.html"),
    adj_shooting = paste0(base_url, "NBA_", year, "_adj_shooting.html")
  )
  
  # Reads tables from URL links
  pergame_table <- read_html(urls$per_game) |> html_element("table") |> html_table()
  per100_table <- read_html(urls$per_poss) |> html_element("table") |> html_table()
  advanced_table <- read_html(urls$advanced) |> html_element("table") |> html_table()
  playbyplay_table <- read_html(urls$play_by_play) |> html_element("table") |> html_table()
  shooting_table <- read_html(urls$shooting) |> html_element("table") |> html_table()
  adjustedshooting_table <- read_html(urls$adj_shooting) |> html_element("table") |> html_table()
  
  # Fixes tables with multidimensional data
  tidy_dimensions <- function(table) {
    table[1, ] <- as.list(paste0(colnames(table), "_", table[1, ]))
    colnames(table) <- table[1, ]
    table <- table[-1, ]
    table <- table %>% rename(Player = `_Player`)
    colnames(table) <- str_remove(colnames(table), "^_")
    return(table)
  }
  
  playbyplay_table <- tidy_dimensions(playbyplay_table)
  shooting_table <- tidy_dimensions(shooting_table)
  adjustedshooting_table <- tidy_dimensions(adjustedshooting_table)
  
  # Function to convert every column except 'Player' and 'Pos' into numeric
  convert_except_player_pos <- function(df) {
    df %>%
      mutate(across(
        !any_of(c("Player", "Pos")),
        ~ as.numeric(str_replace_all(., "[^0-9.-]", ""))
      ))
  }
  
  # Function to average data for each player across their teams
  # On basketball-reference.com, players who play on multiple teams in the same
  # season will sometimes be represented by multiple rows, depending on the table.
  average_aggregate_data <- function(df) {
    df %>%
      group_by(Player) %>%
      summarize(
        Pos = first(Pos),
        across(where(is.numeric), mean, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Calls convert_except_player on every table
  pergame_table <- convert_except_player_pos(pergame_table)
  advanced_table <- convert_except_player_pos(advanced_table)
  playbyplay_table <- convert_except_player_pos(playbyplay_table)
  shooting_table <- convert_except_player_pos(shooting_table)
  adjustedshooting_table <- convert_except_player_pos(adjustedshooting_table)
  
  # Calls average_aggregate_data on every table
  pergame_table <- average_aggregate_data(pergame_table)
  advanced_table <- average_aggregate_data(advanced_table)
  playbyplay_table <- average_aggregate_data(playbyplay_table)
  shooting_table <- average_aggregate_data(shooting_table)
  adjustedshooting_table <- average_aggregate_data(adjustedshooting_table)
  
  # Joins tables
  players <- pergame_table %>%
    left_join(advanced_table, by = "Player") %>%
    left_join(playbyplay_table, by = "Player") %>%
    left_join(shooting_table, by = "Player") %>%
    left_join(adjustedshooting_table, by = "Player") %>%
    select(-ends_with(".y")) %>%
    select(-ends_with(".x.x")) %>%
    select(-ends_with(".x.y"))
  
  # Gets rid of duplicate and/or irrelevant columns
  players <- players %>%
    select(-starts_with("Rk"), 
           -starts_with("Age"), 
           -starts_with("Team"), 
           -starts_with("Awards"),
           -MP,
           -starts_with("Position"))
  
  # Removes trailing '.x' instances from column names
  colnames(players) <- str_remove(colnames(players), "\\.x$")
  
  # Removes duplicate columns - still don't know why the join creates these
  players <- players[, !duplicated(names(players))]
  
  # Removes any characters that the RF model errors on.
  colnames(players) <- colnames(players) %>%
    str_replace("^\\+/-", "PlusMinus_") %>%
    str_replace("^2", "Two_") %>%
    str_replace("^3", "Three_") %>%
    str_replace("%$", "_pct") %>%
    str_replace_all("%", "pct") %>%
    str_replace_all("[/]", "_") %>%
    str_replace_all("\\.", "_") %>%
    str_replace_all("-", "_") %>%
    str_replace_all("\\+", "_plus")
  
  # Removes "League Average" row
  players <- players %>% filter(Player != "League Average")
  
  # Replaces all spaces with underscores
  players <- players %>%
    rename_with(~ str_replace_all(.x, " ", "_"))
  
  # Creates new variable "Starter", a boolean variable that denotes 
  # whether or not a player is considered a starter.
  # The NBA determines eligibility for the "Sixth Man of the Year" award, 
  # the award for best non-starter, by whether or not they started more
  # games than they came off the bench. I used the same metric here.
  players <- players %>% 
    mutate(Starter = ifelse(is.na(G), "NA", 
                            ifelse(is.na(GS), "NA",
                                   ifelse(G - GS < GS, "Starter", "Non-Starter"))))
  
  return(players)
}