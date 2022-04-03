# Script to run and query all the fantasy and NFL data.
# Make sure to run the fpPtsvProjScrapeR.R file first, as that gets the data from Fantasy Pros for fantasy projections.

# Load libraries
library(tidymodels)
library(httr)
library(dplyr)
library(jsonlite)
library(nflfastR)
library(reshape2)
library(nflfastR)
library(directlabels)
library(nflreadr)
library(nflplotR)
library(kit)

future::plan("multisession")

# Load data

# Player weekly stats
stats <- nflfastR::load_player_stats(seasons = c(1999:2021))

# Player roster data
roster <- nflfastR::fast_scraper_roster(seasons = c(1999:2021))

# Get recent seasons PBP
# ids <- nflfastR::fast_scraper_schedules(seasons = c(2000:2021)) %>%
#   dplyr::filter(game_type == "REG") %>%
#   dplyr::pull(game_id)

# Get play-by-play
# pbp <- nflfastR::build_nflfastR_pbp(ids)y

# Load fantasy expectations
ff_exp <- nflreadr::load_ff_opportunity(seasons = c(2010:2021))

# Combine roster with stats
# Subset roster data to join
roster_sub <- roster %>%
  select(gsis_id, position, full_name)

# Join roster data to stats data to add position
stats <- stats %>%
  left_join(roster_sub, by = c("player_id" = "gsis_id")) %>%
  funique()

# Aggregate Fantasy Data
# Keep only offensive positions relevant to fantasy
off <- stats %>%
  filter(position %in% c("QB", "RB", "WR", "TE"))

# Create subset with more recent years
ff <- off %>%
  filter(season_type == 'REG' & season >= 2000) %>%
  select(player_name, full_name, player_id, position, recent_team, season, week, fantasy_points, fantasy_points_ppr) %>%
  unique()

# Modify some names
# plyrDf <- plyrDf %>%
#   mutate(
#     Name = case_when(
#       Name == "AJ Dillon" ~ "A.J. Dillon",
#       Name == "D.J. Moore" ~ "DJ Moore",
#       Name == "D.K. Metcalf" ~ "DK Metcalf",
#       Name == "Amon-Ra. St." ~ "Amon-Ra. St. Brown",
#       Name == "Equanimeous St." ~ "Equanimeous St. Brown",
#       Name == "Cedrick Wilson" ~ "Ced Wilson",
#       Name == "Joshua Palmer" ~ "Josh Palmer",
#       T ~ Name
#     )
#   )

# Join expectation data
# ff <- ff %>%
#   left_join(plyrDf, by = c("full_name" = "Name", "week" = "Week"))

ff <- ff %>%
  group_by(player_id, player_name, position, season) %>%
  mutate(
    pt_change = fantasy_points - lag(fantasy_points, order_by = week),
    pt_change_ppr = fantasy_points_ppr - lag(fantasy_points_ppr, order_by = week)
  ) %>%
  mutate(
    pt_change_pct = round(((fantasy_points - lag(fantasy_points)) / lag(fantasy_points)) * 100, 2),
    pt_change_pct_ppr = round(((fantasy_points_ppr - lag(fantasy_points_ppr)) / lag(fantasy_points_ppr)) * 100, 2),
  ) %>%
  ungroup() # %>%
# filter(!is.na(pt_change_pct) & !is.nan(pt_change_pct_ppr) & !is.infinite(pt_change_pct) & !is.infinite(pt_change_pct_ppr))

ff_agg <- ff %>%
  group_by(player_id, player_name, full_name, position, season) %>%
  summarise(
    n_games = n(),
    avg_pts = round(mean(fantasy_points), 2),
    avg_pts_ppr = round(mean(fantasy_points_ppr), 2),
    med_pts_ppr = round(median(fantasy_points_ppr), 2),
    avg_change = round(mean(pt_change, na.rm = T), 2),
    avg_change_ppr = round(mean(pt_change_ppr, na.rm = T), 2),
    sd_change = round(sd(pt_change, na.rm = T), 2),
    sd_change_ppr = round(sd(pt_change_ppr, na.rm = T), 2),
    avg_ab_change = round(mean(abs(pt_change), na.rm = T), 2),
    avg_ab_change_ppr = round(mean(abs(pt_change_ppr), na.rm = T), 2),
    avg_pt_pct = round(mean(pt_change_pct, na.rm = T), 2),
    avg_pt_pct_ppr = round(mean(pt_change_pct_ppr, na.rm = T), 2),
    abs_med_pt_pct = round(median(abs(pt_change_pct_ppr), na.rm = T), 2),
    total_pts = sum(fantasy_points_ppr, na.rm = T),
    bust_games = n_distinct(week[fantasy_points_ppr <= 5])
    # avg_proj = round(mean(Proj.Pts, na.rm = T), 2),
    # avg_var = round(mean(Var, na.rm = T), 2),
    # med_var = round(median(Var, na.rm = T), 2),
    # wks_ovr = n_distinct(week[Exceeded == "Yes"]),
     # wks_undr = n_distinct(week[Exceeded == "No"])
  )

#####################
# Clean up data for fantasy expectations
ff_exp_agg <- ff_exp %>%
  group_by(season, posteam, player_id, full_name, position) %>%
  summarise(
    n_games = n_distinct(week),
    avg_pts = round(mean(total_fantasy_points, na.rm = T), 3),
    avg_pts_diff = round(mean(total_fantasy_points_diff, na.rm = T), 3),
    avg_pts_exp = round(mean(total_fantasy_points_exp, na.rm = T), 3),
    sd_pts = round(sd(total_fantasy_points, na.rm = T), 3),
    sd_pts_diff = round(sd(total_fantasy_points_diff, na.rm = T), 3),
    sd_pts_exp = round(sd(total_fantasy_points_exp, na.rm = T), 3)
  ) %>%
  ungroup() %>%
  group_by(player_id) %>%
  arrange(season, .by_group = T) %>%
  mutate(
    year_in_league = 1:n(),
    avg_pts_shift = avg_pts - lag(avg_pts),
    avg_pts_diff_shift = avg_pts_diff - lag(avg_pts_diff),
    avg_pts_exp_shift = avg_pts_exp - lag(avg_pts_exp),
    sd_pts_shift = sd_pts - lag(sd_pts),
    sd_pts_diff_shift = sd_pts_diff - lag(sd_pts_diff),
    sd_pts_exp_shift = sd_pts_exp - lag(sd_pts_exp)
  )
