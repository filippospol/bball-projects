# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script retrieves 5-man lineup stats from the NBA API for a given season, with each lineup
# rendered as a row of five player headshots (surname underneath) + the team logo, for {gt}.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nba_5manlineups = function(season, min_poss = 0) {
  pacman::p_load(tidyverse, hoopR, glue)
  
  rawLineups = nba_leaguedashlineups(group_quantity = 5, measure_type = "Advanced", season = season) %>%
    pluck(1) %>%
    select(LINEUP_ID = GROUP_ID, LINEUP_NAME = GROUP_NAME, TEAM_ID, TEAM_ABBREVIATION,
           GP, W, L, MIN, POSS, OFF_RATING, DEF_RATING, NET_RATING) %>%
    mutate(across(c(GP, W, L, MIN, POSS, OFF_RATING, DEF_RATING, NET_RATING), as.numeric))
  
  # surname = last name token WITH any trailing generational suffix re-attached, so nothing is
  # dropped: "Michael Porter Jr." -> "Porter Jr.", "Gary Payton II" -> "Payton II",
  # "Kentavious Caldwell-Pope" -> "Caldwell-Pope", "Nikola Jokic" -> "Jokic".
  surname = function(nm) {
    suf  = str_extract(nm, regex("\\s+(jr|sr|ii|iii|iv|v)\\.?$", ignore_case = TRUE))  # " Jr." / NA
    base = str_remove (nm, regex("\\s+(jr|sr|ii|iii|iv|v)\\.?$", ignore_case = TRUE))  # name w/o suffix
    paste0(word(base, -1), coalesce(suf, ""))                                         # re-attach suffix
  }
  
  # one <img> + surname card per player; five cards make the lineup cell
  player_card = function(id, name) glue(
    "<div style='display:inline-block;width:56px;text-align:center;vertical-align:top;margin:0 1px;'>",
    "<img src='https://cdn.nba.com/headshots/nba/latest/1040x760/{id}.png' width='48'>",
    "<div style='font-size:8px;line-height:1.1;'>{surname(name)}</div></div>"
  )
  
  styleLineups = rawLineups %>%
    mutate(
      TEAM_ID = glue("https://cdn.nba.com/logos/nba/{TEAM_ID}/primary/L/logo.svg"),
      ids     = str_extract_all(LINEUP_ID, "[0-9]+"),      # five player ids
      names   = str_split(LINEUP_NAME, fixed(" - ")),      # split ONLY on " - ", not inner hyphens
      # pair id <-> name, cap to 5, concatenate the five cards into one HTML cell
      LINEUP_HTML = map2_chr(ids, names,
                             ~ paste0(player_card(head(.x, 5), head(.y, 5)), collapse = ""))
    ) %>%
    select(LINEUP_HTML, TEAM_ID, GP:NET_RATING)
  
  if (min_poss > 0) styleLineups = filter(styleLineups, POSS >= min_poss)
  styleLineups
}

# NOT RUN
# Example: 10 random lineups (>=200 poss) for 2024-25 in a {gt} table.
# LINEUP_HTML is plain HTML -> render it with fmt_markdown (NOT gt::html()).
# pacman::p_load(gt, gtExtras)
# lineups25 = nba_5manlineups(season = "2026-27", min_poss = 50)
# 
# set.seed(14)
# lineups25 %>%
#   select(1:3, 7:10) %>%
#   { .[sample(nrow(.), 10), ] } %>%
#   arrange(-POSS) %>%
#   gt() %>%
#   cols_align(align = "center") %>%
#   fmt_markdown(columns = 1) %>%
#   gt_img_rows(columns = 2, height = 30) %>%
#   cols_label(LINEUP_HTML = "Lineup", TEAM_ID = "Team", GP = "Games", POSS = "Poss.",
#              OFF_RATING = "ORtg", DEF_RATING = "DRtg", NET_RATING = "NetRtg")
