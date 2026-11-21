# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Player positions in Cleaning the Glass format for a given season:
#   Point - Combo - Wing - Forward - Big
#
# Method (per CTG's documented approach, https://cleaningtheglass.com/stats/guide/player_positions):
#   - PRIMARY   = position with the most minutes
#   - SECONDARY = position with the 2nd-most minutes, but ONLY if it exceeds `sec_cut`% of minutes
#   - the (primary, secondary) pair maps to a grouping.
# bbref's play-by-play PCT_1..PCT_5 ARE the per-position minute shares CTG describes,
# so the rule is applied straight to them -- no hand-tuned share thresholds.
#
# Forward/Big boundary is role-corrected ("Method B"): a PF-primary is a Big only if Centre
# shows up as a qualified secondary; otherwise he's a Forward. This offsets bbref's habit of
# tagging tall wings (Tatum, Banchero, Anunoby...) as ~95% PF.
#
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ctg_positions = function(season, sec_cut = 10) {
  pacman::p_load(tidyverse, hoopR)
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/mergeStats.R")
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_player_stats.R")

  # NBA.com identity (ids + team), fuzzy-matched onto bbref by name:
  playersNBA = nba_leaguedashplayerstats(season = season, measure_type = "Base") %>%
    pluck(1) %>%
    select(PLAYER_ID, PLAYER_NAME, TEAM_NBA = TEAM_ABBREVIATION)

  # Position shares from bbref's play-by-play table -- now a MODE of bbref_player_stats.
  # The five estimates arrive as data-stats pct_1..pct_5 -> PCT_1..PCT_5 after cleaning.
  pbp = bbref_player_stats(season = season, mode = "play_by_play")

  pos_cols = c(PG_PERCENT = "PCT_1", SG_PERCENT = "PCT_2", SF_PERCENT = "PCT_3",
               PF_PERCENT = "PCT_4", C_PERCENT  = "PCT_5")
  if (!all(pos_cols %in% names(pbp)))
    stop("ctg_positions(): position-estimate columns not found as expected (",
         paste(pos_cols, collapse = ", "), "). Actual columns: ",
         paste(names(pbp), collapse = ", "))

  playersBBRef = pbp %>%
    # one row per player: keep the full-season combined row for anyone traded
    group_by(PLAYER_NAME) %>%
    filter(if (any(str_detect(TEAM, "/"))) str_detect(TEAM, "/") else TRUE) %>%
    slice(1) %>% ungroup() %>%
    select(PLAYER_NAME, SEASON, POS, all_of(pos_cols))   # selects AND renames pct_* -> *_PERCENT

  playersAll = mergeStats(playersNBA, playersBBRef)

  # ---- CTG grouping from the minute-share vector (positions: 1=PG 2=SG 3=SF 4=PF 5=C) ----
  ctg_group_one = function(pg, sg, sf, pf, c) {
    shares = c(pg, sg, sf, pf, c)
    o  = order(shares, decreasing = TRUE)                 # ties resolve to the smaller position
    pr = o[1]                                             # primary
    se = if (shares[o[2]] > sec_cut) o[2] else NA_integer_  # qualified secondary (> sec_cut%)
    if (pr == 1) return(if (!is.na(se) && se == 2) "Combo"   else "Point")    # PG
    if (pr == 2) return(if (!is.na(se) && se == 1) "Combo"   else "Wing")     # SG
    if (pr == 3) return(if (!is.na(se) && se == 4) "Forward" else "Wing")     # SF
    if (pr == 4) return(if (!is.na(se) && se == 5) "Big"     else "Forward")  # PF  (Method B)
    "Big"                                                                     # C
  }

  playersAll %>%
    mutate(CTG_POS = pmap_chr(list(PG_PERCENT, SG_PERCENT, SF_PERCENT, PF_PERCENT, C_PERCENT),
                              ctg_group_one)) %>%
    select(SEASON, PLAYER_ID, PLAYER_NAME, TEAM = TEAM_NBA, CTG_POS) %>%
    return()
}
