#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#
# Quantifying matchup difficulty
# Author: Filippos Polyzos
# Sources: NBA Stats matchups (nba_def_matchups) - bbref advanced (OBPM + usage)
#
# For each defender: the possession-weighted average OFFENSIVE THREAT of the players they
# guard, where a player's threat blends offensive IMPACT (OBPM) and USAGE -- guarding a
# high-usage engine is a heavier load than guarding an equally-rated low-usage role player.
# Higher = you draw the stars, lower = you get hidden on weak offensive players. It says WHO
# you draw, not how well you defend them (pair with a defensive-impact read for that).
#
# Threat = (1 - w_usg) * z(OBPM) + w_usg * z(USG), standardised over rotation players (so a
# small-sample OBPM/usage doesn't set the scale). Low-minute DEFENDERS are split into their
# own bucket (LOW_MINUTE = TRUE), z-scored apart, so bench noise doesn't distort the board.
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

matchup_difficulty = function(season, w_usg = 0.5, min_min = 300, min_off_min = 500, min_matchup_min = 5) {
  pacman::p_load(tidyverse,hoopR,stringi)
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/player-profiling/nba_def_matchups.R")
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_player_stats.R")

  # bbref (threat) and NBA Stats (matchups) spell names differently -> normalise to join by name
  nm = function(x) x %>% stri_trans_general("Latin-ASCII") %>% tolower() %>%
    str_replace_all("[^a-z ]"," ") %>% str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b"," ") %>% str_squish()
  zc = function(x, ref) (x - mean(ref, na.rm=TRUE)) / sd(ref, na.rm=TRUE)   # standardise x on a reference pool

  # defenders: ids, team, minutes (for the output + the low-minute split)
  players = nba_leaguedashplayerstats(season=season) %>%
    pluck(1) %>%
    transmute(PLAYER_ID=as.character(PLAYER_ID), TEAM_ID=as.character(TEAM_ID),
              TEAM=TEAM_ABBREVIATION, MIN=ceiling(as.numeric(MIN)))

  # offensive threat of every player = blend of IMPACT (OBPM) + USAGE, both from bbref advanced
  adv = bbref_player_stats(season, mode="advanced") %>%
    group_by(KEY=nm(PLAYER_NAME)) %>%                                  # traded -> keep the combined "A/B" (full-season) row
    filter(if (any(str_detect(TEAM, "/"))) str_detect(TEAM, "/") else TRUE) %>% slice(1) %>% ungroup() %>%
    transmute(KEY, MIN=as.numeric(MIN), OBPM=as.numeric(OBPM), USG=as.numeric(USG_PCT)) %>%
    filter(!is.na(OBPM), !is.na(USG)) %>% distinct(KEY, .keep_all=TRUE)
  base = adv %>% filter(MIN >= min_off_min)                            # scale learned on rotation players only
  threat = adv %>% transmute(KEY, THREAT = (1 - w_usg) * zc(OBPM, base$OBPM) + w_usg * zc(USG, base$USG))

  # defensive matchups (>= min_matchup_min), tagged with the guarded player's threat
  matchups = nba_def_matchups(season=season) %>%
    select(DEF_PLAYER_ID,DEF_PLAYER_NAME,OFF_PLAYER_NAME,PARTIAL_POSS,MATCHUP_MIN) %>%
    separate(MATCHUP_MIN,into=c("X","Y"),sep=":") %>%
    mutate(X=as.numeric(X), Y=as.numeric(Y)/60, MATCHUP_MIN=X+Y, PARTIAL_POSS=as.numeric(PARTIAL_POSS)) %>%
    select(-c(X,Y)) %>%
    filter(MATCHUP_MIN>=min_matchup_min) %>%
    mutate(KEY=nm(OFF_PLAYER_NAME)) %>%
    left_join(threat, by="KEY")

  # difficulty = possession-weighted mean threat of players guarded; low-minute defenders split off
  diff = matchups %>%
    filter(!is.na(THREAT)) %>%
    group_by(DEF_PLAYER_ID,DEF_PLAYER_NAME) %>%
    summarise(DIFF_RAW=weighted.mean(THREAT, PARTIAL_POSS), .groups="drop") %>%
    left_join(players, by=c("DEF_PLAYER_ID"="PLAYER_ID")) %>%
    mutate(LOW_MINUTE = is.na(MIN) | MIN < min_min)

  # z-score the raw threat WITHIN the qualified pool only; low-minute players ranked below, unscored
  bind_rows(
    diff %>% filter(!LOW_MINUTE) %>% mutate(DIFFICULTY = scale(DIFF_RAW)[,1]),
    diff %>% filter(LOW_MINUTE)  %>% mutate(DIFFICULTY = NA_real_)
  ) %>%
    transmute(PLAYER_ID=DEF_PLAYER_ID, PLAYER_NAME=DEF_PLAYER_NAME, TEAM_ID, TEAM, MIN,
              DIFF_RAW=round(DIFF_RAW,2), DIFFICULTY=round(DIFFICULTY,2), LOW_MINUTE) %>%
    arrange(LOW_MINUTE, desc(DIFFICULTY)) %>%
    return()
}

# NOT RUN
# d = matchup_difficulty("2026-27")
# head(d, 20)                              # toughest assignments (qualified)
# d %>% filter(!LOW_MINUTE) %>% tail(20)   # most hidden defenders
