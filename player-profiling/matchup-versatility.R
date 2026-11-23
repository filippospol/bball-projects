#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#
# Quantifying matchup versatility (POSITIONAL)
# Author: Filippos Polyzos
# Sources: NBA Stats matchups (nba_def_matchups) - offensive_styles (for position)
#
# For each defender: how wide a RANGE OF POSITIONS they guard -- the possession-weighted
# spread (standard deviation) of the lineup position (1=PG .. 5=C) of the players they
# defend, z-scored. A lockdown point-of-attack wing who takes PG/SG/SF stars on different
# nights now reads as versatile; an anchor big who only ever guards the opposing 5 reads
# as low. (The old version spread over OFFENSIVE STYLE, which collapsed to ~1 for exactly
# the perimeter specialists -- Davion Mitchell, Payton Pritchard -- because the stars they
# guard all share one style; positional spread doesn't crush them.)
#
# Low-minute defenders are split into their own bucket (ranked separately, LOW_MINUTE = TRUE)
# so bench noise doesn't tie up the floor or distort the z-scores.
#
# NOTE: switchable bigs (guard 1 through 5) still rank high -- that IS positional versatility.
# To push pure rim-anchors down / perimeter stoppers up, weight by matchup_difficulty.
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

matchup_versatility = function(season, min_min = 300, min_matchup_min = 5) {
  pacman::p_load(tidyverse,hoopR,stringi)
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/player-profiling/nba_def_matchups.R")
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/player-profiling/offensive_styles.R")
  
  # bbref (styles/pos) and NBA Stats (matchups) spell names differently -> normalise to join by name
  nm  = function(x) x %>% stri_trans_general("Latin-ASCII") %>% tolower() %>%
    str_replace_all("[^a-z ]"," ") %>% str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b"," ") %>% str_squish()
  wsd = function(x, w) { m = weighted.mean(x, w); sqrt(weighted.mean((x - m)^2, w)) }  # possession-weighted SD
  
  # defenders: ids, team, minutes (for the output + the low-minute split)
  players = nba_leaguedashplayerstats(season=season) %>%
    pluck(1) %>%
    transmute(PLAYER_ID=as.character(PLAYER_ID), TEAM_ID=as.character(TEAM_ID),
              TEAM=TEAM_ABBREVIATION, MIN=ceiling(as.numeric(MIN)))
  
  # lineup position (1..5) of every player this season, from the bbref POS carried by offensive_styles
  pos1 = c(PG=1, SG=2, SF=3, PF=4, C=5)
  pos  = offensive_styles(season) %>%
    transmute(KEY=nm(PLAYER_NAME), POSNUM=unname(pos1[str_extract(POS, "PG|SG|SF|PF|C")])) %>%  # primary (first-listed)
    filter(!is.na(POSNUM)) %>%
    distinct(KEY, .keep_all=TRUE)
  
  # defensive matchups (>= min_matchup_min), tagged with the guarded player's position
  matchups = nba_def_matchups(season=season) %>%
    select(DEF_PLAYER_ID,DEF_PLAYER_NAME,OFF_PLAYER_NAME,PARTIAL_POSS,MATCHUP_MIN) %>%
    separate(MATCHUP_MIN,into=c("X","Y"),sep=":") %>%
    mutate(X=as.numeric(X), Y=as.numeric(Y)/60, MATCHUP_MIN=X+Y, PARTIAL_POSS=as.numeric(PARTIAL_POSS)) %>%
    select(-c(X,Y)) %>%
    filter(MATCHUP_MIN>=min_matchup_min) %>%
    mutate(KEY=nm(OFF_PLAYER_NAME)) %>%
    left_join(pos, by="KEY") %>%
    filter(!is.na(POSNUM))
  
  # positional versatility = possession-weighted spread of guarded positions (MEAN_POS says WHERE)
  vers = matchups %>%
    group_by(DEF_PLAYER_ID,DEF_PLAYER_NAME) %>%
    summarise(POSS=sum(PARTIAL_POSS), MEAN_POS=weighted.mean(POSNUM, PARTIAL_POSS),
              POS_SPREAD=wsd(POSNUM, PARTIAL_POSS), .groups="drop") %>%
    left_join(players, by=c("DEF_PLAYER_ID"="PLAYER_ID")) %>%
    mutate(LOW_MINUTE = is.na(MIN) | MIN < min_min)
  
  # z-score the spread WITHIN the qualified pool only; low-minute players ranked below, unscored
  bind_rows(
    vers %>% filter(!LOW_MINUTE) %>% mutate(VERSATILITY = scale(POS_SPREAD)[,1]),
    vers %>% filter(LOW_MINUTE)  %>% mutate(VERSATILITY = NA_real_)
  ) %>%
    transmute(PLAYER_ID=DEF_PLAYER_ID, PLAYER_NAME=DEF_PLAYER_NAME, TEAM_ID, TEAM, MIN,
              MEAN_POS=round(MEAN_POS,2), POS_SPREAD=round(POS_SPREAD,2),
              VERSATILITY=round(VERSATILITY,2), LOW_MINUTE) %>%
    arrange(LOW_MINUTE, desc(VERSATILITY)) %>%
    return()
}

# NOT RUN
# v = matchup_versatility("2026-27")
# head(v, 20)                       # most positionally versatile (qualified)
# v %>% filter(!LOW_MINUTE) %>% tail(15)  
