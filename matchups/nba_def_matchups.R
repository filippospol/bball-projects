#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#
# Season-long DEFENSIVE matchups: every defender vs the offensive players
# they guarded, over a full season. Pulled from the NBA Stats
# leagueseasonmatchups endpoint via hoopR (no more hand-rolled headers).
# Author: Filippos Polyzos
# Source: NBA Stats
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

nba_def_matchups = function(season) {
  pacman::p_load(tidyverse, hoopR)
  
  # all team ids, then pull each team's defensive matchups and stack them.
  # def_team_id filters to defenders on that team, so looping all 30 gives the
  # full league defensive-matchup table (one row per defender-offender pair).
  teams = nba_leaguedashteamstats(season = season) %>% pluck(1) %>% pull(TEAM_ID)
  
  map_dfr(teams, function(def_team) {
    nba_leagueseasonmatchups(def_team_id = def_team, per_mode = "Totals", season = season) %>%
      pluck("SeasonMatchups") %>%
      as_tibble()
  })
}
