# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script replicates the Lineup Continuity chart (h/t Owen Phillips).
# Distinct starting lineups per game vs. how far a team is beating/missing its preseason win line.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lineup_continuity = function(season, output = c("chart", "data")) {
  output = match.arg(output)
  pacman::p_load(tidyverse, ggimage, glue, hoopR, magick)
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_lineups.R")
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_odds.R")

  # Distinct starting lineups + preseason win line -- both keyed by nba.com team code (TEAM):
  lineups = bbref_lineups(season)                              # SEASON, TEAM, LINEUPS
  odds    = bbref_odds(season) %>% select(SEASON, TEAM, PROJECTED_WINS)

  # nba.com code -> TEAM_ID, taken live from hoopR. The player table's TEAM_ABBREVIATION is
  # canonical nba.com (BKN/CHA/PHX), which is exactly what bbref_* maps its codes to, so the
  # acronyms match by construction -- no hardcoded ids.
  team_map = nba_leaguedashplayerstats(season = season) %>%
    pluck(1) %>%
    distinct(TEAM = TEAM_ABBREVIATION, TEAM_ID = as.character(TEAM_ID)) %>%
    filter(TEAM != "", TEAM_ID != "0")

  missing = setdiff(unique(lineups$TEAM), team_map$TEAM)
  if (length(missing)) message("No nba.com id for team code(s): ", paste(missing, collapse = ", "))

  # Actual W/L from hoopR, joined on the team id:
  teams = nba_leaguedashteamstats(season = season) %>%
    pluck(1) %>%
    transmute(TEAM_ID = as.character(TEAM_ID), W = as.numeric(W), L = as.numeric(L))

  continuity = lineups %>%
    left_join(odds,     by = c("SEASON", "TEAM")) %>%
    left_join(team_map, by = "TEAM") %>%        # bbref code -> TEAM_ID
    left_join(teams,    by = "TEAM_ID") %>%     # TEAM_ID   -> W/L
    filter(!is.na(PROJECTED_WINS), !is.na(TEAM_ID), !is.na(LINEUPS)) %>%
    mutate(
      GP         = W + L,
      PWIN       = PROJECTED_WINS / 82,
      TWIN       = W / GP,
      DELTA      = TWIN - PWIN,
      CONTINUITY = LINEUPS / GP
    )

  if (output == "data") return(continuity)

  # NBA CDN serves team logos as SVG; geom_image wants raster, so fetch each logo once and
  # rasterize it to a cached PNG (cached in the session tempdir, so re-runs don't re-download).
  logo_png = function(team_id) {
    dir = file.path(tempdir(), "nba-logos")
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    out = file.path(dir, paste0(team_id, ".png"))
    if (!file.exists(out))
      glue("https://cdn.nba.com/logos/nba/{team_id}/primary/L/logo.svg") %>%
        image_read() %>% image_resize("160x") %>% image_write(out, format = "png")
    out
  }
  continuity = continuity %>% mutate(LOGO = map_chr(TEAM_ID, logo_png))

  # Chart:
  ggplot(continuity, aes(x = CONTINUITY, y = DELTA)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(-max(abs(continuity$DELTA)), max(abs(continuity$DELTA)))) +
    scale_x_continuous(limits = c(0, max(continuity$CONTINUITY))) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.4) +
    geom_image(aes(image = LOGO), size = 0.06) +
    labs(title = paste("Continuity Chart:", season),
         subtitle = paste("Source: Basketball Reference | As of", format(Sys.Date(), "%m/%d/%y")),
         x = "# of Distinct Starting Lineups / Games Played",
         y = "Current W% - Projected W%",
         caption = "Inspired by Owen Phillips")
}

# NOT RUN
# lineup_continuity("2024-25")            # the chart
# lineup_continuity("2024-25", "data")    # the tibble behind it
