#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#
# Offensive styles (7) via role-weighted, SEASON-RELATIVE PCA + GMM.
#
# Seven archetypes: Primary Ball Handler, Shot Creator, Secondary Creator,
# Facilitator, Slashing Wing, Movement Shooter, Scoring Big, Roll & Cut Big.
# (A rare 8th cluster, "Offensive Engine" = elite hubs, is folded into Primary
#  Ball Handler / Scoring Big by shot profile.)
#
# Pulls ONLY the two bbref modes it needs: shooting + advanced.
# Features are z-scored WITHIN each season, so a style means the same thing across
# eras (a 2015 Movement Shooter and a 2026 one both shoot far more 3s than their
# own league-year), despite the decade-long rise in 3PA / fall in mid-range.
#
# Qualifier = rate AND volume: MPG >= min_mpg AND GP >= floor, where floor = min_gp for
# a completed season but auto-drops to half the games played in an IN-PROGRESS season
# (so the model fits on current data alone, no past seasons needed). Below-floor players
# are assigned to their nearest style and flagged LOW_SAMPLE = TRUE.
#
# Author: Filippos Polyzos
# Source: Basketball Reference (via bbref_player_stats)
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

offensive_styles = function(seasons, min_mpg = 15, min_gp = 25) {
  pacman::p_load(tidyverse)
  # mclust + clue are used by NAMESPACE only (never attached): mclust exports its own
  # map() which would mask purrr::map and break bbref_helpers' unqualified map() calls.
  for (p in c("mclust", "clue")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_player_stats.R")

  key   = c("SEASON", "PLAYER_NAME", "TEAM")
  feats = c("USG_PCT","AST_PCT","TOV_PCT","FTA_PER_FGA_PCT","PCT_AST_FG2","PCT_AST_FG3",
            "ORB_PCT","PCT_FGA_00_03","PCT_FGA_FG3A","AVG_DIST","PCT_FGA_DUNK","PCT_FG3A_CORNER3")
  wcol  = c("USG_PCT","AST_PCT","PCT_AST_FG2","PCT_AST_FG3")   # centrality -> upweighted

  pull = function(mode) map_dfr(seasons, ~ bbref_player_stats(.x, mode = mode))
  adv = pull("advanced"); sh = pull("shooting")
  dat = adv %>%
    select(all_of(key), POS, GP, MIN, USG_PCT, AST_PCT, TOV_PCT, FTA_PER_FGA_PCT, ORB_PCT) %>%
    left_join(sh %>% select(all_of(key), PCT_FGA_00_03, PCT_FGA_FG3A, AVG_DIST,
                            PCT_FGA_DUNK, PCT_FG3A_CORNER3, PCT_AST_FG2, PCT_AST_FG3), by = key) %>%
    group_by(SEASON, PLAYER_NAME) %>%                           # dedupe traded -> combined "A/B" row
    filter(if (any(str_detect(TEAM, "/"))) str_detect(TEAM, "/") else TRUE) %>% slice(1) %>% ungroup() %>%
    mutate(across(all_of(feats), as.numeric), GP = as.numeric(GP),
           MPG = as.numeric(MIN) / GP) %>%
    group_by(SEASON) %>% mutate(GP_FLOOR = pmin(min_gp, 0.5 * max(GP, na.rm = TRUE))) %>% ungroup() %>%
    mutate(across(all_of(feats), ~ replace_na(., median(., na.rm = TRUE))))

  qual = dat %>% filter(MPG >= min_mpg, GP >= GP_FLOOR)

  # ---- season-relative z-scoring (stats learned on qualified players) ----
  sstats = qual %>% group_by(SEASON) %>%
    summarise(across(all_of(feats), list(m = ~mean(.x), s = ~sd(.x)), .names = "{.col}.{.fn}"), .groups = "drop")
  zmat = function(df) {
    j = left_join(df["SEASON"], sstats, by = "SEASON")
    sapply(feats, function(f) (df[[f]] - j[[paste0(f, ".m")]]) / j[[paste0(f, ".s")]])
  }
  wv  = ifelse(feats %in% wcol, 1.8, 1)
  Zq  = zmat(qual)
  pc  = prcomp(sweep(Zq, 2, wv, "*"), center = FALSE, scale. = FALSE)
  gm  = mclust::Mclust(pc$x[, 1:7], G = 8, modelNames = "VVV", verbose = FALSE)
  qual$cl = gm$classification; qual$PROB = apply(gm$z, 1, max)

  # ---- auto-label 8 clusters from z-centroids ----
  cent = t(sapply(sort(unique(qual$cl)), function(k) colMeans(Zq[qual$cl == k, , drop = FALSE]))); colnames(cent) = feats
  proto = list(
    "Offensive Engine"     = c(USG_PCT=2, AST_PCT=1.5, PCT_AST_FG2=-1, PCT_AST_FG3=-1),
    "Primary Ball Handler" = c(USG_PCT=1, AST_PCT=1, PCT_FGA_FG3A=0.5, PCT_FGA_00_03=-0.5, PCT_AST_FG2=-1),
    "Secondary Creator"    = c(USG_PCT=0.5, AST_PCT=0.3, PCT_FGA_00_03=0.7, AVG_DIST=-0.5),
    "Facilitator"          = c(AST_PCT=2, TOV_PCT=1, USG_PCT=-0.5, PCT_FGA_DUNK=-1, PCT_FGA_FG3A=-0.3),
    "Slashing Wing"        = c(USG_PCT=-0.8, PCT_FGA_00_03=0.6, PCT_FGA_FG3A=0.4, PCT_AST_FG2=0.8, PCT_FGA_DUNK=0.3),
    "Movement Shooter"     = c(PCT_FGA_FG3A=2, AVG_DIST=1.5, PCT_FGA_00_03=-1.2, PCT_AST_FG3=1, USG_PCT=-0.5),
    "Scoring Big"          = c(ORB_PCT=1.2, PCT_FGA_00_03=0.6, PCT_FGA_FG3A=-0.4, AST_PCT=-0.4, PCT_FGA_DUNK=0.4),
    "Roll & Cut Big"       = c(PCT_FGA_00_03=2, PCT_FGA_DUNK=2, ORB_PCT=1.5, PCT_FGA_FG3A=-2, AVG_DIST=-2, AST_PCT=-0.8))
  pm = matrix(0, length(proto), length(feats), dimnames = list(names(proto), feats))
  for (s in names(proto)) pm[s, names(proto[[s]])] = proto[[s]]
  lab = setNames(colnames(cent %*% t(pm))[as.integer(clue::solve_LSAP(cent %*% t(pm), maximum = TRUE))], sort(unique(qual$cl)))

  # ---- assign below-floor players to nearest style (flagged) ----
  sub = dat %>% filter(!(MPG >= min_mpg & GP >= GP_FLOOR))
  if (nrow(sub)) {
    pr = predict(gm, predict(pc, sweep(zmat(sub), 2, wv, "*"))[, 1:7])
    sub$cl = pr$classification; sub$PROB = apply(pr$z, 1, max)
  }

  out = bind_rows(mutate(qual, LOW_SAMPLE = FALSE), if (nrow(sub)) mutate(sub, LOW_SAMPLE = TRUE)) %>%
    mutate(STYLE = lab[as.character(cl)])

  # ---- fold "Offensive Engine" into Primary Ball Handler / Scoring Big by shot profile ----
  z_all = zmat(out)
  interior = z_all[, "PCT_FGA_00_03"] + z_all[, "ORB_PCT"] - z_all[, "PCT_FGA_FG3A"]   # >0 = big/interior
  out$STYLE = ifelse(out$STYLE == "Offensive Engine",
                     ifelse(interior > 0, "Scoring Big", "Primary Ball Handler"), out$STYLE)

  out %>% transmute(SEASON, PLAYER_NAME, TEAM, POS, MPG = round(MPG, 1),
                    STYLE, PROB = round(PROB, 2), LOW_SAMPLE) %>%
    arrange(SEASON, desc(MPG))
}

# NOT RUN
# off = offensive_styles(c("2014-15","2015-16","2016-17","2017-18","2018-19","2019-20",
#                          "2020-21","2021-22","2022-23","2023-24","2024-25","2025-26","2026-27"))
# off %>% count(SEASON, STYLE) %>% pivot_wider(names_from = STYLE, values_from = n)
