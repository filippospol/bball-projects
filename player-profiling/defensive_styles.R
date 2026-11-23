#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#
# Defensive styles (7) via SEASON-RELATIVE PCA + k-means.
#
# Seven archetypes: Anchor Big, Interior Big, Mobile Big, Wing Stopper,
# Point of Attack, Perimeter Guard, Low Activity.
#
# Pulls ONLY the three bbref modes it needs: advanced + play_by_play + per100.
# Features are z-scored WITHIN each season (defensive stats drift less than shot
# profile, but this keeps the two engines consistent and era-robust).
#
# Qualifier = MPG >= min_mpg AND GP >= floor, where floor = min_gp for a completed
# season but auto-drops to half the games played in an IN-PROGRESS season (fits on
# current data alone). Below-floor players are assigned to their nearest style
# (nearest centroid) and flagged LOW_SAMPLE = TRUE.
#
# NOTE: bbref has no defensive tracking/matchup data, so these styles are built from
# activity (block/steal/rebound/fouls) + position estimate. Adding tracking/matchup
# features later would sharpen the perimeter roles (POA vs Chaser vs Helper).
#
# Author: Filippos Polyzos
# Source: Basketball Reference (via bbref_player_stats)
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

defensive_styles = function(seasons, min_mpg = 15, min_gp = 25) {
  pacman::p_load(tidyverse)
  if (!requireNamespace("clue", quietly = TRUE)) install.packages("clue")  # used by namespace, not attached
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_player_stats.R")

  key   = c("SEASON", "PLAYER_NAME", "TEAM")
  feats = c("BLK_PCT","STL_PCT","DRB_PCT","PCT_1","PCT_5","PF_PER_POSS")

  pull = function(mode) map_dfr(seasons, ~ bbref_player_stats(.x, mode = mode))
  adv = pull("advanced"); pbp = pull("play_by_play"); p100 = pull("per100")
  dat = adv %>%
    select(all_of(key), POS, GP, MIN, BLK_PCT, STL_PCT, DRB_PCT) %>%
    left_join(pbp  %>% select(all_of(key), PCT_1, PCT_5),  by = key) %>%
    left_join(p100 %>% select(all_of(key), PF_PER_POSS),   by = key) %>%
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
  Zq = zmat(qual)
  pc = prcomp(Zq, center = FALSE, scale. = FALSE)

  set.seed(0)
  km = kmeans(pc$x[, 1:5], centers = 7, nstart = 25)
  qual$cl = km$cluster

  # ---- auto-label 7 clusters from z-centroids ----
  cent = t(sapply(sort(unique(qual$cl)), function(k) colMeans(Zq[qual$cl == k, , drop = FALSE]))); colnames(cent) = feats
  proto = list(
    "Anchor Big"      = c(BLK_PCT=2, PCT_5=1.5, DRB_PCT=1),
    "Interior Big"    = c(PCT_5=1.5, DRB_PCT=1.5, BLK_PCT=0.3),
    "Mobile Big"      = c(PCT_5=0.6, DRB_PCT=0.6, BLK_PCT=0.3, PCT_1=-0.3),
    "Wing Stopper"    = c(PF_PER_POSS=1, PCT_1=-0.2, PCT_5=-0.3, STL_PCT=0.2),
    "Point of Attack" = c(STL_PCT=2, PCT_1=0.5),
    "Perimeter Guard" = c(PCT_1=2, PCT_5=-0.5),
    "Low Activity"    = c(BLK_PCT=-1, STL_PCT=-1, PF_PER_POSS=-1, DRB_PCT=-0.5))
  pm = matrix(0, length(proto), length(feats), dimnames = list(names(proto), feats))
  for (s in names(proto)) pm[s, names(proto[[s]])] = proto[[s]]
  sc  = cent %*% t(pm); sc = sc - min(sc)   # solve_LSAP needs nonnegative entries; a constant shift leaves the argmax assignment unchanged
  lab = setNames(colnames(sc)[as.integer(clue::solve_LSAP(sc, maximum = TRUE))], sort(unique(qual$cl)))

  # ---- below-floor players -> nearest k-means centroid (flagged) ----
  sub = dat %>% filter(!(MPG >= min_mpg & GP >= GP_FLOOR))
  if (nrow(sub)) {
    Ps = predict(pc, zmat(sub))[, 1:5]
    sub$cl = apply(Ps, 1, function(r) which.min(colSums((t(km$centers) - r)^2)))
  }

  bind_rows(mutate(qual, LOW_SAMPLE = FALSE), if (nrow(sub)) mutate(sub, LOW_SAMPLE = TRUE)) %>%
    mutate(STYLE = lab[as.character(cl)]) %>%
    transmute(SEASON, PLAYER_NAME, TEAM, POS, MPG = round(MPG, 1), STYLE, LOW_SAMPLE) %>%
    arrange(SEASON, desc(MPG))
}

# NOT RUN
# def = defensive_styles(c("2014-15","2015-16","2016-17","2017-18","2018-19","2019-20",
#                          "2020-21","2021-22","2022-23","2023-24","2024-25","2025-26","2026-27"))
# def %>% count(SEASON, STYLE) %>% pivot_wider(names_from = STYLE, values_from = n)
