# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Season rotation / substitution heatmap for a team: for each game minute, the share of the
# team's games a player was on the floor. Headshots beside each name, game axis split into
# quarters (Q1-Q4). Source: ESPN play-by-play via hoopR.
#
# NOTE: ESPN substitution data is imperfect, so the reconstructed on-court lineups can have
# gaps. The function reports the average reconstructed on-court count (should be ~5); if it's
# off, the sub labelling / in-out mapping below is where to adjust.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rotation_heatmap = function(season, team, last_n = NULL) {
  pacman::p_load(tidyverse, hoopR, ggimage, glue, scales)
  
  yr       = as.numeric(substr(season, 1, 4)) + 1      # ESPN end year ("2024-25" -> 2025)
  GAME_LEN = 2880                                      # 48 min regulation, in seconds
  pick     = function(df, cands) { n = intersect(cands, names(df)); if (length(n)) n[1] else NA }
  
  # ---- box scores: team filter, starters, player names + headshots -------------------------
  boxraw = load_nba_player_box(seasons = yr) %>%
    filter(season_type == 2, team_abbreviation == team)
  if (!nrow(boxraw))
    stop("No regular-season box scores for \"", team, "\" in ", season,
         ". `team` is the ESPN abbreviation (e.g. ATL, GS, NY, SA, UTAH, WSH).")
  team_label = if ("team_display_name" %in% names(boxraw)) boxraw$team_display_name[1] else team
  my_team_id = as.character(boxraw$team_id[1])
  
  games = boxraw %>% distinct(game_id, game_date = as.Date(game_date)) %>% arrange(game_date)
  if (!is.null(last_n)) games = tail(games, last_n)
  
  starters = boxraw %>%
    filter(game_id %in% games$game_id, starter == TRUE) %>%
    group_by(game_id) %>% summarise(ids = list(as.character(athlete_id)), .groups = "drop") %>%
    filter(lengths(ids) == 5)                          # keep only clean 5-starter games
  
  pinfo = boxraw %>%
    transmute(athlete_id = as.character(athlete_id), pname = athlete_display_name) %>%
    distinct(athlete_id, .keep_all = TRUE) %>%
    mutate(HEAD = glue("https://a.espncdn.com/i/headshots/nba/players/full/{athlete_id}.png"))
  
  # ---- play-by-play: this team's substitutions, with absolute elapsed seconds --------------
  pbp = load_nba_pbp(seasons = yr) %>% filter(game_id %in% starters$game_id)
  per_col = pick(pbp, c("period_number", "period", "qtr"))
  clk_col = pick(pbp, c("clock_display_value", "clock_display", "time", "clock"))
  typ_col = pick(pbp, c("type_text", "type"))
  
  parse_clock = function(x) { p = str_split_fixed(x, ":", 2); as.numeric(p[, 1]) * 60 + as.numeric(p[, 2]) }
  elapsed = function(period, rem) {                    # seconds since tip
    plen = ifelse(period <= 4, 720, 300)
    done = ifelse(period <= 4, (period - 1) * 720, 4 * 720 + (period - 5) * 300)
    done + (plen - rem)
  }
  
  subs = pbp %>%
    filter(str_detect(tolower(.data[[typ_col]]), "substitut"), as.character(team_id) == my_team_id) %>%
    transmute(game_id,
              in_id  = as.character(athlete_id_1),     # ASSUMPTION: _1 enters, _2 leaves
              out_id = as.character(athlete_id_2),
              t = elapsed(as.numeric(.data[[per_col]]), parse_clock(.data[[clk_col]]))) %>%
    filter(!is.na(t)) %>% arrange(game_id, t)
  
  # ---- reconstruct on-court lineups + sample every 15s -------------------------------------
  samples = seq(7.5, GAME_LEN - 7.5, by = 15)          # 192 points, 4 per game-minute
  sbin    = floor(samples / 60)                        # minute bin 0..47
  spb     = length(samples) / 48
  
  one_game = function(gid) {
    st = starters$ids[[match(gid, starters$game_id)]]
    gs = subs %>% filter(game_id == gid)
    sets = vector("list", nrow(gs) + 1); sets[[1]] = st; cur = st
    for (i in seq_len(nrow(gs))) { cur = union(setdiff(cur, gs$out_id[i]), gs$in_id[i]); sets[[i + 1]] = cur }
    tibble(bin = sbin, player = sets[findInterval(samples, gs$t) + 1]) %>% unnest(player)
  }
  long    = map_dfr(starters$game_id, one_game)
  n_games = length(starters$game_id)
  
  avg_on = nrow(long) / (n_games * length(samples))
  if (abs(avg_on - 5) > 1)
    message(sprintf("Reconstructed on-court count averages %.1f (expected ~5). ESPN sub data may be ",
                    avg_on), "incomplete, or the in/out mapping (athlete_id_1 vs _2) needs flipping.")
  
  # ---- aggregate to per-player / per-minute share, order by total floor time ---------------
  cov0 = long %>% count(player, bin, name = "n")
  ord  = cov0 %>% group_by(player) %>% summarise(tot = sum(n), .groups = "drop") %>%
    arrange(desc(tot)) %>% left_join(pinfo, by = c("player" = "athlete_id"))
  lv   = rev(ord$player); Ny = nrow(ord)
  ord  = ord %>% mutate(player = factor(player, levels = lv))
  
  cov = expand_grid(player = ord$player %>% as.character() %>% unique(), bin = 0:47) %>%
    left_join(cov0, by = c("player", "bin")) %>%
    mutate(share = replace_na(n, 0) / (n_games * spb), player = factor(player, levels = lv))
  
  y_labels = setNames(ord$pname, as.character(ord$player))
  # same cream->orange family as the minutes heatmap, pushed darker at the top end
  pal = c("#fff6e5", "#ffe0a3", "#ffc24d", "#ffa500", "#e07b00", "#b35900")
  
  # ---- plot --------------------------------------------------------------------------------
  ggplot(cov, aes(x = bin + 0.5, y = player, fill = share)) +
    geom_tile() +
    geom_vline(xintercept = c(12, 24, 36), color = "#9a9a9a", linewidth = 0.6) +
    geom_image(data = ord, aes(x = -1.4, y = player, image = HEAD), size = 0.05, inherit.aes = FALSE) +
    scale_fill_gradientn(colours = pal, limits = c(0, 1), guide = "none") +
    scale_x_continuous(breaks = c(6, 18, 30, 42), labels = c("Q1", "Q2", "Q3", "Q4"),
                       expand = expansion(add = c(2.6, 0.3)), position = "top") +
    scale_y_discrete(limits = lv, labels = y_labels, expand = expansion(add = c(0.6, 0.6))) +
    coord_cartesian(clip = "off") +
    labs(title = glue("{team_label} · {season}"),
         subtitle = "Rotation — share of games on court by game minute · Source: ESPN",
         x = NULL, y = NULL, caption = "Chart by: @filippos_pol") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid    = element_blank(),
      axis.text.y   = element_text(hjust = 1, size = 8, color = "#222222"),
      axis.text.x   = element_text(size = 10, color = "#555555", face = "bold"),
      plot.title    = element_text(face = "bold", hjust = 0.5, size = 15),
      plot.subtitle = element_text(color = "#777777", hjust = 0.5, size = 9),
      plot.caption  = element_text(color = "#999999", size = 8),
      plot.margin   = margin(t = 18, r = 18, b = 12, l = 12)
    )
}

# NOT RUN
# rotation_heatmap("2026-27", "MIN")
