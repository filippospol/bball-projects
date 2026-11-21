# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Minutes-by-game heatmap for a team-season (original light / cream->orange theme), now with
# player headshots beside each name, MIN / GP summary columns, and the game axis grouped by month.
# Source: ESPN box scores via hoopR.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

minutes_heatmap = function(season, team, last_n = NULL) {
  pacman::p_load(tidyverse, hoopR, ggimage, glue, scales)
  
  yr = as.numeric(substr(season, 1, 4)) + 1           # ESPN uses the end year ("2022-23" -> 2023)
  
  raw = load_nba_player_box(seasons = yr) %>%
    filter(season_type == 2, team_abbreviation == team)   # 2 = regular season
  if (!nrow(raw))
    stop("No regular-season box scores for \"", team, "\" in ", season,
         ". `team` is the ESPN abbreviation (e.g. ATL, GS, NY, SA, UTAH, WSH).")
  team_label = if ("team_display_name" %in% names(raw)) raw$team_display_name[1] else team
  
  # hoopR renamed this column over time (min -> minutes); grab whichever is present
  min_col = intersect(c("minutes", "min"), names(raw))[1]
  if (is.na(min_col)) stop("Couldn't find a minutes column in load_nba_player_box() output.")
  
  box = raw %>%
    transmute(player = athlete_display_name, athlete_id = as.character(athlete_id),
              min = suppressWarnings(as.numeric(.data[[min_col]])),
              game_id, game_date = as.Date(game_date))
  
  # chronological game index + month label (optionally keep only the last N games)
  games = box %>% distinct(game_id, game_date) %>% arrange(game_date)
  if (!is.null(last_n)) games = tail(games, last_n)
  games = games %>% mutate(game_number = row_number(), month = format(game_date, "%b"))
  box = box %>% inner_join(games, by = c("game_id", "game_date"))
  Nx  = nrow(games)
  
  # player order + MIN / GP summary + headshot url (one per player)
  psum = box %>%
    group_by(player, athlete_id) %>%
    summarise(MIN = sum(min, na.rm = TRUE), GP = sum(min > 0, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(MIN)) %>%
    mutate(HEAD = as.character(glue("https://a.espncdn.com/i/headshots/nba/players/full/{athlete_id}.png")))
  lv = rev(psum$player)                                # factor levels so the top row = most minutes
  Ny = nrow(psum)
  psum = psum %>% mutate(player = factor(player, levels = lv))
  
  # full player x game grid (DNPs -> 0), with a per-cell label colour
  grid = expand_grid(player = as.character(psum$player), game_number = 1:Nx) %>%
    left_join(box %>% select(player, game_number, min), by = c("player", "game_number")) %>%
    mutate(min = replace_na(min, 0),
           player = factor(player, levels = lv),
           lab_col = if_else(min > 0, "#333333", "#c9b8a6"))
  
  # one label + tick per month, at that month's first game
  mstart = games %>% group_by(month) %>% summarise(x = min(game_number), .groups = "drop") %>% arrange(x)
  
  ggplot(grid, aes(x = game_number, y = player)) +
    geom_tile(aes(fill = min), color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(min), color = lab_col), size = 2.6) +
    scale_color_identity() +
    # headshots just left of the grid (geom_image, so they actually render as images)
    geom_image(data = psum, aes(x = -0.2, y = player, image = HEAD), size = 0.065, inherit.aes = FALSE) +
    # MIN / GP columns + headers
    geom_text(data = psum, aes(x = Nx + 1.6, y = player, label = MIN),
              color = "#333333", fontface = "bold", size = 2.9, inherit.aes = FALSE) +
    geom_text(data = psum, aes(x = Nx + 2.6, y = player, label = GP),
              color = "#8a8a8a", size = 2.9, inherit.aes = FALSE) +
    annotate("text", x = Nx + 1.6, y = Ny + 0.8, label = "MIN", fontface = "bold", size = 3, color = "#555555") +
    annotate("text", x = Nx + 2.6, y = Ny + 0.8, label = "GP",  fontface = "bold", size = 3, color = "#555555") +
    # month brackets above the grid
    geom_segment(data = mstart, aes(x = x - 0.5, xend = x - 0.5, y = Ny + 0.6, yend = Ny + 1.4),
                 color = "#999999", linewidth = 0.4, inherit.aes = FALSE) +
    geom_text(data = mstart, aes(x = x - 0.4, y = Ny + 1.4, label = month),
              hjust = 0, vjust = 1, color = "#777777", size = 3, inherit.aes = FALSE) +
    scale_fill_gradient(low = "#fff6e5", high = "#ffa500", guide = "none") +
    scale_x_continuous(breaks = 1:Nx, expand = expansion(add = c(0.5, 0.6))) +
    scale_y_discrete(limits = lv, expand = expansion(add = c(0.6, 1.6))) +
    coord_cartesian(clip = "off") +
    labs(title = glue("{team_label} · {season}"),
         subtitle = "Minutes heatmap · Source: ESPN",
         x = "Game #", y = NULL, caption = "Chart by: @filippos_pol") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid    = element_blank(),
      axis.text.y   = element_text(hjust = 1, size = 7, color = "#222222"),
      axis.text.x   = element_text(size = 8, color = "#555555"),
      axis.title.x  = element_text(size = 9, color = "#555555"),
      plot.title    = element_text(face = "bold", hjust = 0.5, size = 15),
      plot.subtitle = element_text(color = "#777777", hjust = 0.5, size = 10),
      plot.caption  = element_text(color = "#999999", size = 8),
      plot.margin   = margin(t = 28, r = 22, b = 12, l = 12)
    )
}

# NOT RUN
# minutes_heatmap("2025-26", "MIN")              # whole regular season so far
# minutes_heatmap("2025-26", "MIN", last_n = 10)  # just the last 10 games
