# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# The rookie class for a season, from Basketball Reference's "Rookies - Season Stats" page.
#
#   bbref_rookies("2026-27")
#
# season : nba.com format, e.g. "2026-27".
# Returns the FULL rookies table, cleanly named, with SEASON first. Totals (FG..PTS),
# shooting percentages (FG_PCT, FG3_PCT, FT_PCT) and the page's per-game columns
# (MPG, PTS_PG, TRB_PG, AST_PG, STL_PG, BLK_PG). Also pulls each player's bbref id and
# their debut game (date + box-score id) out of the row's links.
#
# Notes / differences from the normal player-stats pages:
#   - The table id is "rookies" and the name column is data-stat="player" (not
#     "name_display"), so this selects the table by id rather than via the shared
#     .bbref_player_table() finder.
#   - There is NO team column and NO position column on this page: it is one
#     season-aggregate row per player, so there is nothing to remap to nba.com codes
#     and no traded-player regrouping to do.
#   - YRS is the player's season count as bbref lists it (1 for a first-year player).
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_helpers.R")

bbref_rookies = function(season) {

  yr  = .bbref_year(season)
  url = glue("https://www.basketball-reference.com/leagues/NBA_{yr}_rookies-season-stats.html")

  doc = bbref_read(url)

  tbl = .bbref_table(doc, "rookies")
  if (is.null(tbl)) stop("No 'rookies' table found at ", url)

  # --- Stat columns (text, keyed by data-stat) via the shared extractor ----------------------
  df = .bbref_extract(tbl)

  # --- Link-derived columns: bbref player id + debut game ------------------------------------
  # Re-walk the same non-thead body rows so the vectors line up with df row-for-row.
  rows = tbl %>% html_elements("tbody > tr")
  cls  = rows %>% html_attr("class") %>% tidyr::replace_na("")
  rows = rows[!str_detect(cls, "thead")]

  href_of = function(stat) rows %>%
    html_element(sprintf("[data-stat='%s'] a", stat)) %>% html_attr("href")

  player_href = href_of("player")
  debut_href  = href_of("debut")

  df$PLAYER_ID      = str_match(player_href, "/players/[a-z]/([a-z0-9]+)\\.html")[, 2]
  df$DEBUT_GAME_ID  = str_match(debut_href,  "/boxscores/([0-9A-Za-z]+)\\.html")[, 2]
  df$DEBUT_DATE     = as.Date(substr(df$DEBUT_GAME_ID, 1, 8), format = "%Y%m%d")

  # --- Clean names ---------------------------------------------------------------------------
  ren = c(
    player     = "PLAYER_NAME", age = "AGE", years = "YRS", g = "GP", mp = "MIN",
    debut      = "DEBUT",
    fg_pct     = "FG_PCT", fg3_pct = "FG3_PCT", ft_pct = "FT_PCT",
    mp_per_g   = "MPG", pts_per_g = "PTS_PG", trb_per_g = "TRB_PG",
    ast_per_g  = "AST_PG", stl_per_g = "STL_PG", blk_per_g = "BLK_PG"
  )
  df = df %>% select(-any_of("ranker"))
  nm = names(df)
  names(df) = ifelse(nm %in% names(ren), ren[nm], toupper(nm))

  # --- Shape + types -------------------------------------------------------------------------
  df = df %>%
    mutate(SEASON = season, .before = 1) %>%
    relocate(any_of(c("SEASON", "PLAYER_NAME", "PLAYER_ID", "AGE", "YRS",
                      "DEBUT_DATE", "DEBUT_GAME_ID", "DEBUT", "GP", "MIN"))) %>%
    .bbref_numify(keep_chr = c("SEASON", "PLAYER_NAME", "PLAYER_ID",
                               "DEBUT", "DEBUT_GAME_ID", "DEBUT_DATE")) %>%
    arrange(PLAYER_NAME)

  df
}
