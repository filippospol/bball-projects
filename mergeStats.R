# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Merge an NBA Stats table (hoopR, keyed by PLAYER_ID) with a Basketball Reference table
# (carrying POS), matching players by name. The hoopR table is the spine: all its rows and
# its PLAYER_ID are kept, bbref's columns are appended.
#
# Strategy:
#   1. normalise names on both sides (strip accents, case, punctuation, Jr./III suffixes)
#   2. join the exact matches  -- ~95% of players, instantly
#   3. fuzzy-match ONLY the leftovers, and only against bbref names not already used, accepting
#      a match only when it is close enough (bad matches are left NA, not forced onto a stranger)
#
# hoopR note: the nba_* stats endpoints return EVERY column as character. `convert_types`
# (default TRUE) type-guesses the merged frame so numeric columns come back numeric -- no more
# as.numeric(MIN) downstream -- while identity/text columns (PLAYER_ID, *_NAME, *_ABBREVIATION,
# SEASON, POS) are left untouched. Set FALSE to keep hoopR's raw character columns.
#
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mergeStats = function(nbacom, bbref, max_dist = 0.25, convert_types = TRUE) {
  pacman::p_load(dplyr, stringr, stringi, readr)

  if (!"PLAYER_ID"   %in% names(nbacom)) stop("`nbacom` must be a hoopR/NBA Stats table (needs PLAYER_ID).")
  if (!"POS"         %in% names(bbref))  stop("`bbref` must be a Basketball Reference table (needs POS).")
  if (!"PLAYER_NAME" %in% names(nbacom) || !"PLAYER_NAME" %in% names(bbref))
    stop("both tables need a PLAYER_NAME column to match on.")

  # --- normalise: accents -> ASCII, lower-case, drop punctuation + generational suffixes ---
  .norm = function(x) x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    str_replace_all("[^a-z ]", " ") %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b", " ") %>%
    str_squish()

  nbacom = nbacom %>% mutate(.key = .norm(PLAYER_NAME), .row = row_number())
  bb     = bbref  %>% mutate(.key = .norm(PLAYER_NAME)) %>%
    select(-PLAYER_NAME) %>%
    distinct(.key, .keep_all = TRUE)                    # one bbref row per name

  # --- 1) exact matches on the normalised key ---
  nbacom = nbacom %>% mutate(.bbkey = if_else(.key %in% bb$.key, .key, NA_character_))

  # --- 2) fuzzy-match only the leftovers, against only the unused bbref names ---
  miss = which(is.na(nbacom$.bbkey))
  pool = setdiff(bb$.key, nbacom$.bbkey)                # bbref names not already claimed
  if (length(miss) && length(pool)) {
    d    = adist(nbacom$.key[miss], pool)               # small matrix: leftovers x pool
    best = max.col(-d, ties.method = "first")           # nearest pool name per leftover
    dist = d[cbind(seq_along(miss), best)]
    tol  = pmax(nchar(nbacom$.key[miss]), nchar(pool[best])) * max_dist  # length-aware tolerance
    ok   = dist <= tol
    nbacom$.bbkey[miss[ok]] = pool[best[ok]]
  }

  n_un = sum(is.na(nbacom$.bbkey))
  if (n_un) message(n_un, " of ", nrow(nbacom),
                    " NBA players had no Basketball Reference match (left as NA).")

  # --- 3) join, restore original hoopR order, drop the helper keys ---
  out = nbacom %>%
    left_join(bb, by = c(".bbkey" = ".key")) %>%
    arrange(.row) %>%
    select(-.key, -.row, -.bbkey)

  # --- 4) fix hoopR's all-character columns (leave identity/text columns alone) ---
  if (convert_types) {
    protect  = str_detect(names(out),
                          regex("_ID$|_NAME$|_ABBREVIATION$|_CODE$|_CITY$|_SLUG$|^SEASON$|^POS$",
                                ignore_case = TRUE))
    chr_cols = names(out)[!protect & vapply(out, is.character, logical(1))]
    if (length(chr_cols)) out = out %>% mutate(across(all_of(chr_cols), parse_guess))
  }

  out
}

# # NOT RUN -- example:
# pacman::p_load(tidyverse, hoopR)
# source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_player_stats.R")
#
# season = "2024-25"
# nbacom = nba_leaguedashplayerstats(season = season, per_mode = "PerGame") %>% pluck(1)  # all character
# bbref  = bbref_player_stats(season = season, mode = "advanced")
# mydf   = mergeStats(nbacom, bbref)   # hoopR spine + bbref cols, numeric columns restored
