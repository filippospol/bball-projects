#### Lineups & rotations

Scripts for looking at who plays together and when. Two of them draw straight to `{gt}` with player headshots and team logos; two are heatmaps; one replicates Owen Phillips' <a href="https://thef5.substack.com/p/health-luck" target="_blank" rel="noopener noreferrer">lineup continuity</a> chart.

They lean on the NBA API and ESPN (both via `hoopR`), so seasons go in as nba.com strings like `"2026-27"`. For the two ESPN-based heatmaps, `team` is the **ESPN** abbreviation, which isn't always nba.com's (e.g. `GS`, `NY`, `SA`, `UTAH`, `WSH`); the functions say so in the error if you miss.

| Function | What it does |
| --- | --- |
| `nba_5manlineups(season, min_poss)` | Five-man lineup advanced stats (GP, W, L, MIN, POSS, OFF/DEF/NET rating) from the NBA API, each lineup drawn as a row of five headshots with surnames underneath plus the team logo, ready for `{gt}`. Filter out the noise with `min_poss`. |
| `minutes_heatmap(season, team, last_n)` | Minutes-by-game heatmap for a team-season (cream → orange), with headshots beside each name, MIN / GP summary columns and the game axis grouped by month. ESPN box scores. |
| `rotation_heatmap(season, team, last_n)` | Season rotation / substitution heatmap: for each game minute, the share of the team's games a player was on the floor, headshots beside each name, axis split into quarters. ESPN play-by-play. |
| `lineup_continuity(season, output)` | The lineup-continuity chart: distinct starting lineups per game against how far a team is beating or missing its preseason win line. `output = "chart"` or `"data"`. |

A quick example:

```r
source("lineups/nba_5manlineups.R")
source("lineups/minutes_heatmap.R")

five <- nba_5manlineups("2026-27", min_poss = 100)
mins <- minutes_heatmap("2024-25", "BOS")
cont <- lineup_continuity("2024-25", output = "chart")
```

Two things to keep in mind. `last_n` on the heatmaps limits them to a team's most recent N games. And ESPN's substitution data is imperfect, so `rotation_heatmap` reconstructs the on-court five itself and can leave small gaps — it reports the average reconstructed on-court count (should land near 5), and if that's off, the sub in/out mapping inside the script is where to look. `lineup_continuity` also pulls `bbref/bbref_lineups.R` and `bbref/bbref_odds.R` the first time it runs.
