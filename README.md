## Analysing Basketball Data

<p>
<a href="https://github.com/filippospol/bball-projects/stargazers"><img src="https://img.shields.io/github/stars/filippospol/bball-projects?logo=github&color=f5c518" alt="Stars" /></a>
<a href="https://github.com/filippospol/bball-projects/commits/main"><img src="https://img.shields.io/github/last-commit/filippospol/bball-projects?logo=git&logoColor=white" alt="Last commit" /></a>
</p>

R scripts for scraping and analysing basketball data.

- `bbref/` — scraper functions for Basketball Reference
- `pbpstats/` — scraper functions for PBP Stats
- `lineups/` — lineup and rotation analysis: five-man `{gt}` tables, minutes and rotation heatmaps, lineup continuity
- `player-profiling/` — positions, data-driven offensive and defensive styles, and defensive matchup metrics
- `misc/` — shared helpers (the name-matching merge) and the `{gt}` / `{ggplot2}` theme
- `shiny-apps/` — code and data behind the Shiny apps

Each folder has its own README with the functions and a quick example. The scrapers go easy on the sites they hit (a shared throttle with automatic back-off), seasons go in as nba.com strings like `"2026-27"` throughout, and most scripts `source()` their helpers straight from this repo on first run.
