#### Player profiling

Classifying players by how they actually play, and quantifying the assignments defenders draw. Instead of the listed PG/SG/SF/PF/C, these scripts group players from their stats: positions in <a href="https://cleaningtheglass.com/stats/guide/player_positions" target="_blank" rel="noopener noreferrer">Cleaning The Glass</a> format, then data-driven offensive and defensive **styles**, then two matchup metrics built on the NBA's defensive-matchup data.

Seasons go in as nba.com strings (`"2026-27"`). The styles scripts take a *vector* of seasons; the matchup scripts take one.

| Function | What it does |
| --- | --- |
| `ctg_positions(season, sec_cut)` | Cleaning the Glass positions — Point, Combo, Wing, Forward, Big — from bbref's play-by-play position shares: primary = most minutes, secondary only if it clears `sec_cut`%, with a role-corrected Forward/Big boundary so tall wings (Tatum, Banchero…) don't all read as Bigs. |
| `offensive_styles(seasons, min_mpg, min_gp)` | Seven offensive archetypes — Primary Ball Handler, Shot Creator, Secondary Creator, Facilitator, Slashing Wing, Movement Shooter, Scoring Big, Roll & Cut Big — via season-relative PCA + a Gaussian mixture. Pulls only bbref `advanced` + `shooting`. |
| `defensive_styles(seasons, min_mpg, min_gp)` | Seven defensive archetypes — Anchor Big, Interior Big, Mobile Big, Wing Stopper, Point of Attack, Perimeter Guard, Low Activity — via season-relative PCA + k-means. Pulls only bbref `advanced` + `play_by_play` + `per100`. |
| `nba_def_matchups(season)` | The season-long defensive-matchup table — every defender against the players they guarded (partial possessions, matchup minutes) — from NBA Stats' `leagueseasonmatchups`, looped over all 30 teams. |
| `matchup_versatility(season, min_min, min_matchup_min)` | How wide a range of positions a defender guards: the possession-weighted spread (SD) of the guarded players' lineup position (1 = PG … 5 = C), z-scored. A wing who takes PG/SG/SF stars reads high; an anchor big who only guards the 5 reads low. |
| `matchup_difficulty(season, w_usg, min_min, …)` | How tough those assignments are: the possession-weighted blend of the guarded players' offensive **impact** (OBPM) and **usage**, z-scored. High = you draw the engines; low = you get hidden on weak offence. |

A quick example:

```r
source("player-profiling/offensive_styles.R")
source("player-profiling/matchup-versatility.R")
source("player-profiling/matchup_difficulty.R")

off  <- offensive_styles("2026-27")
vers <- matchup_versatility("2026-27")
diff <- matchup_difficulty("2026-27")
```

A few notes on how the styles behave. Features are z-scored **within each season**, so a style means the same thing across eras despite the decade-long rise in threes. A player qualifies on `MPG >= min_mpg` **and** `GP >= floor`, where the floor is `min_gp` for a finished season but auto-drops to half the games played in an in-progress one, so the model fits on current data alone with no past seasons needed; players below the floor are assigned to their nearest style and flagged `LOW_SAMPLE`. Offence uses a Gaussian mixture, defence uses k-means — bbref carries no defensive tracking or matchup data, so the defensive styles are built from activity (blocks, steals, rebounds, fouls) plus the position estimate, and adding tracking later would sharpen the perimeter roles.

The two matchup metrics are meant to be read together: versatility is *how many* different positions you cover, difficulty is *how good* the players you cover are. On its own, versatility can reward a weak defender who gets switch-hunted across positions; pairing it with difficulty separates the trusted stoppers from the hidden liabilities. Both source `nba_def_matchups.R` (and versatility also pulls `offensive_styles.R` for position, difficulty pulls `bbref/bbref_player_stats.R` for OBPM + usage) the first time they run.
