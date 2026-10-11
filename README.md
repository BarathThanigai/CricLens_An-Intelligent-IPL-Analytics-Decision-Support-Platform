# CricLens — From Every Ball to Every Insight

CricLens is a locally run IPL analytics and decision-support application built with R and Shiny. Its calculations are based on the ball-by-ball CSV placed in `data/raw/`; the bundled `IPL.csv` was supplied in the project archive. The application computes the verified coverage from records at startup rather than assuming a season range.

## Implementation plan

1. Normalize input aliases and derive a consistent delivery schema with a reusable RDS cache.
2. Compute player, team, venue, phase, and match situation measures in reusable R functions.
3. Present those results across eight Shiny sections, with filters, interactive charts, searchable tables, and CSV export.
4. Add a chronological holdout chase model, role-profile clustering, and an allowlisted AI query router.
5. Include setup instructions and small synthetic calculation fixtures.

## Prerequisites and startup

- Windows and R 4.6.1. The scripts default to `C:\Program Files\R\R-4.6.1\bin\Rscript.exe`; if R is installed elsewhere, add `Rscript.exe` to `PATH`.
- Package installation requires access to CRAN at first setup. Packages are installed into `%LOCALAPPDATA%\R\win-library\4.6`, not the protected Program Files library.

In PowerShell, from this folder, run:

```powershell
Set-ExecutionPolicy -Scope Process Bypass
.\setup.ps1
.\start.ps1
```

The browser opens at [http://localhost:3000](http://localhost:3000). The app binds to `127.0.0.1` and keeps running in that PowerShell window until stopped with Ctrl+C. If port 3000 is busy, close the process using it or change `port=3000` in `run_app.R` and use the corresponding URL.

Equivalent R command after dependencies are installed:

```powershell
$env:R_LIBS_USER = "$env:LOCALAPPDATA\R\win-library\4.6"
Rscript run_app.R
```

## Data files and fields

Put a ball-by-ball `.csv` in `data/raw/`. The app reads the first CSV it finds and caches the normalized data at `data/processed/deliveries.rds`. Delete that cache after changing the source file. Read `data/README.md` for aliases and normalization details.

The essential fields are a match identifier, innings, batting team, batter/striker, and batter or total runs. For richer modules, include season or date, over and legal-ball flags, balls faced, bowler and bowler runs, extras, wicket type, player dismissed, venue, target, toss outcome, and match winner. The ingestion layer recognizes common aliases. Missing fields disable or limit dependent calculations; they are not filled with invented historical values.

## NVIDIA NIM (optional)

The application works without an API key. To enable result explanations, copy `.Renviron.example` to `.Renviron` in this folder and set `NVIDIA_API_KEY` to your key. `NVIDIA_NIM_MODEL` can select a compatible model. Restart the app after editing the file. The key is only read by R server code. The assistant routes to a fixed set of R analytics operations; it never evaluates model-generated code. If NIM is not configured or unavailable, R results remain visible with a status message.

## What is implemented

- **Overview:** dynamic seasons, matches, deliveries, players, teams, venues, season trends, team win comparison, run/wicket leaders, and venue summaries. Global season/team/venue filters update these outputs.
- **Player Intelligence:** runs, innings, balls faced, average, strike rate, boundary rate, bowling figures, season trend, phase performance, and a role-aware impact score. The score compares standardized batting and bowling performance among players in the active filters, applies opportunity shrinkage, and renormalizes weights when a role is missing. It requires at least 30 batting balls or 12 legal bowling balls and at least two eligible peers for that role.
- **Team Intelligence:** match-level wins, losses, no-results/undecided counts, and win percentage among decided matches; phase scoring by selected batting team.
- **Venue Intelligence:** first- and second-innings score means, variation, match sample sizes, and observed venue scoring comparisons. These are outcome proxies, not physical pitch measurements.
- **Head-to-Head:** player batting comparison with optional venue filtering and visible match/ball samples.
- **Predictive Analytics:** a chase-state logistic model, when dated targets and outcomes provide sufficient observations. It trains on first visit to this page and caches the evaluation in `models/chase_model.rds`; subsequent launches reuse it until the processed dataset changes. Training and test matches are chronologically separated; accuracy and Brier score are reported. A simulator shows an estimated chase win probability. A reproducibly seeded k-means table clusters players with sufficient batting sample by strike rate, average, and boundary share.
- **Data Explorer:** canonical schema, types, missing counts, full-dataset text/category/numeric/date filters, filtered record preview (up to 50,000 rows), searchable/paged table, and a CSV download of all matching rows.
- **Ask CricLens:** predefined query intents map to R-computed player, phase, venue, or team summaries. NIM only summarizes supplied R output.

Impact-score helper `impact_score()` uses batting-average z-score plus strike-rate residualized against batting average, and bowling-economy z-score plus wicket-rate residualized against economy. It combines those components with adjustable role weights and square-root opportunity shrinkage; missing role data is excluded and active weights are renormalized. Boundary share is kept separate from the score because it is coupled to scoring rate. This is a custom comparative measure, not an official statistic.

## Structure

`app.R` contains Shiny navigation/reactivity; `R/data_ingestion.R`, `R/analytics.R`, `R/predictive_models.R`, and `R/ai_assistant.R` separate the processing and analysis layers. `readr` ingests CSV input when installed, `tidyr` handles supported missing-value defaults, dplyr handles interactive row filters, and ggplot2 charts are rendered interactively through plotly. `global.R` loads data once per app process. `setup.ps1` installs dependencies, `start.ps1` launches the local app, and `tests/test_analytics.R` checks synthetic examples.

## Tests and known limitations

Run `Rscript tests/test_analytics.R` after setting the R library path. The fixture rows are labelled synthetic and are not IPL findings. The full Shiny interface requires its listed packages and an actual compatible CSV. Run `Rscript -e "parse(file='app.R'); parse(file='R/analytics.R')"` for a syntax check.

This first implementation does not claim pre-match team prediction, player form forecasting, fielding analytics, significance tests, or uncertainty intervals where the data/model do not support them. The predictive model is an in-match empirical estimator only; its test metrics describe the provided historical dataset and do not promise future accuracy. Some ensemble outputs require target, winner, date, or dismissal information. Multi-source match metadata may be supplied alongside delivery rows but this version consumes a combined ball-by-ball table.

## Troubleshooting

- **Missing packages:** run `Set-ExecutionPolicy -Scope Process Bypass; .\setup.ps1`. If CRAN cannot be reached, fix the network/proxy or install the listed packages into the user library.
- **R not found:** use the installation path above or add `Rscript.exe` to `PATH`.
- **Port conflict:** stop the other local service or change the port in `run_app.R`.
- **Dataset empty or rejected:** ensure it is CSV, has a recognized match identifier and batting/run fields, and is in `data/raw/`. Read `data/README.md` for field aliases. Remove `data/processed/deliveries.rds` to rebuild after replacing input.
- **NIM unavailable:** analytics continue to work; configure the optional key or use the example queries.
