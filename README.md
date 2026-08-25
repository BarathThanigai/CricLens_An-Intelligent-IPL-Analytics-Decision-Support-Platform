# CricLens — An Intelligent IPL Analytics & Decision-Support Platform

> **"From Every Ball to Every Insight."**

CricLens is a professional data analytics platform built using **R (v4.6.1)** as the single source of truth for all statistical computations, powered by an interactive **R Shiny / bslib** web application with dark-mode sports design aesthetics, and enhanced by an **NVIDIA NIM API** natural-language AI interface.

---

## 🌟 Key Features

1. **R Analytics Engine (Source of Truth)**
   - Ball-by-ball analysis of **295,732 deliveries** across **1,243 IPL matches** (2008–2026).
   - Player batting & bowling profiles (Averages, Strike Rates, Economy, 50s/100s, Dot Ball %).
   - Match Phase Analysis (Powerplay: Overs 1–6, Middle Overs: Overs 7–15, Death Overs: Overs 16–20).
   - Head-to-Head franchise comparison matrix & toss decision impact metrics.
   - Venue pitch profiles & stadium scoring trends.

2. **Predictive Machine Learning Engine**
   - **Live Match Win Probability Model** (Logistic Regression trained on 136,967 delivery snapshots, achieving **78.06% accuracy**).
   - Interactive live match situation simulator predicting win probabilities for chasing vs defending teams.

3. **NVIDIA NIM AI Integration Layer**
   - Natural Language Query Processing: Ask questions like *"Who has the most runs in IPL?"* or *"Show V Kohli stats"*.
   - Strict AI Safety: NVIDIA NIM maps query intent to validated R functions; the AI never invents or synthesizes numbers independently.

4. **Modern IPL Dark Aesthetics Dashboard**
   - Custom `bslib` layout with deep navy background (`#0B0F19`), IPL accent cyan (`#00D2FF`), electric orange (`#FF5E00`), and gold (`#FFD700`).
   - Interactive `Plotly` analytical charts & responsive `DT` data tables.

---

## 🏗️ Core Architecture

```
User Question / Interaction
          ↓
  Shiny Web Application (bslib / Plotly / DT)
          ↓
  NVIDIA NIM API (Intent Parser)
          ↓
  R Analytics Engine (Single Source of Truth)
          ↓
  Validated Result & Visualizations
          ↓
  Shiny Web Application / User
```

---

## 📁 Repository Structure

```
CricLens/
├── data/
│   ├── raw/
│   │   ├── IPL.csv                   # Raw ball-by-ball dataset (295,732 rows x 64 cols)
│   │   └── IPL dataset.zip
│   └── processed/
│       ├── ipl_cleaned.rds           # Standardized cleaned binary dataset
│       ├── ipl_featured.rds          # Featured dataset with phase tags & cumulative runs
│       ├── player_batting_summary.rds# Pre-calculated batting metrics cache
│       ├── player_bowling_summary.rds# Pre-calculated bowling metrics cache
│       ├── team_summary.rds          # Pre-calculated team win statistics cache
│       ├── venue_summary.rds         # Pre-calculated venue statistics cache
│       └── win_prob_model.rds        # Trained logistic regression model object
│
├── R/
│   ├── data_cleaning.R              # Team standardization & NA handling
│   ├── feature_engineering.R        # Match phase tags, cumulative tracking & caching
│   ├── player_analysis.R            # Player batting/bowling profiles & phase splits
│   ├── team_analysis.R              # Team win %, H2H matrix & toss impact
│   ├── match_analysis.R             # Match worm charts, scorecards & partnerships
│   ├── venue_analysis.R             # Stadium scoring trends & toss bias
│   ├── predictive_models.R          # Live win probability ML model
│   └── visualizations.R             # Dark-themed Plotly chart engine
│
├── app/
│   ├── app.R                        # Main Shiny application entry point
│   ├── ui.R                         # bslib navbar dashboard layout
│   ├── server.R                     # Server reactive wiring
│   ├── www/
│   │   └── custom.css               # Custom IPL dark theme CSS styling
│   └── modules/
│       ├── mod_overview.R           # High-level KPIs & IPL history
│       ├── mod_players.R            # Player search & stats cards
│       ├── mod_teams.R              # Franchise performance & H2H matrix
│       ├── mod_matches.R            # Ball-by-ball match explorer
│       ├── mod_venues.R             # Stadium analytics & pitch explorer
│       ├── mod_predictions.R        # Live Win Predictor tool
│       └── mod_ai_chat.R            # Ask CricLens AI interface
│
├── ai/
│   ├── nim_client.R                 # NVIDIA NIM REST client
│   ├── prompt_templates.R           # System prompts for intent parsing
│   └── query_router.R               # Intent router mapping to R functions
│
├── tests/
│   └── test_data.R                  # Automated verification test suite
│
├── AGENTS.md
└── README.md
```

---

## 🚀 Getting Started

### Prerequisites
- R (v4.6.1 or higher)
- R Packages: `shiny`, `bslib`, `dplyr`, `readr`, `tidyr`, `stringr`, `plotly`, `DT`, `httr`, `jsonlite`

To install missing R packages, run in R:
```R
install.packages(c("shiny", "bslib", "dplyr", "readr", "tidyr", "stringr", "plotly", "DT", "httr", "jsonlite"))
```

### Running the Shiny Application
Launch the app locally from R or command line:

```bash
Rscript -e "shiny::runApp('app', display.mode = 'normal')"
```

Or inside R / RStudio:
```R
shiny::runApp("app")
```

### Running Automated Test Suite
To verify analytical calculations and dataset integrity:
```bash
Rscript tests/test_data.R
```

---

## 🛡️ AI & Safety Directives
- **R is the Single Source of Truth**: All statistical metrics are strictly calculated by R scripts.
- **NVIDIA NIM Integration**: NVIDIA NIM API is used solely for natural-language query interpretation and explaining R-calculated results.
- **Secrets Management**: Set `NVIDIA_API_KEY` in environment variables or `.Renviron`. Never commit API keys to version control.