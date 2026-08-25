# CricLens — AI Development Instructions

## Project

CricLens is an intelligent IPL analytics and decision-support
platform.

Tagline:

"From Every Ball to Every Insight."

The project is primarily a **Data Analytics using R** project.

The goal is to build a professional analytics product rather than
a basic academic dashboard.

---

# Core Architecture

CricLens has three major layers:

1. R Analytics Engine
2. Web Application
3. AI Enhancement Layer

Architecture:

User
 ↓
Web Application
 ↓
R Analytics Engine
 ↓
Analytical Results
 ↓
Web Application

AI operates as an additional natural-language interface:

User
 ↓
Web Application
 ↓
NVIDIA NIM
 ↓
Query Interpretation
 ↓
R Analytics Function
 ↓
Analytical Result
 ↓
NVIDIA NIM
 ↓
Natural Language Explanation
 ↓
User

---

# Critical Rule: R Is the Source of Truth

R performs ALL actual analytics.

This includes:

- Data cleaning
- Data transformation
- Feature engineering
- Statistical analysis
- Player analytics
- Team analytics
- Match analytics
- Venue analytics
- Machine learning
- Predictions
- Analytical visualizations
- Numerical calculations

The AI must NEVER independently calculate or invent analytical
results.

NVIDIA NIM is ONLY responsible for:

- Natural-language understanding
- Query interpretation
- Mapping user questions to available analytics
- Explaining R-generated results

If the AI does not have a validated result from the R analytics
layer, it must not fabricate one.

---

# Technology Stack

## Analytics

- R 4.6.1
- tidyverse
- dplyr
- tidyr
- ggplot2
- plotly

## Web Application

- R Shiny
- bslib
- DT

## AI

- NVIDIA NIM API

## Version Control

- Git
- GitHub

Use additional technologies only when there is a clear technical
reason.

---

# Project Structure

Preferred structure:

CricLens/
│
├── data/
│   ├── raw/
│   └── processed/
│
├── R/
│   ├── data_cleaning.R
│   ├── feature_engineering.R
│   ├── eda.R
│   ├── player_analysis.R
│   ├── team_analysis.R
│   ├── match_analysis.R
│   ├── venue_analysis.R
│   ├── predictive_models.R
│   └── visualizations.R
│
├── app/
│   ├── app.R
│   ├── ui.R
│   ├── server.R
│   └── modules/
│
├── ai/
│   └── nim_client.R
│
├── tests/
│
├── docs/
│
├── AGENTS.md
├── README.md
├── .gitignore
└── LICENSE

The structure may be modified if a strong technical reason exists,
but unnecessary restructuring should be avoided.

---

# Data Rules

Before writing analytics code:

1. Inspect the actual dataset.
2. Identify all files.
3. Inspect column names.
4. Inspect data types.
5. Check missing values.
6. Check duplicate records.
7. Understand relationships between datasets.
8. Determine season coverage.
9. Determine match coverage.

NEVER assume column names.

NEVER fabricate data.

NEVER create fake statistics just to populate the dashboard.

If data is unavailable for a requested metric, clearly communicate
that limitation.

---

# Analytics Modules

## Player Analytics

Potential metrics:

- Runs
- Batting average
- Strike rate
- Boundaries
- Wickets
- Economy
- Season performance
- Venue performance

## Team Analytics

Potential metrics:

- Wins
- Losses
- Win percentage
- Season performance
- Batting performance
- Bowling performance
- Head-to-head performance

## Match Analytics

Potential metrics:

- Toss impact
- Batting first vs chasing
- Winning margins
- Match trends
- Season trends

## Venue Analytics

Potential metrics:

- Average score
- Chasing success
- Venue batting trends
- Venue bowling trends

## Predictive Analytics

Machine learning must be implemented only after descriptive
analytics are stable.

Clearly separate:

- Feature engineering
- Training
- Validation
- Testing
- Prediction

Do not claim model accuracy without proper evaluation.

---

# Web Application

The application should feel like a professional sports analytics
product.

It should NOT look like a default Shiny application.

Preferred characteristics:

- Professional dark interface
- IPL-inspired accent colors
- Rounded cards
- Subtle shadows
- Clean typography
- Strong visual hierarchy
- Interactive charts
- KPI cards
- Responsive layout
- Minimal clutter
- Professional navigation

Potential sections:

1. Overview
2. Players
3. Teams
4. Matches
5. Venues
6. Predictions
7. Ask CricLens

---

# AI Integration

The AI workflow should follow:

User Question
 ↓
NVIDIA NIM
 ↓
Intent Interpretation
 ↓
Structured Analytics Request
 ↓
R Analytics Function
 ↓
Validated Result
 ↓
NVIDIA NIM
 ↓
Natural Language Explanation

Example:

User:

"Who has the highest strike rate at Chepauk?"

NVIDIA NIM identifies:

intent = player_venue_comparison
venue = Chepauk
metric = strike_rate

R performs the actual calculation.

The result is returned to the AI.

The AI explains the result.

---

# AI Safety Rules

The AI must NOT:

- Invent statistics
- Invent players
- Invent matches
- Invent predictions
- Perform hidden calculations
- Override R-generated results
- Present unsupported claims as facts

If an analytical function cannot answer a question, the AI should
state that the requested analysis is currently unavailable.

---

# Code Quality

Write clean, modular and maintainable R code.

Prefer:

- Reusable functions
- Small modules
- Clear variable names
- Meaningful comments
- Separation of concerns

Avoid:

- One massive app.R
- Repeated code
- Hardcoded analytical values
- Hardcoded API keys
- Unnecessary dependencies

---

# Secrets

Never hardcode API keys.

Use environment variables.

Example:

NVIDIA_API_KEY=...

Never commit:

.env
.Renviron

to GitHub.

---

# Development Workflow

Do NOT build the entire application in one step.

Follow these phases:

## Phase 1 — Repository & Data

- Inspect repository
- Inspect dataset
- Establish schema
- Create project structure

## Phase 2 — Data Processing

- Cleaning
- Transformation
- Feature engineering

## Phase 3 — Analytics

- Player analytics
- Team analytics
- Match analytics
- Venue analytics

## Phase 4 — Visualization

- ggplot2
- Plotly
- KPI calculations
- Analytical charts

## Phase 5 — Web Application

- Shiny
- Dashboard
- Navigation
- Filters
- Interactive analytics

## Phase 6 — Predictive Analytics

- Feature selection
- Model training
- Evaluation
- Prediction

## Phase 7 — NVIDIA NIM

- Query interpretation
- Analytics function routing
- Result explanation

## Phase 8 — Testing & Polish

- Functional testing
- Analytical validation
- UI polish
- Performance optimization
- Documentation

---

# AI Coding Agent Rules

Before modifying the project:

1. Inspect existing files.
2. Understand the current architecture.
3. Reuse working code where possible.
4. Do not rewrite unrelated components.
5. Explain significant architectural changes.
6. Run/test modified code.
7. Fix errors before moving forward.

Do not assume that generated code is correct.

Verify analytical calculations.

---

# Important

This is a university project, but it should be implemented with
professional engineering practices.

The final system should be:

- Technically feasible
- Academically defensible
- Reproducible
- Modular
- Testable
- Visually polished
- Honest about limitations

The primary objective is to demonstrate strong data analytics using R.

AI is an enhancement, not the core analytical engine.