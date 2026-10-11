# tests/test_data.R
# CricLens — Automated Analytical & Machine Learning Verification Suite

cat("=====================================================\n")
cat("  CricLens Test Suite: Validating R Engine & Phase 1 \n")
cat("=====================================================\n\n")

suppressMessages({
  source("R/player_analysis.R")
  source("R/team_analysis.R")
  source("R/match_analysis.R")
  source("R/venue_analysis.R")
  source("R/predictive_models.R")
  source("ai/nim_client.R")
  source("ai/prompt_templates.R")
  source("ai/query_router.R")
})

# Test 1: Load Processed Dataset
cat("Test 1: Loading featured dataset...\n")

df <- readRDS(
  "data/processed/ipl_featured.rds"
)

stopifnot(
  nrow(df) > 290000
)

cat(
  "  [PASS] Rows:",
  nrow(df),
  "columns:",
  ncol(df),
  "\n\n"
)


# Test 2: Canonical Venue Normalization Verification
cat("Test 2: Validating Canonical Venue Normalization & Metadata...\n")

stopifnot(
  file.exists("data/processed/metadata.rds")
)

meta <- readRDS(
  "data/processed/metadata.rds"
)

stopifnot(
  meta$num_raw_venues == 60
)

stopifnot(
  meta$num_canonical_venues == 42
)

stopifnot(
  length(meta$unmapped_venues) == 0
)

cat(
  "  [PASS] Successfully mapped",
  meta$num_raw_venues,
  "raw venues to",
  meta$num_canonical_venues,
  "canonical venues with 0 unmapped!\n\n"
)


# Test 3: Top Batters Analytical Sanity Check
cat("Test 3: Validating Top Batters Calculation...\n")

top_b <- get_top_batters(
  df,
  top_n = 5
)

stopifnot(
  top_b$batter[1] == "V Kohli"
)

stopifnot(
  top_b$total_runs[1] >= 9000
)

cat(
  "  [PASS] Leading run scorer:",
  top_b$batter[1],
  "with",
  top_b$total_runs[1],
  "runs.\n\n"
)


# Test 3A: Player Dismissal & Average Validation
cat("Test 3A: Validating Player Dismissals & Batting Average...\n")

kohli_profile <- get_player_batting_profile(
  df,
  "V Kohli"
)

# Independently calculate expected values directly
# from the actual dataset.
standard_innings <- df %>%
  filter(
    innings %in% c(1, 2)
  )

expected_kohli_runs <- sum(
  standard_innings$runs_batter[
    standard_innings$batter == "V Kohli"
  ],
  na.rm = TRUE
)

expected_kohli_outs <- sum(
  standard_innings$player_out == "V Kohli",
  na.rm = TRUE
)

expected_kohli_average <- round(
  expected_kohli_runs / expected_kohli_outs,
  2
)

stopifnot(
  kohli_profile$total_runs == expected_kohli_runs
)

stopifnot(
  kohli_profile$outs == expected_kohli_outs
)

stopifnot(
  abs(
    kohli_profile$average -
      expected_kohli_average
  ) < 0.01
)

cat(
  "  [PASS] Player dismissal calculation matches independent dataset calculation.\n"
)

cat(
  "         - Runs:",
  expected_kohli_runs,
  "| Outs:",
  expected_kohli_outs,
  "| Average:",
  expected_kohli_average,
  "\n\n"
)


# Test 3B: Super Over Phase Classification
cat("Test 3B: Validating Super Over Phase Classification...\n")

super_over_rows <- df %>%
  filter(
    innings > 2
  )

stopifnot(
  nrow(super_over_rows) > 0
)

stopifnot(
  all(
    super_over_rows$match_phase == "Super Over"
  )
)

stopifnot(
  !any(
    super_over_rows$match_phase == "Powerplay"
  )
)

cat(
  "  [PASS] Super Over deliveries correctly classified as Super Over:",
  nrow(super_over_rows),
  "deliveries.\n\n"
)


# Test 4: Top Bowlers Analytical Sanity Check
cat("Test 4: Validating Top Bowlers Calculation...\n")

top_bw <- get_top_bowlers(
  df,
  top_n = 5
)

stopifnot(
  top_bw$bowler[1] == "YS Chahal"
)

stopifnot(
  top_bw$wickets[1] >= 200
)

cat(
  "  [PASS] Leading wicket taker:",
  top_bw$bowler[1],
  "with",
  top_bw$wickets[1],
  "wickets.\n\n"
)


# Test 5: Team Performance Summary Check
cat("Test 5: Validating Team Performance Calculation...\n")

t_summary <- get_team_performance_summary(
  df
)

stopifnot(
  t_summary$team[1] == "Mumbai Indians"
)

stopifnot(
  t_summary$wins[1] == 155
)

cat(
  "  [PASS] Leading franchise:",
  t_summary$team[1],
  "with",
  t_summary$wins[1],
  "wins.\n\n"
)


# Test 6: Chronological ML Train/Test Split & Metrics
cat("Test 6: Validating Chronological Win Probability ML Model & Test Metrics...\n")

model_obj <- readRDS(
  "data/processed/win_prob_model.rds"
)

stopifnot(
  is.list(model_obj) &&
    "test_accuracy" %in% names(model_obj)
)

stopifnot(
  model_obj$train_count > 100000
)

stopifnot(
  model_obj$test_count > 15000
)

stopifnot(
  model_obj$test_accuracy > 70
)

stopifnot(
  model_obj$roc_auc > 0.70
)

stopifnot(
  model_obj$brier_score < 0.25
)

cat(
  "  [PASS] Chronological ML Model Verified on Unseen Test Set (2024-2026):\n"
)

cat(
  "         - Train Count: ",
  model_obj$train_count,
  " | Test Count: ",
  model_obj$test_count,
  "\n",
  sep = ""
)

cat(
  "         - Test Accuracy: ",
  model_obj$test_accuracy,
  "% | ROC-AUC: ",
  model_obj$roc_auc,
  " | Brier Score: ",
  model_obj$brier_score,
  " | Log Loss: ",
  model_obj$log_loss,
  "\n\n",
  sep = ""
)


# Test 7: Query Router Regressions (Isolated from NVIDIA NIM)
cat("Test 7: Validating query routing without NVIDIA NIM...\n")

call_nvidia_nim <- function(
  prompt,
  system_prompt = "You are CricLens AI assistant.",
  model = "unused"
) {
  NULL
}

query_res <- process_criclens_query(
  "Who has the most runs in IPL?",
  df
)

stopifnot(
  grepl(
    "V Kohli",
    query_res
  )
)

cat(
  "  [PASS] Supported query answered by deterministic R fallback.\n\n"
)


# Test 7A: Full Player Name Resolves to Dataset Name
cat("Test 7A: Validating Virat Kohli player-name resolution...\n")

virat_query <- "Show me Virat Kohli career profile"
virat_intent <- fallback_query_intent(
  virat_query,
  df
)
virat_validated <- validate_intent(
  virat_intent,
  df
)
virat_result <- execute_r_analytics(
  virat_validated,
  df
)
virat_response <- process_criclens_query(
  virat_query,
  df
)

stopifnot(
  identical(
    virat_validated$parameters$player_name,
    "V Kohli"
  ),
  identical(
    virat_result$result$player,
    "V Kohli"
  ),
  virat_result$result$total_runs ==
    get_player_batting_profile(df, "V Kohli")$total_runs,
  grepl(
    "Player Profile: **V Kohli**",
    virat_response,
    fixed = TRUE
  ),
  !grepl(
    "T Kohli",
    virat_response,
    fixed = TRUE
  )
)

cat(
  "  [PASS] Virat Kohli resolves to the V Kohli dataset profile.\n\n"
)


# Test 7B: Specific Team Summary
cat("Test 7B: Validating team-specific performance summary...\n")

csk_query <- paste(
  "Show me the team performance summary for",
  "Chennai Super Kings"
)
csk_intent <- fallback_query_intent(
  csk_query,
  df
)
csk_validated <- validate_intent(
  csk_intent,
  df
)
csk_result <- execute_r_analytics(
  csk_validated,
  df
)
csk_response <- process_criclens_query(
  csk_query,
  df
)

stopifnot(
  identical(
    csk_validated$parameters$team_name,
    "Chennai Super Kings"
  ),
  nrow(csk_result$result) == 1,
  identical(
    csk_result$result$team,
    "Chennai Super Kings"
  ),
  grepl(
    "Chennai Super Kings",
    csk_response,
    fixed = TRUE
  ),
  !grepl(
    "Mumbai Indians",
    csk_response,
    fixed = TRUE
  )
)

cat(
  "  [PASS] Chennai Super Kings summary contains only the requested team.\n\n"
)


# Test 7C: Explicit All-Team Summary
cat("Test 7C: Validating league-wide team summary...\n")

all_teams_query <- "Show me the summary for all teams"
all_teams_intent <- fallback_query_intent(
  all_teams_query,
  df
)
all_teams_validated <- validate_intent(
  all_teams_intent,
  df
)
all_teams_result <- execute_r_analytics(
  all_teams_validated,
  df
)
expected_all_teams <- get_team_performance_summary(df)

stopifnot(
  is.null(all_teams_validated$parameters$team_name),
  nrow(all_teams_result$result) ==
    nrow(expected_all_teams),
  setequal(
    all_teams_result$result$team,
    expected_all_teams$team
  )
)

cat(
  "  [PASS] Explicit all-team summary retains every team.\n\n"
)


# Test 7D: Query Text Overrides Missing or Incorrect Parsed Parameters
cat("Test 7D: Validating deterministic query-parameter extraction...\n")

call_nvidia_nim <- function(
  prompt,
  system_prompt = "You are CricLens AI assistant.",
  model = "unused"
) {
  if (!identical(system_prompt, INTENT_PARSER_SYSTEM_PROMPT)) {
    return(NULL)
  }

  if (grepl("Virat Kohli", prompt, fixed = TRUE)) {
    return(
      '{"intent":"player_profile","parameters":{"player_name":"T Kohli"}}'
    )
  }

  if (grepl("Chennai Super Kings", prompt, fixed = TRUE)) {
    return(
      '{"intent":"team_summary","parameters":{}}'
    )
  }

  if (grepl("all teams", prompt, ignore.case = TRUE)) {
    return(
      paste0(
        '{"intent":"team_summary","parameters":',
        '{"team_name":"Chennai Super Kings"}}'
      )
    )
  }

  NULL
}

parsed_virat <- parse_query_intent(
  virat_query,
  df
)
parsed_csk <- parse_query_intent(
  csk_query,
  df
)
parsed_all_teams <- parse_query_intent(
  all_teams_query,
  df
)
nim_virat_response <- process_criclens_query(
  virat_query,
  df
)
nim_csk_response <- process_criclens_query(
  csk_query,
  df
)
nim_all_teams_response <- process_criclens_query(
  all_teams_query,
  df
)

stopifnot(
  identical(
    parsed_virat$parameters$player_name,
    "V Kohli"
  ),
  identical(
    parsed_csk$parameters$team_name,
    "Chennai Super Kings"
  ),
  is.null(
    parsed_all_teams$parameters$team_name
  ),
  grepl(
    "Player Profile: **V Kohli**",
    nim_virat_response,
    fixed = TRUE
  ),
  !grepl(
    "T Kohli",
    nim_virat_response,
    fixed = TRUE
  ),
  grepl(
    "Chennai Super Kings",
    nim_csk_response,
    fixed = TRUE
  ),
  !grepl(
    "Mumbai Indians",
    nim_csk_response,
    fixed = TRUE
  ),
  grepl(
    "Mumbai Indians",
    nim_all_teams_response,
    fixed = TRUE
  )
)

cat(
  "  [PASS] Query text corrects a wrong player name, fills a missing team,",
  "and preserves all-team scope.\n\n"
)


cat("=====================================================\n")
cat("  ALL TEST SUITES PASSED WITH 100% SUCCESS!         \n")
cat("=====================================================\n")