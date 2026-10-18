# tests/test_data.R
# CricLens — Automated Analytical & Machine Learning Verification Suite

cat("=====================================================\n")
cat("  CricLens Test Suite: Validating R Engine & Phase 1 \n")
cat("=====================================================\n\n")

Sys.setenv(NVIDIA_API_KEY = "")

suppressMessages({
  source("R/player_analysis.R")
  source("R/team_analysis.R")
  source("R/match_analysis.R")
  source("R/venue_analysis.R")
  source("R/predictive_models.R")
  source("R/data_filters.R")
  source("R/data_explorer.R")
  source("ai/nim_client.R")
  source("ai/prompt_templates.R")
  source("ai/query_router.R")
})

nim_client_call <- call_nvidia_nim

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
loaded_model <- load_win_prob_model(
  "data/processed/win_prob_model.rds"
)

stopifnot(
  is.list(model_obj) &&
    "test_accuracy" %in% names(model_obj),
  is.list(loaded_model),
  inherits(loaded_model$model, "glm"),
  model_obj$model$family$family == "binomial",
  identical(all.vars(stats::terms(model_obj$model))[1], "is_chase_winner"),
  setequal(
    all.vars(stats::delete.response(stats::terms(model_obj$model))),
    c("runs_required", "balls_remaining", "wickets_remaining", "req_rr", "current_rr")
  )
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

prediction <- predict_live_win_prob(
  target = 180,
  current_runs = 110,
  overs_bowled = 12,
  wickets_down = 3,
  model = model_obj
)

stopifnot(
  prediction$chase_win_prob >= 0,
  prediction$chase_win_prob <= 1,
  prediction$defend_win_prob >= 0,
  prediction$defend_win_prob <= 1,
  abs(prediction$chase_win_prob + prediction$defend_win_prob - 1) < 1e-12,
  round(prediction$chase_win_prob * 100, 1) == 64.2,
  round(prediction$defend_win_prob * 100, 1) == 35.8,
  prediction$balls_remaining == 48,
  prediction$runs_required == 70
)

raw_model_prediction <- predict_live_win_prob(
  target = 180,
  current_runs = 110,
  overs_bowled = 12,
  wickets_down = 3,
  model = model_obj$model
)

stopifnot(
  raw_model_prediction$chase_win_prob == prediction$chase_win_prob,
  raw_model_prediction$defend_win_prob == prediction$defend_win_prob
)

invalid_prediction_inputs <- list(
  list(target = NA_real_, current_runs = 110, overs_bowled = 12, wickets_down = 3),
  list(target = 0, current_runs = 0, overs_bowled = 0, wickets_down = 0),
  list(target = 180, current_runs = -1, overs_bowled = 12, wickets_down = 3),
  list(target = 180, current_runs = 180, overs_bowled = 12, wickets_down = 3),
  list(target = 180, current_runs = 110, overs_bowled = 12.6, wickets_down = 3),
  list(target = 180, current_runs = 110, overs_bowled = 20, wickets_down = 3),
  list(target = 180, current_runs = 110, overs_bowled = 12, wickets_down = 10),
  list(target = 180, current_runs = 110, overs_bowled = 12, wickets_down = 1.5)
)
stopifnot(all(vapply(
  invalid_prediction_inputs,
  function(args) {
    inherits(tryCatch(
      do.call(predict_live_win_prob, c(args, list(model = model_obj))),
      error = identity
    ), "error")
  },
  logical(1)
)))

missing_model <- load_win_prob_model(
  file.path(tempdir(), "missing-win-prob-model.rds")
)
stopifnot(
  inherits(missing_model, "criclens_model_load_error"),
  grepl("not found", missing_model$message, fixed = TRUE)
)

invalid_model_error <- tryCatch(
  {
    predict_live_win_prob(
      target = 180,
      current_runs = 110,
      overs_bowled = 12,
      wickets_down = 3,
      model = list(model = list())
    )
    NULL
  },
  error = identity
)

stopifnot(
  inherits(invalid_model_error, "error"),
  grepl(
    "expected a fitted binomial glm",
    conditionMessage(invalid_model_error),
    fixed = TRUE
  )
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
cat(
  "  [PASS] Wrapper/raw model loading, schema and target validation, valid 0-1 probabilities, legal overs, input ranges, and missing-model diagnostics verified.\n\n"
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

verified_explanation_result <- list(
  intent = "player_profile",
  result = data.frame(player = "V Kohli", total_runs = 9336)
)
stopifnot(
  explanation_numeric_values_are_supported(
    "V Kohli's R-calculated career total is 9,336 runs.",
    "Show V Kohli stats",
    verified_explanation_result
  ),
  !explanation_numeric_values_are_supported(
    "V Kohli scored 9,999 runs.",
    "Show V Kohli stats",
    verified_explanation_result
  )
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

  query_text <- sub(
    "(?s)^.*USER QUERY:\\s*",
    "",
    prompt,
    perl = TRUE
  )

  if (identical(query_text, virat_query)) {
    return(
      '{"intent":"player_profile","parameters":{"player_name":"T Kohli"}}'
    )
  }

  if (identical(query_text, csk_query)) {
    return(
      '{"intent":"team_summary","parameters":{}}'
    )
  }

  if (grepl("all teams", query_text, ignore.case = TRUE)) {
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

cat("Test 7E: Validating robust conversational IPL analytics...\n")

query_warnings <- character()
run_without_warnings <- function(query) {
  withCallingHandlers(
    process_criclens_query(query, df),
    warning = function(warning) {
      query_warnings <<- c(
        query_warnings,
        conditionMessage(warning)
      )
      invokeRestart("muffleWarning")
    }
  )
}

season_batters <- run_without_warnings(
  "Who scored the most runs in IPL 2024?"
)
season_bowlers <- run_without_warnings(
  "Who took the most wickets in IPL 2024?"
)
bumrah_query <- run_without_warnings(
  "How many wickets did Bumrah take in 2024?"
)
bumrah_2024_expected <- get_top_bowlers(
  df,
  top_n = dplyr::n_distinct(df$bowler),
  season_filter = "2024"
) %>%
  filter(.data$bowler == "JJ Bumrah")
player_comparison <- run_without_warnings(
  "Compare the batting performances of Virat Kohli and Rohit Sharma"
)
team_comparison <- run_without_warnings(
  "Compare CSK and MI"
)
venue_query <- run_without_warnings(
  "What are the statistics at Eden Gardens?"
)
team_trend <- run_without_warnings(
  "How does Chennai Super Kings perform across seasons?"
)
win_percentage <- run_without_warnings(
  "Which team has the best win percentage?"
)

stopifnot(
  grepl("Top Run Scorers", season_batters, fixed = TRUE),
  grepl("Top Wicket Takers", season_bowlers, fixed = TRUE),
  grepl("Bowling Profile: **JJ Bumrah**", bumrah_query, fixed = TRUE),
  grepl(
    paste0("**JJ Bumrah** | ", bumrah_2024_expected$wickets[1], " |"),
    bumrah_query,
    fixed = TRUE
  ),
  grepl("Player Batting Comparison", player_comparison, fixed = TRUE),
  grepl("V Kohli", player_comparison, fixed = TRUE),
  grepl("RG Sharma", player_comparison, fixed = TRUE),
  grepl("Head-to-Head", team_comparison, fixed = TRUE),
  grepl("Eden Gardens", venue_query, fixed = TRUE),
  grepl("Season-by-Season Performance", team_trend, fixed = TRUE),
  grepl("Highest Team Win Percentage", win_percentage, fixed = TRUE),
  length(query_warnings) == 0
)

empty_query <- process_criclens_query("   ", df)
unsupported_query <- process_criclens_query(
  "What is the weather in Mumbai tomorrow?",
  df
)
prediction_explanation_query <- process_criclens_query(
  "Explain what the IPL win probability prediction means in CricLens.",
  df
)
stopifnot(
  identical(empty_query, "Please enter a question."),
  grepl("can't answer", unsupported_query, ignore.case = TRUE),
  grepl("team chasing the target", prediction_explanation_query, fixed = TRUE),
  grepl("complement", prediction_explanation_query, fixed = TRUE),
  grepl("not a guarantee", prediction_explanation_query, fixed = TRUE),
  grepl("couldn't process", format_r_result(NULL), ignore.case = TRUE),
  grepl("No bowling data", format_r_result(list(
    intent = "top_bowlers",
    result = NULL
  ))),
  grepl(
    "current CricLens dataset",
    format_r_result(list(intent = "unsupported", parameters = "malformed")),
    fixed = TRUE
  )
)

candidate_fallback <- tryCatch(
  fallback_query_intent("Kohli", df),
  error = identity
)
stopifnot(!inherits(candidate_fallback, "error"))

cat("Test 7H: Validating player comparisons and team history...\n")

bhuvi_vs_kohli <- fallback_query_intent("Bhuvi vs Kohli", df)
bhuvi_validated <- validate_intent(bhuvi_vs_kohli, df)
bhuvi_comparison <- execute_r_analytics(bhuvi_validated, df)
stopifnot(
  identical(bhuvi_vs_kohli$intent, "player_comparison"),
  identical(resolve_player_name("Bhuvi", get_dataset_players(df)), "B Kumar"),
  identical(resolve_player_name("Kohli", get_dataset_players(df)), "V Kohli"),
  setequal(
    c(
      bhuvi_validated$parameters$player1,
      bhuvi_validated$parameters$player2
    ),
    c("B Kumar", "V Kohli")
  ),
  setequal(
    bhuvi_comparison$result$player,
    c("B Kumar", "V Kohli")
  ),
  grepl(
    "Player Batting Comparison",
    format_r_result(bhuvi_comparison),
    fixed = TRUE
  )
)

named_comparison <- fallback_query_intent(
  "Compare Virat Kohli and Bhuvneshwar Kumar",
  df
)
season_comparison <- fallback_query_intent(
  "Kohli versus Bhuvi in IPL 2024",
  df
)
season_comparison_result <- execute_r_analytics(
  validate_intent(season_comparison, df),
  df
)
expected_season_profiles <- dplyr::bind_rows(
  get_player_batting_profile(
    dplyr::filter(df, .data$season_year == 2024),
    "V Kohli"
  ),
  get_player_batting_profile(
    dplyr::filter(df, .data$season_year == 2024),
    "B Kumar"
  )
)
stopifnot(
  named_comparison$intent == "player_comparison",
  setequal(
    c(
      named_comparison$parameters$player1,
      named_comparison$parameters$player2
    ),
    c("V Kohli", "B Kumar")
  ),
  season_comparison$intent == "player_comparison",
  season_comparison$parameters$season == "2024",
  isTRUE(all.equal(
    season_comparison_result$result$total_runs,
    expected_season_profiles$total_runs
  ))
)

kohli_history_intent <- fallback_query_intent(
  "Which team has Kohli played for?",
  df
)
kohli_history_validated <- validate_intent(kohli_history_intent, df)
kohli_history_result <- execute_r_analytics(kohli_history_validated, df)
expected_kohli_teams <- unique(c(
  as.character(df$batting_team[
    df$batter == "V Kohli" |
      df$non_striker == "V Kohli" |
      df$player_out == "V Kohli"
  ]),
  as.character(df$bowling_team[df$bowler == "V Kohli"])
))
expected_kohli_teams <- expected_kohli_teams[
  !is.na(expected_kohli_teams) & nzchar(expected_kohli_teams)
]
stopifnot(
  kohli_history_intent$intent == "player_team_history",
  kohli_history_validated$intent == "player_team_history",
  setequal(
    unique(kohli_history_result$result$team),
    expected_kohli_teams
  ),
  grepl(
    "Teams Represented by **V Kohli**",
    format_r_result(kohli_history_result),
    fixed = TRUE
  ),
  !grepl("Team Performance", format_r_result(kohli_history_result), fixed = TRUE)
)
rendered_kohli_history <- render_markdown_to_html(
  format_r_result(kohli_history_result)
)
stopifnot(
  grepl("<table", rendered_kohli_history, fixed = TRUE),
  grepl("<h5>🏏 Teams Represented by", rendered_kohli_history, fixed = TRUE),
  grepl("<th>Season</th>", rendered_kohli_history, fixed = TRUE),
  grepl("<td>Royal Challengers Bengaluru</td>", rendered_kohli_history, fixed = TRUE)
)

kohli_history_phrasing <- fallback_query_intent(
  "Which teams did Virat Kohli represent?",
  df
)
kohli_season_history <- fallback_query_intent(
  "Show Kohli's teams by season",
  df
)
stopifnot(
  kohli_history_phrasing$intent == "player_team_history",
  kohli_season_history$intent == "player_team_history",
  nrow(kohli_history_result$result) > 0
)

unknown_comparison <- fallback_query_intent(
  "Unknown Cricketer vs Kohli",
  df
)
unknown_player <- fallback_query_intent(
  "Which team has Unknown Cricketer played for?",
  df
)
stopifnot(
  unknown_comparison$intent == "unsupported",
  grepl("Which player", unknown_comparison$parameters$reason, fixed = TRUE),
  unknown_player$intent == "unsupported",
  grepl("couldn't confidently resolve", unknown_player$parameters$reason, fixed = TRUE),
  is.null(resolve_player_name("Unknown Cricketer", get_dataset_players(df)))
)

original_nim_parser <- call_nvidia_nim
call_nvidia_nim <- function(...) {
  '{"intent":"team_summary","parameters":{}}'
}
misrouted_nim_history <- parse_query_intent(
  "Which team has Kohli played for?",
  df
)
misrouted_nim_comparison <- parse_query_intent(
  "Bhuvi vs Kohli",
  df
)
nim_guarded_comparison_response <- process_criclens_query(
  "Bhuvi vs Kohli",
  df
)
nim_guarded_unknown_history <- process_criclens_query(
  "Which team has Unknown Cricketer played for?",
  df
)
call_nvidia_nim <- original_nim_parser
stopifnot(
  misrouted_nim_history$intent == "player_team_history",
  misrouted_nim_comparison$intent == "player_comparison",
  setequal(
    c(
      misrouted_nim_comparison$parameters$player1,
      misrouted_nim_comparison$parameters$player2
    ),
    c("B Kumar", "V Kohli")
  ),
  grepl("Player Batting Comparison", nim_guarded_comparison_response, fixed = TRUE),
  grepl(
    "couldn't confidently resolve",
    nim_guarded_unknown_history,
    fixed = TRUE
  ),
  !grepl("Team Performance", nim_guarded_unknown_history, fixed = TRUE)
)

next_query <- resolve_criclens_followup(
  "Show me the next five",
  "Who scored the most runs in IPL 2024?",
  df
)
next_intent <- fallback_query_intent(next_query, df)
first_five <- get_top_batters(df, top_n = 5, season_filter = "2024")
next_five <- execute_r_analytics(validate_intent(next_intent, df), df)
expected_next_five <- get_top_batters(df, top_n = 10, season_filter = "2024")
stopifnot(
  next_intent$parameters$offset == 5,
  identical(
    next_five$result$batter,
    expected_next_five$batter[6:10]
  ),
  nrow(first_five) == 5
)

season_followup <- resolve_criclens_followup(
  "What about 2023?",
  "Who scored the most runs in IPL 2024?",
  df
)
season_followup_intent <- fallback_query_intent(season_followup, df)
stopifnot(
  season_followup_intent$parameters$season == "2023"
)

comparison_followup <- resolve_criclens_followup(
  "Compare that with Rohit Sharma",
  "Show me Virat Kohli career statistics",
  df
)
comparison_followup_intent <- fallback_query_intent(comparison_followup, df)
stopifnot(
  comparison_followup_intent$intent == "player_comparison",
  setequal(
    c(
      comparison_followup_intent$parameters$player1,
      comparison_followup_intent$parameters$player2
    ),
    c("V Kohli", "RG Sharma")
  )
)

isolated_session_followup <- resolve_criclens_followup(
  "What about MI?",
  "Show V Kohli career profile",
  df
)
stopifnot(
  identical(isolated_session_followup, "What about MI?")
)

source("app/modules/mod_ai_chat.R")
source("app/modules/mod_predictions.R")
first_session_chat <- NULL
nim_status_message <- NULL
shiny::testServer(
  mod_ai_chat_server,
  args = list(df_feat = df),
  {
    session$setInputs(
      user_query = "Who scored the most runs in IPL 2024?",
      submit_btn = 1
    )
    first_session_chat <<- paste(
      as.character(output$chat_response_area),
      collapse = ""
    )
    nim_status_message <<- paste(
      as.character(output$nim_status),
      collapse = ""
    )
  }
)

second_session_chat <- NULL
shiny::testServer(
  mod_ai_chat_server,
  args = list(df_feat = df),
  {
    session$setInputs(
      user_query = "What about MI?",
      submit_btn = 1
    )
    second_session_chat <<- paste(
      as.character(output$chat_response_area),
      collapse = ""
    )
  }
)
stopifnot(
  grepl("Top Run Scorers", first_session_chat, fixed = TRUE),
  grepl("can't answer", second_session_chat, ignore.case = TRUE),
  grepl(
    if (nzchar(Sys.getenv("NVIDIA_API_KEY"))) "Configured" else "Not configured",
    nim_status_message,
    fixed = TRUE
  )
)

prediction_card <- NULL
shiny::testServer(
  mod_predictions_server,
  args = list(win_model = model_obj),
  {
    session$setInputs(
      target = 180,
      current_runs = 110,
      overs_bowled = "12.0",
      wickets_down = 3
    )
    first_prediction_card <- paste(
      as.character(output$win_prob_results_card),
      collapse = ""
    )
    session$setInputs(current_runs = 120)
    updated_prediction_card <- paste(
      as.character(output$win_prob_results_card),
      collapse = ""
    )
    prediction_card <<- c(first_prediction_card, updated_prediction_card)
  }
)
missing_model_card <- NULL
shiny::testServer(
  mod_predictions_server,
  args = list(win_model = missing_model),
  {
    missing_model_card <<- paste(
      as.character(output$win_prob_results_card),
      collapse = ""
    )
  }
)
stopifnot(
  grepl("64.2%", prediction_card[1], fixed = TRUE),
  grepl("Runs Required: 70 runs", prediction_card[1], fixed = TRUE),
  grepl("Runs Required: 60 runs", prediction_card[2], fixed = TRUE),
  grepl("Win probability model unavailable", missing_model_card, fixed = TRUE)
)

cat(
  "  [PASS] varied phrasing, malformed/empty/unsupported input, leaderboard paging,",
  "follow-ups, isolated Shiny chat sessions, reactive prediction updates, and visible model errors.\n\n"
)

cat("Test 7F: Validating malformed NIM response fallback...\n")
nim_test_env <- new.env(parent = environment(nim_client_call))
sys.source("ai/nim_client.R", envir = nim_test_env)
nim_test_env$Sys.getenv <- function(name) {
  if (identical(name, "NVIDIA_API_KEY")) "test-only-key" else ""
}
nim_test_env$POST <- function(...) structure(list(), class = "mock_response")
nim_test_env$status_code <- function(response) 200L
nim_test_env$content <- function(...) list(choices = list("not-a-message-object"))
malformed_nim_events <- list()
malformed_nim_result <- nim_test_env$call_nvidia_nim(
  "test",
  "test system prompt",
  status_callback = function(status, message) {
    malformed_nim_events[[length(malformed_nim_events) + 1L]] <<-
      list(status = status, message = message)
  }
)
stopifnot(
  is.null(malformed_nim_result),
  any(vapply(malformed_nim_events, function(event) {
    identical(event$status, "unavailable")
  }, logical(1))),
  !any(grepl("test-only-key", vapply(
    malformed_nim_events,
    `[[`,
    character(1),
    "message"
  ), fixed = TRUE))
)
cat("  [PASS] Malformed NIM response returns NULL for deterministic fallback.\n\n")

nim_test_env$status_code <- function(response) 410L
http_410_body <- paste0(
  '{"error":{"message":"The model deployment has been retired.",',
  '"authorization":"Bearer test-only-key",',
  '"api_key":"test-only-key"}}'
)
nim_test_key <- nim_test_env$Sys.getenv("NVIDIA_API_KEY")
http_410_body <- paste0(
  '{"error":{"message":"The model deployment has been retired.",',
  '"authorization":"Bearer ',
  nim_test_key,
  '","api_key":"',
  nim_test_key,
  '"}}'
)
nim_test_env$content <- function(response, as, encoding) {
  stopifnot(identical(as, "text"))
  http_410_body
}
http_410_events <- list()
http_410_logs <- capture.output(
  http_410_result <- nim_test_env$call_nvidia_nim(
    "test",
    "test system prompt",
    status_callback = function(status, message) {
      http_410_events[[length(http_410_events) + 1L]] <<-
        list(status = status, message = message)
    }
  ),
  type = "message"
)
http_410_diagnostic <- vapply(
  http_410_events,
  `[[`,
  character(1),
  "message"
)
stopifnot(
  is.null(http_410_result),
  any(vapply(http_410_events, function(event) {
    identical(event$status, "unavailable")
  }, logical(1))),
  any(grepl("HTTP 410", http_410_diagnostic, fixed = TRUE)),
  any(grepl("model deployment has been retired", http_410_diagnostic, fixed = TRUE)),
  !any(grepl("test-only-key", http_410_diagnostic, fixed = TRUE)),
  !any(grepl("test-only-key", http_410_logs, fixed = TRUE)),
  any(grepl("[REDACTED]", http_410_diagnostic, fixed = TRUE))
)
cat("  [PASS] HTTP 410 exposes a useful sanitized response body without credentials.\n\n")

nim_test_env$POST <- function(...) {
  stop(paste("Could not resolve api.nvidia.com.", "Authorization: Bearer test-only-key"))
}
nim_test_env$status_code <- function(response) 200L
nim_test_env$content <- function(...) list(choices = list("not-a-message-object"))
timeout_nim_events <- list()
timeout_nim_logs <- capture.output(
  timeout_nim_result <- nim_test_env$call_nvidia_nim(
    "test",
    "test system prompt",
    status_callback = function(status, message) {
      timeout_nim_events[[length(timeout_nim_events) + 1L]] <<-
        list(status = status, message = message)
    }
  ),
  type = "message"
)
timeout_nim_diagnostic <- vapply(
  timeout_nim_events,
  `[[`,
  character(1),
  "message"
)
stopifnot(
  is.null(timeout_nim_result),
  any(vapply(timeout_nim_events, function(event) {
    identical(event$status, "unavailable")
  }, logical(1))),
  any(grepl("Could not resolve api.nvidia.com", timeout_nim_diagnostic, fixed = TRUE)),
  !any(grepl("test-only-key", timeout_nim_diagnostic, fixed = TRUE)),
  !any(grepl("test-only-key", timeout_nim_logs, fixed = TRUE))
)
cat("  [PASS] Connection failures expose a sanitized diagnostic and use deterministic fallback.\n\n")

cat("Test 7G: Validating NIM-disabled diagnostics...\n")
stopifnot(grepl(
  "NVIDIA_API_KEY",
  nim_client_status(),
  fixed = TRUE
))
nim_disabled_result <- nim_client_call("test", "test system prompt")
stopifnot(is.null(nim_disabled_result))
missing_key_events <- list()
nim_client_call(
  "test",
  "test system prompt",
  status_callback = function(status, message) {
    missing_key_events[[length(missing_key_events) + 1L]] <<-
      list(status = status, message = message)
  }
)
stopifnot(any(vapply(missing_key_events, function(event) {
  identical(event$status, "not_configured")
}, logical(1))))
cat("  [PASS] Missing API key is diagnosed; no network call is required.\n\n")

nim_test_env$Sys.getenv <- function(name, unset = "") {
  if (identical(name, "NVIDIA_API_KEY")) "test-only-key" else ""
}
nim_test_env$POST <- function(url, ..., body, encode, config) {
  stopifnot(
    identical(url, "https://integrate.api.nvidia.com/v1/chat/completions"),
    identical(body$model, "nvidia/nemotron-3.5-lightning-30b-a3b")
  )
  structure(list(), class = "mock_response")
}
nim_test_env$status_code <- function(response) 200L
nim_test_env$content <- function(...) {
  list(choices = list(list(message = list(content = "Mocked NIM response."))))
}
connected_nim_events <- list()
connected_nim_result <- nim_test_env$call_nvidia_nim(
  "test",
  "test system prompt",
  status_callback = function(status, message) {
    connected_nim_events[[length(connected_nim_events) + 1L]] <<-
      list(status = status, message = message)
  }
)
stopifnot(
  identical(connected_nim_result, "Mocked NIM response."),
  any(vapply(connected_nim_events, function(event) {
    identical(event$status, "connected")
  }, logical(1)))
)
cat("  [PASS] Configured NIM uses the project endpoint/model and reports successful connectivity (mocked HTTP).\n\n")


cat("=====================================================\n")
cat("  ALL TEST SUITES PASSED WITH 100% SUCCESS!         \n")
# Test 8: Role-aware impact scores, global filters, and Data Explorer combinations
cat("Test 8: Validating responsive impact scoring and combined filters...\n")
synthetic <- data.frame(
  match_id = rep(1:4, each = 60), innings = 1,
  season_year = rep(c(2023, 2024, 2025, 2026), each = 60),
  date = as.Date("2024-01-01") + rep(0:3, each = 60),
  batting_team = rep(c("Team A", "Team B", "Team A", "Team B"), each = 60),
  bowling_team = rep(c("Team B", "Team A", "Team B", "Team A"), each = 60),
  venue = rep(c("Ground 1", "Ground 1", "Ground 2", "Ground 2"), each = 60),
  batter = rep(c("Player A", "Player B", "Player C", "Player D"), each = 60),
  bowler = rep(c("Player B", "Player C", "Player D", "Player A"), each = 60),
  runs_batter = rep(c(1, 2, 3, 4), each = 60), balls_faced = 1L,
  player_out = NA_character_, runs_bowler = rep(c(1, 2, 3, 4), each = 60),
  valid_ball = 1L, is_bowler_wicket = rep(rep(c(TRUE, FALSE), 30), 4),
  stringsAsFactors = FALSE
)
score_bat <- get_player_impact_scores(synthetic, 1, 0)
score_bowl <- get_player_impact_scores(synthetic, 0, 1)
score_reweighted <- weight_player_impact_scores(get_player_impact_scores(synthetic, 1, 1), 0, 1)
stopifnot(nrow(score_bat) == 4, nrow(score_bowl) == 4)
stopifnot(any(abs(score_bat$impact - score_bowl$impact) > 0.01, na.rm = TRUE))
stopifnot(isTRUE(all.equal(score_bowl$impact, score_reweighted$impact)))
profiles <- get_player_role_profiles(synthetic, c("Player A", "Player B"), "Ground 1")
stopifnot(all(c("Batting", "Bowling") %in% profiles$role), all(profiles$matches > 0), all(profiles$balls > 0))

global_filtered <- filter_global_data(synthetic, season = "2024", team = "Team A", venue = "Ground 1")
stopifnot(nrow(global_filtered) == 60, all(global_filtered$season_year == 2024))
explorer_filtered <- apply_explorer_filters(synthetic, global_search = "player a",
  category_filters = list(list(column = "venue", values = "Ground 1")),
  numeric_filters = list(list(column = "runs_batter", min = 1, max = 1)))
stopifnot(nrow(explorer_filtered) == 60, all(explorer_filtered$batter == "Player A"))
date_filtered <- apply_explorer_filters(synthetic, date_filter = list(column = "date", range = as.Date(c("2024-01-02", "2024-01-03"))))
stopifnot(nrow(date_filtered) == 120)
csv_tmp <- tempfile(fileext = ".csv")
write.csv(explorer_filtered, csv_tmp, row.names = FALSE)
stopifnot(nrow(read.csv(csv_tmp)) == nrow(explorer_filtered))
unlink(csv_tmp)
stopifnot(nrow(apply_explorer_filters(synthetic)) == nrow(synthetic))
cat("  [PASS] Role weights recalculate; H2H roles and samples, combined filters, reset baseline, and filtered CSV verified.\n\n")


cat("=====================================================\n")
cat("  ALL TEST SUITES PASSED WITH 100% SUCCESS!         \n")

cat("=====================================================\n")