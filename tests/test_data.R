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

prediction <- predict_live_win_prob(
  target = 180,
  current_runs = 110,
  overs_bowled = 12,
  wickets_down = 3,
  model = model_obj
)

stopifnot(
  prediction$chase_win_prob >= 1,
  prediction$chase_win_prob <= 99,
  prediction$defend_win_prob >= 1,
  prediction$defend_win_prob <= 99,
  prediction$chase_win_prob + prediction$defend_win_prob == 100
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
  "  [PASS] Live prediction accepts the serialized model wrapper and fitted glm; invalid models fail with a clear message.\n\n"
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
stopifnot(
  identical(empty_query, "Please enter a question."),
  grepl("can't answer", unsupported_query, ignore.case = TRUE),
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
first_session_chat <- NULL
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
  grepl("can't answer", second_session_chat, ignore.case = TRUE)
)

cat(
  "  [PASS] varied phrasing, malformed/empty/unsupported input, leaderboard paging,",
  "follow-ups, and isolated Shiny chat sessions.\n\n"
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
malformed_nim_result <- nim_test_env$call_nvidia_nim(
  "test",
  "test system prompt"
)
stopifnot(is.null(malformed_nim_result))
cat("  [PASS] Malformed NIM response returns NULL for deterministic fallback.\n\n")

cat("Test 7G: Validating NIM-disabled diagnostics...\n")
stopifnot(grepl(
  "NVIDIA_API_KEY is not set",
  nim_client_status(),
  fixed = TRUE
))
nim_disabled_result <- nim_client_call("test", "test system prompt")
stopifnot(is.null(nim_disabled_result))
cat("  [PASS] Missing API key is diagnosed; no network call is required.\n\n")


cat("=====================================================\n")
cat("  ALL TEST SUITES PASSED WITH 100% SUCCESS!         \n")
cat("=====================================================\n")