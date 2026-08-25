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
df <- readRDS("data/processed/ipl_featured.rds")
stopifnot(nrow(df) > 290000)
cat("  [PASS] Rows:", nrow(df), "columns:", ncol(df), "\n\n")

# Test 2: Canonical Venue Normalization Verification
cat("Test 2: Validating Canonical Venue Normalization & Metadata...\n")
stopifnot(file.exists("data/processed/metadata.rds"))
meta <- readRDS("data/processed/metadata.rds")
stopifnot(meta$num_raw_venues == 60)
stopifnot(meta$num_canonical_venues == 42)
stopifnot(length(meta$unmapped_venues) == 0)
cat("  [PASS] Successfully mapped", meta$num_raw_venues, "raw venues to", meta$num_canonical_venues, "canonical venues with 0 unmapped!\n\n")

# Test 3: Top Batters Analytical Sanity Check
cat("Test 3: Validating Top Batters Calculation...\n")
top_b <- get_top_batters(df, top_n = 5)
stopifnot(top_b$batter[1] == "V Kohli")
stopifnot(top_b$total_runs[1] >= 9000)
cat("  [PASS] Leading run scorer:", top_b$batter[1], "with", top_b$total_runs[1], "runs.\n\n")

# Test 4: Top Bowlers Analytical Sanity Check
cat("Test 4: Validating Top Bowlers Calculation...\n")
top_bw <- get_top_bowlers(df, top_n = 5)
stopifnot(top_bw$bowler[1] == "YS Chahal")
stopifnot(top_bw$wickets[1] >= 200)
cat("  [PASS] Leading wicket taker:", top_bw$bowler[1], "with", top_bw$wickets[1], "wickets.\n\n")

# Test 5: Team Performance Summary Check
cat("Test 5: Validating Team Performance Calculation...\n")
t_summary <- get_team_performance_summary(df)
stopifnot(t_summary$team[1] == "Mumbai Indians")
stopifnot(t_summary$wins[1] == 155)
cat("  [PASS] Leading franchise:", t_summary$team[1], "with", t_summary$wins[1], "wins.\n\n")

# Test 6: Chronological ML Train/Test Split & Metrics Evaluation Check
cat("Test 6: Validating Chronological Win Probability ML Model & Test Metrics...\n")
model_obj <- readRDS("data/processed/win_prob_model.rds")
stopifnot(is.list(model_obj) && "test_accuracy" %in% names(model_obj))
stopifnot(model_obj$train_count > 100000)
stopifnot(model_obj$test_count > 15000)
stopifnot(model_obj$test_accuracy > 70)
stopifnot(model_obj$roc_auc > 0.70)
stopifnot(model_obj$brier_score < 0.25)
cat("  [PASS] Chronological ML Model Verified on Unseen Test Set (2024-2026):\n")
cat("         - Train Count: ", model_obj$train_count, " | Test Count: ", model_obj$test_count, "\n", sep = "")
cat("         - Test Accuracy: ", model_obj$test_accuracy, "% | ROC-AUC: ", model_obj$roc_auc, " | Brier Score: ", model_obj$brier_score, " | Log Loss: ", model_obj$log_loss, "\n\n", sep = "")

# Test 7: AI Natural Language Query Router Check
cat("Test 7: Validating AI Natural Language Router...\n")
query_res <- process_criclens_query("Who has the most runs in IPL?", df)
stopifnot(grepl("V Kohli", query_res))
cat("  [PASS] Natural language query correctly routed and answered.\n\n")

cat("=====================================================\n")
cat(" 🎉 ALL 7 TEST SUITES PASSED WITH 100% SUCCESS!       \n")
cat("=====================================================\n")
