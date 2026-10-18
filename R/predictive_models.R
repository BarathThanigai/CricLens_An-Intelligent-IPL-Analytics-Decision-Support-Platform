# R/predictive_models.R
# CricLens — Predictive Analytics Engine
# Machine learning model for 2nd innings match-state win probability prediction

suppressMessages({
  library(dplyr)
  library(stats)
})

MODEL_CACHE_PATH <- "data/processed/win_prob_model.rds"

#' Calculate Area Under ROC Curve (ROC-AUC) using Exact Trapezoidal Integration
calculate_roc_auc <- function(actual, predicted) {
  ord <- order(predicted, decreasing = TRUE)
  actual <- actual[ord]
  predicted <- predicted[ord]

  n_pos <- sum(actual == 1)
  n_neg <- sum(actual == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)

  tp_cumsum <- cumsum(actual == 1)
  fp_cumsum <- cumsum(actual == 0)

  auc <- sum((fp_cumsum[-1] - fp_cumsum[-length(fp_cumsum)]) * (tp_cumsum[-1] + tp_cumsum[-length(tp_cumsum)]) / 2) / (n_pos * n_neg)
  return(round(auc, 4))
}

#' Train & Validate Win Probability Logistic Regression Model using Chronological Train/Test Split
#' @param df Featured IPL dataframe
#' @param train_end_year Year marking end of training period (default 2023)
#' @return List containing trained model object and test evaluation metrics
train_and_validate_win_prob_model <- function(df, train_end_year = 2023) {
  cat("Preparing delivery snapshots for Win Probability Model...\n")

  chase_df <- df %>%
    filter(innings == 2 & !is.na(runs_target) & valid_ball == 1) %>%
    mutate(
      is_chase_winner = as.integer(batting_team == match_won_by),
      balls_remaining = pmax(1, 120 - cum_valid_balls),
      wickets_remaining = pmax(0, 10 - cum_team_wickets),
      runs_required = pmax(0, runs_target - cum_team_runs),
      req_rr = ifelse(balls_remaining > 0, runs_required / (balls_remaining / 6), 30),
      current_rr = ifelse(cum_valid_balls > 0, cum_team_runs / (cum_valid_balls / 6), 0)
    ) %>%
    select(season_year, match_id, is_chase_winner, runs_required, balls_remaining, wickets_remaining, req_rr, current_rr) %>%
    filter(!is.na(is_chase_winner) & !is.na(req_rr) & !is.na(current_rr))

  # Chronological Train / Test Split
  train_df <- chase_df %>% filter(season_year <= train_end_year)
  test_df  <- chase_df %>% filter(season_year > train_end_year)

  cat("Chronological Split Summary:\n")
  cat("  - Training Set (Seasons <= ", train_end_year, "): ", nrow(train_df), " deliveries\n", sep = "")
  cat("    Class Dist: ", sum(train_df$is_chase_winner == 1), " Chase Wins / ", sum(train_df$is_chase_winner == 0), " Defend Wins\n", sep = "")
  cat("  - Testing Set  (Seasons > ", train_end_year, "): ", nrow(test_df), " deliveries\n", sep = "")
  cat("    Class Dist: ", sum(test_df$is_chase_winner == 1), " Chase Wins / ", sum(test_df$is_chase_winner == 0), " Defend Wins\n", sep = "")

  # Fit Logistic Regression Model on Training Set ONLY
  cat("\nFitting Logistic Regression baseline on Training Set...\n")
  win_model <- glm(
    is_chase_winner ~ runs_required + balls_remaining + wickets_remaining + req_rr + current_rr,
    data = train_df,
    family = binomial(link = "logit")
  )

  # Evaluate Model strictly on UNSEEN Test Set
  cat("Evaluating model on unseen Test Set...\n")
  test_pred_prob <- predict(win_model, newdata = test_df, type = "response")
  test_pred_class <- ifelse(test_pred_prob >= 0.5, 1, 0)

  # Compute Metrics on Test Set
  test_acc <- round(mean(test_pred_class == test_df$is_chase_winner) * 100, 2)
  brier_score <- round(mean((test_pred_prob - test_df$is_chase_winner)^2), 4)
  log_loss <- round(-mean(test_df$is_chase_winner * log(pmax(1e-15, test_pred_prob)) + (1 - test_df$is_chase_winner) * log(pmax(1e-15, 1 - test_pred_prob))), 4)
  roc_auc <- calculate_roc_auc(test_df$is_chase_winner, test_pred_prob)

  cat("-----------------------------------------------\n")
  cat("  TEST SET EVALUATION RESULTS (Seasons 2024-2026)\n")
  cat("-----------------------------------------------\n")
  cat("  Test Accuracy: ", test_acc, "%\n", sep = "")
  cat("  ROC-AUC:       ", roc_auc, "\n", sep = "")
  cat("  Brier Score:   ", brier_score, "\n", sep = "")
  cat("  Log Loss:      ", log_loss, "\n", sep = "")
  cat("-----------------------------------------------\n\n")

  result_list <- list(
    model = win_model,
    train_count = nrow(train_df),
    test_count = nrow(test_df),
    train_class_dist = table(train_df$is_chase_winner),
    test_class_dist = table(test_df$is_chase_winner),
    test_accuracy = test_acc,
    roc_auc = roc_auc,
    brier_score = brier_score,
    log_loss = log_loss
  )

  return(result_list)
}

#' Load or Train Cached Win Probability Model
get_win_prob_model <- function(df = NULL) {
  if (file.exists(MODEL_CACHE_PATH)) {
    model_obj <- load_win_prob_model(MODEL_CACHE_PATH)
    if (inherits(model_obj, "criclens_model_load_error")) {
      stop(model_obj$message, call. = FALSE)
    }
    if (is.list(model_obj) && "model" %in% names(model_obj)) {
      return(model_obj$model)
    }
    return(model_obj)
  } else if (!is.null(df)) {
    res <- train_and_validate_win_prob_model(df)
    saveRDS(res, MODEL_CACHE_PATH)
    return(res$model)
  } else {
    stop("Model file not found and no dataset provided to train.")
  }
}

#' Load the saved win probability model without preventing the app from starting
load_win_prob_model <- function(path = MODEL_CACHE_PATH) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(structure(
      list(message = "The win probability model path is invalid."),
      class = "criclens_model_load_error"
    ))
  }

  if (!file.exists(path)) {
    return(structure(
      list(message = paste0("The win probability model file was not found: ", path)),
      class = "criclens_model_load_error"
    ))
  }

  tryCatch(
    readRDS(path),
    error = function(error) {
      structure(
        list(message = paste0(
          "The win probability model could not be loaded: ",
          conditionMessage(error)
        )),
        class = "criclens_model_load_error"
      )
    }
  )
}

#' Predict Match-State Win Probability for Chasing Team
predict_live_win_prob <- function(target, current_runs, overs_bowled, wickets_down, model = NULL) {
  if (is.null(model)) {
    model <- get_win_prob_model()
  }

  if (!inherits(model, "glm") && is.list(model) && "model" %in% names(model)) {
    model <- model[["model"]]
  }

  if (
    !inherits(model, "glm") ||
      is.null(model$family) ||
      model$family$family != "binomial"
  ) {
    stop(
      "predict_live_win_prob: expected a fitted binomial glm or a training result list containing one."
    )
  }

  required_predictors <- c(
    "runs_required",
    "balls_remaining",
    "wickets_remaining",
    "req_rr",
    "current_rr"
  )
  model_predictors <- all.vars(
    stats::delete.response(
      stats::terms(model)
    )
  )
  if (!all(required_predictors %in% model_predictors)) {
    stop(
      "predict_live_win_prob: fitted model is missing one or more live match-state predictors."
    )
  }

  if (!setequal(required_predictors, model_predictors)) {
    stop(
      "predict_live_win_prob: fitted model predictors do not match the supported live match-state schema."
    )
  }

  model_response <- all.vars(stats::terms(model))
  if (length(model_response) != length(required_predictors) + 1L ||
      model_response[1] != "is_chase_winner") {
    stop(
      "predict_live_win_prob: fitted model must predict the is_chase_winner target."
    )
  }

  normalize_input <- function(value, name) {
    if (!is.atomic(value) || length(value) != 1L || is.na(value)) {
      stop("predict_live_win_prob: ", name, " must be a single finite number.", call. = FALSE)
    }
    normalized <- suppressWarnings(as.numeric(value))
    if (is.na(normalized) || !is.finite(normalized)) {
      stop("predict_live_win_prob: ", name, " must be a single finite number.", call. = FALSE)
    }
    normalized
  }

  target <- normalize_input(target, "target")
  current_runs <- normalize_input(current_runs, "current_runs")
  overs_bowled <- normalize_input(overs_bowled, "overs_bowled")
  wickets_down <- normalize_input(wickets_down, "wickets_down")

  if (target <= 0 || target > 300 || target != floor(target)) {
    stop("predict_live_win_prob: target must be a whole-number score from 1 to 300.", call. = FALSE)
  }
  if (current_runs < 0 || current_runs >= target || current_runs != floor(current_runs)) {
    stop("predict_live_win_prob: current_runs must be a whole number from 0 up to, but not including, target.", call. = FALSE)
  }

  overs_tenths <- round(overs_bowled * 10)
  if (abs(overs_bowled * 10 - overs_tenths) > 1e-8 ||
      overs_tenths < 0 || overs_tenths > 195 || overs_tenths %% 10 > 5) {
    stop(
      "predict_live_win_prob: overs_bowled must use valid cricket notation from 0.0 to 19.5.",
      call. = FALSE
    )
  }

  if (wickets_down < 0 || wickets_down > 9 || wickets_down != floor(wickets_down)) {
    stop("predict_live_win_prob: wickets_down must be a whole number from 0 to 9.", call. = FALSE)
  }

  balls_bowled <- (overs_tenths %/% 10) * 6 + overs_tenths %% 10
  balls_remaining <- 120 - balls_bowled
  wickets_remaining <- 10 - wickets_down
  runs_required <- target - current_runs
  req_rr <- ifelse(balls_remaining > 0, runs_required / (balls_remaining / 6), 30)
  current_rr <- ifelse(balls_bowled > 0, current_runs / (balls_bowled / 6), 0)

  new_obs <- data.frame(
    runs_required = runs_required,
    balls_remaining = balls_remaining,
    wickets_remaining = wickets_remaining,
    req_rr = req_rr,
    current_rr = current_rr
  )

  chase_win_prob <- tryCatch(
    as.numeric(stats::predict(model, newdata = new_obs, type = "response")),
    error = function(error) {
      stop(
        "predict_live_win_prob: prediction failed: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  if (length(chase_win_prob) != 1L ||
      !is.finite(chase_win_prob) ||
      chase_win_prob < 0 ||
      chase_win_prob > 1) {
    stop(
      "predict_live_win_prob: model returned an invalid win probability.",
      call. = FALSE
    )
  }

  return(list(
    chase_win_prob = chase_win_prob,
    defend_win_prob = 1 - chase_win_prob,
    runs_required = runs_required,
    balls_remaining = balls_remaining,
    req_rr = round(req_rr, 2),
    current_rr = round(current_rr, 2)
  ))
}

# Run training & validation if executed directly
if (sys.nframe() == 0) {
  df <- readRDS("data/processed/ipl_featured.rds")
  res <- train_and_validate_win_prob_model(df)
  saveRDS(res, MODEL_CACHE_PATH)
  cat("Saved validated win probability model & metadata to:", MODEL_CACHE_PATH, "\n")
}
