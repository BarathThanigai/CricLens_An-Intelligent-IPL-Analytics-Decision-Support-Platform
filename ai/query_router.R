# ai/query_router.R
# CricLens — Natural Language Query Router
# Maps natural language queries to R analytics functions and produces validated natural language answers

suppressMessages({
  library(dplyr)
  library(stringr)
  library(jsonlite)
})

# Source analytical modules if not present
if (!exists("get_top_batters")) source("R/player_analysis.R")
if (!exists("get_team_performance_summary")) source("R/team_analysis.R")
if (!exists("get_venue_leaderboard")) source("R/venue_analysis.R")
if (!exists("call_nvidia_nim")) source("ai/nim_client.R")
if (!exists("INTENT_PARSER_SYSTEM_PROMPT")) source("ai/prompt_templates.R")

#' Convert Markdown string to HTML without external package dependencies
render_markdown_to_html <- function(text) {
  text <- gsub("### (.*)", "<h4 style='color: #00d2ff; font-weight: 700; margin-top: 15px;'>\\1</h4>", text)
  text <- gsub("## (.*)", "<h3 style='color: #ffd700; font-weight: 800;'>\\1</h3>", text)
  text <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", text)
  
  lines <- strsplit(text, "\n")[[1]]
  html_lines <- c()
  in_table <- FALSE
  
  for (line in lines) {
    if (grepl("^\\|", line)) {
      if (grepl("---", line)) next
      cells <- strsplit(line, "\\|")[[1]]
      cells <- cells[nchar(trimws(cells)) > 0]
      if (!in_table) {
        in_table <- TRUE
        html_lines <- c(html_lines, "<table class='table table-dark table-striped my-3'><thead><tr>")
        for (c in cells) html_lines <- c(html_lines, paste0("<th>", trimws(c), "</th>"))
        html_lines <- c(html_lines, "</tr></thead><tbody>")
      } else {
        html_lines <- c(html_lines, "<tr>")
        for (c in cells) html_lines <- c(html_lines, paste0("<td>", trimws(c), "</td>"))
        html_lines <- c(html_lines, "</tr>")
      }
    } else {
      if (in_table) {
        in_table <- FALSE
        html_lines <- c(html_lines, "</tbody></table>")
      }
      if (nchar(trimws(line)) > 0) {
        html_lines <- c(html_lines, paste0("<p style='margin-bottom: 8px;'>", line, "</p>"))
      }
    }
  }
  if (in_table) html_lines <- c(html_lines, "</tbody></table>")
  return(paste(html_lines, collapse = "\n"))
}

#' Main Entrypoint: Process Natural Language Question
#' @param query User question string (e.g. "Who has the most runs in IPL?")
#' @param df Featured dataset
#' @return Formatted response string containing verified R results and explanation
process_criclens_query <- function(query, df) {
  q_lower <- tolower(query)

  # Rule 1: Player Search / Top Batters Query
  if (str_detect(q_lower, "top batter|most run|highest run|run scorer|orange cap|leading run")) {
    r_result <- get_top_batters(df, top_n = 5)
    formatted_table <- paste(
      "### 🏏 Top 5 IPL Run Scorers (Verified R Analytics)\n\n",
      "| Batter | Total Runs | Innings | Average | Strike Rate | 4s | 6s |\n",
      "| :--- | :--- | :--- | :--- | :--- | :--- | :--- |\n",
      paste0("| **", r_result$batter, "** | ", r_result$total_runs, " | ", r_result$innings, " | ", r_result$average, " | ", r_result$strike_rate, " | ", r_result$fours, " | ", r_result$sixes, " |", collapse = "\n"),
      "\n\n**Analytical Insight**: **", r_result$batter[1], "** leads all-time IPL scoring with **", r_result$total_runs[1], " runs** at an average of **", r_result$average[1], "**."
    )
    return(formatted_table)
  }

  # Rule 2: Top Bowlers Query
  if (str_detect(q_lower, "top bowler|most wicket|highest wicket|purple cap|leading wicket")) {
    r_result <- get_top_bowlers(df, top_n = 5)
    formatted_table <- paste(
      "### 🎯 Top 5 IPL Wicket Takers (Verified R Analytics)\n\n",
      "| Bowler | Wickets | Economy | Average | Overs | Dot Balls |\n",
      "| :--- | :--- | :--- | :--- | :--- | :--- |\n",
      paste0("| **", r_result$bowler, "** | ", r_result$wickets, " | ", r_result$economy, " | ", r_result$average, " | ", r_result$overs_bowled, " | ", r_result$dot_balls, " |", collapse = "\n"),
      "\n\n**Analytical Insight**: **", r_result$bowler[1], "** is the highest wicket-taker with **", r_result$wickets[1], " wickets** at an economy of **", r_result$economy[1], "**."
    )
    return(formatted_table)
  }

  # Rule 3: Team Wins Query
  if (str_detect(q_lower, "team win|most win|best team|trophy|franchise win|leaderboard")) {
    r_result <- get_team_performance_summary(df) %>% head(5)
    formatted_table <- paste(
      "### 🏆 Top IPL Franchises by Total Wins (Verified R Analytics)\n\n",
      "| Franchise | Total Matches | Wins | Losses | Win % |\n",
      "| :--- | :--- | :--- | :--- | :--- |\n",
      paste0("| **", r_result$team, "** | ", r_result$matches_played, " | ", r_result$wins, " | ", r_result$losses, " | ", r_result$win_pct, "% |", collapse = "\n"),
      "\n\n**Analytical Insight**: **", r_result$team[1], "** holds the record for most IPL match victories with **", r_result$wins[1], " wins**."
    )
    return(formatted_table)
  }

  # Rule 4: Specific Player Search (e.g. Kohli, Dhoni, Rohit, Bumrah)
  if (str_detect(q_lower, "kohli|rohit|dhoni|bumrah|chahal|warner|samson|rahane|raina|dhawan")) {
    player_match <- case_when(
      str_detect(q_lower, "kohli") ~ "V Kohli",
      str_detect(q_lower, "rohit") ~ "RG Sharma",
      str_detect(q_lower, "dhoni") ~ "MS Dhoni",
      str_detect(q_lower, "bumrah") ~ "JJ Bumrah",
      str_detect(q_lower, "chahal") ~ "YS Chahal",
      str_detect(q_lower, "warner") ~ "DA Warner",
      str_detect(q_lower, "samson") ~ "SV Samson",
      str_detect(q_lower, "rahane") ~ "AM Rahane",
      str_detect(q_lower, "raina") ~ "SK Raina",
      str_detect(q_lower, "dhawan") ~ "S Dhawan",
      TRUE ~ "V Kohli"
    )

    prof <- get_player_batting_profile(df, player_match)
    if (!is.null(prof)) {
      formatted_resp <- paste0(
        "### 📊 Player Profile: **", prof$player, "** (Verified R Analytics)\n\n",
        "- **Matches Played**: ", prof$matches, "\n",
        "- **Innings Batted**: ", prof$innings, "\n",
        "- **Total Runs**: ", prof$total_runs, "\n",
        "- **Batting Average**: ", prof$average, "\n",
        "- **Strike Rate**: ", prof$strike_rate, "\n",
        "- **Boundaries**: ", prof$fours, " Fours (4s) | ", prof$sixes, " Sixes (6s)\n",
        "- **50s / 100s**: ", prof$fifties, " Fifties | ", prof$hundreds, " Hundreds\n"
      )
      return(formatted_resp)
    }
  }

  # Rule 5: Venue / Stadium Queries
  if (str_detect(q_lower, "venue|stadium|pitch|chepauk|wankhede|chinnaswamy|eden gardens|highest score ground")) {
    v_lead <- get_venue_leaderboard(df) %>% head(5)
    formatted_table <- paste(
      "### 🏟️ Stadium Scoring Leaderboard (Verified R Analytics)\n\n",
      "| Stadium | Matches | Avg 1st Inn Score | Chasing Win % |\n",
      "| :--- | :--- | :--- | :--- |\n",
      paste0("| **", v_lead$venue, "** | ", v_lead$matches, " | ", v_lead$avg_1st_inn_score, " | ", v_lead$chasing_win_pct, "% |", collapse = "\n"),
      "\n\n**Analytical Insight**: **", v_lead$venue[1], "** exhibits the highest average 1st innings score of **", v_lead$avg_1st_inn_score[1], " runs**."
    )
    return(formatted_table)
  }

  # Default fallback
  r_result <- get_top_batters(df, top_n = 3)
  return(paste0(
    "### 🤖 Ask CricLens — AI Query Engine\n\n",
    "I analyzed your query: *\"", query, "\"*\n\n",
    "Here are top insights from the verified R analytics engine:\n\n",
    "1. **All-time Leading Run Scorer**: ", r_result$batter[1], " (", r_result$total_runs[1], " runs)\n",
    "2. **Highest Strike Rate (Top 3)**: ", r_result$batter[r_result$strike_rate == max(r_result$strike_rate)][1], " (", max(r_result$strike_rate), " SR)\n\n",
    "You can ask about specific players (e.g. *'Show V Kohli stats'*), team performance (*'Which team has the most wins?'*), or venues (*'Which ground has the highest average score?'*)."
  ))
}
