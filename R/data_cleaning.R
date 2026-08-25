# R/data_cleaning.R
# CricLens — Data Cleaning Module
# Source of truth for cleaning raw IPL dataset

suppressMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

#' Team Name Standardization Mapping
#' Standardizes historical team names to unified franchise names
standardize_team_name <- function(team_vec) {
  case_when(
    team_vec %in% c("Royal Challengers Bangalore", "Royal Challengers Bengaluru") ~ "Royal Challengers Bengaluru",
    team_vec %in% c("Delhi Daredevils", "Delhi Capitals") ~ "Delhi Capitals",
    team_vec %in% c("Kings XI Punjab", "Punjab Kings") ~ "Punjab Kings",
    team_vec %in% c("Rising Pune Supergiant", "Rising Pune Supergiants") ~ "Rising Pune Supergiant",
    TRUE ~ team_vec
  )
}

#' Canonical Venue Name Standardization Mapping
#' Maps raw venue text variants to physical canonical stadium entities
standardize_venue_name <- function(venue_vec) {
  case_when(
    venue_vec %in% c("ACA-VDCA Stadium", "Dr. Y.S. Rajasekhara Reddy ACA-VDCA Cricket Stadium", "Dr. Y.S. Rajasekhara Reddy ACA-VDCA Cricket Stadium, Visakhapatnam") ~ "Dr. Y.S. Rajasekhara Reddy ACA-VDCA Cricket Stadium",
    venue_vec %in% c("Abu Dhabi Cricket Stadium", "Sheikh Zayed Stadium") ~ "Sheikh Zayed Stadium",
    venue_vec %in% c("Arun Jaitley Stadium", "Arun Jaitley Stadium, Delhi", "Feroz Shah Kotla") ~ "Arun Jaitley Stadium",
    venue_vec %in% c("Al Amerat Cricket Ground (Ministry Turf 1)") ~ "Al Amerat Cricket Ground",
    venue_vec %in% c("Barabati Stadium") ~ "Barabati Stadium",
    venue_vec %in% c("Barkatullah Khan Stadium") ~ "Barkatullah Khan Stadium",
    venue_vec %in% c("Bharat Ratna Shri Atal Bihari Vajpayee Ekana Cricket Stadium", "Bharat Ratna Shri Atal Bihari Vajpayee Ekana Cricket Stadium, Lucknow") ~ "Ekana Cricket Stadium",
    venue_vec %in% c("Bloemfontein Custom", "OUTsurance Oval") ~ "OUTsurance Oval",
    venue_vec %in% c("Brabourne Stadium", "Brabourne Stadium, Mumbai") ~ "Brabourne Stadium",
    venue_vec %in% c("Buffalo Park") ~ "Buffalo Park",
    venue_vec %in% c("De Beers Diamond Oval") ~ "De Beers Diamond Oval",
    venue_vec %in% c("Dubai International Cricket Stadium") ~ "Dubai International Cricket Stadium",
    venue_vec %in% c("Eden Gardens", "Eden Gardens, Kolkata") ~ "Eden Gardens",
    venue_vec %in% c("Green Park") ~ "Green Park",
    venue_vec %in% c("Himachal Pradesh Cricket Association Stadium", "Himachal Pradesh Cricket Association Stadium, Dharamsala") ~ "HPCA Stadium, Dharamsala",
    venue_vec %in% c("Holkar Cricket Stadium") ~ "Holkar Cricket Stadium",
    venue_vec %in% c("IS Bindra Stadium", "Punjab Cricket Association IS Bindra Stadium", "Punjab Cricket Association IS Bindra Stadium, Mohali", "Punjab Cricket Association Stadium, Mohali") ~ "PCA IS Bindra Stadium, Mohali",
    venue_vec %in% c("JSCA International Stadium Complex") ~ "JSCA International Stadium Complex",
    venue_vec %in% c("Kingsmead") ~ "Kingsmead",
    venue_vec %in% c("M Chinnaswamy Stadium", "M Chinnaswamy Stadium, Bengaluru", "M. Chinnaswamy Stadium", "M.Chinnaswamy Stadium") ~ "M. Chinnaswamy Stadium",
    venue_vec %in% c("MA Chidambaram Stadium", "MA Chidambaram Stadium, Chepauk", "MA Chidambaram Stadium, Chepauk, Chennai") ~ "MA Chidambaram Stadium, Chepauk",
    venue_vec %in% c("Maharashtra Cricket Association Stadium", "Maharashtra Cricket Association Stadium, Pune", "Subrata Roy Sahara Stadium") ~ "Maharashtra Cricket Association Stadium",
    venue_vec %in% c("Maharaja Yadavindra Singh International Cricket Stadium, Mullanpur") ~ "Maharaja Yadavindra Singh Stadium, Mullanpur",
    venue_vec %in% c("Narendra Modi Stadium, Ahmedabad") ~ "Narendra Modi Stadium",
    venue_vec %in% c("Nehru Stadium") ~ "Nehru Stadium, Kochi",
    venue_vec %in% c("Newlands") ~ "Newlands",
    venue_vec %in% c("New Wanderers Stadium") ~ "Wanderers Stadium",
    venue_vec %in% c("Rajiv Gandhi International Stadium", "Rajiv Gandhi International Stadium, Uppal", "Rajiv Gandhi International Stadium, Uppal, Hyderabad") ~ "Rajiv Gandhi International Stadium",
    venue_vec %in% c("Sector 16 Stadium") ~ "Sector 16 Stadium",
    venue_vec %in% c("Sawai Mansingh Stadium", "Sawai Mansingh Stadium, Jaipur") ~ "Sawai Mansingh Stadium",
    venue_vec %in% c("Shaheed Veer Narayan Singh International Stadium") ~ "Shaheed Veer Narayan Singh International Stadium",
    venue_vec %in% c("Sharjah Cricket Stadium") ~ "Sharjah Cricket Stadium",
    venue_vec %in% c("St George's Park") ~ "St George's Park",
    venue_vec %in% c("SuperSport Park") ~ "SuperSport Park",
    venue_vec %in% c("Vidarbha Cricket Association Stadium, Jamtha") ~ "VCA Stadium, Jamtha",
    venue_vec %in% c("Wankhede Stadium", "Wankhede Stadium, Mumbai") ~ "Wankhede Stadium",
    TRUE ~ venue_vec
  )
}

#' Normalize Season Labels
#' Converts season strings (e.g., '2007/08', '2020/21') into canonical integer years
standardize_season_year <- function(season_vec) {
  case_when(
    season_vec == "2007/08" ~ 2008,
    season_vec == "2009/10" ~ 2010,
    season_vec == "2020/21" ~ 2020,
    TRUE ~ suppressWarnings(as.numeric(season_vec))
  )
}

#' Fill Missing Cities Based on Venue Name
fill_missing_city <- function(df) {
  df %>%
    mutate(
      city = case_when(
        !is.na(city) & city != "Unknown" ~ city,
        str_detect(venue, "Dubai") ~ "Dubai",
        str_detect(venue, "Sharjah") ~ "Sharjah",
        str_detect(venue, "Abu Dhabi") ~ "Abu Dhabi",
        str_detect(venue, "Bengaluru|Chinnaswamy") ~ "Bengaluru",
        str_detect(venue, "Navi Mumbai") ~ "Navi Mumbai",
        str_detect(venue, "Centurion|SuperSport") ~ "Centurion",
        str_detect(venue, "Johannesburg") ~ "Johannesburg",
        str_detect(venue, "Durban") ~ "Durban",
        str_detect(venue, "Port Elizabeth") ~ "Port Elizabeth",
        str_detect(venue, "Cape Town") ~ "Cape Town",
        str_detect(venue, "Kimberley") ~ "Kimberley",
        str_detect(venue, "Bloemfontein") ~ "Bloemfontein",
        str_detect(venue, "Cuttack") ~ "Cuttack",
        str_detect(venue, "Visakhapatnam") ~ "Visakhapatnam",
        str_detect(venue, "Ranchi") ~ "Ranchi",
        str_detect(venue, "Raipur") ~ "Raipur",
        str_detect(venue, "Indore") ~ "Indore",
        str_detect(venue, "Dharamsala") ~ "Dharamsala",
        str_detect(venue, "Kanpur") ~ "Kanpur",
        str_detect(venue, "Rajkot") ~ "Rajkot",
        TRUE ~ "Other"
      )
    )
}

#' Main Function: Clean IPL Raw Data
#' @param raw_path Path to IPL.csv file
#' @return A cleaned tibble with standardized columns and flags
clean_ipl_data <- function(raw_path = "data/raw/IPL.csv") {
  if (!file.exists(raw_path)) {
    # Fallback check if dataset is in root data/ folder
    if (file.exists("data/IPL.csv")) {
      raw_path <- "data/IPL.csv"
    } else {
      stop(paste("Raw dataset not found at", raw_path))
    }
  }

  cat("Loading raw IPL dataset from:", raw_path, "...\n")
  df <- read_csv(raw_path, show_col_types = FALSE)

  cat("Cleaning and standardizing fields...\n")
  df_clean <- df %>%
    # Preserve original raw team names and raw venue names for transparency
    mutate(
      raw_batting_team = batting_team,
      raw_bowling_team = bowling_team,
      raw_toss_winner = toss_winner,
      raw_match_won_by = match_won_by,
      raw_venue = venue
    ) %>%
    # Apply standardized team names & canonical venue names
    mutate(
      batting_team = standardize_team_name(batting_team),
      bowling_team = standardize_team_name(bowling_team),
      toss_winner = standardize_team_name(toss_winner),
      match_won_by = standardize_team_name(match_won_by),
      superover_winner = ifelse(!is.na(superover_winner), standardize_team_name(superover_winner), NA_character_),
      canonical_venue = standardize_venue_name(venue),
      venue = canonical_venue
    ) %>%
    # Standardize Season
    mutate(
      season_year = standardize_season_year(season),
      season_display = paste("IPL", season_year)
    ) %>%
    # Fill missing cities
    fill_missing_city() %>%
    # Wicket categorization flags
    mutate(
      is_wicket = !is.na(wicket_kind),
      is_bowler_wicket = is_wicket & (wicket_kind %in% c("bowled", "caught", "caught and bowled", "lbw", "stumped", "hit wicket")),
      is_run_out = is_wicket & (wicket_kind == "run out")
    ) %>%
    # Ensure date is Date type
    mutate(date = as.Date(date)) %>%
    # Ensure ball delivery valid flag is integer (1 or 0)
    mutate(
      valid_ball = as.integer(valid_ball),
      balls_faced = as.integer(balls_faced)
    )

  # Generate and save metadata summary object
  raw_venues <- unique(df_clean$raw_venue)
  canonical_venues <- unique(df_clean$canonical_venue)
  mapping_table <- df_clean %>% distinct(raw_venue, canonical_venue, city)
  
  metadata <- list(
    num_raw_venues = length(raw_venues),
    num_canonical_venues = length(canonical_venues),
    venue_mappings = mapping_table,
    unmapped_venues = character(0)
  )
  out_dir <- "data/processed"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  saveRDS(metadata, file.path(out_dir, "metadata.rds"))

  cat("Data cleaning completed successfully. Total rows:", nrow(df_clean), "\n")
  return(df_clean)
}

# Execute cleaning script if run directly
if (sys.nframe() == 0) {
  cleaned_df <- clean_ipl_data()
  out_dir <- "data/processed"
  out_path <- file.path(out_dir, "ipl_cleaned.rds")
  saveRDS(cleaned_df, out_path)
  cat("Saved cleaned dataset to:", out_path, "\n")
}
