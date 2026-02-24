#!/usr/bin/env Rscript
################################################################################
# COMPLETE NFL BETTING MODEL v2.1
# Monte Carlo Simulation with High-Value Data Sources
#
# Features:
# - EPA-based advanced metrics (gold standard in NFL analytics)
# - Official NFL injury reports with impact scoring
# - Rest and travel advantage calculations
# - Weather integration (optional API)
# - Matchup-specific analysis
# - Monte Carlo simulation (10,000+ iterations)
# - Realistic predictions
#
# Quick Start:
#   1. install_packages()  # First time only
#   2. predictions <- run_analysis()  # Analyze all upcoming games
#   3. analyze_game("KC", "BUF")  # Analyze specific matchup
#   4. results <- backtest_last_season()  # Backtest
#
# Author: Enhanced Model
# Version: 2.1
################################################################################

install_packages <- function() {
  required_packages <- c("nflreadr", "httr", "jsonlite", "dplyr", "lubridate", "tidyr")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

  if (length(new_packages)) {
    cat("Installing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, quiet = TRUE)
    cat("✓ Installation complete!\n")
  } else {
    cat("✓ All required packages already installed!\n")
  }
}

check_packages <- function() {
  required <- c("nflreadr", "httr", "jsonlite", "dplyr", "lubridate", "tidyr")
  missing <- required[!(required %in% installed.packages()[,"Package"])]
  if (length(missing) > 0) {
    cat("Missing packages:", paste(missing, collapse = ", "), "\n")
    cat("Run: install_packages()\n")
    return(FALSE)
  }
  return(TRUE)
}

load_libraries <- function() {
  if (!check_packages()) stop("Please run install_packages() first")

  suppressPackageStartupMessages({
    library(nflreadr)
    library(httr)
    library(jsonlite)
    library(dplyr)
    library(lubridate)
    library(tidyr)
  })
}

setup_kalshi <- function(email, password) {
  Sys.setenv(KALSHI_EMAIL = email)
  Sys.setenv(KALSHI_PASSWORD = password)
  cat("✓ Kalshi credentials set for this session\n")
}

N_SIMULATIONS <- 10000
CONFIDENCE_LEVEL <- 0.95

get_upcoming_games <- function(days_ahead = 7) {
  cat("Fetching NFL schedule...\n")

  current_year <- year(Sys.Date())
  current_month <- month(Sys.Date())
  season <- ifelse(current_month >= 9, current_year, current_year - 1)

  schedule <- load_schedules(seasons = season)

  today <- Sys.Date()
  target_date <- today + days(days_ahead)

  upcoming <- schedule %>%
    filter(
      gameday >= today,
      gameday <= target_date,
      !is.na(home_team),
      !is.na(away_team)
    ) %>%
    arrange(gameday, gametime)

  cat(sprintf("Found %d games in the next %d days\n", nrow(upcoming), days_ahead))
  return(upcoming)
}

get_kickoff_datetime <- function(game_row, tz = "UTC") {
  if ("game_datetime" %in% names(game_row) && !is.na(game_row$game_datetime)) {
    dt <- game_row$game_datetime
    if (!inherits(dt, "POSIXct")) dt <- as.POSIXct(dt, tz = tz)
    return(dt)
  }

  gd <- game_row$gameday
  gt <- game_row$gametime

  if (is.na(gd)) return(as.POSIXct(Sys.Date(), tz = tz))

  if (!is.na(gt) && nchar(as.character(gt)) >= 3) {
    dt_str <- paste(as.character(gd), as.character(gt))
    dt <- suppressWarnings(as.POSIXct(dt_str, tz = tz))
    if (!is.na(dt)) return(dt)
  }

  return(as.POSIXct(paste(as.character(gd), "12:00:00"), tz = tz))
}

infer_injury_datetime_col <- function(injuries_df) {
  candidate_cols <- c(
    "report_date", "report_datetime",
    "practice_date", "practice_datetime",
    "status_date", "status_datetime",
    "date", "datetime",
    "last_updated", "last_modified", "modified", "updated"
  )
  present <- candidate_cols[candidate_cols %in% names(injuries_df)]
  if (length(present) == 0) return(NULL)
  return(present[1])
}

coerce_to_posix <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = tz))
  out <- suppressWarnings(as.POSIXct(x, tz = tz))
  if (all(is.na(out))) {
    out <- suppressWarnings(as.POSIXct(as.Date(x), tz = tz))
  }
  return(out)
}

get_epa_metrics <- function(season,
                            weeks_back = 4,
                            cutoff_date = NULL,
                            through_week = NULL) {
  cat("Calculating EPA-based advanced metrics...\n")

  pbp <- load_pbp(seasons = season)

  if (is.null(cutoff_date)) {
    if (!is.null(through_week)) {
      pbp <- pbp %>% filter(!is.na(week), week < through_week)
    } else {
      cutoff_date <- Sys.Date()
    }
  }

  if (!is.null(cutoff_date)) {
    if ("game_date" %in% names(pbp)) {
      pbp <- pbp %>% filter(!is.na(game_date), as.Date(game_date) < as.Date(cutoff_date))
    } else {
      max_week_safe <- suppressWarnings(max(pbp$week[!is.na(pbp$week)], na.rm = TRUE))
      pbp <- pbp %>% filter(!is.na(week), week <= max_week_safe)
    }
  }

  if (nrow(pbp) == 0) {
    warning("No play-by-play rows available before cutoff; returning empty EPA metrics.")
    return(data.frame())
  }

  max_week <- suppressWarnings(max(pbp$week, na.rm = TRUE))
  recent_start <- max(1, max_week - weeks_back + 1)

  pbp_recent <- pbp %>% filter(!is.na(week), week >= recent_start, week <= max_week)

  offense_epa <- pbp_recent %>%
    filter(!is.na(posteam), !is.na(epa)) %>%
    group_by(posteam) %>%
    summarise(
      epa_per_play = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      pass_epa = mean(epa[pass == 1], na.rm = TRUE),
      rush_epa = mean(epa[rush == 1], na.rm = TRUE),
      pass_rate = mean(pass, na.rm = TRUE),
      explosive_play_rate = mean(yards_gained >= 15, na.rm = TRUE),
      third_down_epa = mean(epa[down == 3], na.rm = TRUE),
      third_down_success = mean(success[down == 3], na.rm = TRUE),
      red_zone_epa = mean(epa[yardline_100 <= 20], na.rm = TRUE),
      close_game_epa = mean(epa[abs(score_differential) <= 7], na.rm = TRUE),
      fourth_quarter_epa = mean(epa[qtr == 4], na.rm = TRUE),
      total_plays = n(),
      .groups = "drop"
    )

  defense_epa <- pbp_recent %>%
    filter(!is.na(defteam), !is.na(epa)) %>%
    group_by(defteam) %>%
    summarise(
      def_epa_per_play = mean(epa, na.rm = TRUE),
      def_success_rate = mean(success, na.rm = TRUE),
      def_pass_epa = mean(epa[pass == 1], na.rm = TRUE),
      def_rush_epa = mean(epa[rush == 1], na.rm = TRUE),
      def_explosive_rate = mean(yards_gained >= 15, na.rm = TRUE),
      def_third_down_success = mean(success[down == 3], na.rm = TRUE),
      sacks = sum(sack, na.rm = TRUE),
      turnovers_forced = sum(interception, na.rm = TRUE) + sum(fumble_lost, na.rm = TRUE),
      .groups = "drop"
    )

  epa_metrics <- offense_epa %>%
    left_join(defense_epa, by = c("posteam" = "defteam")) %>%
    rename(team = posteam) %>%
    mutate(
      offensive_power = scale(epa_per_play)[,1] +
        scale(success_rate)[,1] +
        scale(explosive_play_rate)[,1],
      defensive_power = -scale(def_epa_per_play)[,1] -
        scale(def_success_rate)[,1] +
        scale(turnovers_forced)[,1],
      overall_epa_rating = offensive_power + defensive_power,
      clutch_rating = scale(third_down_epa)[,1] +
        scale(red_zone_epa)[,1] +
        scale(close_game_epa)[,1]
    )

  cat(sprintf("✓ Calculated EPA metrics for %d teams\n", nrow(epa_metrics)))
  return(epa_metrics)
}

get_injury_data <- function(season = NULL) {
  if (is.null(season)) {
    current_year <- year(Sys.Date())
    current_month <- month(Sys.Date())
    season <- ifelse(current_month >= 9, current_year, current_year - 1)
  }

  cat("Loading injury reports...\n")
  injuries <- NULL

  tryCatch({
    injuries <- load_injuries(seasons = season)
    cat(sprintf("✓ Loaded injury data for %d season\n", season))
  }, error = function(e) {
    cat(sprintf("Note: %d injury data not available, trying %d...\n", season, season - 1))
    tryCatch({
      injuries <<- load_injuries(seasons = season - 1)
      cat(sprintf("✓ Using %d season injury data as reference\n", season - 1))
    }, error = function(e2) {
      cat("Note: Running without injury data (minimal impact)\n")
      injuries <<- data.frame()
    })
  })

  if (is.null(injuries) || nrow(injuries) == 0) return(data.frame())
  return(injuries)
}

process_injuries <- function(injuries_raw) {
  if (is.null(injuries_raw) || nrow(injuries_raw) == 0) return(data.frame())

  status_col <- NULL
  if ("report_status" %in% names(injuries_raw)) status_col <- "report_status"
  if (is.null(status_col) && "status" %in% names(injuries_raw)) status_col <- "status"
  if (is.null(status_col) && "injury_status" %in% names(injuries_raw)) status_col <- "injury_status"

  if (is.null(status_col)) {
    warning("Injury table has no recognized status column (report_status/status/injury_status).")
    return(data.frame())
  }

  injuries_processed <- injuries_raw %>%
    filter(!is.na(team)) %>%
    mutate(
      report_status_std = .data[[status_col]],
      injury_severity = case_when(
        report_status_std == "Out" ~ 1.0,
        report_status_std == "Doubtful" ~ 0.8,
        report_status_std == "Questionable" ~ 0.4,
        report_status_std == "Probable" ~ 0.1,
        TRUE ~ 0
      ),
      position_weight = case_when(
        position == "QB" ~ 3.0,
        position %in% c("LT", "C", "RT") ~ 1.5,
        position %in% c("WR1", "WR") ~ 1.3,
        position %in% c("EDGE", "DT") ~ 1.3,
        position %in% c("CB", "S") ~ 1.2,
        position %in% c("RB", "TE") ~ 1.0,
        TRUE ~ 0.8
      ),
      impact_score = injury_severity * position_weight
    )

  return(injuries_processed)
}

get_injuries_asof <- function(injuries_processed, cutoff_datetime) {
  if (is.null(injuries_processed) || nrow(injuries_processed) == 0) return(data.frame())
  dt_col <- infer_injury_datetime_col(injuries_processed)

  if (is.null(dt_col)) {
    warning("No injury datetime column found; disabling injury adjustments for strict timing.")
    return(data.frame())
  }

  tmp <- injuries_processed
  tmp$.__inj_dt <- coerce_to_posix(tmp[[dt_col]])

  cutoff_datetime <- coerce_to_posix(cutoff_datetime)
  tmp <- tmp %>% filter(!is.na(.__inj_dt), .__inj_dt <= cutoff_datetime)
  tmp$.__inj_dt <- NULL

  return(tmp)
}

calculate_injury_impact <- function(team, week, injuries_asof) {
  if (is.null(injuries_asof) || nrow(injuries_asof) == 0) {
    return(list(impact_score = 0, starters_out = 0))
  }

  if (!is.null(week) && "week" %in% names(injuries_asof)) {
    team_injuries <- injuries_asof %>% filter(team == !!team, week == !!week)
  } else {
    team_injuries <- injuries_asof %>% filter(team == !!team)
  }

  if (nrow(team_injuries) == 0) return(list(impact_score = 0, starters_out = 0))

  total_impact <- sum(team_injuries$impact_score, na.rm = TRUE)

  starters_out <- 0
  if ("report_status_std" %in% names(team_injuries)) {
    starters_out <- sum(team_injuries$report_status_std == "Out" &
                          team_injuries$position_weight >= 1.0, na.rm = TRUE)
  }

  return(list(impact_score = total_impact, starters_out = starters_out))
}

calculate_rest_travel <- function(schedule, upcoming_games) {
  cat("Calculating rest and travel advantages...\n")

  rest_data <- data.frame()

  for (i in 1:nrow(upcoming_games)) {
    game <- upcoming_games[i,]

    home_prev <- schedule %>%
      filter((home_team == game$home_team | away_team == game$home_team),
             gameday < game$gameday) %>%
      arrange(desc(gameday)) %>%
      slice(1)

    away_prev <- schedule %>%
      filter((home_team == game$away_team | away_team == game$away_team),
             gameday < game$gameday) %>%
      arrange(desc(gameday)) %>%
      slice(1)

    home_rest <- ifelse(nrow(home_prev) > 0,
                        as.numeric(difftime(game$gameday, home_prev$gameday, units = "days")),
                        7)

    away_rest <- ifelse(nrow(away_prev) > 0,
                        as.numeric(difftime(game$gameday, away_prev$gameday, units = "days")),
                        7)

    away_travel_distance <- estimate_travel_distance(game$away_team, game$home_team)
    is_thursday <- wday(game$gameday) == 5

    rest_data <- rbind(rest_data, data.frame(
      game_id = ifelse(!is.null(game$game_id), game$game_id, paste0(game$home_team, game$away_team)),
      home_team = game$home_team,
      away_team = game$away_team,
      home_rest_days = home_rest,
      away_rest_days = away_rest,
      rest_advantage = home_rest - away_rest,
      away_travel_distance = away_travel_distance,
      is_short_week = is_thursday,
      is_division_game = check_division_game(game$home_team, game$away_team)
    ))
  }

  cat(sprintf("✓ Calculated rest/travel for %d games\n", nrow(rest_data)))
  return(rest_data)
}

estimate_travel_distance <- function(away_team, home_team) {
  east_teams <- c("BUF", "MIA", "NE", "NYJ", "BAL", "CIN", "CLE", "PIT",
                  "ATL", "CAR", "NO", "TB", "DAL", "NYG", "PHI", "WAS")
  west_teams <- c("DEN", "KC", "LV", "LAC", "ARI", "LAR", "SF", "SEA")

  if ((away_team %in% east_teams && home_team %in% west_teams) ||
      (away_team %in% west_teams && home_team %in% east_teams)) {
    return(2500)
  } else if (away_team %in% west_teams && home_team %in% west_teams) {
    return(1000)
  } else if (away_team %in% east_teams && home_team %in% east_teams) {
    return(500)
  } else {
    return(1500)
  }
}

check_division_game <- function(team1, team2) {
  divisions <- list(
    afc_east = c("BUF", "MIA", "NE", "NYJ"),
    afc_north = c("BAL", "CIN", "CLE", "PIT"),
    afc_south = c("HOU", "IND", "JAX", "TEN"),
    afc_west = c("DEN", "KC", "LV", "LAC"),
    nfc_east = c("DAL", "NYG", "PHI", "WAS"),
    nfc_north = c("CHI", "DET", "GB", "MIN"),
    nfc_south = c("ATL", "CAR", "NO", "TB"),
    nfc_west = c("ARI", "LAR", "SF", "SEA")
  )

  for (division in divisions) {
    if (team1 %in% division && team2 %in% division) return(TRUE)
  }
  return(FALSE)
}

get_weather_forecast <- function(home_team, game_date, api_key = NULL) {
  if (is.null(api_key) || api_key == "") {
    return(list(temperature = 65, wind_speed = 5, precipitation = 0, weather_impact = 0))
  }

  stadium_coords <- list(
    KC = list(lat = 39.0489, lon = -94.4839),
    BUF = list(lat = 42.7738, lon = -78.7870),
    GB = list(lat = 44.5013, lon = -88.0622),
    DEFAULT = list(lat = 40.0, lon = -95.0)
  )

  coords <- stadium_coords[[home_team]]
  if (is.null(coords)) coords <- stadium_coords$DEFAULT

  tryCatch({
    url <- sprintf(
      "https://api.openweathermap.org/data/2.5/forecast?lat=%f&lon=%f&appid=%s&units=imperial",
      coords$lat, coords$lon, api_key
    )

    response <- GET(url)

    if (status_code(response) == 200) {
      weather_data <- content(response, "parsed")
      forecast <- weather_data$list[[1]]$main
      wind <- weather_data$list[[1]]$wind

      temp <- forecast$temp
      wind_speed <- wind$speed

      impact <- 0
      if (wind_speed > 15) impact <- impact + (wind_speed - 15) * 0.1
      if (temp < 20 || temp > 90) impact <- impact + 0.3

      return(list(
        temperature = temp,
        wind_speed = wind_speed,
        precipitation = forecast$humidity > 80,
        weather_impact = impact
      ))
    }
  }, error = function(e) {
    warning("Weather API error: ", e$message)
  })

  return(list(temperature = 65, wind_speed = 5, precipitation = 0, weather_impact = 0))
}

calculate_matchup_advantage <- function(home_team, away_team, epa_metrics) {
  home_stats <- epa_metrics %>% filter(team == home_team)
  away_stats <- epa_metrics %>% filter(team == away_team)

  if (nrow(home_stats) == 0 || nrow(away_stats) == 0) return(0)

  home_pass_advantage <- home_stats$pass_epa - away_stats$def_pass_epa
  away_pass_advantage <- away_stats$pass_epa - home_stats$def_pass_epa
  home_rush_advantage <- home_stats$rush_epa - away_stats$def_rush_epa
  away_rush_advantage <- away_stats$rush_epa - home_stats$def_rush_epa
  home_third_down <- home_stats$third_down_epa - away_stats$def_third_down_success
  away_third_down <- away_stats$third_down_epa - home_stats$def_third_down_success

  matchup_score <- (
    home_pass_advantage * 0.4 +
      home_rush_advantage * 0.2 +
      home_third_down * 0.3 +
      (home_stats$clutch_rating - away_stats$clutch_rating) * 0.1
  ) - (
    away_pass_advantage * 0.4 +
      away_rush_advantage * 0.2 +
      away_third_down * 0.3
  )

  return(matchup_score)
}

calculate_win_probability <- function(home_team, away_team,
                                      epa_metrics, injuries_asof,
                                      rest_travel, weather = NULL,
                                      week = NULL,
                                      n_sims = 10000) {

  home_epa <- epa_metrics %>% filter(team == home_team)
  away_epa <- epa_metrics %>% filter(team == away_team)

  if (nrow(home_epa) == 0 || nrow(away_epa) == 0) {
    warning(sprintf("Missing EPA data for %s vs %s", home_team, away_team))
    return(list(home_win_prob = 0.5, away_win_prob = 0.5, expected_margin = 0))
  }

  base_diff <- (home_epa$overall_epa_rating - away_epa$overall_epa_rating) * 2.0
  home_advantage <- 2.5

  injury_adjustment <- 0
  if (!is.null(injuries_asof) && nrow(injuries_asof) > 0) {
    home_injury_impact <- calculate_injury_impact(home_team, week, injuries_asof)
    away_injury_impact <- calculate_injury_impact(away_team, week, injuries_asof)

    injury_adjustment <- -(home_injury_impact$impact_score - away_injury_impact$impact_score) * 1.0

    if (home_injury_impact$impact_score > 0 || away_injury_impact$impact_score > 0) {
      cat(sprintf("  Injury impact: Home %.1f, Away %.1f\n",
                  home_injury_impact$impact_score, away_injury_impact$impact_score))
    }
  }

  rest_adjustment <- 0
  if (!is.null(rest_travel) && nrow(rest_travel) > 0) {
    game_rest <- rest_travel %>% filter(home_team == !!home_team, away_team == !!away_team)
    if (nrow(game_rest) > 0) {
      rest_adjustment <- game_rest$rest_advantage * 0.5
      if (game_rest$away_travel_distance > 2000) rest_adjustment <- rest_adjustment + 1.0
      if (game_rest$is_short_week) rest_adjustment <- rest_adjustment + 0.5
      cat(sprintf("  Rest advantage: %.1f days (%.1f point impact)\n",
                  game_rest$rest_advantage, rest_adjustment))
    }
  }

  weather_adjustment <- 0
  if (!is.null(weather) && weather$weather_impact > 0) {
    if (home_epa$pass_rate > 0.6) weather_adjustment <- weather_adjustment - weather$weather_impact
    if (away_epa$pass_rate > 0.6) weather_adjustment <- weather_adjustment + weather$weather_impact
    cat(sprintf("  Weather impact: %.1f points\n", weather_adjustment))
  }

  matchup_adjustment <- calculate_matchup_advantage(home_team, away_team, epa_metrics) * 0.8

  expected_diff <- base_diff + home_advantage + injury_adjustment +
    rest_adjustment + weather_adjustment + matchup_adjustment

  base_sd <- 13.5
  sd_adjustment <- 1.0
  if (!is.null(home_epa$total_plays) && home_epa$total_plays > 0) {
    sd_adjustment <- (home_epa$explosive_play_rate + away_epa$explosive_play_rate) / 0.2
    sd_adjustment <- max(0.8, min(1.2, sd_adjustment))
  }
  sd_points <- base_sd * sd_adjustment

  simulated_margins <- rnorm(n_sims, mean = expected_diff, sd = sd_points)

  home_wins <- sum(simulated_margins > 0) / n_sims
  away_wins <- 1 - home_wins

  ci <- quantile(simulated_margins, probs = c(0.025, 0.975))

  blowout_prob_home <- sum(simulated_margins > 14) / n_sims
  blowout_prob_away <- sum(simulated_margins < -14) / n_sims
  close_game_prob <- sum(abs(simulated_margins) <= 7) / n_sims

  return(list(
    home_win_prob = home_wins,
    away_win_prob = away_wins,
    expected_margin = expected_diff,
    median_margin = median(simulated_margins),
    confidence_interval = ci,
    simulated_margins = simulated_margins,
    blowout_prob_home = blowout_prob_home,
    blowout_prob_away = blowout_prob_away,
    close_game_prob = close_game_prob,
    factors = list(
      base_diff = base_diff,
      home_advantage = home_advantage,
      injury_adjustment = injury_adjustment,
      rest_adjustment = rest_adjustment,
      weather_adjustment = weather_adjustment,
      matchup_adjustment = matchup_adjustment
    )
  ))
}

run_analysis <- function(days_ahead = 7, weather_api_key = NULL) {
  cat("\n========================================\n")
  cat("NFL BETTING ANALYSIS\n")
  cat("Enhanced with High-Value Data\n")
  cat("========================================\n\n")

  load_libraries()

  current_year <- year(Sys.Date())
  current_month <- month(Sys.Date())
  season <- ifelse(current_month >= 9, current_year, current_year - 1)

  cat(sprintf("Season: %d\n\n", season))

  cat("STEP 1: Loading data sources...\n")
  cat("--------------------------------\n")

  schedule <- load_schedules(seasons = season)
  upcoming_games <- schedule %>%
    filter(
      gameday >= Sys.Date(),
      gameday <= Sys.Date() + days(days_ahead),
      !is.na(home_team),
      !is.na(away_team)
    ) %>%
    arrange(gameday, gametime)

  if (nrow(upcoming_games) == 0) {
    cat("No games found in the next", days_ahead, "days.\n")
    return(invisible(NULL))
  }

  cat(sprintf("Found %d upcoming games\n", nrow(upcoming_games)))

  epa_metrics <- get_epa_metrics(season, weeks_back = 4, cutoff_date = Sys.Date())

  injuries_raw <- get_injury_data(season)
  injuries_processed <- process_injuries(injuries_raw)

  rest_travel <- calculate_rest_travel(schedule, upcoming_games)

  cat("\n")
  cat("STEP 2: Running predictions...\n")
  cat("-------------------------------\n\n")

  predictions <- data.frame()

  for (i in 1:nrow(upcoming_games)) {
    game <- upcoming_games[i,]
    cat(sprintf("Analyzing: %s @ %s\n", game$away_team, game$home_team))

    kickoff_dt <- get_kickoff_datetime(game, tz = "UTC")
    cutoff_dt <- min(kickoff_dt, as.POSIXct(Sys.time(), tz = "UTC"))
    injuries_asof <- get_injuries_asof(injuries_processed, cutoff_dt)

    weather <- NULL
    if (!is.null(weather_api_key)) {
      weather <- get_weather_forecast(game$home_team, game$gameday, weather_api_key)
    }

    result <- calculate_win_probability(
      home_team = game$home_team,
      away_team = game$away_team,
      epa_metrics = epa_metrics,
      injuries_asof = injuries_asof,
      rest_travel = rest_travel,
      weather = weather,
      week = game$week,
      n_sims = N_SIMULATIONS
    )

    predictions <- rbind(predictions, data.frame(
      gameday = game$gameday,
      week = game$week,
      home_team = game$home_team,
      away_team = game$away_team,
      home_win_prob = result$home_win_prob,
      away_win_prob = result$away_win_prob,
      expected_margin = result$expected_margin,
      ci_lower = result$confidence_interval[1],
      ci_upper = result$confidence_interval[2],
      close_game_prob = result$close_game_prob,
      blowout_prob_home = result$blowout_prob_home,
      blowout_prob_away = result$blowout_prob_away,
      edge = abs(result$home_win_prob - 0.5)
    ))

    cat("\n")
  }

  cat("\n========================================\n")
  cat("BETTING RECOMMENDATIONS\n")
  cat("========================================\n\n")

  predictions <- predictions %>% arrange(desc(edge))

  for (i in 1:nrow(predictions)) {
    game <- predictions[i,]
    cat(sprintf("%s @ %s (Week %d, %s)\n",
                game$away_team, game$home_team, game$week, game$gameday))

    if (game$expected_margin > 0) {
      cat(sprintf("  Pick: %s (%.1f%% win probability)\n",
                  game$home_team, game$home_win_prob * 100))
      cat(sprintf("  Expected margin: %s by %.1f points\n",
                  game$home_team, abs(game$expected_margin)))
    } else {
      cat(sprintf("  Pick: %s (%.1f%% win probability)\n",
                  game$away_team, game$away_win_prob * 100))
      cat(sprintf("  Expected margin: %s by %.1f points\n",
                  game$away_team, abs(game$expected_margin)))
    }

    cat(sprintf("  Edge: %.1f%%\n", game$edge * 100))
    cat(sprintf("  Close game probability: %.1f%%\n", game$close_game_prob * 100))

    if (game$edge >= 0.15) {
      cat("  🔥 STRONG BET - High edge detected\n")
    } else if (game$edge >= 0.10) {
      cat("  ✓ GOOD BET - Solid edge\n")
    } else if (game$edge >= 0.05) {
      cat("  → MODERATE BET - Small edge\n")
    } else {
      cat("  ○ PASS - Insufficient edge\n")
    }

    cat("\n")
  }

  output_file <- sprintf("nfl_predictions_%s.csv", Sys.Date())
  write.csv(predictions, output_file, row.names = FALSE)
  cat(sprintf("Predictions saved to: %s\n\n", output_file))

  return(predictions)
}

analyze_game <- function(home_team, away_team, week = NULL,
                         weather_api_key = NULL) {
  load_libraries()

  current_year <- year(Sys.Date())
  current_month <- month(Sys.Date())
  season <- ifelse(current_month >= 9, current_year, current_year - 1)

  cat(sprintf("\nANALYZING: %s @ %s\n", away_team, home_team))
  cat("========================================\n\n")

  cat("Loading data sources...\n")

  epa_metrics <- get_epa_metrics(season, weeks_back = 4, cutoff_date = Sys.Date())

  injuries_raw <- get_injury_data(season)
  injuries_processed <- process_injuries(injuries_raw)

  schedule <- load_schedules(seasons = season)
  rest_travel <- data.frame()

  weather <- NULL
  if (!is.null(weather_api_key)) {
    weather <- get_weather_forecast(home_team, Sys.Date(), weather_api_key)
    cat(sprintf("\nWeather: %d°F, Wind: %.1f mph\n",
                weather$temperature, weather$wind_speed))
  }

  injuries_asof <- get_injuries_asof(injuries_processed, as.POSIXct(Sys.time(), tz = "UTC"))

  result <- calculate_win_probability(
    home_team, away_team,
    epa_metrics = epa_metrics,
    injuries_asof = injuries_asof,
    rest_travel = rest_travel,
    weather = weather,
    week = week,
    n_sims = N_SIMULATIONS
  )

  cat("\n========================================\n")
  cat("PREDICTION RESULTS\n")
  cat("========================================\n")

  cat(sprintf("%s (HOME): %.1f%% win probability\n",
              home_team, result$home_win_prob * 100))
  cat(sprintf("%s (AWAY): %.1f%% win probability\n",
              away_team, result$away_win_prob * 100))

  if (result$expected_margin > 0) {
    cat(sprintf("Expected margin: %s by %.1f points (HOME FAVORED)\n",
                home_team, abs(result$expected_margin)))
  } else {
    cat(sprintf("Expected margin: %s by %.1f points (AWAY FAVORED)\n",
                away_team, abs(result$expected_margin)))
  }

  cat(sprintf("95%% CI: [%.1f, %.1f]\n",
              result$confidence_interval[1],
              result$confidence_interval[2]))

  cat("\nGame Probabilities:\n")
  cat(sprintf("  Close game (±7): %.1f%%\n", result$close_game_prob * 100))
  cat(sprintf("  %s blowout (14+): %.1f%%\n",
              home_team, result$blowout_prob_home * 100))
  cat(sprintf("  %s blowout (14+): %.1f%%\n",
              away_team, result$blowout_prob_away * 100))

  cat("\nFactors Breakdown:\n")
  cat(sprintf("  Base EPA differential: %.1f\n", result$factors$base_diff))
  cat(sprintf("  Home advantage: %.1f\n", result$factors$home_advantage))
  cat(sprintf("  Injury impact: %.1f\n", result$factors$injury_adjustment))
  cat(sprintf("  Rest/travel: %.1f\n", result$factors$rest_adjustment))
  cat(sprintf("  Weather: %.1f\n", result$factors$weather_adjustment))
  cat(sprintf("  Matchup advantage: %.1f\n", result$factors$matchup_adjustment))

  cat("\nBETTING RECOMMENDATION:\n")
  max_prob <- max(result$home_win_prob, result$away_win_prob)
  favorite <- ifelse(result$expected_margin > 0, home_team, away_team)
  edge <- max_prob - 0.5

  if (max_prob >= 0.65) {
    cat(sprintf("✓ STRONG BET: %s (%.1f%% edge)\n", favorite, edge * 100))
  } else if (max_prob >= 0.55) {
    cat(sprintf("→ MODERATE BET: %s (%.1f%% edge)\n", favorite, edge * 100))
  } else {
    cat(sprintf("○ TOO CLOSE TO CALL (%.1f%% edge)\n", edge * 100))
  }

  if (edge > 0.05) {
    kelly_fraction <- edge * 2 * 0.25
    cat(sprintf("Suggested bet size: %.1f%% of bankroll\n", kelly_fraction * 100))
  }

  cat("\n========================================\n\n")
  return(result)
}

backtest_season <- function(season = 2024, start_week = 5, end_week = 18) {
  cat("\n========================================\n")
  cat(sprintf("BACKTESTING %d SEASON\n", season))
  cat(sprintf("Weeks %d-%d\n", start_week, end_week))
  cat("========================================\n\n")

  load_libraries()

  cat("Loading season schedule...\n")
  schedule <- load_schedules(seasons = season)

  injuries_raw <- get_injury_data(season)
  injuries_processed <- process_injuries(injuries_raw)

  all_results <- data.frame()

  for (week in start_week:end_week) {
    cat(sprintf("\n--- WEEK %d ---\n", week))

    week_games <- schedule %>%
      filter(week == !!week, !is.na(result), !is.na(home_team), !is.na(away_team))

    if (nrow(week_games) == 0) {
      cat("No completed games found\n")
      next
    }

    epa_metrics <- get_epa_metrics(season, weeks_back = 4, through_week = week)

    schedule_before <- schedule %>% filter(week < !!week)
    rest_travel <- calculate_rest_travel(schedule_before, week_games)

    for (i in 1:nrow(week_games)) {
      game <- week_games[i,]
      if (is.na(game$result)) next

      cat(sprintf("  %s @ %s... ", game$away_team, game$home_team))

      tryCatch({
        kickoff_dt <- get_kickoff_datetime(game, tz = "UTC")
        injuries_asof <- get_injuries_asof(injuries_processed, kickoff_dt)

        prediction <- calculate_win_probability(
          home_team = game$home_team,
          away_team = game$away_team,
          epa_metrics = epa_metrics,
          injuries_asof = injuries_asof,
          rest_travel = rest_travel,
          weather = NULL,
          week = week,
          n_sims = N_SIMULATIONS
        )

        actual_home_won <- game$result > 0
        actual_margin <- game$result

        predicted_home_win <- prediction$home_win_prob > 0.5
        predicted_margin <- prediction$expected_margin

        correct_winner <- (actual_home_won == predicted_home_win)
        margin_error <- abs(actual_margin - predicted_margin)

        edge <- abs(prediction$home_win_prob - 0.5)
        would_bet <- edge > 0.05

        all_results <- rbind(all_results, data.frame(
          season = season,
          week = week,
          game_id = game$game_id,
          home_team = game$home_team,
          away_team = game$away_team,
          predicted_home_prob = prediction$home_win_prob,
          predicted_margin = predicted_margin,
          actual_margin = actual_margin,
          actual_home_won = actual_home_won,
          predicted_home_win = predicted_home_win,
          correct_winner = correct_winner,
          margin_error = margin_error,
          edge = edge,
          would_bet = would_bet,
          model_confident = edge > 0.10
        ))

        cat(ifelse(correct_winner, "✓", "✗"), "\n")
      }, error = function(e) {
        cat(sprintf("Error: %s\n", e$message))
      })
    }
  }

  cat("\n========================================\n")
  cat("BACKTEST RESULTS\n")
  cat("========================================\n\n")

  total_games <- nrow(all_results)
  if (total_games == 0) {
    cat("No games evaluated.\n")
    return(invisible(NULL))
  }

  correct_predictions <- sum(all_results$correct_winner)
  accuracy <- correct_predictions / total_games

  cat(sprintf("Total games analyzed: %d\n", total_games))
  cat(sprintf("Correct predictions: %d / %d (%.1f%%)\n",
              correct_predictions, total_games, accuracy * 100))

  bet_games <- all_results %>% filter(would_bet)
  if (nrow(bet_games) > 0) {
    bet_accuracy <- sum(bet_games$correct_winner) / nrow(bet_games)
    cat(sprintf("\nGames with >5%% edge: %d\n", nrow(bet_games)))
    cat(sprintf("Accuracy on bet games: %.1f%%\n", bet_accuracy * 100))
  }

  confident_games <- all_results %>% filter(model_confident)
  if (nrow(confident_games) > 0) {
    confident_accuracy <- sum(confident_games$correct_winner) / nrow(confident_games)
    cat(sprintf("\nGames with >10%% edge: %d\n", nrow(confident_games)))
    cat(sprintf("Accuracy on confident bets: %.1f%%\n", confident_accuracy * 100))
  }

  avg_margin_error <- mean(all_results$margin_error)
  cat(sprintf("\nAverage margin error: %.1f points\n", avg_margin_error))

  all_results$brier_score <- (all_results$predicted_home_prob - as.numeric(all_results$actual_home_won))^2
  brier <- mean(all_results$brier_score)
  cat(sprintf("Brier score: %.3f (lower is better, <0.25 is good)\n", brier))

  if (nrow(bet_games) > 0) {
    cat("\n--- SIMULATED BETTING ROI ---\n")
    cat("(Assuming -110 odds, betting on games with >5% edge)\n")

    wins <- sum(bet_games$correct_winner)
    losses <- nrow(bet_games) - wins

    total_risked <- nrow(bet_games) * 110
    winnings <- wins * 100
    losses_amount <- losses * 110
    net_profit <- winnings - losses_amount
    roi <- (net_profit / total_risked) * 100

    cat(sprintf("Bets placed: %d\n", nrow(bet_games)))
    cat(sprintf("Wins: %d, Losses: %d\n", wins, losses))
    cat(sprintf("Win rate: %.1f%% (need 52.4%% to break even)\n",
                (wins / nrow(bet_games)) * 100))
    cat(sprintf("ROI: %.1f%%\n", roi))

    if (roi > 0) cat("✓ PROFITABLE: Model would have made money\n")
    else cat("✗ UNPROFITABLE: Model would have lost money\n")
  }

  output_file <- sprintf("backtest_results_%d_weeks_%d_%d.csv", season, start_week, end_week)
  write.csv(all_results, output_file, row.names = FALSE)
  cat(sprintf("\nDetailed results saved to: %s\n", output_file))

  return(all_results)
}

backtest_last_season <- function() {
  return(backtest_season(season = 2024, start_week = 5, end_week = 18))
}

compare_to_market <- function(backtest_results, vegas_lines = NULL) {
  cat("\n========================================\n")
  cat("MODEL vs MARKET COMPARISON\n")
  cat("========================================\n\n")

  if (is.null(vegas_lines)) {
    cat("To compare against market, provide Vegas lines data\n")
    cat("Example format:\n")
    cat("  vegas_lines <- data.frame(\n")
    cat("    game_id = c(...),\n")
    cat("    vegas_line = c(...),\n")
    cat("    vegas_home_prob = c(...)\n")
    cat("  )\n\n")
    return(invisible(NULL))
  }

  cat("Comparison analysis would go here\n")
}

list_upcoming_games <- function(days_ahead = 7) {
  load_libraries()

  games <- get_upcoming_games(days_ahead)

  if (nrow(games) == 0) {
    cat("No games found in the next", days_ahead, "days.\n")
    return(invisible(NULL))
  }

  cat("\n========================================\n")
  cat(sprintf("UPCOMING GAMES (Next %d days)\n", days_ahead))
  cat("========================================\n\n")

  for (i in 1:nrow(games)) {
    game <- games[i,]
    cat(sprintf("%s @ %s\n", game$away_team, game$home_team))
    cat(sprintf("  Date: %s\n\n", game$gameday))
  }

  return(games)
}

cat("\n")
cat("╔════════════════════════════════════════╗\n")
cat("║  NFL BETTING MODEL v2.1 - LOADED      ║\n")
cat("╚════════════════════════════════════════╝\n\n")
cat("Features:\n")
cat("  • EPA-based advanced metrics\n")
cat("  • Official injury reports\n")
cat("  • Rest and travel calculations\n")
cat("  • Matchup-specific analysis\n")
cat("  • Weather integration (optional)\n")
cat("  • Realistic predictions\n")
cat("  • Backtesting on historical data\n\n")
cat("Usage:\n")
cat("  predictions <- run_analysis()           # Analyze upcoming games\n")
cat("  analyze_game('KC', 'BUF')              # Analyze specific game\n")
cat("  list_upcoming_games()                  # See schedule\n\n")
cat("Backtesting:\n")
cat("  results <- backtest_last_season()      # Test on 2024 season\n")
cat("  results <- backtest_season(2023)       # Test on 2023 season\n\n")
cat("For weather data (optional):\n")
cat("  predictions <- run_analysis(weather_api_key = 'YOUR_KEY')\n")
cat("  Free key: https://openweathermap.org/api\n\n")
