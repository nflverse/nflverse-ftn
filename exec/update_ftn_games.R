pkgload::load_all()

update_ftn_game_ids <- function(){

  cli::cli_alert_info("Fetching FTN's game listing")
  schedule <- ftn_schedule()
  cli::cli_alert_info("Fetching previously pulled data")
  current_data <- nflreadr::csv_from_url("https://github.com/nflverse/nflverse-ftn/releases/download/raw/ftn_game_id_mapping.csv")

  # grab nflverse_id <-> ftn_id mapping for 2022+ seasons
  game_ids <- schedule[
    season >= 2022,
    c("nflverse_game_id", "ftn_game_id")
  ][
    order(ftn_game_id, decreasing = TRUE)
  ]

  if(nrow(game_ids) == 0 | anyNA(game_ids$nflverse_game_id) | anyNA(game_ids$ftn_game_id)) {
    cli::cli_alert_danger("Something went wrong, exiting...")
    return(invisible(FALSE))
  }

  if (all(current_data$ftn_game_id %in% game_ids$ftn_game_id)){
    cli::cli_alert_success("No new game IDs, exiting...")
    return(invisible(FALSE))
  }

  cli::cli_alert_info("Going to upload ID mapping of {cli::no(nrow(game_ids))} games")
  nflversedata::nflverse_save(
    data_frame = game_ids,
    file_name = "ftn_game_id_mapping",
    nflverse_type = "FTN <-> nflverse game id mapping",
    release_tag = "raw",
    file_types = "csv",
    repo = "nflverse/nflverse-ftn"
  )
  cli::cli_alert_success("Job complete")

  return(invisible(TRUE))
}

update_ftn_game_ids()
