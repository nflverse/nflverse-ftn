pkgload::load_all()

update_ftn <- function(){

  cli::cli_alert_info("Fetching FTN's game listing")
  schedule <- ftn_schedule()[!is.na(updated)]
  cli::cli_alert_info("Fetching previously pulled data")
  current_data <- nflreadr::rds_from_url("https://github.com/nflverse/nflverse-ftn/releases/download/raw/full_ftn_data.rds")

  games_to_update <- merge(
    # Game ID 6134 is 2022_17_BUF_CIN which was cancelled after Damar Hamlin’s cardiac arrest
    schedule[!is.na(updated) & ftn_game_id != 6134, list(ftn_game_id, updated, season, week)],
    current_data[, list(ftn_game_id, date_pulled)] |> unique(),
    by = "ftn_game_id",
    all = TRUE
  )[
    is.na(date_pulled) | date_pulled < updated
  ]

  if(nrow(games_to_update) == 0) {
    cli::cli_alert_danger("No games are out of date, exiting...")
    return(invisible(FALSE))
  }

  cli::cli_alert_info("Found {nrow(games_to_update)} games to update")
  # Using the gid query would be more efficient but in 2024 W1 the query stopped working
  # and we were asked to switch over to the season,week query
  new_games <- ftn_nflverse(year = games_to_update$season, week = games_to_update$week)

  # ftn_nflverse expands a grid which could load more games than required.
  new_games <- new_games[ftn_game_id %in% games_to_update$ftn_game_id]

  if(nrow(new_games) == 0) return(invisible(FALSE))

  ftn_data <- data.table::rbindlist(
    list(
      current_data[!ftn_game_id %in% new_games$ftn_game_id],
      new_games
    )
  )

  cli::cli_alert_info("Saving raw data")
  saveRDS(ftn_data, "exec/full_ftn_data.rds")
  nflversedata::nflverse_upload("exec/full_ftn_data.rds", repo = "nflverse/nflverse-ftn", tag = "raw")

  cli::cli_alert_info("Publishing updated data")
  ftn_data[season %in% new_games$season] |>
    split.data.frame(~season) |>
    purrr::walk(
      \(.x){
        nflversedata::nflverse_save(
          data_frame = .x,
          file_name = paste0("ftn_charting_", .x$season[[1]]),
          nflverse_type = "Charting Data provided by FTNData.com",
          release_tag = "ftn_charting",
          file_types = c("rds","parquet","csv","qs")
        )
      }
    )

  cli::cli_alert_success("Job complete")

  return(invisible(TRUE))
}

update_ftn()
