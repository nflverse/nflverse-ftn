args <- commandArgs(trailingOnly = TRUE)

update_pbp_participation <- function(season = nflreadr::most_recent_season()) {
  if (season < 2024) {
    cli::cli_alert_warning(
      "Data not available to parse from FTN prior to the 2023 season. The data that exists in `load_participation()` for those seasons is solely from NGS and cannot be scraped again."
    )
    return(NULL)
  }

  plays <- nflreadr::load_from_url(
    glue::glue(
      "https://github.com/nflverse/nflverse-ftn/releases/download/raw/ftn_participation_{season}.rds"
    )
  )

  ftn_data <- nflreadr::load_from_url(
    "https://github.com/nflverse/nflverse-ftn/releases/download/raw/full_ftn_data.rds"
  )

  plays <- plays |>
    dplyr::left_join(
      ftn_data,
      by = dplyr::join_by(
        pid == ftn_play_id
      ),
      na_matches = "never"
    ) |> # some plays are missing from `ftn_data`, so we use information from other games to grab this info
    dplyr::group_by(gameId) |>
    dplyr::mutate(
      nflverse_game_id = data.table::fcoalesce(
        nflverse_game_id,
        nflreadr::stat_mode(nflverse_game_id, na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      nflreadr::load_pbp(seasons = season),
      by = dplyr::join_by(nflverse_game_id == game_id, nflplayid == play_id),
      na_matches = "never",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(posteam = data.table::fcoalesce(posteam, ""))

  collapse_pos <- function(x) {
    table(x) |>
      tibble::enframe() |>
      (\(x) paste(x$value, x$name))() |>
      paste0(collapse = ", ")
  }

  personnel <- plays |>
    dplyr::filter(posteam != "") |>
    dplyr::select(
      gameId,
      nflplayid,
      posteam,
      dplyr::starts_with("items"),
      dplyr::starts_with("tmabbr"),
      dplyr::starts_with("tmjers"),
      dplyr::starts_with("gsisid"),
    ) |>
    dplyr::rename_with(\(x) gsub("items.", "", x, fixed = TRUE)) |>
    tidyr::pivot_longer(
      cols = -c(nflplayid, gameId, posteam),
      values_transform = as.character
    ) |>
    tidyr::separate_wider_delim(
      name,
      names = c("name", "player_num"),
      delim = "_"
    ) |>
    tidyr::pivot_wider(id_cols = c(nflplayid, gameId, player_num, posteam)) |>
    dplyr::arrange(position) |>
    dplyr::group_by(
      gameId,
      nflplayid
    ) |>
    dplyr::summarize(
      offense_personnel = collapse_pos(position[posteam == tmabbr]),
      defense_personnel = collapse_pos(position[posteam != tmabbr]),
      players_on_play = paste0(gsisid, collapse = ";"),
      offense_players = paste0(gsisid[posteam == tmabbr], collapse = ";"),
      defense_players = paste0(gsisid[posteam != tmabbr], collapse = ';'),
      n_offense = sum(posteam == tmabbr, na.rm = TRUE),
      n_defense = sum(posteam != tmabbr, na.rm = TRUE),
      offense_names = paste0(name[posteam == tmabbr], collapse = ";"),
      defense_names = paste0(name[posteam != tmabbr], collapse = ";"),
      offense_positions = paste0(position[posteam == tmabbr], collapse = ";"),
      defense_positions = paste0(position[posteam != tmabbr], collapse = ";"),
      offense_numbers = paste0(
        uniformNumber[posteam == tmabbr],
        collapse = ";"
      ),
      defense_numbers = paste0(
        uniformNumber[posteam != tmabbr],
        collapse = ";"
      ),
      .groups = "drop"
    )

  plays <- plays |>
    dplyr::left_join(personnel, by = dplyr::join_by(gameId, nflplayid)) |>
    dplyr::mutate(
      offense_formation = dplyr::case_when(
        offense_formation == "P" ~ "PISTOL",
        offense_formation == "S" ~ "SHOTGUN",
        offense_formation == "U" ~ "UNDER CENTER",
        TRUE ~ NA_character_
      ),
      ngs_air_yards = NA_real_,
      route = stringi::stri_trans_toupper(
        gsub("[0-9]{1,} - ", "", route)
      ),
      defense_coverage_type = dplyr::case_when(
        defense_coverage_type == "0" ~ "COVER_0",
        defense_coverage_type == "1" ~ "COVER_1",
        defense_coverage_type == "2" ~ "COVER_2",
        defense_coverage_type == "2M" ~ "2_MAN",
        defense_coverage_type == "3" ~ "COVER_3",
        defense_coverage_type == "4" ~ "COVER_4",
        defense_coverage_type == "6" ~ "COVER_6",
        defense_coverage_type == "9" ~ "COVER_9",
        defense_coverage_type == "C" ~ "COMBO",
        defense_coverage_type == "N" ~ "BLOWN",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(
      nflverse_game_id,
      old_game_id,
      play_id = nflplayid,
      possession_team = posteam,
      offense_formation,
      offense_personnel,
      defenders_in_box,
      defense_personnel,
      number_of_pass_rushers = n_pass_rushers,
      players_on_play,
      offense_players,
      defense_players,
      n_offense,
      n_defense,
      ngs_air_yards,
      time_to_throw,
      was_pressure,
      route,
      defense_man_zone_type,
      defense_coverage_type,
      offense_names,
      defense_names,
      offense_positions,
      defense_positions,
      offense_numbers,
      defense_numbers,
    )

  current_participation <- nflreadr::load_participation(season)
  if (nrow(current_participation)) {
    plays <- current_participation |>
      dplyr::rows_upsert(plays, by = c("nflverse_game_id", "play_id"))
  }

  cli::cli_process_start("Uploading participation data to nflverse-data")

  nflversedata::nflverse_save(
    data_frame = plays,
    file_name = paste0("pbp_participation_", season),
    nflverse_type = "Participation Data provided by FTNData.com",
    file_types = c("rds", "parquet", "csv", "qs"),
    release_tag = "pbp_participation"
  )
}

cli::cli_process_done()

if (length(args) == 0) {
  update_pbp_participation()
} else if (args[1] == "ALL YEARS") {
  purrr::walk(2024:nflreadr::most_recent_season(), update_pbp_participation)
}
