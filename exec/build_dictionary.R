ftn_to_nflverse <- c(
  "ftn_game_id" = "gid",
  "nflverse_game_id" = "game_id",
  "season" = "year",
  "week" = "week",
  "ftn_play_id" = "pid",
  "nflverse_play_id" = "play_id",
  "starting_hash" = "hash",
  "qb_location" = "qb_pos",
  "n_offense_backfield" = "back",
  "n_defense_box" = "box",
  "is_no_huddle" = "nh",
  "is_motion" = "mot",
  "is_play_action" = "pap",
  "is_screen_pass" = "scre",
  "is_rpo" = "rpo",
  "is_trick_play" = "trick",
  "is_qb_out_of_pocket" = "oop",
  "is_interception_worthy" = "intw",
  "is_throw_away" = "qbta",
  "read_thrown" = "read",
  "is_catchable_ball" = "cball",
  "is_contested_ball" = "cnb",
  "is_created_reception" = "crr",
  "is_drop" = "drp",
  "is_qb_sneak" = "sneak",
  "n_blitzers" = "blz",
  "n_pass_rushers" = "pru",
  "is_qb_fault_sack" = "qbsk",
  "date_pulled" = "date_pulled"
) |>
  tibble::enframe(name = "field_name", value = "ftn_field_name") |>
  dplyr::mutate(order = dplyr::row_number())

# Crudely copypasted <https://docs.google.com/document/d/12WEm0O9TG7VQsn--fkCQAE_3735owJxfxZ8SvysceZU/edit>
# into a Google sheet and applied hand-cleanup
ftn_fields <- googlesheets4::read_sheet("1RRbhD4xDzHm2jkDxRThMGd5nPFtwFBcMhKtW7Dcf_bs") |>
  dplyr::mutate_all(\(x) stringr::str_remove_all(x, "\\n")) |>
  dplyr::distinct(ftn_name, .keep_all = TRUE)

ftn_data_dictionary <- skimr::skim(ftn_nflverse(year = 2022, week = 1)) |>
  dplyr::select(
    field_name = skim_variable,
    field_type = skim_type
  ) |>
  dplyr::left_join(ftn_to_nflverse, by = c("field_name")) |>
  dplyr::left_join(
    ftn_fields |> dplyr::select(ftn_field_name = ftn_name, description = ftn_description),
    by = c("ftn_field_name")
  ) |>
  dplyr::arrange(order) |>
  dplyr::rows_patch(
    tibble::tribble(
      ~field_name, ~description,
      "season", "NFL season starting year. Data is currently available from 2022 onwards.",
      "week", "NFL week number.",
      "nflverse_game_id", "Game ID used by nflverse - built from season, week, home, and away teams",
      "nflverse_play_id", "Play ID used by nflverse, corresponds to GSIS play ID",
      "date_pulled", "Date the data was retrieved from the FTN Data API by nflverse jobs"
    )
  )

data.table::fwrite(ftn_data_dictionary, "ftn_data_dictionary.csv")
