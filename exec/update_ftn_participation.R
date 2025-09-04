season <- Sys.getenv("NFLVERSE_UPDATE_SEASON", unset = NA_character_) |>
  as.integer()

type <- Sys.getenv("NFLVERSE_UPDATE_TYPE", unset = NA_character_)
type <- rlang::arg_match0(type, c("participation"))

ftn_participation <- nflverseftn::ftn_participation(season)

if (nrow(ftn_participation) > 0) {
  nflversedata::nflverse_save(
    data_frame = ftn_participation,
    file_name = paste0("pbp_participation_", season),
    nflverse_type = "Participation Data provided by FTNData.com",
    file_types = c("rds", "parquet", "csv", "qs"),
    release_tag = "pbp_participation"
  )
}
