# We get raw participation data split by week as csv
# This reads all csvs and binds them for upload
# we could pass the vector of paths to readr::read_csv, but it fails
# because some weeks have more columns than others
# The number of columns can be
# 152 (in weeks where no team got a too many men on the field penalty)
# 158 (in weeks where at least one team got a 12 men on the field penalty)
# 164 (in weeks where at least one team got a 13 men on the field penalty. Yes, this happened. Check the data.)
df <- list.files("~/Downloads/2025", full.names = TRUE) |>
  purrr::map(data.table::fread) |>
  data.table::rbindlist(fill = TRUE) |>
  # renaming this because nflversedata tries to coerce week to integer
  dplyr::rename(week_type = week)

# this is commented out intentionally to make the next step break and force
# you to set season correctly
# season <- 2025

nflversedata::nflverse_save(
  data_frame = df,
  file_name = paste0("ftn_participation_", season),
  nflverse_type = "",
  release_tag = "raw",
  file_types = "rds",
  repo = "nflverse/nflverse-ftn"
)
