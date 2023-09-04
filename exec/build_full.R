pkgload::load_all()

schedule <- ftn_schedule()[!is.na(updated)]
ftn_data <- ftn_nflverse(year = unique(schedule$season), week = unique(schedule$week))
ftn_data <- nflreadr:::make_nflverse_data(ftn_data, nflverse_type = "Charting Data provided by FTNData.com")
saveRDS(ftn_data, "exec/full_ftn_data.rds")
piggyback::pb_upload("exec/full_ftn_data.rds", repo = "nflverse/nflverse-ftn", tag = "raw")

ftn_data |>
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
