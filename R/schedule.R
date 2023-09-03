#' Get Schedule
#'
#' Retrieves schedule and attaches nflverse game ID
#'
#' @export
ftn_schedule <- function(){
  schedule_raw <- .ftn_request("schedule") |> data.table::as.data.table()
  schedule <- schedule_raw[
    ,
    list(
      nflverse_game_id = nflreadr::nflverse_game_id(season = seas, week = wk, away = v, home = h),
      ftn_game_id = gid,
      season = seas,
      week = wk,
      weekday = day,
      gameday = date,
      stadium = stad,
      surface = surf,
      updated = lubridate::mdy_hms(updated)
    )
  ][]

  return(schedule)
}
