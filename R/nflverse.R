#' Get nflverse data
#'
#' Retrieves FTN-provided nflverse-specific data
#'
#' @param gid,year,week args to specify games to return
#'
#' @seealso [FTN's data docs](https://docs.google.com/document/d/12WEm0O9TG7VQsn--fkCQAE_3735owJxfxZ8SvysceZU/edit)
#'
#' @export
ftn_nflverse <- function(gid = NULL, year = NULL, week = NULL){
  stopifnot(
    !is.null(gid) || (!is.null(year) && !is.null(week)),
    is.null(year) || is.numeric(year),
    is.null(week) || is.numeric(week),
    nzchar(Sys.getenv("FTN_NFLVERSE_ENDPOINT"))
  )

  endpoint <- Sys.getenv("FTN_NFLVERSE_ENDPOINT")

  if(!is.null(gid)) {
    df_game <- purrr::map(
      gid,
      purrr::possibly(\(gid) .ftn_request(endpoint = endpoint, gid = gid),otherwise = list()),
      .progress = "Downloading ftn nflverse data") |>
      data.table::rbindlist()
  }

  if (!is.null(year) && !is.null(week)){
    params <- expand.grid(
      year = year |> unique() |> sort(),
      week = week |> unique() |> sort()
    )

    df_game <- purrr::map2(
      params$year,
      params$week,
      \(y,w) try(.ftn_request(.endpoint = endpoint, year = y, week = w)),
      .progress = "Downloading ftn nflverse data"
    ) |>
      data.table::rbindlist()
  }

  if(nrow(df_game) == 0) {
    cli::cli_alert_warning("No data returned by API for provided parameters, exiting...")
    return(invisible())
  }

  df_game_parsed <- df_game[
    , lapply(.SD, function(x) replace(x, x == "", NA))
  ][
    , list(
      ftn_game_id = as.integer(gid),
      nflverse_game_id = game_id,
      season = as.integer(year),
      week = as.integer(week),

      ftn_play_id = as.integer(pid),
      nflverse_play_id = as.integer(play_id),

      starting_hash = hash,
      qb_location = qb_pos,
      n_offense_backfield = as.integer(back),

      is_no_huddle = as.integer(nh),
      is_motion = as.integer(mot),

      is_play_action = as.integer(pap),
      is_screen_pass = as.integer(scre),
      is_rpo = as.integer(rpo),
      is_trick_play = as.integer(trick),

      is_qb_out_of_pocket = as.integer(oop),
      is_interception_worthy = as.integer(intw),
      is_throw_away = as.integer(qbta),
      read_thrown = read,

      is_catchable_ball = as.integer(cball),
      is_contested_ball = as.integer(cnb),
      is_created_reception = as.integer(crr),
      is_drop = as.integer(drp),
      is_qb_sneak = as.integer(sneak),

      n_blitzers = as.integer(blz),
      n_pass_rushers = as.integer(pru),
      is_qb_fault_sack = as.integer(qbsk),

      date_pulled = Sys.time(),
      NULL
    )
  ][
    , purrr::map2(
      .SD,
      names(.SD),
      \(col, nm) if(grepl("is_",nm)) return(as.logical(col)) else return(col)
    )
  ]

  out <- data.table::rbindlist(
    list(
      .ftn_nflverse_template(),
      df_game_parsed
    )
  )

  return(out)
}

.ftn_nflverse_template <- function(){
  data.table::data.table(
    ftn_game_id = integer(0),
    nflverse_game_id = character(0),
    season = integer(0),
    week = integer(0),
    ftn_play_id = integer(0),
    nflverse_play_id = integer(0),
    starting_hash = character(0),
    qb_location = character(0),
    n_offense_backfield = integer(0),
    is_no_huddle = logical(0),
    is_motion = logical(0),
    is_play_action = logical(0),
    is_screen_pass = logical(0),
    is_rpo = logical(0),
    is_trick_play = logical(0),
    is_qb_out_of_pocket = logical(0),
    is_interception_worthy = logical(0),
    is_throw_away = logical(0),
    read_thrown = character(0),
    is_catchable_ball = logical(0),
    is_contested_ball = logical(0),
    is_created_reception = logical(0),
    is_drop = logical(0),
    is_qb_sneak = logical(0),
    n_blitzers = integer(0),
    n_pass_rushers = integer(0),
    is_qb_fault_sack = logical(0),
    date_pulled = as.POSIXct(character(0)) |>
      structure(tzone = NULL)
  )
}
