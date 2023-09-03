#' FTN api request
#'
#' @param .url url request
#' @param ... query parameters
#' @param .base_url base url, default data.ftndata.com
#' @param .api_key api key, default to envvar FTN_API_KEY
#' @param .name description
#'
#'
#' @seealso <https://app.swaggerhub.com/apis-docs/FTN-Data/FTN-NFL-API/1.0.0>
#' @export
.ftn_request <- function(.endpoint,
                         ...,
                         .base_url = "https://data.ftndata.com",
                         .api_key = Sys.getenv("FTN_API_KEY")){
  stopifnot(
    length(.endpoint) == 1 && nzchar(.endpoint),
    length(.base_url) == 1 && nzchar(.base_url),
    length(.api_key) == 1 && nzchar(.api_key)
  )

  req_url <- httr::modify_url(
    url = .base_url,
    path = .endpoint,
    query = list(...)
  )

  req <- httr::RETRY(
    verb = "GET",
    url = req_url,
    httr::user_agent("nflverse/nflverse-ftn <https://github.com/nflverse/nflverse-ftn>"),
    httr::add_headers(Authorization = .api_key),
    httr::accept_json()
  )

  if (httr::http_error(req)) {
    cli::cli_abort(
      c(
        "!" = "FTN API request failed with status {httr::http_status(req)$message}",
        "i" = "{httr::content(req, as = 'text')}"
      ),
      call = rlang::caller_env()
    )
  }

  resp <- httr::content(req, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  resp
}
