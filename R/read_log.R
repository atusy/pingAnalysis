tidy_log <- function(df) {
  df |>
    stats::setNames(c("timestamp", "address", "ping")) |>
    dplyr::mutate(
      timestamp = lubridate::parse_date_time(timestamp, order = "ymdHMS"),
      ping = as.numeric(ifelse(ping == "-", NA_character_, ping))
    ) |>
    dplyr::arrange(timestamp)
}

#' Read and parse log file as a data frame.
#'
#' @param path A path to the log file.
#'
#' @value A data frame object with three columns:
#' - timestamp: <dttm> Ordered timestamps of ping.
#' - address: <chr> IPv4 address of server.
#' - ping: <dbl> Elapsed time in msec, or `NA` in case of timeout.
#'
#' @export
read_log <- function(path) {
  path |>
    readr::read_csv(col_names = FALSE, col_types = "c") |>
    tidy_log()
}
