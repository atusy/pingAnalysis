#' Filter logs that records timeout and subsequent logs of resume.
#' @noRd
filter_timeout <- function(log_df) {
  log_df |>
    dplyr::mutate(down = is.na(ping)) |>
    dplyr::group_by(address) |>
    dplyr::filter(down | dplyr::lag(down, default = FALSE)) |>
    dplyr::ungroup()
}

#' Summarize timeout of log
#'
#' @param log A `data.frame` that records the log.
#'
#' @value A data frame with following columns:
#' - address: <chr> IPv4 addresses that suffered timeout.
#' - start: <dttm> When server started to timeout.
#' - end: <dttm> When server resumed or `NA` if not resumed.
#' - n_timeout: <int> Number of timeout before resume.
#'
#' @export
measure_all_timeout <- function(log_df) {
  log_df |>
    filter_timeout() |>
    dplyr::mutate(nth = rev(cumsum(rev(!down)))) |>
    dplyr::group_by(address, nth) |>
    dplyr::summarize(
      start = min(timestamp),
      end = dplyr::if_else(
        any(is.finite(ping)), max(timestamp), lubridate::as_datetime(NA)
      ),
      n_timeout = sum(is.na(ping)),
      .groups = "drop"
    ) |>
    dplyr::select(!nth) |>
    dplyr::arrange(start, end)
}

measure_timeout <- function(log_df, N = 2L) {
  log_df |>
    measure_all_timeout() |>
    dplyr::filter(n_timeout >= N)
}
