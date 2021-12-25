#' Filter logs that records timeout and subsequent logs of resume.
#' @noRd
filter_timeout <- function(log_df) {
  log_df |>
    dplyr::mutate(down = is.na(ping)) |>
    dplyr::group_by(address) |>
    dplyr::filter(down | dplyr::lag(down, default = FALSE)) |>
    dplyr::ungroup()
}

#' Measure timeout of log
#'
#' `measure_timeout()` measures timeout errors that continuously occurs `N`
#' times or more. `N = 1` is equivalent to `measure_all_timeout()`.
#'
#' @param log_df A `data.frame` that records the log.
#' @param N Threshold of continuous errors to be considered as fatal.
#'
#' @return A data frame with following columns:
#' - address: (chr) IPv4 addresses that suffered timeout.
#' - start: (dttm) When server started to timeout.
#' - end: (dttm) When server resumed or `NA` if not resumed.
#' - n_timeout: (int) Number of timeout before resume.
#'
#' @examples
#' log_df <- data.frame(
#'   timestamp = lubridate::ymd_hms(20200101000000 + seq(5)),
#'   address = "192.168.1.1/24",
#'   ping = c(1, NA, 1, NA, NA)
#' )
#'
#' measure_timeout(log_df, N = 1L) # equivalent to measure_all_timeout(log)
#' measure_timeout(log_df, N = 2L)
#'
#' @return data.frame
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

#' @rdname measure_all_timeout
#' @export
measure_timeout <- function(log_df, N = 2L) {
  log_df |>
    measure_all_timeout() |>
    dplyr::filter(n_timeout >= N)
}
