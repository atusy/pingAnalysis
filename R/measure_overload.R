#' Apply rolling mean
#'
#' @param x A numeric vector.
#' @param m A window size.
#'
#' @return
#' A numeric vector. The first `m - 1L` items become `NA`.
#' If window contains `NA`, its result also becomes `NA`.
#'
#' @examples
#' rollmean(c(1, 3, 5, 7), 2L) # c(NA, 2, 4, 6)
#'
#' @noRd
rollmean <- function(x, m) {
  x |>
    slider::slide_dbl(
      function(x) `if`(is.null(x), NA_real_, mean(x)),
      .before = m - 1L,
      .complete = TRUE
    )
}

#' Let overload be timeout
#'
#' To implement measure_overload based on `measure_timeout()`,
#' overload should be treated as if it is timeout.
#'
#' @param ping A numeric vector of ping time.
#' @param m A window size for rolling mean of ping.
#' @param t Threshold of overload.
#'
#' @return A numeric vector
#' @noRd
let_overload_be_timeout <- function(ping, m = 2L, t = 10) {
  average <- dplyr::coalesce(rollmean(ping, m), 0)
  return(dplyr::if_else(average >= t, NA_real_, average))
}


#' Measure overload
#'
#' Summarize the overload conditions when rolling mean of ping exceeds the
#' limit. If a window contains timeout, it is not considered as overload
#' regardless of any finite values within the window.
#' Note that no-response is discarded instead of being treated as timeout by
#' `measure_timeout()`.
#'
#' @inheritParams measure_all_timeout
#' @param m A window size of rolling mean of ping time.
#' @param t Upper limit of rolling mean of ping time.
#'
#' @examples
#' log_df <- data.frame(
#'   timestamp = lubridate::ymd_hms(20200101000000 + seq(6)),
#'   address = "192.168.1.1/24",
#'   ping = c(1, 10, NA, 10, 10, NA)
#' )
#' measure_overload(log_df, m = 1L, t = 10)
#' measure_overload(log_df, m = 2L, t = 10)
#'
#' @inherit measure_all_timeout return
#' @export
measure_overload <- function(log_df, m = 2L, t = 10, N = 1L) {
  log_df |>
    dplyr::group_by(address) |>
    dplyr::mutate(ping = let_overload_be_timeout(ping, m = m, t = t)) |>
    dplyr::ungroup() |>
    measure_timeout(N = N)
}
