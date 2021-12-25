#' Apply rolling mean
#'
#' @param x A numeric vector.
#' @param m A window size.
#'
#' @value
#' A numeric vector. The first `m - 1L` items become `NA`.
#' If window contains `NA`, its result also becomes `NA`.
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
#' @value A numeric vector
#' @noRd
let_overload_be_timeout <- function(ping, m = 2L, t = 10) {
  average <- dplyr::coalesce(rollmean(ping, m), 0)
  return(if_else(average >= t, NA_real_, average))
}


#' Measure overload
#'
#' Summarize the overload conditions when rolling mean of ping exceeds the
#' limit. If a window contains timeout, it is not considered as overload
#' regardless of any finite values within the window.
#'
#' @inheritParams measure_timeout
#' @param m A window size of rolling mean of ping time.
#' @param t Upper limit of rolling mean of ping time.
#'
#' @inheritSection measure_timeout value
measure_overload <- function(log, m = 2L, t = 10, N = 1L) {
  log |>
    dplyr::group_by(address) |>
    dplyr::mutate(ping = let_overload_be_timeout(ping, m)) |>
    measure_timeout(N = N)
}
