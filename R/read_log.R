#' Read and parse log file as a data frame.
#'
#' @param file
#'   A path to the CSV file or a CSV formated string.
#'   First row starts with values without headers.
#'
#' @examples
#' # Prepare data
#' log_csv <- paste(
#'   "20200101000001, 192.168.1.1/24, 5",
#'   "20200101000002, 192.168.1.1/24, 8",
#'   sep = "\n"
#' )
#' path <- tempfile()
#' writeLines(log_csv, path)
#'
#' # Read from path and string.
#' read_log(path)
#' read_log(I(log_csv))
#'
#'
#' @return A data frame object with three columns sorted by these columns:
#' - timestamp: (dttm) Ordered timestamps of ping.
#' - address: (chr) IPv4 address of server.
#' - ping: (dbl) Elapsed time in msec, or `NA` in case of timeout.
#'
#' @export
read_log <- function(file) {
  file |>
    readr::read_csv(col_names = FALSE, col_types = "c") |>
    stats::setNames(c("timestamp", "address", "ping")) |>
    dplyr::mutate(
      timestamp = lubridate::parse_date_time(timestamp, order = "ymdHMS"),
      ping = as.numeric(ifelse(ping == "-", NA_character_, ping))
    ) |>
    dplyr::arrange(timestamp, address, ping)
}
