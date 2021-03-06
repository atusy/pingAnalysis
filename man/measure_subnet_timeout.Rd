% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_subnet_timeout.R
\name{measure_subnet_timeout}
\alias{measure_subnet_timeout}
\title{Measure timeout of subnet, or switch.}
\usage{
measure_subnet_timeout(log_df, N = 1L, address_all = NULL)
}
\arguments{
\item{log_df}{A \code{data.frame} that records the log.}

\item{N}{Threshold of continuous errors to be considered as fatal.}

\item{address_all}{A character vector of all the IPv4 addresses to be logged.
They must be suffixed by prefix-size (e.g., '192.168.1.1/24').}
}
\value{
A data frame with following columns:
\itemize{
\item address: (chr) IPv4 addresses that suffered timeout.
\item start: (dttm) When server started to timeout.
\item end: (dttm) When server resumed or \code{NA} if not resumed.
\item n_timeout: (int) Number of timeout before resume.
}

data.frame
}
\description{
Measure timeout of subnet, or switch.
}
\examples{
# Prepare data
basetime <- 20200101000000
pings <- c(
  # 1  2  3  4  5  6  7  8  9 10
  1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  1, 1, 0, 0, 0, 0, 1, 1, 0, 1,
  1, 1, 1, 0, 0, 0, 0, 1, 0, 1
)
N <- length(pings) / 3L
log_df <- tibble::tibble(
  timestamp = lubridate::ymd_hms(basetime + seq(N)) |> rep(3L),
  address = paste0("192.168.1.", 1L:3L, "/24") |> rep(each = N),
  ping = ifelse(pings == 0, NA_real_, pings)
)

# Measure
measure_subnet_timeout(
  log_df,
  N = 2L,
  address_all = unique(log_df$address)
)

}
