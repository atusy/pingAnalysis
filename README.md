
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pingAnalysis

<!-- badges: start -->

[![R-CMD-check](https://github.com/atusy/pingAnalysis/workflows/R-CMD-check/badge.svg)](https://github.com/atusy/pingAnalysis/actions)
<!-- badges: end -->

Analyze ping log.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atusy/pingAnalysis")
```

## Examples

Load the package with:

``` r
library(pingAnalysis)
```

### Treat all ping timeout as server being out of service.

#### Prepare data

``` r
log_csv <- tempfile(fileext = ".csv")
log_df <- tibble::tibble(
  timestamp = lubridate::ymd_hms(20200101000000 + seq(5)),
  address = "192.168.1.1/24",
  ping = c("1", "-", "1", "-", "-")
)
readr::write_csv(log_df, log_csv, col_names = FALSE)
```

<details>

<summary>View data</summary>

``` r
knitr::kable(log_df)
```

| timestamp           | address        | ping |
| :------------------ | :------------- | :--- |
| 2020-01-01 00:00:01 | 192.168.1.1/24 | 1    |
| 2020-01-01 00:00:02 | 192.168.1.1/24 | \-   |
| 2020-01-01 00:00:03 | 192.168.1.1/24 | 1    |
| 2020-01-01 00:00:04 | 192.168.1.1/24 | \-   |
| 2020-01-01 00:00:05 | 192.168.1.1/24 | \-   |

</details>

#### Analyze

``` r
measure_all_timeout(read_log(log_csv))
#> # A tibble: 2 × 4
#>   address        start               end                 n_timeout
#>   <chr>          <dttm>              <dttm>                  <int>
#> 1 192.168.1.1/24 2020-01-01 00:00:02 2020-01-01 00:00:03         1
#> 2 192.168.1.1/24 2020-01-01 00:00:04 NA                          2
```

### Treat ping timeout as server being out of service **if timeout continues `N`-times or more**.

Data inherits from the previous example.

#### Analyze

  - `N = 1L` is equivalent to `measure_all_timeout()`

<!-- end list -->

``` r
measure_timeout(read_log(log_csv), N = 1L)
#> # A tibble: 2 × 4
#>   address        start               end                 n_timeout
#>   <chr>          <dttm>              <dttm>                  <int>
#> 1 192.168.1.1/24 2020-01-01 00:00:02 2020-01-01 00:00:03         1
#> 2 192.168.1.1/24 2020-01-01 00:00:04 NA                          2
```

  - `N = 2L`

<!-- end list -->

``` r
measure_timeout(read_log(log_csv), N = 2L)
#> # A tibble: 1 × 4
#>   address        start               end                 n_timeout
#>   <chr>          <dttm>              <dttm>                  <int>
#> 1 192.168.1.1/24 2020-01-01 00:00:04 NA                          2
```

### Treat ping result as server being overloaded if rolling mean of ping is greater than or equal to threshold.

#### Prepare data

``` r
basetime <- 20200101000000
pings <- c(
  0, 11, 0, 11, 12, 0, 11, 12, 13,
  0, 11, 0, 11, 12, 0, 11, NA, 13
) |> as.character() |> dplyr::na_if("0") |> dplyr::coalesce("-")
N <- length(pings) / 2L
log_df <- tibble::tibble(
  timestamp = lubridate::ymd_hms(basetime + seq(N)) |> rep(2L),
  address = paste0("192.168.1.", 1L:2L, "/24") |> rep(each = N),
  ping = pings
)
readr::write_csv(log_df, log_csv, col_names = FALSE)
```

<details>

<summary>View data</summary>

``` r
knitr::kable(log_df)
```

| timestamp           | address        | ping |
| :------------------ | :------------- | :--- |
| 2020-01-01 00:00:01 | 192.168.1.1/24 | \-   |
| 2020-01-01 00:00:02 | 192.168.1.1/24 | 11   |
| 2020-01-01 00:00:03 | 192.168.1.1/24 | \-   |
| 2020-01-01 00:00:04 | 192.168.1.1/24 | 11   |
| 2020-01-01 00:00:05 | 192.168.1.1/24 | 12   |
| 2020-01-01 00:00:06 | 192.168.1.1/24 | \-   |
| 2020-01-01 00:00:07 | 192.168.1.1/24 | 11   |
| 2020-01-01 00:00:08 | 192.168.1.1/24 | 12   |
| 2020-01-01 00:00:09 | 192.168.1.1/24 | 13   |
| 2020-01-01 00:00:01 | 192.168.1.2/24 | \-   |
| 2020-01-01 00:00:02 | 192.168.1.2/24 | 11   |
| 2020-01-01 00:00:03 | 192.168.1.2/24 | \-   |
| 2020-01-01 00:00:04 | 192.168.1.2/24 | 11   |
| 2020-01-01 00:00:05 | 192.168.1.2/24 | 12   |
| 2020-01-01 00:00:06 | 192.168.1.2/24 | \-   |
| 2020-01-01 00:00:07 | 192.168.1.2/24 | 11   |
| 2020-01-01 00:00:08 | 192.168.1.2/24 | \-   |
| 2020-01-01 00:00:09 | 192.168.1.2/24 | 13   |

</details>

#### Analyze

Read log

``` r
log_df <- read_log(log_csv)
```

Analyze varying parameters:

  - m: window size for rolling mean
  - t: threshold of ping
  - N: counts of continuous errors

<!-- end list -->

``` r
measure_overload(log_df, m = 1L, t = 10, N = 1L)
#> # A tibble: 8 × 4
#>   address        start               end                 n_timeout
#>   <chr>          <dttm>              <dttm>                  <int>
#> 1 192.168.1.1/24 2020-01-01 00:00:02 2020-01-01 00:00:03         1
#> 2 192.168.1.2/24 2020-01-01 00:00:02 NA                          1
#> 3 192.168.1.1/24 2020-01-01 00:00:04 2020-01-01 00:00:06         2
#> 4 192.168.1.2/24 2020-01-01 00:00:04 NA                          2
#> 5 192.168.1.2/24 2020-01-01 00:00:07 2020-01-01 00:00:08         1
#> 6 192.168.1.1/24 2020-01-01 00:00:07 NA                          2
#> 7 192.168.1.1/24 2020-01-01 00:00:09 NA                          1
#> 8 192.168.1.2/24 2020-01-01 00:00:09 NA                          1
measure_overload(log_df, m = 2L, t = 10, N = 1L)
#> # A tibble: 3 × 4
#>   address        start               end                 n_timeout
#>   <chr>          <dttm>              <dttm>                  <int>
#> 1 192.168.1.1/24 2020-01-01 00:00:05 2020-01-01 00:00:06         1
#> 2 192.168.1.2/24 2020-01-01 00:00:05 NA                          1
#> 3 192.168.1.1/24 2020-01-01 00:00:08 NA                          2
measure_overload(log_df, m = 1L, t = 10, N = 2L)
#> # A tibble: 3 × 4
#>   address        start               end                 n_timeout
#>   <chr>          <dttm>              <dttm>                  <int>
#> 1 192.168.1.1/24 2020-01-01 00:00:04 2020-01-01 00:00:06         2
#> 2 192.168.1.2/24 2020-01-01 00:00:04 NA                          2
#> 3 192.168.1.1/24 2020-01-01 00:00:07 NA                          2
```

### Treat ping result as **subnet**, or network-switch, being out-of-order if all servers fail to response for `N`-times or more.

#### Prepare data

``` r
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
readr::write_csv(log_df, log_csv, col_names = FALSE)
```

<details>

<summary>View data</summary>

``` r
knitr::kable(log_df)
```

| timestamp           | address        | ping |
| :------------------ | :------------- | ---: |
| 2020-01-01 00:00:01 | 192.168.1.1/24 |    1 |
| 2020-01-01 00:00:02 | 192.168.1.1/24 |   NA |
| 2020-01-01 00:00:03 | 192.168.1.1/24 |   NA |
| 2020-01-01 00:00:04 | 192.168.1.1/24 |   NA |
| 2020-01-01 00:00:05 | 192.168.1.1/24 |   NA |
| 2020-01-01 00:00:06 | 192.168.1.1/24 |   NA |
| 2020-01-01 00:00:07 | 192.168.1.1/24 |   NA |
| 2020-01-01 00:00:08 | 192.168.1.1/24 |   NA |
| 2020-01-01 00:00:09 | 192.168.1.1/24 |   NA |
| 2020-01-01 00:00:10 | 192.168.1.1/24 |    1 |
| 2020-01-01 00:00:01 | 192.168.1.2/24 |    1 |
| 2020-01-01 00:00:02 | 192.168.1.2/24 |    1 |
| 2020-01-01 00:00:03 | 192.168.1.2/24 |   NA |
| 2020-01-01 00:00:04 | 192.168.1.2/24 |   NA |
| 2020-01-01 00:00:05 | 192.168.1.2/24 |   NA |
| 2020-01-01 00:00:06 | 192.168.1.2/24 |   NA |
| 2020-01-01 00:00:07 | 192.168.1.2/24 |    1 |
| 2020-01-01 00:00:08 | 192.168.1.2/24 |    1 |
| 2020-01-01 00:00:09 | 192.168.1.2/24 |   NA |
| 2020-01-01 00:00:10 | 192.168.1.2/24 |    1 |
| 2020-01-01 00:00:01 | 192.168.1.3/24 |    1 |
| 2020-01-01 00:00:02 | 192.168.1.3/24 |    1 |
| 2020-01-01 00:00:03 | 192.168.1.3/24 |    1 |
| 2020-01-01 00:00:04 | 192.168.1.3/24 |   NA |
| 2020-01-01 00:00:05 | 192.168.1.3/24 |   NA |
| 2020-01-01 00:00:06 | 192.168.1.3/24 |   NA |
| 2020-01-01 00:00:07 | 192.168.1.3/24 |   NA |
| 2020-01-01 00:00:08 | 192.168.1.3/24 |    1 |
| 2020-01-01 00:00:09 | 192.168.1.3/24 |   NA |
| 2020-01-01 00:00:10 | 192.168.1.3/24 |    1 |

</details>

#### Analyze

Read log.

``` r
log_df <- read_log(log_csv)
```

Assume log contains all the required IP addresses.

``` r
address_all <- unique(log_df$address)
address_all
#> [1] "192.168.1.1/24" "192.168.1.2/24" "192.168.1.3/24"
```

Do the analysis varying `N`.

``` r
measure_subnet_timeout(log_df, N = 1L)
#> # A tibble: 2 × 4
#>   address        start               end                 n_timeout
#>   <chr>          <dttm>              <dttm>                  <int>
#> 1 192.168.1.0/24 2020-01-01 00:00:04 2020-01-01 00:00:07         3
#> 2 192.168.1.0/24 2020-01-01 00:00:09 2020-01-01 00:00:10         1
measure_subnet_timeout(log_df, N = 2L)
#> # A tibble: 2 × 4
#>   address        start               end                 n_timeout
#>   <chr>          <dttm>              <dttm>                  <int>
#> 1 192.168.1.0/24 2020-01-01 00:00:04 2020-01-01 00:00:07         3
#> 2 192.168.1.0/24 2020-01-01 00:00:09 2020-01-01 00:00:10         1
```
