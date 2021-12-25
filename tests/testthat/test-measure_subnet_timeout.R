test_that("coerce_ipv4_to_bits should return a list of 32 binary integers", {
  ipv4 <- c("192.168.1.1/24", "192.168.1.2")
  bits <- coerce_ipv4_to_bits(ipv4)
  expect_length(bits, length(bits))
  purrr::walk(bits, expect_length, 32L)
  expect_setequal(unique(unlist(bits)), c(0L, 1L))
})

test_that("find_subnet returns a vector of chracter sized by prefix-size", {
  prefix_sizes <- c(10, 20, 24)
  ipv4 <- paste0("192.168.1.1/", prefix_sizes)
  subnet <- find_subnet(ipv4)
  expect_length(subnet, length(ipv4))
  expect_equal(stringr::str_length(subnet), prefix_sizes)
})

test_that("Measure timeout of switches", {
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
  expected <- tibble::tibble(
    start = lubridate::ymd_hms(basetime + c(4L, 9L)),
    end = lubridate::ymd_hms(basetime + c(7L, 10L)),
    n_timeout = c(3L, 1L)
  )

  # One switch
  expect_equal(
    measure_subnet_timeout(log_df, N = 1L)[names(expected)] |>
      dplyr::arrange(start, end),
    expected
  )

  # Two switches
  log_df |>
    dplyr::mutate(
      address = stringr::str_replace(address, "^192\\.168\\.1\\.", "192.168.2.")
    ) |>
    dplyr::bind_rows(log_df) |>
    measure_subnet_timeout(N = 1L) |>
    dplyr::select(names(expected)) |>
    expect_equal(dplyr::bind_rows(expected, expected) |> dplyr::arrange(start, end))
})

