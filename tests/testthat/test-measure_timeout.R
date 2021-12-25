test_that("multiplication works", {
  .log <- tibble::tibble(
    timestamp = lubridate::ymd_hms(20200101000000 + seq(5)),
    address = "192.168.1.1/24",
    ping = c(1, NA, 1, NA, NA)
  )
  all_timeout <- measure_all_timeout(.log)
  expect_named(all_timeout, c("address", "start", "end", "n_timeout"))
  expect_equal(nrow(all_timeout), 2L)
  expect_identical(measure_timeout(.log, N = 1L), all_timeout)
  expect_identical(
    measure_timeout(.log, N = 2L),
    all_timeout |> dplyr::filter(n_timeout >= 2L)
  )
})
