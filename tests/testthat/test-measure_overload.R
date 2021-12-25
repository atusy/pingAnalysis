test_that("rollmean", {
  expect_identical(rollmean(c(1, 3, 5, 7), 2L), c(NA, 2, 4, 6))
})

test_that("measure overload", {
  basetime <- 20200101000000
  pings <- c(
    0, 11, 0, 11, 12, 0, 11, 12, 13,
    0, 11, 0, 11, 12, 0, 11, NA, 13
  )
  N <- length(pings) / 2L
  log <- tibble::tibble(
    timestamp = lubridate::ymd_hms(basetime + seq(N)) |> rep(2L),
    address = paste0("192.168.1.", 1L:2L, "/24") |> rep(each = N),
    ping = pings
  )

  #  m and N is 1
  t1 <- measure_overload(log, m = 1L, t = 10, N = 1L)
  expect_named(t1, c("address", "start", "end", "n_timeout"))
  expect_equal(nrow(t1), 7L)
  expect_equal(t1$n_timeout, c(1L, 1L, 2L, 2L, 1L, 3L, 1L))

  # change m to 2
  t2 <- measure_overload(log, m = 2L, t = 10, N = 1L)
  expect_equal(nrow(t2), 3L)
  expect_equal(t2$n_timeout, c(1L, 1L, 2L))

  # change N to 2
  t3 <- measure_overload(log, m = 1L, t = 10, N = 2L)
  expect_equal(nrow(t3), 3L)
  expect_equal(t3$n_timeout, c(2L, 2L, 3L))
})
