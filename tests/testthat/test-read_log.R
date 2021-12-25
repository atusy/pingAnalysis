test_that("Log follows expected format", {
  log_df <- read_log(I(paste(
    "20200101000001, 192.168.1.1/24, 5",
    "20200101000002, 192.168.1.1/24, 8",
    sep = "\n"
  )))
  expect_s3_class(log_df, "data.frame")
  expect_length(log_df, 3L)
  purrr::walk2(
    log_df, c("POSIXct", "character", "numeric"),
    function(column, type) expect_true(inherits(column, type))
  )
})
