test_that("Log follows expected format", {
  log <- read_log(create_dummy_log())
  expect_s3_class(log, 'data.frame')
  expect_length(log, 3L)
  purrr::walk2(
    log, c('POSIXct', 'character', 'numeric'),
    function(column, type) expect_true(inherits(column, type))
  )
})
