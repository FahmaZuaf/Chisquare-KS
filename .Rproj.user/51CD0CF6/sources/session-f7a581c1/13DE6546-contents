test_that("ks_test works with normal data (auto and manual)", {
  x <- simulate_data(100, "normal", list(0, 1))

  # Auto estimate
  expect_s3_class(ks_test(x, "normal"), "htest")

  # Manual
  expect_s3_class(ks_test(x, "normal", list(mean = 0, sd = 1)), "htest")
})

test_that("ks_test works with exponential data", {
  x <- simulate_data(100, "exponential", list(2))

  # Auto
  expect_s3_class(ks_test(x, "exponential"), "htest")

  # Manual
  expect_s3_class(ks_test(x, "exponential", list(rate = 2)), "htest")
})

test_that("ks_test works with gamma data", {
  x <- simulate_data(100, "gamma", list(2, 1))

  # Auto
  expect_s3_class(ks_test(x, "gamma"), "htest")

  # Manual
  expect_s3_class(ks_test(x, "gamma", list(shape = 2, rate = 1)), "htest")
})

test_that("ks_test works with beta data (manual & auto)", {
  x <- simulate_data(100, "beta", list(2, 5))

  # Manual
  expect_s3_class(ks_test(x, "beta", list(shape1 = 2, shape2 = 5)), "htest")

  # Auto
  expect_s3_class(ks_test(x, "beta"), "htest")
})

test_that("ks_test errors for unknown dist", {
  x <- simulate_data(100, "normal", list(0, 1))
  expect_error(ks_test(x, "not_a_dist"))
})
