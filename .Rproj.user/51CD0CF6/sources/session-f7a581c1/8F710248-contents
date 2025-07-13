test_that("chi_square_test works with binomial (manual and auto)", {
  x <- simulate_data(100, "binomial", list(2, 0.5))

  # Auto detection
  expect_s3_class(chi_square_test(x), "htest")

  # Manual parameter
  expect_s3_class(chi_square_test(x, "binomial", list(size = 2, prob = 0.5)), "htest")
})

test_that("chi_square_test works with poisson", {
  x <- simulate_data(100, "poisson", list(3))

  # Auto parameter
  expect_s3_class(chi_square_test(x, "poisson"), "htest")

  # Manual
  expect_s3_class(chi_square_test(x, "poisson", list(lambda = 3)), "htest")
})

test_that("chi_square_test works with geometric", {
  x <- simulate_data(100, "geometric", list(0.3))

  # Auto
  expect_s3_class(chi_square_test(x, "geometric"), "htest")

  # Manual
  expect_s3_class(chi_square_test(x, "geometric", list(prob = 0.3)), "htest")
})

test_that("chi_square_test fails on unknown dist", {
  x <- simulate_data(100, "poisson", list(2))
  expect_error(chi_square_test(x, "unknown_dist"))
})
