test_that("chi_square_test works with bernoulli", {
  x <- simulate_data(100, "bernoulli", list(prob = 0.5))
  expect_s3_class(chi_square_test(x), "htest")
  expect_s3_class(chi_square_test(x, "bernoulli", list(prob = 0.5)), "htest")
})

test_that("chi_square_test works with binomial", {
  x <- simulate_data(100, "binomial", list(size = 2, prob = 0.5))
  expect_s3_class(chi_square_test(x), "htest")
  expect_s3_class(chi_square_test(x, "binomial", list(size = 2, prob = 0.5)), "htest")
})

test_that("chi_square_test works with poisson", {
  x <- simulate_data(100, "poisson", list(lambda = 3))
  expect_s3_class(suppressWarnings(chi_square_test(x)), "htest")
  expect_s3_class(suppressWarnings(chi_square_test(x, "poisson", list(lambda = 3))), "htest")
})

test_that("chi_square_test works with geometric", {
  x <- simulate_data(100, "geometric", list(prob = 0.3))
  expect_s3_class(suppressWarnings(chi_square_test(x)), "htest")
  expect_s3_class(suppressWarnings(chi_square_test(x, "geometric", list(prob = 0.3))), "htest")
})
