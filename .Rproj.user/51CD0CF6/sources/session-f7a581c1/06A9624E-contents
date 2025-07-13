test_that("simulate_data untuk distribusi diskrit", {
  bern <- simulate_data(100, "bernoulli", list(0.5))
  expect_setequal(unique(bern), c(0, 1))
  expect_length(bern, 100)

  binom <- simulate_data(100, "binomial", list(5, 0.5))
  expect_true(all(binom %in% 0:5))
  expect_length(binom, 100)

  pois <- simulate_data(100, "poisson", list(3))
  expect_true(all(pois >= 0))
  expect_length(pois, 100)

  geom <- simulate_data(100, "geometric", list(0.3))
  expect_true(all(geom >= 0))
  expect_length(geom, 100)
})

test_that("simulate_data untuk distribusi kontinu", {
  norm <- simulate_data(100, "normal", list(0, 1))
  expect_type(norm, "double")
  expect_length(norm, 100)

  expo <- simulate_data(100, "exponential", list(1))
  expect_true(all(expo >= 0))
  expect_length(expo, 100)

  gam <- simulate_data(100, "gamma", list(2, 1))
  expect_true(all(gam >= 0))
  expect_length(gam, 100)

  beta <- simulate_data(100, "beta", list(2, 2))
  expect_true(all(beta >= 0 & beta <= 1))
  expect_length(beta, 100)
})

test_that("simulate_data gagal untuk distribusi tak dikenal", {
  expect_error(simulate_data(100, "uniform", list()), "Distribusi tidak dikenali")
})
