#' Simulasi Data dari Berbagai Distribusi
#'
#' Fungsi ini mensimulasikan data dari distribusi diskrit dan kontinu umum
#' berdasarkan jumlah data dan parameter distribusi yang diberikan.
#'
#' @param n Integer. Jumlah observasi yang akan disimulasikan.
#' @param dist Karakter. Nama distribusi, salah satu dari:
#' `"bernoulli"`, `"binomial"`, `"poisson"`, `"geometric"`,
#' `"normal"`, `"exponential"`, `"gamma"`, `"beta"`.
#' @param params List parameter distribusi, atau langsung angka jika hanya satu.
#'
#' @details
#' Parameter `params` diisi sebagai `list(...)`, dengan urutan sesuai distribusi:
#'
#' - **"bernoulli"**: `prob` → `list(prob)`
#' - **"binomial"**: `size`, `prob` → `list(size, prob)`
#' - **"poisson"**: `lambda` → `list(lambda)`
#' - **"geometric"**: `prob` → `list(prob)`
#' - **"normal"**: `mean`, `sd` → `list(mean, sd)`
#' - **"exponential"**: `rate` → `list(rate)`
#' - **"gamma"**: `shape`, `rate` → `list(shape, rate)`
#' - **"beta"**: `shape1`, `shape2` → `list(shape1, shape2)`
#'
#' Anda juga bisa hanya menggunakan angka langsung jika hanya satu parameter,
#' misalnya `params = 0.3` untuk distribusi Bernoulli.
#'
#' @return Vector data hasil simulasi.
#' @examples
#' simulate_data(100, "bernoulli", 0.3)
#' simulate_data(100, "normal", list(0, 1))
#' simulate_data(100, "poisson", list(5))
#' simulate_data(100, "beta", list(2, 5))
#' @importFrom stats rbinom rpois rgeom rnorm rexp rgamma rbeta
#' @export
simulate_data <- function(n, dist, params = list()) {
  if (!is.list(params)) params <- list(params)
  switch(dist,
         "bernoulli" = rbinom(n, size = 1, prob = params[[1]]),
         "binomial" = rbinom(n, size = params[[1]], prob = params[[2]]),
         "poisson" = rpois(n, lambda = params[[1]]),
         "geometric" = rgeom(n, prob = params[[1]]),
         "normal" = rnorm(n, mean = params[[1]], sd = params[[2]]),
         "exponential" = rexp(n, rate = params[[1]]),
         "gamma" = rgamma(n, shape = params[[1]], rate = params[[2]]),
         "beta" = rbeta(n, shape1 = params[[1]], shape2 = params[[2]]),
         stop("Distribusi tidak dikenali.")
  )
}
