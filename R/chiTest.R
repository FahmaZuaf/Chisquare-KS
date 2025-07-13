#' Uji Chi-Square untuk Data Diskrit
#'
#' Fungsi ini melakukan uji Chi-Square goodness-of-fit untuk distribusi diskrit
#' (bernoulli, binomial, poisson, geometric) dengan parameter bisa ditentukan
#' atau diestimasi otomatis dari data.
#'
#' @param data Vector data diskrit.
#' @param dist Nama distribusi teoretik: "bernoulli", "binomial", "poisson", atau "geometric".
#' Jika NULL, maka digunakan "binomial" secara default.
#' @param params List parameter distribusi. Jika NULL, akan diestimasi dari data.
#'
#' @return Objek dari \code{chisq.test}
#'
#' @examples
#' x <- simulate_data(100, "binomial", list(2, 0.5))
#' chi_square_test(x)  # otomatis deteksi distribusi dan estimasi parameter
#' chi_square_test(x, "binomial", list(size = 2, prob = 0.5))
#'
#' x <- simulate_data(100, "poisson", list(3))
#' chi_square_test(x, "poisson")
#'
#' @importFrom stats chisq.test dbinom dpois dgeom
#' @export
chi_square_test <- function(data, dist = "binomial", params = NULL) {
  obs <- table(data)
  x_vals <- as.integer(names(obs))

  # Estimasi parameter jika tidak diberikan
  if (is.null(params)) {
    if (dist == "bernoulli") {
      prob <- mean(data)
      params <- list(prob = prob)

    } else if (dist == "binomial") {
      size <- max(x_vals)
      prob <- mean(data) / size
      params <- list(size = size, prob = prob)

    } else if (dist == "poisson") {
      lambda <- mean(data)
      params <- list(lambda = lambda)

    } else if (dist == "geometric") {
      prob <- 1 / (mean(data) + 1)
      params <- list(prob = prob)

    } else {
      stop("Distribusi tidak dikenali.")
    }
  }

  # Hitung probabilitas teoretik berdasarkan distribusi
  prob <- switch(dist,
                 "bernoulli" = dbinom(x_vals, size = 1, prob = params$prob),
                 "binomial"  = dbinom(x_vals, size = params$size, prob = params$prob),
                 "poisson"   = dpois(x_vals, lambda = params$lambda),
                 "geometric" = dgeom(x_vals, prob = params$prob),
                 stop("Distribusi tidak valid untuk uji chi-square.")
  )

  prob <- prob / sum(prob)  # normalisasi jika perlu
  chisq.test(obs, p = prob)
}
