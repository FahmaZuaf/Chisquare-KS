#' Uji Kolmogorov-Smirnov Manual
#'
#' Melakukan uji KS satu sampel secara manual terhadap distribusi teoretik (normal, eksponensial, dll).
#'
#' @param data Data numerik vektor.
#' @param dist Karakter nama distribusi teoretik: "norm", "exp", "gamma", atau "beta".
#' @param params List parameter distribusi. Misalnya, list(mean = 0, sd = 1) untuk normal.
#'
#' @return List dengan nilai D, Dplus, Dminus, dan p-value (aproksimasi).
#' @importFrom stats ks.test pnorm pexp pgamma pbeta sd var
#' @export
#'
#' @examples
#' x <- rnorm(30)
#' ks_test(x, "norm", list(mean = 0, sd = 1))
ks_test <- function(data, dist = "norm", params = NULL) {
  n <- length(data)
  data_sorted <- sort(data)

  # Estimasi default parameter jika params tidak diberikan
  if (is.null(params)) {
    params <- switch(
      dist,
      norm = list(mean = mean(data), sd = sd(data)),
      exp = list(rate = 1 / mean(data)),
      gamma = {
        m <- mean(data)
        v <- var(data)
        list(shape = m^2 / v, rate = m / v)
      },
      beta = {
        if (any(data <= 0 | data >= 1)) stop("Data beta harus dalam (0,1)")
        m <- mean(data)
        v <- var(data)
        common <- m * (1 - m) / v - 1
        list(shape1 = m * common, shape2 = (1 - m) * common)
      },
      stop("Distribusi tidak didukung.")
    )
  }

  # 🔧 Deteksi & konversi scale → rate (khusus gamma)
  if (dist == "gamma") {
    if (!is.null(params$scale) && is.null(params$rate)) {
      params$rate <- 1 / params$scale
    }
  }

  # Hitung fungsi distribusi kumulatif teoritis
  F_theory <- switch(
    dist,
    norm = pnorm(data_sorted, mean = params$mean, sd = params$sd),
    exp = pexp(data_sorted, rate = params$rate),
    gamma = pgamma(data_sorted, shape = params$shape, rate = params$rate),
    beta = pbeta(data_sorted, shape1 = params$shape1, shape2 = params$shape2),
    stop("Distribusi tidak didukung.")
  )

  # CDF Empiris
  Fn_empirical <- (1:n) / n
  Dplus <- max(Fn_empirical - F_theory)
  Dminus <- max(F_theory - c(0, Fn_empirical[-n]))
  D <- max(Dplus, Dminus)

  # p-value exact dari ks.test
  ks_args <- c(list(x = data, y = paste0("p", dist)), params)
  ks_ref <- do.call(ks.test, ks_args)
  p_value_exact <- ks_ref$p.value

  # Aproksimasi p-value asimtotik
  ks_pval_approx <- function(D, n) {
    if (D <= 0 || n <= 0) return(1)
    lambda <- (sqrt(n) + 0.12 + 0.11 / sqrt(n)) * D
    j <- 1:100
    pval <- 1 - 2 * sum((-1)^(j - 1) * exp(-2 * (lambda^2) * j^2))
    min(max(pval, 0), 1)
  }
  p_value_asym <- ks_pval_approx(D, n)

  return(list(
    D = D,
    Dplus = Dplus,
    Dminus = Dminus,
    p_value_asym = p_value_asym,
    p_value_exact = p_value_exact
  ))
}
