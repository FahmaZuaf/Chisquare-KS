#' Uji Kolmogorov-Smirnov untuk Data Kontinu
#'
#' Fungsi ini melakukan uji Kolmogorov-Smirnov untuk menguji apakah data berasal
#' dari distribusi kontinu teoretik tertentu (normal, eksponensial, gamma, atau beta).
#'
#' @param data Vector numerik. Data hasil pengamatan yang akan diuji.
#' @param dist Karakter. Nama distribusi teoretik: "normal", "exponential", "gamma", atau "beta".
#' @param params List parameter distribusi. Jika NULL, akan dihitung otomatis dari data.
#'
#' @return Objek hasil dari fungsi \code{ks.test}.
#'
#' @examples
#' x <- simulate_data(100, "beta", list(2, 5))
#' ks_test(x, "beta")  # estimasi otomatis shape1 dan shape2
#'
#' x <- simulate_data(100, "normal", list(0, 1))
#' ks_test(x)  # default: normal
#'
#' @importFrom stats ks.test sd var
#' @export
ks_test <- function(data, dist = "normal", params = NULL) {
  if (!is.list(params) && !is.null(params)) params <- list(params)

  if (dist == "normal") {
    mean_val <- if (!is.null(params$mean)) params$mean else mean(data)
    sd_val   <- if (!is.null(params$sd))   params$sd   else sd(data)
    return(ks.test(data, "pnorm", mean = mean_val, sd = sd_val))

  } else if (dist == "exponential") {
    rate_val <- if (!is.null(params$rate)) params$rate else 1 / mean(data)
    return(ks.test(data, "pexp", rate = rate_val))

  } else if (dist == "gamma") {
    shape <- if (!is.null(params$shape)) params$shape else (mean(data)^2 / var(data))
    rate  <- if (!is.null(params$rate))  params$rate  else (mean(data) / var(data))
    return(ks.test(data, "pgamma", shape = shape, rate = rate))

  } else if (dist == "beta") {
    if (is.null(params$shape1) || is.null(params$shape2)) {
      m <- mean(data)
      v <- var(data)
      if (m <= 0 || m >= 1) stop("Data beta harus berada dalam (0,1)")
      temp <- m * (1 - m) / v - 1
      shape1 <- m * temp
      shape2 <- (1 - m) * temp
    } else {
      shape1 <- params$shape1
      shape2 <- params$shape2
    }
    return(ks.test(data, "pbeta", shape1 = shape1, shape2 = shape2))

  } else {
    stop("Distribusi tidak valid.")
  }
}
