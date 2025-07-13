#' Uji Kolmogorov-Smirnov untuk Data Kontinu
#'
#' Fungsi ini melakukan uji Kolmogorov-Smirnov untuk menguji apakah data berasal
#' dari distribusi kontinu teoretik tertentu (normal, eksponensial, gamma, atau beta).
#'
#' @param data Vector numerik. Data hasil pengamatan yang akan diuji.
#' @param dist Karakter. Nama distribusi teoretik, salah satu dari:
#' `"normal"`, `"exponential"`, `"gamma"`, `"beta"`.
#' @param params List berisi parameter distribusi teoretik. Jika `NULL`, akan diestimasi dari data.
#' @param auto_scale Logical. Jika `TRUE` dan distribusi adalah "beta", data di luar (0,1) akan diskalakan otomatis.
#'
#' @return Objek hasil dari fungsi \code{ks.test}.
#'
#' @examples
#' x <- rnorm(100)
#' ks_test(x, "normal")
#'
#' y <- runif(100)
#' ks_test(y, "beta")  # Tidak perlu penskalaan
#'
#' z <- rnorm(100, mean = 5, sd = 2)
#' ks_test(z, "beta", auto_scale = TRUE)  # Otomatis diskalakan ke (0,1)
#'
#' @importFrom stats ks.test pnorm pexp pgamma pbeta sd var
#' @export
ks_test <- function(data, dist = "normal", params = NULL, auto_scale = FALSE) {
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
<<<<<<< HEAD
    # Validasi dan penskalaan otomatis jika perlu
    if (any(data <= 0 | data >= 1)) {
=======
     if (any(data <= 0 | data >= 1)) {
>>>>>>> 536a5468b85308539920bec94553be003738adfe
      if (auto_scale) {
        warning("Data beta di luar (0,1). Data akan diskalakan otomatis ke rentang (0,1).")
        data <- (data - min(data)) / (max(data) - min(data))
      } else {
        stop("Data beta harus berada dalam (0,1). Aktifkan 'auto_scale = TRUE' untuk penskalaan otomatis.")
      }
    }

    if (is.null(params$shape1) || is.null(params$shape2)) {
      m <- mean(data)
      v <- var(data)
      temp <- m * (1 - m) / v - 1
      shape1 <- m * temp
      shape2 <- (1 - m) * temp
    } else {
      shape1 <- params$shape1
      shape2 <- params$shape2
    }

    return(ks.test(data, "pbeta", shape1 = shape1, shape2 = shape2))

  } else {
    stop("Distribusi tidak valid. Gunakan salah satu dari: 'normal', 'exponential', 'gamma', atau 'beta'.")
  }
}
