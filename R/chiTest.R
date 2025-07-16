#' Chi-Square GOF Test Manual (Setara chisq.test bawaan R)
#'
#' @param data Vector numerik diskrit (integer).
#' @param dist Distribusi: "binomial", "poisson", "geometric", "nbinom"
#' @param params List parameter distribusi. Lihat dokumentasi masing-masing.
#'
#' @return List hasil uji: statistik, df, p-value, observed, expected
#' @importFrom stats dbinom dpois dgeom dnbinom pchisq
#' @export
chisq_test_manual <- function(data, dist, params) {
  if (!dist %in% c("binomial", "poisson", "geometric", "nbinom")) {
    stop("Distribusi tidak dikenali.")
  }

  # Support berdasarkan data
  values <- sort(unique(data))
  obs_table <- table(factor(data, levels = values))
  observed <- as.numeric(obs_table)

  # Probabilitas teoritis sesuai support
  probs <- switch(dist,
                  binomial = dbinom(values, size = params$size, prob = params$prob),
                  poisson = dpois(values, lambda = params$lambda),
                  geometric = dgeom(values, prob = params$prob),
                  nbinom = dnbinom(values, size = params$size, prob = params$prob)
  )

  # Normalisasi dan expected count
  probs <- probs / sum(probs)
  expected <- probs * sum(observed)

  # Statistik uji
  chi_sq_stat <- sum((observed - expected)^2 / expected)
  df <- length(observed) - 1 - length(params)
  p_val <- pchisq(chi_sq_stat, df, lower.tail = FALSE)

  list(
    statistic = chi_sq_stat,
    df = df,
    p_value = p_val,
    observed = observed,
    expected = expected
  )
}
