#' Instantaneous AQALY score for a welfare state
#'
#' Computes the estimated welfare score (and its standard error, via the
#' delta method) associated with a given four-dimension welfare state code,
#' relative to a perfect welfare state (score = 1).
#'
#' @param welfare_state Character string of four digits (each between
#'   1 and 9) describing the welfare state to evaluate on the four
#'   dimensions, e.g. `"1359"`. `1` denotes the most positive level on a
#'   dimension. Each digit corresponds to one of the following levels:
#'   * 1 - High-level positive experiences
#'   * 2 - Significant positive experiences
#'   * 3 - Mild positive experiences
#'   * 4 - Minor positive experiences
#'   * 5 - Neutral: negative and positive experiences compensate
#'   * 6 - Minor negative experiences
#'   * 7 - Mild negative experiences
#'   * 8 - Severe negative experiences
#'   * 9 - Very severe negative experiences
#' @param alpha_level Numeric between 0 and 1, or `NULL` (the
#'   default). The significance level used to compute a confidence
#'   interval around the score via the delta method, e.g. `0.05` for a
#'   95% confidence interval. If `NULL`, no interval is computed.
#' @param results_mixedlogit An object containing `coefficients` and
#'   `vcov` elements from a mixed logit estimation (e.g. via the
#'   \pkg{logitr} package). Defaults to the package's bundled estimates
#'   (`estimatesAQALY`), fitted on UK stated-preference data.
#'
#' @return A list with elements `score` (numeric) and `se` (numeric, its
#'   standard error). If `alpha_level` is specified, the list also
#'   includes `lower_bound` and `upper_bound`.
#' @keywords internal
InstantAQALYscore <- function(welfare_state,
                              alpha_level = NULL,
                              results_mixedlogit = estimatesAQALY) {
  
  # Stop if alpha_level is incorrectly set
  if (!is.null(alpha_level) &&
      (alpha_level < 0 || alpha_level > 1)) {
    stop("alpha_level must take values between 0 and 1.")
  }
  
  # Decompose the welfare score
  dim1 <- as.numeric(substr(welfare_state, 1, 1))
  dim2 <- as.numeric(substr(welfare_state, 2, 2))
  dim3 <- as.numeric(substr(welfare_state, 3, 3))
  dim4 <- as.numeric(substr(welfare_state, 4, 4))
  
  # Run the code if we are not at the perfect welfare state
  if (dim1 + dim2 + dim3 + dim4 > 4) {
    
    # Compute the estimated AQALY score
    coefs <- results_mixedlogit$coefficients
    score <- 1 + (
      ifelse(dim1 == 2, 1, 0) * coefs[2] / 2 +
        ifelse(dim1 == 3, 1, 0) * coefs[2] +
        ifelse(dim1 == 4, 1, 0) * (coefs[2] / 2 + coefs[3] / 2) +
        ifelse(dim1 == 5, 1, 0) * coefs[3] +
        ifelse(dim1 == 6, 1, 0) * (coefs[3] / 2 + coefs[4] / 2) +
        ifelse(dim1 == 7, 1, 0) * coefs[4] +
        ifelse(dim1 == 8, 1, 0) * (coefs[4] / 2 + coefs[5] / 2) +
        ifelse(dim1 == 9, 1, 0) * coefs[5] +
        ifelse(dim2 == 2, 1, 0) * coefs[6] / 2 +
        ifelse(dim2 == 3, 1, 0) * coefs[6] +
        ifelse(dim2 == 4, 1, 0) * (coefs[6] / 2 + coefs[7] / 2) +
        ifelse(dim2 == 5, 1, 0) * coefs[7] +
        ifelse(dim2 == 6, 1, 0) * (coefs[7] / 2 + coefs[8] / 2) +
        ifelse(dim2 == 7, 1, 0) * coefs[8] +
        ifelse(dim2 == 8, 1, 0) * (coefs[8] / 2 + coefs[9] / 2) +
        ifelse(dim2 == 9, 1, 0) * coefs[9] +
        ifelse(dim3 == 2, 1, 0) * coefs[10] / 2 +
        ifelse(dim3 == 3, 1, 0) * coefs[10] +
        ifelse(dim3 == 4, 1, 0) * (coefs[10] / 2 + coefs[11] / 2) +
        ifelse(dim3 == 5, 1, 0) * coefs[11] +
        ifelse(dim3 == 6, 1, 0) * (coefs[11] / 2 + coefs[12] / 2) +
        ifelse(dim3 == 7, 1, 0) * coefs[12] +
        ifelse(dim3 == 8, 1, 0) * (coefs[12] / 2 + coefs[13] / 2) +
        ifelse(dim3 == 9, 1, 0) * coefs[13] +
        ifelse(dim4 == 2, 1, 0) * coefs[14] / 2 +
        ifelse(dim4 == 3, 1, 0) * coefs[14] +
        ifelse(dim4 == 4, 1, 0) * (coefs[14] / 2 + coefs[15] / 2) +
        ifelse(dim4 == 5, 1, 0) * coefs[15] +
        ifelse(dim4 == 6, 1, 0) * (coefs[15] / 2 + coefs[16] / 2) +
        ifelse(dim4 == 7, 1, 0) * coefs[16] +
        ifelse(dim4 == 8, 1, 0) * (coefs[16] / 2 + coefs[17] / 2) +
        ifelse(dim4 == 9, 1, 0) * coefs[17]
    )
    score <- unname(score)
    
    # Compute the AQALY score SE
    g_str <- paste0("~")
    if (dim1 == 2) g_str <- paste0(g_str, "x2/2")
    if (dim1 == 3) g_str <- paste0(g_str, "x2")
    if (dim1 == 4) g_str <- paste0(g_str, "x2/2+x3/2")
    if (dim1 == 5) g_str <- paste0(g_str, "x3")
    if (dim1 == 6) g_str <- paste0(g_str, "x3/2+x4/2")
    if (dim1 == 7) g_str <- paste0(g_str, "x4")
    if (dim1 == 8) g_str <- paste0(g_str, "x4/2+x5/2")
    if (dim1 == 9) g_str <- paste0(g_str, "x5")
    if (dim1 > 1 & dim2 + dim3 + dim4 > 3) g_str <- paste0(g_str, "+")
    if (dim2 == 2) g_str <- paste0(g_str, "x6/2")
    if (dim2 == 3) g_str <- paste0(g_str, "x6")
    if (dim2 == 4) g_str <- paste0(g_str, "x6/2+x7/2")
    if (dim2 == 5) g_str <- paste0(g_str, "x7")
    if (dim2 == 6) g_str <- paste0(g_str, "x7/2+x8/2")
    if (dim2 == 7) g_str <- paste0(g_str, "x8")
    if (dim2 == 8) g_str <- paste0(g_str, "x8/2+x9/2")
    if (dim2 == 9) g_str <- paste0(g_str, "x9")
    if (dim2 > 1 & dim3 + dim4 > 2) g_str <- paste0(g_str, "+")
    if (dim3 == 2) g_str <- paste0(g_str, "x10/2")
    if (dim3 == 3) g_str <- paste0(g_str, "x10")
    if (dim3 == 4) g_str <- paste0(g_str, "x10/2+x11/2")
    if (dim3 == 5) g_str <- paste0(g_str, "x11")
    if (dim3 == 6) g_str <- paste0(g_str, "x11/2+x12/2")
    if (dim3 == 7) g_str <- paste0(g_str, "x12")
    if (dim3 == 8) g_str <- paste0(g_str, "x12/2+x13/2")
    if (dim3 == 9) g_str <- paste0(g_str, "x13")
    if (dim3 > 1 & dim4 > 1) g_str <- paste0(g_str, "+")
    if (dim4 == 2) g_str <- paste0(g_str, "x14/2")
    if (dim4 == 3) g_str <- paste0(g_str, "x14")
    if (dim4 == 4) g_str <- paste0(g_str, "x14/2+x15/2")
    if (dim4 == 5) g_str <- paste0(g_str, "x15")
    if (dim4 == 6) g_str <- paste0(g_str, "x15/2+x16/2")
    if (dim4 == 7) g_str <- paste0(g_str, "x16")
    if (dim4 == 8) g_str <- paste0(g_str, "x16/2+x17/2")
    if (dim4 == 9) g_str <- paste0(g_str, "x17")
    
    # Gradient
    g <- stats::as.formula(g_str)
    
    # Standard Error using the delta method
    se <- msm::deltamethod(
      g = g,
      mean = results_mixedlogit$coefficients,
      cov = results_mixedlogit$vcov,
      ses = TRUE
    )
  }
  
  # Return one if perfect welfare
  if (dim1 + dim2 + dim3 + dim4 == 4) {
    score <- 1
    se <- 0
  }
  
  # Return bounds
  if (!is.null(alpha_level)) {
    z_stat <- stats::qnorm(1 - alpha_level / 2)
    lower_bound <- score - z_stat * se
    upper_bound <- score + z_stat * se
  }
  
  if (is.null(alpha_level)) {
    return(list(score = score, se = se))
  }
  list(
    score = score,
    se = se,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
}

#' AQALY score over a period of time
#'
#' Computes the Animal Quality-Adjusted Life Year (AQALY) score, and its
#' standard error, associated with spending a given number of years in a
#' specified welfare state, relative to a perfect welfare state.
#'
#' @param welfare_state Character string of four digits (each between
#'   1 and 9) describing the welfare state to evaluate, e.g. `"7979"`. Each
#'   digit corresponds to one of the following levels:
#'   * 1 - High-level positive experiences
#'   * 2 - Significant positive experiences
#'   * 3 - Mild positive experiences
#'   * 4 - Minor positive experiences
#'   * 5 - Neutral: negative and positive experiences compensate
#'   * 6 - Minor negative experiences
#'   * 7 - Mild negative experiences
#'   * 8 - Severe negative experiences
#'   * 9 - Very severe negative experiences
#' @param number_years Numeric. Number of years (or fraction of a
#'   year, e.g. `30/365` for 30 days) spent in that welfare state.
#' @param alpha_level Numeric between 0 and 1, or `NULL` (the
#'   default). The significance level used to compute a confidence
#'   interval around the score via the delta method, e.g. `0.05` for a
#'   95% confidence interval. If `NULL`, no interval is computed.
#'
#' @return A list with elements `score` (the AQALY score) and `se` (its
#'   standard error). If `alpha_level` is specified, the list also
#'   includes `lower_bound` and `upper_bound`.
#' @export
#'
#' @examples
#' AQALYscore(welfare_state = "7979", number_years = 30 / 365)
AQALYscore <- function(welfare_state,
                       number_years,
                       alpha_level = NULL) {
  res <- InstantAQALYscore(
    welfare_state = welfare_state,
    alpha_level = alpha_level
  )
  
  if (is.null(alpha_level)) {
    return(list(
      score = res$score * number_years,
      se = res$se * number_years
    ))
  }
  list(
    score = res$score * number_years,
    se = res$se * number_years,
    lower_bound = res$lower_bound * number_years,
    upper_bound = res$upper_bound * number_years
  )
}