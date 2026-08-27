#' Instantaneous AQALY score for a welfare state
#'
#' Computes the estimated welfare score (and its standard error, via the
#' delta method) associated with a given four-dimension welfare state code,
#' relative to a perfect welfare state (score = 1).
#'
#' @param welfare_state_funct Character string of four digits (each between
#'   1 and 9) describing the welfare state to evaluate on the four
#'   dimensions, e.g. `"1359"`. `1` denotes the most positive level on a
#'   dimension.
#' @param results_mixedlogit_funct A fitted mixed logit results object
#'   (as returned by [logitr::logitr()]) containing `coefficients` and
#'   `vcov` elements. Defaults to the package's bundled estimates
#'   (`estimatesAQALY`), fitted on UK stated-preference data.
#'
#' @return A list with elements `score` (numeric) and `se` (numeric, its
#'   standard error).
#' @keywords internal
InstantAQALYscore <- function(welfare_state_funct,
                               results_mixedlogit_funct = estimatesAQALY) {

  # welfare_state_funct: welfare state to evaluate (no number of years): four
  # digits between 1 and 9. Ex: "1359". Level 1: Very positive ...
  # results_mixedlogit_funct: results of the mixed logit estimation

  # Decompose the welfare score
  dim1_funct <- as.numeric(substr(welfare_state_funct, 1, 1))
  dim2_funct <- as.numeric(substr(welfare_state_funct, 2, 2))
  dim3_funct <- as.numeric(substr(welfare_state_funct, 3, 3))
  dim4_funct <- as.numeric(substr(welfare_state_funct, 4, 4))

  # Run the code if we are not at the perfect welfare state
  if (dim1_funct + dim2_funct + dim3_funct + dim4_funct > 4) {

    # Compute the estimated AQALY score
    coefs_funct <- results_mixedlogit_funct$coefficients
    score_funct <- 1 + (
      ifelse(dim1_funct == 2, 1, 0) * coefs_funct[2] / 2 +
        ifelse(dim1_funct == 3, 1, 0) * coefs_funct[2] +
        ifelse(dim1_funct == 4, 1, 0) * (coefs_funct[2] / 2 + coefs_funct[3] / 2) +
        ifelse(dim1_funct == 5, 1, 0) * coefs_funct[3] +
        ifelse(dim1_funct == 6, 1, 0) * (coefs_funct[3] / 2 + coefs_funct[4] / 2) +
        ifelse(dim1_funct == 7, 1, 0) * coefs_funct[4] +
        ifelse(dim1_funct == 8, 1, 0) * (coefs_funct[4] / 2 + coefs_funct[5] / 2) +
        ifelse(dim1_funct == 9, 1, 0) * coefs_funct[5] +
        ifelse(dim2_funct == 2, 1, 0) * coefs_funct[6] / 2 +
        ifelse(dim2_funct == 3, 1, 0) * coefs_funct[6] +
        ifelse(dim2_funct == 4, 1, 0) * (coefs_funct[6] / 2 + coefs_funct[7] / 2) +
        ifelse(dim2_funct == 5, 1, 0) * coefs_funct[7] +
        ifelse(dim2_funct == 6, 1, 0) * (coefs_funct[7] / 2 + coefs_funct[8] / 2) +
        ifelse(dim2_funct == 7, 1, 0) * coefs_funct[8] +
        ifelse(dim2_funct == 8, 1, 0) * (coefs_funct[8] / 2 + coefs_funct[9] / 2) +
        ifelse(dim2_funct == 9, 1, 0) * coefs_funct[9] +
        ifelse(dim3_funct == 2, 1, 0) * coefs_funct[10] / 2 +
        ifelse(dim3_funct == 3, 1, 0) * coefs_funct[10] +
        ifelse(dim3_funct == 4, 1, 0) * (coefs_funct[10] / 2 + coefs_funct[11] / 2) +
        ifelse(dim3_funct == 5, 1, 0) * coefs_funct[11] +
        ifelse(dim3_funct == 6, 1, 0) * (coefs_funct[11] / 2 + coefs_funct[12] / 2) +
        ifelse(dim3_funct == 7, 1, 0) * coefs_funct[12] +
        ifelse(dim3_funct == 8, 1, 0) * (coefs_funct[12] / 2 + coefs_funct[13] / 2) +
        ifelse(dim3_funct == 9, 1, 0) * coefs_funct[13] +
        ifelse(dim4_funct == 2, 1, 0) * coefs_funct[14] / 2 +
        ifelse(dim4_funct == 3, 1, 0) * coefs_funct[14] +
        ifelse(dim4_funct == 4, 1, 0) * (coefs_funct[14] / 2 + coefs_funct[15] / 2) +
        ifelse(dim4_funct == 5, 1, 0) * coefs_funct[15] +
        ifelse(dim4_funct == 6, 1, 0) * (coefs_funct[15] / 2 + coefs_funct[16] / 2) +
        ifelse(dim4_funct == 7, 1, 0) * coefs_funct[16] +
        ifelse(dim4_funct == 8, 1, 0) * (coefs_funct[16] / 2 + coefs_funct[17] / 2) +
        ifelse(dim4_funct == 9, 1, 0) * coefs_funct[17]
    )
    score_funct <- unname(score_funct)

    # Compute the AQALY score SE
    g_funct_str <- paste0("~")
    if (dim1_funct == 2) g_funct_str <- paste0(g_funct_str, "x2/2")
    if (dim1_funct == 3) g_funct_str <- paste0(g_funct_str, "x2")
    if (dim1_funct == 4) g_funct_str <- paste0(g_funct_str, "x2/2+x3/2")
    if (dim1_funct == 5) g_funct_str <- paste0(g_funct_str, "x3")
    if (dim1_funct == 6) g_funct_str <- paste0(g_funct_str, "x3/2+x4/2")
    if (dim1_funct == 7) g_funct_str <- paste0(g_funct_str, "x4")
    if (dim1_funct == 8) g_funct_str <- paste0(g_funct_str, "x4/2+x5/2")
    if (dim1_funct == 9) g_funct_str <- paste0(g_funct_str, "x5")
    if (dim1_funct > 1 & dim2_funct + dim3_funct + dim4_funct > 3) g_funct_str <- paste0(g_funct_str, "+")
    if (dim2_funct == 2) g_funct_str <- paste0(g_funct_str, "x6/2")
    if (dim2_funct == 3) g_funct_str <- paste0(g_funct_str, "x6")
    if (dim2_funct == 4) g_funct_str <- paste0(g_funct_str, "x6/2+x7/2")
    if (dim2_funct == 5) g_funct_str <- paste0(g_funct_str, "x7")
    if (dim2_funct == 6) g_funct_str <- paste0(g_funct_str, "x7/2+x8/2")
    if (dim2_funct == 7) g_funct_str <- paste0(g_funct_str, "x8")
    if (dim2_funct == 8) g_funct_str <- paste0(g_funct_str, "x8/2+x9/2")
    if (dim2_funct == 9) g_funct_str <- paste0(g_funct_str, "x9")
    if (dim2_funct > 1 & dim3_funct + dim4_funct > 2) g_funct_str <- paste0(g_funct_str, "+")
    if (dim3_funct == 2) g_funct_str <- paste0(g_funct_str, "x10/2")
    if (dim3_funct == 3) g_funct_str <- paste0(g_funct_str, "x10")
    if (dim3_funct == 4) g_funct_str <- paste0(g_funct_str, "x10/2+x11/2")
    if (dim3_funct == 5) g_funct_str <- paste0(g_funct_str, "x11")
    if (dim3_funct == 6) g_funct_str <- paste0(g_funct_str, "x11/2+x12/2")
    if (dim3_funct == 7) g_funct_str <- paste0(g_funct_str, "x12")
    if (dim3_funct == 8) g_funct_str <- paste0(g_funct_str, "x12/2+x13/2")
    if (dim3_funct == 9) g_funct_str <- paste0(g_funct_str, "x13")
    if (dim3_funct > 1 & dim4_funct > 1) g_funct_str <- paste0(g_funct_str, "+")
    if (dim4_funct == 2) g_funct_str <- paste0(g_funct_str, "x14/2")
    if (dim4_funct == 3) g_funct_str <- paste0(g_funct_str, "x14")
    if (dim4_funct == 4) g_funct_str <- paste0(g_funct_str, "x14/2+x15/2")
    if (dim4_funct == 5) g_funct_str <- paste0(g_funct_str, "x15")
    if (dim4_funct == 6) g_funct_str <- paste0(g_funct_str, "x15/2+x16/2")
    if (dim4_funct == 7) g_funct_str <- paste0(g_funct_str, "x16")
    if (dim4_funct == 8) g_funct_str <- paste0(g_funct_str, "x16/2+x17/2")
    if (dim4_funct == 9) g_funct_str <- paste0(g_funct_str, "x17")

    # Gradient
    g_funct <- stats::as.formula(g_funct_str)

    # Standard Error using the delta method
    se_funct <- msm::deltamethod(
      g = g_funct,
      mean = results_mixedlogit_funct$coefficients,
      cov = results_mixedlogit_funct$vcov,
      ses = TRUE
    )
  }

  # Return one if perfect welfare
  if (dim1_funct + dim2_funct + dim3_funct + dim4_funct == 4) {
    score_funct <- 1
    se_funct <- 0
  }

  return(list(score = score_funct, se = se_funct))
}

#' AQALY score over a period of time
#'
#' Computes the Animal Quality-Adjusted Life Year (AQALY) score, and its
#' standard error, associated with spending a given number of years in a
#' specified welfare state, relative to a perfect welfare state.
#'
#' @param welfare_state_funct Character string of four digits (each between
#'   1 and 9) describing the welfare state to evaluate, e.g. `"7979"`.
#' @param number_years_funct Numeric. Number of years (or fraction of a
#'   year, e.g. `30/365` for 30 days) spent in that welfare state.
#'
#' @return A list with elements `score` (the AQALY score) and `se` (its
#'   standard error).
#' @export
#'
#' @examples
#' AQALYscore(welfare_state_funct = "7979", number_years_funct = 30 / 365)
AQALYscore <- function(welfare_state_funct,
                        number_years_funct) {
  res_funct <- InstantAQALYscore(welfare_state_funct = welfare_state_funct)
  list(
    score = res_funct$score * number_years_funct,
    se = res_funct$se * number_years_funct
  )
}
