#' Mixed logit estimates underlying the AQALY score (UK)
#'
#' Results of the mixed logit model estimated on UK stated-preference
#' (discrete choice experiment) data, used by default to compute AQALY
#' scores. Produced with \pkg{logitr}.
#'
#' @format A list-like object with (at least) the elements:
#' \describe{
#'   \item{coefficients}{Named numeric vector of estimated coefficients.}
#'   \item{vcov}{Variance-covariance matrix of the coefficients.}
#' }
#' @source Combined replicate data, UK sample. See the associated AQALY
#'   paper for estimation details.
"estimatesAQALY"
