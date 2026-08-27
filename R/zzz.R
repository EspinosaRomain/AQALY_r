# estimatesAQALY is lazy-loaded package data (data/estimatesAQALY.rda),
# used as a default argument value in InstantAQALYscore(). R CMD check's
# static analysis (codetools) can't see that it will exist at runtime, so
# it flags it as an undefined global variable. This tells check to ignore it.
#
#' @importFrom utils globalVariables
NULL

utils::globalVariables("estimatesAQALY")
