#' Normalize scores by rank with exponential decay
#'
#' This function converts scores to weights based on their rank, applying an
#' exponential decay factor. Higher ranked scores receive exponentially
#' decreasing weights.
#'
#' @param score Numeric vector of scores to be normalized
#' @param decay Numeric decay factor between 0 and 1 (default: 0.5).
#'   Smaller values result in faster decay
#' @param ties.method Character string specifying how ties are handled.
#'   Options: "average", "first", "last", "random", "max", "min" (default: "min")
#'
#' @return Numeric vector of normalized weights based on rank, with NA values
#'   receiving weight of 0
#'
#' @examples
#' # Normalize a vector of scores
#' scores <- c(10, 20, 15, NA, 25)
#' weights <- normalize_rank(scores)
#'
#' # Use faster decay
#' weights_fast <- normalize_rank(scores, decay = 0.3)
#'
#' @export
normalize_rank <- function(score, decay = 0.5, ties.method = "min") {
  weights <- decay**(rank(score, ties.method = ties.method) - 1)
  weights[is.na(score)] <- 0
  return(weights)
}
