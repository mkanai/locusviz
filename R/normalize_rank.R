normalize_rank <- function(score, decay = 0.5, ties.method = "min") {
  weights = decay ** (rank(score, ties.method = ties.method) - 1)
  weights[is.na(score)] = 0
  return(weights)
}