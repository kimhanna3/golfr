#' Calculate the conflict score given a cumulative and round matrix
#'
#' @description
#' Takes the updated matrix after a round (e.g., from `updatemat()`) and the cumulative
#' matrix from the previous round and computes the conflict score.
#'
#' @param updated_matrix The matrix after the new round is added (e.g., from `updatemat()`).
#' @param previous_matrix The matrix before the new round (e.g., from previous iteration).
#'
#' @return A numeric conflict score for the newly added round.
#' @export
#'
#' @examples
#' m1 <- initmat(c("A", "B", "C"))
#' m2 <- updatemat(m1, c(1,1,2), c("A", "B", "C"))
#' conflict_score(m2, m1)
conflict_score <- function(updated_matrix, previous_matrix) {
  # Calculate the round's weight matrix by subtraction
  round_matrix <- updated_matrix - previous_matrix

  # Extract upper triangle (unique unordered student pairs)
  madePair <- round_matrix[upper.tri(round_matrix)]
  scores_from_previous <- previous_matrix[upper.tri(previous_matrix)]

  score <- sum((scores_from_previous[as.logical(madePair)])^2)
  return(score)
}
