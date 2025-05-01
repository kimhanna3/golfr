#' Count how many student pairs in a group have been grouped together at least twice
#'
#' @description
#' This function checks all pairs in a group and returns how many of them have
#' been paired together two or more times in the past, based on the provided interaction matrix.
#'
#' @param group A character vector of student names forming one group.
#' @param initial_matrix A square matrix tracking student pairings over time.
#'
#' @return An integer: number of student pairs in the group with ≥2 prior overlaps.
#' @export
#'
#' @examples
#' mat <- initmat(c("A", "B", "C"))
#' nooverlap(c("A", "B", "C"), mat)
nooverlap <- function(group, initial_matrix) {
  group <- as.character(unlist(group))
  overlap_count <- 0
  for (i in seq_along(group)) {
    for (j in seq_along(group)) {
      if (i != j) {
        if (initial_matrix[group[i], group[j]] >= 2) {
          overlap_count <- overlap_count + 1
        }
      }
    }
  }
  # Since each pair is double-counted (i,j and j,i), divide by 2
  return(overlap_count / 2)
}
