#' Check if any pair in a group has been assigned together too often
#'
#' @description
#' Uses a globally tracked interaction matrix (e.g., initial_matrix) to determine whether
#' any pair in a group has already been assigned together two or more times.
#'
#' @param group A character vector of student names forming one group.
#'
#' @return TRUE if the group has no pair exceeding the overlap limit (2); FALSE otherwise.
#' @export
#'
#' @examples
#' initial_matrix <- initmat(c("A", "B", "C"))
#' group <- c("A", "B", "C")
#' testoverlap(group)
testoverlap <- function(group) {
  for (i in seq_along(group)) {
    for (j in seq_along(group)) {
      if (i != j) {
        if (initial_matrix[group[i], group[j]] >= 2) {
          return(FALSE)
        }
      }
    }
  }
  return(TRUE)
}
