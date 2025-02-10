#' Update Interaction Matrix with Group Assignments
#'
#' This function updates an existing interaction matrix based on new group assignments.
#' It increments the matrix values to track how often students have been grouped together.
#'
#' @param initialmat A square matrix where rows and columns represent students, and cell values track
#'        the number of times two students have been grouped together.
#' @param group_assignments A vector indicating the group assignments for each student.
#' @param students A vector containing student identifiers corresponding to the matrix row and column names.
#'
#' @return An updated matrix reflecting the new group assignments.
#' @export
#'
#' @examples
#' n_students <- 4
#' student_data <- GenerateData(n_students)
#' initial_matrix <- initmat(student_data$Student)
#' group_assignments <- c(1, 1, 2, 2)  # Example group assignments
#'
#' updated_matrix <- updatemat(initial_matrix, group_assignments, student_data$Student)
#' updated_matrix
updatemat <- function(initialmat, group_assignments, students) {
  numGroups <- length(unique(group_assignments))

  for (g in seq_len(numGroups)) {
    groupMembers <- students[group_assignments == g]
    for (j in seq_along(groupMembers)) {
      for (k in seq_along(groupMembers)) {
        if (j != k) {
          rn <- which(rownames(initialmat) == groupMembers[j])
          cn <- which(colnames(initialmat) == groupMembers[k])
          initialmat[rn, cn] <- 1 + initialmat[rn, cn]
        }
      }
    }
  }

  return(initialmat)
}
