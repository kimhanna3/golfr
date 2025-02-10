#' Assign Groups and Update Interaction Matrix
#'
#' @param student_data A data frame containing student identifiers. The column should be named `Student`.
#' @param students_per_group An integer specifying the number of students per group.
#' @param iterations An integer defining how many rounds of group assignments should be performed.
#'
#' @return A matrix representing the interaction history of students, where each cell indicates
#'         the number of times two students have been grouped together.
#' @export
#'
#' @examples
#' n_students <- 4
#' student_data <- GenerateData(n_students)
#' students_per_group <- 2
#' iterations <- 3
#'
#' matrices_df <- groupassign(student_data, students_per_group, iterations)
#' matrices_df
groupassign <- function(student_data, students_per_group, iterations) {
  # Initialize the interaction matrix
  initial_matrix <- initmat(student_data$Student)

  # Loop through iterations
  for (r in seq_len(iterations)) {
    # Assign groups
    grouped_data <- MakeGroups(student_data, students_per_group, 1, initial_matrix)

    # Update the interaction matrix
    initial_matrix <- updatemat(initial_matrix, grouped_data$Round_1, student_data$Student)
  }

  return(initial_matrix)
}
