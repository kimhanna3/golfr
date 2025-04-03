#' Assign every student into groups with a set number of students per group
#'
#' @description
#' Assign every student into groups with a set number of students per group,
#' over a specified number of iterations. The user must specify which column
#' contains student identifiers.
#'
#' @param df A data frame containing student information.
#' @param students_per_group A positive integer specifying number of students per group.
#' @param iterations A positive integer specifying how many iterations (rounds) to generate.
#' @param student_col A string or numeric index specifying the column in `df` that contains student names.
#'
#' @return A data frame where each row is a student and each column shows their group assignment across rounds.
#' @export
#'
#' @examples
#' exdf <- GenerateData(9)
#' MakeGroups(exdf, 3, 3, student_col = "Student")
MakeGroups <- function(df, students_per_group, iterations, student_col) {
  # Extract student names from specified column
  student_names <- df[[student_col]]

  num_students <- length(student_names)
  num_groups <- ceiling(num_students / students_per_group)

  # Initialize list to store group assignments
  group_assignments_list <- vector("list", length = iterations)

  # Initialize interaction matrix
  initial_matrix <- initmat(student_names)

  for (i in seq_len(iterations)) {
    shuffled_students <- sample(student_names)

    # Assign group numbers
    group_assignments <- rep(seq_len(num_groups),
                             each = students_per_group,
                             length.out = num_students)

    # Handle leftovers
    remaining_students <- num_students %% students_per_group
    if (remaining_students != 0) {
      extra_indices <- (num_students - remaining_students + 1):num_students
      extra_groups <- sample(seq_len(num_groups - 1), remaining_students)
      group_assignments[extra_indices] <- extra_groups
    }

    # Update interaction matrix
    for (g in seq_len(num_groups)) {
      groupMembers <- shuffled_students[group_assignments == g]
      for (j in seq_along(groupMembers)) {
        for (k in seq_along(groupMembers)) {
          if (j != k) {
            rn <- which(rownames(initial_matrix) == groupMembers[j])
            cn <- which(colnames(initial_matrix) == groupMembers[k])
            initial_matrix[rn, cn] <- 1
          }
        }
      }
    }

    # Create round assignment
    iteration_groups <- data.frame(Student = shuffled_students,
                                   round = group_assignments)
    names(iteration_groups) <- c("Student", paste0("Round", i))
    group_assignments_list[[i]] <- iteration_groups
  }

  # Merge group assignments across rounds
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Student", all = TRUE),
                          group_assignments_list,
                          init = data.frame(Student = student_names))

  return(combined_data)
}
